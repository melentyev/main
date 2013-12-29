open System
open System.IO
open System.Collections.Generic
type Register = r1 = 1 | r2  = 2 | r3 = 3 | r4 = 4

type CommandArg = 
    | Register of Register
    | Constant of int
    | Address of CommandArg

type Label = string

type Command = 
    | Add of Register * CommandArg
    | Sub of Register * CommandArg
    | Mul of Register * CommandArg
    | Div of Register * CommandArg
    | Mod of Register * CommandArg
    | Mov of CommandArg * CommandArg
    | Hlt of CommandArg
    | Cmp of Register * CommandArg
    | Jmp of Label
    | Je of CommandArg * Label
    | Jne of CommandArg * Label
    | Jg of CommandArg * Label
    | Jng of CommandArg * Label
    | Jl of CommandArg * Label
    | Jnl of CommandArg * Label

let getWords (lines: string seq) = 
    Seq.map (fun (s: string) -> s.Split [|' ';'\n';'\t'|] |> Array.toSeq) lines 
    |> Seq.concat
    
let parseRegister (tokens: string list) : Register * string list = 
    match tokens.Head with
        | "r1" -> Register.r1
        | "r2" -> Register.r2
        | "r3" -> Register.r3
        | "r4" -> Register.r4
        | _ -> failwith "Not a register!"
    , tokens.Tail

let parseLabel (tokens: string list) : Label * string list = 
    tokens.Head, tokens.Tail

let rec parseCommandArg (tokens: string list) : CommandArg * string list = 
    let constVal = ref 0
    match tokens.Head with
    | "[" -> 
        let carg, tokens = parseCommandArg tokens.Tail 
        Address(carg), tokens.Tail
    | x when not (Int32.TryParse(x, constVal) ) -> 
        Register(fst (parseRegister tokens) ), tokens.Tail
    | _ -> Constant(!constVal), tokens.Tail

let rec parser (parsed: Command list) (labels: Map<Label,int>) (tokens: string list) = 
    let localParse fp1 pf2 tokens =
        let register, tail = fp1 tokens
        register, (pf2 tail)
    if (tokens.IsEmpty) then 
        parsed, labels
    else 
        match tokens.Head with
        | "mov" -> 
            let register, (arg, tail) = localParse parseCommandArg parseCommandArg tokens.Tail
            parser (Mov (register, arg) :: parsed) labels tail 
        | "add" | "sub" | "mul" | "div" | "mod" | "cmp" ->
            let register, (arg, tail) = localParse parseRegister parseCommandArg tokens.Tail
            match tokens.Head with
            | "add" -> parser (Add (register, arg) :: parsed) labels tail
            | "sub" -> parser (Sub (register, arg) :: parsed) labels tail
            | "mul" -> parser (Mul (register, arg) :: parsed) labels tail
            | "div" -> parser (Div (register, arg) :: parsed) labels tail
            | "mod" -> parser (Mod (register, arg) :: parsed) labels tail
            | "cmp" -> parser (Cmp (register, arg) :: parsed) labels tail
            | _ -> failwith "error"  
        | "jmp" -> 
            let tail = tokens.Tail
            parser (Jmp(tail.Head) :: parsed) labels tail.Tail
        | "je" | "jne" | "jg" | "jng" | "jl" | "jnl" ->
            let arg, (label, tail) = localParse parseCommandArg parseLabel tokens.Tail
            match tokens.Head with
            | "je" -> parser (Je(arg, label) :: parsed) labels tail
            | "jne" -> parser (Jne(arg, label) :: parsed) labels tail
            | "jg" -> parser (Jg(arg, label) :: parsed) labels tail
            | "jng" -> parser (Jng(arg, label) :: parsed) labels tail
            | "jl" -> parser (Jl(arg, label) :: parsed) labels tail
            | "jnl" -> parser (Jnl(arg, label) :: parsed) labels tail
            | _ -> failwith "error"  
        | "hlt" -> 
            let arg, tail = parseCommandArg (tokens.Tail)
            parser (Hlt (arg) :: parsed) labels tail
        | lbl when lbl.Chars (lbl.Length - 1) = ':' ->
            parser parsed (labels.Add(lbl.Substring(0, lbl.Length - 1), parsed.Length) ) tokens.Tail
        | _ -> failwith "error"  

let rec execute (program: Command []) (labels: Map<Label,int>) (memory: int []) 
    (regVals: Map<Register, int>) (commandNumber: int) : int =
    let rec evalCommandArg arg =
        match arg with
        | Constant c -> c
        | Register r -> regVals.[r] 
        | Address p -> memory.[evalCommandArg p]
    
    let mapSetValue (map: Map<Register, int>) key value = (map.Remove key |> Map.add key value) 
    let nextStep = execute program labels memory regVals 
    let nextStepChangeRegs = execute program labels memory
    let nextStepArithm dest src op = 
        execute program labels memory 
            (mapSetValue regVals dest <| op regVals.[dest] (evalCommandArg src) ) (commandNumber + 1)
    let nextStepCond src label op = 
        nextStep <| if (op (evalCommandArg src) 0) then labels.[label] else commandNumber + 1
    match program.[commandNumber] with
    | Mov (dest, src) ->
        match dest with
        | Register r -> 
            nextStepChangeRegs (mapSetValue regVals r <| evalCommandArg src ) (commandNumber + 1)
        | Address p -> 
            memory.[evalCommandArg p] <- evalCommandArg src
            nextStep (commandNumber + 1)
        | _ -> failwith "here"
    | Add (dest, src) -> nextStepArithm dest src (+)
    | Sub (dest, src) -> nextStepArithm dest src (-)
    | Mul (dest, src) -> nextStepArithm dest src (*)
    | Div (dest, src) -> nextStepArithm dest src (/)
    | Mod (dest, src) -> nextStepArithm dest src (%)
    | Cmp (dest, src) -> nextStepArithm dest src (-)
    | Jmp (label) -> nextStep ( labels.[label] )
    | Je (src, label) -> nextStepCond src label (=)
    | Jne (src, label) -> nextStepCond src label (<>)
    | Jg (src, label) -> nextStepCond src label (>)
    | Jng (src, label) -> nextStepCond src label (<=)
    | Jl (src, label) -> nextStepCond src label (<)
    | Jnl (src, label) -> nextStepCond src label (>=)
    | Hlt (arg) -> evalCommandArg arg

[<EntryPoint>]
let main argv =
    let filename = @"C:\Users\Admin\SkyDrive\Программирование\main\fs_asm\fs_asm\bin\Debug\input.txt"
    let (programReversed, labels) = 
        getWords (File.ReadAllLines filename |> Array.toSeq) 
        |> Seq.toList
        |> List.filter (fun x -> x <> "")
        |> parser [] Map.empty
    let regVals = 
        Seq.fold (fun (m:Map<Register, int>) r -> m.Add(r, 0) ) Map.empty 
            (List.toSeq [Register.r1; Register.r2; Register.r3; Register.r4]) 

    execute (List.rev programReversed |> List.toArray) labels (Array.create 1000000 0) regVals 0
        |> printf "%A"
    Console.ReadKey() |> ignore
    0
