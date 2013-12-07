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
    | Jmp of Label

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
        | "add" ->
            let register, (arg, tail) = localParse parseRegister parseCommandArg tokens.Tail
            parser (Add (register, arg) :: parsed) labels tail
        | "sub" ->
            let register, (arg, tail) = localParse parseRegister parseCommandArg tokens.Tail
            parser (Sub (register, arg) :: parsed) labels tail
        | "mul" ->
            let register, (arg, tail) = localParse parseRegister parseCommandArg tokens.Tail
            parser (Mul (register, arg) :: parsed) labels tail
        | "div" ->
            let register, (arg, tail) = localParse parseRegister parseCommandArg tokens.Tail
            parser (Div (register, arg) :: parsed) labels tail
        | "mod" ->
            let register, (arg, tail) = localParse parseRegister parseCommandArg tokens.Tail
            parser (Mod (register, arg) :: parsed) labels tail
        | "jmp" -> 
            let tail = tokens.Tail
            parser (Jmp(tail.Head) :: parsed) labels tail.Tail
        | "hlt" -> 
            let arg, tail = parseCommandArg (tokens.Tail)
            parser (Hlt (arg) :: parsed) labels tail
        | lbl when lbl.Chars (lbl.Length - 1) = ':' ->
            parser parsed (labels.Add(lbl.Substring(0, lbl.Length - 1), parsed.Length) ) tokens.Tail
        | _ -> failwith "error"

let execute (programRev: Command list, labels: Map<Label,int>) =
    let program = List.rev programRev |> List.toArray
    let regVals = new Dictionary<Register, int>();
    regVals.Add(Register.r1, 0);
    regVals.Add(Register.r2, 0);
    regVals.Add(Register.r3, 0);
    regVals.Add(Register.r4, 0);
    let evalCommandArg arg =
        match arg with
        | Constant c -> c
        | Register r -> regVals.[r] 
        | _ -> failwith "here"
    let mutable result = 0
    let mutable bDone = false
    let mutable commandNumber = 0
    while not bDone do
        let cmd = program.[commandNumber]
        match cmd with
        | Mov (dest, src) ->
            match dest with
            | Register r -> regVals.[r] <- evalCommandArg src
            | _ -> failwith "here"
        | Add (dest, src) -> regVals.[dest] <- regVals.[dest] + evalCommandArg src 
        | Sub (dest, src) -> regVals.[dest] <- regVals.[dest] - evalCommandArg src 
        | Mul (dest, src) -> regVals.[dest] <- regVals.[dest] * evalCommandArg src 
        | Div (dest, src) -> regVals.[dest] <- regVals.[dest] / evalCommandArg src 
        | Mod (dest, src) -> regVals.[dest] <- regVals.[dest] % evalCommandArg src 
        | Jmp (label) ->
            commandNumber <- labels.[label] - 1
        | Hlt (arg) ->
            bDone <- true
            result <- (evalCommandArg arg)
        | _ -> failwith "here" 
        commandNumber <- commandNumber + 1
    result

[<EntryPoint>]
let main argv =
    let filename = @"C:\Users\Admin\SkyDrive\Программирование\main\fs_asm\fs_asm\bin\Debug\input.txt"
    getWords (File.ReadAllLines filename |> Array.toSeq) 
    |> Seq.toList
    |> List.filter (fun x -> x <> "")
    |> parser [] Map.empty
    |> execute
    |> printf "%A"
    Console.ReadKey() |> ignore
    0
