open System
open FSharpx.Prelude
open YaslmLexer
//open CYaslmParser
open AST
open GenParser.GpInline


let makeFactor s a = Factor (s, a)
let makePower a b c = Power (a, b, c)

type ExprParser() = 
    member x.name = (Name << lxName) <@> matchNext (fun lx -> 
            match lx with LxName s when (Option.isNone << List.tryFind ((=) s)) keywords -> true
                          | _ -> false)
                            
    member x.operator op = matchNext ( (=^=) (Op op))
    member x.constInt = (ConstInt << lxNumber) <@> matchNext (fun lx -> match lx with LxNumber _ -> true | _ -> false)
    
    member x.termDelim = (lxOp <@> (x.operator "+") ) 
                        <|> (lxOp <@> (x.operator "-") ) 
    member x.factorDelim = (lxOp <@> (x.operator "*") ) 
                         <|> (lxOp <@> (x.operator "/") ) 
                         <|> (lxOp <@> (x.operator "%") )                    
    member x.factor =  x.name <|> x.constInt 
    member x.term        = x.samePriorBinaryOp (x.factor) x.factorDelim Term 
    member x.arithExpr   = x.samePriorBinaryOp x.term x.termDelim ArithExpr
    member x.pModule = genp {
        return! Module <@> manyOrNone (x.arithExpr)
        }
     member x.samePriorBinaryOp (expr: GenParser<LexemType, 's, AST>) (opParser: GenParser<LexemType, 's, 'a>) ctor = genp {
        let! hd = expr
        let! tail = manyOrNone (genp { 
                                    let! op = opParser
                                    let! e = expr
                                    return (op, e) })
        return List.fold (fun acc (op, e) -> ctor (acc, op, e) ) hd tail
    }
    member x.Parse (input: LexemType list) = 
                runParser x.pModule input () id (fun r -> failwithf "fail: %A" r)

[<EntryPoint>]
let main argv = 
    let input = IO.File.ReadAllLines("input.ysm") |> Array.map (flip (+) "\n") |> Array.reduce (+) |> str2l
    let lres = yaslmLexer input |> fst 
    let lexs = 
        lres
        |> List.map snd 
        |> List.map (flip (@) [Newline]) 
        |> List.concat
    let prsr = new ExprParser()
    let ast = prsr.Parse lexs
    (*
    let input = IO.File.ReadAllLines("input.ysm") |> Array.map (flip (+) "\n") |> Array.reduce (+) |> str2l
    let lres = yaslmLexer input
    let lexs = fst lres |> List.map snd |> List.map ((@) [Newline]) |> List.concat
    //printf "%A" lexs
    let prsr = new YParser()
    let ast = prsr.Parse lexs*)
    printf "succ: %A" ast
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
