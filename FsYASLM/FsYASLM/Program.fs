open System
open FSharpx.Prelude
open YaslmLexer
open YaslmParser

[<EntryPoint>]
let main argv = 
    let input = IO.File.ReadAllLines("input.ysm") |> Array.map (flip (+) "\n") |> Array.reduce (+) |> str2l
    let lres = yaslmLexer input
    let lexs = fst lres |> List.map snd |> List.map ((@) [Newline]) |> List.concat
    //printf "%A" lexs
    let ast = yaslmParser lexs
    printf "succ: %A" ast
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
