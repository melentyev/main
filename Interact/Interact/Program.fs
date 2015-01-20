open System
open System.IO
open System.Net
open System.Net.Sockets
open Telnet
open System.Threading

let tcpPort = 1488

[<EntryPoint>]
let main argv = 
    printfn "Args: %A" argv
    //Thread.Sleep 10000
    let client = new TcpClient()
    if argv.Length < 2 then 
        Console.WriteLine "Ussage: op [arg]*"
    else 
        let req = argv.[0] + " " + argv.[1]
        let res = 
            async {
                try
                    do! client.ConnectAsync("127.0.0.1", tcpPort) |> Async.AwaitIAsyncResult |> Async.Ignore
                    let st = client.GetStream()
                    let sw = new StreamWriter(st)
                    let sr = new StreamReader(st)
                    printfn "sending request: %s" req
                    do! sw.WriteLineAsync(req) |> Async.AwaitIAsyncResult |> Async.Ignore
                    do! sw.FlushAsync() |> Async.AwaitIAsyncResult |> Async.Ignore
                    let! res = Async.AwaitTask <| sr.ReadToEndAsync()
                    return res
                with _ -> 
                    return "Connection failed"
            }
            |> Async.RunSynchronously
        printfn "Result: %s" req
    0
