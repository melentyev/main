open System
open System.IO
open System.Net
open System.Net.Sockets
open Telnet
open System.Threading

let tcpPort = 1488

[<EntryPoint>]
let main argv = 
    let client = new TcpClient()
    if argv.Length < 1 then 
        Console.WriteLine "No arguments passed"
    else 
        let req = argv.[0]
        let res = 
            async {
                do! client.ConnectAsync("127.0.0.1", tcpPort) |> Async.AwaitIAsyncResult |> Async.Ignore
                let st = client.GetStream()
                let sw = new StreamWriter(st)
                let sr = new StreamReader(st)
                printfn "sending request: %s" req
                do! sw.WriteLineAsync(req) |> Async.AwaitIAsyncResult |> Async.Ignore
                do! sw.FlushAsync() |> Async.AwaitIAsyncResult |> Async.Ignore
                let! res = Async.AwaitTask <| sr.ReadToEndAsync()
                return res
            }
            |> Async.RunSynchronously
        printfn "Response request: %s" req
    0
