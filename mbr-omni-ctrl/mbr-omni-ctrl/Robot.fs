module Robot

open Microsoft.FSharp.Data
open I2C
open System
open System.Net
open System.Net.Sockets
open System.Threading
open System.Collections.Generic
open System.Text

type PadEvent = 
    | Pad of int * ( int * int ) option
    | Button of int
    | Wheel of int
    | Stop

type Robot (?listenPad, ?padPort) =
    let listenPadVal = defaultArg listenPad false
    let padPortVal = defaultArg padPort 4444
    let padEvent = new Event<_>()
    let padMailboxWorkflow (inbox: MailboxProcessor<_>) = 
        let rec loop() = async {
            let! msg = inbox.Receive() 
            padEvent.Trigger(msg)
            return! loop ()
        }
        loop()
    let padMailbox = new MailboxProcessor<PadEvent>(padMailboxWorkflow)
    let server = async {
        let listener = new TcpListener(IPAddress.Any, padPortVal)
        listener.Start()
        printfn "Listening on %d..." padPortVal
        let rec loop() = async {
            let client = listener.AcceptTcpClient()
            let rec clientLoop() = async {
                let request = 
                    let buf = Array.create client.ReceiveBufferSize <| byte 0
                    let count = client.GetStream().Read(buf, 0, buf.Length) 
                    Encoding.ASCII.GetString(buf, 0, count)   
                let notEmpty = (not << String.IsNullOrEmpty)
                request.Split([| '\n' |]) |> Array.filter notEmpty |> Array.iter (fun req -> 
                    padMailbox.Post <| 
                        match req.Split([| ' ' |]) |> Array.toList |> List.filter notEmpty with
                        | ["wheel"; x ] -> Wheel(Int32.Parse(x) )
                        | ["pad"; n; "up"] -> Pad(Int32.Parse(n), None )
                        | ["pad"; n; x; y] -> Pad(Int32.Parse(n), Some (Int32.Parse(x), Int32.Parse(y) ) )
                        | ["btn"; n; "down"] -> Button(Int32.Parse(n) )
                        | _ -> failwithf "error in request %A" req)
                return! clientLoop() 
            }
            Async.Start(clientLoop() )
            return! loop() 
        }
        Async.Start(loop() )
    }

    member this.PadEvent = padEvent.Publish
    member w.PadMailbox = padMailbox
    member w.Start() = 
        padMailbox.Start() 
        if listenPadVal then Async.Start server else ()
    member w.Stop() = padMailbox.Post(Stop)
    
let robot = new Robot(listenPad = true, padPort = 4444)
robot.PadEvent.Add <| fun x -> 
    printfn "%A" x
    ()
robot.Start()

Console.Out.WriteLine "Start..."


Console.ReadKey() |> ignore

