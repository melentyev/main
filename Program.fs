open Microsoft.FSharp.Data
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

type Robot () =
    let padEvent = new Event<_>()
    let padMailboxWorkflow (inbox: MailboxProcessor<_>) = 
        let rec loop() = async {
            let! msg = inbox.Receive() 
            padEvent.Trigger(msg)
            return! loop ()
        }
        loop()
    let padMailbox = new MailboxProcessor<PadEvent>(padMailboxWorkflow)
    member this.PadEvent = padEvent.Publish
    member w.PadMailbox = padMailbox
    member w.Start() = padMailbox.Start() 
    member w.Stop() = padMailbox.Post(Stop)

let robot = new Robot()

let server = async {
    let listener = new TcpListener(IPAddress.Any, 4444)
    listener.Start()
    printfn "Listening..."
    let rec loop() = async {
        let client = listener.AcceptTcpClient()
        let rec clientLoop() = async {
            let request = 
                let buf = Array.create client.ReceiveBufferSize <| byte 0
                let count = client.GetStream().Read(buf, 0, buf.Length) 
                Encoding.ASCII.GetString(buf, 0, count)   
            let filterEmptyString s = s <> ""
            request.Split([| '\n' |]) |> Array.filter (not << String.IsNullOrEmpty) |> Array.iter (fun req -> 
                robot.PadMailbox.Post <| 
                    match req.Split([| ' ' |]) |> Array.toList |> List.filter (not << String.IsNullOrEmpty) with
                    | ["wheel"; x ] -> Wheel(Int32.Parse(x) )
                    | ["pad"; n; "up"] -> PadUp(Int32.Parse(n) )
                    | ["pad"; n; x; y] -> Pad(Int32.Parse(n), Int32.Parse(x), Int32.Parse(y) )
                    | ["btn"; n; "down"] -> Button(Int32.Parse(n) )
                    | _ -> failwithf "error in request %A" req)
            return! clientLoop() 
        }
        Async.Start(clientLoop() )
        return! loop() 
    }
    Async.Start(loop() )
}

robot.PadEvent.Add <| fun x -> 
    printfn "%A" x
    ()
robot.Start()

Console.Out.WriteLine "Start..."
Async.Start server

Console.ReadKey() |> ignore

let gamepad = GamePadListner.Create(portNumber)
gamepad. 
