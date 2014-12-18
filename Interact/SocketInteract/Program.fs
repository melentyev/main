open System
open System.Net
open System.Net.Sockets
open System.Text

[<EntryPoint>]
let main argv = 
    let cl = new TcpClient()
    async {
        //let buf = Array.zeroCreate 100
        cl.Connect("192.168.2.2", 9990)
        let st = cl.GetStream()
        let buf = ASCIIEncoding.ASCII.GetBytes("HELLO")
        do! st.AsyncWrite(buf, 0, buf.Length) 
        let! recv = st.AsyncRead(5)
        printfn "%d" recv.Length
        cl.Close()
    }
    |> Async.RunSynchronously 
    0 // return an integer exit code
