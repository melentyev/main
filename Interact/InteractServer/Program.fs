open System
open System.IO
open System.Net
open System.Text
open System.Net.Sockets
open System.Threading.Tasks
open System.Text.RegularExpressions
open System.Diagnostics
open Telnet

let tcpPort = 1488

let tn = new Terminal("192.168.2.2", 23, 7, 80, 180) // hostname, port, timeout [s], width, height
let login = "ADCDMST"
let pwd = "SYS1"
let acctnum = "ACCT#"
let logonProc = "ISPFPROC"

let awaitTask = Async.Ignore << Async.AwaitIAsyncResult 

let ftpUpdateListFromFile (file: string) =
    let lines = File.ReadAllLines(file)
    let path_pref = Path.GetDirectoryName file
    let fn (l: string) =
        let arr = 
            l.Trim().Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
            |> Array.map(fun s -> s.Trim().Trim([|'\"'; ','|]))
        if arr.Length = 2 && (not (arr.[0].StartsWith("//"))) then Some(arr.[0], arr.[1]) else None
    Array.choose fn lines
    |> Seq.map (fun (dep, dest) -> @"""" + path_pref + @"\" + dep + @"""", dest)
    |> Seq.toList

type SmallFtpClient () = 
    let proc = new Process()
    let mutable sw = null
    let mutable sr = null
    let mutable cout = ""
    do proc.StartInfo.FileName <- "ftp.exe"
    do proc.StartInfo.UseShellExecute <- false
    do proc.StartInfo.RedirectStandardInput <- true
    do proc.StartInfo.RedirectStandardOutput <- true

    static member Receive(fname, local) =
        use cl = new SmallFtpClient()
        cl.Connect()
        cl.ReceiveFile(fname, local)
    static member Send(dep, dest) =
        use cl = new SmallFtpClient()
        cl.Connect()
        cl.SendFile(dep, dest)

    member x.Connect () = 
        proc.Start() |> ignore
        sw <- proc.StandardInput
        sr <- proc.StandardOutput
        sw.WriteLine "open 192.168.2.2"
        sw.WriteLine "IBMUSER"
        sw.WriteLine "SYS1"

    member x.SendFile(dep, dest) = 
        sw.WriteLine ("put " + dep + " " + dest)
        
    member x.ReceiveFile(fname, local) = 
        sw.WriteLine ("recv " + fname + " " + local)

    member x.CapturedOutput = cout

    interface IDisposable with
        member x.Dispose () = 
            sw.WriteLine "quit"
            cout <- sr.ReadToEnd()
            proc.WaitForExit()

let sendAndWaitForString w s = 
        tn.SendResponse(s, true) |> ignore
        tn.WaitForString(w) |> ignore

let sendAndWaitForStringEnter w s =
    tn.SendResponse(s, true) |> ignore
    Threading.Thread.Sleep(15)
    tn.SendResponse("", true) |> ignore
    tn.WaitForString(w) |> ignore

let startTelnetSession () = 
    tn.Connect() |> ignore
    sendAndWaitForString "IKJ56700A ENTER USERID" login
    sendAndWaitForString ("PASSWORD FOR " + login) login
    sendAndWaitForString "ICH70001I" pwd
    sendAndWaitForString "THE PROCEDURE NAME" acctnum
    sendAndWaitForString "LOGON IN PROGRESS" logonProc
    tn.WaitForString("READY") |> ignore
    tn.SendResponse("", true) |> ignore
    tn.VirtualScreen.CleanScreen()

let doSubmit jcl jobname =
    tn.SendResponse("SUB 'MELEN.CNTL(" + jcl + ")'", true) |> ignore
    let s = tn.WaitForRegEx("(IKJ56262I MEMBER)|(SUBMITTED)")
    if s = "IKJ56262I MEMBER" then 
        "FUCK", "FUCK"
    else 
        let pattern = @"JOB " + jobname + @"\(JOB([0-9]*)\) SUBMITTED"
        let m = Regex.Match(tn.VirtualScreen.Hardcopy(), pattern)
        if m.Success then 
            let jid = m.Groups.[1].Value
            let res = tn.VirtualScreen.Hardcopy().TrimEnd();
            tn.VirtualScreen.CleanScreen()
            (jid, res)
        else
            "JID_NOT_FOUND", "JID_NOT_FOUND"

let isReady jobname jobId =
    tn.SendResponse("ST (" + jobname + "(JOB" + jobId + "))", true) |> ignore
    let s = tn.WaitForString("JOB " + jobname + "(JOB" + jobId + ")")
    let res = tn.VirtualScreen.Hardcopy().TrimEnd();
    tn.VirtualScreen.CleanScreen()
    res.Contains("ON OUTPUT QUEUE")
    
let waitForJob jobname jobId = 
    let rec loop () = async {
        if isReady jobname jobId then 
            ()
        else 
            do! Async.Sleep (1000)
            return! loop ()
    }
    loop ()

    

let getSdsfOutput jobname jobId outfile = 
    let tempISFIN = "tempisfin.txt"
    File.WriteAllLines(tempISFIN, 
        [
            "ST"                    
            "SELECT " + jobId
            "PRINT FILE REPORT"
            "FIND " + jobname
            "++XC"
        ])
    let sdsfISFIN = "MELEN.SRC(AAASDSF)"
    let sdsfISFOUT = "MELEN.LISTS(AABSDSF)"
    let sdsfREPORT =  "MELEN.LISTS(AARSDSF)"
    tn.VirtualScreen.CleanScreen()
    SmallFtpClient.Send(tempISFIN, "'" + sdsfISFIN + "'")
    Threading.Thread.Sleep(500)
    [   
        "ALLOC FI(ISFIN)  DA('" + sdsfISFIN + "') SHR"
        "ALLOC FI(ISFOUT) DA('" + sdsfISFOUT + "')"
        "ALLOC FI(REPORT) DA('" + sdsfREPORT + "')"
        "SDSF"
        "FREE FI(ISFOUT)"
        "FREE FI(REPORT)"
    ]
    |> List.iter (fun s -> sendAndWaitForStringEnter "READY" s; tn.VirtualScreen.CleanScreen())
    
    tn.VirtualScreen.CleanScreen()
    SmallFtpClient.Receive("'" + sdsfREPORT + "'", outfile)
    ()
    
[<EntryPoint>]
let main argv = 
    startTelnetSession()
    printfn "Session started"
    let listener = new TcpListener(IPAddress.Any, tcpPort)
    listener.Start()
    let messageBuf = Array.zeroCreate 1024 
    let rec loop() = async {
        let! client = listener.AcceptTcpClientAsync() |> Async.AwaitTask
        use stream = client.GetStream()
        let answer (s: string) = 
            use sw = new StreamWriter(stream)
            sw.WriteAsync(s) |> awaitTask
        let! count = client.GetStream().AsyncRead(messageBuf, 0, messageBuf.Length)
        let request = Encoding.ASCII.GetString(messageBuf, 0, count).TrimEnd [| '\r'; '\n' |]   
        printfn "Request: %s" request
        match request.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) with
        | [|"FTPUPD"; list_path|] -> 
            use cl = new SmallFtpClient()
            cl.Connect()
            ftpUpdateListFromFile (list_path)
            |> List.iter (fun (dep, dest) -> cl.SendFile(dep, dest) )
            
        | [|"SUBMIT"; jcl; jobname |] -> 
            let jobId, text = doSubmit jcl jobname
            if jobId = "FUCK" then 
                do! answer ("JCL not found")
            else    
                do! waitForJob jobname jobId
                getSdsfOutput jobname jobId (jobname + ".txt")
                do! answer(text)
        | [|"FTPGET"; f; loc |] ->  SmallFtpClient.Receive(f, loc)
        | _ -> do! answer ("UNRECOGNIZED REQUEST")

        client.Close()
        return! loop()
    }
    loop() |> Async.RunSynchronously
    listener.Stop()
    0
