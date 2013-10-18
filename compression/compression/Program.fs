module Program

open System 
open System.Drawing 
open System.Windows.Forms 
open System.Text
open System.Configuration
open System.IO
open System.Collections.Generic

type HNode = { parent: HNode ref option; alpha: byte; value: int; bit: byte }

let prepareTree filename =
    let stream = File.OpenRead(filename)
    let limit = 1024
    let buf = Array.zeroCreate limit
    let dict = new SortedDictionary<byte, int>()
    let mutable readCnt = stream.Read(buf, 0, limit)
    let mutable nodes = []
    while (readCnt > 0) do
        for i = 0 to readCnt - 1 do
            let b = buf.[i]
            dict.[b] <- if dict.ContainsKey(b) then dict.[b] + 1 else 1
        readCnt <- stream.Read(buf, 0, limit)
    let mutable list = []
    for v in dict do 
        list <- (ref { parent = None; alpha = v.Key; value = v.Value; bit = 0 }) :: list
    let sorted = List.sort list
    for k in list do 
        nodes <- k :: nodes
    while nodes.Length > 1 do
        let smallest (nodes: HNode ref list) = 
            let mutable MinValue1 = int(1e9)
            let mutable MinValue2 = int(1e9)
            let mutable Min1 = nodes.Head
            let mutable Min2 = nodes.Head
            for v in nodes do
                if (!v).value < MinValue1 then 
                    MinValue1 <- (!v).value
                    Min1 <- v
                else if(!v).value < MinValue2 then
                    MinValue2 <- (!v).value
                    Min2 <- v 
            (Min1, Min2)
        let (l1, l2) = smallest nodes
        (!l1).parent = Some(ref { Parent: None; alpha: none; value = (!l1)}

type MainForm() as form = 
    inherit Form()
    [<DefaultValue>]
    val mutable config: Configuration
    do form.config <- ConfigurationManager.OpenExeConfiguration(ConfigurationUserLevel.None)
    do form.InitializeForm()
   
    member this.InitializeForm() = 
        this.Menu <-  
            let menu = new MainMenu()
            menu.MenuItems.AddRange(
                [|
                    yield new MenuItem("Open", new EventHandler
                        (fun _ _ ->
                            let file = new OpenFileDialog()
                            do file.ShowDialog() |> ignore
                            prepareTree file.FileName
                            //file.SelectedPath
                            )
                    )
                |])
            menu
[<STAThread>] 
Application.Run(new MainForm() )