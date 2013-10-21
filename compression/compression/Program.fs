module Program

open System 
open System.Drawing 
open System.Windows.Forms 
open System.Text
open System.Configuration
open System.IO
open System.Collections.Generic

type HNode = { 
    mutable parent: HNode ref option; 
    alpha: byte; 
    value: int; 
    mutable bit: byte;
}

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
    stream.Close();
    let mutable list = []
    for v in dict do 
        list <- (ref { parent = None; alpha = v.Key; value = v.Value; bit = byte(0) }) :: list
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
        (!l1).parent <- Some(ref { parent = None; alpha = byte(0); value = (!l1).value + (!l2).value; bit = byte(0) } )
        (!l2).parent <- (!l1).parent
        (!l1).bit <- byte(0)
        (!l2).bit <- byte(1)
        nodes <- (!l1).parent.Value :: (List.filter(fun x -> not(x = l1 || x = l2) ) nodes)
    let mutable result = new Dictionary<byte, byte list>()
    for v in list do 
        let rec go_up x str = 
            match (!x).parent with
            | None -> str
            | Some(y) -> go_up y str @ [(!x).bit]  
        let code = go_up v []
        result.[v.Value.alpha] <- code
    result

let encode (codes: Dictionary<byte, byte list>) filename = 
    let stream = File.OpenRead(filename)
    let outStream = File.OpenWrite("out.txt");
    let limit = 1024
    let buf = Array.zeroCreate limit
    let mutable readCnt = stream.Read(buf, 0, limit) 
    let bitcnt = ref 0
    let byteValue = ref 0
    let addbit bit = 
        byteValue := !byteValue * 2 + int(bit)
        bitcnt := !bitcnt + 1
        if !bitcnt = 8 then
            outStream.WriteByte(byte(!byteValue) )
            byteValue := 0
            bitcnt := 0
         
    while (readCnt > 0) do
        for i = 0 to readCnt - 1 do
            let b = buf.[i]
            for c in codes.[b] do
                addbit c
        readCnt <- stream.Read(buf, 0, limit)
    outStream.Close()
    

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
                            let codes = prepareTree file.FileName
                            encode codes file.FileName
                            MessageBox.Show "finished" |> ignore
                            //file.SelectedPath
                            )
                    )
                |])
            menu
[<STAThread>] 
Application.Run(new MainForm() )