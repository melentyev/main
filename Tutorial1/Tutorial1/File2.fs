open System 
open System.Drawing 
open System.Windows.Forms 
open System.Web.Helpers
open System.Net
open System.Runtime.Serialization.Json
open System.Xml
open System.IO
open System.Collections.Generic
 
let webClient = new WebClient() 

type VkHelper = class
    static member APP_ID = "3916880"
    static member API_SECRET = "ZtTnJBXF42Q0oiPepsia"
    static member API_METHOD_URL = "https://api.vk.com/method/"
    
    static member Auth (wb : WebBrowser)  = 
        let url = "https://oauth.vk.com/authorize?client_id=" + VkHelper.APP_ID 
                + "&scope=8&redirect_uri=" + "https://oauth.vk.com/blank.html" + "&display=page&v=3.0&response_type=token"
        wb.Navigate url
    
    static member RunMethodStr name access_token (lParams: list<string>) =
        
        let mutable url = VkHelper.API_METHOD_URL + name + ".xml?access_token=" + access_token
        for p in lParams do
            url <- url + "&" + p
            ()
        webClient.DownloadString url

    static member RunMethod name access_token (lParams: list<string>) =
        let Doc = new XmlDocument() 
        VkHelper.RunMethodStr name access_token lParams |> Doc.LoadXml
        Doc
    static member logout (wb : WebBrowser) =    
        let link = wb.Document.GetElementById("logout_link")
        link.InvokeMember("click") |> ignore
        ()
end

let alert s = 
    MessageBox.Show s

type MainForm() = 
    inherit Form() 

let mutable (checkboxes : Control list) = []
let mutable downloadTask:Async<unit> = async {()}
let mutable runAlbumsTask:Async<unit> = async {()}
let mutable downloadHandler = null
let mutable progressHandler = null
let mutable downloadedCnt = ref 0
let access_token = ref ""
let user_id = ref ""

let debugForm = new Form()
let output = new RichTextBox()
output.Width <- debugForm.ClientSize.Width
output.Height <- debugForm.ClientSize.Height
debugForm.Controls.Add(output)

let syncBtn = new Button()
syncBtn.Text <- "Process"
syncBtn.Left <- 130
syncBtn.Top <- 10

let syncLabel = new Label()
syncLabel.Text <- ""
syncLabel.Left <- 130
syncLabel.Width <- 400
syncLabel.Top <- 40

type strmap() = class
    let mutable root = new Dictionary<string, obj>()
    let dso (s:string) (a:obj) = (a :?> Dictionary<string, obj>).[s]
    let upsent (a:obj) (s:string) = (a :?> Dictionary<string, obj>).ContainsKey(s) |> not
    let conv (a:obj) = a :?> Dictionary<string, obj>
    member x.Item 
        with get (y:string) = root.[y] :?> string
        and set (y:string) (z:string) = root.[y] <- z
    member x.Item 
        with get (y:string*string) = 
            let a,b = y 
            if upsent root a then root.[a] <- new Dictionary<string, obj>() 
            if upsent root.[a] b then null else (root.[a] |> conv).[b] :?> string
        and set (y:string*string) (z:string) = 
            let a,b = y 
            if upsent root a then root.[a] <- new Dictionary<string, obj>() 
            (conv root.[a]).[b] <- z
    member x.Item 
        with get (y:string*string*string) = 
            let a,b,c = y 
            if upsent root a then root.[a] <- new Dictionary<string, obj>() 
            if upsent root.[a] b then (a |> conv).[b] <- new Dictionary<string, obj>() 
            if upsent (root.[a] |> dso b |> conv) c then null else (root.[a] |> dso b |> conv).[c] :?> string
        and set (y:string*string*string) (z:string) = 
            let a,b,c = y 
            if upsent root a then root.[a] <- new Dictionary<string, obj>() 
            if upsent root.[a] b then (a |> conv).[b] <- new Dictionary<string, obj>()
            ((root.[a] |> conv).[b] |> conv).[c] <- z 
end

let k = new strmap()
k.[("c", "d")] <- "3"
let s = k.[("a", "b")]

let t = k.["a"]

let syncProgressSet (s:string) =
    if syncLabel.InvokeRequired then
        syncLabel.Invoke(new Action< string >(fun a -> 
            syncLabel.Text <- a  
            () 
        ), s) |> ignore 
    else
        syncLabel.Text <- s

syncBtn.Click.Add(fun _ -> 
    let folder = new FolderBrowserDialog()
    folder.SelectedPath <- "C:\\Users\\Admin\\Desktop\\Удалять можно\\vk_audio"
    folder.ShowDialog() |> ignore
    let path = folder.SelectedPath
    let remCheckboxes = ref checkboxes
    syncLabel.Text <- "Process started"
    runAlbumsTask <- async {
        match !remCheckboxes with
        | cbControl :: tail -> 
            let cb = (cbControl :?> CheckBox)
            remCheckboxes := tail
            if cb.Checked then
                let dirname = cb.Text   
                Directory.CreateDirectory(path + "\\" + dirname) |> ignore
                let res = VkHelper.RunMethod "audio.get" !access_token ["owner_id=" + !user_id; "album_id=" + cb.Tag.ToString(); "need_user=0"]
                let nodes = res.SelectNodes("/response/audio") |> ref
                let lNodes = ref [for v in !nodes -> v]
                let lNodesCnt = ref (!lNodes).Length

                progressHandler <- new DownloadProgressChangedEventHandler(fun sender args ->
                    let perc = args.ProgressPercentage.ToString()
                    "Progress (" + dirname + "): " + (!downloadedCnt).ToString() + "/" 
                        + (!lNodesCnt).ToString() + ", file: " + perc + "%"
                            |> syncProgressSet
                )

                let downloadHandlerFn sender (res: DownloadDataCompletedEventArgs)=
                    let data = res.Result
                    let v = (!lNodes).Head
                    let title = v.SelectSingleNode("title").InnerText
                    let artist = v.SelectSingleNode("artist").InnerText
                    let rec clearName (s : string) = 
                        if(s.Length < 1) then ""
                        elif (Char.IsLetterOrDigit s.[0] || s.[0] = ' ') then s.[0].ToString() + clearName(s.Substring(1))
                        else clearName(s.Substring(1) )
                    
                    let filename = clearName artist + " - " + clearName title + ".mp3"
                    let filefull = path + "\\" + dirname + "\\" + filename 
                    let file = new FileStream(filefull, FileMode.Create)
                    file.Write(data, 0, data.Length)
                    file.Close()
                    downloadedCnt := !downloadedCnt + 1

                    let labelText = "Progress (" + dirname + "): " + (!downloadedCnt).ToString() + "/" + (!lNodesCnt).ToString()

                    syncProgressSet labelText
                    lNodes := (!lNodes).Tail
                    webClient.DownloadDataCompleted.RemoveHandler downloadHandler
                    webClient.DownloadProgressChanged.RemoveHandler progressHandler
                    Async.StartAsTask(downloadTask) |> ignore 
                
                downloadHandler <- new DownloadDataCompletedEventHandler(downloadHandlerFn)

                downloadedCnt := 0

                downloadTask <- async {
                    if (!lNodes).Length > 0 then
                        let url = (!lNodes).Head.SelectSingleNode("url").InnerText   
                        webClient.DownloadDataAsync(new Uri(url) )
                        webClient.DownloadProgressChanged.AddHandler progressHandler
                        webClient.DownloadDataCompleted.AddHandler downloadHandler
                    else Async.StartAsTask(runAlbumsTask) |> ignore
                }
                Async.StartAsTask(downloadTask) |> ignore 
            else 
                Async.StartAsTask(runAlbumsTask) |> ignore
                
        | [] -> ()
    }
    Async.StartAsTask(runAlbumsTask) |> ignore;
    ())


let form = new MainForm() 
form.Height <- 400
form.Width <- 600

let browser = new WebBrowser();
browser.Height <- 400
browser.Width <- 600

form.Text <- "Form"
form.Controls.Add(browser :> Control);

form.Shown.Add(fun _ -> 
    VkHelper.Auth browser
    ())
    
browser.DocumentCompleted.Add(fun _ -> 
    let frag = browser.Url.Fragment
    if (frag.Length > 1) then
        let parts = frag.Substring(1).Split('&')
        access_token := parts.[0].Split('=').[1]
        user_id := parts.[2].Split('=').[1]
        form.Controls.Remove browser
        let albums = VkHelper.RunMethod "audio.getAlbums" !access_token ["owner_id=" + !user_id; "offset=0"; "count=100"]
            
        let nodes = albums.SelectNodes("/response/album")
        let CH = ref 0
        checkboxes <- [
            for v in nodes do
                let album_id = v.SelectSingleNode("album_id").InnerText
                let title = v.SelectSingleNode("title").InnerText
                let cb = new CheckBox()
                cb.Checked <- false
                cb.Text <- title
                cb.Tag <- album_id
                cb.Top <- !CH
                CH := !CH + 23
                yield (cb :> Control)
            ]
            
        form.Controls.AddRange(List.toArray checkboxes)
        form.Controls.Add(syncBtn)
        form.Controls.Add(syncLabel)
        form.AutoScroll <- true
        ()
    ()
)

[<STAThread>] 
do Application.Run(form) 
(*
let rec gcd a b = 
    if a = 0 then  b else gcd (b % a) a

let p = gcd 327 21
let t = gcd 23521 75217
let v = 2144 / 251
*)