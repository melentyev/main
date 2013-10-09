module File2

open Strmap
open System 
open System.Drawing 
open System.Windows.Forms 
open System.Web.Helpers
open System.Net
open System.Runtime.Serialization.Json
open System.Xml
open System.IO
open System.Collections.Generic
 
type VkHelper(webClient: WebClient) = 
    let APP_ID = "3916880"
    let API_METHOD_URL = "https://api.vk.com/method/"
    
    member this.Auth (wb : WebBrowser)  = 
        let url = "https://oauth.vk.com/authorize?client_id=" + APP_ID 
                + "&scope=8&redirect_uri=" + "https://oauth.vk.com/blank.html" + "&display=page&v=3.0&response_type=token"
        wb.Navigate url
    
    member this.RunMethodStr name access_token (lParams: list<string>) =
        let mutable url = API_METHOD_URL + name + ".xml?access_token=" + access_token
        for p in lParams do
            url <- url + "&" + p
            ()
        webClient.DownloadString url

    member this.RunMethod name access_token (lParams: list<string>) =
        let Doc = new XmlDocument() 
        this.RunMethodStr name access_token lParams |> Doc.LoadXml
        Doc
    member this.logout (wb : WebBrowser) =    
        let link = wb.Document.GetElementById("logout_link")
        link.InvokeMember("click") |> ignore
        ()

let alert s = 
    MessageBox.Show s

type songInfo = { id:string; artist: string; title:string; url: string; albumFolder: string }
type albumInfo = { album_id: string; title: string; }

type MainForm() as form = 
    inherit Form() 
    let syncBtn = new Button(Text = "Process", Left = 130, Top = 10, Visible = false)
    let syncLabel = new Label(Text = "", Left = 130, Width = 400, Top = 40, Visible = false)
    let mainMenu = new MainMenu()
    let mnuFile = new MenuItem(Text = "File")
    let mnuExit = new MenuItem(Text = "E&xit")
    let mnuAccountExit = new MenuItem(Text = "Account Exit")
    let mnuProcessList = new MenuItem()
    let browser = new WebBrowser()
    let access_token = ref ""
    let user_id = ref ""
    let savePath = ref ""
    let currentAlbumTitle = ref ""
    let downloadedCnt = ref 0
    let songList = new ResizeArray<songInfo>()
    let webClient = new WebClient() 
    let vkHelper = new VkHelper(webClient)
    let albumList = new ResizeArray<albumInfo>()
    let (checkboxes : Control list ref) = ref []
    let mutable accountExitDocumentCompletedHandler = null
    do form.InitializeForm()

    member this.InitializeForm() = 
        this.FormBorderStyle <- FormBorderStyle.Sizable
        this.Height <- 400
        this.Width <- 600
        this.AutoScroll <- true
        this.Controls.AddRange([| browser :> Control;
            syncBtn :> Control;
            syncLabel :> Control;
            |]);
        mnuExit.Click.Add(fun _ -> this.Close() )
        mnuFile.MenuItems.AddRange([| mnuAccountExit; mnuExit |])
        mainMenu.MenuItems.AddRange([| mnuFile |])
        this.Menu <- mainMenu

        browser.Width <- this.ClientSize.Width
        browser.Height <- this.ClientSize.Height

        webClient.DownloadProgressChanged.Add(fun args ->
            let perc = args.ProgressPercentage.ToString()
            "Progress (" + !currentAlbumTitle + "): " + (!downloadedCnt).ToString() + "/" 
                + songList.Count.ToString() + ", file: " + perc + "%"
            |> this.syncProgressSet
        )
        mnuProcessList.Click.AddHandler(new EventHandler(this.mnuProcessListClick) )
        mnuAccountExit.Click.AddHandler(new EventHandler(this.mnuAccountExitClick) )
        accountExitDocumentCompletedHandler <- new WebBrowserDocumentCompletedEventHandler(this.accountExitDocumentCompleted1) 
        vkHelper.Auth browser
    member this.mnuAccountExitClick s e =
        browser.Navigate("https://vk.com/");
        browser.DocumentCompleted.AddHandler(accountExitDocumentCompletedHandler)
        
    member this.accountExitDocumentCompleted1 s e =
        let link = browser.Document.GetElementById("logout_link")
        browser.DocumentCompleted.AddHandler(accountExitDocumentCompletedHandler)
        link.InvokeMember("click") |> ignore
         
    member this.browserDocumentCompleted s e = 
        let frag = browser.Url.Fragment
        if (frag.Length > 1) then
            let parts = frag.Substring(1).Split('&')
            access_token := parts.[0].Split('=').[1]
            user_id := parts.[2].Split('=').[1]
            this.Controls.Remove(browser)
            ()

    member this.downloadAlbumList() =
        let albumsRes = vkHelper.RunMethod "audio.getAlbums" !access_token ["owner_id=" + !user_id; "offset=0"; "count=100"]
        let nodes = albumsRes.SelectNodes("/response/album")
        albumList.Clear();
        songList.Clear();
        downloadedCnt := 0
        for v in nodes do
            albumList.Add ({album_id = v.SelectSingleNode("album_id").InnerText;
                title = v.SelectSingleNode("title").InnerText
            })
        let CH = ref 0
        checkboxes := [
            for v in albumList do
                CH := !CH + 23
                yield (new CheckBox(Checked = false, Text = v.title
                     ,Tag = v.album_id, Top = !CH) :> Control)
            ]
        ()
        this.Controls.AddRange(List.toArray !checkboxes)
        Async.StartAsTask(async { this.downloadAlbumSongsList(0) } ) |> ignore
    
    member this.downloadAlbumSongsList(number) = 
        if number >= albumList.Count then
            Async.StartAsTask(async { this.downloadSingleSong(0) } ) |> ignore
        else
            let cb = (!checkboxes).[number] :?> CheckBox
            if cb.Checked then 
                let alb = albumList.[number]
                let songsRes = (vkHelper.RunMethod "audio.get" !access_token) 
                let songsRes = songsRes (["owner_id=" + !user_id; "album_id=" + cb.Tag.ToString(); "need_user=0"])
                let nodes = songsRes.SelectNodes("/response/audio")
                for v in nodes do
                    songList.Add({ id = v.SelectSingleNode("id").InnerText;
                        title = v.SelectSingleNode("title").InnerText;
                        artist = v.SelectSingleNode("artist").InnerText;
                        url = v.SelectSingleNode("url").InnerText;
                        albumFolder = alb.title;
                    })        
            Async.StartAsTask(async { this.downloadAlbumSongsList(number + 1) } ) |> ignore

    member this.downloadSingleSong(number) =
        if number >= albumList.Count then
            alert "Finished" |> ignore
        else
            let song = songList.[number] 
            let data = webClient.DownloadData(song.url)
            
            let rec clearName (s : string) = 
                if(s.Length < 1) then ""
                elif (Char.IsLetterOrDigit s.[0] || s.[0] = ' ') then s.[0].ToString() + clearName(s.Substring(1))
                else clearName(s.Substring(1) )
                    
            let filename = clearName song.artist + " - " + clearName song.title + ".mp3"
            if not(Directory.Exists(!savePath + "\\" + song.albumFolder)) then
                Directory.CreateDirectory(!savePath + "\\" + song.albumFolder) |> ignore
            let filefull = !savePath + "\\" + song.albumFolder + "\\" + filename 
            let file = new FileStream(filefull, FileMode.Create)
            file.Write(data, 0, data.Length)
            file.Close()
            let labelText = "Progress: " + (!downloadedCnt).ToString() + "/" + songList.Count.ToString()
            this.syncProgressSet labelText
            Async.StartAsTask(async { this.downloadSingleSong(number + 1) } ) |> ignore

    member this.syncProgressSet (s) =
        if syncLabel.InvokeRequired then
            syncLabel.Invoke(new Action< string >(fun a -> 
                syncLabel.Text <- a  
                () 
            ), s) |> ignore 
        else
            syncLabel.Text <- s

    member this.mnuProcessListClick s args =
        Async.StartAsTask(async { this.downloadAlbumList() } ) |> ignore
    member this.mnuSelectAlbumsFolderClick(s, args) = 
        let folder = new FolderBrowserDialog()
        folder.SelectedPath <- "C:\\Users\\Admin\\Desktop\\Удалять можно\\vk_audio"
        folder.ShowDialog() |> ignore
        savePath := folder.SelectedPath

let debugForm = new Form()
let output = new RichTextBox(Width = debugForm.ClientSize.Width
    , Height = debugForm.ClientSize.Height)
debugForm.Controls.Add(output)

[<STAThread>] 
do Application.Run(new MainForm() ) 
(*
let rec gcd a b = 
    if a = 0 then  b else gcd (b % a) a

let p = gcd 327 21
let t = gcd 23521 75217
let v = 2144 / 251
*)