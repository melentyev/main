module Main

open System 
open System.Drawing 
open System.Windows.Forms 
open System.Web.Helpers
open System.Net
open System.Runtime.Serialization.Json
open System.Xml
open System.IO
open System.Collections.Generic
open System.Text
 
type songInfo = { id:string; artist: string; title:string; url: string; albumFolder: string }
type albumInfo = { album_id: string; title: string; }
type FormDelegate = delegate of obj -> unit

type VkHelper(webClient: WebClient) as this = 
    let APP_ID = "3916880"
    let API_METHOD_URL = "https://api.vk.com/method/"
    let mutable user_id = ""
    let mutable access_token = ""
    let mutable full_name = ""
    let mutable first_name = ""
    let mutable last_name = ""
    let utf8 = Encoding.GetEncoding("utf-8");
    let win1251 = Encoding.GetEncoding("windows-1251");
        
    let decode (s:string) = 
        Encoding.Convert(utf8, win1251, win1251.GetBytes(s) ) |> win1251.GetString

    let getUserNameLazy = lazy (
        let res = this.RunMethod "users.get" []
        first_name <- res.SelectSingleNode("/response/user/first_name").InnerText |> decode
        last_name <- res.SelectSingleNode("/response/user/last_name").InnerText |> decode
        first_name + " " + last_name 
    )

    member this.setAuthData(s:string) = 
        let parts = s.Substring(1).Split('&');
        access_token <- parts.[0].Split('=').[1]
        user_id <- parts.[2].Split('=').[1] 

    member this.Auth (wb:WebBrowser) = 
        let url = "https://oauth.vk.com/authorize?client_id=" + APP_ID 
                + "&scope=8&redirect_uri=" + "https://oauth.vk.com/blank.html" + "&display=page&v=3.0&response_type=token"
        wb.Navigate url
        ()

    member this.RunMethodStr name (lParams: list<string>) =
        let mutable url = API_METHOD_URL + name + ".xml?access_token=" + access_token + "&v=3.0"
        for p in lParams do
            url <- url + "&" + p
            ()
        webClient.DownloadString url

    member this.RunMethod (name:string) (lParams: list<string>) : XmlDocument =
        let Doc = new XmlDocument() 
        this.RunMethodStr name lParams |> Doc.LoadXml
        Doc

    member this.getUserName() =
        getUserNameLazy.Force()

    member this.getAlbums() = 
        let res = new ResizeArray<albumInfo>()
        let albumsRes = this.RunMethod "audio.getAlbums" ["owner_id=" + user_id; "offset=0"; "count=100"]
        let nodes = albumsRes.SelectNodes("/response/album")
        
        for v in nodes do
            res.Add ({album_id = v.SelectSingleNode("album_id").InnerText;
                title = v.SelectSingleNode("title").InnerText |> decode
            })
        res
    member this.AudioGet(alb) = 
        let songsRes = this.RunMethod "audio.get" ["owner_id=" + user_id; "album_id=" + alb.album_id; "need_user=0"; "count=6000"]
        let res = new ResizeArray<songInfo>();
        let nodes = songsRes.SelectNodes("/response/audio")
        for v in nodes do
            let song = { 
                id = v.SelectSingleNode("aid").InnerText;
                title = v.SelectSingleNode("title").InnerText;
                artist = v.SelectSingleNode("artist").InnerText;
                url = v.SelectSingleNode("url").InnerText;
                albumFolder = alb.title;
            }
            res.Add(song)   
        res

let alert s = 
    MessageBox.Show s

let player = new WMPLib.WindowsMediaPlayerClass();
//player.URL


type MainForm() as form = 
    inherit Form() 
    let syncLabel = new Label(Text = "", Left = 130, Width = 400, Top = 40, Visible = false)
    let mainMenu = new MainMenu()
    let mnuFile = new MenuItem(Text = "File")
    let mnuExit = new MenuItem(Text = "E&xit")
    let mnuSynchronize = new MenuItem("Synchronize")
    let mnuAccountExit = new MenuItem(Text = "Account Exit")
    let mnuProcessList = new MenuItem(Text = "Get albums")
    let browser = new WebBrowser()
    let webClient = new WebClient() 
    let vkHelper = new VkHelper(webClient)
    let (checkboxes : Control list ref) = ref []
    let mutable savePath = "C:\\Users\\Admin\\Desktop\\Удалять можно\\vk_audio"
    let mutable currentAlbumTitle = ""
    let mutable downloadedCnt = 0
    let mutable songList = new ResizeArray<songInfo>()
    let mutable albumList = new ResizeArray<albumInfo>()
    let mutable accountExitDocumentCompletedHandler = null
    let mutable browserAuthDocumentCompletedHandler = null
    let mutable logoutState = "none"

    do form.InitializeForm()

    member this.InitializeForm() = 
        this.FormBorderStyle <- FormBorderStyle.Sizable
        this.Height <- 400
        this.Width <- 600
        this.AutoScroll <- true
        this.Controls.AddRange([| browser :> Control;
            syncLabel :> Control;
        |]);
        mnuProcessList.Click.AddHandler(new EventHandler(this.mnuProcessListClick) )
        mnuAccountExit.Click.AddHandler(new EventHandler(this.mnuAccountExitClick) )
        mnuExit.Click.Add(fun _ -> this.Close() )
        mnuSynchronize.Click.Add(fun _ ->
            this.syncProgressSet "Synchronization started"
            mnuSynchronize.Enabled <- false
            for v in !checkboxes do 
                v.Enabled <- false
            Async.StartAsTask(async { this.downloadAlbumSongsList(0) } ) |> ignore
        )
        mnuFile.MenuItems.AddRange([| mnuAccountExit; mnuExit |])
        mainMenu.MenuItems.AddRange([| mnuFile; mnuProcessList; mnuSynchronize |])
        this.Menu <- mainMenu

        browser.Width <- this.ClientSize.Width
        browser.Height <- this.ClientSize.Height

        webClient.DownloadProgressChanged.Add(fun args ->
            let perc = args.ProgressPercentage.ToString()
            "Progress (" + currentAlbumTitle + "): " + downloadedCnt.ToString() + "/" 
                + songList.Count.ToString() + ", file: " + perc + "%"
            |> this.syncProgressSet
        )
        accountExitDocumentCompletedHandler <- new WebBrowserDocumentCompletedEventHandler(
            this.accountExitDocumentCompleted1) 
        browserAuthDocumentCompletedHandler <- new WebBrowserDocumentCompletedEventHandler(
            this.browserAuthDocumentCompleted)
        browser.DocumentCompleted.AddHandler(browserAuthDocumentCompletedHandler)
        vkHelper.Auth browser 
      

    member this.mnuAccountExitClick s e =
        logoutState <- "start"
        for c in this.Controls do 
            c.Hide()
        browser.Show()
        //browser.DocumentCompleted.RemoveHandler(accountExitDocumentCompletedHandler) 
        browser.DocumentCompleted.AddHandler(accountExitDocumentCompletedHandler)
        browser.Navigate("https://vk.com/");
        
    member this.accountExitDocumentCompleted1 s e =
        let link = browser.Document.GetElementById("logout_link")
        if logoutState = "start" then
            logoutState <- "logout_clicked"
            link.InvokeMember("click") |> ignore
        elif logoutState = "logout_clicked" then
            logoutState <- "completed"
            browser.DocumentCompleted.RemoveHandler(accountExitDocumentCompletedHandler)
            vkHelper.Auth browser
        else
            browser.DocumentCompleted.RemoveHandler(accountExitDocumentCompletedHandler)
         
    member this.browserAuthDocumentCompleted s e = 
        let frag = browser.Url.Fragment
        if (frag.Length > 1) then
            vkHelper.setAuthData frag 
            browser.Url <- new Uri("about:blank")
            browser.DocumentCompleted.RemoveHandler(browserAuthDocumentCompletedHandler)
            for c in this.Controls do
                c.Show();
            browser.Hide();
            this.syncProgressSet ("Authorized as: " + vkHelper.getUserName() )

    member this.downloadAlbumList() =
        albumList.Clear();
        songList.Clear();
        downloadedCnt <- 0
        let CH = ref -20

        albumList <- vkHelper.getAlbums()
        checkboxes := [
            for v in albumList do
                CH := !CH + 23
                yield (new CheckBox(Checked = false, Text = v.title
                     ,Tag = v.album_id, Top = !CH, Width = 120) :> Control)
            ]
        ()
        this.Invoke(new FormDelegate
            (fun x -> 
                this.Controls.AddRange(x :?> Control[]) |> ignore
            ), List.toArray !checkboxes) |> ignore
        //Async.StartAsTask(async { this.downloadAlbumSongsList(0) } ) |> ignore
    
    member this.downloadAlbumSongsList(number) = 
        if number >= albumList.Count then
            Async.StartAsTask(async { this.downloadSingleSong(0) } ) |> ignore
        else
            let cb = (!checkboxes).[number] :?> CheckBox
            if cb.Checked then 
                let alb = albumList.[number]
                songList <- vkHelper.AudioGet(alb)    
            Async.StartAsTask(async { this.downloadAlbumSongsList(number + 1) } ) |> ignore

    member this.downloadSingleSong(number) =
        if number >= songList.Count then
            this.syncProgressSet "Finished"
            this.Invoke(new Action<_>(fun (l:Control list, mnuSynchronize: MenuItem) ->
                mnuSynchronize.Enabled <- true
                for v in l do 
                    (v :?> CheckBox).Enabled <- true
                ()
            ), (!checkboxes, mnuSynchronize)) |> ignore
        else
            let song = songList.[number] 
            let data = webClient.DownloadData(song.url)
            
            let rec clearName (s : string) = 
                if(s.Length < 1) then ""
                elif (Char.IsLetterOrDigit s.[0] || s.[0] = ' ') then s.[0].ToString() + clearName(s.Substring(1))
                else clearName(s.Substring(1) )
                    
            let filename = clearName song.artist + " - " + clearName song.title + ".mp3"
            if not(Directory.Exists(savePath + "\\" + song.albumFolder)) then
                Directory.CreateDirectory(savePath + "\\" + song.albumFolder) |> ignore
            let filefull = savePath + "\\" + song.albumFolder + "\\" + filename 
            let file = new FileStream(filefull, FileMode.Create)
            file.Write(data, 0, data.Length)
            file.Close()
            downloadedCnt <- downloadedCnt + 1
            let labelText = "Progress: " + downloadedCnt.ToString() + "/" + songList.Count.ToString() + "(handled file: " + filename + ")"
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
        folder.ShowDialog() |> ignore
        savePath <- folder.SelectedPath

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