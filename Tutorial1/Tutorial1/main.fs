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
open System.Configuration
 
type songInfo = { id:string; artist: string; title:string; url: string; 
    albumFolder: string; album:string; filefull: string; }
type albumInfo = { album_id: string; title: string; }

type VkHelper(webClient: WebClient, savePath:string) as this = 
    let APP_ID = "3916880"
    let API_METHOD_URL = "https://api.vk.com/method/"
    let mutable user_id = ""
    let mutable access_token = ""
    let mutable full_name = ""
    let mutable first_name = ""
    let mutable last_name = ""
    let utf8 = Encoding.GetEncoding("utf-8");
    let win1251 = Encoding.GetEncoding("windows-1251");
    let mutable _savePath = savePath
        
    

    let rec clearName (s : string) = 
        if(s.Length < 1) then ""
        elif (Char.IsLetterOrDigit s.[0] || s.[0] = ' ') then s.[0].ToString() + clearName(s.Substring(1))
        else clearName(s.Substring(1) )

    let decode (s:string) = 
        Encoding.Convert(utf8, win1251, win1251.GetBytes(s) ) |> win1251.GetString

    let getUserNameLazy = lazy (
        let res = this.RunMethod "users.get" []
        first_name <- res.SelectSingleNode("/response/user/first_name").InnerText |> decode
        last_name <- res.SelectSingleNode("/response/user/last_name").InnerText |> decode
        first_name + " " + last_name 
    )

    member this.savePath 
        with get() = _savePath
        and set(s) = do _savePath <- s

    member this.setAuthData(s:string) = 
        let parts = s.Substring(1).Split('&');
        access_token <- parts.[0].Split('=').[1]
        user_id <- parts.[2].Split('=').[1] 

    member this.Auth (wb:WebBrowser) = 
        wb.Show();
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
            let artist = v.SelectSingleNode("artist").InnerText
            let title =  v.SelectSingleNode("title").InnerText
            let filename = clearName artist + " - " + clearName title + ".mp3"
            let albumFolder = _savePath + "\\" + clearName alb.title
            let song = { 
                id = v.SelectSingleNode("aid").InnerText;
                title = title;
                artist = artist;
                album = alb.title
                url = v.SelectSingleNode("url").InnerText;
                albumFolder =  albumFolder;
                filefull = albumFolder + "\\" + filename;
            }
            res.Add(song)   
        res

let alert s = 
    MessageBox.Show s

type updatableLW =
    inherit ListView
    new () = {
        inherit ListView()
    }
    override this.InitLayout() =
        base.InitLayout()
        this.SetStyle(ControlStyles.OptimizedDoubleBuffer, true)
        let k = 5
        ()

type MainForm() as form = 
    inherit Form()
     
    [<DefaultValue>]
    val mutable config:Configuration
    do form.config <- ConfigurationManager.OpenExeConfiguration(ConfigurationUserLevel.None)
    do if form.config.AppSettings.Settings.["savePath"] = null then 
        form.config.AppSettings.Settings.Add("savePath", "") |> ignore
    let anchorAll = (AnchorStyles.Left ||| AnchorStyles.Right 
        ||| AnchorStyles.Bottom ||| AnchorStyles.Top)
    let syncLabel = new Label(Text = "", Left = 130, Width = 400, Top = 40, Visible = false)
    let mainMenu = new MainMenu()
    let mnuFile = new MenuItem(Text = "File")
    let mnuExit = new MenuItem(Text = "E&xit")

    let mnuSynchronize = new MenuItem(Text = "Synchronize", Enabled = false)
    let mnuAccountExit = new MenuItem(Text = "Account Exit", Enabled = false)
    let mnuProcessList = new MenuItem(Text = "Get albums", Enabled = false)
    let mnuSettings = new MenuItem(Text = "Settings...")

    let albumListView = new ListView(CheckBoxes = true, FullRowSelect = true
            , HideSelection = false, View = View.Details, Scrollable = true
            , Anchor = anchorAll, BorderStyle = BorderStyle.None
            , Location = new Point(0, 0) )
    let downloadQueueListView = new updatableLW(FullRowSelect = true
            , HideSelection = false, View = View.Details, Scrollable = true
            , Anchor = anchorAll, BorderStyle = BorderStyle.None
            , Location = new Point(0, 0) )
    let splitLayout1 = new SplitContainer(BorderStyle = BorderStyle.Fixed3D
            , Location = new Point(0, 0), Anchor = anchorAll, Visible = false)

    let downloadQueueContextMenu = new ContextMenu()
    let statusBar = new StatusStrip(Visible = true, Text = "abc")
    let browser = new WebBrowser(Visible = false)
    let webClient = new WebClient() 
    let downloadClient = new WebClient()
    let mutable _vkHelper = new VkHelper(webClient, form.config.AppSettings.Settings.["savePath"].Value)
    let (checkboxes : Control list ref) = ref []
    let mutable currentAlbumTitle = ""
    let mutable currentSongNumber = 0
    let mutable downloadedCnt = 0
    let mutable songList = new ResizeArray<songInfo>()
    let mutable albumList = new ResizeArray<albumInfo>()
    let mutable accountExitDocumentCompletedHandler = null
    let mutable browserAuthDocumentCompletedHandler = null
    let mutable logoutState = "none"
    let mutable (settingsForm:SettingsForm option) = None 
    do form.InitializeForm()

    member this.vkHelper
        with get() = _vkHelper
        and set v = _vkHelper <- v

    member this.InitializeForm() = 
        this.FormBorderStyle <- FormBorderStyle.Sizable
        this.ClientSize <- new Size(740, 480)
        this.AutoScroll <- true
        settingsForm <- Some(new SettingsForm(form)) 
        
        mnuProcessList.Click.AddHandler(new EventHandler(this.mnuProcessListClick) )
        mnuAccountExit.Click.AddHandler(new EventHandler(this.mnuAccountExitClick) )
        mnuSettings.Click.AddHandler(new EventHandler(this.mnuSettingsClick) )
        mnuExit.Click.Add(fun _ -> this.Close() )
        mnuSynchronize.Click.Add(fun _ ->
            this.syncProgressSet "Synchronization started"
            mnuSynchronize.Enabled <- false
            albumListView.Enabled <- false
            Async.StartAsTask(async { this.downloadAlbumSongsList(0) } ) |> ignore
        )
        mnuFile.MenuItems.AddRange([| mnuAccountExit; mnuExit |])
        mainMenu.MenuItems.AddRange([| mnuFile; mnuProcessList; mnuSynchronize; mnuSettings |])
        this.Menu <- mainMenu
        
        let statusHeight = statusBar.Height
        browser.Width <- this.ClientSize.Width - 0
        browser.Height <- this.ClientSize.Height - 23
        browser.ScriptErrorsSuppressed <- true
        splitLayout1.Width <- this.ClientSize.Width
        splitLayout1.Height <- this.ClientSize.Height - statusHeight
        splitLayout1.Orientation <- Orientation.Horizontal

        albumListView.Columns.AddRange([| new ColumnHeader(Text ="Album", Width=200) |])
        albumListView.Width <- splitLayout1.Panel1.ClientSize.Width
        albumListView.Height <- splitLayout1.Panel1.ClientSize.Height
        splitLayout1.Panel1.Controls.Add(albumListView :> Control)
        
        downloadQueueListView.Columns.AddRange([| new ColumnHeader(Text = "Song", Width=200);
            new ColumnHeader(Text = "Artist", Width = 100);
            new ColumnHeader(Text = "Album", Width = 100);
            new ColumnHeader(Text = "File", Width = 100);
            new ColumnHeader(Text = "Status", Width = 100);
        |])
        downloadQueueListView.Width <- splitLayout1.Panel2.ClientSize.Width
        downloadQueueListView.Height <- splitLayout1.Panel2.ClientSize.Height

        downloadQueueContextMenu.MenuItems.AddRange([|
            let item = new MenuItem(Text = "Process queue")
            //item.Click
            yield item;
            let item = new MenuItem(Text = "Remove selected")
            item.Click.Add(fun _ -> 
                if(downloadQueueListView.Items.[0].Selected) then
                    downloadClient.CancelAsync()
                for v in downloadQueueListView.Items do 
                    if v.Selected then downloadQueueListView.Items.Remove(v)
                
                ()
            )
            yield item;
        |])
        downloadQueueListView.ContextMenu <- downloadQueueContextMenu

        splitLayout1.Panel2.Controls.Add(downloadQueueListView :> Control)
        
        this.Controls.AddRange([| browser :> Control;
            syncLabel :> Control;
            statusBar :> Control;
            splitLayout1 :> Control;
        |]);

        
        //this.DoubleBuffered <- true
        downloadClient.DownloadProgressChanged.Add(fun args ->
            let perc = args.ProgressPercentage.ToString()
            //downloadQueueListView.BeginUpdate()
            if (downloadQueueListView.Items.Count > 0 && (args.ProgressPercentage + 1) % 5 = 0 
                && downloadQueueListView.Items.[0].SubItems.[4].Text <> perc + "%") then 
                downloadQueueListView.Items.[0].SubItems.[4].Text <- perc + "%"
            
            //downloadQueueListView.EndUpdate()
            "Progress (" + currentAlbumTitle + " - " + songList.[downloadedCnt].title + "): " + downloadedCnt.ToString() + "/" 
                + songList.Count.ToString() + ", file: " + perc + "%"
            |> this.syncProgressSet
        )
        downloadClient.DownloadDataCompleted.Add(this.singleSongDownloadedHandler)
        accountExitDocumentCompletedHandler <- new WebBrowserDocumentCompletedEventHandler(
            this.accountExitDocumentCompleted1) 
        browserAuthDocumentCompletedHandler <- new WebBrowserDocumentCompletedEventHandler(
            this.browserAuthDocumentCompleted)
        browser.DocumentCompleted.AddHandler(browserAuthDocumentCompletedHandler)
        this.vkHelper.Auth browser

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
            this.vkHelper.Auth browser
        else
            browser.DocumentCompleted.RemoveHandler(accountExitDocumentCompletedHandler)
           
    member this.browserAuthDocumentCompleted s e = 
        let frag = browser.Url.Fragment
        if (frag.Length > 1) then
            this.vkHelper.setAuthData frag 
            browser.Url <- new Uri("about:blank")
            browser.DocumentCompleted.RemoveHandler(browserAuthDocumentCompletedHandler)
            splitLayout1.Show()
            browser.Hide();
            mnuAccountExit.Enabled <- true
            mnuSynchronize.Enabled <- true
            mnuProcessList.Enabled <- true
            this.syncProgressSet ("Authorized as: " + this.vkHelper.getUserName() )

    member this.downloadAlbumList() =
        albumList.Clear();
        songList.Clear();
        downloadedCnt <- 0
        let CH = ref -20

        albumList <- this.vkHelper.getAlbums()
        checkboxes := [
            for v in albumList do
                CH := !CH + 23
                yield (new CheckBox(Checked = false, Text = v.title
                     ,Tag = v.album_id, Top = !CH, Width = 120) :> Control)
            ]
        albumListView.Invoke(new Action<_>(fun _ ->
            albumListView.Items.Clear()
        ), [null]) |> ignore
        albumListView.Items.Clear()
        for v in albumList do
            let item = new ListViewItem(Text = v.title, Tag = v.album_id, Checked = false)
            albumListView.Invoke(new Action<_>(fun (item: ListViewItem) ->
                albumListView.Items.Add(item) |> ignore
            ), item )|> ignore
  
    member this.downloadAlbumSongsList(number) = 
        if number >= albumList.Count then
            downloadQueueListView.Invoke(new Action<_>(fun _ ->
                downloadQueueListView.Items.AddRange(
                    [| for i = 0 to songList.Count - 1 do
                            let v = songList.[i] 
                            let item = new ListViewItem([| v.title; v.artist; v.album; v.filefull; "Queued"|], -1 )
                            item.Tag <- i
                            yield item |])
                this.processSongQueue()
            ), [null]) |> ignore
        else 
            albumListView.Invoke(new Action<_>(fun (number:int) ->
                let cb = albumListView.Items.[number]
                if cb.Checked then 
                    let alb = albumList.[number]
                    let songs = this.vkHelper.AudioGet(alb)
                    songList.AddRange(songs)    
                Async.StartAsTask(async { this.downloadAlbumSongsList(number + 1) } ) |> ignore
            ), number) |> ignore 
    
    member this.singleSongDownloadedHandler(args) = 
        let number = currentSongNumber
        let song = songList.[number]
        let data = args.Result

        if not(Directory.Exists(song.albumFolder)) then
            Directory.CreateDirectory(song.albumFolder) |> ignore
        let file = new FileStream(song.filefull, FileMode.Create)
        file.Write(data, 0, data.Length)
        file.Close()
        downloadedCnt <- downloadedCnt + 1
        let labelText = ("Progress: " + downloadedCnt.ToString() + "/" + songList.Count.ToString() 
            + "(handled song: " + song.title + ")")
        this.syncProgressSet labelText
        this.Invoke(new Action<_>(fun _ ->
            if(not(args.Cancelled) ) then 
                downloadQueueListView.Items.RemoveAt(0)
            this.processSongQueue() 
        ), None) |> ignore
        

    member this.processSongQueue() =
        if downloadQueueListView.Items.Count = 0 then
            this.syncProgressSet "Finished"
            this.Invoke(new Action<_>(fun (mnuSynchronize: MenuItem) ->
                mnuSynchronize.Enabled <- true
                albumListView.Enabled <- true
            ), (mnuSynchronize)) |> ignore
        else
            let number = downloadQueueListView.Items.[0].Tag :?> int
            let song = songList.[number] 
            currentSongNumber <- number
            currentAlbumTitle <- song.artist
            downloadClient.DownloadDataAsync(new Uri(song.url) );
            
    member this.syncProgressSet (s) =
        statusBar.Invoke(new Action< string >(fun a -> 
            statusBar.Items.Clear()
            statusBar.Items.Add(a) |> ignore
        ), s) |> ignore

    member this.mnuProcessListClick s args =
        Async.StartAsTask(async { this.downloadAlbumList() } ) |> ignore

    member this.mnuSettingsClick s args = 
        settingsForm.Value.Show()

and SettingsForm(mainForm) as settingsForm =
    inherit Form()
    let mainForm = mainForm
    let textBoxSelectSavePath = new TextBox(Location = new Point(10, 10)
        , Width = 400, Text = mainForm.vkHelper.savePath )
    let btnSelectSavePath = new Button(Location = new Point(420, 10), Text = "Browse...")
    let btnOk = new Button(Text = "OK", Location = new Point(333, 50) )
    let btnCancel = new Button(Text = "Cancel", Location = new Point(420, 50) )
    do btnSelectSavePath.Click.Add(settingsForm.btnSelectSavePathClick)
    do btnOk.Click.Add(settingsForm.btnOkClick)
    do btnCancel.Click.Add(settingsForm.btnCancelClick)
    do settingsForm.Visible <- false
    do settingsForm.ClientSize <- new Size(503, 80)
    do settingsForm.Text <- "Settings"
    do settingsForm.FormBorderStyle <- FormBorderStyle.FixedDialog
    do settingsForm.Controls.AddRange([|textBoxSelectSavePath;
        btnSelectSavePath;
        btnOk;
        btnCancel;
    |])
    member this.btnSelectSavePathClick args = 
        let folder = new FolderBrowserDialog()
        folder.ShowDialog() |> ignore
        textBoxSelectSavePath.Text <- folder.SelectedPath

    member this.btnOkClick args = 
        mainForm.vkHelper.savePath <- textBoxSelectSavePath.Text
        mainForm.config.AppSettings.Settings.["savePath"].Value <- mainForm.vkHelper.savePath
        mainForm.config.Save()
        this.Hide()
    
    member this.btnCancelClick args = 
        textBoxSelectSavePath.Text <- mainForm.vkHelper.savePath 
        this.Hide()

let debugForm = new Form()
let output = new RichTextBox(Width = debugForm.ClientSize.Width
    , Height = debugForm.ClientSize.Height)
debugForm.Controls.Add(output)

[<STAThread>] 
do Application.Run(new MainForm() ) 