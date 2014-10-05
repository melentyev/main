module Main

open System 
open System.Drawing 
open System.Windows.Forms 
open System.Web.Helpers
open System.Net
open System.Xml
open System.IO
open System.Collections.Generic
open System.Text
open System.Configuration
open System.Threading

type songInfo = { id:string; artist: string; title:string; url: string; 
    albumFolder: string; album:string; filefull: string; lvItem: ListViewItem option }
type albumInfo = { album_id: string; title: string; lvItem: ListViewItem option }


let alert s = MessageBox.Show s

type updatableLW() as lw =
    inherit ListView()
    //do lw.SetStyle(ControlStyles.UserPaint,true)
    do lw.SetStyle(ControlStyles.OptimizedDoubleBuffer, true)
    do lw.SetStyle(ControlStyles.AllPaintingInWmPaint, true)
    do lw.SetStyle(ControlStyles.Opaque, true)
    do lw.DoubleBuffered <- true
    override this.OnPaint(e) =
        let c = 5
        base.OnPaint(e)
        //this.BeginUpdate()
        //this.EndUpdate()

type VkHelper(webClient: WebClient, savePath:string, namingStyle:string) as vkHelper = 
    let APP_ID = "3916880"
    let API_METHOD_URL = "https://api.vk.com/method/"
    let mutable user_id = ""
    let mutable access_token = ""
    let mutable full_name = ""
    let mutable first_name = ""
    let mutable last_name = ""
    let utf8 = Encoding.GetEncoding("utf-8");
    let win1251 = Encoding.GetEncoding("windows-1251");
    let mutable savePath = savePath
    let mutable namingStyle = namingStyle
    let rec clearName (s : string) = 
        if(s.Length < 1) then ""
        elif (Char.IsLetterOrDigit s.[0] || s.[0] = ' ') then s.[0].ToString() + clearName(s.Substring(1))
        else clearName(s.Substring(1) )
    let decode (s:string) = 
        Encoding.Convert(utf8, win1251, win1251.GetBytes(s) ) |> win1251.GetString

    member this.SavePath 
        with get() = savePath
        and set(value) = savePath <- value
    member this.NamingStyle 
        with get() = namingStyle
        and set(value) = namingStyle <- value

    member this.getUserNameLazy = lazy (
        let res = vkHelper.RunMethod "users.get" []
        first_name <- res.SelectSingleNode("/response/user/first_name").InnerText |> decode
        last_name <- res.SelectSingleNode("/response/user/last_name").InnerText |> decode
        first_name + " " + last_name 
    )

    member this.setAuthData(s:string) = 
        let parts = s.Substring(1).Split('&');
        access_token <- parts.[0].Split('=').[1]
        user_id <- parts.[2].Split('=').[1] 

    member this.Auth (wb:WebBrowser) = 
        let scope = 8 + 4096
        wb.Invoke(new Action<_>(fun _ ->
            wb.Show()
            "https://oauth.vk.com/authorize?client_id=" + APP_ID 
                    + "&scope=" + scope.ToString()+ "&redirect_uri=" 
                    + "https://oauth.vk.com/blank.html" + "&display=page&v=3.0&response_type=token"
            |> wb.Navigate
        ), [null]) |> ignore

    member this.RunMethodStr name (lParams: list<string>) =
        API_METHOD_URL + name + ".xml?access_token=" + access_token + "&v=3.0" + List.fold 
            (fun s p -> s + "&" + p) "" lParams |> webClient.DownloadString

    member this.RunMethod (name:string) (lParams: list<string>): XmlDocument =
        let Doc = new XmlDocument() 
        this.RunMethodStr name lParams |> Doc.LoadXml
        Doc

    member this.getUserName() =
        this.getUserNameLazy.Force()

    member this.getAlbums() = 
        (this.RunMethod "audio.getAlbums" 
            ["owner_id=" + user_id; "offset=0"; "count=100"]).SelectNodes("/response/album") 
        |> Seq.cast<XmlNode>
        |> Seq.fold (fun lst v -> 
            ({album_id = v.SelectSingleNode("album_id").InnerText;
             title = v.SelectSingleNode("title").InnerText |> decode;
             lvItem = None; }) :: lst) []
        |> List.rev
        
    member this.AudioGet(alb) = 
        (this.RunMethod "audio.get" ["owner_id=" + user_id;
            "album_id=" + alb.album_id; "need_user=0"; "count=6000"]).SelectNodes("/response/audio")
        |> Seq.cast<XmlNode>
        |> Seq.fold (fun lst v -> 
            let artist = v.SelectSingleNode("artist").InnerText |> decode
            let title =  v.SelectSingleNode("title").InnerText |> decode
            let filename = 
                match namingStyle with
                | "artist_song" -> clearName artist + " - " + clearName title + ".mp3"
                | "album_song" -> clearName alb.title + " - " + clearName title + ".mp3"
                | _ -> clearName title + ".mp3"

            let albumFolder = savePath + "\\" + clearName alb.title
            { 
                id = v.SelectSingleNode("aid").InnerText;
                title = title;
                artist = artist;
                album = alb.title
                url = v.SelectSingleNode("url").InnerText;
                albumFolder =  albumFolder;
                filefull = albumFolder + "\\" + filename;
                lvItem = None;
            } :: lst) []

and MainForm() as form = 
    inherit Form(AutoScroll = true, ClientSize = new Size(740, 480), 
        FormBorderStyle = FormBorderStyle.Sizable)
     
    let mutable config = ConfigurationManager.OpenExeConfiguration(ConfigurationUserLevel.None)
    do if config.AppSettings.Settings.["savePath"] = null then 
        config.AppSettings.Settings.Add("savePath", "") |> ignore
    do if config.AppSettings.Settings.["namingStyle"] = null then 
        config.AppSettings.Settings.Add("namingStyle", "artist_song") |> ignore
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
    let mutable _vkHelper = 
        new VkHelper (webClient
            , config.AppSettings.Settings.["savePath"].Value
            , config.AppSettings.Settings.["namingStyle"].Value)
    let mutable songQueue: songInfo list = []
    let mutable songProcessing: songInfo list = []
    let mutable queueLock = new Object()
    let mutable albumList = []
    let mutable accountExitDocumentCompletedHandler = null
    let mutable browserAuthDocumentCompletedHandler = null
    let mutable logoutState = "none"
    let mutable settingsForm = null
    let mutable downloadThreadsCount = 3 
    do form.InitializeForm()

    member this.vkHelper
        with get() = _vkHelper
        and set v = _vkHelper <- v
    member this.Config = config
    member this.InitializeForm() = 
        new EventHandler(this.mnuAccountExitClick) |> mnuAccountExit.Click.AddHandler
        new EventHandler(fun _ _ -> 
            Async.Start(async { this.downloadAlbumList() } ) ) |> mnuProcessList.Click.AddHandler
        new EventHandler(fun _ _ -> 
            (unbox<SettingsForm> settingsForm).Show()) |> mnuSettings.Click.AddHandler
        mnuExit.Click.Add(fun _ -> this.Close() )
        mnuSynchronize.Click.Add(fun _ ->
            this.syncProgressSet "Synchronization started"
            mnuSynchronize.Enabled <- false
            albumListView.Enabled <- false
            this.downloadAlbumSongsList()
        )
        let tmpMnu = new MenuItem()
        //tmpMnu.Click.Add(fun _ -> alert(this.vkHelper.RunMethodStr("messages.get") ([]) |> this.vkHelper.decode ) |> ignore );
        mnuFile.MenuItems.AddRange([| mnuAccountExit; mnuExit; tmpMnu |])
        mainMenu.MenuItems.AddRange([| mnuFile; mnuProcessList; mnuSynchronize; mnuSettings |])
        this.Menu <- mainMenu
        
        let statusHeight = statusBar.Height
        browser.AllowNavigation <- true
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
        
        downloadQueueListView.Columns.AddRange
            ([| new ColumnHeader(Text = "Song", Width=200)
                ; new ColumnHeader(Text = "Artist", Width = 100)
                ; new ColumnHeader(Text = "Album", Width = 100)
                ; new ColumnHeader(Text = "File", Width = 100)
                ; new ColumnHeader(Text = "Status", Width = 100)
            |])
        downloadQueueListView.Width <- splitLayout1.Panel2.ClientSize.Width
        downloadQueueListView.Height <- splitLayout1.Panel2.ClientSize.Height

        downloadQueueContextMenu.MenuItems.AddRange(
            [|
                let item = new MenuItem(Text = "Process queue")
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

        settingsForm <- new SettingsForm(form) |> box
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
        mnuProcessList.Enabled <- false
        mnuSynchronize.Enabled <- false
        browser.DocumentCompleted.AddHandler(accountExitDocumentCompletedHandler)
        browser.Navigate("https://vk.com/");
        
    member this.accountExitDocumentCompleted1 _ _ =
        let link = browser.Document.GetElementById("logout_link")
        if logoutState = "start" then
            logoutState <- "logout_clicked"
            link.InvokeMember("click") |> ignore
        elif logoutState = "logout_clicked" then
            logoutState <- "completed"
            browser.DocumentCompleted.RemoveHandler(accountExitDocumentCompletedHandler)
            Async.StartAsTask( async { 
                Thread.Sleep(1000); 
                this.vkHelper.Auth browser
            } ) |> ignore
        else
            browser.DocumentCompleted.RemoveHandler(accountExitDocumentCompletedHandler)
           
    member this.browserAuthDocumentCompleted s e = 
        let frag = browser.Url.Fragment
        if (frag.Length > 1) then
            this.vkHelper.setAuthData frag 
            browser.Url <- new Uri("about:blank")
            browser.DocumentCompleted.RemoveHandler(browserAuthDocumentCompletedHandler)
            splitLayout1.Visible <- true
            browser.Visible <- false
            [ mnuAccountExit; mnuSynchronize; mnuProcessList ] |> List.iter (fun c -> c.Enabled <- true) 
            this.syncProgressSet ("Authorized as: " + this.vkHelper.getUserName() )

    member this.downloadAlbumList() =
        albumList <- List.empty
        songQueue <- List.empty

        albumList <- this.vkHelper.getAlbums()
        albumListView.Invoke(new Action<_>(fun _ ->
            albumListView.Items.Clear()
        ), [null]) |> ignore
        albumList <- List.map (fun v -> 
            let item = new ListViewItem(Text = v.title, Tag = v.album_id, Checked = false)
            albumListView.Invoke(new Action<_>(fun (item: ListViewItem) ->
                albumListView.Items.Add(item) |> ignore
            ), item ) |> ignore
            { v with lvItem = Some(item) } ) albumList
  
    member this.downloadAlbumSongsList() = 
        let rec loop lst = async {
            match lst with 
            | alb :: tail ->
                albumListView.Invoke(new Action<_>(fun _ ->
                    let cb = alb.lvItem.Value
                    if cb.Checked then 
                        let songs = this.vkHelper.AudioGet(alb)
                        songQueue <- (List.map(fun v -> 
                            { v with lvItem = Some(
                                               new ListViewItem(
                                                 [| v.title; v.artist; v.album; v.filefull; "Queued"|], -1 ) ) }
                            ) songs) @ songQueue    
                    
                ), [null]) |> ignore 
                return! loop tail
            | [] -> 
                downloadQueueListView.Invoke(new Action<_>(fun _ ->
                    List.map (fun (v: songInfo) -> 
                            v.lvItem.Value
                        ) songQueue |> List.toArray |> downloadQueueListView.Items.AddRange
                ), [null]) |> ignore
                this.processSongQueue()
        }
        Async.Start (loop albumList) 
            
    member this.DownloadSong () = 
        lock queueLock <| fun () ->
            match songQueue with
            | song :: tail -> 
                songQueue <- tail
                songProcessing <- song :: songProcessing
                let wc = new WebClient()
                wc.DownloadDataCompleted.Add <| fun args ->
                    lock queueLock <| fun () ->
                        songProcessing <- List.filter (fun x -> x <> song) songProcessing 
                        if not args.Cancelled then                         
                            if not <| Directory.Exists(song.albumFolder)  then
                                Directory.CreateDirectory song.albumFolder |> ignore
                            File.WriteAllBytes(song.filefull, args.Result)
                            if songQueue.IsEmpty then 
                                if songProcessing.IsEmpty then 
                                    this.syncProgressSet "Finished"
                                    this.Invoke(new Action<_>(fun _ ->
                                        mnuSynchronize.Enabled <- true
                                        albumListView.Enabled <- true
                                    ), None) |> ignore  
                                else ()
                            else this.DownloadSong()
                        this.Invoke(new Action<_>(fun _ ->
                            if not args.Cancelled then 
                                song.lvItem.Value.Remove()
                        ), None) |> ignore
                
                wc.DownloadProgressChanged.Add <| fun args ->
                    this.Invoke(new Action<_>(fun _ ->
                        song.lvItem.Value.SubItems.[4].Text <- 
                            args.ProgressPercentage.ToString() + "%"
                    ), None) |> ignore

                wc.DownloadDataAsync(new Uri(song.url) )
            | [] -> ()
        
    member this.processSongQueue() =
        for i = 1 to Math.Min(downloadThreadsCount, songQueue.Length) do
            Async.Start (async { 
                this.DownloadSong () 
            })

    member this.syncProgressSet (s) =
        statusBar.Invoke(new Action< string >(fun a -> 
            statusBar.Items.Clear()
            statusBar.Items.Add(a) |> ignore
        ), s) |> ignore

and SettingsForm(mainForm) as settingsForm =
    inherit Form(Visible = false, Text = "Settings", ClientSize = new Size(503, 190)
        , FormBorderStyle = FormBorderStyle.FixedDialog)
    let textBoxSelectSavePath = new TextBox(Location = new Point(10, 10)
        , Width = 400, Text = mainForm.vkHelper.SavePath )
    let btnSelectSavePath = new Button(Location = new Point(420, 10), Text = "Browse...")
    let btnOk = new Button(Text = "OK"
        , Location = new Point(333, settingsForm.ClientSize.Height - 30) )
    let btnCancel = new Button(Text = "Cancel"
        , Location = new Point(420, settingsForm.ClientSize.Height - 30) )
    let (radioNamingStyles, _) =
        List.fold
            (fun (lst: Control list, height) (rb:RadioButton) -> 
                rb.Location <- new Point(10, 62 + height)
                rb.Width <- 300
                rb.Checked <- mainForm.Config.AppSettings.Settings.["namingStyle"].Value = (rb.Tag :?> string)
                ( (rb :> Control) :: lst, height + 26)
            ) ([], 0)
            [ new RadioButton(Text = "<Artist> - <Song>.mp3", Tag="artist_song");
                new RadioButton(Text = "<Album> - <Song>.mp3", Tag="album_song");
                new RadioButton(Text = "<Song>.mp3", Tag="song"); ] 
    let labelNamingStyles = new Label(Text = "File naming style:", Location=new Point(10, 40) ) 
    do btnSelectSavePath.Click.Add(settingsForm.btnSelectSavePathClick)
    do btnOk.Click.Add <| fun args ->
        mainForm.vkHelper.SavePath <- textBoxSelectSavePath.Text
        mainForm.Config.AppSettings.Settings.["savePath"].Value <- mainForm.vkHelper.SavePath
        mainForm.vkHelper.NamingStyle <- textBoxSelectSavePath.Text
        mainForm.Config.AppSettings.Settings.["namingStyle"].Value <- 
            (List.find (fun (c:Control) -> (c :?> RadioButton).Checked) radioNamingStyles).Tag |> string
        mainForm.Config.Save()
        settingsForm.Hide()
    do btnCancel.Click.Add <| fun args -> 
        textBoxSelectSavePath.Text <- mainForm.vkHelper.SavePath 
        settingsForm.Hide() 
    do settingsForm.Controls.AddRange <| Array.concat [ 
        [|
            textBoxSelectSavePath;
            btnSelectSavePath;
            labelNamingStyles;
            btnOk;
            btnCancel;
        |]; List.toArray radioNamingStyles]
    do settingsForm.Closing.Add(fun a -> settingsForm.Hide(); a.Cancel <- true )
    member this.btnSelectSavePathClick args = 
        let folder = new FolderBrowserDialog()
        folder.ShowDialog() |> ignore
        textBoxSelectSavePath.Text <- folder.SelectedPath

let debugForm = new Form()
let output = new RichTextBox(Width = debugForm.ClientSize.Width
    , Height = debugForm.ClientSize.Height)
debugForm.Controls.Add(output)


[<STAThread>] 
do Application.Run(new MainForm() ) 
