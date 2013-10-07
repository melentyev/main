open System
open System.IO
open System.Windows.Forms
open Printf
open System.Net

let form = new Form(Text="My First F# Form", Visible=true)

let menu = form.Menu <- new MainMenu()
let mnuFile = form.Menu.MenuItems.Add("&File")
let filter = "txt files (*.txt)|*.txt|All files (*.*)|*.*"

let mnuiOpen =
    new MenuItem("&Open...",
        new EventHandler(fun _ _ ->
            let
        ),
        Shortcut.CtrlO)

mnuFile.MenuItems.Add(mnuiOpen)

[<STAThread>]
do Application.Run(form)