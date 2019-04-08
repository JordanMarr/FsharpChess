namespace Chess.FabUI.WPF

open System

open Xamarin.Forms
open Xamarin.Forms.Platform.WPF

type MainWindow() = 
    inherit FormsApplicationPage()

module Main = 
    [<EntryPoint>]
    [<STAThread>]
    let main(_args) =

        let app = new System.Windows.Application()
        Forms.Init()
        let window = MainWindow(Title = "F# Chess", Height=530., Width=402.) 
        window.LoadApplication(new Chess.FabUI.App())

        app.Run(window)
