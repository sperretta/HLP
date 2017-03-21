namespace Logger
open System
open System.IO

open ReturnControl.Main

module Logger = 
    let logFile = "SharpData.log"

    let Start = File.AppendAllText(logFile, "SharpData launched at " + (System.DateTime.Now).ToString("HH:mm dd.MM.yy") + Environment.NewLine)
    let Stop = File.AppendAllText(logFile, "SharpData terminated at " + (System.DateTime.Now).ToString("HH:mm dd.MM.yy") + Environment.NewLine)

    let LogInfo (i : string ) = File.AppendAllText(logFile, (System.DateTime.Now).ToString("HH:mm") + "\t[INFO]\t" + i + Environment.NewLine)
    let LogError (e : string ) = File.AppendAllText(logFile, (System.DateTime.Now).ToString("HH:mm") + "\t[ERROR]\t" + e + Environment.NewLine)

    let LogResult (from : ReturnControl.Main.ReturnCode<'a>) =
        match from with
        | Result(res) -> sprintf "%A" res |> LogInfo
        | Error(str) -> LogError str

