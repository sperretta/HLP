namespace Logger
open System
open System.IO

module Logger = 
    let logFile = "SharpData.log"
    let Start = File.AppendAllText(logFile, "SharpData launched at " + (System.DateTime.Now).ToString("HH:mm dd.MM.yy") + Environment.NewLine)
    let Stop = File.AppendAllText(logFile, "SharpData terminated at " + (System.DateTime.Now).ToString("HH:mm dd.MM.yy") + Environment.NewLine)
    let Log n = ""

