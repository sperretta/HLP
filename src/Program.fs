open Server
open Logger

[<EntryPoint>]
let main argv = 
    Logger.Start
    Server.runServer
    Logger.Stop
    0 // return an integer exit code