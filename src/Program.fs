// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Logger

[<EntryPoint>]
let main argv = 
    //printfn "%A" argv
    Logger.Start
    ///
    Logger.Stop
    0 // return an integer exit code
