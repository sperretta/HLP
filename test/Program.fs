// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Expecto

[<Tests>]
let tests = printfn "Tests go under this attribute"
//Probably best to make an individual file with your tests in and call it from here - that way you have a test file for each module.

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
