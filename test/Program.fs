// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Expecto
open ExecutionEngineTests.Create

[<Tests>]
let tests = printfn "Tests go under this attribute"
//Probably best to make an individual file with your tests in and call it from here - that way you have a test file for each module.

[<EntryPoint>]
let main argv = 
    runTests defaultConfig testExEngineCreate |> ignore
    runTests defaultConfig propertiesExEngineCreate |> ignore
    printfn "Press any key to EXIT"
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
