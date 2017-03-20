// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open Expecto
open ExecutionEngineTests.Create
open ExecutionEngineTests.Insert
open ExecutionEngineTests.Delete
open ExecutionEngineTests.Select


[<Tests>]
let tests = printfn "Tests go under this attribute"
//Probably best to make an individual file with your tests in and call it from here - that way you have a test file for each module.

[<EntryPoint>]
let main argv = 
    printfn "Ad-hoc tests to verify ExecutionEngine.Create"
    runTests defaultConfig testExEngineCreate |> ignore
    printfn "Random tests to verify ExecutionEngine.Create"
    runTests defaultConfig propertiesExEngineCreate |> ignore
    printfn "Ad-hoc tests to verify ExecutionEngine.Insert"
    runTests defaultConfig testExEngineInsert |> ignore
    printfn "Random tests to verify ExecutionEngine.Insert"
    runTests defaultConfig propertiesExEngineInsert |> ignore
    printfn "Ad-hoc tests to verify ExecutionEngine.Delete"
    runTests defaultConfig testExEngineDelete |> ignore
    printfn "Random tests to verify ExecutionEngine.Delete"
    runTests defaultConfig propertiesExEngineDelete |> ignore
    printfn "Ad-hoc tests to verify ExecutionEngine.Select"
    runTests defaultConfig testExEngineSelect |> ignore
    printfn "Random tests to verify ExecutionEngine.Select"
    runTests defaultConfig propertiesExEngineSelect |> ignore
 
    printfn "Press any key to EXIT"
    
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
