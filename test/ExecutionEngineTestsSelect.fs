namespace ExecutionEngineTests
module Select =

    open System.IO
    open System
    open ReturnControl.Main
    open databaseStructure.databaseStructure
    open ExecutionEngine.Create
    open ExecutionEngine.Insert
    open ExecutionEngine.Delete
    open EcecutionEngine.Select
    open Expecto


    [<Tests>]
    let testExEngineSelect =