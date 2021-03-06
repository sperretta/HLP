﻿namespace ExecutionEngineTests
module Select =

    open System.IO
    open System
    open ReturnControl.Main
    open databaseStructure.databaseStructure
    open ExecutionEngine.Create
    open ExecutionEngine.Insert
    open ExecutionEngine.Delete
    open ExecutionEngine.Select
    open Expecto
    open LoadSave.LoadSave

    type ColumnNames = 
        | C1
        | C2
        | C3
        | C4
        | C5

    let cList = [ C1,"Names" ; C2,"ID"; C3,"Credit"; C4,"Hex"; C5,"Member"]

    let makeConv lst =
        let m = lst |> Map.ofList
        fun x -> m.[x]

    let makeValidFunc lst =
        fun (x : ColumnNames) (y : boxData) map->
            match Map.find (makeConv lst x) map = y with
            | true -> Result true
            | _ -> Result false

    //makeConv cList C1
    //makeValidFunc cList C1 (Int None)

    [<Tests>]
    let testExEngineSelect =
     // Ad-hoc tests to verify ExecutionEngine.Select
        testList "Tests for the ExecutionEngine.Select module" [
            testCase "test 1" <| fun _ ->
                printfn "Test selecting one whole table, no options"
                let myDBRes = load @"..\..\..\src\testData.txt"
                match myDBRes with
                | Error _ -> Expect.equal ( 0) ( -1) "Could not load database"
                | Result myDB ->
                    let myDBCopy1 = select ["Subject";"Grade";"Lecturer"] ["Grades"] None None None myDB
                    delete ["Literary Characters"] None myDB |> ignore
                    Expect.equal ( string myDBCopy1) ( string (Result myDB)) "Delete / select table"
            testCase "test 2" <| fun _ ->
                printfn "Test selecting one whole table, all options"
                let myDBRes = load @"..\..\..\src\testData.txt"
                let testFunction map =
                    match Map.find "Grade" map with
                    | Int None -> Result false
                    | _ -> Result true 
                match myDBRes with
                | Error _ -> Expect.equal ( 0) ( -1) "Could not load database"
                | Result myDB ->
                    let myDBCopy1Res = select ["Subject";"Grade";"Lecturer"] ["Grades"] (Some testFunction) (Some 1) (Some 1) myDB
                    match myDBCopy1Res with
                    | Error _ -> Expect.equal ( 0) ( -1) "Select function failed."
                    | Result myDBCopy1 ->
                        let myDBCopy2 = ref INilTable
                        create "Grades" [("Subject", String None); ("Grade", Int None); ("Lecturer", String None)] myDBCopy2 |> ignore
                        insert "Grades" [] [String None; Int (Some 15); String (Some "Ms. Smith")] myDBCopy2 |> ignore
                        Expect.equal ( string myDBCopy1) ( string myDBCopy2) "Delete / select table"
        ]
    let propertiesExEngineSelect =
        // Random tests to verify ExecutionEngine.Select
        testList "FsCheck ExecutionEngine.Select" [
            testProperty "Make sure Select doesn't crash" <| fun a b c d e->
                let myDBRes = load @"..\..\..\src\testData.txt"
                match myDBRes with
                | Error _ -> Expect.equal ( 0) ( -1) "Could not load database"
                | Result myDB ->
                    let newDB = select a b c d e myDB
                    Expect.equal ( 0) ( 0) "Select did not fail"
            testProperty "Randomized tests for select" <| fun a b c d e->
                let tabName = "Literary Characters"
                let myDBRes = load @"..\..\..\src\testData.txt"
                match myDBRes with
                | Error _ -> Expect.equal ( 0) ( -1) "Could not load database"
                | Result myDB ->
                    let newDB = select [makeConv cList a] [tabName] (Some (makeValidFunc cList b c)) d e myDB
                    match newDB with
                    | Error _ -> 
                        printfn "%A" newDB
                        Expect.equal ( 0) ( -1) "Select failed"
                    | Result _ -> Expect.equal ( 0) ( 0) "Select succeeded"
            testProperty "Does not crash" <| fun a b c d e f g ->
                select a b c d e f |> ignore
                Expect.equal 0 0 "Does not crash from random inputs"
        ]