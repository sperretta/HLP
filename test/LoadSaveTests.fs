namespace LoadSaveTests
module Test =
    
    open System.IO
    open System
    open ReturnControl.Main
    open databaseStructure.databaseStructure
    open LoadSave.LoadSave
    open Expecto
    let outPath1 = @"testLoadSave\saveDataFile1.txt"
    let outPath2 = @"testLoadSave\saveDataFile2.txt"
    let outPath3 = @"testLoadSave\saveDataFile3.txt"


    [<Tests>]
    let testLoadSave =
        // Ad-hoc tests to verify LoadSave
        testList "Tests for the LoadSave module" [
            testCase "test 1" <| fun _ ->
                printfn "Test loading a database"
                let myDBRes = load @"..\..\..\src\testData.txt"
                match myDBRes with
                | Error _ -> Expect.equal ( 0) ( -1) "Could not load database"
                | Result myDB -> Expect.equal ( 0) ( 0) "Database successfully loaded"
            testCase "test 2" <| fun _ ->
                printfn "Test loading, saving, and then loading a database"
                let myDBRes = load @"..\..\..\src\testData.txt"
                match myDBRes with
                | Error _ -> Expect.equal ( 0) ( -1) "Could not load database"
                | Result myDB -> 
                    save outPath1 myDB |> ignore
                    let myDBCopyRes = load outPath1
                    match myDBCopyRes with
                    | Error _ -> Expect.equal ( 0) ( -1) "Could not load saved database"
                    | Result myDBCopy -> Expect.equal (string myDB) (string myDBCopy) "Database successfully loaded"
        ]
    let propertiesLoadSave =
        // Random tests to verify LoadSave
        testList "FsCheck LoadSave" [
            testProperty "Save a random database" <| fun a ->
                match save outPath1 a with
                | Error _ -> Expect.equal (0) (-1) "Failed to save database"
                | Result _ -> Expect.equal (0) (0) "Save a database"
            testProperty "Save and then load the same database" <| fun a ->
                save outPath2 a |> ignore
                let copy = load outPath2
                match copy with
                | Error _ -> Expect.equal (0) (0) "Invalid database"
                | Result _ -> 
                    save outPath3 a |> ignore
                    Expect.equal (string (Result a)) (string copy) "Save and load a database"
        ]
