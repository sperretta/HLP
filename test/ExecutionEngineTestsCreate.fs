namespace ExecutionEngineTests
module Create =
    
    open System.IO
    open System
    open ReturnControl.Main
    open databaseStructure.databaseStructure
    open ExecutionEngine.Create
    open Expecto

    [<Tests>]
    let testExEngineCreate =
        // Ad-hoc tests to verify ExecutionEngine.Create
        testList "Tests for the ExecutionEngine.Create module" [
            testCase "test 1" <| fun _ ->
                printfn "Test adding a table to an empty database"
                let myDB = ref INilTable
                let comp = ref (TableNode ( ref INilRow, "Small Table", [("Name", String None);("ID", Int None)], ref INilTable  ))
                create "Small Table" [("Name", String None);("ID", Int None)] myDB |> ignore
                Expect.equal (myDB) (comp) "Create a two-column table"
            testCase "test 2" <| fun _ ->
                printfn "Test adding two tables to an empty database"
                let myDB = ref INilTable
                create "Small Table" [("Name", String None);("ID", Int None)] myDB |> ignore
                create "Second Table" [("Hex ID", Byte None);("Member", Bool None);("Credit", Float None)] myDB |> ignore
                let comp2 = ref (TableNode ( ref INilRow, "Second Table", [("Hex ID", Byte None);("Member", Bool None);("Credit", Float None)], ref INilTable ))
                let comp1 = ref (TableNode ( ref INilRow, "Small Table", [("Name", String None);("ID", Int None)], comp2  ))
                Expect.equal (myDB) (comp1) "Two-table database"
            testCase "test 3" <| fun _ ->
                printfn "Test adding another table with the same name to a database"
                let myDB = ref INilTable
                create "Small Table" [("Name", String None);("ID", Int None)] myDB |> ignore
                create "Second Table" [("Hex ID", Byte None);("Member", Bool None);("Credit", Float None)] myDB |> ignore
                let fail = create "Small Table" [("Height", Float None);("Age", Int None)] myDB
                Expect.equal fail (Error "CREATE: The table name specified is already in the database.") "Attempt to add another table with the same name"
        ]
    let propertiesExEngineCreate =
        // Random tests to verify ExecutionEngine.Create
        testList "FsCheck ExecutionEngine.Create" [
            testProperty "Add a random table to an empy database" <| fun a b ->
                let db = ref INilTable
                create a b db = Result ()
            testProperty "Add three random tables to an empy database" <| fun a b c->
                let db = ref INilTable
                create "T1" a db |> ignore
                create "T2" b db |> ignore
                create "T3" c db = Result ()
]