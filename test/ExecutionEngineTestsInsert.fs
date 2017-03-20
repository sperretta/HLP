namespace ExecutionEngineTests
module Insert =
    
    open System.IO
    open System
    open ReturnControl.Main
    open databaseStructure.databaseStructure
    open ExecutionEngine.Create
    open ExecutionEngine.Insert
    open Expecto


    [<Tests>]
    let testExEngineInsert =
        // Ad-hoc tests to verify ExecutionEngine.Insert
        testList "Tests for the ExecutionEngine.Insert module" [
            testCase "test 1" <| fun _ ->
                printfn "Test adding a row to an empty table"
                let myDB = ref INilTable
                let compRowTail = ref INilBox
                let compRow = ref (BoxNode ("Name", String (Some "Amy"), ref INilBox, compRowTail ) )
                compRowTail := BoxNode ("ID", Int (Some 17), compRow, ref INilBox)
                let compTab : table = ref (RowNode (compRow, ref INilRow ))
                let comp = ref (TableNode ( compTab, "Small Table", [("Name", String None);("ID", Int None)], ref INilTable  ))
                create "Small Table" [("Name", String None);("ID", Int None)] myDB |> ignore
                insert "Small Table" ["Name";"ID"] [String (Some "Amy"); Int (Some 17)] myDB |> ignore
                Expect.equal (string myDB) (string comp) "Insert a row to an empty table"
            testCase "test 2" <| fun _ ->
                printfn "Test adding two rows to an empty table"
                let myDB = ref INilTable
                let compRowTail = ref INilBox
                let compRow = ref (BoxNode ("Name", String (Some "Amy"), ref INilBox, compRowTail ) )
                compRowTail := BoxNode ("ID", Int None, compRow, ref INilBox)
                let compRowTail2 = ref INilBox
                let compRow2 = ref (BoxNode ("Name", String (Some "Rory"), ref INilBox, compRowTail ) )
                compRowTail2 := BoxNode ("ID", Int (Some 14), compRow, ref INilBox)
                let compTab2 = ref INilRow
                let compTab : table = ref (RowNode (compRow, compTab2 ))
                compTab2 := RowNode (compRow2, ref INilRow)
                let comp = ref (TableNode ( compTab, "Small Table", [("Name", String None);("ID", Int None)], ref INilTable  ))
                create "Small Table" [("Name", String None);("ID", Int None)] myDB |> ignore
                insert "Small Table" ["Name"] [String (Some "Amy")] myDB |> ignore
                insert "Small Table" [] [String (Some "Rory"); Int (Some 14)] myDB |> ignore
                Expect.equal (string myDB) (string comp) "Two-table database"
            testCase "test 3" <| fun _ ->
                printfn "Test giving faulty column names"
                let myDB = ref INilTable
                create "Small Table" [("Name", String None);("ID", Int None)] myDB |> ignore
                let fail = insert "Small Table" ["Names"] [String (Some "River")] myDB
                Expect.equal fail (Error "INSERT: At least one of the given column names don't match the given table.") "Bad column name"
            testCase "test 4" <| fun _ ->
                printfn "Test giving faulty column types"
                let myDB = ref INilTable
                create "Small Table" [("Name", String None);("ID", Int None)] myDB |> ignore
                let fail = insert "Small Table" ["Name"] [Int (Some 19)] myDB
                Expect.equal fail (Error "Create new row: the given column values don't match the column types.") "Bad column type"
    
        ]
    let propertiesExEngineInsert =
        // Random tests to verify ExecutionEngine.Insert
        testList "FsCheck ExecutionEngine.Insert" [
            testProperty "Add a random row to an empy table" <| fun a b ->
                let myDB = ref INilTable
                create "Small Table" [("Name", String None);("ID", Int None)] myDB |> ignore
                insert "Small Table" ["Name"; "ID"] [String a; Int b] myDB = Result ()
            testProperty "Add three random rows to an empy table" <| fun a b c d e f->
                let myDB = ref INilTable
                create "Small Table" [("Name", String None);("ID", Int None)] myDB |> ignore
                insert "Small Table" ["Name"; "ID"] [String a; Int b] myDB |> ignore
                insert "Small Table" ["Name"; "ID"] [String c; Int d] myDB |> ignore
                insert "Small Table" ["Name"; "ID"] [String e; Int f] myDB = Result ()
]