namespace ExecutionEngineTests
module Create =
    
    open System.IO
    open System
    open databaseStructure.databaseStructure
    open ExecutionEngine.Create
    open Expecto

    [<Tests>]
    let testExEngineCreate =
        testList "Tests for the ExecutionEngine.Create module" [
            testCase "test 1" <| fun _ ->
                let myDB = ref INilTable
                let comp = ref (TableNode ( ref INilRow, "Small Table", [("Name", String None);("ID", Int None)], ref INilTable  ))
                create "Small Table" [("Name", String None);("ID", Int None)] myDB |> ignore
                Expect.equal (myDB) (comp) "Create a two-column table"
        ]