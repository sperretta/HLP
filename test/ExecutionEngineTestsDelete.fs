namespace ExecutionEngineTests
module Delete =

    open System.IO
    open System
    open ReturnControl.Main
    open databaseStructure.databaseStructure
    open ExecutionEngine.Create
    open ExecutionEngine.Insert
    open ExecutionEngine.Delete
    open Expecto


    [<Tests>]
    let testExEngineDelete =
        // Ad-hoc tests to verify ExecutionEngine.Delete
        testList "Tests for the ExecutionEngine.Delete module" [
            testCase "test 1" <| fun _ ->
                printfn "Test adding and removing a table to an empty database"
                let myDB = ref INilTable
                create "Small Table" [("Name", String None);("ID", Int None)] myDB |> ignore
                insert "Small Table" ["Name";"ID"] [String (Some "Amy"); Int (Some 17)] myDB |> ignore
                let comp = ref INilTable
                let tabName = "Small Table"
                let delRes = delete [tabName] None myDB
                match delRes with
                | Error e -> Expect.equal (Error e) (Error( "DELETE: '" + tabName + "' specified for deletion but not found.")) "Delete non-existant table"
                | Result db -> Expect.equal ( db) ( comp) "Delete table"
            testCase "test 2" <| fun _ ->
                printfn "Test adding and removing a table to an empty database"
                let myDB = ref INilTable
                create "Small Table" [("Name", String None);("ID", Int None)] myDB |> ignore
                insert "Small Table" ["Name";"ID"] [String (Some "Amy"); Int (Some 17)] myDB |> ignore
                let comp = ref INilTable
                let tabName = "Gone Table"
                let delRes = delete [tabName] None myDB
                match delRes with
                | Error e -> Expect.equal (Error e) (Error( "DELETE: '" + tabName + "' specified for deletion but not found.")) "Delete non-existant table"
                | Result db -> Expect.equal ( db) ( comp) "Delete table"
            testCase "test 2.5" <| fun _ ->
                let myDB = ref INilTable
                create "Small Table" [("Name", String None);("ID", Int None)] myDB |> ignore
                create "Other Table" [("Hex", Byte None);("ID", Int None)] myDB |> ignore
                match delete ["Small Table"; "Other Table"] None myDB with
                | Error _ -> Expect.equal ( 0) ( 1) "Delete tables"
                | Result db -> Expect.equal (string db) (string (ref INilTable)) "Delete tables"
                
            testCase "test 3" <| fun _ -> 
                let testFunc map =
                    match Map.find "ID" map with
                        | Int (Some _) -> Result true
                        | Int None -> Result false
                let db1 = ref INilTable
                let db2 = ref INilTable
                create "Small Table" [("Name", String None);("ID", Int None)] db1 |> ignore
                insert "Small Table" ["Name";"ID"] [String (Some "Amy"); Int (Some 17)] db1 |> ignore
                insert "Small Table" ["Name";"ID"] [String (Some "Rory"); Int None] db1 |> ignore
                insert "Small Table" ["Name";"ID"] [String (Some "River"); Int (Some 19)] db1 |> ignore
                let db1' = delete ["Small Table"] (Some testFunc) db1

                create "Small Table" [("Name", String None);("ID", Int None)] db2 |> ignore
                insert "Small Table" ["Name";"ID"] [String (Some "Amy"); Int (Some 17)] db2 |> ignore
                insert "Small Table" ["Name";"ID"] [String (Some "Rory"); Int (Some 19)] db2 |> ignore
                match db1' with 
                | Result dbDel -> Expect.equal (string dbDel) (string db2) "Delete row"
                | _ -> Expect.equal ( 1) ( 0) "Delete row"
            ]
    let propertiesExEngineDelete =
        // Random tests to verify ExecutionEngine.Delete
        testList "FsCheck ExecutionEngine.Delete" [
            testProperty "Delete certain rows from random table" <| fun a b c d e f->
                let testFunc map =
                    match Map.find "ID" map with
                        | Int (Some _) -> Result true
                        | Int None -> Result false
                let db1 = ref INilTable
                let db2 = ref INilTable
                create "Small Table" [("Name", String None);("ID", Int None)] db1 |> ignore
                create "Small Table" [("Name", String None);("ID", Int None)] db2 |> ignore
                match b with
                | None -> insert "Small Table" [] [String a; Int b] db1 |> ignore
                | Some _ ->
                    insert "Small Table" [] [String a; Int b] db1 |> ignore
                    insert "Small Table" [] [String a; Int b] db2 |> ignore
                match d with
                | None -> insert "Small Table" [] [String c; Int d] db1 |> ignore
                | Some _ ->
                    insert "Small Table" [] [String c; Int d] db1 |> ignore
                    insert "Small Table" [] [String c; Int d] db2 |> ignore
                match f with
                | None -> insert "Small Table" [] [String e; Int f] db1 |> ignore
                | Some _ ->
                    insert "Small Table" [] [String e; Int f] db1 |> ignore
                    insert "Small Table" [] [String e; Int f] db2 |> ignore
                let dbRes = delete ["Small Table"] (Some testFunc) db1
                match dbRes with 
                | Result dbDel -> Expect.equal (string dbDel) (string db2) "Delete certain rows"
                | _ -> Expect.equal ( 1) ( 0) "Delete certain rows" 
            testProperty "Delete certain rows from random tables" <| fun a1 a2 b c1 c2 d e1 e2 f->
                let testFunc map =
                    match Map.find "ID" map with
                        | Int (Some _) -> Result true
                        | Int None -> Result false
                let db1 = ref INilTable
                let db2 = ref INilTable
                create "Small Table" [("Name", String None);("ID", Int None)] db1 |> ignore
                create "Other Table" [("Hex", Byte None);("ID", Int None)] db1 |> ignore
                create "Small Table" [("Name", String None);("ID", Int None)] db2 |> ignore
                create "Other Table" [("Hex", Byte None);("ID", Int None)] db2 |> ignore
                match b with
                | None -> 
                    insert "Small Table" [] [String a1; Int b] db1 |> ignore
                    insert "Other Table" [] [Byte a2; Int b] db1 |> ignore
                | Some _ ->
                    insert "Small Table" [] [String a1; Int b] db1 |> ignore
                    insert "Other Table" [] [Byte a2; Int b] db1 |> ignore
                    insert "Small Table" [] [String a1; Int b] db2 |> ignore
                    insert "Other Table" [] [Byte a2; Int b] db2 |> ignore
                match d with
                | None -> 
                    insert "Small Table" [] [String c1; Int d] db1 |> ignore
                    insert "Other Table" [] [Byte c2; Int d] db1 |> ignore
                | Some _ ->
                    insert "Small Table" [] [String c1; Int d] db1 |> ignore
                    insert "Other Table" [] [Byte c2; Int d] db1 |> ignore
                    insert "Small Table" [] [String c1; Int d] db2 |> ignore
                    insert "Other Table" [] [Byte c2; Int d] db2 |> ignore
                match f with
                | None -> 
                    insert "Small Table" [] [String e1; Int f] db1 |> ignore
                    insert "Other Table" [] [Byte e2; Int f] db1 |> ignore
                | Some _ ->
                    insert "Small Table" [] [String e1; Int f] db1 |> ignore
                    insert "Other Table" [] [Byte e2; Int f] db1 |> ignore
                    insert "Small Table" [] [String e1; Int f] db2 |> ignore
                    insert "Other Table" [] [Byte e2; Int f] db2 |> ignore
                let dbRes = delete ["Small Table";"Other Table"] (Some testFunc) db1
                match dbRes with 
                | Result dbDel -> Expect.equal (string dbDel) (string db2) "Delete certain rows"
                | _ -> Expect.equal ( 1) ( 0) "Delete certain rows" 
        ]