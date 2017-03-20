namespace ExecutionEngine
module Delete =

    open System.IO
    open System
    open databaseStructure.databaseStructure
    open ReturnControl.Main




    let rec copyBox box tranBox prevTran =
        match !box with
        | INilBox -> ()
        | BoxNode (value, colName, prev, next) ->
            let nextTran = ref INilBox
            tranBox := BoxNode (value, colName, prevTran, nextTran)
            copyBox next nextTran tranBox

    let rec copyRow row (tranRow : table) =
        match !row with
        | INilRow -> ()
        | RowNode (vals, last) ->
            let rowHead = ref INilBox
            let rowCopy = ref INilBox
            copyBox vals rowCopy rowHead
            let nextTran = ref INilRow
            tranRow := RowNode (rowCopy, nextTran)
            copyRow last nextTran
            
    let copyTable db tranTable =
        match !db with
        | INilTable -> ()
        | TableNode (thisTable, tabName, cols, _) ->
            let rowHead = ref INilRow
            copyRow thisTable rowHead
            let nextTable = ref INilTable
            tranTable := TableNode (rowHead, tabName, cols, nextTable)

    let rec deleteTablesRec deleteList db (transTable : tableList ref)=
        match deleteList, !db with
        | [], INilTable -> Result () // Finished list for deletion and the database
        | [], TableNode (_, _, _, tableTail) -> // Finished list for deletion so will copy table over
            copyTable db transTable
            match !transTable with
            | INilTable -> Error "DELETE: Table unsuccessfully copied"
            | TableNode (_, _, _, nextTable) ->
                deleteTablesRec deleteList tableTail nextTable
        | t1 :: tTail, INilTable -> Error ("DELETE: '" + t1 + "' specified for deletion but not found.") // Finished database but not list for deletion
        | t1 :: tTail, TableNode (thisTable, tabName, cols, tableTail) when tabName = t1 -> // Found a table to delete
            deleteTablesRec tTail tableTail transTable
        | _, TableNode (thisTable, tabName, cols, tableTail) -> // This table should not be deleted so will copy it over
            copyTable db transTable
            match !transTable with
            | INilTable -> Error "DELETE: Table unsuccessfully copied"
            | TableNode (_, _, _, nextTable) ->
                deleteTablesRec deleteList tableTail nextTable
                
    let rec deleteCertainRows (thisTable : table) testFunction transTable =
        match !thisTable with
        | INilRow -> Result ()
        | RowNode (values, nextRow) ->
            let rowMap = transformRowMap values
            match testFunction rowMap with
            | Error e -> Error e
            | Result b when b = true -> deleteCertainRows nextRow testFunction transTable
            | Result _ -> 
                let boxHead = ref INilBox
                let beforeHead = ref INilBox
                copyBox values boxHead beforeHead
                let newRow = ref INilRow
                transTable := RowNode (boxHead, newRow)
                deleteCertainRows nextRow testFunction newRow

    let rec deleteRowsRec deleteList testFunction db transTable =
        match deleteList, !db with
        | [], INilTable -> Result()
        | [], TableNode (thisTable, tabName, cols, tableTail) ->
            copyTable db transTable
            match !transTable with
            | INilTable -> Error "DELETE: Table unsuccessfully copied"
            | TableNode (_, _, _, nextTable) ->
                deleteRowsRec deleteList testFunction tableTail nextTable
        | t1 :: tTail, INilTable -> Error ("DELETE: '" + t1 + "' specified for deletion but not found.")
        | t1 :: tTail, TableNode (thisTable, tabName, cols, tableTail) when tabName = t1 ->
            let rowHead = ref INilRow
            match deleteCertainRows thisTable testFunction rowHead with
            | Error e -> Error e
            | Result _ ->
                let nextTable = ref INilTable
                transTable := TableNode(rowHead, tabName, cols, nextTable)
                deleteRowsRec tTail testFunction tableTail nextTable
        | t1 :: tTail, TableNode (thisTable, tabName, cols, tableTail) ->
            copyTable db transTable
            match !transTable with
            | INilTable -> Error "DELETE: Table unsuccessfully copied"
            | TableNode (_, _, _, nextTable) ->
                deleteRowsRec deleteList testFunction tableTail nextTable

    let delete (deleteList : string list) (testFunctionOption : (Map<string,boxData> -> ReturnCode<bool>) option) (db : database) : ReturnCode<database> =
        let head = ref INilTable
        match testFunctionOption with
        | None -> 
            match deleteTablesRec deleteList db head with
            | Error e -> Error e
            | Result _ -> Result head
        | Some testFunction ->
            match deleteRowsRec deleteList testFunction db head with
            | Error e -> Error e
            | Result _ -> Result head

    (*open LoadSave.LoadSave

    let func1 (map : Map<string,boxData> ):  ReturnCode<bool> =
        match Map.find "Member" map with
        | Bool (Some _) -> Result false
        | Bool None -> Result true
    let func2 (map : Map<string,boxData> ):  ReturnCode<bool> =
        match Map.find "Grade" map with
        | Int None -> Result false
        | Int _ -> Result true

    let myPath  = @"C:\Users\Sigrid\Documents\Visual Studio 2015\HLP\src\testData.txt"
    let myDB = load myPath |> (fun (Result a) -> a)
    delete ["Literary Characters"] None myDB
    delete ["A"] None myDB
    delete [] None myDB
    delete ["Literary Characters";"Grades"] None myDB
    delete ["Literary Characters"] (Some func1) myDB
    delete ["Grades"] (Some func2) myDB
    delete [] (Some func1) myDB
    myDB*)