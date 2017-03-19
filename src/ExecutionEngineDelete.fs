namespace ExecutionEngine
module Delete =

    open System.IO
    open System
    open databaseStructure.databaseStructure
    open ReturnControl.Main
    open Select

    let rec copyBox box tranBox prevTran =
        match !box with
        | INilBox -> ()
        | BoxNode (value, colName, prev, next) ->
            let nextTran = ref INilBox
            tranBox := BoxNode (value, colName, prevTran, nextTran)
            copyBox next nextTran tranBox

    let rec copyRow row tranRow =
        match !row with
        | INilRow -> ()
        | RowNode (vals, last) ->
            let rowHead = ref INilBox
            let rowCopy = ref INilBox
            copyBox vals rowCopy rowHead
            let nextTran = ref INilRow
            copyRow last nextTran
            
    let rec copyTable db tranTable =
        match !db with
        | INilTable -> ()
        | TableNode (thisTable, tabName, cols, _) ->
            let rowHead = ref INilRow
            copyRow thisTable rowHead
            let nextTable = ref INilTable
            tranTable := TableNode (rowHead, tabName, cols, nextTable)


    let rec deleteTablesRec deleteList db (transTable : tableList ref)=
        match deleteList, !db with
        | [], INilTable -> Result ()
        | [], TableNode (thisTable, tabName, cols, tableTail) ->
            copyTable db transTable
            match !transTable with
            | INilTable -> Error "DELETE: Table unsuccessfully copied"
            | TableNode (_, _, _, nextTable) ->
                deleteTablesRec deleteList tableTail nextTable
        | t1 :: tTail, INilTable -> Error ("DELETE: " + t1 + " specified for deletion but not found.")
        | t1 :: tTail, TableNode (thisTable, tabName, cols, tableTail) when tabName = t1 ->
            deleteTablesRec tTail tableTail transTable
        | t1 :: tTail, TableNode (thisTable, tabName, cols, tableTail) ->
            deleteTablesRec deleteList tableTail transTable
                

    let deleteTables deleteList db =
        let nextTable = ref INilTable
        match deleteTablesRec deleteList db nextTable with
        | Error e -> Error e
        | Result _ -> Result nextTable     


    let delete (deleteList : string list) (testFunctionOption : (Map<string,boxData> -> ReturnCode<bool>) option) (db : database) : ReturnCode<database> =
        match testFunctionOption with
        | None -> deleteTables deleteList db
        | Some testFunction ->
            
        