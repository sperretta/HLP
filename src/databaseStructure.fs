﻿namespace databaseStructure
module databaseStructure =

    open System.IO
    open System
    open ReturnControl.Main

    type boxData = | String of Option<string>  // To be extended when the wanted data types have been decided
                   | Int of Option<int> 
                   | Float of Option<float>                
                   | Byte of Option<byte>     
                   | Bool of Option<bool>

    // A node has the parameter name (column name), the parameter value and links to the previous and next nodes
    type boxList = BoxNode of ParName: string * Value: boxData * Prev : ref<boxList> * Tl : ref<boxList> | INilBox 
    type row = ref<boxList>                                                                                       
    // The nodes of the table-level list (rowList) have refs to the row-level lists (boxList).
    type rowList = RowNode of BoxList : ref<boxList> * Tl : ref<rowList> | INilRow                            
    type table = ref<rowList>  // Holds all the data in the table
    // Holds all the tables in the database.
    // A node has a table, a table name, a list with column names and types, and a tail pointing to the next table in the database.
    type tableList = TableNode of topList : ref<rowList> * TableName : string * Columns : (string * boxData) list * Tl : ref<tableList> | INilTable 
    type database = ref<tableList>

    // Get the tail from a table (entry after last row)
    let rec rowListLast thisList : table =
        match !thisList with
        | INilRow -> thisList
        | RowNode (_, tail) -> rowListLast tail

    // Get the first row from a table
    let rowListFirstRow (thisList : table) =
        match !thisList with
        | INilRow -> None
        | RowNode (row, _) -> Some row

    // Get a specific table from a database
    let rec chooseTable thisDatabase tableName =
        match !thisDatabase with
        | INilTable -> None
        | TableNode (thisTable, thisName, _, _) when thisName = tableName -> Some thisTable
        | TableNode (_, _, _, otherTables) -> chooseTable otherTables tableName

    // Get unit table: Take a table and return Result value if there's only one cell, else error.
    let tableGetUnitValue thisTable =
         match !thisTable with
         | INilRow -> Error "Table is empty"
         | RowNode (cellList, tail) when !tail = INilRow ->
            match !cellList with
            | INilBox -> Error "Rows are empty"
            | BoxNode(boxName, boxVal, _, nextBox) when !nextBox = INilBox -> Result boxVal
            | _ -> Error "Table is larger than one cell"
         | _ -> Error "Table is larger than one cell"

    let rec transformRowMapRec map restOfRow =
        match !restOfRow with
        | INilBox -> map
        | BoxNode(colName, colVal, _, nextBox) ->
            transformRowMapRec (Map.add colName colVal map) nextBox

    let transformRowMap row =
        transformRowMapRec Map.empty row

    let readFromMap myMap (key : string) e =
        try 
            Map.find key myMap |> Result 
        with
            notInMap -> "SELECT: '" + key + "'" + e |> Error 
   
    let rec transformDatabaseMapRec map restOfDatabase =
        match !restOfDatabase with
        | INilTable -> map
        | TableNode(tab, tableName, colTypes,nextTable) ->
            transformDatabaseMapRec (Map.add tableName (tab,colTypes) map) nextTable

    let transformDatabaseMap db =
        transformDatabaseMapRec Map.empty db  