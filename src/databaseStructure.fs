namespace databaseStructure
module databaseStructure =

    open System.IO
    open System
    open ReturnControl.Main // Function result wrappers. Either Error or Result.

    type boxData = | String of Option<string>
                   | Int of Option<int> 
                   | Float of Option<float>                
                   | Byte of Option<byte>     
                   | Bool of Option<bool>
                   static member (+) (x,y) = // member functions for use in execution engine to set variables
                        match x,y with       // defined by Matt
                        | Int(Some(a)),Int(Some(b)) -> Result(Int(Some(a+b)))
                        | Float(Some(a)),Float(Some(b)) -> Result(Float(Some(a+b)))
                        | Byte(Some(a)),Byte(Some(b)) -> Result(Byte(Some(a+b)))
                        | Bool _, Bool _ -> Error "Booleans don't do maths"
                        | String _, String _ -> Error "Strings don't do maths"
                        | _ -> Error(sprintf "Types do not match %A %A" x y)
                   static member (-) (x,y) = // based on (+), which was given by Matt
                        match x,y with
                        | Int(Some(a)),Int(Some(b)) -> Result(Int(Some(a-b)))
                        | Float(Some(a)),Float(Some(b)) -> Result(Float(Some(a-b)))
                        | Byte(Some(a)),Byte(Some(b)) -> Result(Byte(Some(a-b)))
                        | Bool _, Bool _ -> Error "Booleans don't do maths"
                        | String _, String _ -> Error "Strings don't do maths"
                        | _ -> Error(sprintf "Types do not match %A %A" x y)
                   static member (*) (x,y) = // based on (+), which was given by Matt
                        match x,y with
                        | Int(Some(a)),Int(Some(b)) -> Result(Int(Some(a*b)))
                        | Float(Some(a)),Float(Some(b)) -> Result(Float(Some(a*b)))
                        | Byte(Some(a)),Byte(Some(b)) -> Result(Byte(Some(a*b)))
                        | Bool _, Bool _ -> Error "Booleans don't do maths"
                        | String _, String _ -> Error "Strings don't do maths"
                        | _ -> Error(sprintf "Types do not match %A %A" x y)
                   static member (/) (x,y) = // based on (+), which was given by Matt
                        match x,y with
                        | Int(Some(a)),Int(Some(b)) -> Result(Int(Some(a/b)))
                        | Float(Some(a)),Float(Some(b)) -> Result(Float(Some(a/b)))
                        | Byte(Some(a)),Byte(Some(b)) -> Result(Byte(Some(a/b)))
                        | Bool _, Bool _ -> Error "Booleans don't do maths"
                        | String _, String _ -> Error "Strings don't do maths"
                        | _ -> Error(sprintf "Types do not match %A %A" x y)
                    

    // A node has the parameter name (column name), the parameter value and links to the previous and next nodes
    // A node is equivalent to a cell
    type boxList = BoxNode of ParName: string * Value: boxData * Prev : ref<boxList> * Tl : ref<boxList> | INilBox 
    type row = ref<boxList>                                                                                       
    // The nodes of the table-level list (rowList) have refs to the row-level lists (boxList)
    // A row is equivalent to a row of cells
    type rowList = RowNode of BoxList : ref<boxList> * Tl : ref<rowList> | INilRow                            
    type table = ref<rowList>  // Holds all the data in the table
    // Holds all the tables in the database.
    // A node has a table, a table name, a list with column names and types, and a tail pointing to the next table in the database.
    // A table is equivalent to a table of cells 
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

    // Get unit table: Take a database and return Result value if there's only one cell, else error.
    let tableGetUnitValue (thisDatabase :database)=
        match !thisDatabase with
        | INilTable -> Error "Database is empty"
        | TableNode (thisTable, _, _, tail) when !tail = INilTable ->
             match !thisTable with
             | INilRow -> Error "Table is empty"
             | RowNode (cellList, tail) when !tail = INilRow ->
                match !cellList with
                | INilBox -> Error "Rows are empty"
                | BoxNode(boxName, boxVal, _, nextBox) when !nextBox = INilBox -> Result boxVal
                | _ -> Error "Database is larger than one cell"
             | _ -> Error "Database is larger than one cell"
        | _ -> Error "Database is larger than one cell"

    // Recursive function called by tranformRowMap
    let rec transformRowMapRec map restOfRow =
        match !restOfRow with
        | INilBox -> map
        | BoxNode(colName, colVal, _, nextBox) ->
            transformRowMapRec (Map.add colName colVal map) nextBox
    
    // Transform a row into a map from column names to column values
    let transformRowMap row =
        transformRowMapRec Map.empty row

    // Read a value from a map keyed by string
    // Used to look up column calues from column names, and to look up tables from table names
    let readFromMap myMap (key : string) e =
        try 
            Map.find key myMap |> Result 
        with
            notInMap -> "'" + key + "'" + e |> Error 
   
    // Recursive function called by tranformDatabaseMap
    let rec transformDatabaseMapRec map restOfDatabase =
        match !restOfDatabase with
        | INilTable -> map
        | TableNode(tab, tableName, colTypes,nextTable) ->
            transformDatabaseMapRec (Map.add tableName (tab,colTypes) map) nextTable

    // Transform a database into a map from table names to tables
    let transformDatabaseMap db =
        transformDatabaseMapRec Map.empty db  