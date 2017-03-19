namespace ExecutionEngine
module ExecutionEngineCreate =

    open System.IO
    open System

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

    type ReturnCode<'a> = // From Matt
        | Result of 'a
        | Error of string

    let UnwrapResultThrough func (from:ReturnCode<'a>) = // From Matt
        match from with
        | Result(res) -> Result(func res)
        | Error(str) -> Error(str)

    let UnwrapResultInto func (from:ReturnCode<'a>) = // From Matt
        match from with
        | Result(res) -> func res
        | Error(str) -> Error(str)

    ////////////////////////////////////////////////

    let rec insertEmptyTable columnTypeList tableName database =
        match !database with
        | TableNode (_, _, _, tail) -> insertEmptyTable columnTypeList tableName tail
        | INilTable ->
            let newTail = ref INilTable
            let emptyRow = ref INilRow
            database := TableNode (emptyRow, tableName, columnTypeList, newTail)

    let create tableName columnTypeList database =
        let isPresent = chooseTable database tableName
        match isPresent with
        | Some _ -> Error("CREATE: The table name specified is already in the database.")
        | None _ -> Result(insertEmptyTable columnTypeList tableName database)
    
    ///////////////////////////////////////////////
    // Tests
    //let myDatabase : database = ref INilTable
    //create "User Table" [("User Name", String None);("User ID", Int None);("High Score", Int None)] myDatabase
    //myDatabase
    //create "Grade Table" [("Subject", String None);("Grade", Float None)] myDatabase
    //myDatabase
    //create "User Table" [("Name", String None); ("Byte", Byte None)] myDatabase // Should result in error
    //myDatabase