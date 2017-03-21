namespace ExecutionEngine
module Insert =

    open System.IO
    open System
    open databaseStructure.databaseStructure
    open ReturnControl.Main



    // Gets the types of the tables in the database
    let rec getTableTypes thisDatabase tableName =
        match !thisDatabase with
        | INilTable -> None
        | TableNode (_, thisName, theseColumns, _) when thisName = tableName -> Some theseColumns
        | TableNode (_, _, _, otherTables) -> getTableTypes otherTables tableName

    // Checks that two values have the same type
    let compareTypes valOne valTwo = 
        match (valOne, valTwo) with
        | (String _, String _) -> true
        | (Int _   , Int _   ) -> true
        | (Float _,  Float _ ) -> true
        | (Byte _,   Byte _  ) -> true
        | (Bool _,   Bool _  ) -> true
        | (_, _) -> false

    // Recursive function called by newRow
    let rec newRowRec columnTypesNames columnValues prevNode nextNode =
        match (columnTypesNames, columnValues) with
        | ([],[]) -> Result ()
        | ((colName, colType) :: otherTypesNames, thisVal :: otherValues) when compareTypes colType thisVal -> 
            let newNode = ref INilBox
            nextNode := BoxNode (colName, thisVal, prevNode, newNode)
            newRowRec otherTypesNames otherValues nextNode newNode
        | ((colName, colType) :: otherTypesNames, thisVal :: otherValues) -> 
            Error("Create new row: the given column values don't match the column types.")
        | _ -> Error("Create new row: column specification and new values don't have the same length.")

    // Takes a list with column names and column types, and a list of column values, and builds a linked list
    // of cell nodes from it
    let newRow columnTypesNames columnValues =
        let firstHead = ref INilBox
        let nextNode = ref INilBox
        UnwrapResultThrough (fun () -> nextNode) (newRowRec columnTypesNames columnValues firstHead nextNode)

    // Builds a row from a linked list of cell nodes
    let listBuilder acc lowList =
        let newNode = ref INilRow
        acc := RowNode (lowList, newNode)
        newNode

    // Adds a new row to an existing table
    let addToTable thisTable (colTypesNames, valueList) = 
        let createdRow = newRow colTypesNames valueList
        UnwrapResultThrough (fun createdRow -> listBuilder (rowListLast thisTable) createdRow |> ignore) createdRow
    
    // Takes in a list of colum names and types for reference, a list of column names and a list of cell values,
    // If they match with the reference then a list of (column name, column type), cell value tuples are returned,
    // otherwise error
    let rec getAllColTypesValues refColumns columnList valueList =
        match (refColumns, columnList, valueList) with
        | ([], [], []) -> Result []
        | ([], _ , _) -> Error("INSERT: At least one of the given column names don't match the given table.")
        | ((refName, refType) :: otherRefColumns, (colName : string) :: otherNames, (thisValue :boxData) :: otherValues) when refName = colName ->
            UnwrapResultThrough (fun restOfList -> ((refName, refType), thisValue) :: restOfList ) (getAllColTypesValues otherRefColumns otherNames otherValues)
        | ((refName, refType) :: otherRefColumns, _, _) ->
            UnwrapResultThrough (fun restOfList -> ((refName, refType), refType) :: restOfList ) (getAllColTypesValues otherRefColumns columnList valueList)

    // Takes a list of (column name, column type), cell value tuples (unwrapped output of getAllColTypesValues) 
    // and returns a tuple of (column name, column type) list and cell value list
    let splitIntoLists (colTypesValues : ((string*boxData) * boxData) list) =
        List.foldBack (fun (colName, colType) (accName, accType) -> (colName :: accName, colType :: accType) ) colTypesValues ([], []) 

    // Wrapper for adding a new row to a table
    let addToTableWrapper thisTable refColumns columnList valueList =
        match columnList, List.length refColumns, List.length valueList with
            | [], la, lb when la = lb -> addToTable thisTable (refColumns, valueList)
            | [], _, _ -> Error("INSERT: When column names are unspecified the number of inputs must match the number of columns in the table. ")
            | _ -> 
                let columnSpec = getAllColTypesValues refColumns columnList valueList
                match columnSpec with
                    | Error emes -> Error emes
                    | Result typesValues ->
                        let (columns, values) = splitIntoLists typesValues
                        addToTable thisTable (columns, values) 

    // Function called by execution engine parser to implement inserting a row into a database
    let insert tableName columnList valueList thisDatabase =
        let thisTableOption = chooseTable thisDatabase tableName
        let typeSpecOption = getTableTypes thisDatabase tableName
        match thisTableOption, typeSpecOption with
        | None,_ -> Error("INSERT: Table specified not found")
        | _, None -> Error("INSERT: Table specified not found")
        | Some thisTable, Some typeSpec -> 
            addToTableWrapper thisTable typeSpec columnList valueList