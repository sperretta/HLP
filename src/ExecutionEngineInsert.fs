namespace ExecutionEngine
module Insert =

    open System.IO
    open System
    open databaseStructure.databaseStructure
    open ReturnControl.Main

    //////////////////////////////////////////////////////////////////////
    // Used for building tests. Taken from readTextFile

    let listBuilder acc lowList =
        let newNode = ref INilRow
        acc := RowNode (lowList, newNode)
        newNode

    //////////
    
    let rec getTableTypes thisDatabase tableName =
        match !thisDatabase with
        | INilTable -> None
        | TableNode (_, thisName, theseColumns, _) when thisName = tableName -> Some theseColumns
        | TableNode (_, _, _, otherTables) -> getTableTypes otherTables tableName

    let compareTypes valOne valTwo = 
        match (valOne, valTwo) with
        | (String _, String _) -> true
        | (Int _   , Int _   ) -> true
        | (Float _,  Float _ ) -> true
        | (Byte _,   Byte _  ) -> true
        | (Bool _,   Bool _  ) -> true
        | (_, _) -> false

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

    let newRow columnTypesNames columnValues =
        let firstHead = ref INilBox
        let nextNode = ref INilBox
        UnwrapResultThrough (fun () -> nextNode) (newRowRec columnTypesNames columnValues firstHead nextNode)

    let addToTable thisTable (colTypesNames, valueList) = 
        let newRow = newRow colTypesNames valueList
        UnwrapResultThrough (fun createdRow -> listBuilder (rowListLast thisTable) createdRow |> ignore) newRow
    
    let rec getAllColTypesValues refColumns columnList valueList =
        match (refColumns, columnList, valueList) with
        | ([], [], []) -> Result []
        | ([], _ , _) -> Error("INSERT: At least one of the given column names don't match the given table.")
        | ((refName, refType) :: otherRefColumns, colName :: otherNames, thisValue :: otherValues) when refName = colName ->
            UnwrapResultThrough (fun restOfList -> ((refName, refType), thisValue) :: restOfList ) (getAllColTypesValues otherRefColumns otherNames otherValues)
        | ((refName, refType) :: otherRefColumns, _, _) ->
            UnwrapResultThrough (fun restOfList -> ((refName, refType), refType) :: restOfList ) (getAllColTypesValues otherRefColumns columnList valueList)

    let splitIntoLists colTypesValues =
        List.foldBack (fun (colName, colType) (accName, accType) -> (colName :: accName, colType :: accType) ) colTypesValues ([], []) 

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

    let insert tableName columnList valueList thisDatabase =
        let thisTableOption = chooseTable thisDatabase tableName
        let typeSpecOption = getTableTypes thisDatabase tableName
        match thisTableOption, typeSpecOption with
        | None,_ -> Error("INSERT: Table specified not found")
        | _, None -> Error("INSERT: Table specified not found")
        | Some thisTable, Some typeSpec -> 
            addToTableWrapper thisTable typeSpec columnList valueList