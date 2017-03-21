namespace ExecutionEngine
module Select =

    open System.IO
    open System
    open databaseStructure.databaseStructure
    open ReturnControl.Main

    // Recursive function called by newRow
    let rec newRowRec columnList rowMap prevNode thisNode =
        match columnList with
        | [] -> Result ()
        | col :: colTail ->
            match readFromMap rowMap col " is not in table" with
            | Error e -> Error e
            | Result value ->
                let nextNode = ref INilBox
                thisNode := BoxNode (col, value, prevNode, nextNode)
                newRowRec colTail rowMap thisNode nextNode

    // Takes a list of wanted columns (identified by column names) and a map from column names to cell values
    // for a specific row, and builds a linked list of cell values with the wanted columns and the values from the row map
    let newRow columnList rowMap =
        let firstHead = ref INilBox
        let firstNode = ref INilBox
        UnwrapResultInto (fun _ -> Result firstNode) (newRowRec columnList rowMap firstHead firstNode)

    // Recursive function called by testTable
    let rec testTableRec columnList thisTable testFunction limit offset nextRow =
        match limit, offset with
        | i, j when i=0 -> Result()
        | i, j when j>0 -> 
            match !thisTable with
            | INilRow -> Result()
            | RowNode (thisRow, otherRows) -> testTableRec columnList otherRows testFunction limit (offset - 1) nextRow
        | _,_ ->
            match !thisTable with
            | INilRow -> Result ()
            | RowNode (thisRow, otherRows) ->
                let rowMap = transformRowMap thisRow
                let checkRow = testFunction rowMap
                match checkRow with
                | Error e -> Error e
                | Result ret when ret = false-> testTableRec columnList otherRows testFunction limit (offset - 1) nextRow
                | Result _ -> 
                    let transformedRow = newRow columnList rowMap
                    let futureRow = ref INilRow
                    match transformedRow with
                    | Error e -> Error e
                    | Result tRow ->
                        nextRow := RowNode (tRow, futureRow)
                        testTableRec columnList otherRows testFunction (limit - 1) offset futureRow

    // Function that takes a table and returns a copy with the selected columns for the rows selected by
    // the testFunction, obeying the restrictions set by limit and offset
    let testTable columnList thisTable testFunction limit offset =
        let nextRow = ref INilRow
        let run = testTableRec columnList thisTable testFunction limit offset nextRow
        match run with
        | Error e -> Error e
        | Result _ -> Result nextRow 
        
    // Takes a list of column names and a list of (column names, column types) tuples, and returns
    // a list of (column names, column types) tuples for the column names specified in the first column name list.
    let rec tranCols (specifiedCols : string list) (foundCols : (string*boxData) list)=
         match specifiedCols, foundCols with
         | [], _ -> Result []
         | _, [] -> Error "SELECT: One of the specified columns was not found."
         | specHead :: specTail, (foundString, foundBox) :: foundTail when specHead = foundString -> UnwrapResultThrough (fun tail -> (foundString, foundBox) :: tail) (tranCols specTail foundTail)
         | _, (foundString, foundBox) :: foundTail -> tranCols specifiedCols foundTail
        
    // Recursive funtion called by select to go through a databse and copy the tables and rows wanted
    let rec selectRec columnList tableList testFunction limit offset tableMap transformedTable =
        match tableList with
        | [] -> Result ()
        | t1 :: tTail ->
            match readFromMap tableMap t1 " is not in database" with
            | Error e -> Error e
            | Result (tab,cols) -> 
                let attempt = testTable columnList tab testFunction limit offset 
                match attempt with 
                | Error e -> Error e
                | Result tranTable ->
                    let nextTable = ref INilTable
                    let tranColRes = tranCols columnList cols 
                    match tranColRes with
                    | Error e -> Error e
                    | Result tranCol ->
                        transformedTable := TableNode (tranTable, t1, tranCol, nextTable)
                        selectRec columnList tTail testFunction limit offset tableMap nextTable

    // Takes an option, and returns either the value, or the provided alternative
    let evaluateOption noneOption = function
        | None -> noneOption
        | Some value -> value

    // A testfunction that always returns true
    let acceptAll map = Result true

    // Function called by execution engine parser to implement selecting parts of a database. Returns said subsection of the database
    let select (columnList : string list) (tableList : string list) (testFunctionOpt : (Map<string,boxData> -> ReturnCode<bool>) option) (limitOpt : int option) (offsetOpt : int option) (db : database) : ReturnCode<database> =
        let tableMap = transformDatabaseMap db
        let transformedTable = ref INilTable
        let limit = evaluateOption -1 limitOpt
        let offset = evaluateOption 0 offsetOpt
        let testFunction = evaluateOption acceptAll testFunctionOpt
        match selectRec columnList tableList testFunction limit offset tableMap transformedTable with
        | Error e -> Error e
        | Result _ -> Result transformedTable