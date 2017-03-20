namespace ExecutionEngine
module Select =

    open System.IO
    open System
    open databaseStructure.databaseStructure
    open ReturnControl.Main


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

    let newRow columnList rowMap =
        let firstHead = ref INilBox
        let firstNode = ref INilBox
        UnwrapResultInto (fun _ -> Result firstNode) (newRowRec columnList rowMap firstHead firstNode)

    let rec testTableRec columnList thisTable testFunction limit offset nextRow =
        match limit, offset with
        | i, j when i<=0 -> Result()
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

    let testTable columnList thisTable testFunction limit offset =
        let nextRow = ref INilRow
        let run = testTableRec columnList thisTable testFunction limit offset nextRow
        match run with
        | Error e -> Error e
        | Result _ -> Result nextRow 
        
    let rec tranCols specifiedCols foundCols =
         match specifiedCols, foundCols with
         | [], _ -> Result []
         | _, [] -> Error "SELECT: One of the specified columns was not found."
         | specHead :: specTail, (foundString, foundBox) :: foundTail when specHead = foundString -> UnwrapResultThrough (fun tail -> (foundString, foundBox) :: tail) (tranCols specTail foundTail)
         | _, (foundString, foundBox) :: foundTail -> tranCols specifiedCols foundTail
        
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

    let select (columnList : string list) (tableList : string list) (testFunction : Map<string,boxData> -> ReturnCode<bool>) (limit : int) (offset : int) (db : database) : ReturnCode<database> =
        let tableMap = transformDatabaseMap db
        let transformedTable = ref INilTable
        match selectRec columnList tableList testFunction limit offset tableMap transformedTable with
        | Error e -> Error e
        | Result _ -> Result transformedTable