namespace ExecutionEngine
module ExecutionEngineSelect =

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


/////////////////////////////////////////////////////////////////////////////

   // type boxList = BoxNode of ParName: string * Value: boxData * Prev : ref<boxList> * Tl : ref<boxList> | INilBox 

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
      
    //type rowList = RowNode of BoxList : ref<boxList> * Tl : ref<rowList> | INilRow   
    (*let testRow columnList thisRow testFunction = 
        let rowMap = transformRowMap thisRow
        match testFunction rowMap with
        | Error e -> Error e
        //| Result *)

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
        
    //type tableList = TableNode of topList : ref<rowList> * TableName : string * Columns : (string * boxData) list * Tl : ref<tableList> | INilTable 

    let rec transformDatabaseMapRec map restOfDatabase =
        match !restOfDatabase with
        | INilTable -> map
        | TableNode(tab, tableName, colTypes,nextTable) ->
            transformDatabaseMapRec (Map.add tableName (tab,colTypes) map) nextTable

    let transformDatabaseMap db =
        transformDatabaseMapRec Map.empty db   
        
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


    ///////////////////////////////////////////////////////////////////////////
    // TESTS

    ///////////////////////////////////////////////////
    // Functions to read in a database from a text file.
    let helperStrProcess list f F caller emes=
        match list with
        | "Some" :: c :: tail -> UnwrapResultThrough (fun a -> (c |> f |> Some |> F) :: a) (caller tail)
        | "None" :: tail ->      UnwrapResultThrough (fun a -> F None :: a) (caller tail)
        | e1 :: e2 :: tail -> Error ("READ IN: Value must be an option " + emes + ": " + e1 + " " + e2)
        | e :: tail ->        Error ("READ IN: Value must be an option " + emes + ": " + e)
        | [] ->               Error ("READ IN: " + emes + " empty list")

    let rec strProcess = function
        | [] -> Result []
        | a :: tail when a = "String" -> helperStrProcess tail (fun x -> x) String strProcess a
        | a :: tail when a = "Int"    -> helperStrProcess tail int Int strProcess a
        | a :: tail when a = "Float"  -> helperStrProcess tail float Float strProcess a
        | a :: tail when a = "Byte"   -> helperStrProcess tail byte Byte strProcess a
        | a :: tail when a = "Bool"   -> helperStrProcess tail System.Convert.ToBoolean Bool strProcess a
        | e :: tail                   -> Error ("READ IN: Data type specified not valid " + e)
         
    let compStringType colType inpNext = 
        match inpNext with
        | String a -> colType = "String"
        | Int a    -> colType = "Int"
        | Float a  -> colType = "Float"
        | Byte a   -> colType = "Byte"
        | Bool a   -> colType = "Bool"

    let rec matchInputListsRec columns inpData prevNode nextNode =
        match (columns,inpData) with 
        | ([],[]) -> Result()
        | (colName :: colType :: colTail, inpNext :: inpTail) when compStringType colType inpNext->
            let newNode = ref INilBox
            nextNode := (BoxNode (colName, inpNext, prevNode, newNode))
            matchInputListsRec colTail inpTail nextNode newNode
        | _ -> Error("READ IN: If columns are specified, there should be as many columns as values given.")
    
    let matchInputLists columns inpData =
        let firstHead = ref INilBox
        let nextNode  = ref INilBox
        let attempt = matchInputListsRec columns inpData firstHead nextNode
        UnwrapResultThrough (fun a -> nextNode) attempt

    let listBuilder accList lowList =
        match accList,lowList with
        | Error(e1),Error(e2) -> Error(e1+e2)
        | Error(e1), _ -> Error(e1)
        | _, Error(e2) -> Error(e2)
        | Result acc, Result list ->
            let newNode = ref INilRow
            acc := RowNode (list, newNode)
            Result newNode

    let buildData columns (inpData : ReturnCode<boxData list> list) = 
        let tmp = List.map (UnwrapResultInto (matchInputLists columns)) inpData
        let firstHead = ref INilRow
        let build = List.fold listBuilder (Result firstHead) tmp
        match build with
        | Error(e) -> Error(e)
        | Result _ -> Result firstHead

    let cleanLine (inpString : string) =
        inpString.Split [|' ';'\t'|] 
        |> Array.toList |> List.filter (fun x -> x <> "")

    let readTable inpStrings =
        let splitStrings = List.map cleanLine inpStrings
        let colNames = List.head splitStrings
        let colData = List.tail splitStrings |> List.map strProcess
        buildData colNames colData

    let rec allInOneTable inpStrings rowList =
        match inpStrings with
        | [] -> (List.rev rowList,[])
        | a :: tail when a = "TABLE" -> (List.rev rowList,inpStrings)
        | a :: tail -> allInOneTable tail (a :: rowList)

    let rec sortIntoMultipleRev inpList =
        let (table, otherStrings) = allInOneTable inpList []
        match inpList with
        | [] -> Result []
        | a :: tableName :: tail when a = "TABLE" -> 
            let (table, otherStrings) = allInOneTable tail [] 
            let laterRes = sortIntoMultipleRev otherStrings
            match laterRes with
            | Error _ -> laterRes
            | Result later -> 
                Result ((tableName, table) :: later)
        | _ -> Error("READ IN: A table must have a table name")

    let splitIntoThreeListsHelper (tabName, typeData) acc =
        match acc, typeData with
        | Error(e), _ -> Error(e)
        | Result (nameAcc, typeAcc, dataAcc), typeString :: dataString ->
            Result (tabName :: nameAcc, typeString :: typeAcc, (typeString :: dataString) :: dataAcc)
        | _,_ -> Error("A table must have data types specified") 

    let splitIntoThreeLists inpList =
        List.foldBack splitIntoThreeListsHelper inpList (Result ([], [], []))

    let rec getTypes = function
        | [] -> Result []
        | colName :: colType :: tail -> 
            let nextOnes = getTypes tail
            match nextOnes with
            | Error(e) -> Error(e)
            | Result nOnes -> 
                let tmp = colType :: ["None"] |> strProcess
                match tmp with
                | Error(e) -> Error(e)
                | Result tmp2 ->
                    Result ( (colName, (List.head tmp2) ) :: nOnes )
        | _ -> Error "READ IN: Column name given but no type specified."

    let rec buildDatabaseRec tableList tableNames tableTypes thisNode =
        match tableList, tableNames, tableTypes with
        | ([],[],[]) -> Result ()
        | thisTable :: tableTail, thisName :: nameTail, theseTypes :: typeTail ->
            let nextNode = ref INilTable
            thisNode := TableNode (thisTable, thisName, theseTypes, nextNode )
            buildDatabaseRec tableTail nameTail typeTail nextNode
        | _ -> Error "READ IN: Unequal amount of tables, table names and table type specifications given"

    let buildDatabase tableList tableNames tableTypes =
        let firstNode = ref INilTable
        match buildDatabaseRec tableList tableNames tableTypes firstNode with
        | Error e -> Error e
        | Result _ -> Result firstNode
    
    let returnCodeListFlattener (lis : ReturnCode<'a> list) =
        List.foldBack (fun elRes accRes -> match elRes,accRes with
                                            | Error(e1), Error(e2) -> Error(e1+e2)
                                            | Error(e1), _ -> Error(e1)
                                            | _, Error(e2) -> Error(e2)
                                            | Result el, Result acc -> Result (el :: acc)   ) lis (Result []) 

    let buildDatabaseWrapper inpStrings =
        let decodeRes = inpStrings |> sortIntoMultipleRev |> UnwrapResultInto splitIntoThreeLists
        match decodeRes with
        | Error e -> Error e
        | Result (names, typesRaw, data) ->
            let types = List.map getTypes (List.map cleanLine typesRaw)
            let tables = List.map readTable data
            let typesFlat = returnCodeListFlattener types
            let tablesFlat = returnCodeListFlattener tables
            match typesFlat, tablesFlat with
            | Error(e1), Error(e2) -> Error(e1+e2)
            | Error(e1), _ -> Error(e1)
            | _, Error(e2) -> Error(e2)
            | Result typ, Result tab -> buildDatabase tab names typ

    let readInFull pathName = 
        pathName |> IO.File.ReadLines |> Seq.toList |> buildDatabaseWrapper
    
    ///////////////////////////////////////////////////
    // Tests
    (*let testFunction rowMap =
        match Map.find "ID" rowMap with
        | Int (Some a) when a > 15 && a < 20 -> Result true
        | _ -> Result false
    let allFunction rowMap = Result true

    let path = @"C:\Users\Sigrid\Documents\Visual Studio 2015\HLP\src\testData.txt" 
    let dbRes = readInFull path
    let selected = match dbRes with
                    | Error e -> Error e
                    | Result db ->
                        select ["Names"; "ID"] ["Literary Characters"; "Movie Characters"] allFunction 2 2 db *)