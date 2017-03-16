namespace ExecutionEngine
module ExecutionEngineInsert =

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



    //////////////////////////////////////////////////////////////////////
    // Used for building tests. Taken from readTextFile

    let helperFunc list f F caller emes=
        match list with
        | "Some" :: c :: tail -> (c |> f |> Some |> F) :: caller tail
        | "None" :: tail -> F None :: caller tail
        | e :: tail -> (Some >> String) ("error: " + emes + ": " + e) :: caller tail
        | [] -> [(Some >> String) ("error: " + emes + " empty list")]


    let rec strProcess = function
        | [] -> []
        | a :: tail when a = "String" -> helperFunc tail (fun x -> x) String strProcess a
        | a :: tail when a = "Int"    -> helperFunc tail int Int strProcess a
        | a :: tail when a = "Float"  -> helperFunc tail float Float strProcess a
        | a :: tail when a = "Byte"   -> helperFunc tail byte Byte strProcess a
        | a :: tail when a = "Bool"   -> helperFunc tail System.Convert.ToBoolean Bool strProcess a
        | e :: tail                   -> (Some >> String) ("error: " + e) :: (strProcess tail)
         

       
    let checkType colType inpNext = 
        match inpNext with
        | String a -> colType = "String"
        | Int a    -> colType = "Int"
        | Float a  -> colType = "Float"
        | Byte a   -> colType = "Byte"
        | Bool a   -> colType = "Bool"


    let rec matchInputListsRec columns inpData prevNode nextNode =
        match (columns,inpData) with 
        | ([],[]) -> ()
        | (colName :: colType :: colTail, inpNext :: inpTail) when checkType colType inpNext->
            let newNode = ref INilBox
            nextNode := (BoxNode (colName, inpNext, prevNode, newNode))
            matchInputListsRec colTail inpTail nextNode newNode
        | _ -> printfn "Error: %A" (columns,inpData)
    
    let matchInputLists columns inpData =
        let firstHead = ref INilBox
        let nextNode  = ref INilBox
        matchInputListsRec columns inpData firstHead nextNode
        nextNode

    let listBuilder acc lowList =
        let newNode = ref INilRow
        acc := RowNode (lowList, newNode)
        newNode

    let buildData columns inpData = 
        let tmp = List.map (matchInputLists columns) inpData
        let firstHead = ref INilRow
        List.fold listBuilder firstHead tmp |> ignore
        firstHead

    let readInLine (inpString : string) =
        inpString.Split [|' ';'\t'|] 
        |> Array.toList |> List.filter (fun x -> x <> "")
        |> strProcess

    let cleanLine (inpString : string) =
        inpString.Split [|' ';'\t'|] 
        |> Array.toList |> List.filter (fun x -> x <> "")    
    
    let readTable inpStrings =
        let splitStrings = List.map cleanLine inpStrings
        let colNames = List.head splitStrings
        let colData = List.tail inpStrings |> List.map readInLine
        buildData colNames colData


    // Used for building tests.
    //////////////////////////////////////////////////////////////////////


    
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

    let addToTableTwo thisTable (colTypesNames, valueList) = 
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

    let addToTableTwoWrapper thisTable refColumns columnList valueList =
        match columnList, List.length refColumns, List.length valueList with
            | [], la, lb when la = lb -> addToTableTwo thisTable (refColumns, valueList)
            | [], _, _ -> Error("INSERT: When column names are unspecified the number of inputs must match the number of columns in the table. ")
            | _ -> 
                let columnSpec = getAllColTypesValues refColumns columnList valueList
                match columnSpec with
                    | Error emes -> Error emes
                    | Result typesValues ->
                        let (columns, values) = splitIntoLists typesValues
                        addToTableTwo thisTable (columns, values) 

    let insertTwo tableName columnList valueList thisDatabase =
        let thisTableOption = chooseTable thisDatabase tableName
        let typeSpecOption = getTableTypes thisDatabase tableName
        match thisTableOption, typeSpecOption with
        | None,_ -> Error("INSERT: Table specified not found")
        | _, None -> Error("INSERT: Table specified not found")
        | Some thisTable, Some typeSpec -> 
            addToTableTwoWrapper thisTable typeSpec columnList valueList
            
             

    // Testingcode
    let testSeparate = ["Names String ID Int";"String Some Orlando Int Some 45";"String Some Rebecca Int Some 42"]
    let myTable = readTable testSeparate
    let tableTwo = readTable testSeparate
    rowListFirstRow myTable
    
    //addToTable myTable testColumnNames testValueList
    //addToTableWrapper myTable [] testValueList
    //addToTableWrapper myTable testColumnNames testValueList
    myTable
    tableTwo
    let tail = ref INilTable
    let tabThree = ref INilRow
    let snd = ref (TableNode (tableTwo, "Second Table", [("Names", String None);("ID", Int None)], tail))
    let mid = ref (TableNode (tabThree, "Third Table", [("Users", String None);("ID", Byte None)], snd))
    let first = ref (TableNode (myTable, "First Table", [("Names", String None);("ID", Int None)], mid))
    // first is now a database with two tables that can be used for testing.
    chooseTable first "First Table"
    chooseTable first "Second Table"
    chooseTable first "Third Table"
    getTableTypes first "First Table"
    getTableTypes first "Third Table"
    //colNamesTypesFromDB first "First Table"

    insertTwo "Second Table" ["Names"] [String (Some "Harry")] first
    insertTwo "Third Table"  [] [String (Some "Harry"); Byte (Some 13uy)] first

    first // see results
