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
    type tableList = TableNode of topList : ref<rowList> * TableName : string * Tl : ref<tableList> | INilTable 
    type database = ref<tableList>

    let rec rowListLast thisList : table =
        match !thisList with
        | INilRow -> thisList
        | RowNode (_, tail) -> rowListLast tail

    let rowListFirstRow (thisList : table) =
        match !thisList with
        | INilRow -> None
        | RowNode (row, _) -> Some row

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

    let testSeparate = ["Names String ID Int";"String Some Orlando Int Some 45";"String Some Rebecca Int Some 42"]
    let myTable = readTable testSeparate
    rowListFirstRow myTable

    let extractColumnNamesHelper1 = function
    | String _ -> "String"
    | Int _ -> "Int"
    | Float _ -> "Float"
    | Byte _ -> "Byte"
    | Bool _ -> "Bool"

    let rec extractColumnNamesHelper2 thisRow = 
        match !thisRow with
        | INilBox -> []
        | BoxNode (colName, colVal, prev, next) -> colName :: extractColumnNamesHelper1 colVal :: extractColumnNamesHelper2 next

    let extractColumnNames thisTable =
        let rowOne = rowListFirstRow thisTable
        match rowOne with
        | None -> []
        | Some row -> extractColumnNamesHelper2 row

    let rec extractColumnTypesHelperCopy thisRow columnNames =
        match (!thisRow, columnNames) with
        | (INilBox,_) -> []
        | (BoxNode(parName,parVal,_,rowTail), colName :: colTail) when parName = colName 
            -> parName :: (extractColumnNamesHelper1 parVal) :: extractColumnTypesHelper rowTail colTail
        | (BoxNode(parName,parVal,_,rowTail), _)
            -> parName :: (extractColumnNamesHelper1 parVal) :: extractColumnTypesHelper rowTail columnNames

    let extractColumnTypesHelper2 = function
    | String _ -> String None
    | Int _ -> Int None
    | Float _ -> Float None
    | Byte _ -> Byte None
    | Bool _ -> Bool None

    let rec extractColumnTypesHelper1 thisRow columnNames columnValues =
        match (!thisRow, columnNames, columnValues) with
        | (INilBox,_,_) -> []
        | (BoxNode(parName,parVal,_,rowTail), colName :: colNameTail, colVal :: colValTail) when parName = colName 
            -> (parName, (extractColumnNamesHelper1 parVal), colVal) :: extractColumnTypesHelper1 rowTail colNameTail colValTail
        | (BoxNode(parName,parVal,_,rowTail),_,_)
            -> (parName, (extractColumnNamesHelper1 parVal), extractColumnTypesHelper2 parVal) :: extractColumnTypesHelper1 rowTail columnNames columnValues

    let splitIntoNamesAndValues namesAndValues =
        List.foldBack (fun  (parName, parType, parVal) (colAcc, valAcc) -> (parName :: parType :: colAcc, parVal :: valAcc) ) namesAndValues ([],[])

    splitIntoNamesAndValues [("Name", "String", String None); ("ID", "Int", Int None)]
  //  ParName: string * Value: boxData * Prev : ref<boxList> * Tl : ref<boxList> | INilBox
    let extractColumnTypes rowOne columnNames columnValues =
        extractColumnTypesHelper1 rowOne columnNames columnValues |> splitIntoNamesAndValues
        
    match !myTable with
    | RowNode (row, _) ->
        extractColumnTypes row [] []

    extractColumnNames myTable    

    let testColumnNames = ["Names"; "String"; "ID"; "Int"]
    let testValueList = [String (Some "George"); Int (Some 17)]

    let addToTable thisTable columnNameList valueList =
        // Build a low-level list with the new row
        // Add this to the old table
        // Return the original table with the new row appended
        let newRow = matchInputLists columnNameList valueList
        listBuilder (rowListLast thisTable) newRow |> ignore

    let addToTableWrapper thisTable columnList valueList =
        match columnList with
        | [] -> addToTable thisTable (extractColumnNames thisTable) valueList
        | _ -> addToTable thisTable columnList valueList

    addToTable myTable testColumnNames testValueList
    addToTableWrapper myTable [] testValueList
    addToTableWrapper myTable testColumnNames testValueList
    myTable

    
    (* Todo:
    Function to choose table by tableName
    Function to get columntypes by columnname                               DONE
    Allow for only some values to be given -> rest set as None              DONE
    Put into a common function that takes tableName, columnNameList and valueList 
    Give out an error message if some column names / column values are ignored, or just ignore everything?*)
    //let insert tableName columnNameList valueList