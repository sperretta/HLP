namespace ExecutionEngine
module ExecutionEngineInsert =

    open System.IO
    open System
    type myData = | String of Option<string> | Int of Option<int> | Float of Option<float>                // To be extended when the wanted data types have been decided
                  | Byte of Option<byte>     | Bool of Option<bool>
    type IList = INode of ParName: string * Value: myData * Prev : ref<IList> * Tl : ref<IList> | INilLow // A node has the parameter name (column name), the parameter value
                                                                                                          // and links to the previous and next nodes
    type topList = Node of lowList : ref<IList> * Tl : ref<topList> | INilTop                             // The nodes of the top-level list have refs to low-level lists.
    type data = ref<topList>                                                                              // Holds all the data in the table
    type TableList = TableNode of topList : ref<topList> * TableName : string * Tl : ref<TableList> | INilTable // Holds all the tables in the database.

    let rec topListLast thisList : data =
        match !thisList with
        | INilTop -> thisList
        | Node (_, tail) -> topListLast tail

    let topListFirstRow (thisList : data) =
        match !thisList with
        | INilTop -> None
        | Node (row, _) -> Some row

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
            let newNode = ref INilLow
            nextNode := (INode (colName, inpNext, prevNode, newNode))
            matchInputListsRec colTail inpTail nextNode newNode
        | _ -> printfn "Error: %A" (columns,inpData)
    
    let matchInputLists columns inpData =
        let firstHead = ref INilLow
        let nextNode  = ref INilLow
        matchInputListsRec columns inpData firstHead nextNode
        nextNode


    let listBuilder acc lowList =
        let newNode = ref INilTop
        acc := Node (lowList, newNode)
        newNode

    let buildData columns inpData = 
        let tmp = List.map (matchInputLists columns) inpData
        let firstHead = ref INilTop
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
    topListFirstRow myTable

    let extractColumnNamesHelper1 = function
    | String _ -> "String"
    | Int _ -> "Int"
    | Float _ -> "Float"
    | Byte _ -> "Byte"
    | Bool _ -> "Bool"

    let rec extractColumnNamesHelper2 thisRow = 
        match !thisRow with
        | INilLow -> []
        | INode (colName, colVal, prev, next) -> colName :: extractColumnNamesHelper1 colVal :: extractColumnNamesHelper2 next

    let extractColumnNames thisTable =
        let rowOne = topListFirstRow thisTable
        match rowOne with
        | None -> []
        | Some row -> extractColumnNamesHelper2 row

    extractColumnNames myTable    

    let testColumnNames = ["Names"; "String"; "ID"; "Int"]
    let testValueList = [String (Some "George"); Int (Some 17)]

    let addToTable thisTable columnNameList valueList =
        // Build a low-level list with the new row
        // Add this to the old table
        // Return the original table with the new row appended
        let newRow = matchInputLists columnNameList valueList
        listBuilder (topListLast thisTable) newRow |> ignore

    let addToTableWrapper thisTable columnList valueList =
        match columnList with
        | [] -> addToTable thisTable (extractColumnNames thisTable) valueList
        | _ -> addToTable thisTable columnList valueList

    addToTable myTable testColumnNames testValueList
    addToTableWrapper myTable [] testValueList
    addToTableWrapper myTable testColumnNames testValueList
    myTable