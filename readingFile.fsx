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

// Functions defined elsewhere
//////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////
// Functions to read in a database from a text file.
    let helperStrProcess list f F caller emes=
        match list with
        | "Some" :: c :: tail -> (c |> f |> Some |> F) :: caller tail
        | "None" :: tail -> F None :: caller tail
        | e :: tail -> (Some >> String) ("error: " + emes + ": " + e) :: caller tail
        | [] -> [(Some >> String) ("error: " + emes + " empty list")]

    let rec strProcess = function
        | [] -> []
        | a :: tail when a = "String" -> helperStrProcess tail (fun x -> x) String strProcess a
        | a :: tail when a = "Int"    -> helperStrProcess tail int Int strProcess a
        | a :: tail when a = "Float"  -> helperStrProcess tail float Float strProcess a
        | a :: tail when a = "Byte"   -> helperStrProcess tail byte Byte strProcess a
        | a :: tail when a = "Bool"   -> helperStrProcess tail System.Convert.ToBoolean Bool strProcess a
        | e :: tail                   -> (Some >> String) ("error: " + e) :: (strProcess tail)
         
    let compStringType colType inpNext = 
        match inpNext with
        | String a -> colType = "String"
        | Int a    -> colType = "Int"
        | Float a  -> colType = "Float"
        | Byte a   -> colType = "Byte"
        | Bool a   -> colType = "Bool"

    let rec matchInputListsRec columns inpData prevNode nextNode =
        match (columns,inpData) with 
        | ([],[]) -> ()
        | (colName :: colType :: colTail, inpNext :: inpTail) when compStringType colType inpNext->
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
        | [] -> []
        | a :: tableName :: tail when a = "TABLE" -> 
            let (table, otherStrings) = allInOneTable tail [] 
            (tableName, table) :: sortIntoMultipleRev otherStrings
        | _ -> printfn "Error!" |> fun () -> []

    let splitIntoThreeLists inpList =
        List.foldBack (fun (tabName, typeString :: dataString) (nameAcc, typeAcc, dataAcc) -> 
            (tabName :: nameAcc, typeString :: typeAcc, (typeString :: dataString) :: dataAcc) ) inpList ([], [], [])

    let rec getTypes = function
        | [] -> []
        | colName :: colType :: tail -> (colName, colType :: ["None"] |> strProcess |> List.head) :: getTypes tail

    let rec buildDatabaseRec tableList tableNames tableTypes thisNode =
        match tableList, tableNames, tableTypes with
        | ([],[],[]) -> ()
        | thisTable :: tableTail, thisName :: nameTail, theseTypes :: typeTail ->
            let nextNode = ref INilTable
            thisNode := TableNode (thisTable, thisName, theseTypes, nextNode )
            buildDatabaseRec tableTail nameTail typeTail nextNode

    let buildDatabase tableList tableNames tableTypes =
        let firstNode = ref INilTable
        buildDatabaseRec tableList tableNames tableTypes firstNode
        firstNode

    let buildDatabaseWrapper inpStrings =
        let (names, typesRaw, data) = inpStrings |> sortIntoMultipleRev |> splitIntoThreeLists
        let types = List.map getTypes (List.map cleanLine typesRaw)
        let tables = List.map readTable data
        buildDatabase tables names types

    let readInFull pathName = 
        pathName |> IO.File.ReadLines |> Seq.toList |> buildDatabaseWrapper


        
///////////////////////////////////////////////////
// Functions to save a database to a text file.

    let rec strBuilder = function
        | [] -> []
        | String (Some a) :: tail -> "String Some " + string a :: strBuilder tail
        | String None     :: tail -> "String None"             :: strBuilder tail
        | Int (Some a)    :: tail -> "Int Some " + string a    :: strBuilder tail
        | Int None        :: tail -> "Int None"                :: strBuilder tail
        | Float (Some a)  :: tail -> "Float Some " + string a  :: strBuilder tail
        | Float None      :: tail -> "Float None"              :: strBuilder tail
        | Byte (Some a)   :: tail -> "Byte Some " + string a   :: strBuilder tail
        | Byte None       :: tail -> "Byte None"               :: strBuilder tail
        | Bool (Some a)   :: tail -> "Bool Some " + string a   :: strBuilder tail
        | Bool None       :: tail -> "Bool None"               :: strBuilder tail

    let rec buildLine lineList = 
        match lineList with
        | [] -> ""
        | el :: tail -> el + " " + (buildLine tail)

    let buildOutList myData =
        myData |> List.map strBuilder |> List.map buildLine

    let rec extractBoxValues boxList =
        match !boxList with
        | INilBox -> []
        | BoxNode (colName, value, _, tail) -> value :: extractBoxValues tail

    let rec extractRowValues rowList =
        match !rowList with
        | INilRow -> []
        | RowNode (thisRow, nextRow) -> extractBoxValues thisRow :: extractRowValues nextRow

    let rec extractBoxTypes ( columns : (string * boxData) list ) = 
        match columns with
        | [] -> ""
        | (colName, String _) :: tail -> colName + " String " + extractBoxTypes tail
        | (colName, Int    _) :: tail -> colName + " Int    " + extractBoxTypes tail
        | (colName, Float  _) :: tail -> colName + " Float  " + extractBoxTypes tail
        | (colName, Byte   _) :: tail -> colName + " Byte   " + extractBoxTypes tail
        | (colName, Bool   _) :: tail -> colName + " Bool   " + extractBoxTypes tail

    let rec saveDatabaseStrings database =
        match !database with
        | INilTable -> []
        | TableNode (thisTable, tableName, tableTypes, nextTable) ->
            ("TABLE" :: tableName :: extractBoxTypes tableTypes :: (buildOutList (extractRowValues thisTable) ) ) @ saveDatabaseStrings nextTable
            
    let saveDatabase path database =
        File.WriteAllLines (path, saveDatabaseStrings database |> List.toSeq)
         
    ////////////////////////////////////////////////////////////////////////////////////////////////
    // Tests

    sortIntoMultipleRev ["TABLE"; "User Table"; "User String"; "X Y"; "Z W"; "TABLE"; "Table Two"; "ID Int"; "X Y"; "W Z"]
    |> splitIntoThreeLists
    getTypes (cleanLine "User  String   ID Int")
    sortIntoMultipleRev ["TABLE";"User Table";"Names String ID Int";"String Some Orlando Int Some 45";"String Some Rebecca Int Some 42"; "TABLE"; "Table Two"; "Subject String"; "String Some Math"; "String Some Music"]

    buildDatabaseWrapper ["TABLE";"User Table";"Names String ID Int";"String Some Orlando Int Some 45";"String Some Rebecca Int Some 42"; "TABLE"; "Table Two"; "Subject String"; "String Some Math"; "String Some Music"]


    let exCol = ["Names"; "String"; "ID"; "Int"]
    let exData = [String (Some "Hedda"); Int (Some 1)]
    let exDataMult = [[String (Some "Hedda"); Int (Some 1)];[String (Some "Nora"); Int (Some 6)]]
    let badData = [[String (Some "Hedvig"); Float (Some 3.2)]]

    matchInputLists exCol exData
    buildData exCol exDataMult
    buildData exCol badData

    strProcess ["String"; "Some"; "Orlando"; "Int"; "Some"; "45"]
    strProcess ["Byte"; "Some"; "0xF3"; "Byte"; "None"]
    strProcess ["Bool"; "Some"; "True"; "Bool"; "None"]
    strProcess ["Float"; "Some"; "3.1415926"; "Float"; "None"]
    strProcess ["String"; "some"; "hei"]
    strProcess ["Strange"; "Some"; "Dragon"]

    let myPath  = @"C:\Users\Sigrid\Documents\Visual Studio 2015\HLP\dataFile.txt"
    let outPath = @"C:\Users\Sigrid\Documents\Visual Studio 2015\HLP\outFile.txt"
    let outPathFull = @"C:\Users\Sigrid\Documents\Visual Studio 2015\HLP\outFileFull.txt"
    let someStruct = @"C:\Users\Sigrid\Documents\Visual Studio 2015\HLP\someStructureDataFile.txt"
    let structPath = @"C:\Users\Sigrid\Documents\Visual Studio 2015\HLP\structuredDataFile.txt"
    let fullPath = @"C:\Users\Sigrid\Documents\Visual Studio 2015\HLP\structuredNamedDataFile.txt" 

    let db = readInFull fullPath // Reads in data to a database as saved with final specification (17/3/17)
    File.WriteAllLines (outPathFull, saveDatabaseStrings db |> List.toSeq) // Writes all data into a database, saved with final specification (17/3/17)
    readInFull outPathFull // Can be read back in.
    
    //let fileLines = readFile myPath
    //let k = readIn fileLines
    //let j = ReadInData myPath

    let exCol2 = ["Names"; "String"; "ID"; "Int"; "Credit"; "Float"; "Hex"; "Byte"; "Member"; "Bool"]
    let testSeparate = ["Names String ID Int";"String Some Orlando Int Some 45";"String Some Rebecca Int Some 42"]
    readTable testSeparate
    //buildData exCol2 j

    let testMultiple = ["TABLE";"Names String ID Int";"String Some Orlando Int Some 45";"String Some Rebecca Int Some 42"; "TABLE"; "Subject String"; "String Some Math"; "String Some Music"]

    //wrapper ["TABLE"; "User String"; "X Y"; "Z W"; "TABLE"; "ID Int"; "X Y"; "W Z"] []

    // Get table names in a list, get table tyoes in a list, get a list with strings separated for tables (sortIntoMultiple output)

        


    //sortIntoMultiple ["TABLE"; "User String"; "X Y"; "Z W"; "TABLE"; "ID Int"; "X Y"; "W Z"]
        
    //sortIntoMultiple testMultiple
    //readFile structPath |> Seq.toList |> sortIntoMultiple |> List.map readTable
        
    //readFile someStruct |> Seq.toList |> readTable
    //type TableList = TableNode of topList : ref<topList> * TableName : string * Tl : ref<TableList> | INilTable

   // let s = strBuilder (List.head j )
  //  let t = buildLine s
   // let u = buildOutSeq j
    //buildOutList j
    //File.WriteAllLines(outPath, u)
   // let v = ReadInData outPath


 
        
//    printfn "%A" fileLines
 //   printfn "%A" k
 //   printfn "%A" j

(*
To save database:
    Extract list of values from a table
    Extract column names and types from a table
    Extract table name
    Write "TABLE" 
*)