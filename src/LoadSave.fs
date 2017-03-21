namespace LoadSave
module LoadSave =
    open System.IO
    open System
    open databaseStructure.databaseStructure
    open ReturnControl.Main

///////////////////////////////////////////////////
// Functions to read in a database from a text file.
    
    // Used to read in other values than strings
    let helperStrProcess list f F caller emes=
        match list with
        | "Some" :: c :: tail -> UnwrapResultThrough (fun a -> (c |> f |> Some |> F) :: a) (caller tail)
        | "None" :: tail ->      UnwrapResultThrough (fun a -> F None :: a) (caller tail)
        | e1 :: e2 :: tail -> Error ("READ IN: Value must be an option " + emes + ": " + e1 + " " + e2)
        | e :: tail ->        Error ("READ IN: Value must be an option " + emes + ": " + e)
        | [] ->               Error ("READ IN: " + emes + " empty list")

    let rec helperStrProcessStrings list num =
        match num,list with
        | i,_ when i <= 0 -> Result ("",list)
        | i,cur::tail -> UnwrapResultInto (fun (str, list) -> Result( (" "+cur+str), list) ) (helperStrProcessStrings tail (i - 1))
        | i, [] -> Error "READ IN: String length specified wrongly"

    // Used to read in strings. Different as these can be multi-word.
    let helperStrProcessStringsWrapper list caller =
        match list with
        | [] -> Error ("READ IN: String empty list")
        | a :: t when a = "None" -> UnwrapResultThrough (fun rest -> String None :: rest) (caller t)
        | a :: num :: t when a = "Some" -> 
            let res = helperStrProcessStrings t (int num) 
            match res with
            | Error e -> Error e
            | Result (str, tail) ->
                UnwrapResultThrough (fun rest -> String (Some str.[1..]) :: rest) (caller tail)
        | e :: _ -> Error ("READ IN: Value must be an option String: " + e)         

    // Processes the list of strings found when reading in a row and splitting by words.
    let rec strProcess = function
        | [] -> Result []
        | a :: tail when a = "String" -> helperStrProcessStringsWrapper tail strProcess
        | a :: tail when a = "Int"    -> helperStrProcess tail int Int strProcess a
        | a :: tail when a = "Float"  -> helperStrProcess tail float Float strProcess a
        | a :: tail when a = "Byte"   -> helperStrProcess tail byte Byte strProcess a
        | a :: tail when a = "Bool"   -> helperStrProcess tail System.Convert.ToBoolean Bool strProcess a
        | e :: tail                   -> Error ("READ IN: Data type specified not valid " + e)
         
    // Checks that the value given matches the column datatype.
    let compStringType colType inpNext = 
        match inpNext with
        | String a -> colType = "String"
        | Int a    -> colType = "Int"
        | Float a  -> colType = "Float"
        | Byte a   -> colType = "Byte"
        | Bool a   -> colType = "Bool"

    // Recursive function called by matchInputLists
    let rec matchInputListsRec columns inpData prevNode nextNode =
        match (columns,inpData) with 
        | ([],[]) -> Result()
        | (colName :: colType :: colTail, inpNext :: inpTail) when compStringType colType inpNext->
            let newNode = ref INilBox
            nextNode := (BoxNode (colName, inpNext, prevNode, newNode))
            matchInputListsRec colTail inpTail nextNode newNode
        | _ -> Error("READ IN: If columns are specified, there should be as many columns as values given.")
    
    // Matches the list of input columns with the list of input values
    let matchInputLists columns inpData =
        let firstHead = ref INilBox
        let nextNode  = ref INilBox
        let attempt = matchInputListsRec columns inpData firstHead nextNode
        UnwrapResultThrough (fun a -> nextNode) attempt

    // Used to build up a row from a list of cells and a pointer to use to reference it
    let listBuilder accList lowList =
        match accList,lowList with
        | Error(e1),Error(e2) -> Error(e1+e2)
        | Error(e1), _ -> Error(e1)
        | _, Error(e2) -> Error(e2)
        | Result acc, Result list ->
            let newNode = ref INilRow
            acc := RowNode (list, newNode)
            Result newNode

    // Builds a database from a list of cell values and a list of column names
    let buildData columns (inpData : ReturnCode<boxData list> list) = 
        let tmp = List.map (UnwrapResultInto (matchInputLists columns)) inpData
        let firstHead = ref INilRow
        let build = List.fold listBuilder (Result firstHead) tmp
        match build with
        | Error(e) -> Error(e)
        | Result _ -> Result firstHead

    // Takes a line from a textfile, represented as a single string, and splits it on spaces and tabs
    // then removes whitespace
    let cleanLine (inpString : string) =
        inpString.Split [|' ';'\t'|] 
        |> Array.toList |> List.filter (fun x -> x <> "")

    // Takes a list of strings, each string representing a line from a textfile.
    // Separate them into words, and uses the first to define the column names
    // for a table, and the others to define the cell values for the rows in the table
    // Returns a pointer to the built-up table
    let readTable inpStrings =
        let splitStrings = List.map cleanLine inpStrings
        let colNames = List.head splitStrings
        let colData = List.tail splitStrings |> List.map strProcess
        buildData colNames colData

    // Used to find when one Table starts and another ends
    let rec allInOneTable inpStrings rowList =
        match inpStrings with
        | [] -> (List.rev rowList,[])
        | a :: tail when a = "TABLE" -> (List.rev rowList,inpStrings)
        | a :: tail -> allInOneTable tail (a :: rowList)

    // Takes a list of input strings, and returns a list of tuples of table names and a string list, where this
    // lower string list has the input lines for column types and the cell rows
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

    // Called by splitIntoThreeLists in the fold function
    let splitIntoThreeListsHelper (tabName, typeData) acc =
        match acc, typeData with
        | Error(e), _ -> Error(e)
        | Result (nameAcc, typeAcc, dataAcc), typeString :: dataString ->
            Result (tabName :: nameAcc, typeString :: typeAcc, (typeString :: dataString) :: dataAcc)
        | _,_ -> Error("A table must have data types specified") 

    // splits a list of tuples of table names and column and cell data into three lists
    // The first has the table names
    // The second has the table types
    // The third has tuples of types and cell data
    let splitIntoThreeLists inpList =
        List.foldBack splitIntoThreeListsHelper inpList (Result ([], [], []))

    // Used to extract the types from a list of strings by taking the string for column data type,
    // adding that to a list with "None" and calling strProcess. That will return a list with a 
    // single BoxData value, which is the column type
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

    // Recursive function called by buildDatabase
    let rec buildDatabaseRec tableList tableNames tableTypes thisNode =
        match tableList, tableNames, tableTypes with
        | ([],[],[]) -> Result ()
        | thisTable :: tableTail, thisName :: nameTail, theseTypes :: typeTail ->
            let nextNode = ref INilTable
            thisNode := TableNode (thisTable, thisName, theseTypes, nextNode )
            buildDatabaseRec tableTail nameTail typeTail nextNode
        | _ -> Error "READ IN: Unequal amount of tables, table names and table type specifications given"

    // Function called by buildDatabaseWrapper
    let buildDatabase tableList tableNames tableTypes =
        let firstNode = ref INilTable
        match buildDatabaseRec tableList tableNames tableTypes firstNode with
        | Error e -> Error e
        | Result _ -> Result firstNode
    
    // Changes a list of returnCodes to a Returncode of list. If any of the list elements
    // are errors then those errors are returned, otherwise the list od the successful values is returned
    let returnCodeListFlattener (lis : ReturnCode<'a> list) =
        List.foldBack (fun elRes accRes -> match elRes,accRes with
                                            | Error(e1), Error(e2) -> Error(e1+e2)
                                            | Error(e1), _ -> Error(e1)
                                            | _, Error(e2) -> Error(e2)
                                            | Result el, Result acc -> Result (el :: acc)   ) lis (Result []) 

    // Takes the input strings for a whole database, and
    // - splits it into lists of table names, column names and types, and cell lists (rows). This is decodeRed
    // - extracts types by calling getTypes
    // - extracts tables by calling readTable
    // - builds a database by combining tables, table names and table types.
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

    // Loads the database stored at pathName. pathName should be specified as @""
    // and can be either relative or absolute paths
    let load pathName = 
        pathName |> IO.File.ReadLines |> Seq.toList |> buildDatabaseWrapper





///////////////////////////////////////////////////
// Functions to save a database to a text file.

    // Takes a list of cell values and returns a list of string representations of these. 
    let rec strBuilder = function
        | [] -> []
        | String (Some a) :: tail -> 
            let num = a.Split [|' ';'\t'|] |> Seq.length
            "String Some " + string num + " " + a :: strBuilder tail
        | String None     :: tail -> "String None"             :: strBuilder tail
        | Int (Some a)    :: tail -> "Int Some " + string a    :: strBuilder tail
        | Int None        :: tail -> "Int None"                :: strBuilder tail
        | Float (Some a)  :: tail -> "Float Some " + string a  :: strBuilder tail
        | Float None      :: tail -> "Float None"              :: strBuilder tail
        | Byte (Some a)   :: tail -> "Byte Some " + string a   :: strBuilder tail
        | Byte None       :: tail -> "Byte None"               :: strBuilder tail
        | Bool (Some a)   :: tail -> "Bool Some " + string a   :: strBuilder tail
        | Bool None       :: tail -> "Bool None"               :: strBuilder tail

    // Builds a string by combining a list of string representations of cell values, adding spaces between values
    let rec buildLine lineList = 
        match lineList with
        | [] -> ""
        | el :: tail -> el + " " + (buildLine tail)

    // Builds a list of strings, each representing a single row of cells. 
    let buildOutList myData =
        myData |> List.map strBuilder |> List.map buildLine

    // Takes a linked list of cells and builds a list of cell values
    // Extracts away internal structure of how database is saved
    let rec extractBoxValues boxList =
        match !boxList with
        | INilBox -> []
        | BoxNode (colName, value, _, tail) -> value :: extractBoxValues tail

    // Takes a linked list of rows and builds a list of lists of cell values
    // Extracts away internal structure of how database is saved
    let rec extractRowValues rowList =
        match !rowList with
        | INilRow -> []
        | RowNode (thisRow, nextRow) -> extractBoxValues thisRow :: extractRowValues nextRow

    // Creates a string with column names and column strings
    let rec extractBoxTypes ( columns : (string * boxData) list ) = 
        match columns with
        | [] -> ""
        | (colName, String _) :: tail -> colName + " String " + extractBoxTypes tail
        | (colName, Int    _) :: tail -> colName + " Int    " + extractBoxTypes tail
        | (colName, Float  _) :: tail -> colName + " Float  " + extractBoxTypes tail
        | (colName, Byte   _) :: tail -> colName + " Byte   " + extractBoxTypes tail
        | (colName, Bool   _) :: tail -> colName + " Bool   " + extractBoxTypes tail

    // Converts a table to a list of strings
    let tableToStrings thisTable =
        match !thisTable with 
        | INilTable -> []
        | TableNode (thisTable, tableName, tableTypes, nextTable) ->
            "TABLE" :: tableName :: extractBoxTypes tableTypes :: (buildOutList (extractRowValues thisTable) )

    // Converts a database to a list of strings
    let rec saveDatabaseStrings database =
        match !database with
        | INilTable -> []
        | TableNode (thisTable, tableName, tableTypes, nextTable) ->
            (tableToStrings database) @ saveDatabaseStrings nextTable
            
    // Saves a database into a text file.
    let save path database =
       File.WriteAllLines (path, saveDatabaseStrings database |> List.toSeq) |> Result