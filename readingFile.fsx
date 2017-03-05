    open System.IO
    open System
    type myData = | String of Option<string> | Int of Option<int> | Float of Option<float>                // To be extended when the wanted data types have been decided
                  | Byte of Option<byte>     | Bool of Option<bool>
    type IList = INode of ParName: string * Value: myData * Prev : ref<IList> * Tl : ref<IList> | INilLow // A node has the parameter name (column name), the parameter value
                                                                                                          // and links to the previous and next nodes
    type topList = Node of lowList : ref<IList> * Tl : ref<topList> | INilTop                             // The nodes of the top-level list have refs to low-level lists.
    type data = ref<topList>                                                                              // Holds all the data in the database
     


    let head1  = ref INilLow
    let tail1  = ref INilLow
    let test2 = ref INilLow
    let test1 = ref (INode ("username", (Some >> String) "Orlando", head1, test2))
    test2 := INode ("age", (int >> Some >> Int) "45", test1, tail1)
    test1

    let head2 = ref INilLow
    let tail2 = ref INilLow
    let test4 = ref INilLow
    let test3 = ref (INode ("username", (Some >> String) "Rebecca", head2, test4))

    test4 := INode ("age", (int >> Some >> Int) "17", test3, tail2)
    test3
    
    let tmpT  = ref INilTop
    let tailT = ref INilTop
    let myTopList = (Node (test1, tmpT))
    tmpT := Node (test3, tailT)
    myTopList

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
         
    // Tests
    strProcess ["String"; "Some"; "Orlando"; "Int"; "Some"; "45"]
    strProcess ["Byte"; "Some"; "0xF3"; "Byte"; "None"]
    strProcess ["Bool"; "Some"; "True"; "Bool"; "None"]
    strProcess ["Float"; "Some"; "3.1415926"; "Float"; "None"]
    strProcess ["String"; "some"; "hei"]
    strProcess ["Strange"; "Some"; "Dragon"]


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

    let buildOutSeq myData =
        myData |> List.map strBuilder |> List.map buildLine |> Seq.ofList

    let readInLine (inpString : string) =
        inpString.Split [|' ';'\t'|] 
        |> Array.toList |> List.filter (fun x -> x <> "")
        |> strProcess


    let readIn mySeq =
        mySeq |> Seq.toList |> List.map readInLine

    let myPath  = @"C:\Users\Sigrid\Documents\Visual Studio 2015\HLP\dataFile.txt"
    let outPath = @"C:\Users\Sigrid\Documents\Visual Studio 2015\HLP\outFile.txt"

    let readFile pathString = IO.File.ReadLines pathString

    let fileLines = readFile myPath

    let ReadInData myPath = 
        myPath |> IO.File.ReadLines |> readIn

    let k = readIn fileLines
    let j = ReadInData myPath

    let s = strBuilder (List.head j )
    let t = buildLine s
    let u = buildOutSeq j
    File.WriteAllLines(outPath, u)
    let v = ReadInData outPath


 
        
    printfn "%A" fileLines
    printfn "%A" k
    printfn "%A" j