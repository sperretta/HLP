    open System.IO
    open System
    type myData = | String of Option<string> | Int of Option<int> | Float of Option<float>                // To be extended when the wanted data types have been decided
    type IList = INode of ParName: string * Value: myData * Prev : ref<IList> * Tl : ref<IList> | INilLow // A node has the parameter name (column name), the parameter value
                                                                                                          // and links to the previous and next nodes
    type topList = Node of lowList : ref<IList> * Tl : ref<topList> | INilTop                             // The nodes of the top-level list have refs to low-level lists.
    type data = ref<topList>                                                                              // Holds all the data in the database
     

    let myPath = @"C:\Users\Sigrid\Documents\Visual Studio 2015\HLP\dataFile.txt"

    let readFile pathString = IO.File.ReadLines pathString

    let fileLines = readFile myPath

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

    let rec strProcess = function
        | [] -> []
        | a :: b :: c :: tail when a = "String" -> 
            if b = "Some" then (Some >> String) c :: (strProcess tail)
            else String None :: strProcess (c :: tail)
        | a :: b :: c :: tail when a = "Int"    -> 
            if b = "Some" then (int >> Some >> Int) c :: (strProcess tail) 
            else Int None :: strProcess (c :: tail)
        | a :: b :: c :: tail when a = "Float"  -> 
            if b = "Some" then (float >> Some >> Float) c :: (strProcess tail) 
            else Float None :: strProcess (c :: tail)
        | e :: tail                             -> (Some >> String) "error" :: (strProcess tail)

    strProcess ["String"; "Some"; "Orlando"; "Int"; "Some"; "45"]

    let readInLine (inpString : string) =
        inpString.Split [|' '|] 
        |> Array.toList |> List.filter (fun x -> x <> "")
        |> strProcess

    let readIn mySeq =
        mySeq |> Seq.toList |> List.map readInLine

    let k = readIn fileLines
    
    printfn "%A" fileLines
    printfn "%A" k