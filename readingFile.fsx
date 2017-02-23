    open System.IO
    open System
    type myData = | String of Option<string> | Int of Option<int> | Float of Option<float>

    let myPath = @"C:\Users\Sigrid\Documents\Visual Studio 2015\HLP\dataFile.txt"

    let readFile pathString = IO.File.ReadLines pathString

    let fileLines = readFile myPath


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