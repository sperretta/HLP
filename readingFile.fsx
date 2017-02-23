    open System.IO
    open System
    type myData = | String of string | Int of int | Float of float

    let myPath = @"C:\Users\Sigrid\Documents\Visual Studio 2015\HLP\dataFile.txt"

    let readFile pathString = IO.File.ReadLines pathString

    let fileLines = readFile myPath
    printfn "%A" fileLines

    let rec strProcess = function
        | [] -> []
        | a :: b :: tail when a = "String" -> [String b] :: (strProcess tail)
        | a :: b :: tail when a = "Int"    -> [Int (int b)] :: (strProcess tail) 
        | a :: b :: tail when a = "Float"  -> [Float (float b)] :: (strProcess tail) 
        | e :: tail                        -> [String "error"] :: (strProcess tail)

    let readInLine (inpString : string) =
        inpString.Split [|' '|] 
        |> Array.toList |> List.filter (fun x -> x <> "")
        |> strProcess

    let readIn mySeq =
        mySeq |> Seq.toList |> List.map readInLine

    let k = readIn fileLines
    printfn "%A" k