// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.IO
open System
type myData = | String of Option<string> | Int of Option<int> | Float of Option<float>                // To be extended when the wanted data types have been decided
              | Byte of Option<byte>     | Bool of Option<bool>
type IList = INode of ParName: string * Value: myData * Prev : ref<IList> * Tl : ref<IList> | INilLow // A node has the parameter name (column name), the parameter value
                                                                                                          // and links to the previous and next nodes
type topList = Node of lowList : ref<IList> * Tl : ref<topList> | INilTop                             // The nodes of the top-level list have refs to low-level lists.
type data = ref<topList> 

open Expecto



let fsConfig = { FsCheck.Config.Default with MaxTest = 2000 }


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

//let myPath  = @"C:\Users\Sigrid\Documents\Visual Studio 2015\HLP\dataFile.txt"
//let outPath = @"C:\Users\Sigrid\Documents\Visual Studio 2015\HLP\outFile.txt"

let readFile pathString = IO.File.ReadLines pathString

//let fileLines = readFile myPath

let ReadInData myPath = 
    myPath |> IO.File.ReadLines |> readIn
    
//let k = ReadInData myPath
//printfn "%A" k


[<Tests>]
let tests =
  testList "A test group" [
    testCase "test 1" <| fun _ ->
      Expect.equal (List.map readInLine ["String Some Orlando"]) ([[String (Some "Orlando")]]) "Read in a string"
    testCase "test 2" <| fun _ ->
      Expect.equal (List.map readInLine ["String Some Orlando Int Some 45 Float Some 0.2 Byte Some 0x23 Bool None";"String Some Rebecca Int Some 17 Float Some 3.14159 Byte None Bool Some True"]) ([[String (Some "Orlando"); Int (Some 45); Float (Some 0.2); Byte (Some 0x23uy); Bool None];[String (Some "Rebecca"); Int (Some 17); Float (Some 3.14159); Byte None; Bool (Some true)]]) "Read in a longer string"
    testCase "test 3" <| fun _ -> 
      Expect.equal (List.map readInLine ["String Some Alice     Int Some 18"]) ([[String (Some "Alice"); Int (Some 18)]]) "Read in a string with tabs"
  ]

let properties =
    testList "FsCheck" [
        testProperty "Addition is commutative" <| fun a ->
        a = readInLine (a |> strBuilder |> buildLine)

    ]


[<EntryPoint>]
let main args =
  printfn "--------------------"
  printfn "Property based tests"
  printfn "--------------------"
  runTests defaultConfig  properties |> ignore
  printfn "\n------------"
  printfn "Ad-hoc tests"
  printfn "------------"
  runTests defaultConfig  tests |> ignore

  



  printfn "Press any key to EXIT"
  System.Console.ReadKey() |> ignore
  0