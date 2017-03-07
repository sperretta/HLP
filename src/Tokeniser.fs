namespace Tokeniser
module Tokeniser =
   type tokens = Token.content list

   let charListToString (lst:char list) =
      lst
      |> List.toArray
      |> System.String

   let charToString (ch:char) =
      [| ch |]
      |> System.String

   let isWhitespace (ch:char) =
      let whitespace = [| '\n' ; '\f' ; '\r' ; '\t' ; ' ' |]
      Array.contains ch whitespace

   let isEndStatement (ch:char) =
      if ch = ';' then true else false

   let maths_operators = [| '+' ; '-' ; '*' ; '/' |]
   let operators  = [| '=' ;  '>' ; '<' ; ',' ; '(' ; ')' |]
   let operators2 = [| "<>" ; ">=" ; "<=" |]

   let isValidStop (ch:char) =
      let validStopChars = Array.concat [ maths_operators ; operators ]
      (Array.contains ch validStopChars) || (isWhitespace ch) || (isEndStatement ch)

   let (|OpMatch|_|) (str:char list) =
      let isOp1 (ch:char) =
         Array.concat [maths_operators ; operators]
         |> Array.contains ch
      let (|Match2Ops|_|) = function
         | ch1 :: ch2 :: rest ->
            let opStr =
               [ ch1 ; ch2 ]
               |> charListToString
            if Array.contains opStr operators2 then
               Some(opStr,rest)
            else
               None
         | _ -> None
      match str with
      | ch1 :: rest when isOp1 ch1 ->
         Some(charToString ch1,rest)
      | Match2Ops (opStr,rest) -> Some(opStr,rest)
      | _ -> None

   let (|NumMatch|_|) (str:char list) =
      let numbers = [| '0' .. '9' |]
      let rec parse (outLst:char list) (inLst:char list) =
         match inLst with
         | ch :: rest when Array.contains ch numbers -> parse (ch :: outLst) rest
         | ch :: rest when isValidStop ch -> Some(outLst,rest)
         | [] when outLst.Length <> 0 -> Some(outLst,[])
         | _ -> None
      let output (info: (char list * char list) option) =
         match info with
         | None -> None
         | Some(revNum,rest) ->
            revNum
            |> List.rev
            |> charListToString
            |> int
            |> fun x -> Some(x,rest)
      parse [] str
      |> output

   let (|FloatMatch|_|) (str:char list) =
      let numbers = [| '0' .. '9' |]
      let rec parse (outLst:char list) (inLst:char list) (point:bool) (goodFloat:bool) =
         match inLst with
         | ch :: rest when Array.contains ch numbers ->
            if point && not goodFloat then
               parse (ch :: outLst) rest point true
            else
               parse (ch :: outLst) rest point goodFloat
         | ch :: rest when (ch = '.') && (not point) -> parse (ch :: outLst) rest true false
         | ch :: rest when goodFloat && (isWhitespace ch) -> Some(outLst,rest)
         | [] when goodFloat -> Some(outLst,[])
         | _ -> None
      let output (info: (char list * char list) option) =
         match info with
         | None -> None
         | Some(revNum,rest) ->
            if not (List.contains '.' revNum) then
               '0' :: '.' :: revNum
            else revNum
            |> List.rev
            |> charListToString
            |> float
            |> fun x -> Some(x,rest)
      parse [] str false false
      |> output

   let (|LitMatch|_|) (str:char list) = //?? Need to convert to char and literal - char being '' and literal being ""
      let rec parse (outLst:char list) (inLst:char list) =
         match inLst with
         | ch :: rest when ch = '"' ->
            List.rev outLst
            |> charListToString
            |> fun x ->
                if x.Length < 256 then (x,rest) else failwithf "Literal is too long"
         | ch :: rest -> parse (ch :: outLst) rest
         | [] -> failwithf "Error, literal not finished"
      match str with
      | '"' :: rest -> Some(parse [] rest)
      | _ -> None

   let (|ByteMatch|_|) (str:char list) =
      match str with
      | '\'' :: mid ->
         match mid with
         | byt :: '\'' :: 'B' :: rest when byt <> '\'' -> Some(byte(byt),rest)
         | ch1 :: ch2 :: rest ->
            printfn "Expected ' got %A" ch2
            failwithf "Error"
         | _ -> failwithf "Error, expected '"
      | _ -> None

   let (|CharMatch|_|) (str:char list) =
      match str with
      | '\'' :: mid ->
         match mid with
         | ch :: '\'' :: rest when ch <> '\'' -> Some(ch,rest)
         | ch1 :: ch2 :: rest ->
            printfn "Expected ' got %A" ch2
            failwithf "Error"
         | _ -> failwithf "Error, char not finished"
      | _ -> None

   let (|BoolMatch|_|) (str:char list) =
      match str with
      | 'T' :: 'r' :: 'u' :: 'e' :: rest -> Some(true,rest)
      | 'F' :: 'a' :: 'l' :: 's' :: 'e' :: rest -> Some(false,rest)
      | _ -> None

   let (|NameMatch|_|) (str:char list) =
      let alpha = Array.concat [ [| 'a' .. 'z' |] ; [| 'A' .. 'Z' |] ]
      let alphanum = Array.concat [ alpha ; [| '0' .. '9' |] ]
      let rec parse (outLst:char list) (inLst:char list) =
         match inLst with
         | ch :: rest when Array.contains ch alphanum -> parse (ch :: outLst) rest
         | ch :: rest when isWhitespace ch -> Some(outLst,rest)
         | ch :: rest when isValidStop ch -> Some(outLst,ch :: rest)
         | [] -> Some(outLst,[])
         | _ -> None
      match str with
      | ch :: rest when Array.contains ch alpha ->
         match parse [] (ch :: rest) with
         | None -> None
         | Some(name,rest) ->
            List.rev name
            |> charListToString
            |> fun x -> Some(x,rest)
      | _ -> None

   let getTokens (str:string) =
      let rec parse (outLst:tokens) (inStr:char list) =
         match inStr with
         | ch :: tail when isWhitespace ch -> parse outLst tail
         | ch :: tail when isEndStatement ch -> parse (Token.EndStatement :: outLst) tail
         | OpMatch (op,tail) -> parse (Token.Operator(op) :: outLst) tail
         | NumMatch (num,tail) -> parse (Token.Value(Token.value.Integer(num)) :: outLst) tail
         | FloatMatch (num,tail) -> parse (Token.Value(Token.value.Floating(num)) :: outLst) tail
         | LitMatch (lit,tail) -> parse (Token.Literal(lit) :: outLst) tail
         | ByteMatch (byt,tail) -> parse (Token.Value(Token.value.Byte(byt)) :: outLst) tail
         | BoolMatch (boolean,tail) -> parse (Token.Value(Token.value.Boolean(boolean)) :: outLst) tail
         | CharMatch (ch,tail) -> parse (Token.Value(Token.value.Character(ch)) :: outLst) tail
         | NameMatch (name,tail) -> parse (Token.Name(name) :: outLst) tail
         | [] -> outLst
         | error -> failwithf "Unable to parse %A" error
      List.ofSeq str
      |> parse []
      |> List.rev
