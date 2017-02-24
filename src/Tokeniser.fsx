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
      if ch = ";" then true else false

   let (|OpMatch|_|) (str:char list) =
      let maths_operators = [| '+' ; '-' ; '*' ; '/' |]
      let operators  = [| '=' ;  '>' ; '<' |]
      let operators2 = [| '<>' ; '>=' ; '<=' |]
      let isOp1 (ch:char) =
         Array.concat [maths_operators ; operators]
         |> Array.contains ch
      let (|Match2Ops|_|) (lst:char list) = function
         | ch1 :: ch2 :: rest ->
            let opStr =
               [| ch1 ; ch2 |]
               |> charArrayToString
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
      let numbers = [| 0 .. 9 |]
      let rec parse (outLst:char list) (inLst:char list) (point:bool) (goodFloat:bool) =
         match inLst with
         | ch :: rest when Array.contains ch numbers -> parse (ch :: outLst) rest point goodFloat
            if point && not goodFloat then
               parse (ch :: outLst) rest point true
            else
               parse (ch :: outLst) rest point goodFloat
         | ch :: rest when (ch = '.') && (not point) -> parse (ch :: outLst) rest true false
         | ch :: rest when goodFloat && (isWhitespace ch) -> Some(outLst,rest)
         | [] when goodFloat -> Some(outLst,[])
         | _ -> None
      let output (info: (char list * char list) option) = function
         | None -> None
         | Some(revNum,rest) ->
            if not (List.contains "." revNum) then
               "0" :: "." :: revNum
            else revNum
            |> List.rev
            |> charListToString
            |> float
            |> fun x -> Some(x,rest)
      parse [] str false false
      |> output

   let (|LitMatch|_|) (str:char list) =
      let containers = [| "\"" ; "'" |]
      let rec parse (outLst:char list) (inLst:char list) (container:char) =
         match inLst with
         | ch :: rest when ch = container -> (outLst,rest)
         | ch :: rest -> parse (ch :: outLst) rest container
         | [] -> printfn "Error, literal not finished"
      match str with
      | ch :: rest when Array.contains ch containers -> Some(parse [] rest ch)
      | _ -> None

   let getTokens (str:string) =
      let rec parse (outLst:tokens list) (inStr:char list) =
         match inStr with
         | ch :: tail when isWhitespace ch -> parse outLst tail
         | ch :: tail when isEndStatement ch -> parse (EndStatement :: outLst) tail
         | OpMatch (op,tail) -> parse (Operator(op) :: outLst) tail
         | NumMatch (num,tail) -> parse (Numeric(num) :: outLst) tail
         | LitMatch (lit,tail) -> parse (Literal(lit) :: outLst) tail
         | NameMatch (name,tail) -> parse (Name(name) :: outLst) tail
         | [] -> outLst
         | error -> printfn "Unable to parse %A" error
      List.ofSeq str
      |> parse []
      |> List.rev