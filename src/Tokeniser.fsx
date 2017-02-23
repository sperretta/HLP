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
               Some(opStr)
            else
               None
         | _ -> None
      match str with
      | ch1 :: rest when isOp1 ch1 ->
         Some(charToString ch1,rest)
      | Match2Ops opStr -> Some(opStr)
      | _ -> None

   let getTokens (str:string) =
      let rec parse (outLst:tokens list) (inStr:char list) = function
         | ch :: tail when isWhitespace ch -> parse outLst tail
         | OpMatch (op,tail) -> Operator(op) :: parse outLst tail
         | NumMatch (num,tail) -> Numeric(num) :: parse outLst tail
         | LitMatch (lit,tail) -> Literal(lit) :: parse outLst tail
         | NameMatch (name,tail) -> Name(name) :: parse outLst tail
         | error -> printfn "%A" error
      List.ofSeq str
      |> parse []
