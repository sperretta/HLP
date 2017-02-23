module Tokeniser =
   type tokens = Token.content list

   let whitespace = [| '\n' ; '\f' ; '\r' ; '\t' ; ' ' |]

   let isWhitespace (ch:char) = Array.contains ch whitespace

   let (|OpMatch|_|) (str:char list) =

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
