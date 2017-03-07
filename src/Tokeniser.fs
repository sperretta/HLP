namespace Tokeniser
module Tokeniser =
   ///A list of tokens
   type tokens = Token.content list

   ///Convert a list of characters into a string
   let charListToString (lst:char list) =
      lst
      |> List.toArray
      |> System.String

   ///Convert a single character into a string
   let charToString (ch:char) =
      [| ch |]
      |> System.String

   ///Check if a character is whitespace
   let isWhitespace (ch:char) =
      let whitespace = [| '\n' ; '\f' ; '\r' ; '\t' ; ' ' |]
      Array.contains ch whitespace

   ///Check if a character is the end of a statement
   let isEndStatement (ch:char) =
      if ch = ';' then true else false

   //Useful constants
   let maths_operators = [| '+' ; '-' ; '*' ; '/' |]
   let operators  = [| '=' ;  '>' ; '<' ; ',' ; '(' ; ')' |]
   let operators2 = [| "<>" ; ">=" ; "<=" |]
   let numbers = [| '0' .. '9' |]
   let alpha = Array.concat [ [| 'a' .. 'z' |] ; [| 'A' .. 'Z' |] ]
   let alphanum = Array.concat [ alpha ; numbers ]

   ///Check if a character terminates a name or number match
   let isValidStop (ch:char) =
      let validStopChars = Array.concat [ maths_operators ; operators ]
      Array.contains ch validStopChars || isWhitespace ch || isEndStatement ch

   ///Pattern match for operators
   ///Returns string of operator and remaining characters if successful
   let (|OpMatch|_|) (str:char list) =
      ///Check if a character is a single length operator
      let isOp1 (ch:char) =
         Array.concat [maths_operators ; operators]
         |> Array.contains ch
      ///Pattern match for two character length operators
      ///Return string form of operator (and remaining characters) if match occurs
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

   ///Pattern match for integers
   ///Returns integer and remaining characters if successful
   let (|NumMatch|_|) (str:char list) =
      ///Parse through character list extracting set of consecutive numbers
      let rec parse (outLst:char list) (inLst:char list) =
         match inLst with
         | ch :: rest when Array.contains ch numbers -> parse (ch :: outLst) rest
         | ch :: rest when isValidStop ch && outLst.Length <> 0 -> Some(outLst,rest)
         | [] when outLst.Length <> 0 -> Some(outLst,[])
         | _ -> None
      ///Convert parsed number into usable numerical form
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

   ///Pattern match for floating point numbers
   ///Returns float and remaining characters if successful
   let (|FloatMatch|_|) (str:char list) =
      ///Parse through character list extracting set of consecutive numbers, must include a point
      let rec parse (outLst:char list) (inLst:char list) (point:bool) (goodFloat:bool) =
         match inLst with
         | ch :: rest when Array.contains ch numbers ->
            if point && not goodFloat then
               parse (ch :: outLst) rest point true
            else
               parse (ch :: outLst) rest point goodFloat
         | ch :: rest when (ch = '.') && (not point) -> parse (ch :: outLst) rest true false
         | ch :: rest when goodFloat && (isValidStop ch) -> Some(outLst,rest)
         | [] when goodFloat -> Some(outLst,[])
         | _ -> None
      ///Convert parsed number into usable numerical form
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

   ///Pattern match for literals (strings encapsulated in quotation marks)
   ///Returns string and remaining characters if successful
   let (|LitMatch|_|) (str:char list) =
      ///Process output of parser
      let output (litLst:char list) (rest:char list) =
         List.rev litLst
         |> charListToString
         |> fun x ->
             if x.Length < 256 then (x,rest) else failwithf "Literal is too long"
      ///Parse through character list until find the terminating character (")
      let rec parse (outLst:char list) (inLst:char list) =
         match inLst with
         | ch :: rest when ch = '"' ->
            match rest with
            | ch2 :: tail when isValidStop ch2 -> output outLst (ch2 :: tail)
            | [] -> output outLst []
            | _ -> failwithf "Error, invalid literal (need termination between final \" and next character)"
         | ch :: rest -> parse (ch :: outLst) rest
         | [] -> failwithf "Error, literal not finished"
      match str with
      | '"' :: rest -> Some(parse [] rest)
      | _ -> None

   ///Pattern match for bytes, format: <number>B
   ///Returns string and remaining characters if successful
   let (|ByteMatch|_|) (str:char list) =
      ///Process output of parser
      let output (litLst:char list) (rest:char list) =
         litLst
         |> List.rev
         |> charListToString
         |> int
         |> fun x -> if x < 256 && x > 0 then Some(byte x,rest) else failwithf "Byte is outside range"
      ///Parse through character list extracting set of consecutive numbers, ending in B
      let rec parse (outLst:char list) (inLst:char list) =
         match inLst with
         | ch :: rest when Array.contains ch numbers -> parse (ch :: outLst) rest
         | ch1 :: rest when ch1 = 'B' ->
            match rest with
            | ch2 :: tail when isValidStop ch2 -> output outLst (ch2 :: tail)
            | [] -> output outLst []
            | _ -> failwithf "Error, invalid byte (need termination between B and next character)"
         | _ -> None
      match str with
      | ch :: rest when Array.contains ch numbers -> parse [] rest
      | _ -> None

   ///Pattern match for booleans, IE True or False
   ///Returns bool and remaining characters if successful
   let (|BoolMatch|_|) (str:char list) =
      ///Check for valid break after literal
      let output (boolean:bool) (rest:char list) =
         match rest with
         | [] -> Some(boolean,rest)
         | ch :: tail when isValidStop ch -> Some(boolean,ch::tail)
         | _ -> None
      match str with
      | 'T' :: 'r' :: 'u' :: 'e' :: rest -> output true rest
      | 'F' :: 'a' :: 'l' :: 's' :: 'e' :: rest -> output false rest
      | _ -> None

   ///Pattern match for names, alphanumeric strings that start with an alphabetic character
   ///Returns string and remaining characters if successful
   let (|NameMatch|_|) (str:char list) =
      ///Pase through character list to collect alphanumeric word
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

   ///Convert string into list of tokens
   let getTokens (str:string) =
      ///Parse through character list, converting into tokens
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
         | NameMatch (name,tail) -> parse (Token.Name(name) :: outLst) tail
         | [] -> outLst
         | error -> failwithf "Unable to parse %A" error
      List.ofSeq str
      |> parse []
      |> List.rev
