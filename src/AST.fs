namespace parser
module AST =
   open Tokeniser
   type keyword =
      | Select
      | Insert
      | Update
      | Set
      | Declare
      | Delete
      | From
      | Where
      | Order
      | Limit
      | Offset
      | Value
      | Function
      | Name
      | As
      | Condition
      | Variable
      | Column
      | Alias
      | And
      | Or

   type node =
      | Branch of Name:node * Children:node list
      | Item of Name:node * Child:node
      | Literal of Token.content
      | Key of keyword
      | Null

   type ReturnCode<'a> =
      | Result of 'a
      | Error of string

   let ReturnWrapper (func:(Tokeniser.tokens->ReturnCode<node*Tokeniser.tokens>)) (data:ReturnCode<node list*Tokeniser.tokens>) : ReturnCode<node list*Tokeniser.tokens> =
      match data with
      | Result(res) ->
         snd res
         |> func
         |> fun x ->
            match x with
            | Result(nod,tLst) -> Result(((fst res) @ [nod]),tLst)
            | Error(s) -> Error(s)
      | Error(s) -> Error (s)

   let OptionalReturnWrapper (|MatchFunc|_|) (data:ReturnCode<node list*Tokeniser.tokens>) =
      match data with
      | Result(nLst,tLst) ->
         match tLst with
         | MatchFunc matchedResult ->
            match matchedResult with
            | Result(nod,newTLst) ->
               Result((nLst @ [nod]),newTLst)
            | Error(s) -> Error(s)
         | _ -> Result(nLst,tLst)
      | Error(s) -> Error(s)

   let processTupleConsOnReturn (item:'a) (rest:ReturnCode<'a list*'b>) : ReturnCode<'a list*'b> =
      match rest with
      | Result(res,rest) -> Result(item :: res,rest)
      | Error(s) -> Error(s)

   let processConsOnReturn (item:'a) (rest:ReturnCode<'a list>) : ReturnCode<'a list> =
      match rest with
      | Result(res) -> Result(item::res)
      | error -> error

   let ColumnWrappedList (tokenList:Tokeniser.tokens) : ReturnCode<node*Tokeniser.tokens> =
      let (|FunctionMatch|_|) (lst:Tokeniser.tokens) =
         let validFunctions = [| "AVG" ; "MAX" ; "MIN" ; "SUM" ; "ROUND" |]
         match lst with
         | Token.content.Name(funName) :: Token.content.Operator("(") :: Token.content.Name(colName) :: Token.content.Operator(")") :: rest when Array.contains funName validFunctions ->
            [node.Item(Key(Function),Literal(Token.content.Name(funName))) ; node.Item(Key(Name),Literal(Token.content.Name(colName)))]
            |> fun x -> Some(x,rest)
         | _ -> None
      let (|AliasMatch|_|) (lst:Tokeniser.tokens) =
         match lst with
         | Token.content.Name(colName) :: Token.content.Name("AS") :: Token.content.Literal(colAlias) :: rest ->
            [node.Item(Key(Name),Literal(Token.content.Name(colName))) ; node.Item(Key(Alias),Literal(Token.content.Literal(colAlias)))]
            |> fun x -> Some(x,rest)
         | _ -> None
      let rec parse (lst:Tokeniser.tokens) (nextColumn:bool) : ReturnCode<node list*Tokeniser.tokens> =
         match lst with
         | FunctionMatch (funLst,rest) when nextColumn ->
            processTupleConsOnReturn (Branch(Key(Function),funLst)) (parse rest false)
         | AliasMatch (aliasLst,rest) when nextColumn ->
            processTupleConsOnReturn (Branch(Key(Alias),aliasLst)) (parse rest false)
         | Token.content.Name(name) :: rest when nextColumn ->
            processTupleConsOnReturn (Item(Key(Name),Literal(Token.content.Name(name)))) (parse rest false)
         | Token.content.Operator(",") :: rest when not nextColumn -> parse rest true
         | item :: rest when nextColumn -> Error(sprintf "Expected wrapped column name, got %A" item)
         | rest -> Result([],rest)
      parse tokenList true
      |> fun x ->
         match x with
         | Result(columnLst,rest) -> Result(node.Branch(Key(Column),columnLst),rest)
         | Error(error) -> Error(error)

   let TableList (tokenList:Tokeniser.tokens) : ReturnCode<node*Tokeniser.tokens> =
      let rec parse (lst:Tokeniser.tokens) (nextItem:bool) =
          match lst with
          | Token.content.Name(name) :: rest when nextItem ->
             processTupleConsOnReturn (node.Literal(Token.content.Name(name))) (parse rest false)
          | Token.content.Operator(op) :: rest when op = "," && not nextItem ->
             parse rest true
          | item :: rest when nextItem ->
             Error(sprintf "Expected table name, got %A" item)
          | rest ->
             Result([],rest)
      parse tokenList true
      |> fun x ->
         match x with
         | Result(nLst,tLst) -> Result(node.Branch(Key(From),nLst),tLst)
         | Error (s) -> Error(s)

   let ConditionsList (input:Tokeniser.tokens) : (ReturnCode<node*Tokeniser.tokens> option) =
      let (|ConditionMatch|ConditionError|) (tokenList:Tokeniser.tokens) =
         let isCompareOp str = 
            let compareOperators = [| "=" ; "<>" ; "<" ; ">" ; "<=" ; ">=" |]
            Array.contains str compareOperators
         match tokenList with
         | Token.content.Name(name) :: Token.content.Operator(op) :: rest when isCompareOp op ->
            match rest with
            | Value (value,tail) ->
               [ Item(Name,Token.content.Name(name)) ; Item(Operator,Token.content.Operator(op)) ; value ]
               |> fun x -> ConditionMatch(x,tail)
      let rec parse (tokenList:Tokeniser.tokens) =
         match tokenList with
         | Token.content.Name("AND") :: rest ->
            match rest with
            | ConditionMatch (nod,tail) -> Branch(And,nod) :: parse tail
            | ConditionError str-> Error(str)
         | Token.content.Name("OR") :: rest ->
            match rest with
            | ConditionMatch (nod,tail) -> Branch(Or,nod) :: parse tail
            | ConditionError str-> Error(str)
         | rest -> Result([],rest)
      match input with
      | Token.content.Name("WHERE") :: rest -> Some(
      | _ -> None

   let OrderList (input:Tokeniser.tokens) =

   let LimitItem (input:Tokeniser.tokens) =

   let (|BranchMatch|_|) (tokenList:Tokeniser.tokens) =
      let output (key:Key) (result:ReturnCode<node*Tokeniser.tokens>) =
         match result with
         | Result(res) -> Result(key,fst res,snd res)
         | Error(s) -> Error(s)
      let selectParse (tokenList:Tokeniser.tokens) =
         Result([],tokenList)
         |> ReturnWrapper ColumnWrappedList
         |> ReturnWrapper TableList
         |> OptionalReturnWrapper ConditionsList
         |> OptionalReturnWrapper OrderList
         |> OptionalReturnWrapper LimitItem
         |> fun x -> Some(x)
         //Returns body:node * rest:Tokeniser.tokens
      let insertParse (tokenList:Tokeniser.tokens) =
         //Returns body:node * rest:Tokeniser.tokens
      let updateParse (tokenList:Tokeniser.tokens) =
         //Returns body:node * rest:Tokeniser.tokens
      let setParse (tokenList:Tokeniser.tokens) =
         //Returns body:node * rest:Tokeniser.tokens
      let declareParse (tokenList:Tokeniser.tokens) =
         //Returns body:node * rest:Tokeniser.tokens
      let deleteParse (tokenList:Tokeniser.tokens) =
         //Returns body:node * rest:Tokeniser.tokens
      match tokenList with
      | item :: rest ->
         match item with
         | Token.content.Name("SELECT") -> Some(output (Key(Select)) (selectParse rest))
         | Token.content.Name("INSERT") -> Some(output (Key(Insert)) (insertParse rest))
         | Token.content.Name("UPDATE") -> Some(output (Key(Update)) (updateParse rest))
         | Token.content.Name("SET") -> Some(output (Key(Set)) (setParse rest))
         | Token.content.Name("DECLARE") -> Some(output (Key(Declare)) (declareParse rest))
         | Token.content.Name("DELETE") -> Some(output (Key(Delete)) (deleteParse rest))
         | _ -> None;
      | _ -> None;

   let getTree (tokenList:Tokeniser.tokens) =
      let rec parse (tokens:Tokeniser.tokens) : ReturnCode<node list> =
         match tokens with
         | [] -> ReturnCode<node list>.Result([])
         | BranchMatch returned ->
            match returned with
            | Result(key,body,rest) ->
               processConsOnReturn (node.Branch(key,body)) (parse rest)
            | Error(str) -> Error(str)
         | other ->
            sprintf "Unrecognised sequence %A" other
            |> fun x-> ReturnCode<node list>.Error(x)
      parse tokenList
