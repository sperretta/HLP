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
      | Branch of keyword * Children:node list
      | Item of keyword * Child:node
      | Literal of Token.content
      | Key of keyword
      | Null

   type ReturnCode<'a> =
      | Result of 'a
      | Error of string

   let ReturnWrapper<'a> (func:'a->ReturnCode<'a>) =
      let newFunc (data:ReturnCode<'a>) =
         match data with
         | Result(data) ->
            func data
         | error -> error
      newFunc

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
            [node.Item(keyword.Function,Literal(Token.content.Name(funName))) ; node.Item(keyword.Name,Literal(Token.content.Name(colName)))]
            |> fun x -> Some(x,rest)
         | _ -> None
      let (|AliasMatch|_|) (lst:Tokeniser.tokens) =
         match lst with
         | Token.content.Name(colName) :: Token.content.Name("AS") :: Token.content.Literal(colAlias) :: rest ->
            [node.Item(keyword.Name,Literal(Token.content.Name(colName))) ; node.Item(keyword.Alias,Literal(Token.content.Literal(colAlias)))]
            |> fun x -> Some(x,rest)
         | _ -> None
      let rec parse (lst:Tokeniser.tokens) (nextColumn:bool) : ReturnCode<node list*Tokeniser.tokens> =
         match lst with
         | FunctionMatch (funLst,rest) when nextColumn ->
            processTupleConsOnReturn (Branch(keyword.Function,funLst)) (parse rest false)
         | AliasMatch (aliasLst,rest) when nextColumn ->
            processTupleConsOnReturn (Branch(keyword.Alias,aliasLst)) (parse rest false)
         | Token.content.Name(name) :: rest when nextColumn ->
            processTupleConsOnReturn (Item(keyword.Name,Literal(Token.content.Name(name)))) (parse rest false)
         | Token.content.Operator(",") :: rest when not nextColumn -> parse rest true
         | item :: rest when nextColumn -> Error(sprintf "Expected wrapped column name, got %A" item)
         | rest -> Result([],rest)
      parse tokenList true
      |> fun x ->
         match x with
         | Result(columnLst,rest) -> Result(node.Branch(keyword.Column,columnLst),rest)
         | Error(error) -> Error(error)

   //let TableList (tokenList

   let (|BranchMatch|_|) (tokenList:Tokeniser.tokens) =
      let output (key:keyword) (result:ReturnCode<node*Tokeniser.tokens>) =
         match result with
         | Results(res) ->
            (key,fst res,snd res)
            |> fun x->
               Some(ReturnCode<node*node list*Tokeniser.tokens>.Results(x))
         | Error(s) ->
            Some(ReturnCode<node*node list*Tokeniser.tokens>.Error(s))
      let selectParse (tokenList:Tokeniser.tokens) =
         tokenList
         |> ColumnWrappedList
         //?? Need to change how it is piped, it returns something that should be in the body, pass rest to cons.
         |> ReturnWrapper<node*Tokeniser.tokens> TableList
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
         | Token.content.Name("SELECT") -> Some(output (Key(keyword.Select)) (selectParse rest))
         | Token.content.Name("INSERT") -> Some(output (Key(keyword.Insert)) (insertParse rest))
         | Token.content.Name("UPDATE") -> Some(output (Key(keyword.Update)) (updateParse rest))
         | Token.content.Name("SET") -> Some(output (Key(keyword.Set)) (setParse rest))
         | Token.content.Name("DECLARE") -> Some(output (Key(keyword.Declare)) (declareParse rest))
         | Token.content.Name("DELETE") -> Some(output (Key(keyword.Delete)) (deleteParse rest))
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