module AST =
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

   type 'a ReturnCode =
      | Result of 'a
      | Error of string

   let 'a ReturnWrapper (func:'a->ReturnCode<'a>) =
      let newFunc (data:ReturnCode<'a>) =
         match data with
         | ReturnCode<'a>.Result(data) ->
            func data
         | error -> error
      newFunc

   let processConsOnReturn (item:node) (rest:ReturnCode<node list>) =
      match rest with
      | ReturnCode<node list>.Result(res) -> ReturnCode<node list>.Result(item :: res)
      | ReturnCode<node list>.Error(s) -> ReturnCode<node list>.Error(s)

   let ColumnWrappedList (tokenList:Tokeniser.tokens) =
      let (|FunctionMatch|_|) (lst:Tokeniser.tokens) =
         let validFunctions = [| "AVG" ; "MAX" ; "MIN" ; "SUM" ; "ROUND" |]
         match lst with
         | Token.content.Name(funName) :: Token.content.Operator("(") :: Token.content.Name(colName) :: Token.content.Operator(")") :: rest when Array.contains funName validFunctions ->
            [node.Item(keyword.Function,Token.content.Name(funName)) ; node.Item(keyword.Name,Token.content.Name(colName))]
            |> fun x -> Some(x,rest)
         | _ -> None
      let (|AliasMatch|_|) (lst:Tokeniser.tokens) =
         match lst with
         | Token.content.Name(colName) :: Token.content.Name("AS") :: Token.content.Literal(colAlias) :: rest ->
            [node.Item(keyword.Name,Token.content.Name(colName)) ; node.Item(keyword.Alias,Token.content.Literal(colAlias))]
            |> fun x -> Some(x,rest)
         | _ -> None
      let rec parse (lst:Tokeniser.tokens) (nextColumn:bool) =
         match lst with
         | FunctionMatch (funLst,rest) when nextColumn ->
            processConsOnReturn node.Branch(keyword.Function,funLst) (parse rest false)
         | AliasMatch (aliasLst,rest) when nextColumn ->
            processConsOnReturn node.Branch(keyword.Alias,aliasLst) (parse rest false)
         | Token.content.Name(name) when nextColumn ->
            processConsOnReturn node.Item(keyword.Name,node.Literal(Token.content.Name(name))) (parse rest false)
         | Token.content.Operator(",") :: rest when not nextColumn -> parse rest true
         | item :: rest when nextColumn -> ReturnCode<node list>.Error(printfn "Expected wrapped column name, got %A" item)
         | _ -> ReturnCode<node list>.Result([])
      parse tokenList true
      |> fun x ->
         match x with
         | ReturnCode<node list>.Result(columnLst,rest) -> ReturnCode<node*Tokeniser.tokens>.Result(node.Branch(keyword.Column,columnLst),rest)
         | ReturnCode<node list>.Error(str) -> ReturnCode<node*Tokeniser.tokens>.Error(str)

   let TableList (tokenList

   let (|BranchMatch|_|) (tokenList:Tokeniser.tokens) =
      let output (key:keyword) (result:ReturnCode<node*Tokeniser.tokens>) =
         match result with
         | ReturnCode<node*Tokeniser.tokens>.Results(res) ->
            (key,fst res,snd res)
            |> fun x->
               Some(ReturnCode<node*node list*Tokeniser.tokens>.Results(x))
         | ReturnCode<node*Tokeniser.tokens>.Error(s) ->
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
         | Token.content.Name("SELECT") -> Some(output node.Key(keyword.Select) (selectParse rest))
         | Token.content.Name("INSERT") -> Some(output node.Key(keyword.Insert) (insertParse rest))
         | Token.content.Name("UPDATE") -> Some(output node.Key(keyword.Update) (updateParse rest))
         | Token.content.Name("SET") -> Some(output node.Key(keyword.Set) (setParse rest))
         | Token.content.Name("DECLARE") -> Some(output node.Key(keyword.Declare) (declareParse rest))
         | Token.content.Name("DELETE") -> Some(output node.Key(keyword.Delete) (deleteParse rest))
         | _ -> None;
      | _ -> None;

   let getTree (tokenList:Tokeniser.tokens) =
      let rec parse (tokens:Tokeniser.tokens) =
         match tokens with
         | [] -> ReturnCode<node list>.Result([])
         | BranchMatch returned ->
            match returned with
            | ReturnCode<node*node list*Tokeniser.tokens>.Result(key,body,rest) ->
               ReturnCode<node list>.Result(node.Branch(key,body) :: parse(rest))
            | ReturnCode<node*node list*Tokeniser.tokens>.Error(str) -> ReturnCode<node list>.Error(str)
         | other ->
            printfn "Unrecognised sequence %A" other
            |> fun x-> ReturnCode<node list>.Error(x)
      parse tokenList
