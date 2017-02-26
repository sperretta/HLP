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
      | Values
      | Function
      | Name
      | As
      | Condition
      | Variable

   type node =
      | Branch of Name:node * Children:node list
      | Literal of Token.content
      | Key of keyword
      | Null

   type ReturnCode =
      | Result of node
      | Error of string

   let ColumnWrappedList (tokenList:Tokeniser.tokens) = //??
      let rec parse (lst:Tokeniser.tokens) (nextColumn:bool) =
         match lst with
         | Token.content.Name(columnName) :: rest when nextColumn -> node.Literal(Token.content.Name(columnName)) :: parse rest false
         | Token.content.Operator(",") :: rest when not nextColumn -> parse rest true
      parse tokenList true

   let (|BranchMatch|_|) (tokenList:Tokeniser.tokens) =
      let output (key:keyword) (result:node*Tokeniser.tokens) =
         (key,fst result,snd result)
      let selectParse (tokenList:Tokeniser.tokens) =
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
         | BranchMatch (key,body,rest) -> node.Branch(key,body) :: parse(rest)
         | item :: rest when item = Token.EndStatement -> //Raise level
         | [] -> node.Null
      parse tokenList
