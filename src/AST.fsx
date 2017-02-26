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
         | Name("SELECT") -> Some(output keyword.Select (selectParse rest))
         | Name("INSERT") -> Some(output keyword.Insert (insertParse rest))
         | Name("UPDATE") -> Some(output keyword.Update (updateParse rest))
         | Name("SET") -> Some(output keyword.Set (setParse rest))
         | Name("DECLARE") -> Some(output keyword.Declare (declareParse rest))
         | Name("DELETE") -> Some(output keyword.Delete (deleteParse rest))
         | _ -> None;
      | _ -> None;

   let getTree (tokenList:Tokeniser.tokens) =
      let rec parse (tokens:Tokeniser.tokens) =
         match tokens with
         | BranchMatch (key,body,rest) -> Branch(key,body) :: parse(rest)
         | item :: rest when item = Token.EndStatement -> //Raise level
         | [] -> Null
      parse tokenList
