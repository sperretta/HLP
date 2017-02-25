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
      | NullExp
