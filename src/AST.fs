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

    let UnwrapResultThrough func (from:ReturnCode<'a>) =
        match from with
        | Result(res) -> Result(func res)
        | Error(str) -> Error(str)

    let UnwrapResultInto func (from:ReturnCode<'a>) =
        match from with
        | Result(res) -> func res
        | Error(str) -> Error(str)

    let ReturnWrapper func data : ReturnCode<node list*Tokeniser.tokens*Variable.Variable.typeContainer> =
        let readInputSuccess (nodeList,tokenList,vars) =
            tokenList
            |> func vars
            |> UnwrapResultThrough (fun (node,tLst,newVars) -> ((nodeList) @ [node]),tLst,newVars)
        data
        |> UnwrapResultInto readInputSuccess

    let OptionalReturnWrapper (|MatchFunc|_|) (data:ReturnCode<node list*Tokeniser.tokens*Variable.Variable.typeContainer>) =
        let processReturn (nodeList,tokenList,vars) =
            match tokenList with
            | MatchFunc vars matchedResult ->
                matchedResult
                |> UnwrapResultThrough (fun (node,newTokenList,newVars) -> (nodeList @ [node]),newTokenList,newVars)
            | _ -> Result(nodeList,tokenList,vars)
        data
        |> UnwrapResultInto processReturn

    let ColumnWrappedList (vars:Variable.Variable.typeContainer) (tokenList:Tokeniser.tokens) : ReturnCode<node*Tokeniser.tokens*Variable.Variable.typeContainer> =
        let (|FunctionMatch|_|) (lst:Tokeniser.tokens) =
            let validFunctions = [| "AVG" ; "MAX" ; "MIN" ; "SUM" ; "ROUND" |]
            match lst with
            | Token.content.Name(funName) :: Token.content.Operator("(") :: Token.content.Name(colName) :: Token.content.Operator(")") :: rest when Array.contains funName validFunctions ->
                [Item(Key(Function),Literal(Token.content.Name(funName))) ; Item(Key(Name),Literal(Token.content.Name(colName)))]
                |> fun x -> Some(x,rest)
            | _ -> None
        let (|AliasMatch|_|) (lst:Tokeniser.tokens) =
            match lst with
            | Token.content.Name(colName) :: Token.content.Name("AS") :: Token.content.Literal(colAlias) :: rest ->
                [Item(Key(Name),Literal(Token.content.Name(colName))) ; Item(Key(Alias),Literal(Token.content.Literal(colAlias)))]
                |> fun x -> Some(x,rest)
            | _ -> None
        let rec parse (outLst:node list) (lst:Tokeniser.tokens) (nextColumn:bool) : ReturnCode<node list*Tokeniser.tokens> =
            match lst with
            | FunctionMatch (funLst,rest) when nextColumn -> parse (Branch(Key(Function),funLst) :: outLst) rest false
            | AliasMatch (aliasLst,rest) when nextColumn -> parse (Branch(Key(Alias),aliasLst) :: outLst) rest false
            | Token.content.Name(name) :: rest when nextColumn -> parse (Item(Key(Name),Literal(Token.content.Name(name))) :: outLst) rest false
            | Token.content.Operator(",") :: rest when not nextColumn -> parse outLst rest true
            | item :: rest when nextColumn -> Error(sprintf "Expected wrapped column name, got %A" item)
            | rest -> Result(outLst,rest)
        parse [] tokenList true
        |> UnwrapResultThrough (fun (columnList,rest) -> Branch(Key(Column),(List.rev columnList)),rest,vars)

    let TableList (vars:Variable.Variable.typeContainer) (tokenList:Tokeniser.tokens) : ReturnCode<node*Tokeniser.tokens*Variable.Variable.typeContainer> =
        let rec parse (outLst:node list) (lst:Tokeniser.tokens) (nextItem:bool) =
             match lst with
             | Token.content.Name(name) :: rest when nextItem -> parse (Literal(Token.content.Name(name)) :: outLst) rest false
             | Token.content.Operator(",") :: rest when not nextItem -> parse outLst rest true
             | item :: rest when nextItem -> Error(sprintf "Expected table name, got %A" item)
             | rest -> Result(outLst,rest)
        parse [] tokenList true
        |> UnwrapResultThrough (fun (nodeList,tokenList) -> Branch(Key(From),(List.rev nodeList)),tokenList,vars)

    let ValueMatch (input:Tokeniser.tokens) : ReturnCode<node*Tokeniser.tokens> =
        

    let ConditionsList (vars:Variable.Variable.typeContainer) (input:Tokeniser.tokens) : (ReturnCode<node*Tokeniser.tokens*Variable.Variable.typeContainer> option) =
        let (|ConditionMatch|ConditionError|) (tokenList:Tokeniser.tokens) =
            let isCompareOp str = 
                let compareOperators = [| "=" ; "<>" ; "<" ; ">" ; "<=" ; ">=" |]
                Array.contains str compareOperators
            match tokenList with
            | Token.content.Name(name) :: Token.content.Operator(op) :: rest when isCompareOp op ->
                rest
                |> ValueMatch
                |> fun x ->
                    match x with
                    | Result(value,tail) ->
                        [ Item(Name,Token.content.Name(name)) ; Item(Operator,Token.content.Operator(op)) ; Item(Value,value) ]
                        |> fun y -> ConditionMatch(y,tail)
                    | Error(str) -> ConditionError(str)
            | _ -> ConditionError("Invalid condition format")
        let rec parse (outLst:node list) (tokenList:Tokeniser.tokens) : ReturnCode<node list*Tokeniser.tokens> =
            match tokenList with
            | Token.content.Name("AND") :: rest ->
                match rest with
                | ConditionMatch (nod,tail) -> parse (Branch(And,nod) :: outLst) tail
                | ConditionError str-> Error(str)
            | Token.content.Name("OR") :: rest ->
                match rest with
                | ConditionMatch (nod,tail) -> parse (Branch(Or,nod) :: outLst) tail
                | ConditionError str-> Error(str)
            | rest -> Result(outLst,rest)
        match input with
        | Token.content.Name("WHERE") :: rest -> Some
        | _ -> None

    let OrderList (input:Tokeniser.tokens) =

    let LimitItem (input:Tokeniser.tokens) =
        let (|OffsetMatch|_|) (tokenList:Tokeniser.tokens) =
            match tokenList with
            | Token.content.Name("OFFSET") :: Token.Value(Token.value.Integer(offsetVal)) :: rest -> Some(Result(offsetVal,rest))
            | Token.content.Name("OFFSET") :: rest -> Some(Error("Expected offset value (integer)"))
            | _ -> None
        let successOutput (tokenList:Tokeniser.tokens) (nodeList:node list) =
            (Branch(Key(Limit),nodeList),tokenList)
        let checkForOffset (tokenList:Tokeniser.tokens) =
            let limitItem = Item(Value,Literal(Token.content.Value(Token.value.Integer(limitVal))))
            let outputWithOffset (offsetVal:int,remainList:Tokeniser.tokens) =
                [limitItem ; Item(Offset,Literal(Token.content.Value(Token.value.Integer(offsetVal))))]
                |> successOutput remainList
            match tokenList with
            | OffsetMatch(matchResult) ->
                matchResult
                |> UnwrapResultThrough outputWithOffset
                |> fun x -> Some(x)
            | _ ->
                [limitItem]
                |> successOutput tokenList
                |> fun x -> Some(x)
        match input with
        | Token.content.Name("LIMIT") :: Token.Value(Token.value.Integer(limitVal)) :: rest -> checkForOffset rest
        | Token.content.Name("LIMIT") :: rest -> Some(Error("Expected limit value (integer)"))
        | _ -> None

    let (|BranchMatch|_|) (vars:Variable.Variable.typeContainer) (tokenList:Tokeniser.tokens) =
        let output (key:Key) (result:ReturnCode<node*Tokeniser.tokens>) =
            result
            |> UnwrapResultThrough (fun res -> (key,fst res,snd res))
        let selectParse (tokenList:Tokeniser.tokens) =
            Result([],tokenList,vars)
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
        let rec parse (outLst:node list) (tokens:Tokeniser.tokens) (vars:Variable.Variable.typeContainer) : ReturnCode<node list> =
            match tokens with
            | [] -> Result(outLst)
            | BranchMatch vars matchedStatement ->
                matchedStatement
                |> UnwrapResultInto (fun (key,body,rest,newVars) -> parse (node.Branch(key,body) :: outLst) rest newVars)
            | other ->
                sprintf "Unrecognised sequence %A" other
                |> fun x-> Error(x)
        parse [] tokenList Map.empty
        |> UnwrapResultThrough (fun x -> List.rev x)
