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
        | Number
        | String
        | Column
        | Alias
        | And
        | Or
        | Operator
        | Ascend
        | Descend
        | Into

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

    let ValueItem (vars:Variable.Variable.typeContainer) (input:Tokeniser.tokens) : ReturnCode<node*Tokeniser.tokens> =
        let output (tLst:Tokeniser.tokens) (nod:node) =
            Result(nod,tLst)
        match input with
        | Token.content.Value(value) :: rest ->
            Item(Key(Number),Literal(Token.content.Value(value)))
            |> output rest
        | Token.content.Literal(str) :: rest ->
            Item(Key(String),Literal(Token.content.Literal(str)))
            |> output rest
        | Token.content.Name(varName) :: rest when vars.ContainsKey varName ->
            Item(Key(Variable),Literal(Token.content.Name(varName)))
            |> output rest
        | tok :: _ -> Error(sprintf "Invalid value %A" tok)
        | [] -> Error("Run out of tokens when expected value")
        

    let ConditionsList (vars:Variable.Variable.typeContainer) (input:Tokeniser.tokens) : (ReturnCode<node*Tokeniser.tokens*Variable.Variable.typeContainer> option) =
        let (|ConditionMatch|) (tokenList:Tokeniser.tokens) =
            let isCompareOp str = 
                let compareOperators = [| "=" ; "<>" ; "<" ; ">" ; "<=" ; ">=" |]
                Array.contains str compareOperators
            let output name op (valueItem,tLst) =
                [ Item(Key(Name),Literal(Token.content.Name(name))) ; Item(Key(Operator),Literal(Token.content.Operator(op))) ; Item(Key(Value),valueItem) ]
                |> fun x -> x,tLst
            match tokenList with
            | Token.content.Name(name) :: Token.content.Operator(op) :: rest when isCompareOp op ->
                rest
                |> ValueItem vars
                |> UnwrapResultThrough (output name op)
            | _ -> Error("Invalid condition format")
        let interpretCondition func lst =
            match lst with
            | ConditionMatch condition ->
                condition
                |> UnwrapResultInto func
        let rec parse (outLst:node list) (tokenList:Tokeniser.tokens) : ReturnCode<node list*Tokeniser.tokens> =
            match tokenList with
            | Token.content.Name("AND") :: rest ->
                rest
                |> interpretCondition (fun (nodeBody,newTLst) -> parse (Branch(Key(And),nodeBody) :: outLst) newTLst)
            | Token.content.Name("OR") :: rest ->
                rest
                |> interpretCondition (fun (nodeBody,newTLst) -> parse (Branch(Key(Or),nodeBody) :: outLst) newTLst)
            | rest -> Result(outLst,rest)
        let parseExtraConditions (nodeBody,newTLst) =
            parse [Branch(Key(Condition),nodeBody)] newTLst
            |> UnwrapResultThrough (fun (whereBody,tailTokenList) -> Branch(Key(Where),List.rev whereBody),tailTokenList,vars)
        match input with
        | Token.content.Name("WHERE") :: rest ->
            rest
            |> interpretCondition parseExtraConditions
            |> fun x -> Some(x)
        | _ -> None

    let OrderList (vars:Variable.Variable.typeContainer) (input:Tokeniser.tokens) =
        let rec parse (outLst:node list) (lst:Tokeniser.tokens) (nextItem:bool) =
            match lst with
            | Token.content.Name(columnName) :: Token.content.Name(direction) :: rest when nextItem ->
                let orderItemFoundOf formattedDirection =
                    parse (Branch(Key(Order),[Item(Key(Name),Literal(Token.content.Name(columnName))) ; Key(formattedDirection)]) :: outLst) rest false
                match direction with
                | "ASC" -> orderItemFoundOf Ascend
                | "DESC" -> orderItemFoundOf Descend
                | _ -> Error(sprintf "Expect keyword ASC | DESC, instead got %s" direction)
            | Token.content.Operator(",") :: rest when not nextItem ->
                parse outLst rest true
            | item :: _ when nextItem -> Error(sprintf "Expected column name, got %A" item)
            | Token.content.Name(_) :: item :: _ when nextItem -> Error(sprintf "Expected order direction, got %A" item)
            | rest when not nextItem -> Result(outLst,rest)
            | _ -> Error("Unknown error occured in OrderList")
        match input with
        | Token.content.Name("ORDER") :: Token.content.Name("BY") :: tokenList ->
            parse [] tokenList true
            |> UnwrapResultThrough (fun (nodeList,newTokenList) -> Branch(Key(Order),List.rev nodeList),newTokenList,vars)
            |> fun x -> Some(x)
        | _ -> None

    let TableName (vars:Variable.Variable.typeContainer) (tokenList:Tokeniser.tokens) =
        match tokenList with
        | Token.content.Name(tableName) :: rest ->
            Result(Item(Key(Into),Literal(Token.content.Name(tableName))),rest,vars)
        | item :: rest -> Error(sprintf "Expected table name, got %A" item)
        | [] -> Error("Expected table name, ran out of tokens")
        
    let TableList (vars:Variable.Variable.typeContainer) (tokenList:Tokeniser.tokens) : ReturnCode<node*Tokeniser.tokens*Variable.Variable.typeContainer> =
        let rec parse (outLst:node list) (lst:Tokeniser.tokens) (nextItem:bool) =
             match lst with
             | Token.content.Name(name) :: rest when nextItem -> parse (Literal(Token.content.Name(name)) :: outLst) rest false
             | Token.content.Operator(",") :: rest when not nextItem -> parse outLst rest true
             | item :: rest when nextItem -> Error(sprintf "Expected table name, got %A" item)
             | rest -> Result(outLst,rest)
        match tokenList with
        | Token.content.Name("FROM") :: rest ->
            parse [] rest true
            |> UnwrapResultThrough (fun (nodeList,tokenList) -> Branch(Key(From),(List.rev nodeList)),tokenList,vars)
        | item :: rest -> Error(sprintf "Expected FROM, got %A" item)
        | [] -> Error("Expected FROM, ran out of tokens")

    let LimitItem (var:Variable.Variable.typeContainer) (input:Tokeniser.tokens) =
        let (|OffsetMatch|_|) (tokenList:Tokeniser.tokens) =
            match tokenList with
            | Token.content.Name("OFFSET") :: Token.Value(Token.value.Integer(offsetVal)) :: rest -> Some(Result(offsetVal,rest))
            | Token.content.Name("OFFSET") :: rest -> Some(Error("Expected offset value (integer)"))
            | _ -> None
        let successOutput (tokenList:Tokeniser.tokens) (nodeList:node list) =
            (Branch(Key(Limit),nodeList),tokenList,var)
        let checkForOffset (tokenList:Tokeniser.tokens) (limitVal:int) =
            let limitItem = Item(Key(Value),Literal(Token.content.Value(Token.value.Integer(limitVal))))
            let outputWithOffset (offsetVal:int,remainList:Tokeniser.tokens) =
                [limitItem ; Item(Key(Offset),Literal(Token.content.Value(Token.value.Integer(offsetVal))))]
                |> successOutput remainList
            match tokenList with
            | OffsetMatch(matchResult) ->
                matchResult
                |> UnwrapResultThrough outputWithOffset
                |> fun x -> Some(x)
            | _ ->
                [limitItem]
                |> successOutput tokenList
                |> fun x -> Some(Result(x))
        match input with
        | Token.content.Name("LIMIT") :: Token.Value(Token.value.Integer(limitVal)) :: rest -> checkForOffset rest limitVal
        | Token.content.Name("LIMIT") :: rest -> Some(Error("Expected limit value (integer)"))
        | _ -> None

    let (|BranchMatch|_|) (vars:Variable.Variable.typeContainer) (tokenList:Tokeniser.tokens) =
        let output (key:keyword) (result:ReturnCode<node list*Tokeniser.tokens*Variable.Variable.typeContainer>) =
            result
            |> UnwrapResultThrough (fun (nodeList,tokenList,vars) -> (Key(key),nodeList,tokenList,vars))
        let selectParse (tokenList:Tokeniser.tokens) =
            Result([],tokenList,vars)
            |> ReturnWrapper ColumnWrappedList
            |> ReturnWrapper TableList
            |> OptionalReturnWrapper ConditionsList
            |> OptionalReturnWrapper OrderList
            |> OptionalReturnWrapper LimitItem
        let insertParse (tokenList:Tokeniser.tokens) =
            Result([],tokenList,vars)
            |> ReturnWrapper TableName
            |> OptionalReturnWrapper ColumnNameList
            |> ReturnWrapper ValueList
        let updateParse (tokenList:Tokeniser.tokens) =
            Result([],tokenList,vars)
        let setParse (tokenList:Tokeniser.tokens) =
            Result([],tokenList,vars)
        let declareParse (tokenList:Tokeniser.tokens) =
            Result([],tokenList,vars)
        let deleteParse (tokenList:Tokeniser.tokens) =
            Result([],tokenList,vars)
        match tokenList with
        | item :: rest ->
            match item with
            | Token.content.Name("SELECT") -> Some(output Select (selectParse rest))
            | Token.content.Name("INSERT") -> Some(output Insert (insertParse rest))
            | Token.content.Name("UPDATE") -> Some(output Update (updateParse rest))
            | Token.content.Name("SET") -> Some(output Set (setParse rest))
            | Token.content.Name("DECLARE") -> Some(output Declare (declareParse rest))
            | Token.content.Name("DELETE") -> Some(output Delete (deleteParse rest))
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
