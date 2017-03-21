namespace Parser
module AST =
    open Tokeniser

    open Parser.ControlAndTypes

    open ReturnControl.Main
    open ReturnControl.AST

    let ColumnWrappedList () (tokenList:Tokeniser.tokens) : ReturnCode<node*Tokeniser.tokens> =
        let (|FunctionMatch|_|) (lst:Tokeniser.tokens) =
            let validFunctions = [| "AVG" ; "MAX" ; "MIN" ; "SUM" ; "ROUND" |]
            match lst with
            | Token.Name(funName) :: Token.Operator("(") :: Token.Name(colName) :: Token.Operator(")") :: rest when Array.contains funName validFunctions ->
                [Item(Key(Function),Literal(Token.Name(funName))) ; Item(Key(Name),Literal(Token.Name(colName)))]
                |> fun x -> Some(x,rest)
            | _ -> None
        let (|AliasMatch|_|) (lst:Tokeniser.tokens) =
            match lst with
            | Token.Name(colName) :: Token.Name("AS") :: Token.Literal(colAlias) :: rest ->
                [Item(Key(Name),Literal(Token.Name(colName))) ; Item(Key(Alias),Literal(Token.Literal(colAlias)))]
                |> fun x -> Some(x,rest)
            | _ -> None
        let rec parse (outLst:node list) (lst:Tokeniser.tokens) (nextColumn:bool) : ReturnCode<node list*Tokeniser.tokens> =
            match lst with
            | FunctionMatch (funLst,rest) when nextColumn -> parse (Branch(Key(Function),funLst) :: outLst) rest false
            | AliasMatch (aliasLst,rest) when nextColumn -> parse (Branch(Key(Alias),aliasLst) :: outLst) rest false
            | Token.Name(name) :: rest when nextColumn -> parse (Item(Key(Name),Literal(Token.Name(name))) :: outLst) rest false
            | Token.Operator(",") :: rest when not nextColumn -> parse outLst rest true
            | item :: rest when nextColumn -> Error(sprintf "Expected wrapped column name, got %A" item)
            | rest -> Result(outLst,rest)
        parse [] tokenList true
        |> UnwrapResultThrough (fun (columnList,rest) -> Branch(Key(Column),(List.rev columnList)),rest)

    let ColumnTypeList () (tokenList:Tokeniser.tokens) =
        let (|ColumnTypeMatch|_|) (tLst:Tokeniser.tokens) =
            match tLst with
            | Token.Name(columnName) :: Token.Name(columnType) :: rest when Variable.Variable.isValidVarType columnType ->
                [Item(Key(Name),Literal(Token.Name(columnName))) ; Item(Key(Type),Literal(Token.Name(columnType)))]
                |> fun nodeList -> Branch(Key(Column),nodeList),rest
                |> fun result -> Some(Result(result))
            | Token.Name(_) :: Token.Name(columnType) :: _ ->
                Some(Error(sprintf "Expected valid column type, got %s" columnType))
            | Token.Name(_) :: item :: _ ->
                Some(Error(sprintf "Expected valid column type, got %A" item))
            | Token.Name(_) :: _ ->
                Some(Error("Expected valid column type, ran out of tokens"))
            | _ ->
                None
        let rec parse (outLst:node list) (lst:Tokeniser.tokens) (nextItem:bool) =
            match lst with
            | ColumnTypeMatch matchedOutput when nextItem ->
                matchedOutput
                |> UnwrapResultInto (fun (newNode,newTokenList) -> parse (newNode :: outLst) newTokenList false)
            | Token.Operator(",") :: rest when not nextItem ->
                parse outLst rest true
            | Token.Operator(")") :: rest when not nextItem ->
                Result(outLst,rest)
            | other ->
                Error(sprintf "Expected column type list, got %A" other)
        match tokenList with
        | Token.Operator("(") :: rest ->
            parse [] rest true
            |> UnwrapResultThrough (fun (nodeList,tokenList) -> Branch(Key(Value),List.rev nodeList),tokenList)
        | Token.Operator(op) :: _ ->
            Error(sprintf "Expected \"(\", got %s" op)
        | item :: _ ->
            Error(sprintf "Expected \"(\", got %A" item)
        | [] ->
            Error("Expected \"(\", ran out of tokens")

    let ValueItemWithTypecheck (vars:Variable.Variable.typeContainer) (input:Tokeniser.tokens) =
        let output (tLst:Tokeniser.tokens) (nod:node) (valType:Variable.Variable.varType) =
            Result(nod,tLst,valType)
        match input with
        | Token.Value(value) :: rest ->
            match value with
            | Token.Integer(_) -> Variable.Variable.Integer
            | Token.Byte(_) -> Variable.Variable.Byte
            | Token.Floating(_) -> Variable.Variable.Float
            | Token.Boolean(_) -> Variable.Variable.Boolean
            |> output rest (Item(Key(Number),Literal(Token.Value(value))))
        | Token.Literal(str) :: rest ->
            output rest (Item(Key(String),Literal(Token.Literal(str)))) Variable.Variable.String
        | Token.Name(varName) :: rest when vars.ContainsKey varName ->
            output rest (Item(Key(Variable),Literal(Token.Name(varName)))) vars.[varName]
        | Token.Name(varName) :: rest -> Error(sprintf "Could not find declaration for variable %s" varName)
        | tok :: _ -> Error(sprintf "Invalid value %A" tok)
        | [] -> Error("Run out of tokens when expected value")

    let ValueItem (vars:Variable.Variable.typeContainer) (input:Tokeniser.tokens) =
        ValueItemWithTypecheck vars input
        |> UnwrapResultThrough (fun (nodeList,tokenList,_) -> (nodeList,tokenList))

    let SetColumnList (vars:Variable.Variable.typeContainer) (tokenList:Tokeniser.tokens) =
        let rec parse (outLst:node list) (tLst:Tokeniser.tokens) (nextItem:bool) =
            let makeNode colName (value,newTokenList) =
                Branch(Key(Column),[Item(Key(Name),Literal(Token.Name(colName))) ; value])
                |> fun newNode -> parse (newNode :: outLst) newTokenList false
            match tLst with
            | Token.Name(columnName) :: Token.Operator("=") :: rest when nextItem ->
                rest
                |> ValueItem vars
                |> UnwrapResultInto (makeNode columnName)
            | Token.Name(_) :: Token.Operator(op) :: _ when nextItem ->
                Error(sprintf "Expected operator =, got operator %s" op)
            | Token.Name(_) :: item :: _ when nextItem ->
                Error(sprintf "Expected operator =, got %A" item)
            | item :: _ when nextItem ->
                Error(sprintf "Expected column name, got %A" item)
            | [] when nextItem ->
                Error("Expected column name, ran out of tokens")
            | Token.Operator(",") :: rest when not nextItem ->
                parse outLst rest true
            | rest when not nextItem ->
                Result(outLst,rest)
            | _ ->
                Error("Unknown error in SetColumnList")
        match tokenList with
        | Token.Name("SET") :: rest ->
            parse [] rest true
            |> UnwrapResultThrough (fun (nodeList,newTokenList) -> Branch(Key(Set),List.rev nodeList),newTokenList)
        | Token.Name(word) :: _ ->
            Error(sprintf "Expected keyword SET, got %s" word)
        | item :: _ ->
            Error(sprintf "Expected keyword SET, got %A" item)
        | [] ->
            Error("Expected keyword SET, ran out of tokens")

    let ConditionsList (vars:Variable.Variable.typeContainer) (input:Tokeniser.tokens) : (ReturnCode<node*Tokeniser.tokens> option) =
        let (|ConditionMatch|) (tokenList:Tokeniser.tokens) =
            let isCompareOp str =
                let compareOperators = [| "=" ; "<>" ; "<" ; ">" ; "<=" ; ">=" |]
                Array.contains str compareOperators
            let output name op (valueItem,tLst) =
                [ Item(Key(Name),Literal(Token.Name(name))) ; Item(Key(Operator),Literal(Token.Operator(op))) ; Item(Key(Value),valueItem) ]
                |> fun x -> x,tLst
            match tokenList with
            | Token.Name(name) :: Token.Operator(op) :: rest when isCompareOp op ->
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
            | Token.Name("AND") :: rest ->
                rest
                |> interpretCondition (fun (nodeBody,newTLst) -> parse (Branch(Key(And),nodeBody) :: outLst) newTLst)
            | Token.Name("OR") :: rest ->
                rest
                |> interpretCondition (fun (nodeBody,newTLst) -> parse (Branch(Key(Or),nodeBody) :: outLst) newTLst)
            | rest -> Result(outLst,rest)
        let parseExtraConditions (nodeBody,newTLst) =
            parse [Branch(Key(Condition),nodeBody)] newTLst
            |> UnwrapResultThrough (fun (whereBody,tailTokenList) -> Branch(Key(Where),List.rev whereBody),tailTokenList)
        match input with
        | Token.Name("WHERE") :: rest ->
            rest
            |> interpretCondition parseExtraConditions
            |> fun x -> Some(x)
        | _ -> None

    let OrderList () (input:Tokeniser.tokens) =
        let rec parse (outLst:node list) (lst:Tokeniser.tokens) (nextItem:bool) =
            match lst with
            | Token.Name(columnName) :: Token.Name(direction) :: rest when nextItem ->
                let orderItemFoundOf formattedDirection =
                    parse (Branch(Key(Order),[Item(Key(Name),Literal(Token.Name(columnName))) ; Key(formattedDirection)]) :: outLst) rest false
                match direction with
                | "ASC" -> orderItemFoundOf Ascend
                | "DESC" -> orderItemFoundOf Descend
                | _ -> Error(sprintf "Expect keyword ASC | DESC, instead got %s" direction)
            | Token.Operator(",") :: rest when not nextItem ->
                parse outLst rest true
            | item :: _ when nextItem -> Error(sprintf "Expected column name, got %A" item)
            | Token.Name(_) :: item :: _ when nextItem -> Error(sprintf "Expected order direction, got %A" item)
            | rest when not nextItem -> Result(outLst,rest)
            | _ -> Error("Unknown error occured in OrderList")
        match input with
        | Token.Name("ORDER") :: Token.Name("BY") :: tokenList ->
            parse [] tokenList true
            |> UnwrapResultThrough (fun (nodeList,newTokenList) -> Branch(Key(Order),List.rev nodeList),newTokenList)
            |> fun x -> Some(x)
        | _ -> None

    let TableName () (tokenList:Tokeniser.tokens) =
        match tokenList with
        | Token.Name(tableName) :: rest ->
            Result(Item(Key(Name),Literal(Token.Name(tableName))),rest)
        | item :: rest -> Error(sprintf "Expected table name, got %A" item)
        | [] -> Error("Expected table name, ran out of tokens")

    let WrappedTableName (wrapperWord:string) () (tokenList:Tokeniser.tokens) =
        match tokenList with
        | Token.Name(word) :: rest when word = wrapperWord ->
            TableName () rest
        | Token.Name(word) :: _ ->
            Error(sprintf "Expected keyword \"%s\", got %s" wrapperWord word)
        | item :: _ ->
            Error(sprintf "Expected keyword \"%s\", got %A" wrapperWord item)
        | [] ->
            Error(sprintf "Expected keyword \"%s\", ran out of tokens" wrapperWord)

    let ColumnNameList () (tokenList:Tokeniser.tokens) =
        let rec parse (outLst:node list) (lst:Tokeniser.tokens) (nextItem:bool) (numColumns:int) =
            match lst with
            | Token.Name(name) :: rest when nextItem -> parse (Item(Key(Name),Literal(Token.Name(name))) :: outLst) rest false (numColumns+1)
            | Token.Operator(",") :: rest when not nextItem -> parse outLst rest true numColumns
            | item :: rest when nextItem -> Error(sprintf "Expected column name, got %A" item)
            | [] when nextItem -> Error("Expected column name, ran out of tokens")
            | Token.Operator(")") :: rest -> Result(outLst,rest,numColumns)
            | item :: rest when not nextItem -> Error(sprintf "Expected comma or bracket, got %A" item)
            | [] when not nextItem -> Error("Expected comma or bracket, ran out of tokens")
            | _ -> Error(sprintf "Expected something other than %A" lst)
        match tokenList with
        | Token.Operator("(") :: rest ->
            parse [] rest true 0
            |> UnwrapResultThrough (fun (nodeList,tokenList,numColumns) -> Branch(Key(Column),List.rev nodeList),tokenList,numColumns)
            |> fun x -> Some(x)
        | _ -> None

    let ValueList (vars:Variable.Variable.typeContainer,numColumns:int option) (tokenList:Tokeniser.tokens) : ReturnCode<node*Tokeniser.tokens> =
        let (|ValueMatch|) = ValueItem
        let rec parse (outLst:node list) (lst:Tokeniser.tokens) (nextItem:bool) (numValues:int) =
            match lst with
            | ValueMatch vars matchedValue when nextItem ->
                matchedValue
                |> UnwrapResultInto (fun (valNode,tokenList) -> parse (Item(Key(Value),valNode) :: outLst) tokenList false (numValues+1))
            | Token.Operator(",") :: rest when not nextItem -> parse outLst rest true numValues
            | item :: rest when nextItem -> Error(sprintf "Expected value, got %A" item)
            | Token.Operator(")") :: rest when not nextItem -> Result(outLst,rest,numValues)
            | item :: rest when not nextItem -> Error(sprintf "Expected , or ), got %A" item)
            | [] when nextItem -> Error("Expected value, ran out of tokens")
            | [] when not nextItem -> Error("Expected , or ), ran out of tokens")
            | rest -> Error("Unknown error on sequence")
        let compareValues num =
            match numColumns with
            | Some(numCol) when numCol = num -> true
            | None -> true
            | _ -> false
        let output (nodeList,tokenList,numValues) =
           if compareValues numValues then
               Result(Branch(Key(Value),List.rev nodeList),tokenList)
            else
               Error(sprintf "Number of columns %u does not match number of values %u" numColumns.Value numValues)
        match tokenList with
        | Token.Name("VALUES") :: Token.Operator("(") :: rest ->
            parse [] rest true 0
            |> UnwrapResultInto output
        | Token.Name("VALUES") :: item :: _ ->
            Error(sprintf "Expected operator \"(\", got %A" item)
        | item :: _ ->
            Error(sprintf "Expected keyword \"VALUES\", got %A" item)
        | [] ->
            Error("Expected keyword \"VALUES\", ran out of tokens")

    let TableList () (tokenList:Tokeniser.tokens) : ReturnCode<node*Tokeniser.tokens> =
        let rec parse (outLst:node list) (lst:Tokeniser.tokens) (nextItem:bool) =
             match lst with
             | Token.Name(name) :: rest when nextItem -> parse (Literal(Token.Name(name)) :: outLst) rest false
             | Token.Operator(",") :: rest when not nextItem -> parse outLst rest true
             | item :: rest when nextItem -> Error(sprintf "Expected table name, got %A" item)
             | rest -> Result(outLst,rest)
        match tokenList with
        | Token.Name("FROM") :: rest ->
            parse [] rest true
            |> UnwrapResultThrough (fun (nodeList,tokenList) -> Branch(Key(From),(List.rev nodeList)),tokenList)
        | item :: rest -> Error(sprintf "Expected FROM, got %A" item)
        | [] -> Error("Expected FROM, ran out of tokens")

    let LimitItem () (input:Tokeniser.tokens) =
        let (|OffsetMatch|_|) (tokenList:Tokeniser.tokens) =
            match tokenList with
            | Token.Name("OFFSET") :: Token.Value(Token.Integer(offsetVal)) :: rest -> Some(Result(offsetVal,rest))
            | Token.Name("OFFSET") :: rest -> Some(Error("Expected offset value (integer)"))
            | _ -> None
        let successOutput (tokenList:Tokeniser.tokens) (nodeList:node list) =
            (Branch(Key(Limit),nodeList),tokenList)
        let checkForOffset (tokenList:Tokeniser.tokens) (limitVal:int) =
            let limitItem = Item(Key(Value),Literal(Token.Value(Token.Integer(limitVal))))
            let outputWithOffset (offsetVal:int,remainList:Tokeniser.tokens) =
                [limitItem ; Item(Key(Offset),Literal(Token.Value(Token.Integer(offsetVal))))]
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
        | Token.Name("LIMIT") :: Token.Value(Token.Integer(limitVal)) :: rest -> checkForOffset rest limitVal
        | Token.Name("LIMIT") :: rest -> Some(Error("Expected limit value (integer)"))
        | _ -> None

    let (|BranchMatch|_|) (vars:Variable.Variable.typeContainer) (tokenList:Tokeniser.tokens) =
        let output (key:keyword) (result:ReturnCode<node list*Tokeniser.tokens*Variable.Variable.typeContainer>) =
            result
            |> UnwrapResultThrough (fun (nodeList,tokenList,vars) -> (Key(key),nodeList,tokenList,vars))
        let selectParse (tokenList:Tokeniser.tokens) =
            Result([],tokenList,vars)
            |> ReturnWrapper NoVarsInput NoVarsOutput ColumnWrappedList
            |> ReturnWrapper NoVarsInput NoVarsOutput TableList
            |> OptionalReturnWrapper VarsInput NoVarsOutput ConditionsList
            |> OptionalReturnWrapper NoVarsInput NoVarsOutput OrderList
            |> OptionalReturnWrapper NoVarsInput NoVarsOutput LimitItem
            |> UnwrapResultThrough (fun (nodeList,tokenList,varMap) -> List.rev nodeList,tokenList,varMap)
        let insertParse (tokenList:Tokeniser.tokens) =
            Result([],tokenList,vars)
            |> ReturnWrapper NoVarsInput NoVarsOutput (WrappedTableName "INTO")
            |> OptionalReturnWrapper NoVarsInput NumberOutput ColumnNameList
            |> ReturnWrapper VarsAndNumberInput NoVarsOutput ValueList
            |> UnwrapResultThrough (fun (nodeList,tokenList,varMap) -> List.rev nodeList,tokenList,varMap)
        let updateParse (tokenList:Tokeniser.tokens) =
            Result([],tokenList,vars)
            |> ReturnWrapper NoVarsInput NoVarsOutput TableName
            |> ReturnWrapper VarsInput NoVarsOutput SetColumnList
            |> OptionalReturnWrapper VarsInput NoVarsOutput ConditionsList
            |> UnwrapResultThrough (fun (nodeList,tokenList,varMap) -> List.rev nodeList,tokenList,varMap)
        let setParse (tokenList:Tokeniser.tokens) =
            let (|ValueMatch|) = ValueItemWithTypecheck
            let mathsOperators = [| "+" ; "-" ; "*" ; "/" |]
            let validMathsTypes = [| Variable.Variable.Integer ; Variable.Variable.Float |]
            let rec parse (outLst:node list) (tLst:Tokeniser.tokens) (nextItem:bool) (expressionType:Variable.Variable.varType) =
                let processValueMatch (newNode,newTokenList,valType) =
                    if valType = expressionType then
                        parse (Item(Key(Value),newNode) :: outLst) newTokenList false expressionType
                    else
                        Error(sprintf "Value type %A and expression type %A don't match" valType expressionType)
                match tLst with
                | Token.Name("SELECT") :: rest when nextItem ->
                    selectParse rest
                    |> UnwrapResultInto (fun (nodeList,newTokenList,_) -> parse (Item(Key(Value),Branch(Key(Select),nodeList)) :: outLst) newTokenList false expressionType)
                | Token.Operator(op) :: rest when not nextItem && Array.contains op mathsOperators ->
                    if Array.contains expressionType validMathsTypes then
                        parse (Item(Key(Operator),Literal(Token.Operator(op))) :: outLst) rest true expressionType
                    else
                        Error(sprintf "%A is not a valid type to performs %s operator on" expressionType op)
                | Token.Operator(op) :: _ when not nextItem ->
                    Error(sprintf "Unrecognised operator %s" op)
                | [] when not nextItem ->
                    Error("Expected maths operator or endstatement, ran out of tokens")
                | rest when not nextItem ->
                    Result(outLst,rest)
                | ValueMatch vars matchedResult when nextItem ->
                    matchedResult
                    |> UnwrapResultInto processValueMatch
                | other ->
                    Error(sprintf "Unrecognised pattern in setParse %A" other)
            match tokenList with
            | Token.Name(varName) :: Token.Operator("=") :: rest when vars.ContainsKey(varName) ->
                parse [] rest true vars.[varName]
                |> UnwrapResultThrough (fun (nodeList,newTokenList) -> [Item(Key(Variable),Literal(Token.Name(varName))) ; Branch(Key(Operator),List.rev nodeList)],newTokenList,vars)
            | Token.Name(varName) :: Token.Operator(op) :: _ when vars.ContainsKey(varName) ->
                Error(sprintf "Expected operator =, got %s" op)
            | Token.Name(varName) :: item :: _ when vars.ContainsKey(varName) ->
                Error(sprintf "Expected operator =, got %A" item)
            | Token.Name(varName) :: _ ->
                Error(sprintf "Variable name %s has not been declared" varName)
            | item :: _ ->
                Error(sprintf "Expected variable name, got %A" item)
            | [] ->
                Error("Expected variable name, ran out of tokens")
        let declareParse (tokenList:Tokeniser.tokens) =
            let isValidVarName (name:string) =
                not (vars.ContainsKey(name))
            let updatedVarMap (name:string) (varType:string) =
                Map.add name Variable.Variable.validTypes.[varType] vars
            match tokenList with
            | Token.Name(varName) :: Token.Name(varType) :: rest when isValidVarName varName && Variable.Variable.isValidVarType varType ->
                [Item(Key(Variable),Literal(Token.Name(varName))) ; Item(Key(Type),Literal(Token.Name(varType)))]
                |> fun nodeList -> Result(nodeList,rest,updatedVarMap varName varType)
            | Token.Name(varName) :: Token.Name(varType) :: _ when isValidVarName varName ->
                Error(sprintf "Cannot understand type %s" varType)
            | Token.Name(varName) :: Token.Name(varType) :: _ when Variable.Variable.isValidVarType varType ->
                Error(sprintf "Variable name \"%s\" already has been declared" varName)
            | Token.Name(varName) :: item :: _ ->
                Error(sprintf "Expected variable type, got %A" item)
            | item :: _ ->
                Error(sprintf "Expected variable name, got %A" item)
            | [] ->
                Error("Expected variable name, ran out of tokens")
        let deleteParse (tokenList:Tokeniser.tokens) =
            Result([],tokenList,vars)
            |> ReturnWrapper NoVarsInput NoVarsOutput TableList
            |> OptionalReturnWrapper VarsInput NoVarsOutput ConditionsList
        let createParse (tokenList:Tokeniser.tokens) =
            Result([],tokenList,vars)
            |> ReturnWrapper NoVarsInput NoVarsOutput (WrappedTableName "TABLE")
            |> ReturnWrapper NoVarsInput NoVarsOutput ColumnTypeList
            |> UnwrapResultThrough (fun (nodeList,tokenList,varMap) -> List.rev nodeList,tokenList,varMap)
        match tokenList with
        | item :: rest ->
            match item with
            | Token.Name("SELECT") -> Some(output Select (selectParse rest))
            | Token.Name("INSERT") -> Some(output Insert (insertParse rest))
            | Token.Name("UPDATE") -> Some(output Update (updateParse rest))
            | Token.Name("SET") -> Some(output Set (setParse rest))
            | Token.Name("DECLARE") -> Some(output Declare (declareParse rest))
            | Token.Name("DELETE") -> Some(output Delete (deleteParse rest))
            | Token.Name("CREATE") -> Some(output Create (createParse rest))
            | _ -> None;
        | _ -> None;

    let getTree (tokenList:Tokeniser.tokens) =
        let rec parse (outLst:node list) (tokens:Tokeniser.tokens) (vars:Variable.Variable.typeContainer) (nextStatement:bool) : ReturnCode<node list> =
            match tokens with
            | [] when nextStatement -> Result(outLst)
            | BranchMatch vars matchedStatement when nextStatement ->
                matchedStatement
                |> UnwrapResultInto (fun (key,body,rest,newVars) -> parse (Branch(key,body) :: outLst) rest newVars false)
            | Token.EndStatement :: rest when not nextStatement ->
                parse outLst rest vars true
            | item :: rest when not nextStatement ->
                Error(sprintf "Expected ; got %A" item)
            | [] when not nextStatement ->
                Error("Expected ; ran out of tokens")
            | other ->
                sprintf "Unrecognised sequence in getTree.parse %A" other
                |> fun x-> Error(x)
        parse [] tokenList Map.empty true
        |> UnwrapResultThrough (fun x -> List.rev x)
