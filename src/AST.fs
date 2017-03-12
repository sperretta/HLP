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
        | Create
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
        | Type

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

    type passThroughInputs = {nodeList:node list ; tokenList:Tokeniser.tokens ; varList:Variable.Variable.typeContainer ; number:int option}

    let VarsInput = fun (nodeList,tokenList,vars) ->
        let inputArgs = vars
        let outputs = {nodeList=nodeList ; tokenList=tokenList ; varList=vars ; number = None}
        (tokenList,inputArgs,outputs)
    let VarsOutput constInputs =
        let resultsFromFunc = fun (node,newTokenList,newVars) -> (node :: constInputs.nodeList),newTokenList,newVars
        let passThroughAll = (constInputs.nodeList,constInputs.tokenList,constInputs.varList)
        (resultsFromFunc,passThroughAll)
    let VarsAndNumberInput = fun (nodeList,tokenList,vars,number:int option) ->
        let inputArgs = vars,number
        let outputs = {nodeList=nodeList ; tokenList=tokenList ; varList=vars ; number = number}
        (tokenList,inputArgs,outputs)
    let VarsAndNumberOutput constInputs =
        let resultsFromFunc = fun (node,newTokenList,newVars,newNumber) -> (node :: constInputs.nodeList),newTokenList,newVars,Some(newNumber)
        let passThroughAll = (constInputs.nodeList,constInputs.tokenList,constInputs.varList,constInputs.number)
        (resultsFromFunc,passThroughAll)
    let NumberInput = fun (nodeList,tokenList,vars,number:int option) ->
        let inputArgs = number
        let outputs = {nodeList=nodeList ; tokenList=tokenList ; varList=vars ; number = number}
        (tokenList,inputArgs,outputs)
    let NumberOutput constInputs =
        let resultsFromFunc = fun (node,newTokenList,newNumber) -> (node :: constInputs.nodeList),newTokenList,constInputs.varList,Some(newNumber)
        let passThroughAll = (constInputs.nodeList,constInputs.tokenList,constInputs.varList,constInputs.number)
        (resultsFromFunc,passThroughAll)
    let NoVarsInput = fun (nodeList,tokenList,vars) ->
        let inputArgs = ()
        let outputs = {nodeList=nodeList ; tokenList=tokenList ; varList=vars ; number = None}
        (tokenList,inputArgs,outputs)
    let NoVarsOutput constInputs =
        let resultsFromFunc = fun (node,newTokenList) -> (node :: constInputs.nodeList),newTokenList,constInputs.varList
        let passThroughAll = (constInputs.nodeList,constInputs.tokenList,constInputs.varList)
        (resultsFromFunc,passThroughAll)

    let getTokenList (tokenList,_,_) = tokenList
    let getInputArgs (_,inputArgs,_) = inputArgs
    let getOutputConsts (_,_,outputConsts) = outputConsts

    let ReturnWrapper getInputInfoFrom getOutputFuncFrom func data =
        let runFunc unwrappedData =
            let completedInputInfo = getInputInfoFrom unwrappedData
            let tokenList = getTokenList completedInputInfo
            let inputArgs = getInputArgs completedInputInfo
            let outputProcesses = getOutputFuncFrom (getOutputConsts completedInputInfo)
            let processFuncOutputs = fst outputProcesses
            func inputArgs tokenList
            |> UnwrapResultThrough processFuncOutputs
        data
        |> UnwrapResultInto runFunc

    let OptionalReturnWrapper getInputInfoFrom getOutputFuncFrom (|MatchFunc|_|) data =
        let processReturn unwrappedData =
            let completedInputInfo = getInputInfoFrom unwrappedData
            let tokenList = getTokenList completedInputInfo
            let inputArgs = getInputArgs completedInputInfo
            let outputProcesses = getOutputFuncFrom (getOutputConsts completedInputInfo)
            let processFuncOutputs = fst outputProcesses
            let ignoreFuncOutput = snd outputProcesses
            match tokenList with
            | MatchFunc inputArgs matchedResult ->
                matchedResult
                |> UnwrapResultThrough processFuncOutputs
            | _ -> Result(ignoreFuncOutput)
        data
        |> UnwrapResultInto processReturn

    let ColumnWrappedList () (tokenList:Tokeniser.tokens) : ReturnCode<node*Tokeniser.tokens> =
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
        |> UnwrapResultThrough (fun (columnList,rest) -> Branch(Key(Column),(List.rev columnList)),rest)

    let ColumnTypeList () (tokenList:Tokeniser.tokens) =
        let (|ColumnTypeMatch|_|) (tLst:Tokeniser.tokens) =
            match tLst with
            | Token.content.Name(columnName) :: Token.content.Name(columnType) :: rest when Variable.Variable.isValidVarType columnType ->
                [Item(Key(Name),Literal(Token.content.Name(columnName))) ; Item(Key(Type),Literal(Token.content.Name(columnType)))]
                |> fun nodeList -> Branch(Key(Column),nodeList),rest
                |> fun result -> Some(Result(result))
            | Token.content.Name(_) :: Token.content.Name(columnType) :: _ ->
                Some(Error(sprintf "Expected valid column type, got %s" columnType))
            | Token.content.Name(_) :: item :: _ ->
                Some(Error(sprintf "Expected valid column type, got %A" item))
            | Token.content.Name(_) :: _ ->
                Some(Error("Expected valid column type, ran out of tokens"))
            | _ ->
                None
        let rec parse (outLst:node list) (lst:Tokeniser.tokens) (nextItem:bool) =
            match lst with
            | ColumnTypeMatch matchedOutput when nextItem ->
                matchedOutput
                |> UnwrapResultInto (fun (newNode,newTokenList) -> parse (newNode :: outLst) newTokenList false)
            | Token.content.Operator(",") :: rest when not nextItem ->
                parse outLst rest true
            | Token.content.Operator(")") :: rest when not nextItem ->
                Result(outLst,rest)
            | other ->
                Error(sprintf "Expected column type list, got %A" other)
        match tokenList with
        | Token.content.Operator("(") :: rest ->
            parse [] rest true
            |> UnwrapResultThrough (fun (nodeList,tokenList) -> Branch(Key(Value),List.rev nodeList),tokenList)
        | Token.content.Operator(op) :: _ ->
            Error(sprintf "Expected \"(\", got %s" op)
        | item :: _ ->
            Error(sprintf "Expected \"(\", got %A" item)
        | [] ->
            Error("Expected \"(\", ran out of tokens")

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
        | Token.content.Name(varName) :: rest -> Error(sprintf "Could not find declaration for variable %s" varName)
        | tok :: _ -> Error(sprintf "Invalid value %A" tok)
        | [] -> Error("Run out of tokens when expected value")

    let SetColumnList (vars:Variable.Variable.typeContainer) (tokenList:Tokeniser.tokens) =
        let rec parse (outLst:node list) (tLst:Tokeniser.tokens) (nextItem:bool) =
            let makeNode colName (value,newTokenList) =
                Branch(Key(Column),[Item(Key(Name),Literal(Token.content.Name(colName))) ; value])
                |> fun newNode -> parse (newNode :: outLst) newTokenList false
            match tLst with
            | Token.content.Name(columnName) :: Token.content.Operator("=") :: rest when nextItem ->
                rest
                |> ValueItem vars
                |> UnwrapResultInto (makeNode columnName)
            | Token.content.Name(_) :: Token.content.Operator(op) :: _ when nextItem ->
                Error(sprintf "Expected operator =, got operator %s" op)
            | Token.content.Name(_) :: item :: _ when nextItem ->
                Error(sprintf "Expected operator =, got %A" item)
            | item :: _ when nextItem ->
                Error(sprintf "Expected column name, got %A" item)
            | [] when nextItem ->
                Error("Expected column name, ran out of tokens")
            | Token.content.Operator(",") :: rest when not nextItem ->
                parse outLst rest true
            | rest when not nextItem ->
                Result(outLst,rest)
            | _ ->
                Error("Unknown error in SetColumnList")
        match tokenList with
        | Token.content.Name("SET") :: rest ->
            parse [] rest true
            |> UnwrapResultThrough (fun (nodeList,newTokenList) -> Branch(Key(Set),List.rev nodeList),newTokenList)
        | Token.content.Name(word) :: _ ->
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
            |> UnwrapResultThrough (fun (whereBody,tailTokenList) -> Branch(Key(Where),List.rev whereBody),tailTokenList)
        match input with
        | Token.content.Name("WHERE") :: rest ->
            rest
            |> interpretCondition parseExtraConditions
            |> fun x -> Some(x)
        | _ -> None

    let OrderList () (input:Tokeniser.tokens) =
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
            |> UnwrapResultThrough (fun (nodeList,newTokenList) -> Branch(Key(Order),List.rev nodeList),newTokenList)
            |> fun x -> Some(x)
        | _ -> None

    let TableName () (tokenList:Tokeniser.tokens) =
        match tokenList with
        | Token.content.Name(tableName) :: rest ->
            Result(Item(Key(Name),Literal(Token.content.Name(tableName))),rest)
        | item :: rest -> Error(sprintf "Expected table name, got %A" item)
        | [] -> Error("Expected table name, ran out of tokens")

    let WrappedTableName (wrapperWord:string) () (tokenList:Tokeniser.tokens) =
        match tokenList with
        | Token.content.Name(word) :: rest when word = wrapperWord ->
            TableName () rest
        | Token.content.Name(word) :: _ ->
            Error(sprintf "Expected keyword \"%s\", got %s" wrapperWord word)
        | item :: _ ->
            Error(sprintf "Expected keyword \"%s\", got %A" wrapperWord item)
        | [] ->
            Error(sprintf "Expected keyword \"%s\", ran out of tokens" wrapperWord)

    let ColumnNameList () (tokenList:Tokeniser.tokens) = //?? Need to look for an end bracket
        let rec parse (outLst:node list) (lst:Tokeniser.tokens) (nextItem:bool) (numColumns:int) =
            match lst with
            | Token.content.Name(name) :: rest when nextItem -> parse (Item(Key(Name),Literal(Token.content.Name(name))) :: outLst) rest false (numColumns+1)
            | Token.content.Operator(",") :: rest when not nextItem -> parse outLst rest false numColumns
            | item :: rest when nextItem -> Error(sprintf "Expected column name, got %A" item)
            | [] when nextItem -> Error("Expected column name, ran out of tokens")
            | rest -> Result(outLst,rest,numColumns)
        match tokenList with
        | Token.content.Operator("(") :: rest ->
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
            | Token.content.Operator(",") :: rest when not nextItem -> parse outLst rest true numValues
            | item :: rest when nextItem -> Error(sprintf "Expected value, got %A" item)
            | Token.content.Operator(")") :: rest when not nextItem -> Result(outLst,rest,numValues)
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
        | Token.content.Name("VALUES") :: Token.content.Operator("(") :: rest ->
            parse [] rest true 0
            |> UnwrapResultInto output
        | Token.content.Name("VALUES") :: item :: _ ->
            Error(sprintf "Expected operator \"(\", got %A" item)
        | item :: _ ->
            Error(sprintf "Expected keyword \"VALUES\", got %A" item)
        | [] ->
            Error("Expected keyword \"VALUES\", ran out of tokens")

    let TableList () (tokenList:Tokeniser.tokens) : ReturnCode<node*Tokeniser.tokens> =
        let rec parse (outLst:node list) (lst:Tokeniser.tokens) (nextItem:bool) =
             match lst with
             | Token.content.Name(name) :: rest when nextItem -> parse (Literal(Token.content.Name(name)) :: outLst) rest false
             | Token.content.Operator(",") :: rest when not nextItem -> parse outLst rest true
             | item :: rest when nextItem -> Error(sprintf "Expected table name, got %A" item)
             | rest -> Result(outLst,rest)
        match tokenList with
        | Token.content.Name("FROM") :: rest ->
            parse [] rest true
            |> UnwrapResultThrough (fun (nodeList,tokenList) -> Branch(Key(From),(List.rev nodeList)),tokenList)
        | item :: rest -> Error(sprintf "Expected FROM, got %A" item)
        | [] -> Error("Expected FROM, ran out of tokens")

    let LimitItem () (input:Tokeniser.tokens) =
        let (|OffsetMatch|_|) (tokenList:Tokeniser.tokens) =
            match tokenList with
            | Token.content.Name("OFFSET") :: Token.Value(Token.value.Integer(offsetVal)) :: rest -> Some(Result(offsetVal,rest))
            | Token.content.Name("OFFSET") :: rest -> Some(Error("Expected offset value (integer)"))
            | _ -> None
        let successOutput (tokenList:Tokeniser.tokens) (nodeList:node list) =
            (Branch(Key(Limit),nodeList),tokenList)
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
            Result([],tokenList,vars)
        let declareParse (tokenList:Tokeniser.tokens) =
            let isValidVarName (name:string) =
                not (vars.ContainsKey(name))
            let updatedVarMap (name:string) (varType:string) =
                Map.add name Variable.Variable.validTypes.[varType] vars
            match tokenList with
            | Token.content.Name(varName) :: Token.content.Name(varType) :: rest when isValidVarName varName && Variable.Variable.isValidVarType varType ->
                [Item(Key(Variable),Literal(Token.content.Name(varName))) ; Item(Key(Type),Literal(Token.content.Name(varType)))]
                |> fun nodeList -> Result(nodeList,rest,updatedVarMap varName varType)
            | Token.content.Name(varName) :: Token.content.Name(varType) :: _ when isValidVarName varName ->
                Error(sprintf "Cannot understand type %s" varType)
            | Token.content.Name(varName) :: Token.content.Name(varType) :: _ when Variable.Variable.isValidVarType varType ->
                Error(sprintf "Variable name \"%s\" already has been declared" varName)
            | Token.content.Name(varName) :: item :: _ ->
                Error(sprintf "Expected variable type, got %A" item)
            | item :: _ ->
                Error(sprintf "Expected variable name, got %A" item)
            | [] ->
                Error("Expected variable name, ran out of tokens")
        let deleteParse (tokenList:Tokeniser.tokens) =
            Result([],tokenList,vars)
        let createParse (tokenList:Tokeniser.tokens) =
            Result([],tokenList,vars)
            |> ReturnWrapper NoVarsInput NoVarsOutput (WrappedTableName "FROM")
            |> ReturnWrapper NoVarsInput NoVarsOutput ColumnTypeList
            |> UnwrapResultThrough (fun (nodeList,tokenList,varMap) -> List.rev nodeList,tokenList,varMap)
        match tokenList with
        | item :: rest ->
            match item with
            | Token.content.Name("SELECT") -> Some(output Select (selectParse rest))
            | Token.content.Name("INSERT") -> Some(output Insert (insertParse rest))
            | Token.content.Name("UPDATE") -> Some(output Update (updateParse rest))
            | Token.content.Name("SET") -> Some(output Set (setParse rest))
            | Token.content.Name("DECLARE") -> Some(output Declare (declareParse rest))
            | Token.content.Name("DELETE") -> Some(output Delete (deleteParse rest))
            | Token.content.Name("CREATE") -> Some(output Create (createParse rest))
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
