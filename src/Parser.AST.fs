namespace Parser
module AST =
    //Provides token types
    open Tokeniser

    //Provides control functions and types for parser
    open Parser.ControlAndTypes

    //Provides return wrapper functions
    open ReturnControl.Main
    //Provides return wrapper functions exclusive for the parser
    open ReturnControl.AST
    //Provides variable types
    open Variable

    ///Parse column wrapped list.
    ///Args: Unit (no vars in); token list to be parsed.
    ///Returns: Node with wrapped column list stored in tree, rest of tokens.
    let ColumnWrappedList () (tokenList:Tokeniser.tokens) : ReturnCode<node*Tokeniser.tokens> =
        ///Match column functions: "function(column name)"
        let (|FunctionMatch|_|) (lst:Tokeniser.tokens) =
            let validFunctions = [| "AVG" ; "MAX" ; "MIN" ; "SUM" ; "ROUND" |]
            match lst with
            | Token.Name(funName) :: Token.Operator("(") :: Token.Name(colName) :: Token.Operator(")") :: rest when Array.contains funName validFunctions ->
                [Item(Key(Function),Literal(Token.Name(funName))) ; Item(Key(Name),Literal(Token.Name(colName)))]
                |> fun nodeList -> Some(nodeList,rest)
            | _ -> None
        ///Match column aliases: "column name AS alias name"
        let (|AliasMatch|_|) = function //Input: Tokeniser.tokens
            | Token.Name(colName) :: Token.Name("AS") :: Token.Literal(colAlias) :: rest ->
                [Item(Key(Name),Literal(Token.Name(colName))) ; Item(Key(Alias),Literal(Token.Literal(colAlias)))]
                |> fun nodeList -> Some(nodeList,rest)
            | _ -> None
        ///Parse through list of tokens, finding wrapped column names
        let rec parse (outLst:node list) (lst:Tokeniser.tokens) (nextColumn:bool) =
            match lst with
            | FunctionMatch (funLst,rest) when nextColumn -> parse (Branch(Key(Function),funLst) :: outLst) rest false
            | AliasMatch (aliasLst,rest) when nextColumn -> parse (Branch(Key(Alias),aliasLst) :: outLst) rest false
            | Token.Name(name) :: rest when nextColumn -> parse (Item(Key(Name),Literal(Token.Name(name))) :: outLst) rest false
            | Token.Operator(",") :: rest when not nextColumn -> parse outLst rest true
            | item :: _ when nextColumn -> Error(sprintf "Expected wrapped column name, got %A" item)
            | rest -> Result(outLst,rest)
        parse [] tokenList true
        |> UnwrapResultThrough (fun (columnList,rest) -> Branch(Key(Column),(List.rev columnList)),rest)

    ///Parse column type list.
    ///Args: Unit (no vars in); token list to be parsed.
    ///Returns: Node with column and type information stored in tree, rest of tokens.
    let ColumnTypeList () (tokenList:Tokeniser.tokens) =
        ///Match column and type: "column name type"
        let (|ColumnTypeMatch|_|) = function //Input: Tokeniser.tokens
            | Token.Name(columnName) :: Token.Name(columnType) :: rest when Variable.isValidVarType columnType ->
                [Item(Key(Name),Literal(Token.Name(columnName))) ; Item(Key(Type),Literal(Token.Name(columnType)))]
                |> fun nodeList -> Branch(Key(Column),nodeList),rest
                |> fun res -> Some(Result(res))
            | Token.Name(_) :: Token.Name(columnType) :: _ ->
                Some(Error(sprintf "Expected valid column type, got %s" columnType))
            | Token.Name(_) :: item :: _ ->
                Some(Error(sprintf "Expected valid column type, got %A" item))
            | Token.Name(_) :: _ ->
                Some(Error("Expected valid column type, ran out of tokens"))
            | _ ->
                None
        ///Parse list of tokens, finding sets of column names and types.
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

    ///Parse some value
    ///Args: Variables (name and type for checking); token list to be parsed.
    ///Returns: Node with value information stored in tree, rest of tokens, type of parsed value.
    let ValueItemWithTypecheck (vars:Variable.typeContainer) (input:Tokeniser.tokens) =
        ///Format output of parsed value
        let output (tLst:Tokeniser.tokens) (nod:node) (valType:Variable.varType) = Result(nod,tLst,valType)
        match input with
        | Token.Value(value) :: rest ->
            match value with
            | Token.Integer(_) -> Variable.Integer
            | Token.Byte(_) -> Variable.Byte
            | Token.Floating(_) -> Variable.Float
            | Token.Boolean(_) -> Variable.Boolean
            |> output rest (Item(Key(Number),Literal(Token.Value(value))))
        | Token.Literal(str) :: rest ->
            output rest (Item(Key(String),Literal(Token.Literal(str)))) Variable.String
        | Token.Name(varName) :: rest when vars.ContainsKey varName ->
            output rest (Item(Key(Variable),Literal(Token.Name(varName)))) vars.[varName]
        | Token.Name(varName) :: rest -> Error(sprintf "Could not find declaration for variable %s" varName)
        | tok :: _ -> Error(sprintf "Invalid value %A" tok)
        | [] -> Error("Run out of tokens when expected value")

    ///Parse some value
    ///Args: Variables (name and type for checking); token list to be parsed.
    ///Returns: Node with value information stored in tree, rest of tokens.
    let ValueItem (vars:Variable.typeContainer) (input:Tokeniser.tokens) =
        ValueItemWithTypecheck vars input
        |> UnwrapResultThrough (fun (nodeList,tokenList,_) -> (nodeList,tokenList))

    ///Parse a "set column" list.
    ///Args: Variables (name and type for checking); token list to be parsed.
    ///Returns: Node with set column (column name and value) information stored in tree, rest of tokens.
    let SetColumnList (vars:Variable.typeContainer) (tokenList:Tokeniser.tokens) =
        ///Parse list of tokens, finding sets of column names and values to be set in said columns.
        let rec parse (outLst:node list) (tLst:Tokeniser.tokens) (nextItem:bool) =
            ///Construct a single set, consisting of a column name and value to set it to.
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

    ///Parse a list of conditions.
    ///Args: Variables (name and type for checking); token list to be parsed.
    ///Returns: Node with set column (column name and value) information stored in tree, rest of tokens. (Optional)
    let ConditionsList (vars:Variable.typeContainer) (input:Tokeniser.tokens) : (ReturnCode<node*Tokeniser.tokens> option) =
        ///Match a single condition. "column name operator value"
        let (|ConditionMatch|) (tokenList:Tokeniser.tokens) =
            ///Check if operator is for comparing two values (condition operator)
            let isCompareOp str =
                let compareOperators = [| "=" ; "<>" ; "<" ; ">" ; "<=" ; ">=" |]
                Array.contains str compareOperators
            ///Process output by converting arguments into AST format
            let output name op (valueItem,tLst) =
                [ Item(Key(Name),Literal(Token.Name(name))) ; Item(Key(Operator),Literal(Token.Operator(op))) ; Item(Key(Value),valueItem) ]
                |> fun x -> x,tLst
            match tokenList with
            | Token.Name(name) :: Token.Operator(op) :: rest when isCompareOp op ->
                rest
                |> ValueItem vars
                |> UnwrapResultThrough (output name op)
            | _ -> Error("Invalid condition format")
        ///Parse a single condition
        let interpretCondition func lst =
            match lst with
            | ConditionMatch condition ->
                condition
                |> UnwrapResultInto func
        ///Parse list of tokens, finding remaining (optional) conditions and interpret them into tree form
        let rec parse (outLst:node list) (tokenList:Tokeniser.tokens) : ReturnCode<node list*Tokeniser.tokens> =
            match tokenList with
            | Token.Name("AND") :: rest ->
                rest
                |> interpretCondition (fun (nodeBody,newTLst) -> parse (Branch(Key(And),nodeBody) :: outLst) newTLst)
            | Token.Name("OR") :: rest ->
                rest
                |> interpretCondition (fun (nodeBody,newTLst) -> parse (Branch(Key(Or),nodeBody) :: outLst) newTLst)
            | rest -> Result(outLst,rest)
        ///Convert first condition to tree form and search for optional other conditions - compile into tree
        let parseExtraConditions (nodeBody,newTLst) =
            parse [Branch(Key(Condition),nodeBody)] newTLst
            |> UnwrapResultThrough (fun (whereBody,tailTokenList) -> Branch(Key(Where),List.rev whereBody),tailTokenList)
        match input with
        | Token.Name("WHERE") :: rest ->
            rest
            |> interpretCondition parseExtraConditions
            |> fun x -> Some(x)
        | _ -> None

    ///Parse a list of ordering controllers.
    ///Args: Unit (no variables required); token list to be parsed.
    ///Returns: Node with ordering information stored in tree, rest of tokens. (Optional)
    let OrderList () (input:Tokeniser.tokens) =
        ///Parse through list of tokens, convert to ordering item tree branches
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

    ///Parse a single table name
    ///Args: Unit (no variables required); token list to be parsed.
    ///Returns: Node of table name stored in tree, rest of tokens.
    let TableName () (tokenList:Tokeniser.tokens) =
        match tokenList with
        | Token.Name(tableName) :: rest ->
            Result(Item(Key(Name),Literal(Token.Name(tableName))),rest)
        | item :: rest -> Error(sprintf "Expected table name, got %A" item)
        | [] -> Error("Expected table name, ran out of tokens")

    ///Parse a single (wrapped) table name: Specify the keyword that must appear before the table name.
    ///Args: Keyword that precedes table name; Unit (no variables required); token list to be parsed.
    ///Returns: Node with ordering information stored in tree, rest of tokens. (Optional)
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

    ///Parse a list of ordering controllers.
    ///Args: Unit (no variables required); token list to be parsed.
    ///Returns: Node with column names stored in tree, rest of tokens, number of column names. (Optional)
    let ColumnNameList () (tokenList:Tokeniser.tokens) = //?? Need to look for an end bracket
        ///Parse through list of tokens, convert to tree.
        let rec parse (outLst:node list) (lst:Tokeniser.tokens) (nextItem:bool) (numColumns:int) =
            match lst with
            | Token.Name(name) :: rest when nextItem -> parse (Item(Key(Name),Literal(Token.Name(name))) :: outLst) rest false (numColumns+1)
            | Token.Operator(",") :: rest when not nextItem -> parse outLst rest false numColumns
            | item :: rest when nextItem -> Error(sprintf "Expected column name, got %A" item)
            | [] when nextItem -> Error("Expected column name, ran out of tokens")
            | rest -> Result(outLst,rest,numColumns)
        match tokenList with
        | Token.Operator("(") :: rest ->
            parse [] rest true 0
            |> UnwrapResultThrough (fun (nodeList,tokenList,numColumns) -> Branch(Key(Column),List.rev nodeList),tokenList,numColumns)
            |> fun x -> Some(x)
        | _ -> None

    ///Parse a list of values controllers.
    ///Args: Variables (name and type) * (optional) number of columns; token list to be parsed.
    ///Returns: Node with ordering information stored in tree, rest of tokens. (Optional)
    let ValueList (vars:Variable.typeContainer,numColumns:int option) (tokenList:Tokeniser.tokens) : ReturnCode<node*Tokeniser.tokens> =
        ///Use value item as an active pattern match (no type check).
        let (|ValueMatch|) = ValueItem
        ///Parse through list of tokens, find values and convert to tree - counting number of values found
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
        ///Compare number of column names and number of values
        let compareValues num =
            match numColumns with
            | Some(numCol) when numCol = num -> true
            | None -> true
            | _ -> false
        ///Process parsing output to get output for this function
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

    ///Parse a list of table names.
    ///Args: Unit (no variables used); token list to be parsed.
    ///Returns: Node with list of table names stored in tree, rest of tokens.
    let TableList () (tokenList:Tokeniser.tokens) : ReturnCode<node*Tokeniser.tokens> =
        ///Parse through list of tokens, find table names
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

    ///Parse a limit number and optional offset.
    ///Args: Unit (no variables used); token list to be parsed.
    ///Returns: Node with limit number and offset stored in tree, rest of tokens. (Optional)
    let LimitItem () (input:Tokeniser.tokens) =
        ///Match for offset term (optional).
        let (|OffsetMatch|_|) = function //Input: Tokeniser.tokens
            | Token.Name("OFFSET") :: Token.Value(Token.Integer(offsetVal)) :: rest -> Some(Result(offsetVal,rest))
            | Token.Name("OFFSET") :: rest -> Some(Error("Expected offset value (integer)"))
            | _ -> None
        ///Compile output into tree form if successful
        let successOutput tokenList nodeList = Branch(Key(Limit),nodeList),tokenList
        ///Check for optional offset
        let checkForOffset tokenList (limitVal:int) =
            ///Create item containing limit.
            let limitItem = Item(Key(Value),Literal(Token.Value(Token.Integer(limitVal))))
            ///Compile output into tree form if an offset is specified
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

    ///Parse a singular statement.
    ///Args: Variables (used for name and type checking); token list to be parsed.
    ///Returns: Node containining statement stored in tree, rest of tokens.
    let (|BranchMatch|_|) (vars:Variable.typeContainer) (tokenList:Tokeniser.tokens) =
        ///Format output of branch representing statement.
        let output (key:keyword) (result:ReturnCode<node list*Tokeniser.tokens*Variable.typeContainer>) =
            result
            |> UnwrapResultThrough (fun (nodeList,tokenList,vars) -> (Key(key),nodeList,tokenList,vars))
        ///SELECT ColumnNameWrappedList FROM TableNameList [WHERE ConditionsList] [ORDER BY OrderList] [LIMIT LimitVal [OFFSET OffsetVal]]
        let selectParse (tokenList:Tokeniser.tokens) =
            Result([],tokenList,vars)
            |> ReturnWrapper NoVarsInput NoVarsOutput ColumnWrappedList
            |> ReturnWrapper NoVarsInput NoVarsOutput TableList
            |> OptionalReturnWrapper VarsInput NoVarsOutput ConditionsList
            |> OptionalReturnWrapper NoVarsInput NoVarsOutput OrderList
            |> OptionalReturnWrapper NoVarsInput NoVarsOutput LimitItem
            |> UnwrapResultThrough (fun (nodeList,tokenList,varMap) -> List.rev nodeList,tokenList,varMap)
        ///INSERT INTO TableName [(ColumnNameList)] VALUES (ValueList)
        let insertParse (tokenList:Tokeniser.tokens) =
            Result([],tokenList,vars)
            |> ReturnWrapper NoVarsInput NoVarsOutput (WrappedTableName "INTO")
            |> OptionalReturnWrapper NoVarsInput NumberOutput ColumnNameList
            |> ReturnWrapper VarsAndNumberInput NoVarsOutput ValueList
            |> UnwrapResultThrough (fun (nodeList,tokenList,varMap) -> List.rev nodeList,tokenList,varMap)
        ///UPDATE TableName SET SetColumnList [WHERE ConditionsList]
        let updateParse (tokenList:Tokeniser.tokens) =
            Result([],tokenList,vars)
            |> ReturnWrapper NoVarsInput NoVarsOutput TableName
            |> ReturnWrapper VarsInput NoVarsOutput SetColumnList
            |> OptionalReturnWrapper VarsInput NoVarsOutput ConditionsList
            |> UnwrapResultThrough (fun (nodeList,tokenList,varMap) -> List.rev nodeList,tokenList,varMap)
        ///SET VariableName = Operation
        let setParse (tokenList:Tokeniser.tokens) =
            ///Match value, returning type for checking
            let (|ValueMatch|) = ValueItemWithTypecheck
            let mathsOperators = [| "+" ; "-" ; "*" ; "/" |]
            let validMathsTypes = [| Variable.Integer ; Variable.Float |]
            ///Parse through list of tokens, find set expression
            let rec parse (outLst:node list) (tLst:Tokeniser.tokens) (nextItem:bool) (expressionType:Variable.varType) =
                ///Check types match and process
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
        ///DECLARE VariableName VariableType
        let declareParse (tokenList:Tokeniser.tokens) =
            ///Check if variable hasn't already been declared
            let isValidVarName (name:string) =
                not (vars.ContainsKey(name))
            ///Update variable list with new definition
            let updatedVarMap (name:string) (varType:string) =
                Map.add name Variable.validTypes.[varType] vars
            match tokenList with
            | Token.Name(varName) :: Token.Name(varType) :: rest when isValidVarName varName && Variable.isValidVarType varType ->
                [Item(Key(Variable),Literal(Token.Name(varName))) ; Item(Key(Type),Literal(Token.Name(varType)))]
                |> fun nodeList -> Result(nodeList,rest,updatedVarMap varName varType)
            | Token.Name(varName) :: Token.Name(varType) :: _ when isValidVarName varName ->
                Error(sprintf "Cannot understand type %s" varType)
            | Token.Name(varName) :: Token.Name(varType) :: _ when Variable.isValidVarType varType ->
                Error(sprintf "Variable name \"%s\" already has been declared" varName)
            | Token.Name(varName) :: item :: _ ->
                Error(sprintf "Expected variable type, got %A" item)
            | item :: _ ->
                Error(sprintf "Expected variable name, got %A" item)
            | [] ->
                Error("Expected variable name, ran out of tokens")
        ///DELETE FROM TableName [WHERE ConditionsList]
        let deleteParse (tokenList:Tokeniser.tokens) =
            Result([],tokenList,vars)
            |> ReturnWrapper NoVarsInput NoVarsOutput TableList
            |> OptionalReturnWrapper VarsInput NoVarsOutput ConditionsList
        ///CREATE TABLE TableName (ColumnTypeList)
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

    ///Top level function.
    ///Convert list of tokens into tree form.
    let getTree (tokenList:Tokeniser.tokens) =
        ///Parse through list of tokens, finding statements (separated by ";").
        let rec parse (outLst:node list) (tokens:Tokeniser.tokens) (vars:Variable.typeContainer) (nextStatement:bool) : ReturnCode<node list> =
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
