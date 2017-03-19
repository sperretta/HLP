namespace ExecutionEngine
module Parse =
    open Parser.ControlAndTypes
    open ReturnControl.Main
    open ReturnControl.ExecutionEngine

    open Main

    open Tokeniser
    open Variable

    //let interpretWrappedColumnList columnLeaves =
    //    let rec parse branches =
    //        match branches with
    //        | Item(Key(Column),columnWrappedName) :: rest ->
    //            columnWrappedName
    //            match rest with
    //            | 

    let interpretTableNameList branches =
        let rec parse outLst inLst =
            match inLst with
            | Literal(Token.Name(tableName)) :: rest ->
                parse (tableName :: outLst) rest
            | item :: _ -> Error(sprintf "Expected table name, got %A" item)
            | [] -> Result(outLst)
        parse [] branches
        |> UnwrapResultThrough (fun x -> List.rev x)

    let interpretColumnNameList nodeList =
        let rec parse outLst inLst =
            match inLst with
            | Item(Key(keyword.Name),Literal(Token.Name(colName))) :: rest ->
                parse (colName :: outLst) rest
            | [] -> Result(outLst)
            | other -> Error(sprintf "Expected column name, got %A" other)
        parse [] nodeList
        |> UnwrapResultThrough (fun colList -> List.rev colList)

    let interpretValueItem value (variables:Variable.contentsContainer) =
        match value with
        | Item(Key(Number),Literal(Token.Value(numberItem))) ->
            match numberItem with
            | Token.Integer(num) -> Result(Variable.varContent.Integer (Some num))
            | Token.Floating(num) -> Result(Variable.varContent.Float (Some num))
            | Token.Byte(num) -> Result(Variable.varContent.Byte (Some num))
            | Token.Boolean(num) -> Result(Variable.varContent.Boolean (Some num))
        | Item(Key(String),Literal(Token.Name(stringItem))) ->
            Result(Variable.varContent.String (Some stringItem))
        | Item(Key(Variable),Literal(Token.Name(varName))) ->
            Result(variables.[varName])
        | _ -> Error(sprintf "Expected value item, got %A" value)

    let interpretConditionsList branches variables =
        let operators =
            [
            "=" , (=) ;
            "<>" , (<>) ;
            "<" , (<) ;
            ">" , (>) ;
            "<=" , (<=) ;
            ">=" , (>=)
            ]
            |> Map.ofList
        let newConditionFunc colName op (value:node) =
            let getSimplifiedType = function
                | Variable.varContent.Integer _ -> Variable.varContent.Integer None
                | Variable.varContent.Float _ -> Variable.varContent.Float None
                | Variable.varContent.Boolean _ -> Variable.varContent.Boolean None
                | Variable.varContent.Byte _ -> Variable.varContent.Byte None
                | Variable.varContent.String _ -> Variable.varContent.String None
            let interpretedValue =
                interpretValueItem value variables
            let valueType =
                interpretedValue
                |> UnwrapResultThrough getSimplifiedType
            fun (colData:Variable.rowContainer) ->
                if colData.ContainsKey colName then
                    let simplifiedColType =
                        colData.[colName]
                        |> getSimplifiedType
                    if Map.containsKey op operators then
                        match valueType with
                        | Result(colType) when colType = simplifiedColType ->
                            UnwrapResultThrough (operators.[op] colData.[colName]) interpretedValue
                        | Error(str) -> Error(str)
                        | _ -> Error(sprintf "Type of column and value do not match for %s" colName)
                    else Error(sprintf "Operator %s not recognised" op)
                else Error(sprintf "Column name %s not recognised" colName)
        let interpretCondition cond =
            match cond with
            | Item(Key(keyword.Name),Literal(Token.Name(colName))) :: Item(Key(Operator),Literal(Token.Operator(op))) :: Item(Key(Value),value) :: [] ->
                newConditionFunc colName op value
            | _ -> Error(sprintf "Expected condition, got %A" cond)
        let rec parse (outFunc:Map<string,Variable.Variable.contentsContainer> -> ReturnCode<bool>) inLst =
            match inLst with
            | Branch(Key(And),cond) :: rest ->
                fun x ->
                    match (interpretCondition cond) x with
                    | Result(y) ->
                        match outFunc x with
                        | Result(z) -> Result(y && z)
                        | Error(str) -> Error(str)
                    | Error(str) -> Error(str)
                |> fun x -> parse x rest
            | Branch(Key(Or),cond) :: rest ->
                fun x ->
                    match (interpretCondition cond) x with
                    | Result(y) ->
                        match outFunc x with
                        | Result(z) -> Result(y || z)
                        | Error(str) -> Error(str)
                    | Error(str) -> Error(str)
                |> fun x -> parse x rest
            | [] -> Result(outFunc)
        match branches with
        | Branch(Key(Condition),cond) :: rest ->
            parse (interpretCondition cond) rest
        | item :: _ -> Error(sprintf "Expected condition, got %A" item)
        | [] -> Error("Expected condition, ran out of tree")

    let interpetSelect branches variables =
        let unwrapChildren =
            match branches with
            | Branch(Key(Column),wrappedColumnList) :: Branch(Key(From),tableNameList) :: optionalLeaves ->
                UnwrapTwoResultsInto (fun x y -> Result(x,y,optionalLeaves)) (interpretColumnNameList wrappedColumnList) (interpretTableNameList tableNameList)
            | Branch(Key(Column),_) :: item :: _ ->
                Error (sprintf "Expected from branch, got %A" item)
            | item :: _ ->
                Error (sprintf "Expected column branch, got %A" item)
            | [] ->
                Error("Expected column branch, ran out of tree")
        let (|MatchOrder|_|) opt =
            match opt with
            | Branch(Key(Order),_) :: rest -> Some(Error("Order is currently not supported"))
            | _ -> None
        let (|MatchLimit|_|) opt =
            let matchOffset opt2 =
                match opt2 with
                | Item(Key(Offset),Literal(Token.Value(Token.Integer(offsetVal)))) :: [] -> Some(offsetVal)
                | _ -> None
            match opt with
            | Branch(Key(Limit),limitOptions) :: [] ->
                match limitOptions with
                | Item(Key(Value),Literal(Token.Value(Token.Integer(limitVal)))) :: rest ->
                    matchOffset rest
                    |> fun x -> Some(Result(limitVal,x))
                | _ -> Some(Error("Broken tree in matchlimit"))
            | _ -> None
        let dealWithOptionals opt =
            match opt with
            | Branch(Key(Where),condList) :: rest ->
                match rest with
                | MatchOrder err -> err
                | MatchLimit res ->
                    UnwrapTwoResultsThrough (fun x (y,z) -> Some(x),y,z) (interpretConditionsList condList variables) res
            | MatchOrder err -> err
            | MatchLimit res -> UnwrapResultThrough (fun (x,y) -> None,x,y) res
        let sendToBackend (colList,tabList,optional) =
            dealWithOptionals optional
            |> UnwrapResultThrough
                (fun (condFunc,limitVal,offsetVal) ->
                    Select.select colList tabList condFunc limitVal offsetVal
                )
        unwrapChildren
        |> UnwrapResultThrough sendToBackend
                
    let interpetInsert branches (variables:Variable.Variable.contentsContainer) =
        let interpretValueList nodeList =
            let rec parse outLst inLst =
                match inLst with
                | Item(Key(Value),valueItem) :: rest ->
                    interpretValueItem valueItem variables
                    |> UnwrapResultInto (fun x -> parse (x::outLst) rest)
                | [] -> Result(outLst)
                | item :: _ -> Error(sprintf "Expected value item, got %A" item)
            parse [] nodeList
            |> UnwrapResultThrough (fun lst -> List.rev lst)
        let (|MatchValueList|) nodeLst =
            match nodeLst with
            | Branch(Key(Value),valueList) :: [] ->
                interpretValueList valueList
            | [] -> Error("Expected value list, ran out of tree")
            | other -> Error(sprintf "Expected value list, got %A" other)
        let (|MatchColumnAndValueList|_|) nodeLst =
            match nodeLst with
            | Branch(Key(Column),columnNameList) :: rest ->
                match rest with
                | MatchValueList valueList ->
                    UnwrapTwoResultsThrough (fun x y -> x,y) (interpretColumnNameList columnNameList) valueList
                    |> fun x -> Some(x)
            | _ -> None
        let matchRest nodeLst =
            match nodeLst with
            | MatchColumnAndValueList matchedResult -> matchedResult
            | MatchValueList matchedValueList ->
                matchedValueList
                |> UnwrapResultInto (fun x -> Result([],x))
            | _ -> Error (sprintf "Expected column list or value list, got %A" nodeLst)
        match branches with
        | Item(Key(Into),Literal(Token.Name(tableName))) :: rest ->
            matchRest rest
            |> UnwrapResultThrough (fun (colList,valueList) -> Insert.insert tableName colList valueList)
            |> UnwrapResultThrough (fun func -> Backend.Database.run func)
        | item :: _ -> Error(sprintf "Expected table name, got %A" item)
        | [] -> Error("Expected table name, but damaged tree, missing next branch")

    //let interpetUpdate branches variables =
    //    open Update

    let interpretSet branches (variables:Variable.Variable.contentsContainer) =
        //open Set
        let interpretExpression exp =
            let validOperators =
                [
                "+" , (+) ;
                "-" , (-) ;
                "*" , (*) ;
                "/" , (/)
                ]
                |> Map.ofList
            let interpretSetValue value =
                match value with
                | Item(Key(Select),_)
                | Item(Key())
            let rec parse outValue inLst =
                match inLst with
                | Item(Key(Operator),Literal(Token.Operator(op))) :: Item(Key(Value),value) :: rest ->
                    if validOperators.ContainsKey op then
                        interpretSetValue value




                        validOperators.[op] outValue
                    else Error(sprintf "Invalid operator in expression %s" op)
                | [] -> Result(outValue)
                | _ -> Error(sprintf "Expected operator and value, got %A" inLst)
            match exp with
            | Item(Key(Value),value) :: rest ->
                interpretSetValue value
                |> UnwrapResultInto (fun x -> parse x rest)
            | _ -> Error(sprintf "Expected value, got %A" exp)
        match branches with
        | Branch(Key(Variable),Literal(Token.Name(varName))) :: Item(Key(Operator),exp) :: [] ->
            if variables.ContainsKey varName then
                interpretExpression exp



            else Error(sprintf "No variable declared %s" varName)
        | _ -> Error(sprintf "Expected \"varName valueString\", got %A" branches)

    let interpetDeclare branches (variables:Variable.Variable.contentsContainer) =
        match branches with
        | Item(Key(Variable),Literal(Token.Name(varName))) :: Item(Key(Type),Literal(Token.Name(varType))) :: [] ->
            if variables.ContainsKey(varName) then Error(sprintf "Variable name \"%s\" has already been declared" varName)
            else
                if Map.containsKey varType Variable.Variable.validTypes then
                    Map.add varName Variable.Variable.validTypes.[varType] variables
                    |> fun x -> Result(x)
                else Error(sprintf "Unrecognised variable type \"%s\"" varType)
        | _ -> Error (sprintf "Unrecognised sequence after DECLARE %A" branches)

    let interpetDelete branches variables =
        match branches with
        | Branch(Key(From),tableList) :: rest ->
            match rest with
            | Branch(Key(Where),conditionsList) :: [] ->
                interpretConditionsList conditionsList variables
                |> UnwrapResultInto (fun x -> Result(Some x))
            | item :: _ -> Error(sprintf "Expected nothing or conditions list, got %A" item)
            | [] -> Result(None)
            |> UnwrapTwoResultsInto (fun x y -> Delete.delete x y) (interpretTableNameList tableList)
            |> fun func -> Backend.Database.run func
        | item :: _ -> Error(sprintf "Expected table list, got %A" item)
        | [] -> Error("Expected table list, ran out of tree")

    let interpetCreate branches =
        let interpretColumnTypeItem children =
            match children with
            | Item(Key(keyword.Name),Literal(Token.Name(colName))) :: Item(Key(Type),Literal(Token.Name(colType))) ::  [] ->
                if Map.containsKey colType Variable.Variable.validTypes then Result(colName,Variable.Variable.validTypes.[colType])
                else Error(sprintf "Unrecognised column type %s" colType)
            | _ -> Error(sprintf "Unrecognised column-type pattern %A" children)
        let interpretColumnTypeList children =
            let rec parse outLst (inLst:node list) =
                match inLst with
                | Branch(Key(Column),columnType) :: rest ->
                    interpretColumnTypeItem columnType
                    |> UnwrapResultInto (fun newItem -> parse (newItem :: outLst) rest)
                | item :: _ -> Error(sprintf "Expected column node, got %A" item)
                | [] -> Result(outLst)
            parse [] children
        match branches with
        | Item(Key(keyword.Name),Literal(Token.Name(tableName))) :: Branch(Key(Value),valueChildren) :: [] ->
            interpretColumnTypeList valueChildren
            |> UnwrapResultThrough (fun valueList -> Create.create tableName (List.rev valueList))
            |> UnwrapResultThrough (fun func -> Backend.Database.run func)
        | _ -> Error (sprintf "Unrecognised sequence after CREATE %A" branches)

    let runThroughTree (tree:node list) =
        let rec parse (branches:node list) (varMap:Variable.contentsContainer) (returnTableList:returnTableListType) = //Returntabletype list =
            match branches with
            | Branch(Key(Select),children) :: rest ->
                interpretSelect children varMap
                |> UnwrapResultInto (fun readTable -> parse rest varMap (readTable :: returnTableList))
            | Branch(Key(Insert),children) :: rest ->
                interpretInsert children varMap
                |> UnwrapResultInto (fun () -> parse rest varMap returnTableList)
            | Branch(Key(Update),children) :: rest ->
                interpretUpdate children varMap
                |> UnwrapResultInto (fun () -> parse rest varMap returnTableList)
            | Branch(Key(Set),children) :: rest ->
                interpretSet children varMap
                |> UnwrapResultInto (fun newVarMap -> parse rest newVarMap returnTableList)
            | Branch(Key(Declare),children) :: rest ->
                interpretDeclare children varMap
                |> UnwrapResultInto (fun newVarMap -> parse rest newVarMap returnTableList)
            | Branch(Key(Delete),children) :: rest ->
                interpretDelete children varMap
                |> UnwrapResultInto (fun () -> parse rest varMap returnTableList)
            | Branch(Key(Create),children) :: rest ->
                interpretCreate children
                |> UnwrapResultInto (fun () -> parse rest varMap returnTableList)
            | item :: _ -> Error(sprintf "Unrecognised node %A in top level parse" item)
            | [] -> Result(varMap,returnTableList)
        let convertToOutput (varMap,returnTableList) =
            //Convert var map to strings, reverse table list
            (varMap,List.rev returnTableList)
        parse tree Map.empty []
        |> UnwrapResultThrough convertToOutput
