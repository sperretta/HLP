namespace ExecutionEngine
module Parse =
    open Parser.ControlAndTypes
    open ReturnControl.Main
    open ReturnControl.ExecutionEngine

    open Main

    open Tokeniser

    //let interpretWrappedColumnList columnLeaves =
    //    let rec parse branches =
    //        match branches with
    //        | Item(Key(Column),columnWrappedName) :: rest ->
    //            columnWrappedName
    //            match rest with
    //            | 

    let interpetSelect branches variables =
        //open Select
        let unwrapChildren =
            match branches with
            | Branch(Key(Column),wrappedColumnList) :: Branch(Key(From),tableNameList) :: optionalLeaves ->
                ((unwrapWrappedColumnList wrappedColumnList),(unwrapTableNameList tableNameList),optionalLeaves)
                |> fun x -> Result(x)
            | Branch(Key(Column),_) :: item :: _ ->
                Error (sprintf "Expected from branch, got %A" item)
            | item :: _ ->
                Error (sprintf "Expected column branch, got %A" item)
            | [] ->
                Error("Expected column branch, ran out of tree")
        unwrapChildren
                

    let interpetInsert branches (variables:Variable.Variable.contentsContainer) =
        let interpretValueItem value =
            match value with
            | Item(Key(Number),Literal(Token.Value(numberItem))) ->
                match numberItem with
                | Token.Integer(num) -> Result(Variable.Variable.varContent.Integer (Some num))
                | Token.Floating(num) -> Result(Variable.Variable.varContent.Float (Some num))
                | Token.Byte(num) -> Result(Variable.Variable.varContent.Byte (Some num))
                | Token.Boolean(num) -> Result(Variable.Variable.varContent.Boolean (Some num))
            | Item(Key(String),Literal(Token.Name(stringItem))) ->
                Result(Variable.Variable.varContent.String (Some stringItem))
            | Item(Key(Variable),Literal(Token.Name(varName))) ->
                Result(variables.[varName])
            | _ -> Error(sprintf "Expected value item, got %A" value)
        let interpretValueList nodeList =
            let rec parse outLst inLst =
                match inLst with
                | Item(Key(Value),valueItem) :: rest ->
                    interpretValueItem valueItem
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
        let interpretColumnNameList nodeList =
            let rec parse outLst inLst =
                match inLst with
                | Item(Key(keyword.Name),Literal(Token.Name(colName))) :: rest ->
                    parse (colName :: outLst) rest
                | [] -> Result(outLst)
                | other -> Error(sprintf "Expected column name, got %A" other)
            parse [] nodeList
            |> UnwrapResultThrough (fun colList -> List.rev colList)
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
            |> UnwrapResultInto (fun (colList,valueList) -> Insert.run tableName colList valueList)
            |> fun func -> Backend.Database.run func
        | item :: _ -> Error(sprintf "Expected table name, got %A" item)
        | [] -> Error("Expected table name, but damaged tree, missing next branch")


    let interpetUpdate branches variables =
        open Update
    let interpetSet branches variables =
        open Set

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
        open Delete

    let interpetCreate branches =
        let interpretColumnTypeItem children =
            match children with
            | Item(Key(Name),Literal(Token.Name(colName))) :: Item(Key(Type),Literal(Token.Name(colType))) ::  [] ->
                if Map.containsKey colType Variable.Variable.validTypes then (colName,Variable.Variable.validTypes.[colType])
                else Error(sprintf "Unrecognised column type %s" colType)
            | _ -> Error(sprinft "Unrecognised column-type pattern %A" children)
        let interpretColumnTypeList children =
            let rec parse outLst (inLst:node list) =
                match inLst with
                | Branch(Key(Column),columnType) :: rest ->
                    interpretColumnTypeItem columnType
                    |> UnwrapResultInto (fun newItem -> parse (newItem :: outLst) rest)
                | [] -> Result(outLst)
        match branches with
        | Item(Key(Name),Literal(Token.Name(tableName))) :: Branch(Key(Value),valueChildren) :: [] ->
            interpretColumnTypeList valueChildren
            |> fun valueList -> Create.run tableName (List.rev valueList)
            |> fun func -> Backend.Database.run func
        | _ -> Error (sprinf "Unrecognised sequence after CREATE %A" branches)

    let runThroughTree (tree:node list) =
        let rec parse (branches:node list) (varMap:Variable.Variable.contentsContainer) (returnTableList:returnTableListType) = //Returntabletype list =
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
