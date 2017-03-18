namespace ExecutionEngine
module Parse =
    open Parser.ControlAndTypes
    open ReturnControl.Main

    open Main

    open Tokeniser

    let interpretWrappedColumnList columnLeaves =
        let rec parse branches =
            match branches with
            | Item(Key(Column),columnWrappedName) :: rest ->
                columnWrappedName
                match rest with
                | 

    let interpetSelect branches variables =
        open Select
        let unwrapChildren =
            match branches with
            | Branch(Key(Column),wrappedColumnList) :: Branch(Key(From),tableNameList) :: optionalLeaves ->
                (unwrapWrappedColumnList wrappedColumnList),(unwrapTableNameList tableNameList),optionalLeaves
            | Branch(Key(Column),_) :: item :: _ ->
                Error (sprintf "Expected from branch, got %A" item)
            | item :: _ ->
                Error (sprintf "Expected column branch, got %A" item)
            | [] ->
                Error("Expected column branch, ran out of tree")
                

    let interpetInsert branches variables =
        open Insert
    let interpetUpdate branches variables =
        open Update
    let interpetSet branches variables =
        open Set

    let interpetDeclare branches variables =
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
