namespace ExecutionEngine
module Parse =
    open Parser.ControlAndTypes
    open ReturnControl.Main

    let interpetSelect branches variables =
        open Select
    let interpetInsert branches variables =
        open Insert
    let interpetUpdate branches variables =
        open Update
    let interpetSet branches variables =
        open Set
    let interpetDeclare branches variables =
        open Declare
    let interpetDelete branches variables =
        open Delete
    let interpetCreate branches =
        open Create

    let runThroughTree (tree:node list) =
        let rec parse (branches:node list) (varMap:Variable.Variable.contentContainer) (returnTableList:returnTableListType) = //Returntabletype list =
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
