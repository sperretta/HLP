namespace ExecutionEngine
module Parse =
    open Parser.ControlAndTypes
    let runThroughTree (tree:node list) =
        let rec parse (branch:node list) =
            match branch with
            | Branch(Key(Select),children) :: rest
            | Branch(Key(Insert),children) :: rest
            | Branch(Key(Update),children) :: rest
            | Branch(Key(Set),children) :: rest
            | Branch(Key(Declare),children) :: rest
            | Branch(Key(Delete),children) :: rest
            | Branch(Key(Create),children) :: rest
            | item :: _ -> Error(sprintf "Unrecognised node in top level parse" item)