namespace ReturnControl
module ExecutionEngine =
    open Main
    let UnwrapTwoResultsThrough func (from1:ReturnCode<'a>) (from2:ReturnCode<'b>) =
        match from1 with
        | Result(res) ->
            match from2 with
            | Result(res2) -> Result(func res res2)
            | Error(str) -> Error(str)
        | Error(str) -> Error(str)

    let UnwrapTwoResultsInto func (from1:ReturnCode<'a>) (from2:ReturnCode<'b>) =
        match from1 with
        | Result(res) ->
            match from2 with
            | Result(res2) -> func res res2
            | Error(str) -> Error(str)
        | Error(str) -> Error(str)