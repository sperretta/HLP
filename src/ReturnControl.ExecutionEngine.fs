namespace ReturnControl
module ExecutionEngine =
    open Main
    ///Unwrap two results and apply function to them if successful, wrap in success
    let UnwrapTwoResultsThrough func (from1:ReturnCode<'a>) (from2:ReturnCode<'b>) =
        match from1 with
        | Result(res) ->
            match from2 with
            | Result(res2) -> Result(func res res2)
            | Error(str) -> Error(str)
        | Error(str) -> Error(str)

    ///Unwrap two results and apply function to them if successful, check success
    let UnwrapTwoResultsInto func (from1:ReturnCode<'a>) (from2:ReturnCode<'b>) =
        match from1 with
        | Result(res) ->
            match from2 with
            | Result(res2) -> func res res2
            | Error(str) -> Error(str)
        | Error(str) -> Error(str)