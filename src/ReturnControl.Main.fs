namespace ReturnControl
module Main =
    ///Result or error
    type ReturnCode<'a> =
        | Result of 'a
        | Error of string

    ///If success, apply function to result and wrap in success, else return error
    let UnwrapResultThrough func (from:ReturnCode<'a>) =
        match from with
        | Result(res) -> Result(func res)
        | Error(str) -> Error(str)

    ///If success, apply function (with ReturnCode output) to result, else return error
    let UnwrapResultInto func (from:ReturnCode<'a>) =
        match from with
        | Result(res) -> func res
        | Error(str) -> Error(str)