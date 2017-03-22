namespace ReturnControl
open Main
module AST =
    //Parsing control functions for AST

    ///Get token list from parsing output control function
    let getTokenList (tokenList,_,_) = tokenList
    ///Get input arguments from parsing output control function
    let getInputArgs (_,inputArgs,_) = inputArgs
    ///Get constant outputs from parsing output control function
    let getOutputConsts (_,_,outputConsts) = outputConsts

    ///Wrapper for controlling parsing for mandatory syntactic units.
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

    ///Wrapper for controlling parsing for optional syntactic units.
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