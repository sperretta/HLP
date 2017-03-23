namespace TokeniserTests
module Tests =
    open Expecto
    open Tokeniser
    open ReturnControl.Main

    [<Tests>]
    let testTokeniser =
        testList "Tests for the tokeniser" [
            testCase "Empty list" <| fun () ->
                let expectedOutput = Result(List<Token.content>.Empty)
                let actualOutput = Tokeniser.getTokens("")
                Expect.equal (expectedOutput) (actualOutput) "Empty string"
            testProperty "Random output with normal single spaces" <| fun expectedOutput ->
                let input =
                    let rec parse outLst inLst =
                        let newOutLst rest str =
                            sprintf "%s " str
                            |> fun x -> parse (x :: outLst) rest
                        match inLst with
                        | Token.Name(str) :: rest -> newOutLst rest str
                        | Token.Literal(str) :: rest ->
                            sprintf "\"%s\"" str
                            |> newOutLst rest
                        | Token.Value(Token.Integer(value)) :: rest ->
                            sprintf "%i" value
                            |> newOutLst rest
                        | Token.Value(Token.Floating(value)) :: rest ->
                            sprintf "%f" value
                            |> newOutLst rest
                        | Token.Value(Token.Byte(value)) :: rest->
                            sprintf "%iB" value
                            |> newOutLst rest
                        | Token.Value(Token.Boolean(value)) :: rest ->
                            if value then "True" else "False"
                            |> newOutLst rest
                        | Token.Operator(opStr) :: rest -> newOutLst rest opStr
                        | Token.EndStatement :: rest -> newOutLst rest ";"
                        | [] -> outLst
                    parse [] expectedOutput
                    |> List.rev
                    |> List.fold (+) ""
                let actualOutput = Tokeniser.getTokens(input)
                let wrappedExpectedOutput = Result expectedOutput
                Expect.equal wrappedExpectedOutput actualOutput "Random input data"
        ]