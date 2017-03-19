namespace Parser
module ControlAndTypes =
    open Tokeniser

    type keyword =
        | Select
        | Insert
        | Update
        | Set
        | Declare
        | Delete
        | Create
        | From
        | Where
        | Order
        | Limit
        | Offset
        | Value
        | Function
        | Name
        | As
        | Condition
        | Variable
        | Number
        | String
        | Column
        | Alias
        | And
        | Or
        | Operator
        | Ascend
        | Descend
        | Into
        | Type

    type node =
        | Branch of Name:node * Children:node list
        | Item of Name:node * Child:node
        | Literal of Token.content
        | Key of keyword
        | Null

    type passThroughInputs = {nodeList:node list ; tokenList:Tokeniser.tokens ; varList:Variable.Variable.typeContainer ; number:int option}

    let VarsInput = fun (nodeList,tokenList,vars) ->
        let inputArgs = vars
        let outputs = {nodeList=nodeList ; tokenList=tokenList ; varList=vars ; number = None}
        (tokenList,inputArgs,outputs)
    let VarsOutput constInputs =
        let resultsFromFunc = fun (node,newTokenList,newVars) -> (node :: constInputs.nodeList),newTokenList,newVars
        let passThroughAll = (constInputs.nodeList,constInputs.tokenList,constInputs.varList)
        (resultsFromFunc,passThroughAll)
    let VarsAndNumberInput = fun (nodeList,tokenList,vars,number:int option) ->
        let inputArgs = vars,number
        let outputs = {nodeList=nodeList ; tokenList=tokenList ; varList=vars ; number = number}
        (tokenList,inputArgs,outputs)
    let VarsAndNumberOutput constInputs =
        let resultsFromFunc = fun (node,newTokenList,newVars,newNumber) -> (node :: constInputs.nodeList),newTokenList,newVars,Some(newNumber)
        let passThroughAll = (constInputs.nodeList,constInputs.tokenList,constInputs.varList,constInputs.number)
        (resultsFromFunc,passThroughAll)
    let NumberInput = fun (nodeList,tokenList,vars,number:int option) ->
        let inputArgs = number
        let outputs = {nodeList=nodeList ; tokenList=tokenList ; varList=vars ; number = number}
        (tokenList,inputArgs,outputs)
    let NumberOutput constInputs =
        let resultsFromFunc = fun (node,newTokenList,newNumber) -> (node :: constInputs.nodeList),newTokenList,constInputs.varList,Some(newNumber)
        let passThroughAll = (constInputs.nodeList,constInputs.tokenList,constInputs.varList,constInputs.number)
        (resultsFromFunc,passThroughAll)
    let NoVarsInput = fun (nodeList,tokenList,vars) ->
        let inputArgs = ()
        let outputs = {nodeList=nodeList ; tokenList=tokenList ; varList=vars ; number = None}
        (tokenList,inputArgs,outputs)
    let NoVarsOutput constInputs =
        let resultsFromFunc = fun (node,newTokenList) -> (node :: constInputs.nodeList),newTokenList,constInputs.varList
        let passThroughAll = (constInputs.nodeList,constInputs.tokenList,constInputs.varList)
        (resultsFromFunc,passThroughAll)