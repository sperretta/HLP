namespace Parser
module ControlAndTypes =
    //Allows use of Tokensiser types
    open Tokeniser

    ///Keywords used in tree
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

    ///AST Node
    type node =
        | Branch of Name:node * Children:node list
        | Item of Name:node * Child:node
        | Literal of Token.content
        | Key of keyword
        | Null

    //Parse control: deal with inputs/outputs to token parsing functions

    ///Record of data passed between token parsing functions.
    ///Data is stored here to be passed around token parsing functions if optional (or if they have optional arguments).
    type passThroughInputs = {nodeList:node list ; tokenList:Tokeniser.tokens ; varList:Variable.Variable.typeContainer ; number:int option}

    //Components of parse control functions
    //Input Args: Arguments to be sent to the parsing function
    //Outputs: Outputs of the parsing function if optional (and not triggered). Also outputs of parsing control if parsing function doesn't output item.

    ///Parsing function uses variables
    let VarsInput = fun (nodeList,tokenList,vars) ->
        let inputArgs = vars
        let outputs = {nodeList=nodeList ; tokenList=tokenList ; varList=vars ; number = None}
        (tokenList,inputArgs,outputs)
    ///Parsing function outputs variables
    let VarsOutput constInputs =
        let resultsFromFunc = fun (node,newTokenList,newVars) -> (node :: constInputs.nodeList),newTokenList,newVars
        let passThroughAll = (constInputs.nodeList,constInputs.tokenList,constInputs.varList)
        (resultsFromFunc,passThroughAll)
    ///Parsing function uses variables and some integer (optional)
    let VarsAndNumberInput = fun (nodeList,tokenList,vars,number:int option) ->
        let inputArgs = vars,number
        let outputs = {nodeList=nodeList ; tokenList=tokenList ; varList=vars ; number = number}
        (tokenList,inputArgs,outputs)
    ///Parsing function outputs variables and some integer (optional)
    let VarsAndNumberOutput constInputs =
        let resultsFromFunc = fun (node,newTokenList,newVars,newNumber) -> (node :: constInputs.nodeList),newTokenList,newVars,Some(newNumber)
        let passThroughAll = (constInputs.nodeList,constInputs.tokenList,constInputs.varList,constInputs.number)
        (resultsFromFunc,passThroughAll)
    ///Parsing function uses some integer (optional)
    let NumberInput = fun (nodeList,tokenList,vars,number:int option) ->
        let inputArgs = number
        let outputs = {nodeList=nodeList ; tokenList=tokenList ; varList=vars ; number = number}
        (tokenList,inputArgs,outputs)
    ///Parsing function outputs some integer (optional)
    let NumberOutput constInputs =
        let resultsFromFunc = fun (node,newTokenList,newNumber) -> (node :: constInputs.nodeList),newTokenList,constInputs.varList,Some(newNumber)
        let passThroughAll = (constInputs.nodeList,constInputs.tokenList,constInputs.varList,constInputs.number)
        (resultsFromFunc,passThroughAll)
    ///Parsing function uses no variables or number - only takes in node list (tree) and token list (interpreted input)
    let NoVarsInput = fun (nodeList,tokenList,vars) ->
        let inputArgs = ()
        let outputs = {nodeList=nodeList ; tokenList=tokenList ; varList=vars ; number = None}
        (tokenList,inputArgs,outputs)
    ///Parsing function outputs no variables or numbers - doesn't change any parameters
    let NoVarsOutput constInputs =
        let resultsFromFunc = fun (node,newTokenList) -> (node :: constInputs.nodeList),newTokenList,constInputs.varList
        let passThroughAll = (constInputs.nodeList,constInputs.tokenList,constInputs.varList)
        (resultsFromFunc,passThroughAll)