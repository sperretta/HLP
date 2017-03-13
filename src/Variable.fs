namespace Variable
module Variable =
    type varType =
        | Integer
        | Float
        | Boolean
        | Byte
        | String

    type varContent =
        | Integer of int
        | Float of float
        | Boolean of bool
        | Byte of byte
        | String of string

    type typeContainer = Map<string,varType>

    type contentsContainer = Map<string,varType*varContent>

    let validTypes =
        [
        "string" , varType.String ;
        "byte" , varType.Byte ;
        "int" , varType.Integer ;
        "float" , varType.Float ;
        "boolean" , varType.Boolean
        ]
        |> Map.ofList

    let isValidVarType (name:string) =
        Map.containsKey name validTypes
