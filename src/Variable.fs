namespace Variable
module Variable =
    type varType =
        | Integer
        | Float
        | Boolean
        | Byte
        | String

    type varContent =
        | Integer of int option
        | Float of float option
        | Boolean of bool option
        | Byte of byte option
        | String of string option

    type typeContainer = Map<string,varType>

    type contentsContainer = Map<string,varContent>

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
