namespace Variable
module Variable =
    type varType =
        | Integer
        | Float
        | Boolean
        | Byte
        | String

    type typeContainer = Map<string,varType>

    let validTypes =
        [
        "string" , String ;
        "byte" , Byte ;
        "int" , Integer ;
        "float" , Float ;
        "boolean" , Boolean
        ]
        |> Map.ofList

    let isValidVarType (name:string) =
        Map.containsKey name validTypes
