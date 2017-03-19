namespace Variable
module Variable =
    type varType =
        | Integer
        | Float
        | Boolean
        | Byte
        | String

    type typeContainer = Map<string,varType>
    type contentsContainer = Map<string,varType*databaseStructure.databaseStructure.boxData>
    type rowContainer = Map<string,databaseStructure.databaseStructure.boxData>

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
