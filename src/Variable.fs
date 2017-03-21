namespace Variable
module Variable =
    type varType =
        | Integer
        | Float
        | Boolean
        | Byte
        | String

    type typeContainer = Map<string,varType>
    type rowContainer = Map<string,databaseStructure.databaseStructure.boxData>
    type contentsContainer = rowContainer

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

    let validDatabaseTypes =
        [
        "string" , databaseStructure.databaseStructure.String None ;
        "byte" , databaseStructure.databaseStructure.Byte None ;
        "int" , databaseStructure.databaseStructure.Int None ;
        "float" , databaseStructure.databaseStructure.Float None ;
        "boolean" , databaseStructure.databaseStructure.Bool None ;
        ]
        |> Map.ofList