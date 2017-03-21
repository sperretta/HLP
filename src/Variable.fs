namespace Variable
module Variable =
    ///Types a variable can take
    type varType =
        | Integer
        | Float
        | Boolean
        | Byte
        | String

    ///Variable declaration for comparing variables without values
    type typeContainer = Map<string,varType>
    ///Row data type: name of variable name and stored data
    type rowContainer = Map<string,databaseStructure.databaseStructure.boxData>
    type contentsContainer = rowContainer

    ///Map to convert name of type into actual variable type
    let validTypes =
        [
        "string" , String ;
        "byte" , Byte ;
        "int" , Integer ;
        "float" , Float ;
        "boolean" , Boolean
        ]
        |> Map.ofList

    ///Check if the string given names a valid type
    let isValidVarType (name:string) =
        Map.containsKey name validTypes

    ///Map to convert name of type into database storage type
    ///Used for comparing types (blank database value created)
    let validDatabaseTypes =
        [
        "string" , databaseStructure.databaseStructure.String None ;
        "byte" , databaseStructure.databaseStructure.Byte None ;
        "int" , databaseStructure.databaseStructure.Int None ;
        "float" , databaseStructure.databaseStructure.Float None ;
        "boolean" , databaseStructure.databaseStructure.Bool None ;
        ]
        |> Map.ofList