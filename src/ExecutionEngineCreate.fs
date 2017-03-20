namespace ExecutionEngine
module Create =

    open System.IO
    open System
    open databaseStructure.databaseStructure
    open ReturnControl.Main

    let rec insertEmptyTable columnTypeList tableName database =
        match !database with
        | TableNode (_, _, _, tail) -> insertEmptyTable columnTypeList tableName tail
        | INilTable ->
            let newTail = ref INilTable
            let emptyRow = ref INilRow
            database := TableNode (emptyRow, tableName, columnTypeList, newTail)

    let create tableName columnTypeList database =
        let isPresent = chooseTable database tableName
        match isPresent with
        | Some _ -> Error("CREATE: The table name specified is already in the database.")
        | None _ -> Result(insertEmptyTable columnTypeList tableName database)