namespace ExecutionEngine
module Main =
    type ColumnFunctions =
        | AVG
        | MAX
        | MIN
        | SUM
        | ROUND

    type ColumnNameWrappedListType =
        | Function of FunName:ColumnFunctions*ColName:string
        | Name of string
        | Alias of ColName:string*AliasName:string

    type rowData = Map<string,databaseStructure.databaseStructure.boxData>