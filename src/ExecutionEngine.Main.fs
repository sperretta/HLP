namespace ExecutionEngine
module Main =
    ///Valid functions for select statements.
    type ColumnFunctions =
        | AVG
        | MAX
        | MIN
        | SUM
        | ROUND

    ///Store converted column functions.
    type ColumnNameWrappedListType =
        | Function of FunName:ColumnFunctions*ColName:string
        | Name of string
        | Alias of ColName:string*AliasName:string

    ///Store data from a row
    type rowData = Map<string,databaseStructure.databaseStructure.boxData>