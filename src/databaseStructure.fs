namespace databaseStructure
module databaseStructure =

    open System.IO
    open System

    type boxData = | String of Option<string>  // To be extended when the wanted data types have been decided
                   | Int of Option<int> 
                   | Float of Option<float>                
                   | Byte of Option<byte>     
                   | Bool of Option<bool>

    // A node has the parameter name (column name), the parameter value and links to the previous and next nodes
    type boxList = BoxNode of ParName: string * Value: boxData * Prev : ref<boxList> * Tl : ref<boxList> | INilBox 
    type row = ref<boxList>                                                                                       
    // The nodes of the table-level list (rowList) have refs to the row-level lists (boxList).
    type rowList = RowNode of BoxList : ref<boxList> * Tl : ref<rowList> | INilRow                            
    type table = ref<rowList>  // Holds all the data in the table
    // Holds all the tables in the database.
    type tableList = TableNode of topList : ref<rowList> * TableName : string * Tl : ref<tableList> | INilTable 
    type database = ref<tableList>

    let rec rowListLast thisList : table =
        match !thisList with
        | INilRow -> thisList
        | RowNode (_, tail) -> rowListLast tail

    let rowListFirstRow (thisList : table) =
        match !thisList with
        | INilRow -> None
        | RowNode (row, _) -> Some row