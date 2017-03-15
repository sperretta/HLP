namespace databaseStructure
module databaseStructure =

    open System.IO
    open System
    type myData = | String of Option<string> | Int of Option<int> | Float of Option<float>                // To be extended when the wanted data types have been decided
                  | Byte of Option<byte>     | Bool of Option<bool>
    type IList = INode of ParName: string * Value: myData * Prev : ref<IList> * Tl : ref<IList> | INilLow // A node has the parameter name (column name), the parameter value
                                                                                                          // and links to the previous and next nodes
    type topList = Node of lowList : ref<IList> * Tl : ref<topList> | INilTop                             // The nodes of the top-level list have refs to low-level lists.
    type data = ref<topList>                                                                              // Holds all the data in the table
    type TableList = TableNode of topList : ref<topList> * TableName : string * Tl : ref<TableList> | INilTable // Holds all the tables in the database.

    let rec topListLast thisList : data =
        match !thisList with
        | INilTop -> thisList
        | Node (_, tail) -> topListLast tail

    let topListFirstRow (thisList : data) =
        match !thisList with
        | INilTop -> None
        | Node (row, _) -> Some row