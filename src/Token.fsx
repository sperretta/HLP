module Token =
    type content =
        | Name of string
        | Literal of string
        | Numeric of float
        | Operator of string
    type node =
        | Node of Type : content * Tail : node
        | Null
