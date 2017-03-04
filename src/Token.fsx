module Token =
    type number =
        | Integer of int
        | Floating of float
    type content =
        | Name of string
        | Literal of string
        | Numeric of number
        | Operator of string
        | EndStatement
