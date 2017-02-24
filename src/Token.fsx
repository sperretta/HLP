module Token =
    type content =
        | Name of string
        | Literal of string
        | Numeric of float //?? Need int too? Interpreting all as floats?
        | Operator of string
        | EndStatement
