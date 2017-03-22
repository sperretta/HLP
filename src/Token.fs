namespace Tokeniser
module Token =
    ///Value type and contents
    type value =
        | Integer of int
        | Floating of float
        | Byte of byte
        | Boolean of bool
    ///Token type
    type content =
        | Name of string
        | Literal of string
        | Value of value
        | Operator of string
        | EndStatement
