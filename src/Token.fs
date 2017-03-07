namespace Tokeniser
module Token =
    type value =
        | Integer of int
        | Floating of float
        | Character of char
        | Byte of byte
        | Boolean of bool
    type content =
        | Name of string
        | Literal of string
        | Value of value
        | Operator of string
        | EndStatement
