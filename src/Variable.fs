namespace Variable
module Variable =
    type content =
        | Integer of int
        | Float of float
        | Boolean of bool
        | Byte of byte
        | String of string

    type container = Map<string,content>