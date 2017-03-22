namespace Variable
module Printer =

    open databaseStructure

    ///Print variable values
    let rec unwrapVarsTuple outLst inLst =
        match inLst with
        | (varName,varValue) :: rest ->
            match varValue with
            | databaseStructure.String(Some(str)) -> str
            | databaseStructure.Byte(Some(byt)) -> sprintf "%i" byt
            | databaseStructure.Int(Some(integ)) -> sprintf "%i" integ
            | databaseStructure.Float(Some(floating)) -> sprintf "%f" floating
            | databaseStructure.Bool(Some(boolean)) -> sprintf "%b" boolean
            | _ -> "None"
            |> sprintf "%s\t%s" varName
            |> fun str -> unwrapVarsTuple (str :: outLst) rest
        | [] -> outLst

    ///Print out all variables
    let run (vars:Variable.contentsContainer) =
        Map.toList vars
        |> unwrapVarsTuple []
        |> List.rev