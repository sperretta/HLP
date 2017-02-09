open System.IO
open System

let myPath = @"C:\cygwin64\fsharp\project\textFile.txt"

let readFile pathString = IO.File.ReadLines pathString

let fileLines = readFile myPath
fileLines
