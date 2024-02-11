open System

open MFS.Lib.Lexer

printfn "Enter some MonkeyFS code:"

let input = Console.ReadLine()
let tokens = newLexer input |> tokenizeCode

tokens |> Seq.iter (printfn "%A")