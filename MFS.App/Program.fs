open System

open MFS.Lib.Lexer

printfn "Enter some MonkeyFS code:"

// read code from user
let input = Console.ReadLine()
// tokenize the code
let tokens = newLexer input |> tokenizeCode

// print each token
tokens |> Seq.iter (printfn "%A")