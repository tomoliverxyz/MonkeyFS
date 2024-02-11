module MFS.Lib.Lexer

open System
open Token

// lexer data type
type Lexer =
    { Input: string
      Position: int
      ReadPosition: int
      Ch: char }

// function to get next character in input
let peekNextChar (lexer: Lexer): char =
    if lexer.ReadPosition >= lexer.Input.Length
    then Char.MinValue
    else lexer.Input[lexer.ReadPosition]

// function to move the lexer's current position forward on given input
let readNextChar (lexer: Lexer): Lexer =
    { lexer with
        Position = lexer.ReadPosition
        ReadPosition = lexer.ReadPosition + 1
        Ch = peekNextChar lexer }

// function to create a new lexer
let newLexer (input: string): Lexer =
    { Input = input
      Position = 0
      ReadPosition = 0
      Ch = Char.MinValue } |> readNextChar

// function to check if current character is a char
let isChar (ch: char): bool =
    ch >= 'a' && ch <= 'z' || ch >= 'A' && ch <= 'Z' || ch = '_'

// function to check if current character is a number
let isNumber (ch: char): bool =
    ch >= '0' && ch <= '9'

// function to skip over whitespace for current character
let rec skipWhitespace (lexer: Lexer): Lexer =
    if lexer.Ch = ' ' || lexer.Ch = '\t' || lexer.Ch = '\n' || lexer.Ch = '\r'
    then readNextChar lexer |> skipWhitespace
    else lexer

// function to keep reading input while the current character matches the current checker (isChar or isNumber)
let readNextWhile (lexer: Lexer, checker: char -> bool): Lexer*string  =
    let start = lexer.Position

    let rec read (lexer: Lexer): Lexer =
        if checker lexer.Ch
        then readNextChar lexer |> read
        else lexer

    let lexer = read lexer
    lexer, lexer.Input.Substring(start, lexer.Position - start)

// function to read create a token based on the current character
let readToken (lexer: Lexer): Lexer*Token =
    let lexer = skipWhitespace lexer

    let advance (tokenType: TokenType): Lexer*Token =
        readNextChar lexer, newToken (tokenType, lexer.Ch.ToString())

    let advancePeeked (tokenType: TokenType, peekChar: char) =
        readNextChar lexer |> readNextChar, newToken (tokenType, lexer.Ch.ToString() + peekChar.ToString())

    match lexer.Ch with
    | '=' ->
        let peekedChar = peekNextChar lexer
        in if peekedChar = '=' then advancePeeked (EqualTo, peekedChar) else advance Assign
    | '!' ->
        let peekedChar = peekNextChar lexer
        in if peekedChar = '=' then advancePeeked (NotEqualTo, peekedChar) else advance Bang
    | '<' ->
        let peekedChar = peekNextChar lexer
        in if peekedChar = '=' then advancePeeked (LessThanOrEqualTo, peekedChar) else advance LessThan
    | '>' ->
        let peekedChar = peekNextChar lexer
        in if peekedChar = '=' then advancePeeked (GreaterThanOrEqualTo, peekedChar) else advance GreaterThan
    | '+' -> advance Plus
    | '-' -> advance Minus
    | '*' -> advance Asterisk
    | '/' -> advance Slash
    | ';' -> advance SemiColon
    | ',' -> advance Comma
    | '(' -> advance Lparen
    | ')' -> advance Rparen
    | '{' -> advance Lsquirly
    | '}' -> advance Rsquirly
    | Char.MinValue -> advance Eof
    | char when isChar char ->
        let (lexer, substring) = readNextWhile (lexer, isChar)
        in (lexer, newToken (lookupIdent substring, substring))
    | char when isNumber char ->
        let (lexer, substring) = readNextWhile (lexer, isNumber)
        in (lexer, newToken (Int (substring |> int), substring))
    | _ -> advance Illegal

// function to generate code tokens
let rec tokenizeCode (lexer: Lexer): Token seq =
    seq {
        match readToken lexer with
        | _, token when token.TokenType = Eof -> yield newToken (Eof, Char.MinValue.ToString())
        | lexer, token ->
            yield token
            yield! tokenizeCode lexer
    }
