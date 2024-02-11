module MFS.Lib.Token

// token data type
type TokenType =
    | Ident of string
    | Int of int
    | Illegal
    | Eof
    | Assign
    | Plus
    | Minus
    | Bang
    | Asterisk
    | Slash
    | Comma
    | SemiColon
    | Lparen
    | Rparen
    | Lsquirly
    | Rsquirly
    | Function
    | Let
    | LessThan
    | LessThanOrEqualTo
    | GreaterThan
    | GreaterThanOrEqualTo
    | EqualTo
    | NotEqualTo
    | True
    | False
    | If
    | Else
    | Return

// token literal data type
type Token =
    { TokenType: TokenType
      Literal: string }

// function to create a new token
let newToken (tokenType: TokenType, literal: string): Token =
    { TokenType = tokenType
      Literal = literal }

// function to lookup an identifier
let lookupIdent (keyword: string): TokenType =
    match keyword with
    | "fn" -> Function
    | "let" -> Let
    | "true" -> True
    | "false" -> False
    | "if" -> If
    | "else" -> Else
    | "return" -> Return
    | _ -> Ident keyword
