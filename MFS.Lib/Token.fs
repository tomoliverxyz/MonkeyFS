module MFS.Lib.Token

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

type Token =
    { TokenType: TokenType
      Literal: string }

let newToken (tokenType: TokenType, literal: string): Token =
    { TokenType = tokenType
      Literal = literal }

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
