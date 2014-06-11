module YaslmLexer

open GenParser.GpInline
open FSharpx.Prelude
open System

type LexemType = 
           | Indent
           | Dedent
           | Newline
           | LxName of name: string
           | LxNumber of value: int
           | LxLiteral of string
           | Keyword of string
           | Op of string
           | Comma
           | Semicolon
           | ParenthesisOpen
           | ParenthesisClose
           | BracketOpen
           | BracketClose
           | BraceOpen
           | BraceClose
           | End

type Lexem = {
    lxType      : LexemType;
    lxSrcLine   : int;
    lxLinePos   : int
} 

type LexerState = { 
    lsLogged      : string list;
    lsIndentStack : int list
}    

type Expr = 
    | Const of float
    | NegExpr of Expr
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr

let l2str l = new String(List.toArray l)
let str2l (s: string) = s.ToCharArray() |> Array.toList
let lxName = function LxName s -> s
                    | _        -> failwith "error"
                    
let lxNumber = function LxNumber n -> n
                      | _        -> failwith "error"
                    
let lxOp = function Op s -> s
                    | _        -> failwith "error"

let opSymbolsList = "<>=+-*/%$^|&!./|\\:".ToCharArray() |> Array.toList
let isKnownSymbol c = List.tryFind ((=) c) opSymbolsList |> Option.isSome

let space            = matchNext ( (=) ' ')
let eol              = matchNext ( (=) '\n')
let opSymbol         = matchNext isKnownSymbol

let parenthesisOpen  = konst ParenthesisOpen <@> matchNext ( (=) '(' )
let parenthesisClose = konst ParenthesisClose <@> matchNext ( (=) ')' ) 
let braceOpen        = konst Indent <@> matchNext ( (=) '{' )           /// TEMP HACK
let braceClose       = konst Dedent <@> matchNext ( (=) '}' )  
let bracketOpen      = konst BraceOpen <@> matchNext ( (=) '[' )
let bracketClose     = konst BraceClose <@> matchNext ( (=) ']' ) 
let comma            = konst Comma <@> matchNext ( (=) ',' )
let semicolon        = konst Semicolon <@> matchNext ( (=) ';' ) 
let digit            = matchNext (Char.IsDigit)
let number           = (fun l -> l2str l |>  Int32.Parse |> LxNumber ) <@> many digit  
let operator         = (Op << l2str) <@> many opSymbol
let name             = (LxName << l2str) <@> many (matchNext (fun c -> Char.IsLetterOrDigit c || c = '_'))
let lexem = manyOrNone space >> 
                (   parenthesisOpen
                <|> parenthesisClose 
                <|> braceOpen 
                <|> braceClose 
                <|> bracketOpen 
                <|> bracketClose 
                <|> semicolon 
                <|> comma
                <|> number
                <|> operator
                <|> name)

let line = tuple2 <@> (manyOrNone space) <*> (many lexem |--> eol)

let yaslmLexer input = 
    runParser (many line) input () id (fun r -> failwith "fail: %A" r)