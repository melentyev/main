open GenParser.GpInline
open FSharpx.Prelude
open System

type LexemType = 
           | Indent
           | Dedent
           | Newline
           | LxName of name: char list
           | LxNumber of value: int
           | LxLiteral of char list
           | Keyword of char list
           | Op of char list
           | Comma
           | Semicolon
           | ParenthesisOpen
           | ParenthesisClose
           | BracketOpen
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

(*let addLexem t  =
    gpModify (fun st0 -> 
        let l = { lxType = t; lxSrcLine = st0.lsSrcLine; lxLinePos = st0.lsLinePos } 
        { st0 with lsResult = l :: st0.lsResult} )*)
    

type Expr = 
    | Const of float
    | NegExpr of Expr
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr

let l2str l = new String(List.toArray l)
let str2l (s: string) = s.ToCharArray() |> Array.toList

let opSymbolsList = "<>=+-*/%$^|&!./|\\:".ToCharArray() |> Array.toList
let isKnownSymbol c = List.tryFind ((=) c) opSymbolsList |> Option.isSome

let space            = matchNext ( (=) ' ')
let eol              = matchNext ( (=) '\n')
let opSymbol         = matchNext isKnownSymbol
let parenthesisOpen  = konst ParenthesisOpen <@> matchNext ( (=) '(' )
let parenthesisClose = konst ParenthesisClose <@> matchNext ( (=) ')' ) 
let comma            = konst Comma <@> matchNext ( (=) ')' )
let semicolon        = konst Semicolon <@> matchNext ( (=) ')' ) 
let digit            = matchNext (Char.IsDigit)
let number           = (fun l -> String(l |> List.toArray) |>  Int32.Parse |> LxNumber ) <@> many digit  
let operator         = Op <@> many opSymbol
let name             = LxName <@> many (matchNext (fun c -> Char.IsLetterOrDigit c || c = '_'))
let lexem = manyOrNone space >> 
                (   parenthesisOpen
                <|> parenthesisClose 
                <|> semicolon 
                <|> comma
                <|> number
                <|> operator
                <|> name
                <|> mfail)

let line = tuple2 <@> (manyOrNone space) <*> followedBy (many lexem) (eol)

let parse (inp : char list) = 
    runParser (many line) inp ()


[<EntryPoint>]
let main argv = 
    let inp = str2l "aЯd2 ( asd ))adasd\nadassd\n   dsa dsdjas jjjas jdsad\n"
    let res = parse inp
    printf "%A" res
    //let r1 = runParser (manyOrNone <| matchNext((=)'a')) ("baab".ToCharArray() |> Array.toList) ()
    //printf "%A" r1
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
