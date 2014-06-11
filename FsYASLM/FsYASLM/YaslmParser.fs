module YaslmParser

open GenParser.GpInline
open YaslmLexer
open FSharpx.Prelude

type CompOp = Eq | NotEq | LessEq | GrEq | Less | Gr 


let keywords = ["if"; "then"; "else"; "while"; "do"; "for"; "in"]

type AST = Module of AST list
           | TupleBinding of (AST list) * AST         // NAME (',' NAME)* '=' suite
           | FuncBinding of AST * AST * AST        // NAME varargslist '=' suite
           | Varargslist of AST list
           | Arg of AST list
           | Suite of AST list
           | IndentedSuite of AST list * AST list
           | CompoundExprStmt of AST list
           | AssignStmt of AST * AST
           | IfExpr of AST * AST * (AST option)
           | Test of AST
           | Testlist of AST list
           | Cons of AST * AST
           | OrTest of AST list
           | AndTest of AST list
           | NotTest of AST
           | Comparison of AST * CompOp * AST
           | BtwOrExpr  of AST list
           | XorExpr of AST list
           | AndExpr of AST list
           | ShiftExpr of AST * string * AST
           | ArithExpr of AST * string * AST
           | Term of AST * string * AST
           | Factor of string * AST
           | Power of AST * (AST list) * (AST option)
           | Lambdef of AST * AST                // '\' varargslist '->' suite
           | Callarg of AST
           | Name of string
           | Scope of AST
           | ConstInt of int
           | ConstDouble of float
           | ConstString of string

let makeFactor s a = Factor (s, a)
let makePower a b c = Power (a, b, c)

let (=^=) l1 = function
    | ParenthesisOpen -> match l1 with ParenthesisOpen -> true | _ -> false
    | ParenthesisClose -> match l1 with ParenthesisClose -> true | _ -> false
    | Op x -> match l1 with Op y when x = y -> true | _ -> false
    | LxName x -> match l1 with LxName y when x = y -> true | _ -> false
    | _ -> false

let curry3 f a b c = f (a,b,c)

let test       = ref null
let suite      = ref null
let binding    = ref null
let exprStmt   = ref null
let factor     = ref null
let power      = ref null
let notTest    = ref null
let assignStmt = ref null
let compoundExpr = ref null
let arg        = ref null

//let string = (ConstString << lxName) <@> matchNext (fun lx -> match lx with LxName _ -> true | _ -> false)
let name = (Name << lxName) <@> matchNext (fun lx -> 
        match lx with LxName s when (Option.isNone << List.tryFind ((=) s)) keywords -> true
                      | _ -> false)
let constInt = (ConstInt << lxNumber) <@> matchNext (fun lx -> match lx with LxNumber _ -> true | _ -> false)
let keyword expected = matchNext (fun lx -> match lx with LxName s when s = expected   -> true 
                                                              | _ -> false)
                                                              
let operator op = matchNext ( (=^=) (Op op))

let compOp = (konst Eq <@> (operator "==")) 
          <|> (konst NotEq <@> (operator "/="))
          <|> (konst Gr <@> (operator ">")) 
          <|> (konst GrEq <@> (operator ">="))
          <|> (konst Less <@> (operator "<"))
          <|> (konst LessEq <@> (operator "<="))
let shiftOp = (lxOp <@> (operator ">>>") ) 
          <|> (lxOp <@> (operator "<<<") ) 
let termDelim = (lxOp <@> (operator "+") ) 
            <|> (lxOp <@> (operator "-") ) 
let factorDelim = (lxOp <@> (operator "*") ) 
              <|> (lxOp <@> (operator "/") ) 
              <|> (lxOp <@> (operator "%") ) 
let factorPrefixOp = (lxOp <@> (operator "+") ) 
                 <|> (lxOp <@> (operator "-") ) 
                 <|> (lxOp <@> (operator "~") ) 

let semicolon = matchNext ((=^=) Semicolon)
let comma = matchNext ((=^=) Comma)
let arrowLeft = matchNext ((=^=) (Op "<-"))
let arrowRight = matchNext ((=^=) (Op "->"))
let backslash = matchNext ((=^=) (Op  "\\"))
let newline = matchNext ((=^=) Newline) 
let indent = matchNext ((=^=) Indent) 
let dedent = matchNext ((=^=) Dedent)
let parenthesisOpen = matchNext ((=^=) ParenthesisOpen)
let parenthesisClose = matchNext ( (=^=) ParenthesisClose)

let binaryOp expr op ctor = 
    (fun l -> if List.length l = 1 then List.head l else ctor l) <@> ( separatedBy expr op)
  
let samePriorBinaryOp (expr: GenParser<LexemType, 's, AST>) (opParser: GenParser<LexemType, 's, 'a>) ctor = genp {
    let! hd = expr
    let! tail = manyOrNone (genp { 
                                let! op = opParser
                                let! e = expr
                                return (op, e) })
    return List.fold (fun acc (op, e) -> ctor (acc, op, e) ) hd tail
}

let atom =  //wrappedBy testlist (parenthesisOpen, parenthesisClose) 
            name 
        <|> constInt 

let callarg = Callarg <@> atom
let trailer = (Scope <@> (operator "." >> name) ) <|> callarg

factor := (makeFactor <@> factorPrefixOp <*> !factor) <|> future power
power := makePower <@> atom <*> manyOrNone trailer <*> oneOrNone (operator "**" >> future factor)

let term        = samePriorBinaryOp (!factor) factorDelim Term 
let arithExpr   = samePriorBinaryOp term termDelim ArithExpr
let shiftExpr   = samePriorBinaryOp arithExpr shiftOp ShiftExpr
let andExpr     = binaryOp shiftExpr (operator "&") AndExpr

let xorExpr        = binaryOp andExpr (operator "^") XorExpr
let btwOrExpr      = binaryOp xorExpr (operator "|") BtwOrExpr
let comparison     = samePriorBinaryOp btwOrExpr compOp Comparison
notTest := (NotTest <@> (keyword "not" >> !notTest ) ) <|> comparison
let andTest        = binaryOp (!notTest) (keyword "and") AndTest
let orTest         = binaryOp andTest (keyword "or") OrTest

arg := (Arg <@> ( separatedBy name comma) ) <|> ((future arg) /--/ (parenthesisOpen, parenthesisClose))
let varargslist = Varargslist <@> many (!arg)

let lambdef = (curry Lambdef) <@> (backslash >> (followedBy varargslist arrowRight) ) <*> (future suite)

let ifExpr = genp {
    do! (ignore <@> keyword "if")
    let! cond = future test
    do! (ignore <@> keyword "then")
    let! th = future exprStmt <|> !suite 
    do! (ignore <@> keyword "else")
    let! el = oneOrNone (future exprStmt <|> future suite)
    return IfExpr(cond, th, el)
}

test := (lambdef <|> ifExpr <|> orTest (*cons*))
//let cons = (fun l r -> maybe l (Cons l) r ) <$> orTest <*> oneOrNone (operator "::" >> cons) 

let testlist = Testlist <@> (separatedBy !test comma)

assignStmt := (curry AssignStmt) <@> testlist  <*> (arrowLeft >> future assignStmt)
exprStmt := !assignStmt <|> testlist

let compoundExprStmt =  CompoundExprStmt 
                        <@> !exprStmt -|- semicolon 
                        |--> oneOrNone semicolon 
                        |--> newline
let whileStmt = keyword "while" >> future test >> keyword "do" >> !suite
let forStmt = keyword "for" >> testlist >> keyword "in" >> testlist >> keyword "do" >> !suite

compoundExpr := (newline >> !compoundExpr) <|> whileStmt <|> forStmt <|> compoundExprStmt



let indentedSuite = curry IndentedSuite <@> (newline >> indent >> manyOrNone !binding)
                              <*> (many (!compoundExpr) |--> dedent)
suite := indentedSuite <|> future compoundExpr
let funcBinding = curry3 FuncBinding <@> name <*> (varargslist |--> operator "=") <*> future suite
let tupleBinding = curry TupleBinding <@> (name -|- comma) <*> (operator "=" >> future suite)
binding := genp {
    return! (newline >> future binding) <|> tupleBinding <|> funcBinding
    }

let pModule = genp {
    return! Module <@> manyOrNone (!binding)
    }


(*
let tst1 = ref null

tst1 := curry Factor <@> (lxOp <@> operator "-") <*> (name <|> future tst1) //(gpGetState >>= (fun _ -> name <|> !tst1))

let yaslmParser input = 
    //runParser pModule input () id (fun r -> failwithf "fail: %A" r)
    runParser (!tst1) [Op "-"; Op "-"; LxName "aaa"] () id (fun r -> failwithf "fail: %A" r)

    *)
let yaslmParser input = 
    runParser pModule input () id (fun r -> failwithf "fail: %A" r)