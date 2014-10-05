module CYaslmParser

open GenParser.GpInline
open YaslmLexer
open FSharpx.Prelude
open AST
   
let makeFactor s a = Factor (s, a)
let makePower a b c = Power (a, b, c)

let curry3 f a b c = f (a,b,c)   
    
type YParser() = 
    member x.name = (Name << lxName) <@> matchNext (fun lx -> 
            match lx with LxName s when (Option.isNone << List.tryFind ((=) s)) keywords -> true
                          | _ -> false)
    member x.constInt = (ConstInt << lxNumber) <@> matchNext (fun lx -> match lx with LxNumber _ -> true | _ -> false)
    member x.keyword expected = matchNext (fun lx -> match lx with LxName s when s = expected   -> true 
                                                                  | _ -> false)
                                                              
    member x.operator op = matchNext ( (=^=) (Op op))
    member x.compOp = (konst Eq <@> (x.operator "==")) 
                      <|> (konst NotEq <@> (x.operator "/="))
                      <|> (konst Gr <@> (x.operator ">")) 
                      <|> (konst GrEq <@> (x.operator ">="))
                      <|> (konst Less <@> (x.operator "<"))
                      <|> (konst LessEq <@> (x.operator "<="))
    member x.shiftOp = (lxOp <@> (x.operator ">>>") ) 
                        <|> (lxOp <@> (x.operator "<<<") ) 
    member x.termDelim = (lxOp <@> (x.operator "+") ) 
                        <|> (lxOp <@> (x.operator "-") ) 
    member x.factorDelim = (lxOp <@> (x.operator "*") ) 
                         <|> (lxOp <@> (x.operator "/") ) 
                         <|> (lxOp <@> (x.operator "%") ) 
    member x.factorPrefixOp = (lxOp <@> (x.operator "+") ) 
                            <|> (lxOp <@> (x.operator "-") ) 
                            <|> (lxOp <@> (x.operator "~") ) 

    member x.semicolon = matchNext ((=^=) Semicolon)
    member x.comma = matchNext ((=^=) Comma)
    member x.arrowLeft = matchNext ((=^=) (Op "<-"))
    member x.arrowRight = matchNext ((=^=) (Op "->"))
    member x.backslash = matchNext ((=^=) (Op  "\\"))
    member x.newline = matchNext ((=^=) Newline) 
    member x.indent = matchNext ((=^=) Indent) 
    member x.dedent = matchNext ((=^=) Dedent)
    member x.parenthesisOpen = matchNext ((=^=) ParenthesisOpen)
    member x.parenthesisClose = matchNext ( (=^=) ParenthesisClose)

    member x.binaryOp expr op ctor = 
        (fun l -> if List.length l = 1 then List.head l else ctor l) <@> ( separatedBy expr op)
  
    member x.samePriorBinaryOp (expr: GenParser<LexemType, 's, AST>) (opParser: GenParser<LexemType, 's, 'a>) ctor = genp {
        let! hd = expr
        let! tail = manyOrNone (genp { 
                                    let! op = opParser
                                    let! e = expr
                                    return (op, e) })
        return List.fold (fun acc (op, e) -> ctor (acc, op, e) ) hd tail
    }

    member x.atom =  //wrappedBy testlist (parenthesisOpen, parenthesisClose) 
                x.name 
                <|> x.constInt 

    member x.callarg = Callarg <@> x.atom
    member x.trailer = (Scope <@> (x.operator "." >> x.name) ) <|> x.callarg

    member x.factor = (makeFactor <@> x.factorPrefixOp <*> x.factor) <|> x.power
    member x.power = makePower <@> x.atom <*> manyOrNone x.trailer <*> oneOrNone (x.operator "**" >> x.factor)

    member x.term        = x.samePriorBinaryOp (x.factor) x.factorDelim Term 
    member x.shiftExpr   = x.samePriorBinaryOp x.arithExpr x.shiftOp ShiftExpr
    member x.arithExpr   = x.samePriorBinaryOp x.term x.termDelim ArithExpr
    
    member x.andExpr     = x.binaryOp x.shiftExpr (x.operator "&") AndExpr

    member x.xorExpr        = x.binaryOp x.andExpr (x.operator "^") XorExpr
    member x.btwOrExpr      = x.binaryOp x.xorExpr (x.operator "|") BtwOrExpr
    member x.comparison     = x.samePriorBinaryOp x.btwOrExpr x.compOp Comparison
    member x.notTest = (NotTest <@> (x.keyword "not" >> x.notTest ) ) <|> x.comparison
    member x.andTest        = x.binaryOp (x.notTest) (x.keyword "and") AndTest
    member x.orTest         = x.binaryOp x.andTest (x.keyword "or") OrTest

    member x.arg = (Arg <@> ( separatedBy x.name x.comma) ) <|> ((x.arg) /--/ (x.parenthesisOpen, x.parenthesisClose))
    member x.varargslist = Varargslist <@> many (x.arg)

    member x.lambdef = (curry Lambdef) <@> (x.backslash >> (followedBy x.varargslist x.arrowRight) ) <*> (x.suite)

    member x.ifExpr = genp {
        do! (ignore <@> x.keyword "if")
        let! cond = x.test
        do! (ignore <@> x.keyword "then")
        let! th = x.exprStmt <|> x.suite 
        do! (ignore <@> x.keyword "else")
        let! el = oneOrNone (x.exprStmt <|> x.suite)
        return IfExpr(cond, th, el)
    }

    member x.test = (x.lambdef <|> x.ifExpr <|> x.orTest (*cons*))
    //let cons = (fun l r -> maybe l (Cons l) r ) <$> orTest <*> oneOrNone (operator "::" >> cons) 

    member x.testlist = Testlist <@> (separatedBy x.test x.comma)

    member x.assignStmt = (curry AssignStmt) <@> x.testlist  <*> (x.arrowLeft >> x.assignStmt)
    member x.exprStmt = x.assignStmt <|> x.testlist

    member x.compoundExprStmt = CompoundExprStmt 
                                <@> x.exprStmt -|- x.semicolon 
                                |--> oneOrNone x.semicolon 
                                |--> x.newline
    member x.whileStmt = x.keyword "while" >> x.test >> x.keyword "do" >> x.suite
    member x.forStmt = x.keyword "for" >> x.testlist >> x.keyword "in" >> x.testlist >> x.keyword "do" >> x.suite

    member x.compoundExpr = (x.newline >> x.compoundExpr) <|> x.whileStmt <|> x.forStmt <|> x.compoundExprStmt



    member x.indentedSuite = curry IndentedSuite <@> (x.newline >> x.indent >> manyOrNone x.binding)
                                  <*> (many (x.compoundExpr) |--> x.dedent)
    member x.suite = x.indentedSuite <|> x.compoundExpr
    member x.funcBinding = curry3 FuncBinding <@> x.name <*> (x.varargslist |--> x.operator "=") <*> x.suite
    member x.tupleBinding = curry TupleBinding <@> (x.name -|- x.comma) <*> (x.operator "=" >> x.suite)
    member x.binding = genp {
        return! (x.newline >> x.binding) <|> x.tupleBinding <|> x.funcBinding
        }

    member x.pModule = genp {
        return! Module <@> manyOrNone (x.binding)
        }
    member x.Parse (input: LexemType list) = 
                runParser x.pModule input () id (fun r -> failwithf "fail: %A" r)