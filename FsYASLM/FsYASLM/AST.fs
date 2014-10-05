module AST

let keywords = ["if"; "then"; "else"; "while"; "do"; "for"; "in"]

type CompOp = Eq | NotEq | LessEq | GrEq | Less | Gr 

type AST = 
    Module of AST list
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