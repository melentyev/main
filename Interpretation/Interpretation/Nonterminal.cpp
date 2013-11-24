#include "interpretation.h"

#define DECL_NEW(pC, new_C, C) pC Nonterminal::new_C() { return pC(new C(this->owner) ); }

namespace Interpretation {
    Token & Nonterminal::currentToken() {
        return owner->currentToken(); 
    }
    Token & Nonterminal::nextToken() {
        owner->tokenNumber++;
        return owner->currentToken(); 
    }
    void Nonterminal::tryParse(std::function<void(void)> fnTry, std::function < void(std::exception&) > fnCatch) {
        int buffer = owner->tokenNumber;
        try {
            fnTry();
        }
        catch (std::exception &e) {
            owner->tokenNumber = buffer;
            fnCatch(e);
        }
    }
    void Nonterminal::throwTokenExpected(TokenType tok) {
        throw exception( (string("Token expected") + to_string(tok) ).c_str() );
    }
    pNamespace Nonterminal::new_Namespace() {
        return pNamespace(new Namespace(this->owner, ""));
    }
    DECL_NEW(pDeclarativeStatement, new_DeclarativeStatement, DeclarativeStatement)
    DECL_NEW(pTypename, new_Typename, Typename)
    DECL_NEW(pFunctionArgs, new_FunctionArgs, FunctionArgs)
    DECL_NEW(pExpr, new_Expr, Expr)
    DECL_NEW(pVarDeclaration, new_VarDeclaration, VarDeclaration)
    DECL_NEW(pStatementsBlock, new_StatementsBlock, StatementsBlock)
    DECL_NEW(pVarDeclarationAllowDefault, new_VarDeclarationAllowDefault, VarDeclarationAllowDefault)
    DECL_NEW(pStatement, new_Statement, Statement)
    DECL_NEW(pSingleExpr, new_SingleExpr, SingleExpr)
    DECL_NEW(pThrowableExpr, new_ThrowableExpr, ThrowableExpr)
    DECL_NEW(pLogicalOrExpr, new_LogicalOrExpr, LogicalOrExpr)
    DECL_NEW(pLogicalAndExpr, new_LogicalAndExpr, LogicalAndExpr)
    DECL_NEW(pComparisonExpr, new_ComparisonExpr, ComparisonExpr)
    DECL_NEW(pArithmetics, new_Arithmetics, Arithmetics)
    DECL_NEW(pTerm, new_Term, Term)
    DECL_NEW(pFactor, new_Factor, Factor)
    DECL_NEW(pSingle, new_Single, Single)
    
    pComplId Nonterminal::new_ComplId(const std::string &s) {
        pComplId res = pComplId(new ComplId(owner, s, owner->parsingStatementsBlockStack.back() ) );
        return res;
    }
}