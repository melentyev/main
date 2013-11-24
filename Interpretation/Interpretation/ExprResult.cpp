#include "interpretation.h"

namespace Interpretation 
{
    pExprResult ExprResult::binaryOperation(TokenType op, pExprResult rhs) 
    { 
        if(type->binaryOperations.count(make_pair(op, rhs->type) ) > 0) 
        { 
            return type->binaryOperations[make_pair(op, rhs->type) ](this, rhs); 
        }
        else 
        {
            throw exception("binaryOperation not defined");
        }
    }
    pExprResult ExprResult::functionCallOperator(const vector<pSingleExpr> &args) 
    {
        return type->functionCallOperator(pExprResult(this), args);
    }
    pExprResult ExprResult::arrayIndexerOperator(const vector<pSingleExpr> &args) 
    {
        return type->arrayIndexerOperator(pExprResult(this), args);
    }
    pExprResult Parser::new_ExprResult(pType type) 
    {
        return pExprResult(new ExprResult(type) );
    }
}