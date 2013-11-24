#include "interpretation.h"

namespace Interpretation 
{
    pExprResult ExprResult::binaryOperation(TokenType op, pExprResult rhs) 
    { 
        if (type->binaryOperations.count(make_pair(op, rhs->type) ) > 0) 
        { 
            return type->binaryOperations[make_pair(op, rhs->type) ](this, rhs); 
        }
        else 
        {
            throw exception("binaryOperation not defined");
        }
    }
    
    pExprResult ExprResult::unaryPrefixOperation(TokenType op) 
    {
        if(type->unaryPrefixOperations.count(op) > 0) 
        { 
            return type->unaryPrefixOperations[op](this); 
        }
        else 
        {
            throw exception("unaryPrefixOperation not defined");
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
    bool ExprResult::isLogicalTrue()
    {
        if (type->isLogicalTrue) {
            type->isLogicalTrue(pExprResult(this) );
        }
        else if(type->typeCasts.count(type->owner->basicTypes[TT_BOOL]) > 0) {
            return type->typeCasts[type->owner->basicTypes[TT_BOOL]](this)->value._bool;
        }
    }
    pExprResult ExprResult::typeCast(pType tn) 
    {
        if(type->typeCasts.count(tn) > 0) 
        { 
            return type->typeCasts[tn](this); 
        }
        else 
        {
            throw exception("typeCast not defined");
        }
    }
    pExprResult Parser::new_ExprResult(pType type) 
    {
        return pExprResult(new ExprResult(type) );
    }
}