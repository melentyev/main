#include "interpretation.h"

namespace Interpretation 
{
    pExprResult LogicalAndExpr::execute() 
    {
        pExprResult res = (*begin(inner))->execute();
        for (auto it = (++(begin(inner) ) ); it != end(inner); it++) {
            if (! res->isLogicalTrue() ) {
                return res;
            }
            pExprResult rhs = (*it)->execute();// MEMORY LEAK
            if (res->type->binaryOperations.count(make_pair(TT_DOUBLE_AMP, rhs->type) ) > 0) {
                res = res->binaryOperation(TT_DOUBLE_AMP, (*it)->execute() ); 
            }
            else {
                res = owner->basicTypes[TT_BOOL]->valueOf
                    ( ( res->isLogicalTrue() && rhs->isLogicalTrue() ) ? "true" : "false"); 
            }
        }
        return res;
    }
    pLogicalAndExpr LogicalAndExpr::parse() 
    {
        inner.push_back(new_ComparisonExpr()->parse() );
        while (currentToken().type == TT_DOUBLE_AMP) {
            nextToken();
            inner.push_back(new_ComparisonExpr()->parse() );
        }
        return pLogicalAndExpr(this);
    }
}