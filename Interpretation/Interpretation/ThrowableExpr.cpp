#include "interpretation.h"

namespace Interpretation 
{
    pExprResult ThrowableExpr::execute() 
    {
        if (this->innerLogical) {
            return innerLogical->execute();
        }
        else {
            pExprResult lhs = this->lhs->execute();
            pExprResult rhs = innerThrowable->execute();
            return lhs->binaryOperation(this->operation, rhs);
        }
    }
    pThrowableExpr ThrowableExpr::parse() {
        tryParse([this]() { 
            pFactor lhs_candidate = new_Factor()->parse(); 
            if (currentToken().type == TT_ASSIGN || currentToken().type == TT_PLUS_ASSIGN 
                || currentToken().type == TT_MINUS_ASSIGN) {
                    lhs = lhs_candidate;
                    operation = currentToken().type;
                    nextToken();
                    innerThrowable = new_ThrowableExpr()->parse();
            }
            else {
                throw exception( (string(__FILE__) + ": " + to_string(__LINE__) ).c_str() );
            }
        }, [this](exception &e) { 
            innerLogical = new_LogicalOrExpr()->parse(); 
        });
        return pThrowableExpr(this);
    }
}
