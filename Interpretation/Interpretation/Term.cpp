#include "interpretation.h"

namespace Interpretation 
{
    pExprResult Term::execute() {
        pExprResult res = (*begin(inner))->execute();
        auto op_it = begin(operators);
        for (auto it = (++(begin(inner) ) ); it != end(inner); it++, op_it++) {
            res = res->binaryOperation(*op_it, (*it)->execute() ); // MEMORY LEAK
        }
        return res;
    }
    pTerm Term::parse() 
    {
        inner.push_back(new_Factor()->parse() );
        while (currentToken().type == TT_ASTERISK || currentToken().type == TT_SLASH 
            || currentToken().type == TT_PERCENT ) 
        {
                operators.push_back(currentToken().type);
                nextToken();
                inner.push_back(new_Factor()->parse() );
        }
        return pTerm(this);
    }
}