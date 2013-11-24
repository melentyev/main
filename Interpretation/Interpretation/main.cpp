#include "interpretation.h"
using namespace std;

namespace Interpretation 
{
    pSingleExpr SingleExpr::parse() {
        if (currentToken().type == TT_THROW) {
            bThrow = true;
        }
        inner = new_ThrowableExpr()->parse();
        return pSingleExpr(this);
    }
    
    
    pComparisonExpr ComparisonExpr::parse() 
    {
        inner.push_back(new_Arithmetics()->parse() );
        while (currentToken().type == TT_EQUAL || currentToken().type == TT_GR_EQ 
            || currentToken().type == TT_LESS_EQ || currentToken().type == TT_GR 
            || currentToken().type == TT_LESS ) 
        {
                operators.push_back(currentToken().type);
                nextToken();
                inner.push_back(new_Arithmetics()->parse() );
        }
        return pComparisonExpr(this);
    }
    
    
    
    pExprResult SingleExpr::execute() {
        if (bThrow) {
            return nullptr;        // TODO: throw!!!
        }
        else {
            return this->inner->execute();
        }
    }
    pExprResult ComparisonExpr::execute() {
        pExprResult res = (*begin(inner))->execute();
        auto op_it = begin(operators);
        for (auto it = (++(begin(inner) ) ); it != end(inner); it++, op_it++) {
            res = res->binaryOperation(*op_it, (*it)->execute() ); // MEMORY LEAK
        }
        return res;
    }
}

using namespace Interpretation;
int (main)(int (argc) ) { 
    ifstream in = ifstream("input.txt", ios_base::in);
    ::Interpretation::Parser *parser = new Interpretation::Parser();
    parser->parse(in);
    parser->basicTypes;
    parser->run();
    system("pause");
    return 0;
}