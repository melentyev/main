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
    pLogicalOrExpr LogicalOrExpr::parse() {
        inner.push_back(new_LogicalAndExpr()->parse() );
        while (currentToken().type == TT_DOUBLE_PIPE) {
            nextToken();
            inner.push_back(new_LogicalAndExpr()->parse() );
        }
        return pLogicalOrExpr(this);
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
    pArithmetics Arithmetics::parse() 
    {
        inner.push_back(new_Term()->parse() );
        while (currentToken().type == TT_PLUS || currentToken().type == TT_MINUS) 
        {
                operators.push_back(currentToken().type);
                nextToken();
                inner.push_back(new_Term()->parse() );
        }
        return pArithmetics(this);
    }
    pTerm Term::parse() 
    {
        inner.push_back(new_Factor()->parse() );
        while (currentToken().type == TT_ASTERISK || currentToken().type == TT_SLASH) {
                operators.push_back(currentToken().type);
                nextToken();
                inner.push_back(new_Factor()->parse() );
        }
        return pTerm(this);
    }
    
    pExprResult SingleExpr::execute() {
        if (bThrow) {
            return nullptr;        // TODO: throw!!!
        }
        else {
            return this->inner->execute();
        }
    }
    pExprResult LogicalOrExpr::execute() {
        pExprResult res = (*begin(inner))->execute();
        for (auto it = (++(begin(inner) ) ); it != end(inner); it++) {
            res = res->binaryOperation(TT_DOUBLE_AMP, (*it)->execute() ); // MEMORY LEAK
        }
        return res;
    }
    pExprResult LogicalAndExpr::execute() {
        pExprResult res = (*begin(inner))->execute();
        for (auto it = (++(begin(inner) ) ); it != end(inner); it++) {
            res = res->binaryOperation(TT_DOUBLE_PIPE, (*it)->execute() ); // MEMORY LEAK
        }
        return res;
    }
    pExprResult ComparisonExpr::execute() {
        pExprResult res = (*begin(inner))->execute();
        auto op_it = begin(operators);
        for (auto it = (++(begin(inner) ) ); it != end(inner); it++, op_it++) {
            res = res->binaryOperation(*op_it, (*it)->execute() ); // MEMORY LEAK
        }
        return res;
    }
    pExprResult Arithmetics::execute() {
        pExprResult res = (*begin(inner))->execute();
        auto op_it = begin(operators);
        for (auto it = (++(begin(inner) ) ); it != end(inner); it++, op_it++) {
            res = res->binaryOperation(*op_it, (*it)->execute() ); // MEMORY LEAK
        }
        return res;
    }
    pExprResult Term::execute() {
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