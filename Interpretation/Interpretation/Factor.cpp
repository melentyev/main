#include "interpretation.h"

namespace Interpretation 
{ 
     pExprResult Factor::execute() 
     {
        if (isUnary) {
            return innerFactor->execute()->unaryPrefixOperation(this->unaryOperator);
        }
        else {
            pExprResult res = innerSingle->execute();
            for (auto op: postfixOps) {
                if (op.first == TT_PARENTHESIS_OPEN) {
                    res = res->functionCallOperator(op.second);// MEMORY LEAK 
                }
                else {
                    res = res->arrayIndexerOperator(op.second);
                }
            }
            return res;
        }
    }
    pFactor Factor::parse() {
        bool match = false;
        if (currentToken().type == TT_MINUS 
            || currentToken().type == TT_EXCL 
            || currentToken().type == TT_ASTERISK 
            || currentToken().type == TT_AMP
            || currentToken().type == TT_TILDA
            || currentToken().type == TT_DELETE) {
                isUnary = true;   
                match = true;
                unaryOperator = currentToken().type;  
                nextToken();
                innerFactor = new_Factor()->parse();  
        }
        else if (currentToken().type == TT_PARENTHESIS_OPEN) {
            tryParse([this]() {
                nextToken();
                auto tn = new_Typename()->parse();
                if (currentToken().type == TT_PARENTHESIS_CLOSE) {
                    isUnary = true;
                    this->innerTypename = tn;
                    nextToken();
                }
                else {
                    throw exception( (__FILE__ + to_string(__LINE__) ).c_str() );
                }
            },
            [this, &match](exception &e) {
                match = false;
            });
        }
        if (!match) {
            innerSingle = new_Single()->parse();
            while (currentToken().type == TT_BRACKET_OPEN || currentToken().type == TT_PARENTHESIS_OPEN) {
                postfixOps.push_back(make_pair(currentToken().type, vector<pSingleExpr>() ) );
                nextToken();
                if (currentToken().type != TT_PARENTHESIS_CLOSE) {
                    postfixOps.back().second.push_back( new_SingleExpr()->parse() );
                }
                while (currentToken().type == TT_COMMA) {
                    nextToken();
                    postfixOps.back().second.push_back( new_SingleExpr()->parse() );
                }
                if (Token::matchBrackets(postfixOps.back().first, currentToken().type ) ) {
                    nextToken();
                }
                else {
                    throw exception( (__FILE__ + to_string(__LINE__) ).c_str() );
                }
            }
        }
        return pFactor(this);
    }
}