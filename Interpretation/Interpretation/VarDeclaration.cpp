#include "interpretation.h"

namespace Interpretation { 
    pVarDeclaration VarDeclaration::parse() {
        if (currentToken().type == TT_PARENTHESIS_OPEN) {
            vd = new_VarDeclaration()->parse();
            id = vd->id;
            if (currentToken().type == TT_PARENTHESIS_CLOSE) {
                nextToken();
            }
            else {
                throw exception( (string("Exception") + to_string(__LINE__) ).c_str() );
            }
        }
        else {
            if(currentToken().type == TT_ASTERISK) {
                isPointer = true;
            }
            else if(currentToken().canBeIdentifier() ) {
                id = currentToken().strVal;
            }
            else {
                throw exception( (string("Exception") + to_string(__LINE__) ).c_str() );
            }
            if (nextToken().type == TT_BRACKET_OPEN) {
                isArray = true;
                nextToken();
                if (currentToken().type == TT_NUMBER || currentToken().type == TT_WORD) {
                    nArraySize = currentToken();
                    nextToken();
                }
                if (currentToken().type != TT_BRACE_CLOSE) {
                    throw exception( (string("Exception") + to_string(__LINE__) ).c_str() );
                }
            }
        }
            
        return pVarDeclaration(this);
    }
}