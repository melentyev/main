#include "interpretation.h"

namespace Interpretation {  
    pTypename Typename::parse() {
        switch (currentToken().type) {
        case TT_INT:
            simple = true, simpleType = TT_INT;
            break;
        case TT_CHAR:
            simple = true, simpleType = TT_CHAR;
            break;
        case TT_FLOAT:
            simple = true, simpleType = TT_FLOAT;
            break;
        case TT_DOUBLE:
            simple = true, simpleType = TT_FLOAT;
            break;
        case TT_UNSIGNED:
            simple = true;
            switch (nextToken().type) {
            case TT_INT:
                simpleType = TT_INT;
                break;
            case TT_CHAR:
                simpleType = TT_CHAR;
                break;
            default:
                throw exception ("Typename: undefined type");
                break;
            }
            break;
        default:
            throw exception ("Typename: undefined type");
            break;
        }
        nextToken();
        return pTypename(this);
    }
}