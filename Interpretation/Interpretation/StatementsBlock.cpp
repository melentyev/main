#include "interpretation.h"

namespace Interpretation { 
    pStatementsBlock StatementsBlock::parse() {
        if (currentToken().type != TT_BRACE_OPEN) {
            throw exception(to_string(__LINE__).c_str() );
        }
        owner->parsingStatementsBlockStack.push_back(pStatementsBlock(this) );
        nextToken();
        while (currentToken().type != TT_BRACE_CLOSE) {
            statements.push_back(new_Statement()->parse() );
            statements.back()->parentBlock = pStatementsBlock(this);
        }
        nextToken();
        owner->parsingStatementsBlockStack.pop_back();
        return pStatementsBlock(this);
    }
}