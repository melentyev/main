#include "interpretation.h"

namespace Interpretation {
    pNamespace Namespace::parse() {
        if (currentToken().type == TT_NAMESPACE) {
            if (nextToken().canBeIdentifier() ) {
                id = currentToken().strVal;
                if(nextToken().type != TT_BRACE_OPEN) {
                    throw exception(to_string(__LINE__).c_str() );
                }
                while (currentToken().type != TT_BRACE_CLOSE) {
                    declarations.push_back(new_DeclarativeStatement()->parse() );
                }
            }
            else {
                throw exception(to_string(__LINE__).c_str() );
            }
        }
        return pNamespace(this);
    }
}