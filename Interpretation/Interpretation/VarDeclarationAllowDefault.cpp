#include "interpretation.h"

namespace Interpretation 
{ 
    pVarDeclarationAllowDefault VarDeclarationAllowDefault::parse() {
        vd = new_VarDeclaration()->parse();
        if (currentToken().type == TT_ASSIGN) {
            nextToken();
            rhs = new_Expr()->parse();
        }
        return pVarDeclarationAllowDefault(this);
    }
}