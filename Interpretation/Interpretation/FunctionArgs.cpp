#include "interpretation.h"

namespace Interpretation 
{ 
    pFunctionArgs FunctionArgs::parse() 
    {
        while (currentToken().type != TT_PARENTHESIS_CLOSE) {
            typenames.push_back(new_Typename()->parse() );
            vds.push_back(new_VarDeclarationAllowDefault()->parse() );
            if (currentToken().type == TT_COMMA) 
            {
                nextToken();
            }
        }
        return pFunctionArgs(this);
    }
}