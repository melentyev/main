#include "interpretation.h"

namespace Interpretation {  
    pDeclarativeStatement DeclarativeStatement::parse() {
        Token &tok = owner->currentToken();
        if (tok.type == TT_NAMESPACE) {
            _namespace = new_Namespace()->parse();       
        }
        else if(tok.type == TT_TYPEDEF) {
                
        }
        else {
            pTypename tn = new_Typename()->parse();
            pVarDeclarationAllowDefault vd = new_VarDeclarationAllowDefault();
            vd->parse();
            if (owner->currentToken().type == TT_PARENTHESIS_OPEN) {
                if (!vd->canBeFunctionDeclaration() ) {
                    throw exception( (string("Exception") + to_string(__LINE__) ).c_str() );
                }
                nextToken();
                function = pFunction(new Function(owner, vd->vd->id) );
                function->args = new_FunctionArgs()->parse();
                
                owner->currentToken();
                if (owner->currentToken().type == TT_PARENTHESIS_CLOSE) {
                    if (owner->nextToken().type == ';') {
                        function->statementsBlock = nullptr;    
                    }
                    else {
                        function->statementsBlock = new_StatementsBlock()->parse();
                        for(auto vd: function->args->vds) {
                            function->statementsBlock->vars[vd->vd->id] = nullptr;
                        }
                    }
                }
                else {
                    throw exception( (string("Unexpeted token") + to_string(__LINE__) ).c_str() );
                }
            }
            else if(currentToken().type == TT_COMMA) 
            {
                    
            }
            else if (currentToken().type == TT_SEMICOLON)
            {
                vds.push_back(vd);
                nextToken();
            }
            else 
            {
                throw exception( (string("Unexpeted token") + to_string(__LINE__) ).c_str() );
            }
        }
        return pDeclarativeStatement(this);
    }
}