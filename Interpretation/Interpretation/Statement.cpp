#include "interpretation.h"

namespace Interpretation { 
    void Statement::execute() {
        if (isSpecial) {
            pExprResult res = nullptr;
            switch (specialType) {
            case TT_ECHO:
                res = expr->execute();
                if (res->type == owner->basicTypes[TT_DOUBLE] )
                {
                    cout << "Program output: " << res->value._double << endl;
                }
                else 
                {
                    cout << "Program output: " << res->value._int << endl;
                }
                
                break;
            case TT_RETURN:
                owner->callStack.back()->returnValue = expr->execute();
                break;
            case TT_IF:
                break;
            }
        }
        else if (tn) {
            for (auto &vdDef: this->vds) {
                parentBlock->vars[vdDef->vd->id] = (vdDef->rhs ? vdDef->rhs->execute() : nullptr);
                
            }
        }
    }
    pStatement Statement::parse() {
        if (currentToken().type == TT_BRACE_OPEN) {
            this->sb = new_StatementsBlock()->parse();
        }
        else if (currentToken().type == TT_BREAK 
            || currentToken().type == TT_RETURN 
            || currentToken().type == TT_ECHO 
            || currentToken().type == TT_CONTINUE) {
                isSpecial = true;
                specialType = currentToken().type;
                nextToken();
                if (specialType == TT_RETURN || specialType == TT_ECHO) {
                    expr = new_Expr()->parse();
                }
                if (currentToken().type != TT_SEMICOLON) {
                    throwTokenExpected(TT_SEMICOLON);
                }
                nextToken();
        }
        else if (currentToken().type == TT_IF) {
            
        }
        else if (currentToken().type == TT_WHILE) {

        }
        else if (currentToken().type == TT_FOR) {

        }
        else {
            tryParse([this]() {
                tn = new_Typename()->parse();
                while (currentToken().type != TT_SEMICOLON) {
                    vds.push_back(new_VarDeclarationAllowDefault()->parse() );
                }
            },
            [this](exception &e) {
                vds.clear();
                tn = nullptr;
                expr = new_Expr()->parse();
                if (currentToken().type != TT_SEMICOLON) {
                    throw exception(to_string(__LINE__).c_str() );
                }
            });
            nextToken();
        }
        return pStatement(this);
    }
}