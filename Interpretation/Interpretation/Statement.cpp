#include "interpretation.h"

namespace Interpretation 
{ 
    void Statement::execute() 
    {
        if (isSpecial) 
        {
            pExprResult res = nullptr;
            switch (specialType) 
            {
            case TT_ECHO:
                res = expr->execute();
                if (res->type == owner->basicTypes[TT_DOUBLE])
                {
                    cout << "Program output: " << res->value._double << endl;
                }
                else if (res->type == owner->basicTypes[TT_BOOL]) 
                {
                    cout << "Program output: " << (bool)(res->value._bool) << endl;
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
                res = expr->execute();
                if (res->type->isLogicalTrue || res->type->unaryPrefixOperations.count(TT_BOOL) > 0) {
                    if (res->type->isLogicalTrue && res->isLogicalTrue() 
                        || res->unaryPrefixOperation(TT_BOOL)->isLogicalTrue() ) 
                    {
                        statement1->execute();
                    }
                    else {
                        statement2->execute();
                    }
                }
                else {
                    // TODO EXCEPTION
                }
                break;
            case TT_FOR:
                owner->callStack.back()->statementsBlockStack.push_back(localStatementsBlock);
                statement1->execute();
                for (statement1->execute(); expr->execute()->isLogicalTrue(); expr2->execute() ) {
                    statement2->execute();
                }
                owner->callStack.back()->statementsBlockStack.pop_back();
                break;
            case TT_WHILE:
                owner->callStack.back()->statementsBlockStack.push_back(localStatementsBlock);
                while( expr->execute()->isLogicalTrue() ) 
                {
                    statement1->execute();
                }
                owner->callStack.back()->statementsBlockStack.pop_back();
                break;
            }
        }
        else if (tn) {
            for (auto &vdDef: this->vds) {
                parentBlock->vars[vdDef->vd->id] = (vdDef->rhs ? vdDef->rhs->execute() : nullptr);
                
            }
        }
        else if(sb) {
            owner->callStack.back()->statementsBlockStack.push_back(sb);
            for (auto &st: sb->statements) {
                st->execute();
            }
            owner->callStack.back()->statementsBlockStack.pop_back();
        }
        else {
            expr->execute();
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
        else if (currentToken().type == TT_IF) 
        {
            isSpecial = true;
            specialType = currentToken().type;
            nextToken();
            expr = new_Expr()->parse();
            statement1 = new_Statement()->parse();
            if (currentToken().type == TT_ELSE) 
            {
                nextToken();
                statement2 = new_Statement()->parse();
            }
        }
        else if (currentToken().type == TT_WHILE) 
        {
            localStatementsBlock = new_StatementsBlock();
            owner->parsingStatementsBlockStack.push_back(localStatementsBlock);
            isSpecial = true;
            specialType = currentToken().type;
            if (nextToken().type != TT_PARENTHESIS_OPEN) 
            {
                throw exception( (string(__FILE__) + ": " + to_string(__LINE__) ).c_str() );
            }
            nextToken();
            expr = new_Expr()->parse();
            if (currentToken().type != TT_PARENTHESIS_CLOSE) 
            {
                throw exception( (string(__FILE__) + ": " + to_string(__LINE__) ).c_str() );
            }
            nextToken();
            statement1 = new_Statement()->parse();
            statement1->parentBlock = localStatementsBlock;
            owner->parsingStatementsBlockStack.pop_back();
        }
        else if (currentToken().type == TT_FOR) {
            localStatementsBlock = new_StatementsBlock();
            owner->parsingStatementsBlockStack.push_back(localStatementsBlock);
            isSpecial = true;
            specialType = currentToken().type;
            if (nextToken().type != TT_PARENTHESIS_OPEN) {
                throw exception( (string(__FILE__) + ": " + to_string(__LINE__) ).c_str() );
            }
            nextToken();
            statement1 = new_Statement()->parse();
            statement1->parentBlock = localStatementsBlock;
            expr = new_Expr()->parse();
            if (currentToken().type != TT_SEMICOLON) {
                throw exception( (string(__FILE__) + ": " + to_string(__LINE__) ).c_str() );
            }
            nextToken();
            expr2 = new_Expr()->parse();
            if (currentToken().type != TT_PARENTHESIS_CLOSE) {
                throw exception( (string(__FILE__) + ": " + to_string(__LINE__) ).c_str() );
            }
            nextToken();
            statement2 = new_Statement()->parse();
            statement2->parentBlock = localStatementsBlock;
            owner->parsingStatementsBlockStack.pop_back();
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