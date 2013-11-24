#include "interpretation.h"

namespace Interpretation {  
    Parser::Parser() {
        globalNamespace = pNamespace(new Namespace(this, "") ); 
        globalNamespace->statementsBlock = globalNamespace->new_StatementsBlock();
        pType functionType = basicTypes[TT_FUNCTION] = new Interpretation::Type();
        functionType->functionCallOperator = 
            [functionType, this](pExprResult funcPtr, const vector<pSingleExpr>& args) 
        {
            pFunction func = ((Function*)funcPtr->value.pointer);
            return func->execute(args);
        };
        pType intType = basicTypes[TT_INT] = new Interpretation::Type
            ([this](const string &s) 
        {
            pExprResult res = new_ExprResult();
            res->value._int = stoi(s);
            return res;
        });
        intType->binaryOperations[make_pair(TT_PLUS, intType)] = 
            [intType, this](pExprResult lhs, pExprResult rhs) 
        {
            pExprResult res = new_ExprResult(intType);
            res->value._int = lhs->value._int + rhs->value._int;
            return res;
        };
        intType->binaryOperations[make_pair(TT_MINUS, intType)] = 
            [intType, this](pExprResult lhs, pExprResult rhs) 
        {
            pExprResult res = new_ExprResult(intType);
            res->value._int = lhs->value._int - rhs->value._int;
            return res;
        };
        intType->binaryOperations[make_pair(TT_ASTERISK, intType)] = 
            [intType, this](pExprResult lhs, pExprResult rhs) 
        {
            pExprResult res = new_ExprResult(intType);
            res->value._int = lhs->value._int * rhs->value._int;
            return res;
        };

        pType doubleType = basicTypes[TT_DOUBLE] = new Interpretation::Type
            ([this](const string &s) 
        {
            pExprResult res = new_ExprResult();
            res->value._double = stod(s);
            return res;
        });
        doubleType->binaryOperations[make_pair(TT_PLUS, doubleType)] = 
            [doubleType, this](pExprResult lhs, pExprResult rhs) 
        {
            pExprResult res = new_ExprResult(doubleType);
            res->value._double = lhs->value._double + rhs->value._double;
            return res;
        };
        doubleType->binaryOperations[make_pair(TT_MINUS, doubleType)] = 
            [doubleType, this](pExprResult lhs, pExprResult rhs) 
        {
            pExprResult res = new_ExprResult(doubleType);
            res->value._double = lhs->value._double - rhs->value._double;
            return res;
        };
        doubleType->binaryOperations[make_pair(TT_ASTERISK, doubleType)] = 
            [doubleType, this](pExprResult lhs, pExprResult rhs) 
        {
            pExprResult res = new_ExprResult(doubleType);
            res->value._double = lhs->value._double * rhs->value._double;
            return res;
        };
        doubleType->binaryOperations[make_pair(TT_SLASH, doubleType)] = 
            [doubleType, this](pExprResult lhs, pExprResult rhs) 
        {
            pExprResult res = new_ExprResult(doubleType);
            res->value._double = lhs->value._double / rhs->value._double;
            return res;
        };
        globalNamespace->statementsBlock->vars["sin"] = new_ExprResult(functionType);
        globalNamespace->statementsBlock->vars["sin"]->value.pointer 
            = new Function(this, "sin", [this, doubleType](const vector<pSingleExpr> &args) 
        {
            pExprResult res = new_ExprResult(doubleType);
            res->value._double = sin( ( *begin(args) )->execute()->value._double);
            return res;
        });
    }
    void Parser::run() {
        try {
            if (globalNamespace->statementsBlock->vars.count("main") > 0) {
                globalNamespace->statementsBlock->vars["main"]->functionCallOperator(vector<pSingleExpr>());
            }
            else {
                throw exception("entry point not found");
            }
        }
        catch (exception &e) {
            cout << e.what() << endl;
            system("pause");
        }
    }
    void Parser::parse(ifstream &stream) 
    {
        string str;
        while (getline(stream, str) ) 
        {
            splitIntoTokens(str); 
        }
        tokens.push_back(Token(TT_END, "") );
        /*for (auto token = tokens.begin(); token != tokens.end(); token++) 
        {
            cout << token->strVal << endl;
        }*/
        tokenNumber = 0;
        parsingStatementsBlockStack.push_back(globalNamespace->statementsBlock);
        try 
        {
            while (hasMoreTokens() ) 
            {
                pDeclarativeStatement pDecl = pDeclarativeStatement(new DeclarativeStatement(this) )->parse();
                if (pDecl->_namespace) 
                {
                    globalNamespace->namespaces[pDecl->_namespace->id] = pDecl->_namespace;
                }
                else if (pDecl->function) 
                {
                    globalNamespace->statementsBlock->vars[pDecl->function->id] = 
                        pExprResult(new ExprResult(basicTypes[TT_FUNCTION]) );
                    globalNamespace->statementsBlock->vars[pDecl->function->id]->value.pointer =
                        pDecl->function;
                }
                else 
                {
                    /*for (auto vd: pDecl->vds) {
                        globalNamespace->variables[pDecl->function->id] = pDecl->function;
                    }*/
                }
            }
        }
        catch (exception &e) 
        {
            cout << e.what() << endl;
            system("pause");
        }
    }
    void Parser::splitIntoTokens(string s) 
    {
        string::iterator it = s.begin();
        static bool isStringToken = false;
        static bool isEscapeSequence = false;
        static Token current(TT_UNDEFINED, "");
        static auto pushToken = [this](Token &token ) 
        { 
            if(token.type != TT_UNDEFINED) 
            {
                if (token.type == TT_WORD) 
                {
                    if (token.strVal == "int") token.type = TT_INT;
                    else if (token.strVal == "char") token.type = TT_CHAR;
                    else if (token.strVal == "float") token.type = TT_FLOAT;
                    else if (token.strVal == "double") token.type = TT_DOUBLE;
                    else if (token.strVal == "long") token.type = TT_LONG;
                    else if (token.strVal == "bool") token.type = TT_BOOL;
                    else if (token.strVal == "return") token.type = TT_RETURN;
                    else if (token.strVal == "while") token.type = TT_WHILE;
                    else if (token.strVal == "if") token.type = TT_IF;
                    else if (token.strVal == "for") token.type = TT_FOR;
                    else if (token.strVal == "break") token.type = TT_BREAK;
                    else if (token.strVal == "continue") token.type = TT_CONTINUE;
                    else if (token.strVal == "class") token.type = TT_CLASS;
                    else if (token.strVal == "struct") token.type = TT_STRUCT;
                    else if (token.strVal == "echo") token.type = TT_ECHO;
                    else if (token.strVal == "true") token.type = TT_TRUE;
                    else if (token.strVal == "false") token.type = TT_FALSE;
                }
                else if(token.type == TT_SIGNS) 
                {
                    if (token.strVal == "+") token.type = TT_PLUS;
                    else if (token.strVal == "-") token.type = TT_MINUS;
                    else if (token.strVal == "=") token.type = TT_ASSIGN;
                    else if (token.strVal == "==") token.type = TT_EQUAL;
                    else if (token.strVal == ">=") token.type = TT_GR_EQ;
                    else if (token.strVal == "<=") token.type = TT_LESS_EQ;
                    else if (token.strVal == "!=") token.type = TT_NOT_EQ;
                    else if (token.strVal == "||") token.type = TT_DOUBLE_PIPE;
                    else if (token.strVal == "&&") token.type = TT_DOUBLE_AMP;
                    
                    else if (token.strVal == "*") token.type = TT_ASTERISK;
                    else if (token.strVal == ";") token.type = TT_SEMICOLON;
                    else if (token.strVal == ",") token.type = TT_COMMA;
                    else if (token.strVal == ".") token.type = TT_DOT;
                    else if (token.strVal == "!") token.type = TT_EXCL;
                    
                }
                tokens.push_back(token); 
                token = Token(TT_UNDEFINED, "");
            }
        };
        while (it != s.end() ) 
        {
            if (isStringToken) 
            {
                switch(*it) 
                {
                case '\\':
                    if (isEscapeSequence) 
                    {
                        switch (*it) 
                        {
                        case 'n':
                            current.strVal += "\n", isEscapeSequence = false;
                            break;
                        case 'r':
                            current.strVal += "\r", isEscapeSequence = false;
                            break;
                        case 't':
                            current.strVal += "\t", isEscapeSequence = false;
                            break;
                        case '\"':
                            current.strVal += "\"", isEscapeSequence = false;
                            break;
                        default:
                            isEscapeSequence = false;
                            break;
                        }
                    }
                    else {
                        isEscapeSequence = true;
                    }
                case '\"':
                    pushToken(current);
                    isStringToken = false; 
                    break;
                default:
                    current.strVal += *it;
                    break;
                }
            }
            else if (*it == '\"') 
            {
                pushToken(current);
                current.type = TT_STRING;
                isStringToken = true;
            }
            else if (*it == ' ' || *it == '\n' || *it == '\t') 
            {
                pushToken(current);
            }
            else if (*it == '(') 
            {
                pushToken(current), pushToken(current = Token(TT_PARENTHESIS_OPEN, "(") );
            }
            else if (*it == ')') 
            {
                pushToken(current), pushToken(current = Token(TT_PARENTHESIS_CLOSE, ")"));
            }
            else if (*it == '{') 
            {
                pushToken(current), pushToken(current = Token(TT_BRACE_OPEN, "{"));
            }
            else if (*it == '}') 
            {
                pushToken(current), pushToken(current = Token(TT_BRACE_CLOSE, "}"));
            }
            else if (*it == '[') 
            {
                pushToken(current), pushToken(current = Token(TT_BRACKET_OPEN, "{"));
            }
            else if (*it == ']') 
            {
                pushToken(current), pushToken(current = Token(TT_BRACKET_CLOSE, "}"));
            }
            else if (*it == '.') 
            {
                if (current.type == TT_NUMBER) 
                {
                    current.strVal += *it;
                }
                else 
                {
                    pushToken(current);
                    current.type = TT_SIGNS; 
                    current.strVal += *it;
                }
            }
            else if (isdigit(*it) ) {
                if (current.type == TT_UNDEFINED) {
                    current = Token(TT_NUMBER, string("") + *it);
                }
                else if (current.type == TT_NUMBER || current.type == TT_WORD 
                    || current.strVal == "." || current.strVal == "-")
                {
                    current.strVal += *it;
                    if (current.type == TT_SIGNS) 
                    {
                        current.type = TT_NUMBER;
                    }
                }
            }
            else if (Token::canBeUsedInIdentifier(*it) ) {
                if (current.type != TT_WORD) {
                    if (current.type == TT_UNDEFINED) {
                        current = Token(TT_WORD, string("") );
                    }
                    else if(current.type != TT_NUMBER) {
                        pushToken(current);
                        current.type = TT_WORD;
                    }
                }
                current.strVal += *it; 
            }
            else 
            {
                if (current.type != TT_SIGNS) 
                {
                    pushToken(current);
                    current.type = TT_SIGNS;
                }
                current.strVal += *it;
            }
            it++;
        }
        pushToken(current);
    }
    Token & Parser::currentToken() 
    {
        if (tokenNumber >= (int)tokens.size() ) 
        {
            throw exception("Token expected, but not found");
        }
        else 
        {
            return *(tokens.begin() + tokenNumber);
        }
    }
    Token & Parser::nextToken() 
    {
        tokenNumber++;
        return currentToken();
    }
    TokenType Parser::languageTypeByToken(const Token &t) 
    {
        if(t.type == TT_NUMBER) {
            bool isInt = true;
            bool isFloat = false;
            auto k = 1.0F;
            for (auto c: t.strVal) 
            {
                if (c == '.' || c == 'e' || c == 'E' || c == 'f' || c == 'F') 
                {
                        isInt = false;
                }
                if (c == 'f' || c == 'F') 
                {
                    isFloat = true;
                }
            }
            return (isInt ? TT_INT : (isFloat ? TT_FLOAT : TT_DOUBLE) );
        }
        else if(t.type == TT_STRING) return TT_STRING;
        else if(t.type == TT_TRUE || t.type == TT_FALSE) return TT_BOOL;
        return TT_UNDEFINED;
    }

}