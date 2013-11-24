#pragma once

#include <cstring>
#include <windows.h>
#include <iostream>
#include <fstream>
#include <cctype>
#include <vector>
#include <string>
#include <iostream>
#include <algorithm>
#include <memory>
#include <exception>
#include <functional>
#include <map>

using namespace std;

namespace Interpretation 
{
    enum TokenType;
    class Token;
    class Nonterminal;
    class Parser;
    class Namespace;
    class DeclarativeStatement;
    class VarDeclarationAllowDefault;
    class VarDeclaration;
    class Typename;
    class FunctionArgs;
    class StatementsBlock;
    class Statement;
    class Expr;
    class SingleExpr;
    class ThrowableExpr;
    class LogicalOrExpr;
    class LogicalAndExpr;
    class ComparisonExpr;
    class Arithmetics;
    class Term;
    class Factor;
    class Single;
    class ComplId;
    class Function;
    class ExprResult;
    class Type;
    class StackFrame;
    typedef Namespace* pNamespace; 
    typedef DeclarativeStatement* pDeclarativeStatement; 
    typedef VarDeclarationAllowDefault* pVarDeclarationAllowDefault; 
    typedef VarDeclaration* pVarDeclaration; 
    typedef Typename* pTypename; 
    typedef FunctionArgs* pFunctionArgs; 
    typedef StatementsBlock* pStatementsBlock; 
    typedef Statement* pStatement; 
    typedef Expr* pExpr; 
    typedef SingleExpr* pSingleExpr; 
    typedef ThrowableExpr* pThrowableExpr; 
    typedef LogicalOrExpr* pLogicalOrExpr; 
    typedef LogicalAndExpr* pLogicalAndExpr; 
    typedef ComparisonExpr* pComparisonExpr; 
    typedef Arithmetics* pArithmetics; 
    typedef Term* pTerm; 
    typedef Factor* pFactor; 
    typedef Single* pSingle;
    typedef ComplId* pComplId;
    typedef Function* pFunction; 
    typedef ExprResult* pExprResult; 
    typedef Type* pType;
    typedef StackFrame* pStackFrame;

    enum TokenType 
    {
        TT_UNDEFINED,
        TT_NUMBER,
        TT_WORD, 
        TT_STRING, 
        TT_END,
        TT_PARENTHESIS_OPEN,    // "("
        TT_PARENTHESIS_CLOSE,   // ")"
        TT_BRACE_OPEN,          // "{"
        TT_BRACE_CLOSE,         // "}"
        TT_BRACKET_OPEN,        // "["
        TT_BRACKET_CLOSE,       // "]"
        TT_ASSIGN,              // "="
        TT_LESS,                // "<"
        TT_GR,                  // ">"
        TT_PLUS_ASSIGN,         // "+="
        TT_MINUS_ASSIGN,        // "-="
        TT_ASTERISK_ASSIGN,     // "*="
        TT_SLASH_ASSIGN,        // "/="
        TT_EQUAL,               // "=="
        TT_GR_EQ,               // ">="
        TT_LESS_EQ,             // "<="
        TT_NOT_EQ,              // "!="
        TT_ARROW,               // "->"
        TT_PLUS,                // "+"
        TT_MINUS,               // "-"
        TT_ASTERISK,            // "*"
        TT_SLASH,               // "/"
        TT_PERCENT,             // "%"
        TT_AMP,                 // "&"
        TT_PIPE,                // "|"
        TT_DOUBLE_AMP,          // "&&"
        TT_DOUBLE_PIPE,         // "||"
        TT_COMMA,               // ","
        TT_DOT,                 // "."
        TT_SEMICOLON,           // ";"
        TT_QUESTION_MARK,       // "?"
        TT_EXCL,                // "!"
        TT_TILDA,               // "~"
        TT_SIGNS,
        TT_NAMESPACE,           // "namespace"
        TT_TYPEDEF,             // "typedef"
        TT_STRUCT,              // "struct"
        TT_CLASS,               // "class
        TT_PUBLIC,              // "public"
        TT_PROTECTED,           // "protected"
        TT_PRIVATE,             // "private"
        TT_AUTO,                // "auto"
        TT_INLINE,              // "inline"
        TT_VOID,                // "void"
        TT_CONST,               // "const"
        TT_RETURN,              // "return"
        TT_IF,                  // "if"
        TT_ELSE,                // "else"
        TT_WHILE,               // "while"
        TT_FOR,                 // "for"
        TT_DO,                  // "do"
        TT_DELETE,              // "delete"
        TT_BREAK,               // "break"
        TT_CONTINUE,            // "continue"
        TT_UNSIGNED,            // "unsigned"
        TT_INT,                 // "int"
        TT_CHAR,                // "char"
        TT_LONG,                // "long"
        TT_FLOAT,               // "float"
        TT_DOUBLE,              // "double"
        TT_BOOL,                // "bool"
        TT_THROW,               // "throw"
        TT_ECHO,                // "echo"
        TT_TRUE,                // "true"
        TT_FALSE,               // "false"
        TT_FUNCTION,            // for function type
    };

    class Token 
    {
    public:
        Token(TokenType _type = TT_UNDEFINED, const string &_val = "") : type(_type), strVal(_val) {}
        TokenType type;
        string strVal;
        bool canBeIdentifier() {
            if (type != TT_WORD) {
                return false;
            }
            if (isalpha(strVal[0]) || strVal[0] == '_') { 
                for(auto c: strVal) {
                    if(! (Token::canBeUsedInIdentifier(c) ) ) {
                        return false;
                    }
                }
            }
            else {
                return false;
            }
            return  true;
        }
        static bool canBeUsedInIdentifier(char c) {
            return (c == '_' || isdigit(c) || isalpha(c) );
        }
        static bool matchBrackets(const TokenType &a, const TokenType &b) {
            return 
                ( a == TT_PARENTHESIS_OPEN && b == TT_PARENTHESIS_CLOSE 
                || a == TT_BRACE_OPEN && b == TT_BRACE_CLOSE
                || a == TT_BRACKET_OPEN && b == TT_BRACKET_CLOSE);
        }
    };
    class Parser 
    {
    public:
        vector<Token> tokens;
        map<TokenType, pType> basicTypes;
        int tokenNumber;
        pNamespace globalNamespace;
        Parser();
        vector<pStackFrame> callStack;
        vector<pStatementsBlock> parsingStatementsBlockStack;
        void parse(ifstream &stream);
        void run();
        void Parser::splitIntoTokens(string s);
        pExprResult new_ExprResult(pType type = nullptr);
        Token &currentToken();
        Token &nextToken();
        bool hasMoreTokens() {
            return tokenNumber < ((int) tokens.size() - 1);
        }
        TokenType languageTypeByToken(const Token &t);
        ~Parser() {
            
        }
    };
    class Nonterminal {
    public:
        Parser *owner;
        Nonterminal(Parser *_owner) : owner(_owner) {}
        Token & currentToken();
        Token & nextToken();
        void tryParse(std::function<void(void)> fnTry, std::function < void(std::exception&) > fnCatch);
        void throwTokenExpected(TokenType tok);
        pNamespace new_Namespace();
        pExpr new_Expr();
        pDeclarativeStatement new_DeclarativeStatement();
        pTypename new_Typename();
        pFunctionArgs new_FunctionArgs();
        pVarDeclaration new_VarDeclaration();
        pStatementsBlock new_StatementsBlock();
        pVarDeclarationAllowDefault new_VarDeclarationAllowDefault();
        pStatement new_Statement();
        pSingleExpr new_SingleExpr();
        pThrowableExpr new_ThrowableExpr();
        pLogicalOrExpr new_LogicalOrExpr();
        pLogicalAndExpr new_LogicalAndExpr();
        pComparisonExpr new_ComparisonExpr();
        pArithmetics new_Arithmetics();
        pTerm new_Term();
        pFactor new_Factor();
        pSingle new_Single();
        pComplId new_ComplId(const std::string &s);
    };
    class Executable {
    public:
        virtual pExprResult execute() = 0;
    };

    class Namespace : public Nonterminal 
    {
    public:
        string id;
        map<string, pNamespace> namespaces;
        pStatementsBlock statementsBlock;
        vector<pDeclarativeStatement> declarations;
        Namespace(Parser *_owner, const string &_id): Nonterminal(_owner), id(_id) {}
        pNamespace parse();
    };
    class Function : public Nonterminal 
    {
    public:
        string id;
        pStatementsBlock statementsBlock;
        pFunctionArgs args;
        std::function<pExprResult(const vector<pSingleExpr>&)> specialBehavior;
        Function(Parser *_owner, const string &_id, 
            std::function<pExprResult(const vector<pSingleExpr>&)> _behavior = nullptr) 
            : Nonterminal(_owner), id(_id), specialBehavior(_behavior) {}
        pExprResult execute(const vector<pSingleExpr>& args = vector<pSingleExpr>() );
    };
    class Typename : public Nonterminal 
    {
    public:
        bool simple;
        bool simpleTypeIsUnsigned;
        bool simpleTypeIsLong;
        TokenType simpleType;
        Typename(Parser *_onwer) : Nonterminal(_onwer), simple(false), 
            simpleType(TT_UNDEFINED), simpleTypeIsLong(false), simpleTypeIsUnsigned(false) {}
        pTypename parse();
    };
    class Expr : public Nonterminal, public Executable 
    {
    public:
        vector<pSingleExpr> ses;
        Expr(Parser *_owner) : Nonterminal(_owner) {}
        pExpr parse();
        virtual pExprResult execute() override;
    };
    class VarDeclaration : public Nonterminal 
    {
    public:
        string id;
        bool isPointer;
        bool isArray;
        Token nArraySize;
        pVarDeclaration vd;
        VarDeclaration(Parser *_owner) : Nonterminal(_owner), vd(nullptr), nArraySize(Token() ), 
            isArray(false), isPointer(false) {}
        pVarDeclaration parse();
    };
    class VarDeclarationAllowDefault : public Nonterminal 
    {
    public:
        pVarDeclaration vd;
        pExpr rhs;
        VarDeclarationAllowDefault(Parser *_owner) : Nonterminal(_owner), vd(nullptr), rhs(nullptr) {}
        
        pVarDeclarationAllowDefault parse();
        bool canBeFunctionDeclaration() 
        {
            return !(rhs);
        }
    };
    class FunctionArgs : public Nonterminal 
    {
    public:
        vector<pTypename> typenames;
        vector<pVarDeclarationAllowDefault> vds;
        FunctionArgs(Parser *_owner) : Nonterminal(_owner) {}
        pFunctionArgs parse();
    };
    class DeclarativeStatement : public Nonterminal {
    public:
        pFunction function; 
        pNamespace _namespace;
        vector <pVarDeclarationAllowDefault> vds;
        DeclarativeStatement(Parser *_owner) : Nonterminal(_owner), function(nullptr), _namespace(nullptr) {}
        pDeclarativeStatement parse();
    };
    class StatementsBlock : public Nonterminal {
    public:
        vector<pStatement> statements;
        map<string, pExprResult> vars;
        StatementsBlock(Parser *_owner) : Nonterminal(_owner) {}
        pStatementsBlock parse();
    };
    class Statement : public Nonterminal {
    public:
        pStatementsBlock sb;
        vector<pVarDeclarationAllowDefault> vds;
        pTypename tn;
        pExpr expr, expr2;
        bool isSpecial;
        TokenType specialType;
        pStatementsBlock parentBlock;
        pStatement statement1, statement2;
        pStatementsBlock localStatementsBlock;
        Statement(Parser *_owner) : Nonterminal(_owner), sb(nullptr), expr(nullptr), 
            tn(nullptr), isSpecial(false), specialType(TT_UNDEFINED), parentBlock(nullptr) {}
        pStatement parse();
        void execute();
    };
    class SingleExpr : public Nonterminal, public Executable {
    public:
        bool bThrow;
        pThrowableExpr inner;
        SingleExpr(Parser *_owner) : Nonterminal(_owner), bThrow(false) {}
        pSingleExpr parse();
        virtual pExprResult execute() override;
    }; 
    class ThrowableExpr : public Nonterminal, public Executable {
    public:
        pFactor lhs;
        pLogicalOrExpr innerLogical;
        pThrowableExpr innerThrowable;
        TokenType operation;
        ThrowableExpr(Parser *_owner) : Nonterminal(_owner), lhs(nullptr), 
            innerLogical(nullptr), innerThrowable(nullptr), operation(TT_UNDEFINED) {}
        pThrowableExpr parse();
        virtual pExprResult execute() override;
    };
    class LogicalOrExpr : public Nonterminal, public Executable {
    public:
        vector<pLogicalAndExpr> inner;
        LogicalOrExpr(Parser *_owner) : Nonterminal(_owner) {}
        pLogicalOrExpr parse();
        virtual pExprResult execute() override;
    };
    class LogicalAndExpr : public Nonterminal, public Executable {
    public:
        vector<pComparisonExpr> inner;
        LogicalAndExpr(Parser *_owner) : Nonterminal(_owner) {}
        pLogicalAndExpr parse();
        virtual pExprResult execute() override;
    };
    class ComparisonExpr : public Nonterminal, public Executable {
    public:
        vector<pArithmetics> inner;
        vector<TokenType> operators;
        ComparisonExpr(Parser *_owner) : Nonterminal(_owner) {}
        pComparisonExpr parse();
        virtual pExprResult execute() override;
    };
    class Arithmetics : public Nonterminal, public Executable {
    public:
        vector<pTerm> inner;
        vector<TokenType> operators;
        Arithmetics(Parser *_owner) : Nonterminal(_owner) {}
        pArithmetics parse();
        virtual pExprResult execute() override;
    };
    class Term : public Nonterminal, public Executable {
    public:
        vector<pFactor> inner;
        vector<TokenType> operators;
        Term(Parser *_owner) : Nonterminal(_owner) {}
        pTerm parse();
        virtual pExprResult execute() override;
    };
    class Factor : public Nonterminal, public Executable {
    public:
        TokenType unaryOperator;
        bool isUnary;
        pFactor innerFactor;
        pTypename innerTypename;
        pSingle innerSingle;
        vector<pair<TokenType, vector<pSingleExpr> > > postfixOps;
        Factor(Parser *_owner) : Nonterminal(_owner), isUnary(false),
             innerFactor(nullptr), unaryOperator(TT_UNDEFINED) {}
        pFactor parse();
        virtual pExprResult execute() override;
    };
    class Single : public Nonterminal, public Executable {
    public:
        string id;
        Token innerToken;
        pExpr innerExpr;
        Single(Parser *_owner) : Nonterminal(_owner), innerToken(Token(TT_UNDEFINED, "") ),
            innerExpr(nullptr), id("") {}
        pSingle parse();
        virtual pExprResult execute() override;
    };
    class ComplId : public Nonterminal {
    public:
        string id;
        pStatementsBlock parentBlock;
        pExprResult varValue;
        ComplId(Parser *_owner, const string &_id, pStatementsBlock _pb = nullptr) 
            : Nonterminal(_owner), parentBlock(_pb), id(_id) {}
        pComplId parse();
    };
    union Value {
        int _int;
        unsigned int uInt;
        char _char;
        float _float;
        double _double;
        bool _bool;
        long long ll;
        unsigned long long ull;
        void *pointer;
    };
    class Type {
    public:
        bool isComplicated, isIntegral;
        Type(bool _isIntegral = false, std::function<pExprResult(const std::string &)> __valueOf = nullptr) 
            : isComplicated (false), _valueOf(__valueOf), isIntegral(_isIntegral),
            functionCallOperator(nullptr), arrayIndexerOperator(nullptr), isLogicalTrue(nullptr) {}
        map <pair<TokenType, pType>, std::function<pExprResult(pExprResult, pExprResult) > > binaryOperations;
        map <TokenType, std::function<pExprResult(pExprResult) > > unaryPrefixOperations;
        std::function<pExprResult(const std::string &)> _valueOf;
        std::function<pExprResult(pExprResult, const vector<pSingleExpr>&)> functionCallOperator;
        std::function<pExprResult(pExprResult, const vector<pSingleExpr>&)> arrayIndexerOperator;
        std::function<bool(pExprResult)> isLogicalTrue;
        
        pExprResult valueOf(const std::string &s);
    };
    class ExprResult {
    public:
        pType type;
        Value value;
        ExprResult(pType _type = nullptr) : type(_type) {}
        pExprResult binaryOperation(TokenType op, pExprResult rhs);
        pExprResult functionCallOperator(const vector<pSingleExpr> &args);
        pExprResult arrayIndexerOperator(const vector<pSingleExpr> &index);
        pExprResult unaryPrefixOperation(TokenType op);
        bool isLogicalTrue();
    };
    class StackFrame {
    public:
        StackFrame() : returnValue(nullptr) {}
        vector<pStatementsBlock> statementsBlockStack;
        pExprResult returnValue;
    };
}