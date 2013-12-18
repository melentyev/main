#include "declarations.h"

#define EMPTY_SYMBOL_BUF EOF
#define CHARBUF_CHUNK 4

FILE *globalStream;
Token globalCurrentToken;
Label *symbolicLabels;
Command *commands;
int labelsCount = 0, commandNumber = 0, commandsCount = 0;
int globalCharBuf = EMPTY_SYMBOL_BUF;

typedef struct StrToCommandTypeMapping
{
    char *strVal;
    CommandType cmdType;
} StrToCommandTypeMapping;

typedef struct StrToRegisterNameMapping
{
    char *strVal;
    Register reg;
} StrToRegisterNameMapping;

StrToCommandTypeMapping keywords[KEYWORDS_COUNT] = {
    {"mov", CT_MOV},
    {"add", CT_ADD},
    {"sub", CT_SUB},
    {"mul", CT_MUL},
    {"div", CT_DIV},
    {"mod", CT_MOD},
    {"jmp", CT_JMP},
    {"je", CT_JE},
    {"jne", CT_JNE},
    {"jl", CT_JL},
    {"jnl", CT_JNL},
    {"jg", CT_JG},
    {"jng", CT_JNG},
    {"hlt", CT_HLT}
};

StrToRegisterNameMapping registers[REGISTERS_COUNT] = {
    {"r1", R_R1},
    {"r2", R_R2},
    {"r3", R_R3},
    {"r4", R_R4},
};

int currentChar(FILE *stream)
{
    return (globalCharBuf != EMPTY_SYMBOL_BUF) ? globalCharBuf : nextChar(stream);
}

int nextChar(FILE *stream)
{
    return (globalCharBuf = getc(stream) );
}

int findKeyword (const char *s)
{
    int found = -1, i;
    for (i = 0; i < KEYWORDS_COUNT; i++)
    {
        if (!strcmp(keywords[i].strVal, s) )
        {
            found = i;
            break;
        }
    }
    return found;
}

int findRegister (const char *s)
{
    int found = -1, i;
    for (i = 0; i < REGISTERS_COUNT; i++)
    {
        if (!strcmp(registers[i].strVal, s) )
        {
            found = i;
            break;
        }
    }
    return found;
}

int isKeyword (const char *s)
{
    return (findKeyword(s) != -1);
}

int isRegister (const char *s)
{
    return (findRegister(s) != -1);
}

Register getRegister(const char *s)
{
    int pos = findRegister(s);
    if (pos != -1)
    {
        return registers[pos].reg;
    }
    else
    {
        error(ET_EXPECTED_REGISTER);
        return R_R1;
    }
}

void parserInit(FILE *stream)
{
    symbolicLabels = NULL;
    globalStream = stream;
    commandsCount = 0;
    commandNumber = 0;
}

char* charbufAppend(char *s, int len, int c)
{
    if (len % CHARBUF_CHUNK == 0)
    {
        s = (char*)REALLOCATE(s, sizeof(char) * (len + CHARBUF_CHUNK + 1) );
    }
    s[len++] = c;
    s[len] = 0;
    return s;
}

Token nextToken()
{
    Token tok;
    int c;
    while ( ( c = currentChar(globalStream) ) == ' ' || c == '\t' || c == '\n')
    {
        nextChar(globalStream);
    }
    if(c == EOF)
    {
        tok.type = TT_EOF;
    }
    else if (c == ';')
    {
        while ( ( c = nextChar(globalStream) ) != '\n' && c != EOF );
        nextChar(globalStream);
    }
    else if (c == ':')
    {
        tok.type = TT_COLON;
        nextChar(globalStream);
    }
    else if (c == ';')
    {
        tok.type = TT_SEMICOLON;
        nextChar(globalStream);
    }
    else if(c == ',')
    {
        tok.type = TT_COMMA;
        nextChar(globalStream);
    }
    else if(c == '-' || isdigit(c) )
    {
        int sign = (c == '-' ? -1 : 1), val = (c == '-' ? 0 : (c - '0') );
        while(isdigit(nextChar(globalStream) ) )
        {
            val = val * 10 + (currentChar(globalStream) - '0');
        }
        tok.type = TT_NUMBER;
        tok.value._int = sign * val;
    }
    else
    {
        int len = 0;
        tok.type = TT_WORD;
        tok.value.str = NULL;
        tok.value.str = charbufAppend(tok.value.str, len++, c);
        while(isalnum(nextChar(globalStream) ) )
        {
            tok.value.str = charbufAppend(tok.value.str, len++, currentChar(globalStream));
        }
    }
    globalCurrentToken = tok;
    return tok;
}

Token currentToken()
{
    return (globalCurrentToken.type == TT_UNDEFINED)
            ? nextToken()
            : globalCurrentToken;
}

void addLabel(char *name, int commandNumber)
{
    symbolicLabels = (Label*)REALLOCATE(symbolicLabels, sizeof(Label) * (labelsCount + 1) );
    symbolicLabels[labelsCount].name = name;
    symbolicLabels[labelsCount].commandNumber = commandNumber;
    labelsCount++;
}

void addCommand(Command cmd)
{
    commands = (Command*)REALLOCATE(commands, sizeof(Command) * (commandsCount + 1) );
    commands[commandsCount++] = cmd;
    commandNumber++;
}

CommandType commandType(const char* s)
{
    int found = findKeyword(s);
    if (found != -1)
    {
        return keywords[found].cmdType;
    }
    else
    {
        error(ET_UNEXPECTED_TOKEN);
        return 0;
    }
}

CommandArg parseCommandArg(int allowRecursive)
{
    CommandArg res;
    if (currentToken().type == TT_BRACKET_OPEN)
    {
        if (allowRecursive)
        {
            nextToken();
            CommandArg *inner = (CommandArg*)ALLOCATE(sizeof(CommandArg) );
            *inner = parseCommandArg(0);
            res.type = AT_ADDRESS;
            res.argv.addr = inner;
            if (currentToken().type != TT_BRACKET_CLOSE)
            {
                error(ET_EXPECTED_BRACKET_CLOSE);
            }
        }
        else
        {
            error(ET_UNEXPECTED_TOKEN);
        }
    }
    else if (currentToken().type == TT_NUMBER)
    {
        res.type = AT_CONST;
        res.argv._const = currentToken().value._int;
    }
    else if (currentToken().type == TT_WORD)
    {
        int reg;
        if ( (reg = findRegister(currentToken().value.str) ) != -1)
        {
            res.type = AT_REGISTER;
            res.argv.r = reg;
        }
        else
        {
            res.type = AT_LABEL;
            res.argv.label = currentToken().value.str;
        }
    }
    nextToken();
    return res;
}

void parseInput()
{
    while (currentToken().type != TT_EOF)
    {
        Token tok1 = currentToken();
        if (tok1.type == TT_EOL)
        {
            nextToken();
        }
        else if (tok1.type == TT_SEMICOLON)
        {
            while(nextToken().type != TT_EOL);
            nextToken();
        }
        else if (tok1.type == TT_WORD)
        {
            Token tok2 = nextToken();
            if (tok2.type == TT_COLON)
            {
                addLabel(tok1.value.str, commandsCount);
                nextToken();
            }
            else
            {
                Command cmd;
                cmd.type = commandType(tok1.value.str);
                FREE(tok1.value.str);
                switch (cmd.type)
                {
                case CT_MOV: case CT_ADD: case CT_SUB: case CT_MUL:
                case CT_MOD: case CT_DIV:
                    cmd.args.arg1 = parseCommandArg(1);
                    cmd.args.arg2 = parseCommandArg(1);
                    break;
                case CT_JE: case CT_JNE:
                case CT_JL: case CT_JNL:
                case CT_JG: case CT_JNG:
                    cmd.args.arg1 = parseCommandArg(1);
                    cmd.args.arg2 = parseCommandArg(1);
                    if (cmd.args.arg2.type != AT_LABEL)
                    {
                        error(ET_UNEXPECTED_TOKEN);
                    }
                    break;
                case CT_JMP:
                    cmd.args.arg1 = parseCommandArg(1);
                    if (cmd.args.arg1.type != AT_LABEL)
                    {
                        error(ET_UNEXPECTED_TOKEN);
                    }
                    break;
                case CT_HLT:
                    cmd.args.arg1 = parseCommandArg(1);
                    break;
                }
                addCommand(cmd);
            }
        }
        else
        {
            error(ET_UNEXPECTED_TOKEN);
        }
    }
}
