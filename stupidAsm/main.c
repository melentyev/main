#include "declarations.h"

void error(ErrorType type)
{
    switch (type)
    {
    case ET_DIVISION_BY_ZERO:
        puts("Division by zero.");
        break;
    case ET_CONST_ASSIGN:
        puts("ET_CONST_ASSIGN");
        break;
    case ET_UNEXPECTED_TOKEN:
        puts("ET_UNEXPECTED_TOKEN");
        break;
    case ET_UNEXPECTED_EOF:
        puts("ET_RUNTIME_ERROR");
        break;
    case ET_RUNTIME_ERROR:
        puts("ET_RUNTIME_ERROR");
        break;
    }
}

int main(void)
{
    FILE *in = fopen("input.txt", "r");
    int result;
    interpreterInit();
    parserInit(in);
    parseInput();
    linkProgram();
    result = runProgram();
    printf("Calculation result: %d", result);
    return 0;
}
