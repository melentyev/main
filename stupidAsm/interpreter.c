#include "declarations.h"

int registerValues[REGISTERS_COUNT];
int programMemory[MEMORY_SIZE];
extern Command *commands;
InterpreterState globalState;

void interpreterInit()
{
    commands = NULL;
    globalState.halt = 0;
    globalState.result = 0;
    globalState.commandNumber = 0;
}

int* resolveArg(CommandArg *arg)
{
    switch (arg->type)
    {
    case AT_REGISTER:
        return registerValues + (arg->argv.r);
        break;
    case AT_CONST:
        return &arg->argv._const;
        break;
    case AT_ADDRESS:
        return programMemory + (*resolveArg(arg->argv.addr) );
        break;
    case AT_LABEL:
        return &arg->argv.mappedLabel;
        break;
    default:
        return NULL;
        break;
    }
}

void arithmeticalCommand(Command *cmd)
{
    if (cmd->args.arg1.type != AT_REGISTER)
    {
        error(ET_CONST_ASSIGN);
    }
    switch(cmd->type)
    {
    case CT_ADD:
        registerValues[cmd->args.arg1.argv.r] += *resolveArg(&cmd->args.arg2);
        break;
    case CT_SUB:
        registerValues[cmd->args.arg1.argv.r] -= *resolveArg(&cmd->args.arg2);
        break;
    case CT_MUL:
        registerValues[cmd->args.arg1.argv.r] *= *resolveArg(&cmd->args.arg2);
        break;
    case CT_DIV:
        registerValues[cmd->args.arg1.argv.r] /= *resolveArg(&cmd->args.arg2);
        break;
    case CT_MOD:
        registerValues[cmd->args.arg1.argv.r] %= *resolveArg(&cmd->args.arg2);
        break;
    default:
        break;
    }
}

void executeCommand(Command *cmd)
{
    switch(cmd->type)
    {
    case CT_ADD: case CT_SUB: case CT_MUL: case CT_DIV: case CT_MOD:
        arithmeticalCommand(cmd);
        globalState.commandNumber++;
        break;
    case CT_MOV:
        *resolveArg(&cmd->args.arg1) = *resolveArg(&cmd->args.arg2);
        globalState.commandNumber++;
        break;
    case CT_JMP:
        globalState.commandNumber = cmd->args.arg1.argv.mappedLabel;
        break;
    case CT_JE:
        globalState.commandNumber = (*resolveArg(&cmd->args.arg1) == 0)
                ? cmd->args.arg2.argv.mappedLabel
                : globalState.commandNumber + 1;
        break;
    case CT_JNE:
        globalState.commandNumber = (*resolveArg(&cmd->args.arg1) != 0)
                ? cmd->args.arg2.argv.mappedLabel
                : globalState.commandNumber + 1;
        break;
    case CT_JL:
        globalState.commandNumber = (*resolveArg(&cmd->args.arg1) < 0)
                ? cmd->args.arg2.argv.mappedLabel
                : globalState.commandNumber + 1;
        break;
    case CT_JNL:
        globalState.commandNumber = (*resolveArg(&cmd->args.arg1) >= 0)
                ? cmd->args.arg2.argv.mappedLabel
                : globalState.commandNumber + 1;
        break;
    case CT_JG:
        globalState.commandNumber = (*resolveArg(&cmd->args.arg1) > 0)
                ? cmd->args.arg2.argv.mappedLabel
                : globalState.commandNumber + 1;
        break;
    case CT_JNG:
        globalState.commandNumber = (*resolveArg(&cmd->args.arg1) <= 0)
                ? cmd->args.arg2.argv.mappedLabel
                : globalState.commandNumber + 1;
        break;
    case CT_HLT:
        globalState.halt = 1;
        globalState.result = *resolveArg(&cmd->args.arg1);
        break;
    }
}

int runProgram()
{
    globalState.commandNumber = 0;
    while (!globalState.halt)
    {
        executeCommand(commands + globalState.commandNumber);
    }
    return globalState.result;
}
