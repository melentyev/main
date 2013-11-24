#include "interpretation.h"

namespace Interpretation {
    pExprResult Function::execute(const vector<pSingleExpr>& argsVal) {
        if (specialBehavior) {
            return specialBehavior(argsVal);
        }
        owner->callStack.push_back(new StackFrame() );
        owner->callStack.back()->statementsBlockStack.push_back(owner->globalNamespace->statementsBlock);
        owner->callStack.back()->statementsBlockStack.push_back(statementsBlock);
        auto itVal = argsVal.begin();
        for (auto it = this->args->vds.begin(); it != this->args->vds.end(); it++, itVal++) {
            statementsBlock->vars[(*it)->vd->id] = (*itVal)->execute();
        }
        for (auto &st: statementsBlock->statements) {
            st->execute();
            if (owner->callStack.back()->returnValue) {
                break;
            }
        }
        auto result = owner->callStack.back()->returnValue;
        owner->callStack.back()->statementsBlockStack.pop_back();// MEMORY LEAK
        owner->callStack.back()->statementsBlockStack.pop_back();// MEMORY LEAK
        owner->callStack.pop_back(); // MEMORY LEAK
        return result;
    }
}