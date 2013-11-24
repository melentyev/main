#include "interpretation.h"

namespace Interpretation {
    pExprResult Type::valueOf(const std::string &s) {
        pExprResult res = _valueOf(s);
        res->type = this;
        return res;
    }
}