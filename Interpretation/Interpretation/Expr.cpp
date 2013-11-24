#include "interpretation.h"

namespace Interpretation {
    pExprResult Expr::execute() {
        pExprResult res = nullptr;
        for (auto it = begin(ses); it != end(ses); it++) {
            res = (*it)->execute();
            if (it != end(ses) ) {
                // TODO: DESTROY RES
            }
        }
        return res;
    }
    pExpr Expr::parse() {
        ses.push_back(new_SingleExpr()->parse() );
        while (currentToken().type == TT_COMMA) {
            nextToken();
            ses.push_back(new_SingleExpr()->parse() );
        }
        return pExpr(this);
    }
}