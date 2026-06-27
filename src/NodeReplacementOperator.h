#ifndef NODE_REPLACEMENT_OPERATOR_H
#define NODE_REPLACEMENT_OPERATOR_H

#include "Operator.h"

class NodeReplacementOperator : public Operator {
public:
    NodeReplacementOperator(SEXP original_symbol, SEXP replacement)
        : Operator(original_symbol), replacement(replacement)
    {
        if (replacement != R_NilValue)
            R_PreserveObject(replacement);
    }

    NodeReplacementOperator(const NodeReplacementOperator&) = delete;
    NodeReplacementOperator& operator=(const NodeReplacementOperator&) = delete;

    ~NodeReplacementOperator() override
    {
        if (replacement != R_NilValue)
            R_ReleaseObject(replacement);
    }

    std::string getType() const override {
        return "NodeReplacementOperator";
    }

    SEXP makeReplacement() const {
        if (replacement == R_NilValue)
            return R_NilValue;
        return Rf_duplicate(replacement);
    }

    SEXP infoReplacement() const {
        if (replacement == R_NilValue)
            return Rf_install("NULL");
        return replacement;
    }

private:
    SEXP replacement;
};

#endif
