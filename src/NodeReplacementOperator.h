#ifndef NODE_REPLACEMENT_OPERATOR_H
#define NODE_REPLACEMENT_OPERATOR_H

#include "Operator.h"

class NodeReplacementOperator : public Operator {
public:
    // `info`, when given, is shown as the new value instead of `replacement`.
    NodeReplacementOperator(SEXP original_symbol, SEXP replacement, SEXP info = R_NilValue)
        : Operator(original_symbol), replacement(replacement), info(info)
    {
        if (replacement != R_NilValue)
            R_PreserveObject(replacement);
        if (info != R_NilValue)
            R_PreserveObject(info);
    }

    NodeReplacementOperator(const NodeReplacementOperator&) = delete;
    NodeReplacementOperator& operator=(const NodeReplacementOperator&) = delete;

    ~NodeReplacementOperator() override
    {
        if (replacement != R_NilValue)
            R_ReleaseObject(replacement);
        if (info != R_NilValue)
            R_ReleaseObject(info);
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
        if (info != R_NilValue)
            return info;
        if (replacement == R_NilValue)
            return Rf_install("NULL");
        return replacement;
    }

private:
    SEXP replacement;
    SEXP info;
};

#endif
