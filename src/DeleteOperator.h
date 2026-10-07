#ifndef DEL_OPERATOR_H
#define DEL_OPERATOR_H

#include "Operator.h"

class DeleteOperator : public Operator {
public: 
    DeleteOperator(SEXP symbol) : Operator(symbol){}
    virtual ~DeleteOperator() = default;

    std::string getType() const override {
        return "DeleteOperator";
    }

    // Note: deletion is handled structurally by Mutator::applyDeleteMutation and
    // is not a symbol replacement, so DeleteOperator deliberately does not
    // implement flip() (it does not inherit from SymbolSwapOperator).
};

#endif