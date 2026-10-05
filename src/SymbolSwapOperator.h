// SymbolSwapOperator.h
#ifndef SYMBOL_SWAP_OPERATOR_H
#define SYMBOL_SWAP_OPERATOR_H

#include "Operator.h"

// Mutates a call by replacing its function symbol, e.g. `a + b` -> `a - b`.
class SymbolSwapOperator : public Operator {
public:
    SymbolSwapOperator(SEXP from, SEXP to) : Operator(from), replacement(to) {}

    std::string getType() const override {
        return "SymbolSwapOperator";
    }

    void flip(SEXP node) const {
        SETCAR(node, replacement);
    }

private:
    SEXP replacement; // an installed symbol, never collected
};

#endif // SYMBOL_SWAP_OPERATOR_H
