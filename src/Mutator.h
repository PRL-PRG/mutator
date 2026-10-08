// Mutator.h
#ifndef MUTATOR_H
#define MUTATOR_H

#include <string>
#include <vector>
#include <memory>
#include <utility>
#include "OperatorPos.h"
#include <R.h>
#include <Rinternals.h>

// A mutant and its mutation_info. Both are unprotected: store or protect them
// before the next allocation. `ok` is false when the mutation does not apply.
struct Mutation {
    SEXP mutant = R_NilValue;
    SEXP info = R_NilValue;
    bool ok = false;
};

// Class to Handle Mutation Application
class Mutator {
public:
    Mutator() = default;
    ~Mutator() = default;

    // Apply a given subset of operator flips to the original expression
    //SEXP applyMutations(SEXP expr, const std::vector<OperatorPos>& ops, int mask);
    Mutation applyMutation(SEXP expr, const std::vector<OperatorPos>& ops, int whichOpIndex);

    Mutation applyFlipMutation(SEXP expr, const std::vector<OperatorPos>& ops, int whichOpIndex);

    Mutation applyDeleteMutation(SEXP expr, const std::vector<OperatorPos>& ops, int whichOpIndex);

    Mutation applyNodeReplacementMutation(SEXP expr, const std::vector<OperatorPos>& ops, int whichOpIndex);
};

#endif // MUTATOR_H
