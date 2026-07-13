#include <R.h>
#include <Rinternals.h>
#include <Rembedded.h>
#include <R_ext/Parse.h>
#include <R_ext/Print.h>

// Undefine the 'length' macro defined by Rinternals.h to avoid conflicts with the C++ standard library
#undef length

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <regex>
#include <vector>
#include <unordered_set>
#include <unordered_map>

#include <R.h>
#include <Rinternals.h>
#include "ASTHandler.h"
#include "Mutator.h"
#include <vector>

static SEXP mutate_single(SEXP expr_sexp, SEXP src_ref_sexp, bool is_inside_block)
{
    if (TYPEOF(expr_sexp) == EXPRSXP) {
        if (Rf_length(expr_sexp) == 0)
            Rf_error("EXPRSXP input has no expressions.");
        expr_sexp = VECTOR_ELT(expr_sexp, 0);
    }

    SEXP buffer = R_NilValue;
    R_xlen_t n_mutants = 0;
    int n = 0;

    {
        ASTHandler astHandler;
        std::vector<OperatorPos> operators =
            astHandler.gatherOperators(expr_sexp, src_ref_sexp, is_inside_block);

        n = static_cast<int>(operators.size());
        if (n > 0) {
            Mutator mutator;
            buffer = PROTECT(Rf_allocVector(VECSXP, n));

            for (int i = 0; i < n; ++i) {
                auto result = mutator.applyMutation(expr_sexp, operators, i);
                if (result.second) {
                    SET_VECTOR_ELT(buffer, n_mutants, result.first);
                    ++n_mutants;
                }
            }

            // The operator vector owns preserved replacement objects. Preserve
            // the buffer independently before that vector is destroyed.
            R_PreserveObject(buffer);
            UNPROTECT(1);
        }
    }

    if (n == 0)
        return Rf_allocVector(VECSXP, 0);

    SEXP res = PROTECT(Rf_allocVector(VECSXP, n_mutants));
    for (R_xlen_t i = 0; i < n_mutants; ++i)
        SET_VECTOR_ELT(res, i, VECTOR_ELT(buffer, i));

    R_ReleaseObject(buffer);
    UNPROTECT(1);
    return res;
}

extern "C" SEXP C_mutate_single(SEXP expr_sexp, SEXP src_ref_sexp, SEXP is_inside_block_sexp)
{
    int is_inside_block = Rf_asLogical(is_inside_block_sexp);
    if (is_inside_block == NA_LOGICAL) {
        Rf_error("`is_inside_block` must be TRUE or FALSE.");
    }

    return mutate_single(expr_sexp, src_ref_sexp, is_inside_block != 0);
}

static bool isValidMutant(SEXP mutant)
{
    int n_protected = 0;
    int error = 0;

    SEXP deparse_call = PROTECT(Rf_lang2(Rf_install("deparse"), mutant));
    ++n_protected;

    SEXP text = R_tryEval(deparse_call, R_BaseEnv, &error);
    if (error != 0 || TYPEOF(text) != STRSXP) {
        UNPROTECT(n_protected);
        return false;
    }

    PROTECT(text);
    ++n_protected;

    ParseStatus status;
    SEXP parsed = PROTECT(R_ParseVector(text, -1, &status, R_NilValue));
    ++n_protected;

    bool valid = status == PARSE_OK && TYPEOF(parsed) == EXPRSXP;
    UNPROTECT(n_protected);
    return valid;
}

extern "C" SEXP C_mutate_file(SEXP exprs)
{
    if (TYPEOF(exprs) != EXPRSXP)
        Rf_error("Input must be an expression list (EXPRSXP).");

    SEXP src_ref = PROTECT(Rf_getAttrib(exprs, Rf_install("srcref")));
    if (TYPEOF(src_ref) != VECSXP || Rf_length(src_ref) != Rf_length(exprs))
        Rf_error("'srcref' attribute missing or malformed.");

    const int n_expr = Rf_length(exprs);

    std::vector<SEXP> valid_mutants;

    for (int i = 0; i < n_expr; ++i) {
        SEXP cur_expr     = VECTOR_ELT(exprs, i);
        SEXP cur_src_ref  = VECTOR_ELT(src_ref, i);

        // Each entry is a top-level expression, so it is never itself a
        // statement inside a `{ }` block. Block nesting (and the statement
        // deletions it enables) is discovered as the AST is traversed.
        SEXP cur_mutants  = PROTECT(mutate_single(cur_expr, cur_src_ref, false));
        if (TYPEOF(cur_mutants) != VECSXP)
            Rf_error("C_mutate_single did not return a list for expression %d.", i);

        const int n_mut   = Rf_length(cur_mutants);
        for (int j = 0; j < n_mut; ++j) {
            SEXP file_mut = PROTECT(Rf_allocVector(EXPRSXP, n_expr));
            SEXP mut = PROTECT(VECTOR_ELT(cur_mutants, j));
            SEXP mut_info = PROTECT(Rf_getAttrib(mut, Rf_install("mutation_info")));

            for (int k = 0; k < n_expr; ++k) {
                if (k == i) {
                    SET_VECTOR_ELT(file_mut, k, mut);
                } else {
                    SET_VECTOR_ELT(file_mut, k, VECTOR_ELT(exprs, k));
                }
            }
            Rf_setAttrib(file_mut, Rf_install("mutation_info"), mut_info);

            if (isValidMutant(file_mut)) {
                R_PreserveObject(file_mut);
                valid_mutants.push_back(file_mut);
            }

            UNPROTECT(3);
        }

        UNPROTECT(1);
    }

    // Build the final R list
    const R_xlen_t n_valid = static_cast<R_xlen_t>(valid_mutants.size());
    SEXP res = PROTECT(Rf_allocVector(VECSXP, n_valid));

    for (R_xlen_t i = 0; i < n_valid; ++i)
        SET_VECTOR_ELT(res, i, valid_mutants[i]);

    for (SEXP mut : valid_mutants)
        R_ReleaseObject(mut);

    UNPROTECT(2);
    return res;
}
