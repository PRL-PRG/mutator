#include <testthat.h>
#include <R.h>
#include <Rinternals.h>
#include "ASTHandler.h"
#include "Mutator.h"
#include "PlusOperator.h"
#include "DeleteOperator.h"

static SEXP makeSrcref()
{
    SEXP srcref = Rf_allocVector(INTSXP, 4);
    INTEGER(srcref)
    [0] = 1;
    INTEGER(srcref)
    [1] = 1;
    INTEGER(srcref)
    [2] = 1;
    INTEGER(srcref)
    [3] = 5;
    return srcref;
}

context("Mutator C++ tests")
{

    test_that("applyMutation runs on first discovered operator")
    {
        SEXP expr = PROTECT(Rf_lang3(Rf_install("+"), Rf_install("a"), Rf_install("b")));
        SEXP srcref = PROTECT(makeSrcref());

        ASTHandler handler;
        std::vector<OperatorPos> ops = handler.gatherOperators(expr, srcref, false);

        if (ops.size() > 0)
        {
            Mutator mutator;
            auto result = mutator.applyMutation(expr, ops, 0);
            SEXP mutated = PROTECT(result.first);
            bool valid_type = TYPEOF(mutated) == LANGSXP ||
                              TYPEOF(mutated) == EXPRSXP ||
                              TYPEOF(mutated) == VECSXP;
            expect_true(valid_type);
            UNPROTECT(1);
        }
        else
        {
            expect_true(true);
        }

        UNPROTECT(2);
    }

    test_that("applyMutation returns false for invalid operator index")
    {
        SEXP expr = PROTECT(Rf_lang3(Rf_install("+"), Rf_install("a"), Rf_install("b")));
        std::vector<OperatorPos> ops;

        ops.push_back(OperatorPos({0}, std::make_unique<PlusOperator>(), 1, 1, 1, 5, Rf_install("+")));
        Mutator mutator;
        auto result = mutator.applyMutation(expr, ops, 9);

        expect_true(result.second == false);
        expect_true(result.first == R_NilValue);
        UNPROTECT(1);
    }

    test_that("applyFlipMutation records mutation_info on valid path")
    {
        SEXP expr = PROTECT(Rf_lang3(Rf_install("+"), Rf_install("a"), Rf_install("b")));
        std::vector<OperatorPos> ops;
        ops.push_back(OperatorPos({}, std::make_unique<PlusOperator>(), 1, 1, 1, 5, Rf_install("+")));

        Mutator mutator;
        auto result = mutator.applyFlipMutation(expr, ops, 0);
        SEXP mutated = PROTECT(result.first);

        expect_true(result.second == true);
        expect_true(TYPEOF(mutated) == LANGSXP);
        expect_true(Rf_getAttrib(mutated, Rf_install("mutation_info")) != R_NilValue);
        UNPROTECT(2);
    }

    test_that("applyDeleteMutation rejects root deletion")
    {
        SEXP expr = PROTECT(Rf_lang3(Rf_install("+"), Rf_install("a"), Rf_install("b")));
        std::vector<OperatorPos> ops;
        ops.push_back(OperatorPos({}, std::make_unique<DeleteOperator>(expr), 1, 1, 1, 5, expr));

        Mutator mutator;
        auto result = mutator.applyDeleteMutation(expr, ops, 0);

        expect_true(result.second == false);
        expect_true(result.first == R_NilValue);
        UNPROTECT(1);
    }

    test_that("applyDeleteMutation removes the first argument for path index zero")
    {
        SEXP expr = PROTECT(Rf_lang3(Rf_install("+"), Rf_install("a"), Rf_install("b")));
        std::vector<OperatorPos> ops;
        ops.push_back(OperatorPos({0}, std::make_unique<DeleteOperator>(expr), 1, 1, 1, 5, expr));

        Mutator mutator;
        auto result = mutator.applyDeleteMutation(expr, ops, 0);
        SEXP mutated = PROTECT(result.first);

        expect_true(result.second == true);
        expect_true(TYPEOF(mutated) == LANGSXP);
        expect_true(CAR(CDR(mutated)) == Rf_install("b"));
        expect_true(CDDR(mutated) == R_NilValue);
        expect_true(Rf_getAttrib(mutated, Rf_install("mutation_info")) != R_NilValue);
        UNPROTECT(2);
    }

    test_that("applyDeleteMutation removes the second argument for path index one")
    {
        SEXP expr = PROTECT(Rf_lang3(Rf_install("+"), Rf_install("a"), Rf_install("b")));
        std::vector<OperatorPos> ops;
        ops.push_back(OperatorPos({1}, std::make_unique<DeleteOperator>(expr), 1, 1, 1, 5, expr));

        Mutator mutator;
        auto result = mutator.applyDeleteMutation(expr, ops, 0);
        SEXP mutated = PROTECT(result.first);

        expect_true(result.second == true);
        expect_true(TYPEOF(mutated) == LANGSXP);
        expect_true(CAR(CDR(mutated)) == Rf_install("a"));
        expect_true(CDDR(mutated) == R_NilValue);
        expect_true(Rf_getAttrib(mutated, Rf_install("mutation_info")) != R_NilValue);
        UNPROTECT(2);
    }

    test_that("applyDeleteMutation uses zero-based CDR paths for nested calls")
    {
        SEXP inner = PROTECT(Rf_lang3(Rf_install("g"), Rf_install("a"), Rf_install("b")));
        SEXP expr = PROTECT(Rf_lang3(Rf_install("f"), inner, Rf_install("c")));
        std::vector<OperatorPos> ops;
        ops.push_back(OperatorPos({0, 1}, std::make_unique<DeleteOperator>(inner), 1, 1, 1, 5, inner));

        Mutator mutator;
        auto result = mutator.applyDeleteMutation(expr, ops, 0);
        SEXP mutated = PROTECT(result.first);

        expect_true(result.second == true);
        expect_true(TYPEOF(mutated) == LANGSXP);

        SEXP mutated_inner = CAR(CDR(mutated));
        expect_true(TYPEOF(mutated_inner) == LANGSXP);
        expect_true(CAR(mutated_inner) == Rf_install("g"));
        expect_true(CAR(CDR(mutated_inner)) == Rf_install("a"));
        expect_true(CDDR(mutated_inner) == R_NilValue);

        UNPROTECT(3);
    }

    test_that("applyMutation flips each supported operator")
    {
        const char *symbols[] = {
            "+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">=", "&", "|", "&&", "||"};

        ASTHandler handler;
        Mutator mutator;

        for (const char *symbol : symbols)
        {
            SEXP expr = PROTECT(Rf_lang3(Rf_install(symbol), Rf_install("a"), Rf_install("b")));
            SEXP srcref = PROTECT(makeSrcref());

            std::vector<OperatorPos> ops = handler.gatherOperators(expr, srcref, false);
            auto result = mutator.applyMutation(expr, ops, 0);
            SEXP mutated = PROTECT(result.first);

            expect_true(result.second == true);
            expect_true(TYPEOF(mutated) == LANGSXP);
            UNPROTECT(3);
        }
    }

    test_that("applyFlipMutation fails for invalid traversal path")
    {
        SEXP expr = PROTECT(Rf_lang3(Rf_install("+"), Rf_install("a"), Rf_install("b")));
        std::vector<OperatorPos> ops;
        ops.push_back(OperatorPos({2}, std::make_unique<PlusOperator>(), 1, 1, 1, 5, Rf_install("+")));

        Mutator mutator;
        auto result = mutator.applyFlipMutation(expr, ops, 0);

        expect_true(result.second == false);
        expect_true(result.first == R_NilValue);
        UNPROTECT(1);
    }

    test_that("applyFlipMutation records CHARSXP original_symbol and NA file_path")
    {
        SEXP expr = PROTECT(Rf_lang3(Rf_install("+"), Rf_install("a"), Rf_install("b")));
        std::vector<OperatorPos> ops;
        ops.push_back(OperatorPos({}, std::make_unique<PlusOperator>(), 1, 1, 1, 5, Rf_mkChar("+")));

        Mutator mutator;
        auto result = mutator.applyFlipMutation(expr, ops, 0);
        SEXP mutated = PROTECT(result.first);

        expect_true(result.second == true);
        SEXP info = PROTECT(Rf_getAttrib(mutated, Rf_install("mutation_info")));
        expect_true(TYPEOF(info) == VECSXP);

        SEXP orig = VECTOR_ELT(info, 4); // original_symbol
        SEXP file_path = VECTOR_ELT(info, 6);
        expect_true(TYPEOF(orig) == STRSXP);
        expect_true(std::string(CHAR(STRING_ELT(orig, 0))) == "+");
        expect_true(TYPEOF(file_path) == STRSXP);
        expect_true(STRING_ELT(file_path, 0) == NA_STRING);

        UNPROTECT(3);
    }

    test_that("applyFlipMutation records STRSXP original_symbol")
    {
        SEXP expr = PROTECT(Rf_lang3(Rf_install("+"), Rf_install("a"), Rf_install("b")));
        std::vector<OperatorPos> ops;
        SEXP original_symbol = PROTECT(Rf_mkString("+"));
        ops.push_back(OperatorPos({}, std::make_unique<PlusOperator>(), 1, 1, 1, 5, original_symbol));

        Mutator mutator;
        auto result = mutator.applyFlipMutation(expr, ops, 0);
        SEXP mutated = PROTECT(result.first);

        expect_true(result.second == true);
        SEXP info = PROTECT(Rf_getAttrib(mutated, Rf_install("mutation_info")));
        expect_true(TYPEOF(info) == VECSXP);

        SEXP orig = VECTOR_ELT(info, 4); // original_symbol
        expect_true(TYPEOF(orig) == STRSXP);
        expect_true(std::string(CHAR(STRING_ELT(orig, 0))) == "+");

        UNPROTECT(4);
    }
}
