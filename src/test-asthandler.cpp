#include <testthat.h>
#include <R.h>
#include <Rinternals.h>
#include "ASTHandler.h"
#include "DeleteOperator.h"
#include "NodeReplacementOperator.h"

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

context("ASTHandler C++ tests")
{

    test_that("gatherOperators runs on a basic call")
    {
        SEXP expr = PROTECT(Rf_lang3(Rf_install("+"), Rf_install("a"), Rf_install("b")));
        SEXP srcref = PROTECT(makeSrcref());

        ASTHandler handler;
        std::vector<OperatorPos> ops = handler.gatherOperators(expr, srcref, false);

        expect_true(ops.size() >= 0);

        UNPROTECT(2);
    }

    test_that("gatherOperators discovers supported operator calls")
    {
        const char *symbols[] = {
            "+", "-", "*", "/", "==", "!=", "<", ">", "<=", ">=", "&", "|", "&&", "||"};

        ASTHandler handler;
        for (const char *symbol : symbols)
        {
            SEXP expr = PROTECT(Rf_lang3(Rf_install(symbol), Rf_install("a"), Rf_install("b")));
            SEXP srcref = PROTECT(makeSrcref());

            std::vector<OperatorPos> ops = handler.gatherOperators(expr, srcref, false);
            expect_true(ops.size() >= 1);

            UNPROTECT(2);
        }
    }

    test_that("gatherOperators creates value mutations for constants")
    {
        SEXP expr = PROTECT(Rf_ScalarInteger(42));
        SEXP srcref = PROTECT(makeSrcref());

        ASTHandler handler;
        std::vector<OperatorPos> ops = handler.gatherOperators(expr, srcref, false);

        expect_true(!ops.empty());
        expect_true(dynamic_cast<NodeReplacementOperator *>(ops[0].op.get()) != nullptr);
        UNPROTECT(2);
    }

    test_that("gatherOperators skips delete mutation for block braces")
    {
        SEXP inner = PROTECT(Rf_lang3(Rf_install("+"), Rf_install("a"), Rf_install("b")));
        SEXP block = PROTECT(Rf_lang2(Rf_install("{"), inner));
        SEXP srcref = PROTECT(makeSrcref());

        ASTHandler handler;
        std::vector<OperatorPos> ops = handler.gatherOperators(block, srcref, true);

        int delete_count = 0;
        int plus_count = 0;
        for (const auto &op : ops)
        {
            if (dynamic_cast<DeleteOperator *>(op.op.get()) != nullptr)
                ++delete_count;
            if (TYPEOF(op.original_symbol) == SYMSXP &&
                std::string(CHAR(PRINTNAME(op.original_symbol))) == "+")
                ++plus_count;
        }

        // Expect exactly one deletion candidate for inner '+' call, not for '{'.
        expect_true(delete_count == 1);
        expect_true(plus_count >= 1);

        UNPROTECT(3);
    }

    test_that("gatherOperators reads filename from srcfile environment")
    {
        SEXP expr = PROTECT(Rf_lang3(Rf_install("+"), Rf_install("a"), Rf_install("b")));
        SEXP srcref = PROTECT(makeSrcref());

        SEXP srcfile_env = PROTECT(R_NewEnv(R_EmptyEnv, TRUE, 29));
        Rf_defineVar(Rf_install("filename"), Rf_mkString("env_file.R"), srcfile_env);
        Rf_setAttrib(srcref, Rf_install("srcfile"), srcfile_env);

        ASTHandler handler;
        std::vector<OperatorPos> ops = handler.gatherOperators(expr, srcref, false);

        expect_true(!ops.empty());
        expect_true(ops[0].file_path == "env_file.R");

        UNPROTECT(3);
    }
}
