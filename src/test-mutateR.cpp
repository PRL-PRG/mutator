#include <testthat.h>
#include <R.h>
#include <Rinternals.h>

extern "C" SEXP C_mutate_file(SEXP exprs, SEXP operators);

context("mutateR C++ entrypoints")
{

    test_that("C_mutate_file returns a list")
    {
        SEXP expr = PROTECT(Rf_lang3(Rf_install("+"), Rf_install("a"), Rf_install("b")));
        SEXP exprs = PROTECT(Rf_allocVector(EXPRSXP, 1));
        SET_VECTOR_ELT(exprs, 0, expr);

        SEXP srcref = PROTECT(Rf_allocVector(VECSXP, 1));
        SEXP single_sr = PROTECT(Rf_allocVector(INTSXP, 4));
        INTEGER(single_sr)
        [0] = 1;
        INTEGER(single_sr)
        [1] = 1;
        INTEGER(single_sr)
        [2] = 1;
        INTEGER(single_sr)
        [3] = 5;
        SET_VECTOR_ELT(srcref, 0, single_sr);
        Rf_setAttrib(exprs, Rf_install("srcref"), srcref);

        SEXP result = PROTECT(C_mutate_file(exprs, R_NilValue));
        expect_true(TYPEOF(result) == VECSXP);

        UNPROTECT(5);
    }
}
