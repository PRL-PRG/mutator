#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

// Declare the function
extern SEXP C_mutate_single(SEXP expr_sexp, SEXP src_ref_sexp, SEXP is_inside_block, SEXP operators);

extern SEXP C_mutate_file(SEXP exprs, SEXP operators);
extern SEXP run_testthat_tests(SEXP use_xml_sxp);

// Define the registration table
static const R_CallMethodDef CallEntries[] = {
    {"C_mutate_single", (DL_FUNC)&C_mutate_single, 4},
    {"C_mutate_file", (DL_FUNC)&C_mutate_file, 2},
    {"run_testthat_tests", (DL_FUNC)&run_testthat_tests, 1},
    {NULL, NULL, 0}};

// Register the functions
void R_init_mutator(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}