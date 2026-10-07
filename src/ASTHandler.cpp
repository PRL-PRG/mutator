// ASTHandler.cpp

#include <algorithm>
#include <climits>
#include <map>
#include <unordered_set>
#include <iostream>
#include <Rversion.h>
#include "ASTHandler.h"
#include "SymbolSwapOperator.h"
#include "DeleteOperator.h"
#include "NodeReplacementOperator.h"

static struct CachedSyms
{
    SEXP s_lbrace = Rf_install("{");
    SEXP s_rbrace = Rf_install("}");
    SEXP s_filename = Rf_install("filename");
    SEXP s_plus = Rf_install("+");
    SEXP s_minus = Rf_install("-");
    SEXP s_mul = Rf_install("*");
    SEXP s_div = Rf_install("/");
    SEXP s_pow = Rf_install("^");
    SEXP s_mod = Rf_install("%%");
    SEXP s_intdiv = Rf_install("%/%");
    SEXP s_eq = Rf_install("==");
    SEXP s_neq = Rf_install("!=");
    SEXP s_lt = Rf_install("<");
    SEXP s_gt = Rf_install(">");
    SEXP s_le = Rf_install("<=");
    SEXP s_ge = Rf_install(">=");
    SEXP s_and = Rf_install("&");
    SEXP s_or = Rf_install("|");
    SEXP s_land = Rf_install("&&");
    SEXP s_lor = Rf_install("||");
    SEXP s_not = Rf_install("!");
    SEXP s_if = Rf_install("if");
    SEXP s_while = Rf_install("while");
    SEXP s_for = Rf_install("for");
    SEXP s_repeat = Rf_install("repeat");
    SEXP s_function = Rf_install("function");
    SEXP s_return = Rf_install("return");
    SEXP s_break = Rf_install("break");
    SEXP s_next = Rf_install("next");
    SEXP s_lparen = Rf_install("(");
    SEXP s_assign = Rf_install("<-");
    SEXP s_eq_assign = Rf_install("=");
    SEXP s_super_assign = Rf_install("<<-");
    SEXP s_subset2 = Rf_install("[[");
    SEXP s_subset = Rf_install("[");
    SEXP s_colon = Rf_install(":");
    SEXP s_seq_len = Rf_install("seq_len");
    SEXP s_seq_along = Rf_install("seq_along");
    SEXP s_length = Rf_install("length");
    SEXP s_drop = Rf_install("drop");
    SEXP s_expr = Rf_install("expr");
    SEXP s_invisible = Rf_install("invisible");
    SEXP s_stop = Rf_install("stop");
    SEXP s_stopifnot = Rf_install("stopifnot");
    SEXP s_trycatch = Rf_install("tryCatch");
    SEXP s_try = Rf_install("try");
    SEXP s_with_handlers = Rf_install("withCallingHandlers");
    SEXP s_srcref = Rf_install("srcref");
    SEXP s_mutinfo = Rf_install("mutation_info");
} SYM;

static bool isSymbol(SEXP x, SEXP sym)
{
    return TYPEOF(x) == SYMSXP && x == sym;
}

static bool isCallTo(SEXP x, SEXP sym)
{
    return TYPEOF(x) == LANGSXP && isSymbol(CAR(x), sym);
}

static bool isAssignmentSymbol(SEXP fun)
{
    return isSymbol(fun, SYM.s_assign) ||
           isSymbol(fun, SYM.s_eq_assign) ||
           isSymbol(fun, SYM.s_super_assign);
}

static bool isScalarConstant(SEXP x)
{
    switch (TYPEOF(x))
    {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case STRSXP:
    case CPLXSXP:
        return Rf_length(x) == 1;
    default:
        return x == R_NilValue;
    }
}

static bool isMutableScalarConstant(SEXP x)
{
    return x != R_NilValue && isScalarConstant(x);
}

static bool isNumericScalarConstant(SEXP x)
{
    return (TYPEOF(x) == INTSXP || TYPEOF(x) == REALSXP) && Rf_length(x) == 1;
}

static bool isNAConstant(SEXP x)
{
    if (!isScalarConstant(x) || x == R_NilValue)
        return false;
    switch (TYPEOF(x))
    {
    case LGLSXP:
        return LOGICAL(x)[0] == NA_LOGICAL;
    case INTSXP:
        return INTEGER(x)[0] == NA_INTEGER;
    case REALSXP:
        return ISNA(REAL(x)[0]);
    case STRSXP:
        return STRING_ELT(x, 0) == NA_STRING;
    case CPLXSXP:
        return ISNA(COMPLEX(x)[0].r) && ISNA(COMPLEX(x)[0].i);
    default:
        return false;
    }
}

static bool isFortyTwo(SEXP x)
{
    if (!isNumericScalarConstant(x))
        return false;
    if (TYPEOF(x) == INTSXP)
        return INTEGER(x)[0] != NA_INTEGER && INTEGER(x)[0] == 42;
    return !ISNA(REAL(x)[0]) && !ISNAN(REAL(x)[0]) && REAL(x)[0] == 42.0;
}

// `value_42`: 0 -> 42, nonzero -> 0. Off by default (many trivial type-error kills).
static SEXP makeScalarValueReplacement(SEXP x)
{
    if (!isNumericScalarConstant(x))
        return R_NilValue;

    if (TYPEOF(x) == INTSXP)
    {
        int value = INTEGER(x)[0];
        if (value == NA_INTEGER)
            return R_NilValue;
        return Rf_ScalarInteger(value == 0 ? 42 : 0);
    }

    double value = REAL(x)[0];
    if (ISNA(value) || ISNAN(value))
        return R_NilValue;
    return Rf_ScalarReal(value == 0.0 ? 42.0 : 0.0);
}

static SEXP makeNAReplacement(SEXP x)
{
    switch (TYPEOF(x))
    {
    case LGLSXP:
        return Rf_ScalarLogical(NA_LOGICAL);
    case INTSXP:
        return Rf_ScalarInteger(NA_INTEGER);
    case REALSXP:
        return Rf_ScalarReal(NA_REAL);
    case STRSXP:
        return Rf_ScalarString(NA_STRING);
    case CPLXSXP:
    {
        Rcomplex z;
        z.r = NA_REAL;
        z.i = NA_REAL;
        return Rf_ScalarComplex(z);
    }
    default:
        return R_NilValue;
    }
}

static SEXP makeFortyTwo()
{
    return Rf_ScalarReal(42.0);
}

// A length-1 NA of the requested R type: NA (logical), NA_integer_, NA_real_,
// NA_character_. Used to swap an NA constant for a differently-typed NA, probing
// coercion sensitivity (e.g. NA vs NA_real_ in `vapply`/`c()`/column types).
static SEXP makeNAOfType(int type)
{
    switch (type)
    {
    case LGLSXP:
        return Rf_ScalarLogical(NA_LOGICAL);
    case INTSXP:
        return Rf_ScalarInteger(NA_INTEGER);
    case REALSXP:
        return Rf_ScalarReal(NA_REAL);
    case STRSXP:
        return Rf_ScalarString(NA_STRING);
    default:
        return R_NilValue;
    }
}

// `const_off_by_one`: n -> n + delta, keeping the type. NULL if not applicable.
static SEXP makeOffByOne(SEXP x, int delta)
{
    if (TYPEOF(x) == INTSXP && Rf_length(x) == 1)
    {
        int v = INTEGER(x)[0];
        // NA_INTEGER is INT_MIN, so INT_MIN + 1 - 1 would be NA.
        if (v == NA_INTEGER || (delta > 0 && v == INT_MAX) || (delta < 0 && v == INT_MIN + 1))
            return R_NilValue;
        return Rf_ScalarInteger(v + delta);
    }
    if (TYPEOF(x) == REALSXP && Rf_length(x) == 1)
    {
        double v = REAL(x)[0];
        if (!R_FINITE(v))
            return R_NilValue;
        return Rf_ScalarReal(v + delta);
    }
    return R_NilValue;
}

static bool isLogicalConstant(SEXP x, int value)
{
    return TYPEOF(x) == LGLSXP && Rf_length(x) == 1 && LOGICAL(x)[0] == value;
}

static bool isNonNABool(SEXP x)
{
    return TYPEOF(x) == LGLSXP && Rf_length(x) == 1 && LOGICAL(x)[0] != NA_LOGICAL;
}

// Copy of `call` without its k-th argument (0-based).
static SEXP dropArgument(SEXP call, int k)
{
    SEXP copy = PROTECT(Rf_duplicate(call));
    SEXP prev = copy;
    for (int j = 0; j < k; ++j)
        prev = CDR(prev);
    SETCDR(prev, CDDR(prev));
    UNPROTECT(1);
    return copy;
}

using SymbolSet = std::unordered_set<SEXP>;

static SymbolSet makeSymbolSet(std::initializer_list<const char *> names)
{
    SymbolSet set;
    for (const char *name : names)
        set.insert(Rf_install(name));
    return set;
}

static SEXP makeNotCall(SEXP expr)
{
    SEXP duplicated = PROTECT(Rf_duplicate(expr));
    SEXP call = PROTECT(Rf_lang2(SYM.s_not, duplicated));
    UNPROTECT(2);
    return call;
}

static bool isOrdinaryFunctionCall(SEXP expr)
{
    if (TYPEOF(expr) != LANGSXP || TYPEOF(CAR(expr)) != SYMSXP)
        return false;

    SEXP fun = CAR(expr);
    if (isAssignmentSymbol(fun))
        return false;

    return !(isSymbol(fun, SYM.s_plus) ||
             isSymbol(fun, SYM.s_minus) ||
             isSymbol(fun, SYM.s_mul) ||
             isSymbol(fun, SYM.s_div) ||
             isSymbol(fun, SYM.s_eq) ||
             isSymbol(fun, SYM.s_neq) ||
             isSymbol(fun, SYM.s_lt) ||
             isSymbol(fun, SYM.s_gt) ||
             isSymbol(fun, SYM.s_le) ||
             isSymbol(fun, SYM.s_ge) ||
             isSymbol(fun, SYM.s_and) ||
             isSymbol(fun, SYM.s_or) ||
             isSymbol(fun, SYM.s_land) ||
             isSymbol(fun, SYM.s_lor) ||
             isSymbol(fun, SYM.s_not) ||
             isSymbol(fun, SYM.s_if) ||
             isSymbol(fun, SYM.s_while) ||
             isSymbol(fun, SYM.s_for) ||
             isSymbol(fun, SYM.s_repeat) ||
             isSymbol(fun, SYM.s_function) ||
             isSymbol(fun, SYM.s_return) ||
             isSymbol(fun, SYM.s_break) ||
             isSymbol(fun, SYM.s_next) ||
             isSymbol(fun, SYM.s_lbrace) ||
             isSymbol(fun, SYM.s_lparen));
}

static void addNodeReplacement(std::vector<OperatorPos> &ops,
                               const std::vector<int> &path,
                               int start_line,
                               int start_col,
                               int end_line,
                               int end_col,
                               SEXP original,
                               SEXP replacement,
                               const std::string &file_path,
                               const char *operator_id,
                               SEXP info = R_NilValue)
{
    SEXP protected_replacement = PROTECT(replacement);
    PROTECT(info);
    ops.push_back({path,
                   std::make_unique<NodeReplacementOperator>(original, protected_replacement, info),
                   start_line,
                   start_col,
                   end_line,
                   end_col,
                   original,
                   file_path,
                   operator_id});
    UNPROTECT(2);
}

static SEXP getVarFromFrame(SEXP env, SEXP name)
{
#if R_VERSION >= R_Version(4, 5, 0)
    return R_getVar(name, env, FALSE);
#else
    return Rf_findVarInFrame(env, name);
#endif
}

static bool extractSrcrefBounds(SEXP srcref, int &start_line, int &start_col, int &end_line, int &end_col)
{
    if (TYPEOF(srcref) != INTSXP || LENGTH(srcref) < 4)
        return false;
    const int *p = INTEGER(srcref);
    const int n = LENGTH(srcref);
    start_line = p[0];
    end_line = p[2];
    // An R srcref is (first_line, first_byte, last_line, last_byte,
    // first_column, last_column, ...). Report character columns (indices 4/5)
    // when present; the short 4-element form carries no column information, so
    // fall back to the byte offsets (indices 1/3) as the best approximation.
    if (n >= 6)
    {
        start_col = p[4];
        end_col = p[5];
    }
    else
    {
        start_col = p[1];
        end_col = p[3];
    }
    return true;
}

bool ASTHandler::isDeletableStatement(SEXP stmt)
{
    // Any genuine statement of a `{ }` block can be removed. Anything that does
    // not survive a deparse/re-parse round-trip is discarded downstream, so we
    // do not need to second-guess individual statement kinds here.
    return stmt != R_NilValue;
}

std::vector<OperatorPos> ASTHandler::gatherOperators(SEXP expr, SEXP src_ref,
                                                     bool is_inside_block)
{
    if (!extractSrcrefBounds(src_ref, _start_line, _start_col, _end_line, _end_col))
        Rf_error("src_ref must be an integer vector of length 4 or more");

    _file_path.clear();

    SEXP srcfile = PROTECT(Rf_getAttrib(src_ref, Rf_install("srcfile")));
    if (srcfile != R_NilValue)
    {
        SEXP filename = PROTECT(Rf_getAttrib(srcfile, SYM.s_filename));
        if (TYPEOF(filename) == STRSXP && LENGTH(filename) > 0)
        {
            _file_path = CHAR(STRING_ELT(filename, 0));
        }
        else if (TYPEOF(srcfile) == ENVSXP)
        {
            SEXP env_name = PROTECT(getVarFromFrame(srcfile, SYM.s_filename));
            if (env_name != R_UnboundValue && TYPEOF(env_name) == STRSXP && LENGTH(env_name) > 0)
            {
                _file_path = CHAR(STRING_ELT(env_name, 0));
            }
            UNPROTECT(1);
        }
        UNPROTECT(1);
    }
    UNPROTECT(1);

    (void)is_inside_block; // block nesting is now detected during traversal

    std::vector<OperatorPos> ops;
    std::vector<int> path;
    gatherOperatorsRecursive(expr, path, ops, false);

    if (_filter_operators)
    {
        ops.erase(std::remove_if(ops.begin(), ops.end(),
                                 [this](const OperatorPos &op)
                                 { return _enabled_operators.count(op.operator_id) == 0; }),
                  ops.end());
    }
    return ops;
}

void ASTHandler::gatherOperatorsRecursive(SEXP expr, std::vector<int> path,
                                          std::vector<OperatorPos> &ops, bool parent_is_block)
{
    if (TYPEOF(expr) != LANGSXP)
    {
        if (isMutableScalarConstant(expr))
        {
            SEXP scalar_replacement = makeScalarValueReplacement(expr);
            if (scalar_replacement != R_NilValue)
            {
                addNodeReplacement(ops, path, _start_line, _start_col, _end_line, _end_col,
                                   expr, scalar_replacement, _file_path, "value_42");
            }

            if (!isNAConstant(expr))
            {
                SEXP na_replacement = makeNAReplacement(expr);
                if (na_replacement != R_NilValue)
                {
                    addNodeReplacement(ops, path, _start_line, _start_col, _end_line, _end_col,
                                       expr, na_replacement, _file_path, "na_replace");
                }
            }
            else
            {
                // The constant is already an NA: swap it for each of the other
                // typed NAs (NA <-> NA_integer_ <-> NA_real_ <-> NA_character_).
                static const int na_types[] = {LGLSXP, INTSXP, REALSXP, STRSXP};
                for (int ty : na_types)
                {
                    if (ty == TYPEOF(expr))
                        continue;
                    SEXP na_swap = makeNAOfType(ty);
                    if (na_swap != R_NilValue)
                    {
                        addNodeReplacement(ops, path, _start_line, _start_col, _end_line, _end_col,
                                           expr, na_swap, _file_path, "na_type_swap");
                    }
                }
            }

            for (int delta : {1, -1})
            {
                SEXP shifted = makeOffByOne(expr, delta);
                if (shifted != R_NilValue)
                    addNodeReplacement(ops, path, _start_line, _start_col, _end_line, _end_col,
                                       expr, shifted, _file_path, "const_off_by_one");
            }

            if (isNonNABool(expr))
                addNodeReplacement(ops, path, _start_line, _start_col, _end_line, _end_col,
                                   expr, Rf_ScalarLogical(!LOGICAL(expr)[0]), _file_path, "bool_flip");

            if (TYPEOF(expr) == STRSXP && STRING_ELT(expr, 0) != NA_STRING &&
                CHAR(STRING_ELT(expr, 0))[0] != '\0')
                addNodeReplacement(ops, path, _start_line, _start_col, _end_line, _end_col,
                                   expr, Rf_mkString(""), _file_path, "string_empty");

            addNodeReplacement(ops, path, _start_line, _start_col, _end_line, _end_col,
                               expr, R_NilValue, _file_path, "const_null");
        }
        return;
    }

    int node_start_line = _start_line;
    int node_start_col = _start_col;
    int node_end_line = _end_line;
    int node_end_col = _end_col;

    SEXP node_srcref = Rf_getAttrib(expr, SYM.s_srcref);
    extractSrcrefBounds(node_srcref,
                        node_start_line,
                        node_start_col,
                        node_end_line,
                        node_end_col);

    SEXP fun = CAR(expr);

    struct SwapEntry
    {
        SEXP to;
        const char *id;
    };
    static const std::multimap<SEXP, SwapEntry> swaps = {
        {SYM.s_plus, {SYM.s_minus, "arith_swap"}},
        {SYM.s_minus, {SYM.s_plus, "arith_swap"}},
        {SYM.s_mul, {SYM.s_div, "arith_swap"}},
        {SYM.s_div, {SYM.s_mul, "arith_swap"}},
        {SYM.s_eq, {SYM.s_neq, "rel_swap"}},
        {SYM.s_neq, {SYM.s_eq, "rel_swap"}},
        {SYM.s_lt, {SYM.s_gt, "rel_swap"}},
        {SYM.s_gt, {SYM.s_lt, "rel_swap"}},
        {SYM.s_le, {SYM.s_ge, "rel_swap"}},
        {SYM.s_ge, {SYM.s_le, "rel_swap"}},
        {SYM.s_and, {SYM.s_or, "logic_swap"}},
        {SYM.s_or, {SYM.s_and, "logic_swap"}},
        {SYM.s_land, {SYM.s_lor, "logic_swap"}},
        {SYM.s_lor, {SYM.s_land, "logic_swap"}},
        {SYM.s_lt, {SYM.s_le, "rel_boundary"}},
        {SYM.s_le, {SYM.s_lt, "rel_boundary"}},
        {SYM.s_gt, {SYM.s_ge, "rel_boundary"}},
        {SYM.s_ge, {SYM.s_gt, "rel_boundary"}},
        {SYM.s_pow, {SYM.s_mul, "arith_extra"}},
        {SYM.s_mod, {SYM.s_intdiv, "arith_extra"}},
        {SYM.s_intdiv, {SYM.s_mod, "arith_extra"}},
        {SYM.s_break, {SYM.s_next, "loop_ctrl"}},
        {SYM.s_next, {SYM.s_break, "loop_ctrl"}},
        {SYM.s_super_assign, {SYM.s_assign, "super_assign"}},
        {SYM.s_land, {SYM.s_and, "scalar_vector_logic"}},
        {SYM.s_lor, {SYM.s_or, "scalar_vector_logic"}},
        {SYM.s_and, {SYM.s_land, "scalar_vector_logic"}},
        {SYM.s_or, {SYM.s_lor, "scalar_vector_logic"}},
        {SYM.s_subset2, {SYM.s_subset, "index_ops"}},
        {Rf_install("any"), {Rf_install("all"), "fun_swap"}},
        {Rf_install("all"), {Rf_install("any"), "fun_swap"}},
        {Rf_install("min"), {Rf_install("max"), "fun_swap"}},
        {Rf_install("max"), {Rf_install("min"), "fun_swap"}},
        {Rf_install("pmin"), {Rf_install("pmax"), "fun_swap"}},
        {Rf_install("pmax"), {Rf_install("pmin"), "fun_swap"}},
        {Rf_install("which.min"), {Rf_install("which.max"), "fun_swap"}},
        {Rf_install("which.max"), {Rf_install("which.min"), "fun_swap"}},
        {Rf_install("head"), {Rf_install("tail"), "fun_swap"}},
        {Rf_install("tail"), {Rf_install("head"), "fun_swap"}},
        {Rf_install("nrow"), {Rf_install("ncol"), "fun_swap"}},
        {Rf_install("ncol"), {Rf_install("nrow"), "fun_swap"}},
        {Rf_install("isTRUE"), {Rf_install("isFALSE"), "fun_swap"}},
        {Rf_install("isFALSE"), {Rf_install("isTRUE"), "fun_swap"}},
        {Rf_install("sub"), {Rf_install("gsub"), "fun_swap"}},
        {Rf_install("gsub"), {Rf_install("sub"), "fun_swap"}},
        {Rf_install("floor"), {Rf_install("ceiling"), "fun_swap_extra"}},
        {Rf_install("ceiling"), {Rf_install("floor"), "fun_swap_extra"}},
        {Rf_install("rownames"), {Rf_install("colnames"), "fun_swap_extra"}},
        {Rf_install("colnames"), {Rf_install("rownames"), "fun_swap_extra"}},
        {Rf_install("rowSums"), {Rf_install("colSums"), "fun_swap_extra"}},
        {Rf_install("colSums"), {Rf_install("rowSums"), "fun_swap_extra"}},
        {Rf_install("rowMeans"), {Rf_install("colMeans"), "fun_swap_extra"}},
        {Rf_install("colMeans"), {Rf_install("rowMeans"), "fun_swap_extra"}},
        {Rf_install("paste"), {Rf_install("paste0"), "fun_swap_extra"}},
        {Rf_install("paste0"), {Rf_install("paste"), "fun_swap_extra"}},
        {Rf_install("startsWith"), {Rf_install("endsWith"), "fun_swap_extra"}},
        {Rf_install("endsWith"), {Rf_install("startsWith"), "fun_swap_extra"}},
        {Rf_install("union"), {Rf_install("intersect"), "fun_swap_extra"}},
        {Rf_install("intersect"), {Rf_install("union"), "fun_swap_extra"}},
        {Rf_install("sapply"), {Rf_install("lapply"), "fun_swap_extra"}},
        {Rf_install("is.null"), {Rf_install("is.na"), "fun_swap_extra"}}};

    auto range = swaps.equal_range(fun);
    for (auto it = range.first; it != range.second; ++it)
    {
        ops.push_back({path, std::make_unique<SymbolSwapOperator>(fun, it->second.to),
                       node_start_line, node_start_col, node_end_line, node_end_col,
                       fun, _file_path, it->second.id});
    }

    if (isSymbol(fun, SYM.s_not) && CDR(expr) != R_NilValue)
    {
        SEXP arg = CADR(expr);
        addNodeReplacement(ops, path, node_start_line, node_start_col,
                           node_end_line, node_end_col, expr, arg, _file_path, "not_remove");
    }

    // Flip TRUE/FALSE argument defaults. Formals are a pairlist that mutation
    // paths cannot enter, so the whole formals list is replaced.
    if (isSymbol(fun, SYM.s_function) && CDR(expr) != R_NilValue && TYPEOF(CADR(expr)) == LISTSXP)
    {
        SEXP formals = CADR(expr);
        std::vector<int> formals_path = path;
        formals_path.push_back(0);
        int k = 0;
        for (SEXP f = formals; f != R_NilValue; f = CDR(f), ++k)
        {
            if (!isNonNABool(CAR(f)))
                continue;
            SEXP mutated = PROTECT(Rf_duplicate(formals));
            SEXP cell = mutated;
            for (int j = 0; j < k; ++j)
                cell = CDR(cell);
            SEXP flipped = PROTECT(Rf_ScalarLogical(!LOGICAL(CAR(f))[0]));
            SETCAR(cell, flipped);
            addNodeReplacement(ops, formals_path, node_start_line, node_start_col,
                               node_end_line, node_end_col, CAR(f), mutated, _file_path,
                               "bool_flip", flipped);
            UNPROTECT(2);
        }
    }

    // seq_len(n) -> 1:n, seq_along(x) -> 1:length(x)
    if ((isSymbol(fun, SYM.s_seq_len) || isSymbol(fun, SYM.s_seq_along)) &&
        CDR(expr) != R_NilValue && CDDR(expr) == R_NilValue)
    {
        SEXP upper = CADR(expr);
        if (isSymbol(fun, SYM.s_seq_along))
            upper = Rf_lang2(SYM.s_length, upper);
        PROTECT(upper);
        SEXP one = PROTECT(Rf_ScalarReal(1));
        SEXP range = Rf_lang3(SYM.s_colon, one, upper);
        UNPROTECT(2);
        addNodeReplacement(ops, path, node_start_line, node_start_col,
                           node_end_line, node_end_col, expr, range, _file_path, "seq_idiom");
    }

    // Drop a named argument so that its default applies.
    {
        int k = 0;
        for (SEXP a = CDR(expr); a != R_NilValue; a = CDR(a), ++k)
        {
            SEXP tag = TAG(a);
            if (tag == R_NilValue)
                continue;
            if (isSymbol(fun, SYM.s_subset) && tag == SYM.s_drop)
            {
                // x[i, j, drop = FALSE] -> x[i, j]; drop = TRUE is the default.
                if (isLogicalConstant(CAR(a), FALSE))
                {
                    SEXP dropped = PROTECT(dropArgument(expr, k));
                    addNodeReplacement(ops, path, node_start_line, node_start_col,
                                       node_end_line, node_end_col, expr,
                                       dropped, _file_path, "drop_idiom");
                    UNPROTECT(1);
                }
                continue;
            }
            static const SymbolSet droppable = makeSymbolSet(
                {"na.rm", "drop", "fixed", "perl", "exact", "decreasing",
                 "simplify", "sep", "collapse", "envir", "inherits"});
            if (droppable.count(tag))
            {
                SEXP shown = PROTECT(Rf_mkString("<default>"));
                SEXP dropped = PROTECT(dropArgument(expr, k));
                addNodeReplacement(ops, path, node_start_line, node_start_col,
                                   node_end_line, node_end_col, tag,
                                   dropped, _file_path, "named_arg_drop", shown);
                UNPROTECT(2);
            }
        }
    }

    // rev(x) -> x, as.numeric(x) -> x, ...
    static const SymbolSet unwrappable = makeSymbolSet(
        {"rev", "sort", "unique", "abs", "unname", "trimws", "tolower", "toupper",
         "na.omit", "as.integer", "as.numeric", "as.double", "as.character",
         "as.vector", "drop", "suppressWarnings", "suppressMessages", "invisible"});
    if (unwrappable.count(fun) && CDR(expr) != R_NilValue && TAG(CDR(expr)) == R_NilValue)
    {
        addNodeReplacement(ops, path, node_start_line, node_start_col,
                           node_end_line, node_end_col, expr, CADR(expr), _file_path, "call_unwrap");
    }

    // tryCatch(expr, ...) -> expr. Unlike stop() below, not gated on blocks:
    // deleting the statement also drops `expr`, unwrapping keeps it.
    if ((isSymbol(fun, SYM.s_trycatch) || isSymbol(fun, SYM.s_try) ||
         isSymbol(fun, SYM.s_with_handlers)) &&
        CDR(expr) != R_NilValue && (TAG(CDR(expr)) == R_NilValue || TAG(CDR(expr)) == SYM.s_expr))
    {
        addNodeReplacement(ops, path, node_start_line, node_start_col,
                           node_end_line, node_end_col, expr, CADR(expr), _file_path, "error_handling");
    }

    // stop(...) -> invisible(NULL), unless stmt_delete already removes it
    if ((isSymbol(fun, SYM.s_stop) || isSymbol(fun, SYM.s_stopifnot)) && !parent_is_block)
    {
        addNodeReplacement(ops, path, node_start_line, node_start_col,
                           node_end_line, node_end_col, expr,
                           Rf_lang2(SYM.s_invisible, R_NilValue), _file_path, "error_handling");
    }

    // max(a, b) <-> pmax(a, b), min <-> pmin, with at least two unnamed arguments
    {
        static const std::map<SEXP, SEXP> parallel = {
            {Rf_install("max"), Rf_install("pmax")}, {Rf_install("pmax"), Rf_install("max")},
            {Rf_install("min"), Rf_install("pmin")}, {Rf_install("pmin"), Rf_install("min")}};
        auto it = parallel.find(fun);
        if (it != parallel.end())
        {
            int unnamed = 0;
            for (SEXP a = CDR(expr); a != R_NilValue; a = CDR(a))
                if (TAG(a) == R_NilValue)
                    ++unnamed;
            if (unnamed >= 2)
                ops.push_back({path, std::make_unique<SymbolSwapOperator>(fun, it->second),
                               node_start_line, node_start_col, node_end_line, node_end_col,
                               fun, _file_path, "scalar_vector_fun"});
        }
    }

    // x[i] -> x[[i]], only for a single plain index
    if (isSymbol(fun, SYM.s_subset))
    {
        SEXP args = CDR(expr);
        if (args != R_NilValue && CDR(args) != R_NilValue && CDDR(args) == R_NilValue &&
            TAG(args) == R_NilValue && TAG(CDR(args)) == R_NilValue &&
            CADR(args) != R_MissingArg)
        {
            ops.push_back({path, std::make_unique<SymbolSwapOperator>(fun, SYM.s_subset2),
                           node_start_line, node_start_col, node_end_line, node_end_col,
                           fun, _file_path, "index_ops"});
        }
    }

    // Unary minus: -x -> x
    if (isSymbol(fun, SYM.s_minus) && CDR(expr) != R_NilValue && CDDR(expr) == R_NilValue)
    {
        addNodeReplacement(ops, path, node_start_line, node_start_col,
                           node_end_line, node_end_col, expr, CADR(expr), _file_path, "arith_extra");
    }

    // a && b -> a, a && b -> b
    if ((isSymbol(fun, SYM.s_land) || isSymbol(fun, SYM.s_lor)) &&
        CDR(expr) != R_NilValue && CDDR(expr) != R_NilValue)
    {
        for (SEXP operand : {CADR(expr), CADDR(expr)})
            addNodeReplacement(ops, path, node_start_line, node_start_col,
                               node_end_line, node_end_col, expr, operand, _file_path, "and_or_operand");
    }

    if (isSymbol(fun, SYM.s_if) && CDR(expr) != R_NilValue)
    {
        SEXP condition = CADR(expr);
        std::vector<int> condition_path = path;
        condition_path.push_back(0);
        for (int value : {TRUE, FALSE})
        {
            if (!isLogicalConstant(condition, value))
                addNodeReplacement(ops, condition_path, node_start_line, node_start_col,
                                   node_end_line, node_end_col, condition,
                                   Rf_ScalarLogical(value), _file_path, "cond_force");
        }
    }

    if ((isSymbol(fun, SYM.s_if) || isSymbol(fun, SYM.s_while)) && CDR(expr) != R_NilValue)
    {
        SEXP condition = CADR(expr);
        if (!isCallTo(condition, SYM.s_not))
        {
            std::vector<int> condition_path = path;
            condition_path.push_back(0);
            addNodeReplacement(ops, condition_path, node_start_line, node_start_col,
                               node_end_line, node_end_col, condition,
                               makeNotCall(condition), _file_path, "cond_negate");
        }
    }

    if (isAssignmentSymbol(fun) && CDR(expr) != R_NilValue && CDDR(expr) != R_NilValue)
    {
        SEXP rhs = CADDR(expr);
        if (!isFortyTwo(rhs))
        {
            std::vector<int> rhs_path = path;
            rhs_path.push_back(1);
            addNodeReplacement(ops, rhs_path, node_start_line, node_start_col,
                               node_end_line, node_end_col, rhs, makeFortyTwo(), _file_path, "value_42");
        }
    }

    if (isSymbol(fun, SYM.s_return) && CDR(expr) != R_NilValue)
    {
        SEXP value = CADR(expr);
        if (!isScalarConstant(value))
        {
            std::vector<int> return_value_path = path;
            return_value_path.push_back(0);
            addNodeReplacement(ops, return_value_path, node_start_line, node_start_col,
                               node_end_line, node_end_col, value, R_NilValue, _file_path, "return_null");
        }
    }

    if (isOrdinaryFunctionCall(expr))
    {
        addNodeReplacement(ops, path, node_start_line, node_start_col,
                           node_end_line, node_end_col, expr, makeFortyTwo(), _file_path, "value_42");
    }

    // A `{ ... }` block exposes each of its direct children as a statement that
    // can be deleted. Detecting this here, rather than via a single whole-tree
    // flag, means deletion is offered for real block statements at any nesting
    // depth, and never for sub-expressions such as an operand of `+`.
    const bool this_is_block =
        (TYPEOF(fun) == SYMSXP && fun == SYM.s_lbrace);

    // R does not attach a "srcref" to individual statement language objects.
    // For a `{ }` block it instead stores a *list* of per-statement srcrefs as
    // the block's own "srcref" attribute, in statement order. That list is the
    // only precise source location for a statement (a statement's own bounds,
    // when present at all, are frequently as wide as the surrounding block), so
    // we index into it rather than reading the child's attribute.
    SEXP block_srcrefs = R_NilValue;
    if (this_is_block)
    {
        SEXP sr = Rf_getAttrib(expr, SYM.s_srcref);
        if (TYPEOF(sr) == VECSXP)
            block_srcrefs = sr;
    }

    int idx = 0;
    for (SEXP next = CDR(expr); next != R_NilValue; next = CDR(next), ++idx)
    {
        SEXP child = CAR(next);
        std::vector<int> child_path = path;
        child_path.push_back(idx);

        if (this_is_block && isDeletableStatement(child))
        {
            // Fall back to the block's (inherited) bounds when keep.source did
            // not produce a per-statement srcref list.
            int del_start_line = node_start_line;
            int del_start_col = node_start_col;
            int del_end_line = node_end_line;
            int del_end_col = node_end_col;
            // The block's srcref list is offset by one: element 0 describes the
            // `{` itself, so statement `idx` (0-based over CDR) is element idx+1.
            if (block_srcrefs != R_NilValue && idx + 1 < Rf_length(block_srcrefs))
                extractSrcrefBounds(VECTOR_ELT(block_srcrefs, idx + 1),
                                    del_start_line, del_start_col,
                                    del_end_line, del_end_col);

            SEXP del_symbol = (TYPEOF(child) == LANGSXP) ? CAR(child) : child;
            auto del = std::make_unique<DeleteOperator>(child);
            ops.push_back({child_path, std::move(del), del_start_line, del_start_col,
                           del_end_line, del_end_col, del_symbol, _file_path, "stmt_delete"});
        }

        if (isSymbol(fun, SYM.s_return) && idx == 0 && isScalarConstant(child))
            continue;

        gatherOperatorsRecursive(child, child_path, ops, this_is_block);
    }
}
