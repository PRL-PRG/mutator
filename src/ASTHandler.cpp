// ASTHandler.cpp

#include <map>
#include <functional>
#include <iostream>
#include <Rversion.h>
#include "ASTHandler.h"
#include "PlusOperator.h"
#include "MinusOperator.h"
#include "DivideOperator.h"
#include "MultiplyOperator.h"
#include "EqualOperator.h"
#include "NotEqualOperator.h"
#include "LessThanOperator.h"
#include "MoreThanOperator.h"
#include "LessThanOrEqualOperator.h"
#include "MoreThanOrEqualOperator.h"
#include "AndOperator.h"
#include "OrOperator.h"
#include "LogicalOrOperator.h"
#include "LogicalAndOperator.h"
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

// # nocov start (only reached from the disabled value-replacement family)
static bool isAssignmentSymbol(SEXP fun)
{
    return isSymbol(fun, SYM.s_assign) ||
           isSymbol(fun, SYM.s_eq_assign) ||
           isSymbol(fun, SYM.s_super_assign);
}
// # nocov end

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

// # nocov start (only reached from the disabled value-replacement family)
static bool isNumericScalarConstant(SEXP x)
{
    return (TYPEOF(x) == INTSXP || TYPEOF(x) == REALSXP) && Rf_length(x) == 1;
}
// # nocov end

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

// # nocov start (kEnableValueReplacements is disabled; see below)
static bool isFortyTwo(SEXP x)
{
    if (!isNumericScalarConstant(x))
        return false;
    if (TYPEOF(x) == INTSXP)
        return INTEGER(x)[0] != NA_INTEGER && INTEGER(x)[0] == 42;
    return !ISNA(REAL(x)[0]) && !ISNAN(REAL(x)[0]) && REAL(x)[0] == 42.0;
}
// # nocov end

// The constant-value replacements, namely numeric `0 -> 42` and `nonzero -> 0`
// (makeScalarValueReplacement), plus assignment-RHS `-> 42` and ordinary-call
// `-> 42`, generate many low-signal / near-equivalent mutants and many trivial
// type-error kills, so they are disabled for now. The code is kept intact; flip
// this to `true` to re-enable the whole family.
static constexpr bool kEnableValueReplacements = false;

static SEXP makeScalarValueReplacement(SEXP x)
{
    if (!kEnableValueReplacements)
        return R_NilValue;

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

// # nocov start (only reached from the disabled value-replacement family)
static SEXP makeFortyTwo()
{
    return Rf_ScalarReal(42.0);
}
// # nocov end

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

static SEXP makeNotCall(SEXP expr)
{
    return Rf_lang2(SYM.s_not, Rf_duplicate(expr));
}

// # nocov start (only reached from the disabled value-replacement family)
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
// # nocov end

static void addNodeReplacement(std::vector<OperatorPos> &ops,
                               const std::vector<int> &path,
                               int start_line,
                               int start_col,
                               int end_line,
                               int end_col,
                               SEXP original,
                               SEXP replacement,
                               const std::string &file_path)
{
    SEXP protected_replacement = PROTECT(replacement);
    ops.push_back({path,
                   std::make_unique<NodeReplacementOperator>(original, protected_replacement),
                   start_line,
                   start_col,
                   end_line,
                   end_col,
                   original,
                   file_path});
    UNPROTECT(1);
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

    SEXP srcfile = Rf_getAttrib(src_ref, Rf_install("srcfile"));
    if (srcfile != R_NilValue)
    {
        SEXP filename = Rf_getAttrib(srcfile, SYM.s_filename);
        if (TYPEOF(filename) == STRSXP && LENGTH(filename) > 0)
        {
            _file_path = CHAR(STRING_ELT(filename, 0));
        }
        else if (TYPEOF(srcfile) == ENVSXP)
        {
            SEXP env_name = getVarFromFrame(srcfile, SYM.s_filename);
            if (env_name != R_UnboundValue && TYPEOF(env_name) == STRSXP && LENGTH(env_name) > 0)
            {
                _file_path = CHAR(STRING_ELT(env_name, 0));
            }
        }
    }

    (void)is_inside_block; // block nesting is now detected during traversal

    std::vector<OperatorPos> ops;
    std::vector<int> path;
    gatherOperatorsRecursive(expr, path, ops);
    return ops;
}

void ASTHandler::gatherOperatorsRecursive(SEXP expr, std::vector<int> path,
                                          std::vector<OperatorPos> &ops)
{
    if (TYPEOF(expr) != LANGSXP)
    {
        if (isMutableScalarConstant(expr))
        {
            SEXP scalar_replacement = makeScalarValueReplacement(expr);
            if (scalar_replacement != R_NilValue)
            {
                addNodeReplacement(ops, path, _start_line, _start_col, _end_line, _end_col,
                                   expr, scalar_replacement, _file_path);
            }

            if (!isNAConstant(expr))
            {
                SEXP na_replacement = makeNAReplacement(expr);
                if (na_replacement != R_NilValue)
                {
                    addNodeReplacement(ops, path, _start_line, _start_col, _end_line, _end_col,
                                       expr, na_replacement, _file_path);
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
                                           expr, na_swap, _file_path);
                    }
                }
            }

            addNodeReplacement(ops, path, _start_line, _start_col, _end_line, _end_col,
                               expr, R_NilValue, _file_path);
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

    /* operator map – keys are the cached symbols */
    static const std::map<SEXP, std::function<std::unique_ptr<Operator>()>> op_map = {
        {SYM.s_plus, []
         { return std::make_unique<PlusOperator>(); }},
        {SYM.s_minus, []
         { return std::make_unique<MinusOperator>(); }},
        {SYM.s_mul, []
         { return std::make_unique<MultiplyOperator>(); }},
        {SYM.s_div, []
         { return std::make_unique<DivideOperator>(); }},
        {SYM.s_eq, []
         { return std::make_unique<EqualOperator>(); }},
        {SYM.s_neq, []
         { return std::make_unique<NotEqualOperator>(); }},
        {SYM.s_lt, []
         { return std::make_unique<LessThanOperator>(); }},
        {SYM.s_gt, []
         { return std::make_unique<MoreThanOperator>(); }},
        {SYM.s_le, []
         { return std::make_unique<LessThanOrEqualOperator>(); }},
        {SYM.s_ge, []
         { return std::make_unique<MoreThanOrEqualOperator>(); }},
        {SYM.s_and, []
         { return std::make_unique<AndOperator>(); }},
        {SYM.s_or, []
         { return std::make_unique<OrOperator>(); }},
        {SYM.s_land, []
         { return std::make_unique<LogicalAndOperator>(); }},
        {SYM.s_lor, []
         { return std::make_unique<LogicalOrOperator>(); }}};

    if (auto it = op_map.find(fun); it != op_map.end())
    {
        auto op = it->second();
        ops.push_back({path, std::move(op), node_start_line, node_start_col,
                       node_end_line, node_end_col, fun, _file_path});
    }

    if (isSymbol(fun, SYM.s_not) && CDR(expr) != R_NilValue)
    {
        SEXP arg = CADR(expr);
        addNodeReplacement(ops, path, node_start_line, node_start_col,
                           node_end_line, node_end_col, expr, arg, _file_path);
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
                               makeNotCall(condition), _file_path);
        }
    }

    if (kEnableValueReplacements && isAssignmentSymbol(fun) && CDR(expr) != R_NilValue && CDDR(expr) != R_NilValue)
    {
        SEXP rhs = CADDR(expr);
        if (!isFortyTwo(rhs))
        {
            std::vector<int> rhs_path = path;
            rhs_path.push_back(1);
            addNodeReplacement(ops, rhs_path, node_start_line, node_start_col,
                               node_end_line, node_end_col, rhs, makeFortyTwo(), _file_path);
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
                               node_end_line, node_end_col, value, R_NilValue, _file_path);
        }
    }

    if (kEnableValueReplacements && isOrdinaryFunctionCall(expr))
    {
        addNodeReplacement(ops, path, node_start_line, node_start_col,
                           node_end_line, node_end_col, expr, makeFortyTwo(), _file_path);
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
                           del_end_line, del_end_col, del_symbol, _file_path});
        }

        if (isSymbol(fun, SYM.s_return) && idx == 0 && isScalarConstant(child))
            continue;

        gatherOperatorsRecursive(child, child_path, ops);
    }
}
