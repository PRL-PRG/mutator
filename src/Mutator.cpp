// Mutator.cpp
#include "Mutator.h"
#include "DeleteOperator.h"
#include "NodeReplacementOperator.h"
#include "SymbolSwapOperator.h"

static SEXP asStringOrNA(SEXP x)
{
    if (x == R_NilValue)
        return Rf_ScalarString(NA_STRING);

    if (TYPEOF(x) == SYMSXP)
        return Rf_mkString(CHAR(PRINTNAME(x)));

    if (TYPEOF(x) == CHARSXP && x != NA_STRING)
        return Rf_ScalarString(x);

    // Non-NA character: show the string content. An NA string (NA_character_)
    // falls through to deparse() below so it is labelled "NA_character_" rather
    // than an R-level NA (which would render as "<deleted>").
    if (TYPEOF(x) == STRSXP && Rf_length(x) > 0 && STRING_ELT(x, 0) != NA_STRING)
        return Rf_ScalarString(STRING_ELT(x, 0));

    int error = 0;
    SEXP quoted = PROTECT(Rf_lang2(Rf_install("quote"), x));
    SEXP deparse_call = PROTECT(Rf_lang2(Rf_install("deparse"), quoted));
    SEXP text = R_tryEval(deparse_call, R_BaseEnv, &error);
    if (error == 0 && TYPEOF(text) == STRSXP && Rf_length(text) > 0)
    {
        PROTECT(text);
        SEXP out = PROTECT(Rf_ScalarString(STRING_ELT(text, 0)));
        UNPROTECT(4);
        return out;
    }
    UNPROTECT(2);

    return Rf_ScalarString(NA_STRING);
}

static SEXP buildMutationInfo(const OperatorPos &pos, SEXP new_symbol)
{
    int n_protect = 0;

    SEXP info = PROTECT(Rf_allocVector(VECSXP, 8));
    ++n_protect;
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 8));
    ++n_protect;

    SET_STRING_ELT(names, 0, Rf_mkChar("start_line"));
    SET_STRING_ELT(names, 1, Rf_mkChar("start_col"));
    SET_STRING_ELT(names, 2, Rf_mkChar("end_line"));
    SET_STRING_ELT(names, 3, Rf_mkChar("end_col"));
    SET_STRING_ELT(names, 4, Rf_mkChar("original_symbol"));
    SET_STRING_ELT(names, 5, Rf_mkChar("new_symbol"));
    SET_STRING_ELT(names, 6, Rf_mkChar("file_path"));
    SET_STRING_ELT(names, 7, Rf_mkChar("operator_id"));

    SEXP v = PROTECT(Rf_ScalarInteger(pos.start_line));
    ++n_protect;
    SET_VECTOR_ELT(info, 0, v);
    UNPROTECT(1);
    --n_protect;

    v = PROTECT(Rf_ScalarInteger(pos.start_col));
    ++n_protect;
    SET_VECTOR_ELT(info, 1, v);
    UNPROTECT(1);
    --n_protect;

    v = PROTECT(Rf_ScalarInteger(pos.end_line));
    ++n_protect;
    SET_VECTOR_ELT(info, 2, v);
    UNPROTECT(1);
    --n_protect;

    v = PROTECT(Rf_ScalarInteger(pos.end_col));
    ++n_protect;
    SET_VECTOR_ELT(info, 3, v);
    UNPROTECT(1);
    --n_protect;

    v = PROTECT(asStringOrNA(pos.original_symbol));
    ++n_protect;
    SET_VECTOR_ELT(info, 4, v);
    UNPROTECT(1);
    --n_protect;

    v = PROTECT(asStringOrNA(new_symbol));
    ++n_protect;
    SET_VECTOR_ELT(info, 5, v);
    UNPROTECT(1);
    --n_protect;

    if (!pos.file_path.empty())
        v = PROTECT(Rf_mkString(pos.file_path.c_str()));
    else
        v = PROTECT(Rf_ScalarString(NA_STRING));
    ++n_protect;
    SET_VECTOR_ELT(info, 6, v);
    UNPROTECT(1);
    --n_protect;

    if (!pos.operator_id.empty())
        v = PROTECT(Rf_mkString(pos.operator_id.c_str()));
    else
        v = PROTECT(Rf_ScalarString(NA_STRING));
    ++n_protect;
    SET_VECTOR_ELT(info, 7, v);
    UNPROTECT(1);
    --n_protect;

    Rf_setAttrib(info, R_NamesSymbol, names);

    UNPROTECT(n_protect);
    return info;
}

Mutation Mutator::applyMutation(SEXP expr, const std::vector<OperatorPos> &ops, int which)
{
    if (which < 0 || which >= static_cast<int>(ops.size()))
        return {};

    if (dynamic_cast<DeleteOperator *>(ops[which].op.get()))
        return applyDeleteMutation(expr, ops, which);
    if (dynamic_cast<NodeReplacementOperator *>(ops[which].op.get()))
        return applyNodeReplacementMutation(expr, ops, which);
    return applyFlipMutation(expr, ops, which);
}

Mutation Mutator::applyFlipMutation(SEXP expr, const std::vector<OperatorPos> &ops, int which)
{
    SEXP mutated = PROTECT(Rf_duplicate(expr)); // [0]

    const OperatorPos &pos = ops[which];
    SEXP node = mutated;
    for (int idx : pos.path)
    {
        if (idx < 0 || node == R_NilValue || TYPEOF(node) != LANGSXP)
        {
            UNPROTECT(1);
            return {};
        }

        SEXP nxt = CDR(node);
        for (int j = 0; j < idx; ++j)
        {
            if (nxt == R_NilValue)
            {
                UNPROTECT(1);
                return {};
            }
            nxt = CDR(nxt);
        }

        if (nxt == R_NilValue)
        {
            UNPROTECT(1);
            return {};
        }
        node = CAR(nxt);
    }

    if (node == R_NilValue || TYPEOF(node) != LANGSXP)
    {
        UNPROTECT(1);
        return {};
    }

    // perform the operator‑specific flip
    const auto *repl = dynamic_cast<const SymbolSwapOperator *>(pos.op.get());
    if (repl == nullptr)
    {
        UNPROTECT(1);
        return {};
    }
    repl->flip(node);

    SEXP info = PROTECT(buildMutationInfo(pos, CAR(node))); // [1]
    UNPROTECT(2);
    return {mutated, info, true};
}

Mutation Mutator::applyDeleteMutation(SEXP expr, const std::vector<OperatorPos> &ops, int which)
{
    SEXP dup = PROTECT(Rf_duplicate(expr)); // [0]
    const auto &pos = ops[which];
    const auto &path = pos.path;

    if (path.empty())
    {
        UNPROTECT(1);
        return {};
    }

    // navigate to parent SEXP that owns the element to delete
    SEXP parent = dup;
    for (size_t i = 0; i + 1 < path.size(); ++i)
    {
        int idx = path[i];
        if (idx < 0 || parent == R_NilValue || TYPEOF(parent) != LANGSXP)
        {
            UNPROTECT(1);
            return {};
        }
        SEXP iter = CDR(parent);
        for (int j = 0; j < idx; ++j)
        {
            if (iter == R_NilValue)
            {
                UNPROTECT(1);
                return {};
            }
            iter = CDR(iter);
        }
        if (iter == R_NilValue)
        {
            UNPROTECT(1);
            return {};
        }
        parent = CAR(iter);
    }

    int delIdx = path.back();
    if (delIdx < 0 || parent == R_NilValue || TYPEOF(parent) != LANGSXP)
    {
        UNPROTECT(1);
        return {};
    }

    SEXP args = CDR(parent);
    if (args == R_NilValue)
    {
        UNPROTECT(1);
        return {};
    }

    if (delIdx == 0)
    {
        SETCDR(parent, CDR(args));
    }
    else
    {
        // Move to the argument cons cell immediately before the one to remove.
        SEXP prev = args;
        for (int i = 0; i < delIdx - 1; ++i)
        {
            if (prev == R_NilValue)
            {
                UNPROTECT(1);
                return {};
            }
            prev = CDR(prev);
        }

        if (prev == R_NilValue || CDR(prev) == R_NilValue)
        {
            UNPROTECT(1);
            return {};
        }
        SETCDR(prev, CDDR(prev)); // skip over the element to delete
    }

    SEXP info = PROTECT(buildMutationInfo(pos, R_NilValue)); // [1]
    UNPROTECT(2);
    return {dup, info, true};
}

Mutation Mutator::applyNodeReplacementMutation(SEXP expr, const std::vector<OperatorPos> &ops, int which)
{
    if (which < 0 || which >= static_cast<int>(ops.size()))
        return {};

    const OperatorPos &pos = ops[which];
    const auto *repl = dynamic_cast<const NodeReplacementOperator *>(pos.op.get());
    if (repl == nullptr)
        return {};

    SEXP dup = PROTECT(Rf_duplicate(expr)); // [0]
    SEXP replacement = PROTECT(repl->makeReplacement()); // [1]

    if (pos.path.empty())
    {
        SEXP info = PROTECT(buildMutationInfo(pos, repl->infoReplacement())); // [2]
        UNPROTECT(3);
        return {replacement, info, true};
    }

    SEXP parent = dup;
    for (size_t i = 0; i + 1 < pos.path.size(); ++i)
    {
        int idx = pos.path[i];
        if (idx < 0 || parent == R_NilValue || TYPEOF(parent) != LANGSXP)
        {
            UNPROTECT(2);
            return {};
        }

        SEXP iter = CDR(parent);
        for (int j = 0; j < idx; ++j)
        {
            if (iter == R_NilValue)
            {
                UNPROTECT(2);
                return {};
            }
            iter = CDR(iter);
        }
        if (iter == R_NilValue)
        {
            UNPROTECT(2);
            return {};
        }
        parent = CAR(iter);
    }

    int target_idx = pos.path.back();
    if (target_idx < 0 || parent == R_NilValue || TYPEOF(parent) != LANGSXP)
    {
        UNPROTECT(2);
        return {};
    }

    SEXP iter = CDR(parent);
    for (int j = 0; j < target_idx; ++j)
    {
        if (iter == R_NilValue)
        {
            UNPROTECT(2);
            return {};
        }
        iter = CDR(iter);
    }
    if (iter == R_NilValue)
    {
        UNPROTECT(2);
        return {};
    }

    SETCAR(iter, replacement);

    SEXP info = PROTECT(buildMutationInfo(pos, repl->infoReplacement())); // [2]

    UNPROTECT(3);
    return {dup, info, true};
}
