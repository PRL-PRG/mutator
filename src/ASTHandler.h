// ASTHandler.h
#ifndef AST_HANDLER_H
#define AST_HANDLER_H

#include <string>
#include <vector>
#include <memory>
#include <unordered_set>
#include "OperatorPos.h"
#include <R.h>
#include <Rinternals.h>

// Class to Handle AST Traversal and Operator Gathering
class ASTHandler
{
public:
    ASTHandler() = default;
    ~ASTHandler() = default;

    // Gather all operators in the AST
    std::vector<OperatorPos> gatherOperators(SEXP expr, SEXP src_ref, bool is_inside_block);

    // Keep only operators with these ids. Without a call, all are kept.
    void setEnabledOperators(const std::unordered_set<std::string> &ids)
    {
        _enabled_operators = ids;
        _filter_operators = true;
    }

private:
    int _start_line;
    int _start_col;
    int _end_line;
    int _end_col;
    std::string _file_path;
    std::unordered_set<std::string> _enabled_operators;
    bool _filter_operators = false;
    bool _parent_is_block = false; // set for the node being visited

    // Recursive helper function. Block nesting is tracked intrinsically during
    // traversal: a node is a deletable statement iff its parent is a `{ }` block.
    void gatherOperatorsRecursive(SEXP expr, std::vector<int> path, std::vector<OperatorPos> &ops);

    static bool isDeletableStatement(SEXP stmt);
};

#endif // AST_HANDLER_H