// OperatorPos.h
#ifndef OPERATOR_POS_H
#define OPERATOR_POS_H

#include <vector>
#include <memory>
#include <string>
#include "Operator.h"

// Struct to Hold Operator Position and Pointer
struct OperatorPos
{
    std::vector<int> path;        // Path indices from root to reach this operator
    std::unique_ptr<Operator> op; // Smart pointer to the Operator object

    int start_line;
    int start_col;
    int end_line;
    int end_col;
    // Possibly store the original operator symbol too, if you want
    SEXP original_symbol;
    std::string file_path;
    std::string operator_id; // e.g. "rel_swap", "stmt_delete"

    // Constructor for convenience
    OperatorPos(const std::vector<int> &p, std::unique_ptr<Operator> operator_ptr, int start_line,
                int start_col, int end_line, int end_col, SEXP original_symbol, const std::string &file_path = "",
                const std::string &operator_id = "")
        : path(p), op(std::move(operator_ptr)), start_line(start_line), start_col(start_col),
          end_line(end_line), end_col(end_col), original_symbol(original_symbol), file_path(file_path),
          operator_id(operator_id)
    {
    }
};

#endif // OPERATOR_POS_H