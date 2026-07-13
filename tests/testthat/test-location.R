test_that("imputed_function_slot identifies function-definition assignments", {
    imputed_function_slot <- resolve_mutator_fn("imputed_function_slot")

    expect_equal(imputed_function_slot(quote(f <- function(x) x)), 3L)
    expect_equal(imputed_function_slot(str2lang("f = function() 1")), 3L)
    expect_equal(imputed_function_slot(quote(f <<- function(x) x + 1)), 3L)
    expect_null(imputed_function_slot(quote(x <- 1)))
    expect_null(imputed_function_slot(quote(f(1))))
    expect_null(imputed_function_slot(quote(x)))
})

test_that("is_transparent_brace recognises injected transparent braces", {
    is_transparent_brace <- resolve_mutator_fn("is_transparent_brace")

    sr <- structure(c(1L, 1L, 1L, 5L, 1L, 5L, 1L, 1L), class = "srcref")
    sr2 <- structure(c(2L, 1L, 2L, 5L, 1L, 5L, 2L, 2L), class = "srcref")

    transparent <- quote({
        x + 1
    })
    attr(transparent, "srcref") <- list(sr, sr)
    expect_true(is_transparent_brace(transparent))

    # Differing entries -> not a transparent (source-invisible) wrapper.
    nontransparent <- transparent
    attr(nontransparent, "srcref") <- list(sr, sr2)
    expect_false(is_transparent_brace(nontransparent))

    # A brace with no srcref, and non-brace calls, are not transparent braces.
    bare <- quote({
        x + 1
    })
    attr(bare, "srcref") <- NULL
    expect_false(is_transparent_brace(bare))
    expect_false(is_transparent_brace(quote(x + 1)))
    expect_false(is_transparent_brace(quote(x)))
})

test_that("mutation_diff_path locates the changed AST node", {
    mutation_diff_path <- resolve_mutator_fn("mutation_diff_path")

    # Identical expressions -> NULL.
    expect_null(mutation_diff_path(quote(a + b), quote(a + b)))
    # Operator symbol changed -> path to the callee slot.
    expect_equal(mutation_diff_path(quote(a + b), quote(a - b)), 1L)
    # Operand changed -> path to that argument slot.
    expect_equal(mutation_diff_path(quote(a + b), quote(c + b)), 2L)
    # Nested change -> multi-step path.
    expect_equal(mutation_diff_path(quote(f(a + b)), quote(f(a - b))), c(2L, 1L))
    # Structural difference (different lengths) -> differs at this node.
    expect_equal(mutation_diff_path(quote(f(a, b)), quote(f(a))), integer(0))
})

test_that("location refinement sharpens operator mutants when imputesrcref is present", {
    skip_if_not_installed("imputesrcref")
    mutate_file <- resolve_mutator_fn("mutate_file")

    src <- tempfile(fileext = ".R")
    out_dir <- tempfile("mutations_")
    on.exit(unlink(c(src, out_dir), recursive = TRUE), add = TRUE)

    # The `+` lives in a call-argument position (which imputesrcref wraps) on a
    # line distinct from the function definition, so coarse bounds would span
    # the whole function.
    writeLines(c(
        "add <- function(a, b) {",
        "  z <- 0",
        "  h(a + b)",
        "}"
    ), src)

    mutants <- mutate_file(src, out_dir = out_dir, max_line_deletions = 0)

    plus <- Filter(function(m) grepl("'\\+'", m$info), mutants)
    expect_gt(length(plus), 0)

    # The reported range is pinned to the single line carrying `a + b` (line 3),
    # not the whole 1-4 function span.
    for (m in plus) {
        expect_match(m$info, "Range: 3:[0-9]+-3:[0-9]+")
        expect_equal(m$loc$start_line, 3L)
        expect_equal(m$loc$end_line, 3L)
    }

    # Mutant files must never contain injected transparent braces: the imputed
    # AST is only a location oracle, never deparsed into output.
    texts <- vapply(mutants, function(m) {
        paste(readLines(m$path, warn = FALSE), collapse = "\n")
    }, character(1))
    expect_true(any(grepl("h\\(a - b\\)", texts)))
    for (m in mutants) {
        txt <- paste(readLines(m$path, warn = FALSE), collapse = "\n")
        expect_false(grepl("\\{\\s*\\n\\s*a [+-] b\\s*\\n\\s*\\}", txt))
    }
})

test_that("statement-srcref fallback sharpens locations without imputesrcref", {
    refine <- mutator:::refine_mutation_info

    src <- tempfile(fileext = ".R")
    on.exit(unlink(src), add = TRUE)
    writeLines(c(
        "f <- function(x) {",        # 1
        "  mask <- is.na(x)",        # 2
        "  ret <- rep(NA, length(x))",  # 3
        "  ret",                     # 4
        "}"                          # 5
    ), src)
    parsed <- parse(src, keep.source = TRUE)

    # Mutate the `NA` constant inside `rep(NA, length(x))` on line 3.
    m <- parsed
    m[[1]][[3]][[3]][[3]][[3]][[2]] <- quote(NA_integer_)

    # Coarse bounds span the whole function (1-4) as the engine would report.
    coarse <- list(file_path = src, start_line = 1L, start_col = 1L,
                   end_line = 4L, end_col = 1L)

    # With no imputed tree the statement fallback still pins it to line 3.
    refined <- refine(coarse, parsed, NULL, m)
    expect_equal(refined$start_line, 3L)
    expect_equal(refined$end_line, 3L)
})

test_that("nearest_statement_srcref returns the enclosing block statement srcref", {
    nss <- mutator:::nearest_statement_srcref

    src <- tempfile(fileext = ".R")
    on.exit(unlink(src), add = TRUE)
    writeLines(c(
        "g <- function(x) {",  # 1
        "  a <- x + 1",        # 2
        "  a",                 # 3
        "}"                    # 4
    ), src)
    parsed <- parse(src, keep.source = TRUE)

    # Path to the `+` inside `a <- x + 1`: [[3]] fn-def, [[3]] body block,
    # [[2]] first statement, [[3]] RHS `x + 1`, [[1]] the `+`.
    sr <- nss(parsed[[1]], c(3L, 3L, 2L, 3L, 1L))
    expect_false(is.null(sr))
    expect_equal(as.integer(sr)[1], 2L)   # statement is on line 2
    expect_equal(as.integer(sr)[3], 2L)

    # A path crossing no block yields NULL (nothing to sharpen).
    expect_null(nss(quote(a + b), c(1L)))
})
