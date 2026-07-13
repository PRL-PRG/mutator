test_that("ignore_directive_ranges parses # mutator:ignore directives", {
    ignore_directive_ranges <- resolve_mutator_fn("ignore_directive_ranges")

    # No directives -> not whole-file, no ranges.
    none <- ignore_directive_ranges(c("a <- 1", "b <- 2"))
    expect_false(none$whole_file)
    expect_equal(none$ranges, list())

    # Empty input is handled.
    empty <- ignore_directive_ranges(character())
    expect_false(empty$whole_file)
    expect_equal(empty$ranges, list())

    # A start/end pair yields the inclusive region.
    region <- ignore_directive_ranges(c(
        "x <- 1",
        "# mutator:ignore-start",
        "y <- 2",
        "z <- 3",
        "# mutator:ignore-end",
        "w <- 4"
    ))
    expect_false(region$whole_file)
    expect_equal(region$ranges, list(c(2L, 5L)))

    # An unmatched -start runs through the end of the file.
    open <- ignore_directive_ranges(c("a <- 1", "# mutator:ignore-start", "b <- 2"))
    expect_equal(open$ranges, list(c(2L, 3L)))

    # ignore-file flips whole_file regardless of position.
    wf <- ignore_directive_ranges(c("a <- 1", "# mutator:ignore-file", "b <- 2"))
    expect_true(wf$whole_file)

    # `-start` is not mistaken for `-file` (prefix-disambiguation).
    expect_false(ignore_directive_ranges("# mutator:ignore-start")$whole_file)

    # A bare `# mutator:ignore` is NOT a recognised directive (no single-line form).
    bare <- ignore_directive_ranges(c("x <- 1 # mutator:ignore", "y <- 2"))
    expect_false(bare$whole_file)
    expect_equal(bare$ranges, list())
})

test_that("ignore_directive_ranges honours covr's # nocov annotations", {
    ignore_directive_ranges <- resolve_mutator_fn("ignore_directive_ranges")

    # covr `# nocov start` / `# nocov end` delimit a region (inclusive).
    region <- ignore_directive_ranges(c(
        "x <- 1",
        "# nocov start",
        "y <- 2",
        "# nocov end",
        "z <- 3"
    ))
    expect_equal(region$ranges, list(c(2L, 4L)))

    # A bare `# nocov` excludes just its own line, including as a trailing comment.
    single <- ignore_directive_ranges(c(
        "a <- 1",
        "stop('unreachable') # nocov",
        "b <- 2"
    ))
    expect_equal(single$ranges, list(c(2L, 2L)))

    # An unmatched `# nocov start` runs to end of file.
    open <- ignore_directive_ranges(c("a <- 1", "# nocov start", "b <- 2"))
    expect_equal(open$ranges, list(c(2L, 3L)))

    # `# nocov start` is treated as a region start, not a single-line exclusion.
    expect_equal(
        ignore_directive_ranges(c("# nocov start", "x <- 1", "# nocov end"))$ranges,
        list(c(1L, 3L))
    )

    # mutator and covr markers can both appear; both regions are captured.
    mixed <- ignore_directive_ranges(c(
        "# mutator:ignore-start", "a <- 1", "# mutator:ignore-end",  # 1-3
        "b <- 2",                                                     # 4
        "c <- 3 # nocov"                                              # 5
    ))
    expect_equal(mixed$ranges, list(c(1L, 3L), c(5L, 5L)))
})

test_that("covrignore_excluded_files drops files listed in .covrignore", {
    f <- resolve_mutator_fn("covrignore_excluded_files")

    pkg <- tempfile()
    dir.create(file.path(pkg, "R"), recursive = TRUE)
    on.exit(unlink(pkg, recursive = TRUE), add = TRUE)
    files <- file.path(pkg, "R", c(
        "keep.R", "drop.R", "import-standalone-x.R", "import-standalone-y.R"
    ))
    for (x in files) writeLines("f <- function() 1", x)

    # No .covrignore: everything kept.
    expect_equal(f(files, pkg), files)

    # Exact path and glob patterns (relative to the package root).
    writeLines(c("R/drop.R", "R/import-standalone-*.R"), file.path(pkg, ".covrignore"))
    expect_equal(basename(f(files, pkg)), "keep.R")

    # Blank lines and non-matching patterns are inert.
    writeLines(c("", "   ", "R/does-not-exist-*.R"), file.path(pkg, ".covrignore"))
    expect_equal(f(files, pkg), files)

    # A directory entry expands to the files under it.
    writeLines("R", file.path(pkg, ".covrignore"))
    expect_equal(f(files, pkg), character(0))
})

test_that("is_excluded_range tests inclusive span overlap", {
    is_excluded_range <- resolve_mutator_fn("is_excluded_range")

    expect_false(is_excluded_range(3, 3, list()))             # no ranges
    expect_true(is_excluded_range(3, 3, list(c(2, 4))))       # inside
    expect_true(is_excluded_range(1, 2, list(c(2, 4))))       # overlaps lower edge
    expect_true(is_excluded_range(4, 9, list(c(2, 4))))       # overlaps upper edge
    expect_false(is_excluded_range(5, 9, list(c(2, 4))))      # disjoint
    expect_true(is_excluded_range(1, 9, list(c(2, 4))))       # span contains range
    # NA / empty bounds are treated as "cannot attribute" -> not excluded.
    expect_false(is_excluded_range(NA, NA, list(c(2, 4))))
    expect_false(is_excluded_range(integer(), integer(), list(c(2, 4))))
})

test_that("filter_excluded_files drops files matching glob patterns", {
    filter_excluded_files <- resolve_mutator_fn("filter_excluded_files")

    files <- c("/p/R/import-standalone-x.R", "/p/R/import-standalone-y.R", "/p/R/core.R")

    # Glob matches both standalone files, keeps core.R.
    expect_equal(
        basename(filter_excluded_files(files, "import-standalone-*")),
        "core.R"
    )
    # Exact literal name works too.
    expect_equal(
        basename(filter_excluded_files(files, "core.R")),
        c("import-standalone-x.R", "import-standalone-y.R")
    )
    # Multiple patterns are unioned.
    expect_length(filter_excluded_files(files, c("import-standalone-*", "core.R")), 0)
    # NULL / empty is a no-op.
    expect_equal(filter_excluded_files(files, NULL), files)
    expect_equal(filter_excluded_files(files, character()), files)
    # Non-character errors.
    expect_error(filter_excluded_files(files, 123), "character vector")
})

# --- imputesrcref-based location refinement (optional dependency) ------------

