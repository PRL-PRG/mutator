resolve_mutator_fn <- function(name) {
    get0(name,
        mode = "function",
        inherits = TRUE,
        ifnotfound = get(name, envir = asNamespace("mutator"))
    )
}

test_that("delete_line_mutants creates indexed mutant files", {
    delete_line_mutants <- resolve_mutator_fn("delete_line_mutants")

    src <- tempfile(fileext = ".R")
    out_dir <- tempfile("mutations_")
    dir.create(out_dir)
    on.exit(unlink(c(src, out_dir), recursive = TRUE), add = TRUE)

    writeLines(c(
        "# comment",
        "",
        "x <- 1",
        "y <- 2"
    ), src)

    set.seed(1)
    mutants <- delete_line_mutants(
        src_file = src,
        out_dir = out_dir,
        file_base = "example.R",
        max_del = 2,
        start_idx = 10
    )

    expect_length(mutants, 2)
    expect_equal(basename(mutants[[1]]$path), "example.R_010.R")
    expect_equal(basename(mutants[[2]]$path), "example.R_011.R")
    expect_true(all(vapply(mutants, function(m) file.exists(m$path), logical(1))))
    expect_true(all(vapply(mutants, function(m) is.list(m$info), logical(1))))
    expect_true(all(vapply(mutants, function(m) identical(m$info$mutation_type, "line_deletion"), logical(1))))
    expect_true(all(vapply(mutants, function(m) !is.null(m$info$deleted_line), logical(1))))
})

test_that("delete_line_mutants returns empty list when no valid lines", {
    delete_line_mutants <- resolve_mutator_fn("delete_line_mutants")

    src <- tempfile(fileext = ".R")
    out_dir <- tempfile("mutations_")
    dir.create(out_dir)
    on.exit(unlink(c(src, out_dir), recursive = TRUE), add = TRUE)

    writeLines(c("", "# only comment"), src)

    mutants <- NULL
    expect_warning(
        mutants <- delete_line_mutants(src, out_dir = out_dir, max_del = 3),
        "No valid lines to delete"
    )

    expect_equal(mutants, list())
})

test_that("create_equivalent_mutant_prompt includes required sections", {
    create_equivalent_mutant_prompt <- resolve_mutator_fn("create_equivalent_mutant_prompt")

    prompt <- create_equivalent_mutant_prompt(
        original_code = "x <- 1",
        mutant_details = list(
            list(id = "file_001", mutation_info = "x <- 0"),
            list(id = "file_002", mutation_info = "x <- 2")
        )
    )

    expect_match(prompt, "Original code", fixed = TRUE)
    expect_match(prompt, "file_001", fixed = TRUE)
    expect_match(prompt, "file_002", fixed = TRUE)
    expect_match(prompt, "DONT_KNOW", fixed = TRUE)
    expect_match(prompt, "JSON", fixed = TRUE)
})

test_that("create_equivalent_mutant_prompt embeds mutated code when provided", {
    create_equivalent_mutant_prompt <- resolve_mutator_fn("create_equivalent_mutant_prompt")

    prompt <- create_equivalent_mutant_prompt(
        original_code = "f <- function(x) x + 1",
        mutant_details = list(
            list(id = "f_001", mutation_info = "'+' -> '-'", mutated_code = "f <- function(x) x - 1")
        )
    )

    # The indented marker only appears in a per-mutant block, not the intro text.
    expect_match(prompt, "\n  mutated code:\n", fixed = TRUE)
    expect_match(prompt, "f <- function(x) x - 1", fixed = TRUE)

    # Without mutated_code the per-mutant section is omitted (e.g. the exported
    # entry point called with only a change description).
    prompt2 <- create_equivalent_mutant_prompt(
        original_code = "f <- function(x) x + 1",
        mutant_details = list(list(id = "f_001", mutation_info = "'+' -> '-'"))
    )
    expect_no_match(prompt2, "\n  mutated code:\n", fixed = TRUE)
})

test_that("parse_equivalence_verdicts reads JSON object, array and fenced forms", {
    parse_equivalence_verdicts <- resolve_mutator_fn("parse_equivalence_verdicts")

    obj <- '{"results":[{"id":"a_001","verdict":"EQUIVALENT"},{"id":"b_002","verdict":"NOT_EQUIVALENT"}]}'
    v <- parse_equivalence_verdicts(obj)
    expect_equal(v[["a_001"]], "EQUIVALENT")
    expect_equal(v[["b_002"]], "NOT_EQUIVALENT")

    arr <- '[{"id":"a_001","verdict":"DONT_KNOW"}]'
    expect_equal(parse_equivalence_verdicts(arr)[["a_001"]], "DONT_KNOW")

    fenced <- "```json\n{\"results\":[{\"id\":\"a_001\",\"verdict\":\"EQUIVALENT\"}]}\n```"
    expect_equal(parse_equivalence_verdicts(fenced)[["a_001"]], "EQUIVALENT")

    # Surrounding prose is tolerated; non-JSON returns NULL so the caller falls back.
    pre <- "Here is my answer:\n{\"results\":[{\"id\":\"a_001\",\"verdict\":\"EQUIVALENT\"}]}"
    expect_equal(parse_equivalence_verdicts(pre)[["a_001"]], "EQUIVALENT")
    expect_null(parse_equivalence_verdicts("no json here"))
    expect_null(parse_equivalence_verdicts(NULL))
})

test_that("classify_equivalence_verdict normalises tokens", {
    classify_equivalence_verdict <- resolve_mutator_fn("classify_equivalence_verdict")

    expect_true(classify_equivalence_verdict("EQUIVALENT")$equivalent)
    expect_false(classify_equivalence_verdict("not equivalent")$equivalent)
    expect_equal(classify_equivalence_verdict("NOT_EQUIVALENT")$status, "NOT EQUIVALENT")
    expect_true(is.na(classify_equivalence_verdict("DONT_KNOW")$equivalent))
    expect_true(is.na(classify_equivalence_verdict(NA)$equivalent))
    expect_true(is.na(classify_equivalence_verdict("gibberish")$equivalent))
})

test_that("fallback_line_verdicts does not bleed verdicts across mutants", {
    fallback_line_verdicts <- resolve_mutator_fn("fallback_line_verdicts")

    # m1 has no verdict on its own line; the EQUIVALENT belongs to m2. The old
    # greedy `mid.*EQUIVALENT` regex would have matched m1 against m2's verdict.
    content <- paste(
        "Mutant a.R_a.R_001.R: analysis pending",
        "Mutant a.R_a.R_002.R: EQUIVALENT",
        sep = "\n"
    )
    v <- fallback_line_verdicts(content, c("a.R_a.R_001.R", "a.R_a.R_002.R"))
    expect_true(is.na(v[["a.R_a.R_001.R"]]))
    expect_equal(v[["a.R_a.R_002.R"]], "EQUIVALENT")

    # "NOT EQUIVALENT" must not be read as EQUIVALENT.
    expect_equal(fallback_line_verdicts("Mutant x: NOT EQUIVALENT", "x")[["x"]], "NOT_EQUIVALENT")
})

test_that("identify_equivalent_mutants batches requests and merges all verdicts", {
    identify_equivalent_mutants <- resolve_mutator_fn("identify_equivalent_mutants")

    src <- tempfile(fileext = ".R")
    writeLines("f <- function(x) x + 1", src)
    on.exit(unlink(src), add = TRUE)

    n <- 7
    survived <- list()
    for (i in seq_len(n)) {
        survived[[sprintf("m_%03d", i)]] <- list(mutation_info = sprintf("change %d", i))
    }

    calls <- 0
    testthat::local_mocked_bindings(
        # Stand in for the network call: answer only the ids in *this* batch's
        # prompt, so we verify each batch is self-contained and all merge.
        call_openai_api = function(prompt, config) {
            calls <<- calls + 1
            ids <- unique(gsub('"', "", regmatches(prompt, gregexpr('"m_[0-9]+"', prompt))[[1]]))
            entries <- vapply(ids, function(id) {
                sprintf('{"id":"%s","verdict":"NOT_EQUIVALENT"}', id)
            }, character(1))
            content <- sprintf('{"results":[%s]}', paste(entries, collapse = ","))
            list(choices = list(list(message = list(content = content))))
        },
        .package = "mutator"
    )

    res <- identify_equivalent_mutants(
        src, survived,
        api_config = list(api_key = "k", model = "m"),
        batch_size = 3, workers = 1
    )

    # 7 mutants / batch of 3 -> 3 requests (3, 3, 1).
    expect_equal(calls, 3)
    # Every mutant is classified -- none dropped across batches.
    expect_length(res, n)
    expect_true(all(vapply(
        res, function(m) identical(m$equivalence_status, "NOT EQUIVALENT"), logical(1)
    )))
})

test_that("make_unified_diff emits a minimal single hunk", {
    make_unified_diff <- resolve_mutator_fn("make_unified_diff")

    orig <- c("a <- 1", "if (x < 0) y <- 2", "z <- 3")
    mut <- c("a <- 1", "if (x > 0) y <- 2", "z <- 3")
    d <- make_unified_diff(orig, mut, context = 0)
    expect_match(d, "^@@")
    expect_match(d, "-if (x < 0) y <- 2", fixed = TRUE)
    expect_match(d, "+if (x > 0) y <- 2", fixed = TRUE)
    # With no context, unchanged surrounding lines are not included.
    expect_no_match(d, "z <- 3", fixed = TRUE)
    expect_no_match(d, "a <- 1", fixed = TRUE)

    # A line deletion shows a '-' line and NO '+' change line (guards the R
    # paste0("+", character(0)) == "+" quirk).
    del <- make_unified_diff(c("keep1", "drop", "keep2"), c("keep1", "keep2"), context = 0)
    expect_match(del, "-drop", fixed = TRUE)
    expect_false(any(grepl("^\\+", strsplit(del, "\n", fixed = TRUE)[[1]])))

    # Identical inputs produce no diff.
    expect_equal(make_unified_diff(c("a", "b"), c("a", "b")), "")
})

test_that("create_equivalent_mutant_prompt renders a unified diff when provided", {
    create_equivalent_mutant_prompt <- resolve_mutator_fn("create_equivalent_mutant_prompt")

    prompt <- create_equivalent_mutant_prompt(
        original_code = "f <- function(x) x + 1",
        mutant_details = list(
            list(id = "f_001", mutation_info = "'+' -> '-'",
                 diff = "@@ -1,1 +1,1 @@\n-f <- function(x) x + 1\n+f <- function(x) x - 1")
        )
    )
    expect_match(prompt, "\n  diff:\n```diff\n", fixed = TRUE)
    expect_match(prompt, "+f <- function(x) x - 1", fixed = TRUE)
})

test_that("equivalence reasons are captured only for EQUIVALENT verdicts", {
    parse_equivalence_verdicts <- resolve_mutator_fn("parse_equivalence_verdicts")
    identify_equivalent_mutants <- resolve_mutator_fn("identify_equivalent_mutants")

    # The parser attaches per-id reasons as the "reasons" attribute.
    j <- paste0(
        '{"results":[{"id":"a","verdict":"EQUIVALENT","reason":"same for all x"},',
        '{"id":"b","verdict":"NOT_EQUIVALENT"}]}'
    )
    v <- parse_equivalence_verdicts(j)
    expect_equal(v[["a"]], "EQUIVALENT")
    expect_equal(attr(v, "reasons")[["a"]], "same for all x")
    expect_false("b" %in% names(attr(v, "reasons")))

    # End to end: the reason is stored on the equivalent mutant only.
    src <- tempfile(fileext = ".R")
    writeLines("f <- function(x) x + 1", src)
    on.exit(unlink(src), add = TRUE)
    survived <- list(
        m_eq = list(mutation_info = "'+' -> '-'"),
        m_neq = list(mutation_info = "'+' -> '*'")
    )
    testthat::local_mocked_bindings(
        call_openai_api = function(prompt, config) {
            list(choices = list(list(message = list(content = paste0(
                '{"results":[{"id":"m_eq","verdict":"EQUIVALENT","reason":"x is unused"},',
                '{"id":"m_neq","verdict":"NOT_EQUIVALENT"}]}'
            )))))
        },
        .package = "mutator"
    )
    res <- identify_equivalent_mutants(src, survived, api_config = list(api_key = "k", model = "m"))
    expect_true(isTRUE(res$m_eq$equivalent))
    expect_equal(res$m_eq$equivalence_reason, "x is unused")
    expect_false(isTRUE(res$m_neq$equivalent))
    expect_null(res$m_neq$equivalence_reason)
})

# Save/restore a single environment variable across a test.
restore_env_var <- function(name, value) {
    if (is.na(value)) {
        Sys.unsetenv(name)
    } else {
        args <- list(value)
        names(args) <- name
        do.call(Sys.setenv, args)
    }
}

test_that("get_openai_config uses defaults, environment variables and base_url", {
    get_openai_config <- resolve_mutator_fn("get_openai_config")
    reset_openai_config <- resolve_mutator_fn("reset_openai_config")
    reset_openai_config()

    old <- Sys.getenv(c("OPENAI_API_KEY", "OPENAI_MODEL", "OPENAI_BASE_URL"), unset = NA_character_)
    on.exit(
        {
            reset_openai_config()
            restore_env_var("OPENAI_API_KEY", old[["OPENAI_API_KEY"]])
            restore_env_var("OPENAI_MODEL", old[["OPENAI_MODEL"]])
            restore_env_var("OPENAI_BASE_URL", old[["OPENAI_BASE_URL"]])
        },
        add = TRUE
    )
    Sys.unsetenv(c("OPENAI_API_KEY", "OPENAI_MODEL", "OPENAI_BASE_URL"))

    empty_dir <- tempfile("cfg_empty_")
    dir.create(empty_dir)
    on.exit(unlink(empty_dir, recursive = TRUE), add = TRUE)

    cfg <- get_openai_config(dir = empty_dir)
    expect_equal(cfg$api_key, "")
    expect_equal(cfg$model, "gpt-4")
    expect_equal(cfg$base_url, "https://api.openai.com/v1")

    Sys.setenv(
        OPENAI_API_KEY = "env-key",
        OPENAI_MODEL = "env-model",
        OPENAI_BASE_URL = "https://env.example/v1"
    )
    cfg2 <- get_openai_config(dir = empty_dir)
    expect_equal(cfg2$api_key, "env-key")
    expect_equal(cfg2$model, "env-model")
    expect_equal(cfg2$base_url, "https://env.example/v1")
})

test_that("get_openai_config reads a .openai_config DCF file and does not walk parents", {
    get_openai_config <- resolve_mutator_fn("get_openai_config")
    reset_openai_config <- resolve_mutator_fn("reset_openai_config")
    reset_openai_config()

    old_key <- Sys.getenv("OPENAI_API_KEY", unset = NA_character_)
    on.exit(
        {
            reset_openai_config()
            restore_env_var("OPENAI_API_KEY", old_key)
        },
        add = TRUE
    )
    Sys.setenv(OPENAI_API_KEY = "env-key")

    d <- tempfile("cfg_dir_")
    dir.create(d)
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    writeLines(
        c("api_key: file-key", "model: file-model", "base_url: https://file.example/v1"),
        file.path(d, ".openai_config")
    )

    cfg <- get_openai_config(dir = d)
    # File overrides the environment variable.
    expect_equal(cfg$api_key, "file-key")
    expect_equal(cfg$model, "file-model")
    expect_equal(cfg$base_url, "https://file.example/v1")

    # A config in the parent dir is NOT picked up for a child dir.
    child <- file.path(d, "sub")
    dir.create(child)
    expect_equal(get_openai_config(dir = child)$api_key, "env-key")
})

test_that("set_openai_config overrides file and environment, per field", {
    get_openai_config <- resolve_mutator_fn("get_openai_config")
    set_openai_config <- resolve_mutator_fn("set_openai_config")
    reset_openai_config <- resolve_mutator_fn("reset_openai_config")
    reset_openai_config()
    on.exit(reset_openai_config(), add = TRUE)

    d <- tempfile("cfg_set_")
    dir.create(d)
    on.exit(unlink(d, recursive = TRUE), add = TRUE)
    writeLines(c("api_key: file-key", "model: file-model"), file.path(d, ".openai_config"))

    set_openai_config(api_key = "set-key", base_url = "https://set.example/v1")
    cfg <- get_openai_config(dir = d)
    expect_equal(cfg$api_key, "set-key") # setter beats file
    expect_equal(cfg$model, "file-model") # untouched field falls back to file
    expect_equal(cfg$base_url, "https://set.example/v1")
})

test_that("build_chat_completions_url appends path and respects full endpoints", {
    build_chat_completions_url <- resolve_mutator_fn("build_chat_completions_url")

    expect_equal(
        build_chat_completions_url("https://api.openai.com/v1"),
        "https://api.openai.com/v1/chat/completions"
    )
    expect_equal(
        build_chat_completions_url("https://api.openai.com/v1/"),
        "https://api.openai.com/v1/chat/completions"
    )
    expect_equal(
        build_chat_completions_url("https://x/v1/chat/completions"),
        "https://x/v1/chat/completions"
    )
})

test_that("C_mutate_file validates input types and srcref", {
    expect_error(
        .Call("C_mutate_file", 1L, PACKAGE = "mutator"),
        "EXPRSXP"
    )

    exprs <- expression(a + b)
    attr(exprs, "srcref") <- list(1:3)

    expect_error(
        .Call("C_mutate_file", exprs, PACKAGE = "mutator"),
        "length 4"
    )
})

test_that("mutate_package supports a user-provided mutation_dir", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    skip_if_not_installed("pkgload")
    skip_if_not_installed("furrr")
    skip_if_not_installed("future")

    pkg_info <- create_test_package("testMutatoRCustomDir")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    custom_mutation_dir <- tempfile("mutations_keep_")
    result <- mutate_package(
        pkg_dir = pkg_info$pkg_dir,
        cores = 1,
        mutation_dir = custom_mutation_dir,
        isFullLog = TRUE
    )
    on.exit(unlink(custom_mutation_dir, recursive = TRUE), add = TRUE)

    expect_true(is.list(result))
    expect_true(dir.exists(custom_mutation_dir))

    mutated_files <- list.files(custom_mutation_dir, pattern = "\\.R$", full.names = TRUE)
    expect_true(length(mutated_files) > 0)
})

test_that("mutate_file falls back to line-deletion mutants when C call fails", {
    mutate_file <- resolve_mutator_fn("mutate_file")

    src <- tempfile(fileext = ".R")
    out_dir <- tempfile("mutate_file_out_")
    dir.create(out_dir, recursive = TRUE)
    on.exit(unlink(c(src, out_dir), recursive = TRUE), add = TRUE)

    writeLines(c("f <- function(x) x + 1", "f(1)"), src)

    mutants <- mutate_file(src, out_dir = out_dir)

    expect_true(length(mutants) >= 1)
    expect_true(any(vapply(mutants, function(m) grepl("deleted line", m$info, fixed = TRUE), logical(1))))
    expect_true(all(vapply(mutants, function(m) file.exists(m$path), logical(1))))
})

test_that("max_line_deletions caps and can disable line-deletion mutants", {
    mutate_file <- resolve_mutator_fn("mutate_file")

    src <- tempfile(fileext = ".R")
    on.exit(unlink(src), add = TRUE)
    # A pure top-level script (no { } blocks) so all mutants come from
    # line-deletion, isolating the effect of max_line_deletions.
    writeLines(sprintf("x%d <- %d", 1:10, 1:10), src)

    count_line_dels <- function(mutants) {
      sum(vapply(mutants, function(m) grepl("deleted line", m$info, fixed = TRUE), logical(1)))
    }

    out0 <- tempfile("md0_"); dir.create(out0); on.exit(unlink(out0, recursive = TRUE), add = TRUE)
    expect_equal(count_line_dels(mutate_file(src, out_dir = out0, max_line_deletions = 0)), 0)

    out3 <- tempfile("md3_"); dir.create(out3); on.exit(unlink(out3, recursive = TRUE), add = TRUE)
    expect_equal(count_line_dels(mutate_file(src, out_dir = out3, max_line_deletions = 3)), 3)

    out9 <- tempfile("md9_"); dir.create(out9); on.exit(unlink(out9, recursive = TRUE), add = TRUE)
    expect_equal(count_line_dels(mutate_file(src, out_dir = out9, max_line_deletions = 9)), 9)

    expect_error(mutate_file(src, out_dir = out0, max_line_deletions = -1), "max_line_deletions")
    expect_error(mutate_file(src, out_dir = out0, max_line_deletions = 1.5), "whole number")
})

test_that("mutate_file restores keep.source option", {
    mutate_file <- resolve_mutator_fn("mutate_file")

    old_options <- options(keep.source = FALSE)
    on.exit(options(old_options), add = TRUE)

    src <- tempfile(fileext = ".R")
    out_dir <- tempfile("mutate_file_options_")
    dir.create(out_dir, recursive = TRUE)
    on.exit(unlink(c(src, out_dir), recursive = TRUE), add = TRUE)

    writeLines("f <- function(x) x + 1", src)
    mutate_file(src, out_dir = out_dir)

    expect_false(getOption("keep.source"))
})

test_that("mutate_package handles empty test results as killed mutants", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    pkg_info <- create_test_package("testMutatoREmptyResults")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    mutation_dir <- tempfile("mutations_mock_")
    dir.create(mutation_dir, recursive = TRUE)
    on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)

    mut_path <- file.path(mutation_dir, "my_abs.R_001.R")
    writeLines("my_abs <- function(x) x", mut_path)

    testthat::local_mocked_bindings(
        mutate_file = function(...) {
            list(list(path = mut_path, info = "mock mutation"))
        },
        .package = "mutator"
    )
    testthat::local_mocked_bindings(
        future_map = function(.x, .f, ...) {
            out <- lapply(.x, function(...) list())
            names(out) <- names(.x)
            out
        },
        furrr_options = function(...) NULL,
        .package = "furrr"
    )
    testthat::local_mocked_bindings(
        plan = function(...) NULL,
        .package = "future"
    )

    result <- mutate_package(
        pkg_dir = pkg_info$pkg_dir,
        cores = 1,
        isFullLog = TRUE,
        mutation_dir = mutation_dir
    )

    expect_true(is.list(result))
    expect_true(length(result$test_results) >= 1)
    expect_true(all(vapply(result$test_results, function(x) identical(x, "KILLED"), logical(1))))
})

test_that("mutate_package restores previous future plan on errors", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    pkg_info <- create_test_package("testMutatoRFuturePlanRestore")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    mutation_dir <- tempfile("mutations_future_plan_")
    dir.create(mutation_dir, recursive = TRUE)
    on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)

    mut_path <- file.path(mutation_dir, "my_abs.R_001.R")
    writeLines("my_abs <- function(x) x", mut_path)

    plan_calls <- list()
    testthat::local_mocked_bindings(
        mutate_file = function(...) {
            list(list(path = mut_path, info = "mock mutation"))
        },
        .package = "mutator"
    )
    testthat::local_mocked_bindings(
        future_map = function(...) {
            stop("future_map failed")
        },
        furrr_options = function(...) {
            list(...)
        },
        .package = "furrr"
    )
    testthat::local_mocked_bindings(
        plan = function(...) {
            args <- list(...)
            plan_calls[[length(plan_calls) + 1]] <<- args
            if (length(args) == 0) "previous-plan" else invisible(NULL)
        },
        .package = "future"
    )

    expect_error(
        mutate_package(
            pkg_dir = pkg_info$pkg_dir,
            cores = 1,
            mutation_dir = mutation_dir
        ),
        "future_map failed"
    )

    expect_true(any(vapply(
        plan_calls,
        function(args) length(args) == 1 && identical(args[[1]], "previous-plan"),
        logical(1)
    )))
})

test_that("mutate_package marks timed-out mutants as HANG", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    pkg_info <- create_test_package("testMutatoRTimeoutHang")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    mutation_dir <- tempfile("mutations_hang_")
    dir.create(mutation_dir, recursive = TRUE)
    on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)

    mut_path <- file.path(mutation_dir, "my_abs.R_001.R")
    writeLines("my_abs <- function(x) x", mut_path)

    testthat::local_mocked_bindings(
        mutate_file = function(...) {
            list(list(path = mut_path, info = "mock mutation"))
        },
        .package = "mutator"
    )
    testthat::local_mocked_bindings(
        future_map = function(.x, .f, ...) {
            out <- lapply(.x, function(...) "HANG")
            names(out) <- names(.x)
            out
        },
        furrr_options = function(...) NULL,
        .package = "furrr"
    )
    testthat::local_mocked_bindings(
        plan = function(...) NULL,
        .package = "future"
    )

    result <- mutate_package(
        pkg_dir = pkg_info$pkg_dir,
        cores = 1,
        mutation_dir = mutation_dir
    )

    expect_equal(unname(unlist(result$test_results)), "HANG")
    expect_equal(result$package_mutants[[1]]$status, "HANG")
})

test_that("mutate_package passes explicit timeout to inline worker execution", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    pkg_info <- create_test_package("testMutatoRExplicitTimeout")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    mutation_dir <- tempfile("mutations_timeout_override_")
    dir.create(mutation_dir, recursive = TRUE)
    on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)

    mut_path <- file.path(mutation_dir, "my_abs.R_001.R")
    writeLines("my_abs <- function(x) x", mut_path)

    observed_timeout <- NA_real_

    testthat::local_mocked_bindings(
        mutate_file = function(...) {
            list(list(path = mut_path, info = "mock mutation"))
        },
        .package = "mutator"
    )
    testthat::local_mocked_bindings(
        future_map = function(.x, .f, ...) {
            dots <- list(...)
            if (!is.null(dots$.options) && !is.null(dots$.options$globals)) {
                observed_timeout <<- dots$.options$globals$effective_timeout_seconds
            }
            out <- lapply(.x, .f)
            names(out) <- names(.x)
            out
        },
        furrr_options = function(...) {
            list(...)
        },
        .package = "furrr"
    )
    testthat::local_mocked_bindings(
        plan = function(...) NULL,
        .package = "future"
    )

    result <- mutate_package(
        pkg_dir = pkg_info$pkg_dir,
        cores = 1,
        mutation_dir = mutation_dir,
        timeout_seconds = 12.5
    )

    expect_true(is.list(result))
    expect_equal(observed_timeout, 12.5)
})

test_that("mutate_package applies a floor to derived timeouts", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    pkg_info <- create_test_package("testMutatoRTimeoutFloor")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    mutation_dir <- tempfile("mutations_timeout_floor_")
    dir.create(mutation_dir, recursive = TRUE)
    on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)

    mut_path <- file.path(mutation_dir, "my_abs.R_001.R")
    writeLines("my_abs <- function(x) x", mut_path)

    observed_timeout <- NA_real_

    testthat::local_mocked_bindings(
        mutate_file = function(...) {
            list(list(path = mut_path, info = "mock mutation"))
        },
        .package = "mutator"
    )
    testthat::local_mocked_bindings(
        future_map = function(.x, .f, ...) {
            dots <- list(...)
            if (!is.null(dots$.options) && !is.null(dots$.options$globals)) {
                observed_timeout <<- dots$.options$globals$effective_timeout_seconds
            }
            out <- lapply(.x, .f)
            names(out) <- names(.x)
            out
        },
        furrr_options = function(...) {
            list(...)
        },
        .package = "furrr"
    )
    testthat::local_mocked_bindings(
        plan = function(...) NULL,
        .package = "future"
    )

    result <- mutate_package(
        pkg_dir = pkg_info$pkg_dir,
        cores = 1,
        mutation_dir = mutation_dir
    )

    expect_true(is.list(result))
    expect_gte(observed_timeout, 5)
})

test_that("mutate_package computes equivalent mutant summary when enabled", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    pkg_info <- create_test_package("testMutatoREquivSummary")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    mutation_dir <- tempfile("mutations_eq_")
    dir.create(mutation_dir, recursive = TRUE)
    on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)

    mut_paths <- c(
        file.path(mutation_dir, "my_abs.R_001.R"),
        file.path(mutation_dir, "my_abs.R_002.R"),
        file.path(mutation_dir, "my_abs.R_003.R")
    )
    original_impl <- c(
        "my_abs <- function(x) {",
        "  if (x < 0) {",
        "    return(-x)",
        "  }",
        "  return(x)",
        "}"
    )
    for (p in mut_paths) writeLines(original_impl, p)

    testthat::local_mocked_bindings(
        mutate_file = function(...) {
            list(
                list(path = mut_paths[[1]], info = "m1"),
                list(path = mut_paths[[2]], info = "m2"),
                list(path = mut_paths[[3]], info = "m3")
            )
        },
        identify_equivalent_mutants = function(src_file, file_mutants, api_config = NULL, ...) {
            ids <- names(file_mutants)
            if (length(ids) >= 1) {
                file_mutants[[ids[[1]]]]$equivalent <- TRUE
                file_mutants[[ids[[1]]]]$equivalence_status <- "EQUIVALENT"
            }
            if (length(ids) >= 2) {
                file_mutants[[ids[[2]]]]$equivalent <- FALSE
                file_mutants[[ids[[2]]]]$equivalence_status <- "NOT EQUIVALENT"
            }
            if (length(ids) >= 3) {
                file_mutants[[ids[[3]]]]$equivalent <- NA
                file_mutants[[ids[[3]]]]$equivalence_status <- "DONT KNOW"
            }
            file_mutants
        },
        .package = "mutator"
    )

    result <- NULL
    # Progress and the summary are now emitted via message() (stderr), so
    # capture conditions rather than stdout.
    output <- testthat::capture_messages({
        result <- mutate_package(
            pkg_dir = pkg_info$pkg_dir,
            cores = 1,
            isFullLog = TRUE,
            detectEqMutants = TRUE,
            mutation_dir = mutation_dir
        )
    })

    expect_true(any(grepl("Equivalent:", output)))
    expect_true(any(grepl("Not Equivalent:", output)))
    expect_true(any(grepl("Uncertain:", output)))
    expect_true(is.list(result))
    expect_true(length(result$package_mutants) >= 1)
})

test_that("max_mutants validation rejects invalid values", {
    mutate_file <- resolve_mutator_fn("mutate_file")
    mutate_package <- resolve_mutator_fn("mutate_package")

    src <- tempfile(fileext = ".R")
    out_dir <- tempfile("mutations_validation_")
    dir.create(out_dir, recursive = TRUE)
    on.exit(unlink(c(src, out_dir), recursive = TRUE), add = TRUE)
    writeLines("x <- 1", src)

    expect_error(mutate_file(src, out_dir = out_dir, max_mutants = -1), "max_mutants")
    expect_error(mutate_file(src, out_dir = out_dir, max_mutants = c(1, 2)), "single finite")
    expect_error(mutate_file(src, out_dir = out_dir, max_mutants = 1.5), "whole number")

    expect_error(mutate_package(tempdir(), max_mutants = -1), "max_mutants")
    expect_error(mutate_package(tempdir(), max_mutants = c(1, 2)), "single finite")
    expect_error(mutate_package(tempdir(), max_mutants = 1.5), "whole number")
})

test_that("timeout parameter validation rejects invalid values", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    expect_error(mutate_package(tempdir(), timeout_seconds = 0), "timeout_seconds")
    expect_error(mutate_package(tempdir(), timeout_seconds = c(1, 2)), "single finite")
})

test_that("mutate_package caps total mutants with max_mutants", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    skip_if_not_installed("pkgload")
    skip_if_not_installed("furrr")
    skip_if_not_installed("future")

    pkg_info <- create_test_package("testMutatoRMaxMutantsCap")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    mutation_dir <- tempfile("mutations_cap_")
    dir.create(mutation_dir, recursive = TRUE)
    on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)

    mut_paths <- c(
        file.path(mutation_dir, "my_abs.R_001.R"),
        file.path(mutation_dir, "my_abs.R_002.R"),
        file.path(mutation_dir, "my_abs.R_003.R")
    )
    for (p in mut_paths) writeLines("my_abs <- function(x) x", p)

    testthat::local_mocked_bindings(
        mutate_file = function(...) {
            list(
                list(path = mut_paths[[1]], info = "m1"),
                list(path = mut_paths[[2]], info = "m2"),
                list(path = mut_paths[[3]], info = "m3")
            )
        },
        .package = "mutator"
    )
    testthat::local_mocked_bindings(
        future_map = function(.x, .f, ...) {
            out <- lapply(.x, function(...) TRUE)
            names(out) <- names(.x)
            out
        },
        furrr_options = function(...) NULL,
        .package = "furrr"
    )
    testthat::local_mocked_bindings(
        plan = function(...) NULL,
        .package = "future"
    )

    result <- mutate_package(
        pkg_dir = pkg_info$pkg_dir,
        cores = 1,
        mutation_dir = mutation_dir,
        max_mutants = 2
    )

    expect_true(is.list(result))
    expect_length(result$test_results, 2)
    expect_length(result$package_mutants, 2)
})

test_that("mutate_package supports max_mutants set to zero", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    skip_if_not_installed("pkgload")
    skip_if_not_installed("furrr")
    skip_if_not_installed("future")

    pkg_info <- create_test_package("testMutatoRZeroMutants")
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    mutation_dir <- tempfile("mutations_zero_cap_")
    dir.create(mutation_dir, recursive = TRUE)
    on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)

    mut_path <- file.path(mutation_dir, "my_abs.R_001.R")
    writeLines("my_abs <- function(x) x", mut_path)

    testthat::local_mocked_bindings(
        mutate_file = function(...) {
            list(list(path = mut_path, info = "m1"))
        },
        .package = "mutator"
    )
    testthat::local_mocked_bindings(
        plan = function(...) NULL,
        .package = "future"
    )

    result <- mutate_package(
        pkg_dir = pkg_info$pkg_dir,
        cores = 1,
        mutation_dir = mutation_dir,
        max_mutants = 0
    )

    expect_true(is.list(result))
    expect_equal(result$test_results, list())
    expect_equal(result$package_mutants, list())
})

test_that("detectEqMutants runs the real equivalence workflow for underscore filenames", {
    # Regression test: the source file backing a survived mutant used to be
    # recovered with strsplit(id, "_")[[1]][1], which mangles any name
    # containing '_' (e.g. my_abs.R -> "my"). identify_equivalent_mutants then
    # tried to readLines("<pkg>/R/my") and errored. Here we exercise the real
    # workflow (only the network call is stubbed) to lock in the fix.
    mutate_package <- resolve_mutator_fn("mutate_package")

    skip_if_not_installed("pkgload")
    skip_if_not_installed("furrr")
    skip_if_not_installed("future")

    pkg_info <- create_test_package("testMutatoREquivReal") # ships R/my_abs.R
    on.exit(cleanup_test_package(pkg_info), add = TRUE)

    mutation_dir <- tempfile("mutations_eq_real_")
    dir.create(mutation_dir, recursive = TRUE)
    on.exit(unlink(mutation_dir, recursive = TRUE), add = TRUE)

    # A mutant identical to the original survives the test suite.
    mut_path <- file.path(mutation_dir, "my_abs.R_001.R")
    writeLines(c(
        "my_abs <- function(x) {",
        "  if (x < 0) {",
        "    return(-x)",
        "  }",
        "  return(x)",
        "}"
    ), mut_path)

    # The mutant id mirrors how mutate_package builds it: <src>_<mutant file>.
    expected_id <- paste("my_abs.R", basename(mut_path), sep = "_")

    # Make get_openai_config() return a non-empty key so the workflow proceeds
    # past its early return, without touching the real environment/config files.
    old_key <- Sys.getenv("OPENAI_API_KEY", unset = NA_character_)
    on.exit(
        if (is.na(old_key)) Sys.unsetenv("OPENAI_API_KEY") else Sys.setenv(OPENAI_API_KEY = old_key),
        add = TRUE
    )
    Sys.setenv(OPENAI_API_KEY = "test-key")

    testthat::local_mocked_bindings(
        mutate_file = function(...) {
            list(list(path = mut_path, info = "identity mutant"))
        },
        # Stub only the network call; identify_equivalent_mutants (incl. the
        # readLines(src_file) that used to crash) runs for real.
        call_openai_api = function(prompt, config) {
            list(choices = list(list(message = list(
                content = paste0("Mutant ", expected_id, ": EQUIVALENT")
            ))))
        },
        .package = "mutator"
    )

    result <- mutate_package(
        pkg_dir = pkg_info$pkg_dir,
        cores = 1,
        detectEqMutants = TRUE,
        mutation_dir = mutation_dir
    )

    # Completing at all is the core regression assertion.
    expect_true(is.list(result))
    expect_true(expected_id %in% names(result$package_mutants))

    mutant <- result$package_mutants[[expected_id]]
    expect_equal(mutant$status, "SURVIVED")
    # The equivalence verdict was attached, proving the correct source file was
    # read and the survived mutant was matched back to it.
    expect_true(isTRUE(mutant$equivalent))
    expect_equal(mutant$equivalence_status, "EQUIVALENT")
})

test_that("extract_harness_test_args mirrors the testthat.R harness", {
  mk <- function(lines) {
    f <- tempfile(fileext = ".R")
    writeLines(lines, f)
    f
  }

  # Canonical harness with a filter: the filter is forwarded, package dropped.
  expect_equal(
    extract_harness_test_args(mk(c(
      "library(testthat)", "library(jsonlite)",
      "test_check(\"jsonlite\", filter = \"toJSON|fromJSON\")"
    ))),
    list(filter = "toJSON|fromJSON")
  )

  # Plain harness: nothing to forward (full suite runs).
  expect_equal(
    extract_harness_test_args(mk(c("library(testthat)", "test_check(\"pkg\")"))),
    list()
  )

  # `package` named, and a `reporter` argument is stripped.
  expect_equal(
    extract_harness_test_args(mk(
      "test_check(package = \"pkg\", reporter = \"summary\", filter = \"x\")"
    )),
    list(filter = "x")
  )

  # Namespaced call is recognised too.
  expect_equal(
    extract_harness_test_args(mk("testthat::test_check(\"pkg\", filter = \"y\")")),
    list(filter = "y")
  )

  # A non-literal argument cannot be evaluated from literals -> safe fallback
  # (empty list = run the full suite rather than guess).
  expect_equal(
    extract_harness_test_args(mk(c("flt <- \"a|b\"", "test_check(\"pkg\", filter = flt)"))),
    list()
  )

  # No harness file, or no test_check() call -> empty list.
  expect_equal(extract_harness_test_args(tempfile(fileext = ".R")), list())
  expect_equal(
    extract_harness_test_args(mk(c("library(tinytest)", "test_package(\"pkg\")"))),
    list()
  )
})

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
