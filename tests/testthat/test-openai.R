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
    # Every mutant is classified; none dropped across batches.
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

test_that("mutate_package computes equivalent mutant summary when enabled", {
    mutate_package <- resolve_mutator_fn("mutate_package")

    pkg_info <- create_test_package("testMutatorEquivSummary")
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

    pkg_info <- create_test_package("testMutatorEquivReal") # ships R/my_abs.R
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


test_that("reset_openai_config clears programmatically set values", {
    store <- get(".openai_config_store", envir = asNamespace("mutator"))
    # Restore the clean (empty) default regardless of how the test exits, so it
    # cannot leak config into other tests.
    on.exit(reset_openai_config(), add = TRUE)

    set_openai_config(model = "sentinel-model", base_url = "http://sentinel.invalid")
    expect_true(all(c("model", "base_url") %in% ls(store)))

    reset_openai_config()
    expect_length(ls(store, all.names = TRUE), 0L)
})
