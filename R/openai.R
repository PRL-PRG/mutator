# nocov start
#' Identify equivalent mutants using OpenAI API
#'
#' Analyzes survived mutants to determine if they are functionally equivalent
#' to the original code using OpenAI's language models.
#'
#' @param src_file Path to the original source file
#' @param survived_mutants List of mutants that survived test execution
#' @param api_config Optional API configuration (will be loaded if NULL)
#' @param batch_size Maximum number of mutants sent in a single API request.
#'   Smaller batches keep each response short enough to avoid truncation (which
#'   silently drops verdicts) and let batches run concurrently. Defaults to 25.
#' @param workers Number of API requests to run concurrently (requires a
#'   forking platform). Defaults to 1 (sequential).
#' @param report Whether to print per-file progress and an equivalence summary
#'   to the console. Defaults to `TRUE`. `mutate_package()` sets this to `FALSE`
#'   when running many files in parallel, so it can print one aggregated summary
#'   (and a single progress bar) instead of one per batch.
#'
#' @return Updated list of survived mutants with equivalence information
#'
#' @examples
#' src <- tempfile(fileext = ".R")
#' writeLines("add <- function(x, y) x + y", src)
#' survived <- list(mutant_001 = list(mutation_info = "x + y -> x - y"))
#' suppressWarnings(identify_equivalent_mutants(
#'     src,
#'     survived,
#'     api_config = list(api_key = "", model = "gpt-4")
#' ))
#'
#' @export
identify_equivalent_mutants <- function(src_file, survived_mutants, api_config = NULL,
                                        batch_size = 25, workers = 1, report = TRUE) {
    # Load API configuration if not provided
    if (is.null(api_config)) {
        api_config <- get_openai_config()
    }

    # If no API key is available, return early
    if (is.null(api_config$api_key) || api_config$api_key == "") {
        warning("OpenAI API key not found. Skipping equivalent mutant detection.")
        return(survived_mutants)
    }

    ids <- names(survived_mutants)
    if (length(ids) == 0) {
        return(survived_mutants)
    }

    # Read original source code once; it is shared by every request. Each mutant
    # is shown to the model as a small *unified diff* of its edit (plus a short
    # `change:` label): compact, unambiguous, and in a format LLMs read
    # natively, without embedding the full mutated file per mutant.
    orig_raw_lines <- readLines(src_file, warn = FALSE)
    orig_code <- paste(orig_raw_lines, collapse = "\n")

    # Two references are needed to produce a clean (single-hunk) diff:
    #  - AST mutants are the whole file *re-deparsed* with one expression
    #    changed, so they must be diffed against a consistently re-deparsed
    #    original (otherwise reformatting shows up as spurious changes);
    #  - line-deletion mutants are the raw original minus one line, so they are
    #    diffed against the raw original.
    orig_deparsed_lines <- tryCatch(
        unlist(lapply(parse(src_file, keep.source = FALSE), deparse), use.names = FALSE),
        error = function(e) orig_raw_lines
    )

    mutant_diff <- function(mid) {
        m <- survived_mutants[[mid]]
        if (is.null(m$mutant_file) || !file.exists(m$mutant_file)) {
            return(NULL)
        }
        mut_lines <- tryCatch(readLines(m$mutant_file, warn = FALSE), error = function(e) NULL)
        if (is.null(mut_lines)) {
            return(NULL)
        }
        is_line_deletion <- is.character(m$mutation_info) &&
            any(grepl("deleted line", m$mutation_info, fixed = TRUE))
        reference <- if (is_line_deletion) orig_raw_lines else orig_deparsed_lines
        diff <- make_unified_diff(reference, mut_lines)
        if (nzchar(diff)) diff else NULL
    }

    # Split the mutants into bounded batches so no single response is large
    # enough to be slow or to truncate (which previously dropped verdicts).
    batch_size <- max(1L, as.integer(batch_size))
    batches <- unname(split(ids, ceiling(seq_along(ids) / batch_size)))

    verbose <- isTRUE(getOption("mutator.verbose", FALSE))

    classify_batch <- function(batch_ids) {
        mutant_details <- lapply(batch_ids, function(mid) {
            list(
                id = mid,
                mutation_info = survived_mutants[[mid]]$mutation_info,
                diff = mutant_diff(mid)
            )
        })
        prompt <- create_equivalent_mutant_prompt(orig_code, mutant_details)
        if (verbose) {
            message("Prompt being sent to OpenAI:\n", prompt)
        }
        response <- call_openai_api(prompt, api_config)
        if (inherits(response, "openai_api_error")) {
            # Propagate the cause so the caller can surface it; the empty vector
            # marks the batch as having produced no verdicts.
            return(structure(character(0), eq_error = response$message))
        }
        if (is.null(response) || is.null(response$choices) || length(response$choices) == 0) {
            return(structure(character(0), eq_error = "empty or malformed API response"))
        }
        content <- response$choices[[1]]$message$content
        if (verbose) {
            message("Answer received from OpenAI API:\n", content)
        }
        verdicts <- parse_equivalence_verdicts(content)
        if (is.null(verdicts)) {
            # Not usable JSON: fall back to a strict per-line scan (matches each
            # id literally, verdict must be on the same line) to avoid the
            # cross-mutant "bleed" a greedy whole-response regex would cause.
            verdicts <- fallback_line_verdicts(content, batch_ids)
        }
        verdicts
    }

    if (report) {
        message(sprintf(
            "Analyzing %d survived mutant(s) for %s in %d batch(es)...",
            length(ids), basename(src_file), length(batches)
        ))
    }

    # API calls are network-bound, so run batches concurrently when possible.
    use_parallel <- workers > 1 && length(batches) > 1 && future::supportsMulticore()
    batch_results <- if (use_parallel) {
        parallel::mclapply(batches, classify_batch, mc.cores = min(workers, length(batches)))
    } else {
        lapply(batches, classify_batch)
    }

    # Merge the per-batch id -> raw-verdict maps (and the id -> reason maps the
    # model supplies for EQUIVALENT verdicts).
    verdicts <- character(0)
    reasons <- character(0)
    for (r in batch_results) {
        if (!is.null(r) && !inherits(r, "try-error")) {
            verdicts <- c(verdicts, r)
            r_reasons <- attr(r, "reasons")
            if (!is.null(r_reasons)) {
                reasons <- c(reasons, r_reasons)
            }
        }
    }

    equiv_count <- 0
    not_equiv_count <- 0
    unknown_count <- 0
    for (mid in ids) {
        raw <- if (mid %in% names(verdicts)) verdicts[[mid]] else NA_character_
        cls <- classify_equivalence_verdict(raw)
        survived_mutants[[mid]]$equivalent <- cls$equivalent
        survived_mutants[[mid]]$equivalence_status <- cls$status
        if (isTRUE(cls$equivalent)) {
            equiv_count <- equiv_count + 1
            # Reasons are only requested (and stored) for EQUIVALENT mutants:
            # the rare, high-stakes calls worth auditing.
            if (mid %in% names(reasons)) {
                survived_mutants[[mid]]$equivalence_reason <- unname(reasons[[mid]])
                if (report) message(sprintf("  EQUIVALENT %s: %s", mid, reasons[[mid]]))
            } else if (report) {
                message(sprintf("  EQUIVALENT %s (no reason given)", mid))
            }
        } else if (isFALSE(cls$equivalent)) {
            not_equiv_count <- not_equiv_count + 1
        } else {
            unknown_count <- unknown_count + 1
        }
    }

    if (report) {
        message("Equivalence Analysis Summary:")
        message(sprintf("  Equivalent:     %d", equiv_count))
        message(sprintf("  Not Equivalent: %d", not_equiv_count))
        message(sprintf("  Uncertain:      %d", unknown_count))
    }

    # Record batch outcomes so a caller running many batches in parallel (where
    # the per-call warnings from forked workers may not surface) can report how
    # many failed and why. A failed batch produced no verdicts (NULL/empty), so
    # its mutants fell through to NA / "DONT KNOW" (Uncertain); the eq_error
    # attribute carries the cause (HTTP body, network error) where known.
    failed <- vapply(
        batch_results,
        function(r) is.null(r) || inherits(r, "try-error") || length(r) == 0,
        logical(1)
    )
    eq_errors <- unlist(
        lapply(batch_results, function(r) attr(r, "eq_error", exact = TRUE)),
        use.names = FALSE
    )
    attr(survived_mutants, "eq_n_batches") <- length(batches)
    attr(survived_mutants, "eq_failed_batches") <- sum(failed)
    attr(survived_mutants, "eq_errors") <- eq_errors

    return(survived_mutants)
}

#' Create a prompt for equivalent mutant detection
#'
#' Generates a well-formatted prompt for the OpenAI API to analyze
#' if mutants are equivalent to the original code.
#'
#' @param original_code String containing the original source code
#' @param mutant_details List of mutant details including IDs and mutation info
#'
#' @return A formatted prompt string for the OpenAI API
create_equivalent_mutant_prompt <- function(original_code, mutant_details) {
    ids <- vapply(mutant_details, function(m) as.character(m$id), character(1))

    mutant_info <- paste(vapply(mutant_details, function(m) {
        block <- paste0("- id: \"", m$id, "\"\n  change: ", m$mutation_info)
        if (!is.null(m$diff) && nzchar(m$diff)) {
            block <- paste0(block, "\n  diff:\n```diff\n", m$diff, "\n```")
        }
        if (!is.null(m$mutated_code) && nzchar(m$mutated_code)) {
            block <- paste0(block, "\n  mutated code:\n```r\n", m$mutated_code, "\n```")
        }
        block
    }, character(1)), collapse = "\n\n")

    prompt <- paste0(
        "You are given an original R function and a set of mutants of it. Each ",
        "mutant applies one small change to the original code, shown as a unified ",
        "diff under `diff:` (the `change:` line is a short label for it). Reason ",
        "about the edit shown in the diff, applied to the original code above.\n\n",
        "A mutant is EQUIVALENT only if it produces the same observable behaviour ",
        "as the original for every possible input -- identical return value, and ",
        "the same errors, warnings and side effects. It is NOT_EQUIVALENT if there ",
        "exists any input for which behaviour differs. If you cannot establish ",
        "either with confidence, answer DONT_KNOW.\n\n",
        "Reason about R semantics specifically: vectorisation and recycling, ",
        "NA/NULL handling, integer vs double, coercion rules, lazy evaluation, and ",
        "the difference between `&`/`&&` and `|`/`||`.\n\n",
        "Respond with JSON only -- no prose, no markdown code fences -- as an ",
        "object of exactly this shape:\n",
        "{\"results\": [{\"id\": \"<mutant id>\", ",
        "\"verdict\": \"EQUIVALENT\" | \"NOT_EQUIVALENT\" | \"DONT_KNOW\", ",
        "\"reason\": \"<one short sentence>\"}]}\n",
        "Include `reason` ONLY when the verdict is EQUIVALENT, explaining why the ",
        "mutant is behaviourally identical to the original for every input; omit ",
        "it for NOT_EQUIVALENT and DONT_KNOW.\n",
        "Include exactly one entry for each of these ids: ",
        paste(sprintf("\"%s\"", ids), collapse = ", "), ".\n\n",
        "Original code:\n```r\n", original_code, "\n```\n\n",
        "Mutants:\n", mutant_info, "\n"
    )

    return(prompt)
}

#' Call OpenAI API
#'
#' Makes a POST request to the OpenAI Chat Completions API.
#'
#' @param prompt The prompt to send to the API
#' @param config API configuration with key and model information
#'
#' @return On success, the parsed API response. On failure, an
#'   `openai_api_error` object: a list with a `message` describing the cause
#'   (HTTP status plus response body, or the network error), so callers can
#'   surface *why* a request failed rather than a bare `NULL`.
call_openai_api <- function(prompt, config) {
    api_error <- function(message) {
        structure(list(message = message), class = "openai_api_error")
    }
    tryCatch(
        {
            # Create the request body without temperature parameter
            request_body <- list(
                model = config$model,
                messages = list(
                    list(
                        role = "system",
                        content = paste0(
                            "You are a precise program-analysis assistant for the R ",
                            "language. You decide whether a mutant of an R function is ",
                            "semantically equivalent to the original across all possible ",
                            "inputs. You are conservative: when equivalence is not ",
                            "certain, you answer DONT_KNOW rather than guess. You reply ",
                            "with valid JSON only, with no prose or markdown."
                        )
                    ),
                    list(
                        role = "user",
                        content = prompt
                    )
                )
            )

            # Convert to JSON with proper settings
            json_body <- jsonlite::toJSON(request_body, auto_unbox = TRUE)

            # Resolve the endpoint from the (OpenAI-compatible) base URL so the
            # same code can target alternative providers.
            base_url <- config$base_url
            if (is.null(base_url) || !nzchar(base_url)) {
                base_url <- "https://api.openai.com/v1"
            }

            # Make the API request
            response <- httr::POST(
                url = build_chat_completions_url(base_url),
                httr::add_headers(
                    "Content-Type" = "application/json",
                    "Authorization" = paste("Bearer", config$api_key)
                ),
                body = json_body,
                encode = "json"
            )

            code <- httr::status_code(response)
            if (code == 200) {
                return(httr::content(response, as = "parsed", type = "application/json"))
            }
            # Keep the response body, not just the status reason: providers put
            # the actionable detail there (e.g. "Invalid model name passed in
            # model=qwen-3.5"). Trim so one bad batch cannot flood the console.
            body <- tryCatch(
                httr::content(response, "text", encoding = "UTF-8"),
                error = function(e) ""
            )
            body <- gsub("\\s+", " ", trimws(as.character(body)))
            if (nchar(body) > 300) body <- paste0(substr(body, 1, 300), "...")
            api_error(sprintf("HTTP %s%s", code, if (nzchar(body)) paste0(": ", body) else ""))
        },
        error = function(e) {
            api_error(conditionMessage(e))
        }
    )
}

# nocov end

# Internal store for configuration set programmatically via set_openai_config().
.openai_config_store <- new.env(parent = emptyenv())

# Default endpoint base for the OpenAI Chat Completions API.
.openai_default_base_url <- "https://api.openai.com/v1"

#' Set OpenAI API configuration for the current session
#'
#' Overrides the API key, model and/or base URL used by the equivalent-mutant
#' detection workflow. Values set here take precedence over a `.openai_config`
#' file and over environment variables. Arguments left `NULL` are unchanged.
#'
#' @param api_key API key string.
#' @param model Model name (e.g. `"gpt-4"`).
#' @param base_url Base URL of an OpenAI-compatible Chat Completions API, such
#'   as `"https://api.openai.com/v1"` or `"http://localhost:11434/v1"`.
#' @param max_parallel_requests Maximum number of equivalence-detection API
#'   requests to run concurrently. Use this to stay under a provider's
#'   per-key parallel-request limit (exceeding it returns HTTP 429). `NA`
#'   (the default) imposes no cap beyond the run's own `cores`.
#'
#' @return Invisibly, the resulting configuration (see [get_openai_config()]).
#'
#' @examples
#' set_openai_config(model = "gpt-4o-mini")
#' get_openai_config()$model
#' reset_openai_config()
#'
#' @export
set_openai_config <- function(api_key = NULL, model = NULL, base_url = NULL,
                              max_parallel_requests = NULL) {
    if (!is.null(api_key)) assign("api_key", as.character(api_key)[1], envir = .openai_config_store)
    if (!is.null(model)) assign("model", as.character(model)[1], envir = .openai_config_store)
    if (!is.null(base_url)) assign("base_url", as.character(base_url)[1], envir = .openai_config_store)
    if (!is.null(max_parallel_requests)) {
        assign("max_parallel_requests", as.integer(max_parallel_requests)[1], envir = .openai_config_store)
    }
    invisible(get_openai_config())
}

#' Clear session OpenAI configuration
#'
#' Removes any values set with [set_openai_config()], reverting to configuration
#' taken from a `.openai_config` file or environment variables.
#'
#' @return Invisibly `NULL`.
#'
#' @examples
#' set_openai_config(model = "gpt-4o-mini")
#' reset_openai_config()
#'
#' @export
reset_openai_config <- function() {
    rm(list = ls(envir = .openai_config_store, all.names = TRUE), envir = .openai_config_store)
    invisible(NULL)
}

#' Get OpenAI API configuration
#'
#' Resolves the API key, model and base URL used for equivalent-mutant
#' detection. Each field is resolved independently, with this precedence
#' (highest first):
#'
#' 1. values set with [set_openai_config()];
#' 2. a `.openai_config` file in `dir` (a human-readable "field: value" file
#'    that is parsed, never executed);
#' 3. the environment variables `OPENAI_API_KEY`, `OPENAI_MODEL`,
#'    `OPENAI_BASE_URL` and `OPENAI_MAX_PARALLEL_REQUESTS`;
#' 4. built-in defaults (model `"gpt-4"`, the public OpenAI base URL).
#'
#' @param dir Directory to search for a `.openai_config` file. Only this
#'   directory is consulted (parent directories are not). Defaults to the
#'   current working directory; pass `NULL` to ignore config files.
#'
#' @return A list with elements `api_key`, `model`, `base_url` and
#'   `max_parallel_requests` (an integer cap on concurrent API requests, or
#'   `NA` for no cap).
#'
#' @examples
#' config <- get_openai_config()
#' names(config)
#'
#' @export
get_openai_config <- function(dir = getwd()) {
    defaults <- list(
        api_key = "", model = "gpt-4", base_url = .openai_default_base_url,
        max_parallel_requests = NA_integer_
    )

    env_cfg <- list()
    if (nzchar(Sys.getenv("OPENAI_API_KEY"))) env_cfg$api_key <- Sys.getenv("OPENAI_API_KEY")
    if (nzchar(Sys.getenv("OPENAI_MODEL"))) env_cfg$model <- Sys.getenv("OPENAI_MODEL")
    if (nzchar(Sys.getenv("OPENAI_BASE_URL"))) env_cfg$base_url <- Sys.getenv("OPENAI_BASE_URL")
    if (nzchar(Sys.getenv("OPENAI_MAX_PARALLEL_REQUESTS"))) {
        env_cfg$max_parallel_requests <- Sys.getenv("OPENAI_MAX_PARALLEL_REQUESTS")
    }

    file_cfg <- if (!is.null(dir)) read_openai_config_file(dir) else list()
    store_cfg <- as.list(.openai_config_store)

    pick <- function(key) {
        if (!is.null(store_cfg[[key]])) {
            store_cfg[[key]]
        } else if (!is.null(file_cfg[[key]])) {
            file_cfg[[key]]
        } else if (!is.null(env_cfg[[key]])) {
            env_cfg[[key]]
        } else {
            defaults[[key]]
        }
    }

    # A cap on concurrent API requests (e.g. a provider's max_parallel_requests).
    # NA means "no cap": callers fall back to their own parallelism. Anything
    # not a positive whole number is treated as no cap.
    mpr <- suppressWarnings(as.integer(pick("max_parallel_requests")))
    if (length(mpr) != 1L || is.na(mpr) || mpr < 1L) mpr <- NA_integer_

    list(
        api_key = pick("api_key"), model = pick("model"), base_url = pick("base_url"),
        max_parallel_requests = mpr
    )
}

# Read a `.openai_config` file (DCF: "field: value" lines) from `dir`. Returns a
# named list of any of api_key/model/base_url/max_parallel_requests present
# (case-insensitive). The file is parsed, never executed, so it cannot run
# arbitrary code.
read_openai_config_file <- function(dir) {
    path <- file.path(dir, ".openai_config")
    if (!file.exists(path)) {
        return(list())
    }
    dcf <- tryCatch(read.dcf(path), error = function(e) NULL)
    if (is.null(dcf) || nrow(dcf) < 1L) {
        return(list())
    }

    fields <- tolower(colnames(dcf))
    out <- list()
    for (key in c("api_key", "model", "base_url", "max_parallel_requests")) {
        idx <- match(key, fields)
        if (!is.na(idx)) {
            value <- trimws(unname(dcf[1L, idx]))
            if (!is.na(value) && nzchar(value)) {
                out[[key]] <- value
            }
        }
    }
    out
}

# Build the Chat Completions endpoint from a base URL. Accepts either a base
# (".../v1") or an already-complete ".../chat/completions" URL.
build_chat_completions_url <- function(base_url) {
    base <- sub("/+$", "", base_url)
    if (grepl("/chat/completions$", base)) {
        base
    } else {
        paste0(base, "/chat/completions")
    }
}

# Best-effort query of an OpenAI-compatible proxy for this key's concurrency
# limit. LiteLLM (which this targets) serves `GET {root}/key/info`, returning
# `info$max_parallel_requests`. Returns a positive integer, or NA when
# unavailable (any error, a non-LiteLLM endpoint, or no limit set on the key).
# Never throws and uses a short timeout, so a slow or absent endpoint cannot
# stall a run; callers treat NA as "no auto-detected cap".
query_api_parallel_limit <- function(config) {
    tryCatch(
        {
            base <- config$base_url
            if (is.null(base) || !nzchar(base)) {
                return(NA_integer_)
            }
            # Admin routes live at the server root, not under /v1.
            root <- sub("/v1/?$", "", sub("/+$", "", base))
            resp <- httr::GET(
                paste0(root, "/key/info"),
                httr::add_headers(Authorization = paste("Bearer", config$api_key)),
                httr::timeout(5)
            )
            if (httr::status_code(resp) != 200) {
                return(NA_integer_)
            }
            parsed <- httr::content(resp, as = "parsed", type = "application/json")
            lim <- parsed$info$max_parallel_requests
            if (is.null(lim)) {
                return(NA_integer_)
            }
            lim <- suppressWarnings(as.integer(lim))
            if (length(lim) != 1L || is.na(lim) || lim < 1L) NA_integer_ else lim
        },
        error = function(e) NA_integer_
    )
}

# Build a minimal one-hunk unified diff between two vectors of lines. A mutant
# differs from the original in a single contiguous region, so a full LCS diff is
# unnecessary: we match the common prefix and suffix and emit only the region
# between them (with a few lines of context). Returns "" when the inputs are
# identical.
make_unified_diff <- function(orig_lines, mut_lines, context = 3L) {
    no <- length(orig_lines)
    nm <- length(mut_lines)

    p <- 0L
    while (p < min(no, nm) && identical(orig_lines[[p + 1L]], mut_lines[[p + 1L]])) {
        p <- p + 1L
    }
    s <- 0L
    while (s < (min(no, nm) - p) && identical(orig_lines[[no - s]], mut_lines[[nm - s]])) {
        s <- s + 1L
    }

    o_from <- p + 1L
    o_to <- no - s
    m_from <- p + 1L
    m_to <- nm - s
    o_changed <- if (o_from <= o_to) orig_lines[o_from:o_to] else character(0)
    m_changed <- if (m_from <= m_to) mut_lines[m_from:m_to] else character(0)
    if (length(o_changed) == 0 && length(m_changed) == 0) {
        return("")
    }

    ctx_start <- max(1L, p - context + 1L)
    ctx_before <- if (p >= ctx_start) orig_lines[ctx_start:p] else character(0)
    after_start <- no - s + 1L
    after_end <- min(no, after_start + context - 1L)
    ctx_after <- if (after_start <= after_end) orig_lines[after_start:after_end] else character(0)

    # Prefix each group with its marker, but only when non-empty: note that
    # paste0("+", character(0)) is "+" (not character(0)) in R, which would emit
    # a spurious line for pure deletions/insertions or empty context.
    marked <- function(marker, lines) if (length(lines)) paste0(marker, lines) else character(0)
    hunk <- c(
        sprintf("@@ -%d,%d +%d,%d @@", o_from, length(o_changed), m_from, length(m_changed)),
        marked(" ", ctx_before),
        marked("-", o_changed),
        marked("+", m_changed),
        marked(" ", ctx_after)
    )
    paste(hunk, collapse = "\n")
}

# Map a raw verdict token (from the model) to an equivalence flag and a stable
# display status. Matching is on letters only, so "NOT_EQUIVALENT",
# "NOT EQUIVALENT" and "not-equivalent" are treated identically, and anything
# unrecognised (including NA) is conservatively reported as uncertain.
classify_equivalence_verdict <- function(verdict) {
    token <- toupper(gsub("[^A-Za-z]", "", as.character(verdict)[1]))
    if (identical(token, "EQUIVALENT")) {
        list(equivalent = TRUE, status = "EQUIVALENT")
    } else if (identical(token, "NOTEQUIVALENT")) {
        list(equivalent = FALSE, status = "NOT EQUIVALENT")
    } else {
        list(equivalent = NA, status = "DONT KNOW")
    }
}

# Locate the outermost JSON array or object embedded in text, returning it as a
# string (or NULL if none is found). Whichever of '[' or '{' appears first wins.
extract_json_block <- function(text) {
    text <- as.character(text)[1]
    if (is.na(text)) {
        return(NULL)
    }
    arr <- regexpr("[", text, fixed = TRUE)
    obj <- regexpr("{", text, fixed = TRUE)

    if (arr > 0 && (obj < 0 || arr < obj)) {
        start <- arr
        close <- "]"
    } else if (obj > 0) {
        start <- obj
        close <- "}"
    } else {
        return(NULL)
    }

    hits <- gregexpr(close, text, fixed = TRUE)[[1]]
    end <- if (length(hits) == 1 && hits[1] == -1) -1L else max(hits)
    if (end > start) substr(text, start, end) else NULL
}

# Parse the model's response into a named character vector mapping mutant id ->
# raw verdict string. Returns NULL when no usable JSON can be recovered, so the
# caller can fall back to a line-based scan. Any per-id `reason` strings the
# model supplied (requested only for EQUIVALENT verdicts) are attached as the
# "reasons" attribute (a named character vector), so the return type stays a
# plain verdict vector for existing callers.
parse_equivalence_verdicts <- function(content) {
    if (is.null(content)) {
        return(NULL)
    }
    content <- as.character(content)[1]
    if (is.na(content) || !nzchar(content)) {
        return(NULL)
    }

    cleaned <- gsub("```[A-Za-z]*", "", content)
    cleaned <- gsub("```", "", cleaned)

    block <- extract_json_block(cleaned)
    if (is.null(block)) {
        return(NULL)
    }

    parsed <- tryCatch(
        jsonlite::fromJSON(block, simplifyDataFrame = TRUE),
        error = function(e) NULL
    )
    if (is.null(parsed)) {
        return(NULL)
    }

    records <- if (is.data.frame(parsed)) {
        parsed
    } else if (is.list(parsed) && is.data.frame(parsed$results)) {
        parsed$results
    } else if (is.list(parsed) && is.data.frame(parsed$mutants)) {
        parsed$mutants
    } else {
        NULL
    }

    if (is.null(records) || !all(c("id", "verdict") %in% names(records))) {
        return(NULL)
    }

    verdicts <- as.character(records$verdict)
    names(verdicts) <- as.character(records$id)

    if ("reason" %in% names(records)) {
        reasons <- as.character(records$reason)
        names(reasons) <- as.character(records$id)
        reasons <- reasons[!is.na(reasons) & nzchar(reasons)]
        if (length(reasons) > 0) {
            attr(verdicts, "reasons") <- reasons
        }
    }
    verdicts
}

# Strict fallback when the response is not valid JSON: for each id, scan lines,
# match the id literally (fixed = TRUE), and accept only a verdict found on the
# same line. Returns a named character vector (NA where no verdict was found).
fallback_line_verdicts <- function(content, ids) {
    out <- rep(NA_character_, length(ids))
    names(out) <- ids
    if (is.null(content)) {
        return(out)
    }
    content <- as.character(content)[1]
    if (is.na(content) || !nzchar(content)) {
        return(out)
    }

    lines <- strsplit(content, "\n", fixed = TRUE)[[1]]
    for (id in ids) {
        for (ln in lines) {
            if (!grepl(id, ln, fixed = TRUE)) {
                next
            }
            up <- toupper(ln)
            # Check NOT EQUIVALENT before EQUIVALENT (substring of the former).
            verdict <- if (grepl("NOT", up, fixed = TRUE) && grepl("EQUIVALENT", up, fixed = TRUE)) {
                "NOT_EQUIVALENT"
            } else if (grepl("EQUIVALENT", up, fixed = TRUE)) {
                "EQUIVALENT"
            } else if (grepl("KNOW", up, fixed = TRUE)) {
                "DONT_KNOW"
            } else {
                NA_character_
            }
            if (!is.na(verdict)) {
                out[[id]] <- verdict
                break
            }
        }
    }
    out
}
