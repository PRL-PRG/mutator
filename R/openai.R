# nocov start
#' Identify equivalent mutants using OpenAI API
#'
#' Analyzes survived mutants to determine if they are functionally equivalent
#' to the original code using OpenAI's language models.
#'
#' @param src_file Path to the original source file
#' @param survived_mutants List of mutants that survived test execution
#' @param api_config Optional API configuration (will be loaded if NULL)
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
identify_equivalent_mutants <- function(src_file, survived_mutants, api_config = NULL) {
    # Load API configuration if not provided
    if (is.null(api_config)) {
        api_config <- get_openai_config()
    }

    # If no API key is available, return early
    if (is.null(api_config$api_key) || api_config$api_key == "") {
        warning("OpenAI API key not found. Skipping equivalent mutant detection.")
        return(survived_mutants)
    }

    # Read original source code
    orig_code <- paste(readLines(src_file), collapse = "\n")

    # Group mutants by source file
    mutants_by_file <- list()
    for (id in names(survived_mutants)) {
        file_name <- strsplit(id, "_")[[1]][1]
        if (is.null(mutants_by_file[[file_name]])) {
            mutants_by_file[[file_name]] <- list()
        }
        mutants_by_file[[file_name]][[id]] <- survived_mutants[[id]]
    }

    # Track counts for each category
    equiv_count <- 0
    not_equiv_count <- 0
    unknown_count <- 0

    # Process each source file
    for (file_name in names(mutants_by_file)) {
        file_mutants <- mutants_by_file[[file_name]]

        # Prepare mutant information for the prompt
        mutant_details <- lapply(names(file_mutants), function(mid) {
            list(
                id = mid,
                mutation_info = file_mutants[[mid]]$mutation_info
            )
        })

        # Create the prompt
        prompt <- create_equivalent_mutant_prompt(orig_code, mutant_details)

        cat("\nAnalyzing mutants with OpenAI API...\n")
        cat("Prompt being sent to OpenAI:\n")
        cat("----------------------------------------\n")
        cat(prompt)
        cat("\n----------------------------------------\n\n")

        # Call OpenAI API
        response <- call_openai_api(prompt, api_config)

        # Process response
        if (!is.null(response)) {
            parsed <- response

            cat("Answer received from OpenAI API\n")
            cat("----------------------------------------\n")
            cat(parsed$choices[[1]]$message$content)
            cat("\n----------------------------------------\n\n")

            if (!is.null(parsed$choices) && length(parsed$choices) > 0) {
                # Extract equivalent mutants information from response
                equivalent_analysis <- parsed$choices[[1]]$message$content

                # Update the mutants with equivalence information
                for (mid in names(file_mutants)) {
                    if (grepl(paste0(mid, ".*EQUIVALENT"), equivalent_analysis, ignore.case = TRUE) &&
                        !grepl(paste0(mid, ".*NOT EQUIVALENT"), equivalent_analysis, ignore.case = TRUE)) {
                        survived_mutants[[mid]]$equivalent <- TRUE
                        survived_mutants[[mid]]$equivalence_status <- "EQUIVALENT"
                        equiv_count <- equiv_count + 1
                        cat(sprintf("Mutant %s identified as EQUIVALENT\n", mid))
                    } else if (grepl(paste0(mid, ".*NOT EQUIVALENT"), equivalent_analysis, ignore.case = TRUE)) {
                        survived_mutants[[mid]]$equivalent <- FALSE
                        survived_mutants[[mid]]$equivalence_status <- "NOT EQUIVALENT"
                        not_equiv_count <- not_equiv_count + 1
                        cat(sprintf("Mutant %s identified as NOT EQUIVALENT\n", mid))
                    } else if (grepl(paste0(mid, ".*DONT KNOW"), equivalent_analysis, ignore.case = TRUE)) {
                        survived_mutants[[mid]]$equivalent <- NA
                        survived_mutants[[mid]]$equivalence_status <- "DONT KNOW"
                        unknown_count <- unknown_count + 1
                        cat(sprintf("Mutant %s: DONT KNOW\n", mid))
                    } else {
                        # Default to unknown if no clear determination
                        survived_mutants[[mid]]$equivalent <- NA
                        survived_mutants[[mid]]$equivalence_status <- "DONT KNOW"
                        unknown_count <- unknown_count + 1
                        cat(sprintf("Mutant %s: No clear determination, marking as DONT KNOW\n", mid))
                    }
                }
            }
        }
    }

    cat("\nEquivalence Analysis Summary:\n")
    cat(sprintf("  Equivalent:     %d\n", equiv_count))
    cat(sprintf("  Not Equivalent: %d\n", not_equiv_count))
    cat(sprintf("  Uncertain:      %d\n", unknown_count))
    cat("\n")

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
    mutant_info <- paste(sapply(mutant_details, function(m) {
        paste0("Mutant ID: ", m$id, "\nMutation: ", m$mutation_info, "\n")
    }), collapse = "\n")

    prompt <- paste0(
        "Determine if the following mutants are equivalent to the original code. ",
        "An equivalent mutant has the same behavior as the original code under all ",
        "possible inputs.\n\n",
        "Be conservative in your assessment. For each mutant, respond with one of these options:\n",
        "- 'EQUIVALENT': Only if you are certain the mutant is functionally identical to the original code\n",
        "- 'NOT EQUIVALENT': Only if you are certain the mutant changes behavior for some inputs\n",
        "- 'DONT KNOW': If you are uncertain or cannot determine equivalence\n\n",
        "Only answer with certainty if you are sure. If there's any doubt, use 'DONT KNOW'.\n\n",
        "Original code:\n```\n", original_code, "\n```\n\n",
        "Survived mutants:\n", mutant_info
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
#' @return API response as text, or NULL if request failed
call_openai_api <- function(prompt, config) {
    tryCatch(
        {
            # Create the request body without temperature parameter
            request_body <- list(
                model = config$model,
                messages = list(
                    list(
                        role = "system",
                        content = paste0(
                            "You are an expert in program analysis, ",
                            "particularly in identifying equivalent mutants in code."
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

            # Make the API request
            response <- httr::POST(
                url = "https://api.openai.com/v1/chat/completions",
                httr::add_headers(
                    "Content-Type" = "application/json",
                    "Authorization" = paste("Bearer", config$api_key)
                ),
                body = json_body,
                encode = "json"
            )

            # Check for HTTP errors
            httr::stop_for_status(response)

            if (httr::status_code(response) == 200) {
                return(httr::content(response, as = "parsed", type = "application/json"))
            } else {
                warning(
                    "OpenAI API error: ",
                    httr::content(response, "text", encoding = "UTF-8")
                )
                return(NULL)
            }
        },
        error = function(e) {
            warning("Error calling OpenAI API: ", e$message)
            return(NULL)
        }
    )
}

#' Get OpenAI API configuration
#'
#' Retrieves API key and model configuration from environment variables
#' or a configuration file.
#'
#' @return List containing api_key and model values
#'
#' @examples
#' config <- get_openai_config()
#' names(config)
#'
#' @export
get_openai_config <- function() {
    api_key <- Sys.getenv("OPENAI_API_KEY", "")
    model <- Sys.getenv("OPENAI_MODEL", "gpt-4")

    if (api_key == "") {
        # candidates: .openai_config.R and .openai_config.R.template
        candidates <- c(".openai_config.R", ".openai_config.R.template")
        wd <- normalizePath(getwd())
        config_path <- NULL

        # walk up until we hit root
        repeat {
            for (f in candidates) {
                p <- file.path(wd, f)
                if (file.exists(p)) {
                    config_path <- p
                    break
                }
            }
            if (!is.null(config_path)) break
            parent <- dirname(wd)
            if (parent == wd) break
            wd <- parent
        }

        if (!is.null(config_path)) {
            config_env <- new.env()
            try(source(config_path, local = config_env), silent = TRUE)

            # pick up either lowercase or uppercase var names
            if (exists("api_key", envir = config_env)) api_key <- get("api_key", envir = config_env)
            if (exists("OPENAI_API_KEY", envir = config_env)) api_key <- get("OPENAI_API_KEY", envir = config_env)
            if (exists("model", envir = config_env)) model <- get("model", envir = config_env)
            if (exists("OPENAI_MODEL", envir = config_env)) model <- get("OPENAI_MODEL", envir = config_env)
        }
    }

    list(api_key = api_key, model = model)
}
# nocov end
