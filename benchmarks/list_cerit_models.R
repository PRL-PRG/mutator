#!/usr/bin/env Rscript
#
# list_cerit_models.R -- inspect the configured CERIT/LiteLLM endpoint.
#
# Uses the same OpenAI-compatible configuration as equivalence detection, but
# never prints the API key.

args_all <- commandArgs(trailingOnly = FALSE)
this_file <- sub("^--file=", "", args_all[grep("^--file=", args_all)])
BENCH_DIR <- if (length(this_file)) dirname(normalizePath(this_file)) else
  file.path(getwd(), "benchmarks")
Sys.setenv(BENCH_ROOT = BENCH_DIR)

source(file.path(BENCH_DIR, "lib", "common.R"))
suppressWarnings(suppressMessages(pkgload::load_all(REPO_ROOT, quiet = TRUE)))

argv <- commandArgs(trailingOnly = TRUE)
as_json <- "--json" %in% argv

cfg <- get_openai_config()
if (!nzchar(cfg$api_key)) {
  stop("No OPENAI_API_KEY / .openai_config api_key available.", call. = FALSE)
}

base <- sub("/+$", "", cfg$base_url)
root <- sub("/v1/?$", "", base)

get_json <- function(url) {
  resp <- httr::GET(
    url,
    httr::add_headers(Authorization = paste("Bearer", cfg$api_key)),
    httr::timeout(20)
  )
  if (httr::status_code(resp) != 200) {
    body <- tryCatch(httr::content(resp, "text", encoding = "UTF-8"), error = function(e) "")
    stop(sprintf("GET %s failed with HTTP %s: %s", url, httr::status_code(resp), body),
      call. = FALSE)
  }
  httr::content(resp, as = "parsed", type = "application/json")
}

models_resp <- get_json(paste0(base, "/models"))
model_ids <- sort(unique(vapply(models_resp$data, function(m) as.character(m$id %||% ""), character(1))))
model_ids <- model_ids[nzchar(model_ids)]

info_resp <- tryCatch(get_json(paste0(root, "/model/info")), error = function(e) NULL)
info_by_model <- list()
if (!is.null(info_resp$data)) {
  for (m in info_resp$data) {
    nm <- as.character(m$model_name %||% "")
    if (!nzchar(nm)) next
    info_by_model[[nm]] <- m
  }
}

non_chat_re <- paste(c(
  "embed", "rerank", "whisper", "audio", "transcription",
  "all-proxy-models", "proxy"
), collapse = "|")
task_alias_re <- "^(agentic|coder|mini|thinker)$"

rows <- lapply(model_ids, function(id) {
  inf <- info_by_model[[id]]
  mi <- if (!is.null(inf)) inf$model_info else list()
  lp <- if (!is.null(inf)) inf$litellm_params else list()
  mode <- as.character(mi$mode %||% "")
  provider <- as.character(mi$litellm_provider %||% lp$custom_llm_provider %||% "")
  include <- !(grepl(non_chat_re, id, ignore.case = TRUE) ||
    grepl(non_chat_re, mode, ignore.case = TRUE) ||
    grepl(task_alias_re, id, ignore.case = TRUE))
  data.frame(
    model = id,
    mode = mode,
    provider = provider,
    curated_default = id %in% EQ_DEFAULT_MODELS,
    included_by_filter = include,
    stringsAsFactors = FALSE
  )
})

out <- do.call(rbind, rows)
out <- out[order(!out$curated_default, !out$included_by_filter, out$model), ]

if (as_json) {
  jsonlite::write_json(out, stdout(), pretty = TRUE, auto_unbox = TRUE)
  cat("\n")
} else {
  cat(sprintf("base_url=%s\n", cfg$base_url))
  cat(sprintf("configured_model=%s\n\n", cfg$model))
  print(out, row.names = FALSE)
}
