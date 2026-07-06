# Extract the arguments a package's tests/testthat.R harness passes to
# testthat::test_check(), so the testthat strategy can run exactly the tests the
# harness (and R CMD check) would. testthat::test_check(package, reporter, ...)
# is just test_dir("testthat", package = ..., reporter = ..., ..., load_package
# = "installed"); the only author-controlled behaviour we need to mirror lives
# in `...` (most commonly `filter`). Returns a named list of arguments to
# forward to testthat::test_dir(), with `package` and `reporter` removed (the
# mutator supplies its own reporter and loads the dev package via load_all()).
# Returns list() when there is no harness, no test_check() call, or the call
# cannot be parsed/evaluated from literals, in which case the full suite runs.
extract_harness_test_args <- function(harness_file) {
  if (!file.exists(harness_file)) {
    return(list())
  }
  exprs <- tryCatch(parse(harness_file), error = function(e) NULL)
  if (is.null(exprs)) {
    return(list())
  }

  is_test_check_call <- function(fn) {
    (is.symbol(fn) && identical(as.character(fn), "test_check")) ||
      (is.call(fn) && identical(fn[[1L]], as.name("::")) &&
        identical(as.character(fn[[3L]]), "test_check"))
  }

  for (e in exprs) {
    if (!is.call(e) || !is_test_check_call(e[[1L]])) {
      next
    }

    # Turn the test_check(...) call into a list(...) call so its arguments can be
    # captured, after stripping the ones the mutator controls. `package` is
    # either named or the first positional argument; `reporter` is always named.
    call_list <- e
    call_list[[1L]] <- quote(list)
    arg_names <- names(call_list)
    if (is.null(arg_names)) {
      arg_names <- rep("", length(call_list))
    }
    if ("reporter" %in% arg_names) {
      call_list[["reporter"]] <- NULL
      arg_names <- names(call_list)
    }
    if ("package" %in% arg_names) {
      call_list[["package"]] <- NULL
    } else {
      # First positional argument (index >= 2; index 1 is the `list` symbol).
      positional <- which(arg_names == "")
      positional <- positional[positional >= 2L]
      if (length(positional) > 0) {
        call_list[[positional[1L]]] <- NULL
      }
    }

    args <- tryCatch(eval(call_list, envir = baseenv()), error = function(e) NULL)
    if (is.null(args) || !is.list(args)) {
      return(list())
    }
    return(args)
  }

  list()
}

get_package_name <- function(pkg_path) {
  description_path <- file.path(pkg_path, "DESCRIPTION")
  if (!file.exists(description_path)) {
    stop("Cannot determine package name: DESCRIPTION file is missing.", call. = FALSE)
  }
  desc <- read.dcf(description_path)
  if (!"Package" %in% colnames(desc)) {
    stop("Cannot determine package name: DESCRIPTION has no 'Package' field.", call. = FALSE)
  }
  desc[1, "Package"]
}
