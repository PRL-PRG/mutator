test_that("create_mutant_package_copy links unchanged content and copies the mutated file", {
  # Exercises the copy helper directly (no baseline/subprocess run), so it pins
  # down exactly the symlink-vs-copy behaviour of a mutant package copy.
  # Sys.readlink-based assertions are not reliable on Windows CI.
  skip_on_os("windows")

  # Skip when symlinks are not supported in this environment.
  probe_dir <- tempfile()
  dir.create(probe_dir)
  probe_src <- file.path(probe_dir, "source.txt")
  probe_dst <- file.path(probe_dir, "target.txt")
  writeLines("probe", probe_src)
  symlink_supported <- isTRUE(tryCatch(file.symlink(probe_src, probe_dst), error = function(e) FALSE))
  unlink(probe_dir, recursive = TRUE)
  skip_if_not(symlink_supported)

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  pkg_dir <- file.path(temp_dir, "testMutatorLinks")
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  writeLines("Package: testMutatorLinks\nVersion: 0.1.0\nLicense: MIT",
    file.path(pkg_dir, "DESCRIPTION"))
  writeLines("f_add <- function(x, y) { x + y }", file.path(pkg_dir, "R", "f_add.R"))
  writeLines("f_sub <- function(x, y) { x - y }", file.path(pkg_dir, "R", "f_sub.R"))

  # A mutated version of f_add.R that the copy should materialise in place of it.
  mutated_file <- tempfile(fileext = ".R")
  writeLines("f_add <- function(x, y) { x - y }", mutated_file)

  mutant_pkg <- mutator:::create_mutant_package_copy(
    pkg_dir = pkg_dir,
    src_file = file.path(pkg_dir, "R", "f_add.R"),
    mutated_file = mutated_file,
    target_root = tempfile("mut_"),
    isolate = FALSE
  )

  # Unchanged top-level content is symlinked to the original.
  expect_true(nzchar(Sys.readlink(file.path(mutant_pkg, "DESCRIPTION"))))

  mutant_r_files <- list.files(file.path(mutant_pkg, "R"), pattern = "\\.R$", full.names = TRUE)
  symlink_count <- sum(nzchar(vapply(mutant_r_files, Sys.readlink, character(1))))
  # Exactly one file in R/ is copied (the mutated one); all others are linked.
  expect_equal(symlink_count, length(mutant_r_files) - 1)
  # The copied file carries the mutated content, not the original.
  expect_identical(
    readLines(file.path(mutant_pkg, "R", "f_add.R")),
    "f_add <- function(x, y) { x - y }"
  )
})

test_that("link_or_copy copies directories when symlinks are unavailable", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  source <- file.path(temp_dir, "source")
  target <- file.path(temp_dir, "target")
  dir.create(file.path(source, "nested"), recursive = TRUE)
  dir.create(file.path(source, "empty"))
  writeLines("copied", file.path(source, "nested", "file.txt"))

  expect_true(mutator:::link_or_copy(
    source,
    target,
    recursive = TRUE,
    link = function(...) FALSE
  ))
  expect_true(dir.exists(file.path(target, "empty")))
  expect_identical(
    readLines(file.path(target, "nested", "file.txt")),
    "copied"
  )
})

test_that("removing a linked package copy leaves the source tree intact", {
  # Safety net for the directory-linking of create_mutant_package_copy: tearing
  # down a mutant copy (as session cleanup does) must never follow a link into
  # the source and delete the originals. This matters most on Windows, where
  # inst/ is a Sys.junction; it runs on every platform so the junction path is
  # actually exercised on Windows CI.
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  pkg_dir <- file.path(temp_dir, "srcPkg")
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "inst", "extra"), recursive = TRUE)
  writeLines("Package: srcPkg\nVersion: 0.1.0\nLicense: MIT",
    file.path(pkg_dir, "DESCRIPTION"))
  writeLines("f <- function() TRUE", file.path(pkg_dir, "R", "f.R"))
  writeLines("keep me", file.path(pkg_dir, "inst", "extra", "data.txt"))

  mutated_file <- tempfile(fileext = ".R")
  writeLines("f <- function() FALSE", mutated_file)
  mutant_pkg <- mutator:::create_mutant_package_copy(
    pkg_dir = pkg_dir,
    src_file = file.path(pkg_dir, "R", "f.R"),
    mutated_file = mutated_file,
    target_root = tempfile("mut_")
  )

  # Tear the copy down the way callr/session cleanup would.
  unlink(dirname(mutant_pkg), recursive = TRUE, force = TRUE)

  # Anything reached through a link must survive the teardown.
  keep <- file.path(pkg_dir, "inst", "extra", "data.txt")
  expect_true(file.exists(keep))
  expect_identical(readLines(keep), "keep me")
  expect_true(file.exists(file.path(pkg_dir, "R", "f.R")))
})

test_that("create_mutant_package_copy deep-copies tests/ when isolate = TRUE", {
  skip_on_os("windows")

  # Skip when symlinks are not supported (otherwise everything is copied anyway
  # and the symlink-vs-copy distinction this test relies on does not exist).
  probe_dir <- tempfile()
  dir.create(probe_dir)
  probe_src <- file.path(probe_dir, "source.txt")
  probe_dst <- file.path(probe_dir, "target.txt")
  writeLines("probe", probe_src)
  symlink_supported <- isTRUE(tryCatch(file.symlink(probe_src, probe_dst), error = function(e) FALSE))
  unlink(probe_dir, recursive = TRUE)
  skip_if_not(symlink_supported)

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  pkg_dir <- file.path(temp_dir, "testMutatorIsolate")
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests", "testthat"), recursive = TRUE)
  writeLines("Package: testMutatorIsolate\nVersion: 0.1.0\nLicense: MIT",
    file.path(pkg_dir, "DESCRIPTION"))
  writeLines("f_add <- function(x, y) { x + y }", file.path(pkg_dir, "R", "f_add.R"))
  writeLines("test_that(\"add\", { expect_equal(f_add(1, 2), 3) })",
    file.path(pkg_dir, "tests", "testthat", "test-basic.R"))

  mutated_file <- tempfile(fileext = ".R")
  writeLines("f_add <- function(x, y) { x - y }", mutated_file)

  mutant_pkg <- mutator:::create_mutant_package_copy(
    pkg_dir = pkg_dir,
    src_file = file.path(pkg_dir, "R", "f_add.R"),
    mutated_file = mutated_file,
    target_root = tempfile("mut_"),
    isolate = TRUE,
    test_strategy = "testthat"
  )

  # DESCRIPTION (not in the isolate set) is still symlinked, but tests/ is a real
  # copied directory rather than a symlink to the shared original.
  expect_true(nzchar(Sys.readlink(file.path(mutant_pkg, "DESCRIPTION"))))
  expect_identical(Sys.readlink(file.path(mutant_pkg, "tests")), "")
  expect_true(dir.exists(file.path(mutant_pkg, "tests", "testthat")))
  expect_true(file.exists(file.path(mutant_pkg, "tests", "testthat", "test-basic.R")))
})

test_that("testthat copies cannot create snapshots in the source package", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  pkg_dir <- file.path(temp_dir, "testMutatorSnapshots")
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  dir.create(file.path(pkg_dir, "tests", "testthat"), recursive = TRUE)
  writeLines("Package: testMutatorSnapshots\nVersion: 0.1.0\nLicense: MIT",
    file.path(pkg_dir, "DESCRIPTION"))
  writeLines("f <- function() TRUE", file.path(pkg_dir, "R", "f.R"))
  writeLines("test_that(\"f\", expect_true(f()))",
    file.path(pkg_dir, "tests", "testthat", "test-f.R"))

  mutated_file <- tempfile(fileext = ".R")
  writeLines("f <- function() FALSE", mutated_file)
  mutant_pkg <- mutator:::create_mutant_package_copy(
    pkg_dir,
    file.path(pkg_dir, "R", "f.R"),
    mutated_file,
    tempfile("mut_"),
    test_strategy = "testthat"
  )

  expect_identical(Sys.readlink(file.path(mutant_pkg, "tests")), "")
  dir.create(file.path(mutant_pkg, "tests", "testthat", "_snaps"))
  expect_false(dir.exists(file.path(pkg_dir, "tests", "testthat", "_snaps")))
})
