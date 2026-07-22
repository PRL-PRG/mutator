## R CMD check results

0 errors | 0 warnings | 1 note

* This is a resubmission of a new package. We removed the default output path from
  `mutate_file()`, so callers must explicitly select where generated files are
  written. All examples and tests write only within the session temporary
  directory. We also updated every working-directory change to register its
  restoration immediately with `on.exit()`.

* Since the original 0.1.0 submission, the package has also gained first-class
  tinytest support, explicit test-strategy selection, and new or expanded
  vignettes covering package mutation, configuration, and continuous
  integration. These changes and the accompanying fixes are detailed in
  `NEWS.md`.

* The note "Suggests or Enhances not in mainstream repositories: imputesrcref"
  is expected. 'imputesrcref' is an optional enhancement available only from
  GitHub (<https://github.com/PRL-PRG/imputesrcref>); it is used to refine
  reported mutation source locations when present. It is listed under Enhances
  (not Imports/Suggests) and is accessed strictly conditionally via
  requireNamespace(); the package is fully functional and all tests pass when it
  is not installed.

## Reverse dependencies

There are currently no downstream dependencies for this package.

## References

There are no external references describing the implementation methods for this
initial submission.
