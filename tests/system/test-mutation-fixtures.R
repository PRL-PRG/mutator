for (package in system_selected_packages()) {
  local({
    fixture <- package
    test_that(sprintf("mutation results for %s remain stable", fixture), {
      reference <- run_system_fixture_result(fixture)
      expect_snapshot_value(
        normalise_mutation_result(
          reference,
          file.path(SYSTEM_ROOT, "packages", "system", fixture)
        ),
        style = "json",
        variant = Sys.getenv("MUTATOR_SYSTEM_PROFILE", unset = "smoke")
      )

      # Some pinned fixtures intentionally snapshot a baseline error. There is
      # no mutation result to compare in that case; the snapshot remains the
      # regression check for the error itself.
      if (!inherits(reference, "error")) {
        variants <- system_invariance_variants(fixture)
        invariant_sample <- min(
          system_profile()$max_mutants,
          SYSTEM_INVARIANCE_MAX_MUTANTS
        )
        invariant_reference <- if (
          identical(invariant_sample, system_profile()$max_mutants)
        ) {
          reference
        } else {
          run_system_fixture_result(
            fixture,
            list(max_mutants = invariant_sample)
          )
        }
        for (variant in names(variants)) {
          candidate <- run_system_fixture_result(
            fixture,
            variants[[variant]]
          )
          expect_system_result_invariant(
            invariant_reference,
            candidate,
            variant
          )
        }
      }
    })
  })
}
