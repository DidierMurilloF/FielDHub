test_that("lattice and incomplete-block designs are as efficient as the recorded baseline", {
  skip_on_cran()
  skip_unless_golden_platform()
  baseline <- readRDS(test_path("fixtures", "nested-goldens-ci.rds"))
  grid <- nested_golden_grid("ci")
  records <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
    g <- grid[i, ]
    iterations <- if (is.na(g$iterations)) NULL else g$iterations
    do.call(rbind, lapply(seq_len(g$seeds), function(seed) {
      nested_golden_record(g$caller, g$t, g$k, g$r, seed, iterations)
    }))
  }))
  res <- nested_acceptance(baseline, nested_summary(records), bootstrap = FALSE)
  # Strict expected failure, removed in Task 9 Step 5 of the phase 2 plan.
  # The fallback rule asks for 0 fallbacks wherever the degrees-of-freedom rule
  # holds, and the recorded baseline (blocksdesign) fell back on some seeds of
  # such shapes (onestage_nested 9/3/2). The pre-change build therefore fails
  # that rule against its own baseline, and on that rule only. The pending
  # shapes are read from the fixture. They must fail `fallback_ok` and pass
  # every other rule, so this test fails as soon as the engine reaches 0
  # fallbacks, and the block must then go.
  pending <- vapply(seq_len(nrow(baseline)), function(i) {
    baseline$caller[i] %in% nested_onestage_callers && isTRUE(baseline$fallbacks[i] > 0) &&
      onestage_rule_holds(baseline$t[i], baseline$k[i], baseline$r[i],
                          baseline$caller[i] == "onestage_latinized")
  }, logical(1))
  shapes <- function(rows) paste(utils::capture.output(print(res[rows, c("caller", "t", "k", "r")])), collapse = "\n")
  expect_true(all(res$pass[!pending] %in% c(TRUE, NA)),
              info = shapes(!pending & !is.na(res$pass) & !res$pass))
  expect_true(all(res$fallback_ok[pending] %in% FALSE),
              info = shapes(pending & !res$fallback_ok %in% FALSE))
  # noninferior_ok is NA in CI (no bootstrap)
  others <- res$worst_ok %in% TRUE & res$constant_ok %in% TRUE & res$optimum_ok %in% TRUE &
    res$errors_ok %in% TRUE & res$clash_ok %in% TRUE & res$noninferior_ok %in% c(TRUE, NA)
  expect_true(all(others[pending]), info = shapes(pending & !others))
})
