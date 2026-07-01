library(FielDHub)

test_that("improve_efficiency() returns efficiencies when no swap improves the design", {
  # Regression test (CAL): improve_efficiency() only assigned best_efficiencies
  # inside the "new A-efficiency is higher" branch, so when no treatment swap
  # improved the design it referenced an undefined object at return() and errored
  # with "object 'best_efficiencies' not found".
  # This minimal design forces the no-improvement path two independent ways: with
  # one plot per (Level_1, Level_2) cell swap_treatments() cannot swap anything,
  # and the complete row blocks are already A-optimal (A-efficiency 1).
  nt <- 4L
  reps <- 2L
  design <- data.frame(
    Level_1    = factor(rep(seq_len(reps), each = nt)),
    Level_2    = factor(rep(seq_len(nt), times = reps)),
    Level_3    = factor(rep(paste0(seq_len(reps), ".B1"), each = nt)),
    plots      = seq_len(nt * reps),
    treatments = factor(rep(seq_len(nt), times = reps))
  )

  res <- improve_efficiency(design, iterations = 5, seed = 1)

  expect_type(res, "list")
  expect_named(res, c("best_design", "best_efficiencies", "best_a_efficiency"))
  expect_s3_class(res$best_efficiencies, "data.frame")
  expect_true("A-Efficiency" %in% names(res$best_efficiencies))
  # No swap improved the design, so the best design is the starting design.
  expect_equal(nrow(res$best_design), nrow(design))
})
