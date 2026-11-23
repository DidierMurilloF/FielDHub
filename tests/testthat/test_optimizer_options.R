test_that("these calls leave options() unchanged", {
  calls <- list(
    quote(incomplete_blocks(t = 12, k = 4, reps = 2, seed = 17)),
    quote(row_column(t = 12, nrows = 3, reps = 2, method = "twostage", seed = 22)),
    quote(row_column(t = 12, nrows = 3, reps = 2, method = "onestage", seed = 22)),
    quote(do_optim(design = "sparse", lines = 20, l = 4, copies_per_entry = 3,
                   add_checks = TRUE, checks = 2, seed = 1)),
    quote(multi_location_prep(lines = 80, l = 4, copies_per_entry = 5, checks = 2,
                              rep_checks = c(4, 4), allow_fillers = TRUE, seed = 1)),
    quote(sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4, seed = 1)))
  previous <- options("warn", "contrasts")
  on.exit(options(previous))
  for (call in calls) for (warn in c(-1, 1, 2)) {
    options(warn = warn, contrasts = c("contr.sum", "contr.helmert"))
    before <- options("warn", "contrasts")
    suppressWarnings(eval(call))
    expect_identical(options("warn", "contrasts"), before, info = deparse(call)[1])
  }
})
