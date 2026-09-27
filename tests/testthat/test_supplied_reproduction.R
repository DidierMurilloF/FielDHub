supplied_reproduction_cases <- list(
  split_plot = list(reps = 2, type = 1, factorLabels = FALSE,
                    data = data.frame(W = c("Dry", "Wet", NA), S = c("A", "B", "C"))),
  split_split_plot = list(reps = 2, type = 1, factorLabels = FALSE,
                          data = data.frame(W = c("Dry", "Wet", NA),
                                            S = c("A", "B", "C"), SS = c("D", "E", "F"))),
  strip_plot = list(reps = 2, plotNumber = 101, factorLabels = FALSE,
                    data = data.frame(H = c("Dry", "Wet", NA), V = c("A", "B", "C"))),
  full_factorial = list(reps = 2, type = 1, factorLabels = FALSE,
                        data = data.frame(F = c("Water", "Water", "Dose", "Dose"),
                                          L = c("Low", "High", "Low", "High"))),
  RCBD_augmented = list(lines = 50, checks = 3, b = 5, year = 2026,
                        data = data.frame(ENTRY = 101:153, NAME = paste0("Custom ", 1:53))),
  optimized_arrangement = list(nrows = 12, ncols = 10, year = 2026,
                               data = data.frame(ENTRY = 101:204, NAME = paste0("Custom ", 1:104),
                                                 REPS = c(rep(5, 4), rep(1, 100)))),
  partially_replicated = list(nrows = 8, ncols = 8, year = 2026,
                              data = data.frame(ENTRY = 101:157, NAME = paste0("Custom ", 1:57),
                                                REPS = c(rep(2, 7), rep(1, 50)))),
  diagonal_arrangement = list(nrows = 15, ncols = 20, checks = 4, year = 2026,
                              data = data.frame(ENTRY = 101:374, NAME = paste0("Custom ", 1:274))),
  multi_location_prep = list(lines = 40, l = 3, copies_per_entry = 5, checks = 2,
                             rep_checks = c(4, 4), allow_fillers = TRUE, year = 2026,
                             data = data.frame(ENTRY = 101:142, NAME = paste0("Custom ", 1:42))),
  sparse_allocation = list(lines = 120, l = 4, copies_per_entry = 3, checks = 4, year = 2026,
                           data = data.frame(ENTRY = 101:224, NAME = paste0("Custom ", 1:124)))
)

for (engine in names(supplied_reproduction_cases)) local({
  fun <- engine
  inputs <- c(supplied_reproduction_cases[[fun]], list(seed = 38))
  test_that(paste(fun, "replays normalized supplied data"), {
    x <- do.call(fun, inputs)
    expect_type(x$metadata$parameters, "list")
    expect_identical(do.call(fun, x$metadata$parameters), x)
  })
})
