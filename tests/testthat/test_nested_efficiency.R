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
  failing <- res[!is.na(res$pass) & !res$pass, c("caller", "t", "k", "r")]
  expect_true(all(res$pass %in% c(TRUE, NA)),
              info = paste(utils::capture.output(print(failing)), collapse = "\n"))
})
