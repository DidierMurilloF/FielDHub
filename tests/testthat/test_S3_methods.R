library(FielDHub)

# Lines printed by print(summary(x)) between two numbered section headings.
summary_section <- function(x, from, to) {
  out <- utils::capture.output(print(summary(x)))
  start <- grep(from, out, fixed = TRUE)[1]
  end <- grep(to, out, fixed = TRUE)[1]
  out[(start + 1):(end - 1)]
}

test_that("print() and print(summary()) return the design invisibly", {
  # Regression test: print.FielDHub() returned the head() of the field book
  # instead of invisible(x).
  crd <- CRD(t = 4, reps = 3, seed = 1)
  utils::capture.output(res <- withVisible(print(crd)))
  expect_false(res$visible)
  expect_identical(res$value, crd)
  utils::capture.output(res <- withVisible(print(summary(crd))))
  expect_false(res$visible)
})

test_that("print(summary()) shows the data input of p-rep and optimized designs", {
  # Regression test: the summary read x$data_entry, but partially_replicated()
  # and optimized_arrangement() store the data input as x$dataEntry, so the
  # summary printed NULL.
  prep <- partially_replicated(nrows = 8, ncols = 8, repGens = c(50, 7),
                               repUnits = c(1, 2), planter = "cartesian",
                               seed = 1)
  expect_match(summary_section(prep, "4. Structure", "5. Structure"),
               "data.frame", all = FALSE)
  optim <- optimized_arrangement(nrows = 12, ncols = 10, lines = 100,
                                 amountChecks = 20, checks = 1:5, seed = 1)
  expect_match(summary_section(optim, "4. Structure", "5. Structure"),
               "data.frame", all = FALSE)
})

test_that("print(summary()) describes sparse allocation designs", {
  # Regression test: print.summary.FielDHub() had no branch for
  # id_design == "Sparse", so summary() printed nothing.
  sparse <- sparse_allocation(lines = 120, l = 4, copies_per_entry = 3,
                              checks = 4, seed = 1234)
  expect_output(print(summary(sparse)), "Sparse Allocation")
  expect_output(print(summary(sparse)), "field book")
})

test_that("print(summary()) describes multi-location p-rep designs", {
  # Regression test: print.summary.FielDHub() had no branch for
  # id_design == "MultiPrep", so summary() printed nothing.
  multi_prep <- multi_location_prep(lines = 80, l = 4, copies_per_entry = 5,
                                    checks = 2, rep_checks = c(4, 4),
                                    allow_fillers = TRUE, seed = 1)
  expect_output(print(summary(multi_prep)), "Multi-Location Partially Replicated")
  expect_match(summary_section(multi_prep, "3. Structure", "4. Structure"),
               "data.frame", all = FALSE)
})

test_that("plot() explains that split_families() results have no layout", {
  # Regression test: plot() on a split_families() result failed with
  # "object of type 'closure' is not subsettable".
  set.seed(77)
  gen_list <- data.frame(ENTRY = 1:200, NAME = paste0("SB-", 1:200),
                         FAMILY = sample(1:20, size = 200, replace = TRUE))
  families <- split_families(l = 2, data = gen_list)
  expect_error(plot(families), "split_families\\(\\) results have no field layout")
})
