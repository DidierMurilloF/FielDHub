test_that("optimizer-backed engines restore caller warning and contrast options", {
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

test_that("optimizer option restoration also runs after errors", {
  previous <- options("warn", "contrasts")
  on.exit(options(previous))
  options(warn = 1, contrasts = c("contr.sum", "contr.poly"))
  before <- options("warn", "contrasts")
  expect_error(do_optim(design = "sparse", lines = 2, l = 10, copies_per_entry = 1,
                        add_checks = TRUE, checks = 2, seed = 1))
  expect_identical(options("warn", "contrasts"), before)
})

test_that("the scoped optimizer guard preserves unrelated options and nesting", {
  previous <- options("warn", "contrasts", "fieldhub.test.option")
  on.exit(options(previous))
  options(warn = 1, contrasts = c("contr.sum", "contr.poly"))
  inner <- function() {
    local_optimizer_options()
    options(warn = -1, contrasts = c("contr.helmert", "contr.poly"))
  }
  outer <- function() {
    local_optimizer_options()
    options(warn = 0, contrasts = c("contr.treatment", "contr.poly"))
    inner()
    expect_identical(getOption("warn"), 0L)
    expect_identical(getOption("contrasts"), c("contr.treatment", "contr.poly"))
    options(fieldhub.test.option = "retained")
  }
  outer()
  expect_identical(getOption("warn"), 1L)
  expect_identical(getOption("contrasts"), c("contr.sum", "contr.poly"))
  expect_identical(getOption("fieldhub.test.option"), "retained")
})
