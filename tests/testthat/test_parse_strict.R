library(FielDHub)

test_that("numeric input parsers reject a trailing empty token", {
  for (text in c("2,", "2,  ", "2,3,")) {
    expect_error(FielDHub:::parse_whole_numbers(text, "Plots"), class = "fieldhub_input_error")
    expect_error(FielDHub:::parse_rep_checks(text, 2), class = "fieldhub_input_error")
  }
})

test_that("text parsers report malformed scalar inputs as classed errors", {
  for (text in list(NA_character_, c("1", "2"), list("1"))) {
    err <- expect_error(FielDHub:::parse_whole_numbers(text, "Plots"),
                        class = "fieldhub_input_error")
    expect_match(conditionMessage(err), "Plots", fixed = TRUE)
    expect_error(FielDHub:::parse_rep_checks(text, 2), class = "fieldhub_input_error")
  }
})

test_that("check counts reject nonnumeric, nonscalar and unrepresentable integers", {
  for (x in list("2", c(1, 2), Inf, .Machine$integer.max + 1)) {
    expect_warning(
      expect_error(FielDHub:::parse_n_checks(x), class = "fieldhub_input_error"),
      NA
    )
  }
})

test_that("replication previews require finite positive whole numbers", {
  for (text in c("0", "-1", "1.5", "Inf", "NaN", "1e309")) {
    expect_error(FielDHub:::parse_rep_checks(text, 2), class = "fieldhub_input_error")
  }
})

test_that("replication parsing validates the number of checks before recycling", {
  for (n in list(NA_real_, 0, -1, 1.5, "2", c(1, 2))) {
    expect_error(FielDHub:::parse_rep_checks("2", n), "# of Checks",
                 class = "fieldhub_input_error")
  }
})

test_that("p-rep group inputs are read as whole numbers of the same length", {
  groups <- FielDHub:::parse_rep_groups("75,150", "2,1")
  expect_identical(groups$repGens, c(75, 150))
  expect_identical(groups$repUnits, c(2, 1))
  expect_identical(groups$total_plots, 300)

  # Regression test: "75,abc" became c(75, NA), and the p-rep design then
  # failed with a raw R error from 1:sum(NA).
  err <- expect_error(FielDHub:::parse_rep_groups("75,abc", "2,1"),
                      class = "fieldhub_input_error")
  expect_match(conditionMessage(err), "# of Entries Per Rep Group could not read \"abc\"",
               fixed = TRUE)
  expect_error(FielDHub:::parse_rep_groups("75,150", "2,x"), "# of Rep Per Group",
               class = "fieldhub_input_error")
  expect_error(FielDHub:::parse_rep_groups("75,150", "2"), "same number of values",
               class = "fieldhub_input_error")
  expect_error(FielDHub:::parse_rep_groups("", "2"), "blank", class = "fieldhub_input_error")
})
