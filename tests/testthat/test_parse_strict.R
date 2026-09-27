library(FielDHub)

test_that("numeric input parsers reject a trailing empty token", {
  for (text in c("2,", "2,  ", "2,3,")) {
    expect_false(FielDHub:::parse_whole_numbers(text, "Plots")$ok)
    expect_false(FielDHub:::parse_rep_checks(text, 2)$ok)
  }
})

test_that("text parsers report malformed scalar inputs without throwing", {
  for (text in list(NA_character_, c("1", "2"), list("1"))) {
    out <- FielDHub:::parse_whole_numbers(text, "Plots")
    expect_false(out$ok)
    expect_null(out$value)
    expect_type(out$message, "character")
    reps <- FielDHub:::parse_rep_checks(text, 2)
    expect_false(reps$ok)
    expect_null(reps$value)
  }
})

test_that("check counts reject nonnumeric, nonscalar and unrepresentable integers", {
  for (x in list("2", c(1, 2), Inf, .Machine$integer.max + 1)) {
    expect_warning(out <- FielDHub:::parse_n_checks(x), NA)
    expect_false(out$ok)
    expect_null(out$value)
  }
})

test_that("replication previews require finite positive whole numbers", {
  for (text in c("0", "-1", "1.5", "Inf", "NaN", "1e309")) {
    out <- FielDHub:::parse_rep_checks(text, 2)
    expect_false(out$ok)
    expect_null(out$value)
  }
})

test_that("replication parsing validates the number of checks before recycling", {
  for (n in list(NA_real_, 0, -1, 1.5, "2", c(1, 2))) {
    out <- FielDHub:::parse_rep_checks("2", n)
    expect_false(out$ok)
    expect_null(out$value)
  }
})
