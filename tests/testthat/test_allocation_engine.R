# do_optim() allocates with the block engine (spec-m5): no blocksdesign, even
# allocations, the seed contract, and results that must not change.

alloc_optim <- function(lines, l, copies, seed, ...) {
  design <- if (copies < l) "sparse" else "prep"
  do_optim(design = design, lines = lines, l = l, copies_per_entry = copies,
           add_checks = design == "sparse", checks = if (design == "sparse") 1 else NULL,
           force_balance = FALSE, seed = seed, ...)
}
alloc_is_even <- function(x, copies, l) {
  N <- as.matrix(x$allocation)
  low <- copies %/% l
  all(rowSums(N) == copies) && all(N >= low & N <= low + 1) &&
    diff(range(colSums(N))) <= 1
}

test_that("sparse and p-rep allocations never call blocksdesign", {
  local_mocked_bindings(blocks = function(...) stop("blocksdesign::blocks() was called"),
                        .package = "blocksdesign")
  data_sparse <- data.frame(ENTRY = 1:64, NAME = c(paste0("CH", 1:4), paste0("L", 1:60)))
  data_prep <- data.frame(ENTRY = 1:60, NAME = paste0("L", 1:60))
  expect_s3_class(do_optim("sparse", 60, 4, 3, add_checks = TRUE, checks = 4, seed = 1), "Sparse")
  expect_s3_class(do_optim("sparse", 60, 4, 3, add_checks = TRUE, checks = 4, seed = 1,
                           data = data_sparse), "Sparse")
  expect_s3_class(do_optim("prep", 60, 3, 4, seed = 2), "MultiPrep")
  expect_s3_class(do_optim("prep", 60, 3, 4, seed = 2, data = data_prep), "MultiPrep")
  expect_s3_class(do_optim("prep", 60, 3, 4, add_checks = TRUE, checks = 2,
                           rep_checks = c(2, 2), seed = 2), "MultiPrep")
  expect_s3_class(do_optim("prep", 20, 1, 3, seed = 3), "MultiPrep")
  expect_s3_class(do_optim("sparse", 20, 1, 1, add_checks = TRUE, checks = 2, seed = 3), "Sparse")
  expect_s3_class(sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4,
                                    seed = 4), "fieldhub_sparse_allocation")
  expect_s3_class(multi_location_prep(lines = 150, l = 3, copies_per_entry = 4, checks = 4,
                                      rep_checks = c(2, 2, 2, 2), seed = 5),
                  "fieldhub_multi_location_prep")
  expect_s3_class(multi_location_prep(lines = 150, l = 1, copies_per_entry = 2, checks = 4,
                                      rep_checks = c(2, 2, 2, 2), seed = 6),
                  "fieldhub_multi_location_prep")
})

test_that("allocations are even for every number of locations and copies", {
  skip_on_cran()
  for (v in c(2, 20, 120)) for (l in 2:8) for (copies in setdiff(seq_len(2 * l - 1), l)) {
    if (v * copies < 2 * l || v + l - 1 > v * copies) next
    x <- alloc_optim(v, l, copies, seed = v + 10 * l + copies)
    expect_true(alloc_is_even(x, copies, l), label = sprintf("(%d, %d, %d)", v, l, copies))
  }
  for (x in list(c(50, 10, 9), c(50, 10, 11), c(50, 10, 19), c(100, 20, 39), c(60, 3, 7),
                 c(60, 4, 9), c(60, 4, 12))) {
    y <- alloc_optim(x[1], x[2], x[3], seed = 1)
    expect_true(alloc_is_even(y, x[3], x[2]), label = paste(x, collapse = ", "))
  }
  # sparse with as many copies as locations or more: do_optim() accepts it
  y <- do_optim("sparse", 40, 3, 4, add_checks = TRUE, checks = 1, force_balance = FALSE, seed = 1)
  expect_true(alloc_is_even(y, 4, 3))
  # one location: every copy there
  expect_true(alloc_is_even(alloc_optim(30, 1, 3, seed = 1), 3, 1))
})

test_that("the smallest designs (two plots per location) are even", {
  for (l in 3:6) {
    x <- alloc_optim(l, l, 2, seed = l)
    expect_true(alloc_is_even(x, 2, l))
  }
})

test_that("an uneven search result is refused as an internal error", {
  local_mocked_bindings(blocks_engine = function(treatments, replicates, blocks, ...) {
    lines <- rep(seq_len(treatments), each = replicates)
    list(Design = data.frame(
      Level_1 = factor(paste0("B", rep(seq_len(blocks), each = length(lines) / blocks)),
                       levels = paste0("B", seq_len(blocks))),
      treatments = factor(sort(lines), levels = seq_len(treatments))),
      search = list(stop_reason = "budget", evaluations = 1, even_repairs = 0L))
  })
  expect_error(do_optim("prep", 10, 2, 4, seed = 1), class = "fieldhub_internal_error")
})

test_that("the search outcome is recorded outside the replay parameters", {
  x <- do_optim("prep", 50, 4, 7, seed = 3)
  rec <- x$metadata$allocation_search
  expect_named(rec, c("stop_reason", "evaluations", "even_repairs"))
  expect_true(rec$stop_reason %in% c("budget", "stale", "time_limit"))
  expect_gt(rec$evaluations, 0)
  expect_type(rec$even_repairs, "integer")
  expect_false("allocation_search" %in% names(x$metadata$parameters))
  expect_length(fieldhub_metadata_problems(x$metadata), 0)
  one <- do_optim("prep", 20, 1, 3, seed = 3)$metadata$allocation_search
  expect_identical(one, list(stop_reason = "one_location", evaluations = 0, even_repairs = 0L))
})

test_that("the same seed gives the same allocation, and the recorded call rebuilds it", {
  a <- do_optim("sparse", 120, 4, 3, add_checks = TRUE, checks = 4, seed = 15)
  b <- do_optim("sparse", 120, 4, 3, add_checks = TRUE, checks = 4, seed = 15)
  expect_identical(a, b)
  expect_identical(do.call(do_optim, a$metadata$parameters), a)
  p <- do_optim("prep", 80, 3, 5, seed = 9)
  expect_identical(do.call(do_optim, p$metadata$parameters), p)
})

test_that("the caller's random stream is restored after an allocation", {
  set.seed(77)
  expected <- runif(3)
  set.seed(77)
  do_optim("prep", 60, 3, 4, seed = 1)
  expect_identical(runif(3), expected)
})

test_that("allocations passed in, and one-location allocations, are unchanged from 1.7.0", {
  skip_on_cran()  # the fixture was recorded on Linux
  base <- readRDS(test_path("fixtures", "m5-base-allocations.rds"))
  expect_identical(
    sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4,
                      sparse_list = base$sparse_list, seed = 31, year = 2026)$fieldBook,
    base$sparse_given)
  expect_identical(
    multi_location_prep(lines = 150, l = 3, copies_per_entry = 4, checks = 4,
                        rep_checks = c(2, 2, 2, 2), optim_list = base$prep_list,
                        seed = 32, year = 2026)$fieldBook,
    base$prep_given)
  expect_identical(
    multi_location_prep(lines = 150, l = 1, copies_per_entry = 2, checks = 4,
                        rep_checks = c(2, 2, 2, 2), seed = 11, year = 2026)$fieldBook,
    base$prep_one_location)
  expect_identical(do_optim("prep", lines = 20, l = 1, copies_per_entry = 3, seed = 5)$allocation,
                   base$optim_one_prep)
  expect_identical(do_optim("sparse", lines = 20, l = 1, copies_per_entry = 1, add_checks = TRUE,
                            checks = 2, seed = 6)$allocation,
                   base$optim_one_sparse)
})

test_that("allocation sizes the search cannot take are refused in the user's terms", {
  expect_error(do_optim("prep", 1, 2, 3, seed = 1), "at least 2 lines",
               class = "fieldhub_input_error")
  expect_error(do_optim("sparse", 20, 1001, 2, add_checks = TRUE, checks = 1, seed = 1),
               "at most 1000 locations", class = "fieldhub_input_error")
  expect_error(do_optim("sparse", 2e5, 600, 2, add_checks = TRUE, checks = 1, seed = 1),
               "line-location pairs", class = "fieldhub_input_error")
})
