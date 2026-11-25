# do_optim(), sparse_allocation() and multi_location_prep(): allocation of
# entry copies to locations.

alloc_optim <- function(lines, l, copies, seed, ...) {
  design <- if (copies < l) "sparse" else "prep"
  do_optim(design = design, lines = lines, l = l, copies_per_entry = copies,
           add_checks = design == "sparse", checks = if (design == "sparse") 1 else NULL,
           force_balance = FALSE, seed = seed, ...)
}

# Even: every entry has all its copies, floor(copies / l) or one more at each
# location, and location totals differ by at most one
alloc_is_even <- function(x, copies, l) {
  N <- as.matrix(x$allocation)
  low <- copies %/% l
  all(rowSums(N) == copies) && all(N >= low & N <= low + 1) && diff(range(colSums(N))) <= 1
}

# Signals of a call: TRUE when it raises `class` and leaves the caller's stream
signals_without_draw <- function(call, class) {
  set.seed(819)
  before <- .Random.seed
  err <- tryCatch({ eval(call); NULL }, error = identity)
  inherits(err, class) && identical(.Random.seed, before)
}

# ---- Every entry point builds ----

test_that("sparse and p-rep allocations build from every entry point", {
  data_sparse <- data.frame(ENTRY = 1:64, NAME = c(paste0("CH", 1:4), paste0("L", 1:60)))
  data_prep <- data.frame(ENTRY = 1:60, NAME = paste0("L", 1:60))
  cases <- list(
    sparse = list(quote(do_optim("sparse", 60, 4, 3, add_checks = TRUE, checks = 4, seed = 1)), "Sparse"),
    sparse_data = list(quote(do_optim("sparse", 60, 4, 3, add_checks = TRUE, checks = 4, seed = 1,
                                      data = data_sparse)), "Sparse"),
    prep = list(quote(do_optim("prep", 60, 3, 4, seed = 2)), "MultiPrep"),
    prep_data = list(quote(do_optim("prep", 60, 3, 4, seed = 2, data = data_prep)), "MultiPrep"),
    prep_checks = list(quote(do_optim("prep", 60, 3, 4, add_checks = TRUE, checks = 2, rep_checks = c(2, 2),
                                      seed = 2)), "MultiPrep"),
    prep_one_location = list(quote(do_optim("prep", 20, 1, 3, seed = 3)), "MultiPrep"),
    sparse_one_location = list(quote(do_optim("sparse", 20, 1, 1, add_checks = TRUE, checks = 2, seed = 3)),
                               "Sparse"),
    sparse_allocation = list(quote(sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4,
                                                     seed = 4)), "fieldhub_sparse_allocation"),
    multi_location_prep = list(quote(multi_location_prep(lines = 150, l = 3, copies_per_entry = 4, checks = 4,
                                                         rep_checks = c(2, 2, 2, 2), seed = 5)),
                               "fieldhub_multi_location_prep"),
    multi_location_prep_one = list(quote(multi_location_prep(lines = 150, l = 1, copies_per_entry = 2,
                                                             checks = 4, rep_checks = c(2, 2, 2, 2), seed = 6)),
                                   "fieldhub_multi_location_prep")
  )
  for (nm in names(cases)) expect_s3_class(eval(cases[[nm]][[1]]), cases[[nm]][[2]])
})

# ---- Allocations are even ----

test_that("allocations are even for every number of locations and copies", {
  skip_on_cran()
  grid <- expand.grid(v = c(2, 20, 120), l = 2:8, copies = 1:15)
  grid <- grid[grid$copies <= 2 * grid$l - 1 & grid$copies != grid$l &
                 grid$v * grid$copies >= 2 * grid$l & grid$v + grid$l - 1 <= grid$v * grid$copies, ]
  grid$seed <- grid$v + 10 * grid$l + grid$copies
  extra <- data.frame(v = c(50, 50, 50, 100, 60, 60, 60, 30), l = c(10, 10, 10, 20, 3, 4, 4, 1),
                      copies = c(9, 11, 19, 39, 7, 9, 12, 3), seed = 1)
  grid <- rbind(grid, extra)
  even <- vapply(seq_len(nrow(grid)), function(i) {
    with(grid[i, ], alloc_is_even(alloc_optim(v, l, copies, seed = seed), copies, l))
  }, logical(1))
  expect(all(even), sprintf("(lines, l, copies) = (%s) is uneven",
                            paste(unlist(grid[which(!even)[1], 1:3]), collapse = ", ")))
  # sparse with as many copies as locations or more: do_optim() accepts it
  y <- do_optim("sparse", 40, 3, 4, add_checks = TRUE, checks = 1, force_balance = FALSE, seed = 1)
  expect_true(alloc_is_even(y, 4, 3))
})

test_that("a square-lattice shape gives every two lines exactly one shared location", {
  # q^2 lines in r q locations of q plots with r = q + 1 copies and q prime is
  # the affine plane of order q, a balanced incomplete block design (lambda = 1),
  # so the A-optimal allocation has every off-diagonal concurrence equal to 1.
  cases <- list(order_5 = list(q = 5, seed = 1))
  for (nm in names(cases)) {
    q <- cases[[nm]]$q
    x <- do_optim("sparse", lines = q^2, l = q * (q + 1), copies_per_entry = q + 1,
                  add_checks = TRUE, checks = 1, seed = cases[[nm]]$seed)
    N <- as.matrix(x$allocation)
    concurrence <- tcrossprod(N)
    off <- concurrence[upper.tri(concurrence)]
    expect(all(off == 1), sprintf("case %s: %d line pairs do not share exactly one location (range %s)",
                                  nm, sum(off != 1), paste(range(off), collapse = "-")))
  }
})

test_that("the smallest designs (two plots per location) are even", {
  even <- vapply(3:6, function(l) alloc_is_even(alloc_optim(l, l, 2, seed = l), 2, l), logical(1))
  expect(all(even), sprintf("l = %d is uneven", (3:6)[which(!even)[1]]))
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

# ---- force_balance ----

test_that("force_balance gives each smaller location one more copy", {
  balanced <- do_optim("sparse", 7, 3, 2, add_checks = TRUE, checks = 1, seed = 1)
  plain <- do_optim("sparse", 7, 3, 2, add_checks = TRUE, checks = 1, seed = 1, force_balance = FALSE)
  # 14 copies over 3 locations: totals 5, 5, 4 before balancing
  expect_equal(list(plain = sort(unname(colSums(plain$allocation))), balanced = unname(colSums(balanced$allocation))),
               list(plain = c(4, 5, 5), balanced = c(5, 5, 5)))
})

test_that("force_balance warns and keeps the allocation when no entry can be added", {
  # Regression: the balancing loop walked up the rows with no lower bound and
  # ran forever or wrote into the wrong rows. With 5 copies over 2 locations
  # every entry has 2 or 3 copies at each, so no p-rep entry with one copy can get a second.
  expect_warning(warned <- do_optim("prep", 5, 2, 5, seed = 1), class = "fieldhub_design_warning")
  plain <- do_optim("prep", 5, 2, 5, seed = 1, force_balance = FALSE)
  expect_identical(warned$allocation, plain$allocation)
})

# ---- Seeds and reproduction ----

test_that("allocation results record their inputs, and the same seed rebuilds them", {
  cases <- list(
    sparse = list(args = list(design = "sparse", lines = 12, l = 4, copies_per_entry = 3, add_checks = TRUE,
                              checks = 2, rep_checks = c(2, 2), seed = 38),
                  class = c("fieldhub_sparse_optimization", "Sparse")),
    prep = list(args = list(design = "prep", lines = 12, l = 4, copies_per_entry = 5, add_checks = TRUE,
                            checks = 2, rep_checks = c(2, 2), seed = 38),
                class = c("fieldhub_multi_prep_optimization", "MultiPrep")),
    sparse_large = list(args = list(design = "sparse", lines = 120, l = 4, copies_per_entry = 3,
                                    add_checks = TRUE, checks = 4, seed = 15),
                        class = c("fieldhub_sparse_optimization", "Sparse")),
    prep_large = list(args = list(design = "prep", lines = 80, l = 3, copies_per_entry = 5, seed = 9),
                      class = c("fieldhub_multi_prep_optimization", "MultiPrep"))
  )
  for (nm in names(cases)) {
    out <- do.call(do_optim, cases[[nm]]$args)
    ok <- c(class = identical(class(out), cases[[nm]]$class),
            design = identical(out$metadata$design, paste0("allocation_", cases[[nm]]$args$design)),
            seed = identical(out$metadata$seed, cases[[nm]]$args$seed),
            rng_kind = identical(out$metadata$rng_kind, RNGkind()),
            package_version = identical(out$metadata$package_version,
                                        as.character(utils::packageVersion("FielDHub"))),
            same_seed = identical(do.call(do_optim, cases[[nm]]$args), out),
            rebuilds = identical(do.call(do_optim, out$metadata$parameters), out))
    expect(all(ok), sprintf("case %s: %s fails", nm, paste(names(ok)[!ok], collapse = ", ")))
  }
})

test_that("automatic allocation seeds consume exactly one draw from the caller's stream", {
  set.seed(91)
  expected <- sample.int(.Machine$integer.max, 1L)
  after_one_draw <- .Random.seed
  set.seed(91)
  out <- do_optim(design = "sparse", lines = 12, l = 4, copies_per_entry = 3, add_checks = TRUE, checks = 2)
  expect_identical(.Random.seed, after_one_draw)
  expect_identical(out$metadata$seed, expected)
  expect_identical(do.call(do_optim, out$metadata$parameters), out)
})

test_that("the caller's random stream is restored after an allocation", {
  set.seed(77)
  expected <- runif(3)
  set.seed(77)
  do_optim("prep", 60, 3, 4, seed = 1)
  expect_identical(runif(3), expected)
})

test_that("the design defaults to sparse", {
  args <- list(lines = 20, l = 4, copies_per_entry = 3, add_checks = TRUE, checks = 2, seed = 1)
  expect_equal(do.call(do_optim, args), do.call(do_optim, c(list(design = "sparse"), args)))
})

# ---- Input errors ----

test_that("allocation engines reject malformed line, copy and check counts before any draw", {
  specs <- list(
    do_optim = list(design = "sparse", lines = 20, l = 4, copies_per_entry = 3, add_checks = TRUE, checks = 2),
    sparse_allocation = list(lines = 120, l = 4, copies_per_entry = 3, checks = 4),
    multi_location_prep = list(lines = 80, l = 4, copies_per_entry = 5, checks = 2,
                               rep_checks = c(4, 4), allow_fillers = TRUE))
  bad <- list(numeric(), NA_real_, NaN, Inf, 0, -1, 2.5, "2", TRUE, list(2), matrix(2), c(2, 2))
  for (engine in names(specs)) for (parameter in c("lines", "copies_per_entry", "checks")) {
    ok <- vapply(bad, function(value) {
      args <- c(specs[[engine]], list(seed = 19))
      args[parameter] <- list(value)
      signals_without_draw(as.call(c(as.name(engine), args)), "fieldhub_input_error")
    }, logical(1))
    expect(all(ok), sprintf("%s(%s = %s): no fieldhub_input_error, or the caller's stream moved", engine,
                            parameter, paste(deparse(bad[[which(!ok)[1]]]), collapse = " ")))
  }
})

test_that("allocation inputs outside their contract are input errors", {
  cases <- list(
    rep_checks = lapply(list(numeric(), NA_real_, Inf, c(4, NA), c(4, .5), c(4, -1), "4", TRUE, matrix(c(4, 4))),
                        function(bad) bquote(do_optim(design = "prep", lines = 20, l = 4, copies_per_entry = 5,
                                                      add_checks = TRUE, checks = 2, rep_checks = .(bad), seed = 1))),
    desired_avg = lapply(list(NULL, numeric(), NA_real_, Inf, 0, -1, "2", TRUE, matrix(2), c(2, 2)),
                         function(bad) bquote(multi_location_prep(lines = 80, l = 4, desired_avg = .(bad), seed = 1,
                                                                  allow_fillers = TRUE))),
    design = lapply(list(NA_character_, c("sparse", "prep"), matrix("sparse"), list("sparse")),
                    function(bad) bquote(do_optim(design = .(bad), lines = 20, l = 4, copies_per_entry = 3,
                                                  add_checks = TRUE, checks = 2, seed = 1))),
    missing_copies = list(quote(do_optim(design = "sparse", lines = 20, l = 4, add_checks = TRUE, checks = 2,
                                         seed = 1)),
                          quote(do_optim(design = "prep", lines = 20, l = 4, add_checks = TRUE, checks = 2,
                                         seed = 1))),
    infeasible_blocks = list(quote(do_optim(design = "sparse", lines = 2, l = 10, copies_per_entry = 1,
                                            add_checks = TRUE, checks = 2, seed = 1))),
    # A supplied plan skips do_optim()'s check; merging the data still needs one replication per check
    rep_checks_with_optim_list = list(quote(multi_location_prep(
      lines = 80, l = 4, copies_per_entry = 5, checks = 2, rep_checks = c(4, 4, 4), allow_fillers = TRUE,
      data = data.frame(ENTRY = 1:82, NAME = c("CH1", "CH2", paste0("L", 1:80))), seed = 1,
      optim_list = do_optim("prep", lines = 80, l = 4, copies_per_entry = 5, add_checks = TRUE, checks = 2,
                            rep_checks = c(4, 4), seed = 1))))
  )
  for (nm in names(cases)) {
    ok <- vapply(cases[[nm]], function(call) {
      inherits(tryCatch({ eval(call); NULL }, error = identity), "fieldhub_input_error")
    }, logical(1))
    expect(all(ok), sprintf("%s: %s is not a fieldhub_input_error", nm,
                            paste(deparse(cases[[nm]][[which(!ok)[1]]]), collapse = " ")))
  }
})

test_that("allocation sizes the search cannot take are explained in the user's terms", {
  expect_error(do_optim("prep", 1, 2, 3, seed = 1), "at least 2 lines", class = "fieldhub_input_error")
  expect_error(do_optim("sparse", 20, 1001, 2, add_checks = TRUE, checks = 1, seed = 1),
               "at most 1000 locations", class = "fieldhub_input_error")
  expect_error(do_optim("sparse", 2e5, 600, 2, add_checks = TRUE, checks = 1, seed = 1),
               "line-location pairs", class = "fieldhub_input_error")
})
