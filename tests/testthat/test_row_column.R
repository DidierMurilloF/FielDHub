library(testthat)
library(FielDHub)

# Helper: check that a row_column output is a valid resolvable row-column design.
expect_valid_row_column <- function(des, t, nrows, r) {
  ncols <- t / nrows
  expect_s3_class(des, "FielDHub")
  expect_setequal(
    names(des),
    c("infoDesign", "blocksModel", "resolvableBlocks", "concurrence", "fieldBook",
      "metadata")
  )
  fb <- des$fieldBook
  expect_true(all(c("ID", "LOCATION", "PLOT", "REP", "ROW", "COLUMN", "ENTRY") %in% names(fb)))
  expect_equal(nrow(fb), t * r)
  expect_equal(sort(unique(fb$ROW)), 1:nrows)
  expect_equal(sort(unique(fb$COLUMN)), 1:ncols)
  # Each treatment appears exactly once per replicate (resolvable).
  reps_per_entry <- table(fb$ENTRY, fb$REP)
  expect_true(all(reps_per_entry == 1))
  # Exactly one plot per (REP, ROW, COLUMN) cell.
  cells <- table(fb$REP, fb$ROW, fb$COLUMN)
  expect_true(all(cells == 1))
  # The efficiency report includes the joint row-by-column efficiency.
  bm <- des$blocksModel[[1]]
  expect_true(all(c("Rep", "Row", "Column", "Row-by-Column") %in% bm$Level))
}

# Helper: joint row-by-column A-Efficiency reported for a design.
joint_aeff <- function(des) {
  bm <- des$blocksModel[[1]]
  bm$`A-Efficiency`[bm$Level == "Row-by-Column"]
}

# Independent oracle for the joint row-by-column A-Efficiency, computed from a
# field book. Uses an SVD-based projection onto the block column space, which is
# a different route than the qr.resid() used internally, so it catches a
# regression to a rank-blind block basis (the historical bug, which used qr.Q()
# on the rank-deficient block model and underestimated the joint efficiency).
# For nested (two-stage / latinize = FALSE) designs the rows and columns are
# taken within each replicate; for latinized designs they are shared across
# replicates (crossed = TRUE).
joint_aeff_oracle <- function(fb, crossed = FALSE, with_reps = FALSE) {
  if (crossed) {
    RepRow <- factor(fb$ROW)
    RepCol <- factor(fb$COLUMN)
  } else {
    RepRow <- factor(paste(fb$REP, fb$ROW, sep = "|"))
    RepCol <- factor(paste(fb$REP, fb$COLUMN, sep = "|"))
  }
  TF <- factor(fb$ENTRY)
  v <- nlevels(TF)
  r <- mean(table(TF))
  Tind <- model.matrix(~ TF - 1)
  # with_reps adds the replicate main effect explicitly; for a valid resolvable
  # design it must not change the joint efficiency, which is what lets the
  # internal function omit Reps from the ~ Rows + Cols block model.
  Bind <- if (with_reps) model.matrix(~ factor(fb$REP) + RepRow + RepCol)
          else model.matrix(~ RepRow + RepCol)
  sv <- svd(Bind)
  keep <- sv$d > sv$d[1] * 1e-9        # numerical rank of the block space
  U <- sv$u[, keep, drop = FALSE]
  resid <- Tind - U %*% (t(U) %*% Tind)  # (I - P_B) T via SVD projection
  C <- crossprod(resid)
  ev <- sort(eigen(C, symmetric = TRUE, only.values = TRUE)$values,
             decreasing = TRUE)
  eff <- ev[seq_len(v - 1)] / r
  (v - 1) / sum(1 / eff)
}

test_that("row_column() exposes method and latinize with the expected defaults", {
  fmls <- formals(row_column)
  # The default method is onestage (first element of the match.arg vector) and
  # latinize defaults to FALSE.
  expect_equal(eval(fmls$method), c("onestage", "twostage"))
  expect_false(eval(fmls$latinize))
  # iterations defaults to NULL; a method-appropriate value is chosen at runtime.
  expect_null(fmls$iterations)
})

test_that("default (onestage) produces a valid resolvable row-column design", {
  des <- row_column(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 30)
  expect_valid_row_column(des, t = 24, nrows = 6, r = 2)
  # The default optimization method is onestage.
  expect_equal(des$infoDesign$optimization, "onestage")
})

test_that("method = 'twostage' produces a valid resolvable row-column design", {
  des <- row_column(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 100,
                    method = "twostage")
  expect_valid_row_column(des, t = 24, nrows = 6, r = 2)
  expect_equal(des$infoDesign$optimization, "twostage")
})

test_that("method = 'onestage' produces a valid resolvable row-column design", {
  # onestage defaults to latinize = FALSE (nested), like the two-stage method.
  des <- row_column(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 30,
                    method = "onestage")
  expect_valid_row_column(des, t = 24, nrows = 6, r = 2)
})

test_that("method = 'onestage' with latinize = TRUE is valid and latinized", {
  # Engine results can differ across platforms and builds.
  skip_on_cran()
  des <- row_column(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 30,
                    method = "onestage", latinize = TRUE)
  expect_valid_row_column(des, t = 24, nrows = 6, r = 2)
  # Latinized: no treatment repeats the same row or the same column across reps.
  fb <- des$fieldBook
  row_clash <- sum(tapply(fb$ROW, fb$ENTRY, function(x) sum(duplicated(x))))
  col_clash <- sum(tapply(fb$COLUMN, fb$ENTRY, function(x) sum(duplicated(x))))
  expect_equal(row_clash, 0)
  expect_equal(col_clash, 0)
})

test_that("latinize = TRUE is ignored with a warning for method = 'twostage'", {
  # method must be set explicitly here: twostage is no longer the default, so
  # without it the call would run onestage + latinize and not warn.
  expect_warning(
    des <- row_column(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 100,
                      method = "twostage", latinize = TRUE),
    'onestage',
    class = "fieldhub_design_warning"
  )
  # The design is still produced; latinize simply has no effect for twostage.
  expect_valid_row_column(des, t = 24, nrows = 6, r = 2)
})

test_that("latinize = TRUE warns when full latinization is infeasible (r > ncols)", {
  # t = 12, nrows = 4 gives ncols = 3; with r = 4 > ncols some treatments must
  # repeat a column across replicates, so a warning is expected.
  expect_warning(
    des <- row_column(t = 12, nrows = 4, reps = 4, seed = 7, iterations = 20,
                      method = "onestage", latinize = TRUE),
    "full latinization",
    class = "fieldhub_design_warning"
  )
  expect_valid_row_column(des, t = 12, nrows = 4, r = 4)
})

test_that("an unknown method is rejected", {
  expect_error(
    row_column(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 30, method = "greedy")
  )
})

test_that("onestage improves the joint row-by-column A-Efficiency over twostage", {
  # Compares two heuristic optimizer outcomes, so keep off CRAN (version drift).
  skip_on_cran()
  des_two <- row_column(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 200,
                        method = "twostage")
  # latinize = FALSE: the nested joint optimization targets the same
  # within-replicate criterion that twostage does, so the comparison is fair.
  des_one <- row_column(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 100,
                        method = "onestage", latinize = FALSE)
  jt <- joint_aeff(des_two)
  jo <- joint_aeff(des_one)
  expect_true(is.finite(jt) && is.finite(jo))
  # onestage optimizes the joint criterion directly, so it should be at least as
  # good as the greedy two-stage design. Use a non-strict comparison: both are
  # heuristics and a tie is possible for a small design.
  expect_gte(jo, jt)
  # Guard the comparison from the other side: the two-stage joint value must
  # itself reproduce the independent (nested) oracle, so we are comparing two
  # correctly computed quantities rather than an artefact.
  expect_equal(jt, joint_aeff_oracle(des_two$fieldBook, crossed = FALSE),
               tolerance = 1e-4)
})

test_that("reported joint A-Efficiency matches an independent oracle (nested, r = 2 and r = 3)", {
  configs <- list(
    list(t = 24, nrows = 6, r = 2, seed = 21),  # r = 2
    list(t = 36, nrows = 6, r = 3, seed = 21)   # r = 3
  )
  for (cfg in configs) {
    des <- row_column(t = cfg$t, nrows = cfg$nrows, reps = cfg$r, seed = cfg$seed,
                      iterations = 30, method = "onestage", latinize = FALSE)
    reported <- joint_aeff(des)
    oracle <- joint_aeff_oracle(des$fieldBook, crossed = FALSE)
    # The reported value must reproduce the independent oracle (guards against a
    # regression to the rank-blind qr.Q() basis, which underestimated it).
    expect_equal(reported, oracle, tolerance = 1e-4)
    # Adding the replicate main effect must not change it.
    expect_equal(reported, joint_aeff_oracle(des$fieldBook, crossed = FALSE, with_reps = TRUE),
                 tolerance = 1e-4)
    expect_gt(reported, 0)
    expect_lte(reported, 1)
  }
})

test_that("reported joint A-Efficiency matches the crossed oracle for latinized designs", {
  # For a latinized design the joint efficiency is computed on the crossed
  # (shared) row and column model that design() actually optimized, so the
  # oracle must use the shared row/column factors too.
  des <- row_column(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 30,
                    method = "onestage", latinize = TRUE)
  reported <- joint_aeff(des)
  oracle <- joint_aeff_oracle(des$fieldBook, crossed = TRUE)
  expect_equal(reported, oracle, tolerance = 1e-4)
  # Adding the replicate main effect must not change it (validates that omitting
  # Reps from the crossed ~ Rows + Cols block model is legitimate).
  expect_equal(reported, joint_aeff_oracle(des$fieldBook, crossed = TRUE, with_reps = TRUE),
               tolerance = 1e-4)
  expect_gt(reported, 0)
  expect_lte(reported, 1)
})

test_that("onestage falls back to twostage (with a warning) when over-parameterized", {
  # t = 8, nrows = 4, ncols = 2, r = 2 has no residual df for the nested joint
  # model, so onestage is infeasible; row_column() must fall back to twostage
  # instead of failing.
  skip_on_cran()
  # capture_warnings() so the incidental "NaNs produced" warnings that the
  # two-stage efficiency report emits for such a degenerate design (pre-existing
  # behaviour, unrelated to the fallback) do not clutter the test output.
  fallback <- NULL
  w <- capture_warnings(withCallingHandlers(
    des <- row_column(t = 8, nrows = 4, reps = 2, seed = 21, iterations = 20,
                      method = "onestage", latinize = FALSE),
    warning = function(cnd) {
      if (grepl("returned a twostage design", conditionMessage(cnd))) fallback <<- cnd
    }
  ))
  expect_s3_class(fallback, "fieldhub_design_warning")
  expect_match(w, "twostage", all = FALSE)
  expect_match(w, "returned a twostage design", all = FALSE)
  expect_false(any(grepl("blocksdesign", w)))
  expect_valid_row_column(des, t = 8, nrows = 4, r = 2)
  # The produced design is the two-stage fallback.
  expect_equal(des$infoDesign$optimization, "twostage")
})

test_that("the default (onestage) falls back for a small app-style design", {
  # The Shiny app offers nrows = 2 by default for t = 8 and passes no method or
  # iterations, so the default must not dead-end on such small designs.
  skip_on_cran()
  fallback <- NULL
  w <- capture_warnings(withCallingHandlers(
    des <- row_column(t = 8, nrows = 2, reps = 2, seed = 21),
    warning = function(cnd) {
      if (grepl("returned a twostage design", conditionMessage(cnd))) fallback <<- cnd
    }
  ))
  expect_s3_class(fallback, "fieldhub_design_warning")
  expect_match(w, "returned a twostage design", all = FALSE)
  expect_false(any(grepl("blocksdesign", w)))
  expect_valid_row_column(des, t = 8, nrows = 2, r = 2)
  expect_equal(des$infoDesign$optimization, "twostage")
})

test_that("method = 'onestage' works with a treatment data frame", {
  treatments <- paste0("ND-", 1:30)
  treatment_list <- data.frame(ENTRY = 1:30, TREATMENT = treatments)
  des <- row_column(t = 30, nrows = 5, reps = 2, seed = 15, iterations = 30,
                    method = "onestage", data = treatment_list)
  expect_valid_row_column(des, t = 30, nrows = 5, r = 2)
  expect_true("TREATMENT" %in% names(des$fieldBook))
  expect_setequal(unique(des$fieldBook$TREATMENT), treatments)
})

test_that("row_column() resolvableBlocks holds each replicate's row-by-column layout", {
  # Regression test: the per-location subsets were taken with
  # levels(fieldBook$LOCATION), which is NULL because LOCATION is not a
  # factor, so every subset was empty and every matrix was filled with NA.
  des <- row_column(t = 12, nrows = 3, reps = 2, l = 2, plotNumber = c(101, 201),
                    locationNames = c("B", "A"), method = "twostage", seed = 1)
  expect_named(des$resolvableBlocks, c("Loc_B", "Loc_A"))
  fb <- des$fieldBook
  for (site in c("B", "A")) {
    blocks <- des$resolvableBlocks[[paste0("Loc_", site)]]
    expect_named(blocks, c("rep1", "rep2"))
    for (j in 1:2) {
      plots <- fb[fb$LOCATION == site & fb$REP == j, ]
      expected <- matrix(NA, nrow = 3, ncol = 4)
      expected[cbind(plots$ROW, plots$COLUMN)] <- plots$ENTRY
      expect_equal(blocks[[j]], expected)
    }
  }
})

test_that("two-stage row_column() returns complete replicate grids", {
  des <- row_column(t = 20, nrows = 4, reps = 2, method = "twostage", seed = 1)
  fb <- des$fieldBook
  expect_true(all(table(fb$ENTRY, fb$REP) == 1))
  for (rep in unique(fb$REP)) {
    cells <- table(factor(fb$ROW[fb$REP == rep], levels = 1:4),
                   factor(fb$COLUMN[fb$REP == rep], levels = 1:5))
    expect_true(all(cells == 1))
  }
})

# The twostage row step (nested-engine spec section 6.8) swaps plots only inside
# a column of a replicate, so it must keep the column design built before it.
twostage_column_sets <- function(fb) {
  key <- paste(fb$LOCATION, fb$REP, fb$COLUMN)
  lapply(split(fb$ENTRY, key), sort)
}

test_that("the twostage row step keeps each column's treatments", {
  # iterations = 1 is the smallest row-search effort; the column design is
  # built before the row step from the same seed, so it is the same in both.
  small <- row_column(t = 30, nrows = 5, reps = 3, method = "twostage", seed = 11,
                      iterations = 1)
  full <- row_column(t = 30, nrows = 5, reps = 3, method = "twostage", seed = 11)
  expect_identical(twostage_column_sets(full$fieldBook), twostage_column_sets(small$fieldBook))
  # The full search did move plots between rows.
  row_of <- function(fb) fb$ROW[order(fb$REP, fb$ENTRY)]
  expect_false(identical(row_of(full$fieldBook), row_of(small$fieldBook)))
})

test_that("every replicate of a twostage design holds each entry once", {
  des <- row_column(t = 36, nrows = 4, reps = 4, method = "twostage", seed = 5)
  fb <- des$fieldBook
  expect_true(all(table(factor(fb$ENTRY, levels = 1:36), fb$REP) == 1L))
  expect_true(all(table(fb$REP, fb$ROW, fb$COLUMN) == 1L))
})

test_that("the reported twostage row A-efficiency is that of the field book's rows", {
  des <- row_column(t = 30, nrows = 6, reps = 3, method = "twostage", seed = 3)
  fb <- des$fieldBook
  # Treatment x (REP, ROW) incidence; C = r I - N K^-1 N', A is the harmonic
  # mean of its v - 1 nonzero eigenvalues divided by r.
  N <- table(factor(fb$ENTRY), paste(fb$REP, fb$ROW))
  r <- 3
  C <- r * diag(nrow(N)) - N %*% diag(1 / colSums(N)) %*% t(N)
  ev <- sort(eigen(C, symmetric = TRUE, only.values = TRUE)$values, decreasing = TRUE)
  eff <- ev[seq_len(nrow(N) - 1L)] / r
  bm <- des$blocksModel[[1]]
  reported <- bm$`A-Efficiency`[bm$Level == "Row"]
  expect_lt(abs(reported - length(eff) / sum(1 / eff)), 1e-7)
})

test_that("the twostage row A-efficiency reaches the recorded baseline mean", {
  # Engine results can differ across platforms and builds.
  skip_on_cran()
  # 0.9424677 is the mean row A over seeds 1..200 of FielDHub 1.7.0's R row
  # step for this shape (tools/nested-goldens-rc-baseline.csv.gz).
  des <- row_column(t = 100, nrows = 5, reps = 3, method = "twostage", seed = 1)
  bm <- des$blocksModel[[1]]
  expect_gte(bm$`A-Efficiency`[bm$Level == "Row"], 0.9424677)
})

test_that("the smallest twostage effort still returns connected rows", {
  # With rows of two plots the column design's rows are often disconnected
  # (Row A-Efficiency 0). The row step connects them first, whatever the
  # search effort is.
  for (seed in c(3, 10, 11, 18, 19)) {
    des <- row_column(t = 20, nrows = 10, reps = 2, method = "twostage", seed = seed,
                      iterations = 1)
    bm <- des$blocksModel[[1]]
    expect_gt(bm$`A-Efficiency`[bm$Level == "Row"], 0)
  }
})

test_that("row and column A-Bounds follow the reported coding", {
  # Spec 5.2b and 6.2: latinized rows and columns are shared across the
  # replicates (general class); nested ones lie within replicates that hold
  # every entry once (resolvable class). The replicate factor's bound is 1.
  factor_rows <- function(des) {
    bm <- des$blocksModel[[1]]
    stats::setNames(bm[["A-Bound"]], bm$Level)
  }
  expected <- list(`TRUE` = c(Row = 0.8707723, Column = 0.9387755),
                   `FALSE` = c(Row = 0.6529968, Column = 0.7806789))
  expect_valid_bounds <- function(des) {
    bm <- des$blocksModel[[1]]
    bm <- bm[bm$Level %in% c("Rep", "Row", "Column"), ]
    expect_true(all(bm[["A-Efficiency"]] <= bm[["A-Bound"]] + 1.5e-7))
  }
  for (L in c(TRUE, FALSE)) {
    des <- row_column(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 20, latinize = L)
    bounds <- factor_rows(des)
    expect_equal(bounds[c("Row", "Column")], expected[[as.character(L)]], tolerance = 1e-7)
    expect_equal(bounds[["Rep"]], 1)
    expect_valid_bounds(des)
  }
  expect_valid_bounds(row_column(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 20,
                                 method = "twostage"))
})

row_a <- function(des, i = 1) {
  bm <- des$blocksModel[[i]]
  bm$`A-Efficiency`[bm$Level == "Row"]
}

test_that("onestage reaches the recorded Row optimum", {
  # Engine results can differ across platforms and builds.
  skip_on_cran()
  # Row A of FielDHub 1.7.0's onestage, constant over its seeds (phase 2 spec 6.1).
  shapes <- list(list(t = 48, nrows = 6, reps = 3, latinize = FALSE, row = 0.8581470),
                 list(t = 24, nrows = 6, reps = 2, latinize = FALSE, row = 0.6500942),
                 list(t = 24, nrows = 6, reps = 2, latinize = TRUE, row = 0.8697479))
  for (s in shapes) {
    for (seed in 1:3) {
      des <- row_column(t = s$t, nrows = s$nrows, reps = s$reps, latinize = s$latinize,
                        seed = seed, iterations = 20)
      expect_equal(des$infoDesign$optimization, "onestage")
      expect_gte(row_a(des), s$row - 1.5e-7)
    }
  }
})

test_that("onestage reaches the recorded joint efficiency on a larger nested shape", {
  # Engine results can differ across platforms and builds.
  skip_on_cran()
  # Mean joint D of FielDHub 1.7.0's onestage over 200 seeds at iterations = 20
  # (SD 0.000135). The one joint search of the call has to get there; ten
  # passes and five escapes ended 0.0002 to 0.0005 below it on these seeds.
  for (seed in 1:3) {
    des <- row_column(t = 100, nrows = 5, reps = 4, seed = seed, iterations = 20)
    expect_equal(des$infoDesign$optimization, "onestage")
    bm <- des$blocksModel[[1]]
    expect_gte(bm$`D-Efficiency`[bm$Level == "Row-by-Column"], 0.7457799)
  }
})

test_that("onestage runs where blocksdesign fell back on some seeds", {
  skip_on_cran()
  shapes <- list(list(t = 9, nrows = 3, reps = 2, latinize = FALSE),
                 list(t = 4, nrows = 2, reps = 3, latinize = FALSE),
                 list(t = 4, nrows = 2, reps = 4, latinize = FALSE),
                 list(t = 4, nrows = 2, reps = 4, latinize = TRUE))
  for (s in shapes) {
    for (seed in 1:10) {
      w <- capture_warnings(
        des <- row_column(t = s$t, nrows = s$nrows, reps = s$reps, latinize = s$latinize,
                          seed = seed)
      )
      expect_false(any(grepl("twostage", w)))
      if (s$latinize) expect_match(w, "full latinization", all = FALSE)
      expect_equal(des$infoDesign$optimization, "onestage")
      expect_false(is.na(joint_aeff(des)))
    }
  }
})

test_that("several locations each get their own onestage design", {
  layout <- function(fb) fb$ENTRY[order(fb$REP, fb$ROW, fb$COLUMN)]
  for (L in c(FALSE, TRUE)) {
    des <- row_column(t = 12, nrows = 3, reps = 2, l = 3, seed = 7, iterations = 20,
                      latinize = L)
    expect_identical(row_column(t = 12, nrows = 3, reps = 2, l = 3, seed = 7, iterations = 20,
                                latinize = L), des)
    fb <- des$fieldBook
    sites <- unique(fb$LOCATION)
    expect_length(sites, 3)
    layouts <- lapply(sites, function(site) layout(fb[fb$LOCATION == site, ]))
    expect_false(identical(layouts[[1]], layouts[[2]]) && identical(layouts[[2]], layouts[[3]]))
    for (i in seq_along(sites)) {
      one <- des
      one$fieldBook <- fb[fb$LOCATION == sites[i], ]
      one$blocksModel <- des$blocksModel[i]
      expect_valid_row_column(one, t = 12, nrows = 3, r = 2)
    }
  }
})

test_that("latinized onestage keeps uploaded labels and has no clashes", {
  skip_on_cran()
  labels <- data.frame(ENTRY = 1:24, TREATMENT = paste0("G", 1:24))
  des <- row_column(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 20,
                    latinize = TRUE, data = labels)
  fb <- des$fieldBook
  expect_true(all(table(factor(fb$TREATMENT, levels = labels$TREATMENT), fb$REP) == 1L))
  expect_equal(sum(tapply(fb$ROW, fb$TREATMENT, function(x) sum(duplicated(x)))), 0)
  expect_equal(sum(tapply(fb$COLUMN, fb$TREATMENT, function(x) sum(duplicated(x)))), 0)
})

test_that("iterations sets the joint effort only", {
  skip_on_cran()
  # The rows are searched in full whatever iterations is.
  des <- row_column(t = 24, nrows = 6, reps = 2, seed = 3, iterations = 1)
  expect_equal(des$infoDesign$optimization, "onestage")
  expect_gte(row_a(des), 0.6500942 - 1.5e-7)
  expect_no_warning(big <- row_column(t = 12, nrows = 3, reps = 2, seed = 5, iterations = 5000))
  expect_identical(row_column(t = 12, nrows = 3, reps = 2, seed = 5, iterations = 5000), big)
})
