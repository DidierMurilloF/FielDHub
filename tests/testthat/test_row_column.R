library(testthat)
library(FielDHub)

# Helper: check that a row_column output is a valid resolvable row-column design.
expect_valid_row_column <- function(des, t, nrows, r) {
  ncols <- t / nrows
  expect_s3_class(des, "FielDHub")
  expect_setequal(
    names(des),
    c("infoDesign", "blocksModel", "resolvableBlocks", "concurrence", "fieldBook")
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
# replicates (crossed = TRUE). This value equals the A-Efficiency blocksdesign
# reports for the full ~ Reps + Rows + Cols model of the corresponding coding.
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
  des <- row_column(t = 24, nrows = 6, r = 2, seed = 21, iterations = 30)
  expect_valid_row_column(des, t = 24, nrows = 6, r = 2)
  # The default optimization method is onestage.
  expect_equal(des$infoDesign$optimization, "onestage")
})

test_that("method = 'twostage' produces a valid resolvable row-column design", {
  des <- row_column(t = 24, nrows = 6, r = 2, seed = 21, iterations = 100,
                    method = "twostage")
  expect_valid_row_column(des, t = 24, nrows = 6, r = 2)
  expect_equal(des$infoDesign$optimization, "twostage")
})

test_that("method = 'onestage' produces a valid resolvable row-column design", {
  # onestage defaults to latinize = FALSE (nested), like the two-stage method.
  des <- row_column(t = 24, nrows = 6, r = 2, seed = 21, iterations = 30,
                    method = "onestage")
  expect_valid_row_column(des, t = 24, nrows = 6, r = 2)
})

test_that("method = 'onestage' with latinize = TRUE is valid and latinized", {
  # Achieving zero clashes depends on the blocksdesign optimizer converging, so
  # keep this off CRAN where the optimizer version can differ.
  skip_on_cran()
  des <- row_column(t = 24, nrows = 6, r = 2, seed = 21, iterations = 30,
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
    des <- row_column(t = 24, nrows = 6, r = 2, seed = 21, iterations = 100,
                      method = "twostage", latinize = TRUE),
    'onestage'
  )
  # The design is still produced; latinize simply has no effect for twostage.
  expect_valid_row_column(des, t = 24, nrows = 6, r = 2)
})

test_that("latinize = TRUE warns when full latinization is infeasible (r > ncols)", {
  # t = 12, nrows = 4 gives ncols = 3; with r = 4 > ncols some treatments must
  # repeat a column across replicates, so a warning is expected.
  expect_warning(
    des <- row_column(t = 12, nrows = 4, r = 4, seed = 7, iterations = 20,
                      method = "onestage", latinize = TRUE),
    "full latinization"
  )
  expect_valid_row_column(des, t = 12, nrows = 4, r = 4)
})

test_that("an unknown method is rejected", {
  expect_error(
    row_column(t = 24, nrows = 6, r = 2, seed = 21, iterations = 30, method = "greedy")
  )
})

test_that("onestage improves the joint row-by-column A-Efficiency over twostage", {
  # Compares two heuristic optimizer outcomes, so keep off CRAN (version drift).
  skip_on_cran()
  des_two <- row_column(t = 24, nrows = 6, r = 2, seed = 21, iterations = 200,
                        method = "twostage")
  # latinize = FALSE: the nested joint optimization targets the same
  # within-replicate criterion that twostage does, so the comparison is fair.
  des_one <- row_column(t = 24, nrows = 6, r = 2, seed = 21, iterations = 100,
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
    des <- row_column(t = cfg$t, nrows = cfg$nrows, r = cfg$r, seed = cfg$seed,
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
  des <- row_column(t = 24, nrows = 6, r = 2, seed = 21, iterations = 30,
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
  # instead of failing. Depends on blocksdesign erroring for this geometry.
  skip_on_cran()
  # capture_warnings() so the incidental "NaNs produced" warnings that the
  # two-stage efficiency report emits for such a degenerate design (pre-existing
  # behaviour, unrelated to the fallback) do not clutter the test output.
  w <- capture_warnings(
    des <- row_column(t = 8, nrows = 4, r = 2, seed = 21, iterations = 20,
                      method = "onestage", latinize = FALSE)
  )
  expect_match(w, "twostage", all = FALSE)
  expect_valid_row_column(des, t = 8, nrows = 4, r = 2)
  # The produced design is the two-stage fallback.
  expect_equal(des$infoDesign$optimization, "twostage")
})

test_that("the default (onestage) falls back for a small app-style design", {
  # The Shiny app offers nrows = 2 by default for t = 8 and passes no method or
  # iterations, so the default must not dead-end on such small designs.
  skip_on_cran()
  expect_warning(
    des <- row_column(t = 8, nrows = 2, r = 2, seed = 21),
    'twostage'
  )
  expect_valid_row_column(des, t = 8, nrows = 2, r = 2)
  expect_equal(des$infoDesign$optimization, "twostage")
})

test_that("method = 'onestage' works with a treatment data frame", {
  treatments <- paste0("ND-", 1:30)
  treatment_list <- data.frame(ENTRY = 1:30, TREATMENT = treatments)
  des <- row_column(t = 30, nrows = 5, r = 2, seed = 15, iterations = 30,
                    method = "onestage", data = treatment_list)
  expect_valid_row_column(des, t = 30, nrows = 5, r = 2)
  expect_true("TREATMENT" %in% names(des$fieldBook))
  expect_setequal(unique(des$fieldBook$TREATMENT), treatments)
})
