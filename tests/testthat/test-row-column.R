# Resolvable row-column designs: row_column(), its onestage and twostage
# methods, latinization, efficiency report and input contract.

# Reported efficiencies are rounded to 7 decimals (at most 5e-8 from the exact
# value); 1e-10 more covers the dense references below, whose error is about
# n * eps * kappa < 1e-11 (n <= 108 plots, kappa of C at most 8.2 and of the
# block model at most 19.4 on these designs).
round_tol <- 5e-8 + 1e-10

# Why a row_column() result is not a valid resolvable row-column design of t
# entries in nrows rows and reps replicates (character(0) when it is).
row_column_problems <- function(des, t, nrows, r) {
  fb <- des$fieldBook
  bm <- des$blocksModel[[1]]
  ncols <- t / nrows
  c(
    if (!inherits(des, "FielDHub")) "not a FielDHub result",
    if (!setequal(names(des), c("infoDesign", "blocksModel", "resolvableBlocks", "concurrence",
                                "fieldBook", "metadata"))) "unexpected components",
    if (!all(c("ID", "LOCATION", "PLOT", "REP", "ROW", "COLUMN", "ENTRY") %in% names(fb)))
      "field book columns missing",
    if (nrow(fb) != t * r) "wrong number of plots",
    if (!setequal(fb$ROW, seq_len(nrows)) || !setequal(fb$COLUMN, seq_len(ncols))) "wrong grid",
    if (!all(table(factor(fb$ENTRY, levels = seq_len(t)), fb$REP) == 1L))
      "an entry is not once per replicate",
    if (!all(table(fb$REP, fb$ROW, fb$COLUMN) == 1L)) "a (REP, ROW, COLUMN) cell is not filled once",
    if (!all(c("Rep", "Row", "Column", "Row-by-Column") %in% bm$Level)) "efficiency levels missing"
  )
}
expect_valid_row_column <- function(des, t, nrows, r, case = "") {
  problems <- row_column_problems(des, t, nrows, r)
  expect(length(problems) == 0L, paste0(case, ": ", paste(problems, collapse = "; ")))
}

efficiency <- function(des, level, column = "A-Efficiency", i = 1) {
  bm <- des$blocksModel[[i]]
  bm[[column]][match(level, bm$Level)]
}

# Independent oracle for the joint row-by-column A-efficiency of a field book:
# the treatment information after an SVD projection onto the block space (a
# different route than the qr.resid() used internally, so it catches a
# regression to a rank-blind block basis, which underestimated the joint
# efficiency). Nested designs take rows and columns within each replicate,
# latinized ones share them (crossed = TRUE). with_reps adds the replicate
# main effect, which must not change a resolvable design's efficiency.
joint_aeff_oracle <- function(fb, crossed = FALSE, with_reps = FALSE) {
  rows <- if (crossed) factor(fb$ROW) else factor(paste(fb$REP, fb$ROW, sep = "|"))
  cols <- if (crossed) factor(fb$COLUMN) else factor(paste(fb$REP, fb$COLUMN, sep = "|"))
  treatments <- model.matrix(~ factor(fb$ENTRY) - 1)
  v <- ncol(treatments)
  r <- nrow(fb) / v
  blocks <- if (with_reps) model.matrix(~ factor(fb$REP) + rows + cols) else model.matrix(~ rows + cols)
  sv <- svd(blocks)
  U <- sv$u[, sv$d > sv$d[1] * 1e-9, drop = FALSE]
  resid <- treatments - U %*% (t(U) %*% treatments)
  ev <- sort(eigen(crossprod(resid), symmetric = TRUE, only.values = TRUE)$values, decreasing = TRUE)
  (v - 1) / sum(r / ev[seq_len(v - 1)])
}

# Convexity bound on the A-efficiency of v treatments replicated r times in b
# blocks of k <= v plots whose incidence has rank at most `rank` (b for shared
# rows or columns, b - r + 1 for blocks nested in r complete replicates): at
# most m = min(v, rank) - 1 efficiency factors differ from 1 and they sum to
# m - T with T = b / r - 1, so their harmonic mean is at most
# (v - 1) / (v - 1 - m + m^2 / (m - T)).
convexity_a_bound <- function(v, r, b, rank) {
  m <- min(v, rank) - 1
  T <- b / r - 1
  (v - 1) / (v - 1 - m + m^2 / (m - T))
}

# Valid designs and methods ---------------------------------------------------

test_that("row_column() returns valid resolvable designs for each method", {
  labels <- data.frame(ENTRY = 1:30, TREATMENT = paste0("ND-", 1:30))
  cases <- list(
    default = list(args = list(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 30),
                   method = "onestage"),
    twostage = list(args = list(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 100,
                                method = "twostage"), method = "twostage"),
    twostage_four_reps = list(args = list(t = 36, nrows = 4, reps = 4, method = "twostage",
                                          seed = 5), method = "twostage"),
    onestage_data = list(args = list(t = 30, nrows = 5, reps = 2, seed = 15, iterations = 30,
                                     method = "onestage", data = labels), method = "onestage")
  )
  for (nm in names(cases)) {
    a <- cases[[nm]]$args
    des <- do.call(row_column, a)
    expect_valid_row_column(des, a$t, a$nrows, a$reps, nm)
    expect_equal(des$infoDesign$optimization, cases[[nm]]$method, info = nm)
  }
  expect_setequal(des$fieldBook$TREATMENT, labels$TREATMENT)
})

test_that("an unknown method is rejected", {
  expect_error(
    row_column(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 30, method = "greedy"),
    class = "fieldhub_input_error"
  )
})

test_that("latinized onestage designs repeat no entry in a row or column", {
  skip_on_cran()
  clashes <- function(fb, by) sum(tapply(fb[[by]], fb$TREATMENT, function(x) sum(duplicated(x))))
  labels <- data.frame(ENTRY = 1:24, TREATMENT = paste0("G", 1:24))
  cases <- list(
    numeric_t = row_column(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 30,
                           method = "onestage", latinize = TRUE),
    uploaded_labels = row_column(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 20,
                                 latinize = TRUE, data = labels)
  )
  for (nm in names(cases)) {
    des <- cases[[nm]]
    fb <- des$fieldBook
    expect_valid_row_column(des, 24, 6, 2, nm)
    expect_equal(c(row = clashes(fb, "ROW"), column = clashes(fb, "COLUMN")), c(row = 0, column = 0),
                 info = nm)
  }
  expect_setequal(cases$uploaded_labels$fieldBook$TREATMENT, labels$TREATMENT)
})

test_that("latinize warns where it cannot apply and the design is still built", {
  # twostage ignores latinize; with r > ncols full latinization is infeasible.
  cases <- list(
    twostage = list(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 100,
                    method = "twostage", latinize = TRUE),
    infeasible = list(t = 12, nrows = 4, reps = 4, seed = 7, iterations = 20,
                      method = "onestage", latinize = TRUE)
  )
  for (nm in names(cases)) {
    a <- cases[[nm]]
    expect_warning(des <- do.call(row_column, a), class = "fieldhub_design_warning")
    expect_valid_row_column(des, a$t, a$nrows, a$reps, nm)
  }
})

test_that("onestage falls back to twostage, with a warning, when over-parameterized", {
  skip_on_cran()
  # t = 8, nrows = 4 (and the app's default nrows = 2) leave no residual df for
  # the nested joint model; row_column() must return a twostage design.
  cases <- list(
    explicit = list(t = 8, nrows = 4, reps = 2, seed = 21, iterations = 20,
                    method = "onestage", latinize = FALSE),
    app_default = list(t = 8, nrows = 2, reps = 2, seed = 21)
  )
  for (nm in names(cases)) {
    a <- cases[[nm]]
    fallback <- NULL
    w <- capture_warnings(withCallingHandlers(
      des <- do.call(row_column, a),
      warning = function(cnd) {
        if (grepl("returned a twostage design", conditionMessage(cnd))) fallback <<- cnd
      }
    ))
    expect_s3_class(fallback, "fieldhub_design_warning")
    expect_false(any(grepl("blocksdesign", w)), info = nm)
    expect_valid_row_column(des, a$t, a$nrows, a$reps, nm)
    expect_equal(des$infoDesign$optimization, "twostage", info = nm)
    expect_null(des$infoDesign$joint_searches)
  }
})

test_that("onestage runs on small shapes without falling back", {
  skip_on_cran()
  shapes <- list(list(t = 9, nrows = 3, reps = 2, latinize = FALSE),
                 list(t = 4, nrows = 2, reps = 3, latinize = FALSE),
                 list(t = 4, nrows = 2, reps = 4, latinize = FALSE),
                 list(t = 4, nrows = 2, reps = 4, latinize = TRUE))
  for (s in shapes) {
    ok <- vapply(1:10, function(seed) {
      w <- capture_warnings(
        des <- row_column(t = s$t, nrows = s$nrows, reps = s$reps, latinize = s$latinize, seed = seed)
      )
      !any(grepl("twostage", w)) && (!s$latinize || any(grepl("full latinization", w))) &&
        des$infoDesign$optimization == "onestage" && !is.na(efficiency(des, "Row-by-Column"))
    }, logical(1))
    expect(all(ok), sprintf("t = %d, nrows = %d, reps = %d, latinize = %s: seed %d fell back",
                            s$t, s$nrows, s$reps, s$latinize, which(!ok)[1]))
  }
})

test_that("onestage builds a large latinized design without falling back", {
  skip_on_cran()
  # No design is refused for its size; the design stays latinized with shared
  # rows and columns: 2 rows and 500 columns (nested would be 4 and 1000).
  w <- capture_warnings(
    des <- row_column(t = 1000, nrows = 2, reps = 2, latinize = TRUE, seed = 1, iterations = 20)
  )
  expect_false(any(grepl("twostage|too large|not latinized", w)))
  expect_valid_row_column(des, 1000, 2, 2, "t = 1000")
  expect_equal(c(efficiency(des, c("Row", "Column"), "Blocks"), des$infoDesign$joint_searches$requested),
               c(2, 500, 1))
})

test_that("onestage warns once when its joint search cannot start", {
  skip_on_cran()
  conds <- list()
  des <- withCallingHandlers(
    row_column(t = 2500, nrows = 2, reps = 3, seed = 1, iterations = 20),
    warning = function(cnd) {
      conds[[length(conds) + 1L]] <<- cnd
      invokeRestart("muffleWarning")
    }
  )
  expect_length(conds, 1)
  expect_s3_class(conds[[1]], "fieldhub_design_warning")
  expect_equal(des$infoDesign$optimization, "onestage")
})

test_that("several locations each get their own reproducible onestage design", {
  layout <- function(fb) fb$ENTRY[order(fb$REP, fb$ROW, fb$COLUMN)]
  for (L in c(FALSE, TRUE)) {
    des <- row_column(t = 12, nrows = 3, reps = 2, l = 3, seed = 7, iterations = 20, latinize = L)
    expect_equal(row_column(t = 12, nrows = 3, reps = 2, l = 3, seed = 7, iterations = 20,
                            latinize = L), des)
    fb <- des$fieldBook
    sites <- unique(fb$LOCATION)
    layouts <- lapply(sites, function(site) layout(fb[fb$LOCATION == site, ]))
    expect_false(length(unique(layouts)) == 1L)
    problems <- unlist(lapply(seq_along(sites), function(i) {
      one <- des
      one$fieldBook <- fb[fb$LOCATION == sites[i], ]
      one$blocksModel <- des$blocksModel[i]
      row_column_problems(one, 12, 3, 2)
    }))
    expect(length(sites) == 3L && length(problems) == 0L,
           sprintf("latinize = %s: %d locations; %s", L, length(sites), paste(problems, collapse = "; ")))
  }
})

test_that("resolvableBlocks holds each replicate's row-by-column layout per location", {
  # The per-location subsets were once taken with levels() of a character
  # column, so every matrix was filled with NA.
  des <- row_column(t = 12, nrows = 3, reps = 2, l = 2, plotNumber = c(101, 201),
                    locationNames = c("B", "A"), method = "twostage", seed = 1)
  fb <- des$fieldBook
  expected <- lapply(c(Loc_B = "B", Loc_A = "A"), function(site) {
    lapply(c(rep1 = 1, rep2 = 2), function(j) {
      plots <- fb[fb$LOCATION == site & fb$REP == j, ]
      m <- matrix(NA, nrow = 3, ncol = 4)
      m[cbind(plots$ROW, plots$COLUMN)] <- plots$ENTRY
      m
    })
  })
  expect_equal(des$resolvableBlocks, expected)
})

test_that("a minimal twostage design whose rows cannot improve still reports efficiencies", {
  # Two rows of two plots per replicate: the row search has nothing to gain,
  # and the result must still carry the row efficiencies (it once errored).
  des <- row_column(t = 4, nrows = 2, reps = 2, method = "twostage", seed = 1)
  a <- efficiency(des, c("Rep", "Row"))
  expect(length(a) == 2L && all(is.finite(a) & a > 0 & a <= 1),
         sprintf("Rep and Row A-efficiencies: %s", paste(a, collapse = ", ")))
})

# Iterations ----------------------------------------------------------------

test_that("row_column() rejects invalid iteration budgets before randomization", {
  values <- list(Inf, -Inf, NaN, NA_real_, 1.5, 1 + 1i, numeric(), c(1, 2), matrix(1), "2", -1,
                 .Machine$integer.max + 1, 0)
  set.seed(19)
  before <- .Random.seed
  for (value in values) {
    expect_error(row_column(t = 12, nrows = 3, reps = 2, seed = 27, method = "twostage",
                            iterations = value),
                 class = "fieldhub_input_error")
  }
  expect_identical(.Random.seed, before)
})

test_that("iterations sets the joint effort only", {
  skip_on_cran()
  # The rows are searched in full whatever iterations is: the recorded onestage
  # row optimum of this shape (FielDHub 1.7.0) is reached at iterations = 1.
  des <- row_column(t = 24, nrows = 6, reps = 2, seed = 3, iterations = 1)
  expect_gte(efficiency(des, "Row"), 0.6500942 - round_tol)
  expect_no_warning(big <- row_column(t = 12, nrows = 3, reps = 2, seed = 5, iterations = 5000))
  expect_equal(row_column(t = 12, nrows = 3, reps = 2, seed = 5, iterations = 5000), big)
})

test_that("infoDesign reports the joint searches of onestage", {
  skip_on_cran()
  # The default 200 iterations ask for max(1, round(10 * 200 / 200)) = 10.
  des <- row_column(t = 24, nrows = 6, reps = 2, seed = 1, iterations = 200)
  expect_equal(unlist(des$infoDesign$joint_searches[c("requested", "completed")]),
               c(requested = 10, completed = 10))
  two <- row_column(t = 24, nrows = 6, reps = 2, l = 2, seed = 1, iterations = 200)
  expect_length(two$infoDesign$joint_searches$completed, 2)
  twostage <- row_column(t = 24, nrows = 6, reps = 2, seed = 1, method = "twostage")
  expect_null(twostage$infoDesign$joint_searches)
})

# Twostage row step -----------------------------------------------------------

test_that("the twostage row step keeps each column's treatments", {
  # The row step swaps plots only inside a column of a replicate. The column
  # design is built from the seed before it, so it is the same at the smallest
  # and at the full row effort.
  column_sets <- function(fb) lapply(split(fb$ENTRY, paste(fb$LOCATION, fb$REP, fb$COLUMN)), sort)
  row_of <- function(fb) fb$ROW[order(fb$REP, fb$ENTRY)]
  small <- row_column(t = 30, nrows = 5, reps = 3, method = "twostage", seed = 11, iterations = 1)
  full <- row_column(t = 30, nrows = 5, reps = 3, method = "twostage", seed = 11)
  expect_identical(column_sets(full$fieldBook), column_sets(small$fieldBook))
  # The full search did move plots between rows.
  expect_false(identical(row_of(full$fieldBook), row_of(small$fieldBook)))
})

test_that("the smallest twostage effort still returns connected rows", {
  # With rows of two plots the column design's rows are often disconnected
  # (Row A-Efficiency 0); the row step connects them first.
  a <- vapply(c(3, 10, 11, 18, 19), function(seed) {
    efficiency(row_column(t = 20, nrows = 10, reps = 2, method = "twostage", seed = seed,
                          iterations = 1), "Row")
  }, numeric(1))
  expect(all(a > 0), sprintf("seed %d has Row A-efficiency 0", c(3, 10, 11, 18, 19)[which.min(a)]))
})

# Efficiency report -------------------------------------------------------------

test_that("the reported twostage row A-efficiency is that of the field book's rows", {
  des <- row_column(t = 30, nrows = 6, reps = 3, method = "twostage", seed = 3)
  fb <- des$fieldBook
  # Treatment x (REP, ROW) incidence; C = r I - N K^-1 N' (30 x 30, kappa < 10).
  N <- table(factor(fb$ENTRY), paste(fb$REP, fb$ROW))
  C <- 3 * diag(nrow(N)) - N %*% diag(1 / colSums(N)) %*% t(N)
  ev <- sort(eigen(C, symmetric = TRUE, only.values = TRUE)$values, decreasing = TRUE)
  eff <- ev[seq_len(nrow(N) - 1L)] / 3
  expect_lte(abs(efficiency(des, "Row") - length(eff) / sum(1 / eff)), round_tol)
})

test_that("the reported joint A-efficiency matches an independent oracle", {
  # Rows: arguments, crossed rows and columns (latinized). The reported value
  # must equal the oracle with and without the replicate main effect.
  cases <- list(
    twostage = list(list(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 200,
                         method = "twostage"), FALSE),
    nested_r2 = list(list(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 30,
                          method = "onestage", latinize = FALSE), FALSE),
    nested_r3 = list(list(t = 36, nrows = 6, reps = 3, seed = 21, iterations = 30,
                          method = "onestage", latinize = FALSE), FALSE),
    latinized = list(list(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 30,
                          method = "onestage", latinize = TRUE), TRUE)
  )
  for (nm in names(cases)) {
    des <- do.call(row_column, cases[[nm]][[1]])
    crossed <- cases[[nm]][[2]]
    reported <- efficiency(des, "Row-by-Column")
    oracle <- c(joint_aeff_oracle(des$fieldBook, crossed),
                joint_aeff_oracle(des$fieldBook, crossed, with_reps = TRUE))
    expect(length(reported) == 1L && max(abs(reported - oracle)) <= round_tol,
           sprintf("%s: reported %.7f, oracle %.10f", nm, reported, oracle[1]))
  }
})

test_that("onestage improves the joint A-efficiency over twostage", {
  # Compares two heuristic optimizer outcomes, so keep off CRAN.
  skip_on_cran()
  two <- row_column(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 200, method = "twostage")
  one <- row_column(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 100,
                    method = "onestage", latinize = FALSE)
  expect_gte(efficiency(one, "Row-by-Column"), efficiency(two, "Row-by-Column"))
})

test_that("row and column A-Bounds are valid for the reported coding", {
  # Latinized rows and columns are shared across the replicates (rank of the
  # incidence at most b); nested ones lie within replicates that hold every
  # entry once (rank at most b - r + 1). For t = 24 in 6 rows and 4 columns and
  # 2 replicates the bound is at most the convexity bound of its coding and at
  # least the reported efficiency; the replicate factor's bound is 1.
  coding <- list(
    latinized = list(Row = c(b = 6, k = 8, rank = 6), Column = c(b = 4, k = 12, rank = 4)),
    nested = list(Row = c(b = 12, k = 4, rank = 11), Column = c(b = 8, k = 6, rank = 7))
  )
  cases <- list(
    latinized = row_column(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 20, latinize = TRUE),
    nested = row_column(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 20, latinize = FALSE),
    twostage = row_column(t = 24, nrows = 6, reps = 2, seed = 21, iterations = 20, method = "twostage")
  )
  for (nm in names(cases)) {
    bm <- cases[[nm]]$blocksModel[[1]]
    code <- coding[[if (nm == "latinized") "latinized" else "nested"]]
    limit <- c(Rep = 1, vapply(code, function(x) convexity_a_bound(24, 2, x[["b"]], x[["rank"]]), 1))
    bound <- setNames(bm[["A-Bound"]], bm$Level)[names(limit)]
    a <- setNames(bm[["A-Efficiency"]], bm$Level)[names(limit)]
    ok <- a <= bound + round_tol & bound <= limit + round_tol
    expect(all(ok) && abs(bound[["Rep"]] - 1) <= round_tol,
           sprintf("%s: %s A %.7f, A-Bound %.7f, convexity bound %.7f", nm, names(limit)[which.min(ok)],
                   a[which.min(ok)], bound[which.min(ok)], limit[which.min(ok)]))
  }
})

# Quality floors from FielDHub 1.7.0 ------------------------------------------

test_that("row_column() reaches the efficiencies recorded from FielDHub 1.7.0", {
  # Engine results can differ across platforms and builds.
  skip_on_cran()
  # Rows: arguments, seeds, level, efficiency column, recorded value.
  # - twostage row A: mean over seeds 1-200 of 1.7.0's R row step;
  # - onestage row A: 1.7.0's onestage optimum, constant over its seeds;
  # - onestage joint D at t = 100: 1.7.0's mean over 200 seeds (SD 0.000135;
  #   the worst of seeds 1:100 here was 0.00025 above it).
  cases <- list(
    list(list(t = 100, nrows = 5, reps = 3, method = "twostage"), 1, "Row", "A-Efficiency", 0.9424677),
    list(list(t = 48, nrows = 6, reps = 3, latinize = FALSE, iterations = 20), 1:3, "Row",
         "A-Efficiency", 0.8581470),
    list(list(t = 24, nrows = 6, reps = 2, latinize = FALSE, iterations = 20), 1:3, "Row",
         "A-Efficiency", 0.6500942),
    list(list(t = 24, nrows = 6, reps = 2, latinize = TRUE, iterations = 20), 1:3, "Row",
         "A-Efficiency", 0.8697479),
    list(list(t = 100, nrows = 5, reps = 4, iterations = 20), 1:3, "Row-by-Column",
         "D-Efficiency", 0.7457799)
  )
  for (case in cases) {
    got <- vapply(case[[2]], function(seed) {
      des <- do.call(row_column, c(case[[1]], seed = seed))
      expected_method <- if (is.null(case[[1]]$method)) "onestage" else case[[1]]$method
      if (des$infoDesign$optimization != expected_method) return(-Inf)
      efficiency(des, case[[3]], case[[4]])
    }, numeric(1))
    expect(all(got >= case[[5]] - round_tol),
           sprintf("t = %d, nrows = %d: seed %d %s %s = %.7f, recorded %.7f", case[[1]]$t,
                   case[[1]]$nrows, case[[2]][which.min(got)], case[[3]], case[[4]], min(got), case[[5]]))
  }
})

# Input contract ----------------------------------------------------------------

test_that("row-column labels and supplied entries fail with input conditions", {
  # Validated before the optimizer can disguise malformed labels as an
  # internal error.
  for (bad in list(c("A", NA, "C", "D"), c("A", " ", "C", "D"), list(1, 2, 3, 4))) {
    expect_error(row_column(t = bad, nrows = 2, reps = 2, seed = 41, iterations = 1),
                 class = "fieldhub_input_error")
  }
  expect_error(row_column(t = 12, nrows = 3, reps = 2, seed = 41, iterations = 1,
                          data = data.frame(ENTRY = 1:12)), class = "fieldhub_input_error")
})
