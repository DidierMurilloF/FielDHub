library(FielDHub)

# Characterisation of the plot-numbering the deleted `planter_transform()`
# used to do (R/utils_names_layout.R, before this file existed), captured
# from its five call sites in R/utils_layout.R (generate_vertical_layout(),
# generate_horizontal_layout(), iblock_layouts()'s with_rows(), and the
# vertical/horizontal branches of square_layouts()) before it was deleted.
# The expected vectors below are literal output of the old function, run at
# these exact argument shapes:
#
#   planter_transform(plots, planter, cols, reps = NULL, mode = NULL, units = NULL)
#
#   A: mode = NULL          (generate_vertical_layout(), square_layouts() vertical)
#   B: mode = "Horizontal"/"horizontal", not "Grid" (generate_horizontal_layout(),
#      square_layouts() horizontal, iblock_layouts()'s horizontal branch)
#   C: mode = "Grid"        (iblock_layouts()'s grid_panel branch)
#
# `plots_along_grid()`/`plots_along_grid_by_rep()` (R/utils_layout.R), both
# built on `planting_path()`, now replace it. This test proves they reproduce
# every shape exactly, odd and even row counts, single and multiple reps,
# serpentine and cartesian.

test_that("plots_along_grid() reproduces planter_transform()'s mode = NULL shapes", {
  # A1: cols = 5, nCuts = 3 (odd number of grid rows), serpentine
  expect_identical(
    plots_along_grid(1:15, ncols = 5, planter = "serpentine"),
    c(1L, 2L, 3L, 4L, 5L, 10L, 9L, 8L, 7L, 6L, 11L, 12L, 13L, 14L, 15L)
  )
  # A2: same shape, cartesian: plots pass through unchanged
  expect_identical(plots_along_grid(1:15, ncols = 5, planter = "cartesian"), 1:15)
  # A3: cols = 4, nCuts = 4 (even number of grid rows), serpentine
  expect_identical(
    plots_along_grid(101:116, ncols = 4, planter = "serpentine"),
    c(101L, 102L, 103L, 104L, 108L, 107L, 106L, 105L, 109L, 110L, 111L, 112L,
      116L, 115L, 114L, 113L)
  )
  # A4: same shape, cartesian
  expect_identical(plots_along_grid(101:116, ncols = 4, planter = "cartesian"), 101:116)
})

test_that("plots_along_grid_by_rep() reproduces planter_transform()'s mode = \"Horizontal\" shapes", {
  # B1: 2 reps, grid_cols = 3 (cols / reps = 6 / 2), 2 rows per rep (even), serpentine
  expect_identical(
    plots_along_grid_by_rep(1:12, grid_cols = 3, reps = 2, planter = "serpentine"),
    c(1L, 2L, 3L, 6L, 5L, 4L, 7L, 8L, 9L, 12L, 11L, 10L)
  )
  # B2: same shape, cartesian
  expect_identical(plots_along_grid_by_rep(1:12, grid_cols = 3, reps = 2, planter = "cartesian"), 1:12)
  # B3: 3 reps, grid_cols = 3 (cols / reps = 9 / 3), 3 rows per rep (odd), serpentine;
  #     the serpentine direction restarts at row 1 of every rep
  expect_identical(
    plots_along_grid_by_rep(1:27, grid_cols = 3, reps = 3, planter = "serpentine"),
    c(1L, 2L, 3L, 6L, 5L, 4L, 7L, 8L, 9L, 10L, 11L, 12L, 15L, 14L, 13L, 16L,
      17L, 18L, 19L, 20L, 21L, 24L, 23L, 22L, 25L, 26L, 27L)
  )
  # B4: same shape, cartesian
  expect_identical(plots_along_grid_by_rep(1:27, grid_cols = 3, reps = 3, planter = "cartesian"), 1:27)
  # B5: a single rep (3 rows, odd), serpentine
  expect_identical(
    plots_along_grid_by_rep(1:9, grid_cols = 3, reps = 1, planter = "serpentine"),
    c(1L, 2L, 3L, 6L, 5L, 4L, 7L, 8L, 9L)
  )
})

test_that("plots_along_grid_by_rep() reproduces planter_transform()'s mode = \"Grid\" shapes", {
  # C1: 2 reps, grid_cols = units = 4, 2 rows per rep (even), serpentine
  expect_identical(
    plots_along_grid_by_rep(1:16, grid_cols = 4, reps = 2, planter = "serpentine"),
    c(1L, 2L, 3L, 4L, 8L, 7L, 6L, 5L, 9L, 10L, 11L, 12L, 16L, 15L, 14L, 13L)
  )
  # C2: same shape, cartesian
  expect_identical(plots_along_grid_by_rep(1:16, grid_cols = 4, reps = 2, planter = "cartesian"), 1:16)
  # C3: 2 reps, grid_cols = units = 4, 3 rows per rep (odd), serpentine
  expect_identical(
    plots_along_grid_by_rep(1:24, grid_cols = 4, reps = 2, planter = "serpentine"),
    c(1L, 2L, 3L, 4L, 8L, 7L, 6L, 5L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L,
      20L, 19L, 18L, 17L, 21L, 22L, 23L, 24L)
  )
  # C4: same shape, cartesian
  expect_identical(plots_along_grid_by_rep(1:24, grid_cols = 4, reps = 2, planter = "cartesian"), 1:24)
  # C5: a single rep (2 rows, even), serpentine
  expect_identical(
    plots_along_grid_by_rep(1:8, grid_cols = 4, reps = 1, planter = "serpentine"),
    c(1L, 2L, 3L, 4L, 8L, 7L, 6L, 5L)
  )
  # C6: same shape, cartesian
  expect_identical(plots_along_grid_by_rep(1:8, grid_cols = 4, reps = 1, planter = "cartesian"), 1:8)
})

test_that("plots_along_grid() is planting_path() applied to a field-book-ordered vector", {
  for (ncols in 2:5) {
    for (nrows in 1:4) {
      plots <- seq_len(nrows * ncols)
      for (planter in c("serpentine", "cartesian")) {
        path <- planting_path(nrows, ncols, planter)
        M <- matrix(0L, nrow = nrows, ncol = ncols)
        M[path] <- plots
        expect_identical(plots_along_grid(plots, ncols, planter), as.vector(t(M)))
      }
    }
  }
})

# --- Static check: planting_path() is the only place that reverses ---------
# alternate rows for a serpentine planter. `planter_transform()` used to
# hand-roll this with `if (i %% 2 == 0) rev(...)`; this proves no core
# function (present or future) does the same thing outside `planting_path()`.

#' Whether `e` is a call to `%% 2` (with the literal 2 as its right-hand
#' side), anywhere inside expression `e` -- the odd/even test an alternate-
#' row check is built from.
#' @noRd
fielddhub_contains_mod2 <- function(e) {
  if (!is.call(e)) return(FALSE)
  if (is.symbol(e[[1]]) && identical(as.character(e[[1]]), "%%") && length(e) == 3 &&
      is.numeric(e[[3]]) && isTRUE(as.numeric(e[[3]]) == 2)) {
    return(TRUE)
  }
  for (i in seq_len(length(e))) {
    if (is.call(e[[i]]) && fielddhub_contains_mod2(e[[i]])) return(TRUE)
  }
  FALSE
}

#' Whether `e` is an `if` node whose condition tests `%% 2` (an odd/even,
#' i.e. alternate-row, test) and whose consequent or alternate branch
#' contains a call to `rev()` -- the "reverse every other row" pattern.
#' @noRd
fielddhub_is_alternate_row_reversal_if <- function(e) {
  if (!is.call(e) || !is.symbol(e[[1]]) || !identical(as.character(e[[1]]), "if")) {
    return(FALSE)
  }
  has_mod2 <- fielddhub_contains_mod2(e[[2]])
  has_rev <- ("rev" %in% all.names(e[[3]])) ||
    (length(e) >= 4 && "rev" %in% all.names(e[[4]]))
  isTRUE(has_mod2) && isTRUE(has_rev)
}

#' Every `if (... %% 2 ...) ... rev(...) ...` node inside `expr`, walking the
#' whole parse tree (branches of other `if`s, loop bodies, nested calls).
#' @noRd
fielddhub_alternate_row_reversal_ifs <- function(expr) {
  hits <- list()
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    if (fielddhub_is_alternate_row_reversal_if(e)) hits[[length(hits) + 1]] <<- e
    n <- length(e)
    for (i in seq_len(n)) {
      if (is.call(e[[i]])) walk(e[[i]])
    }
  }
  walk(expr)
  hits
}

test_that("the static check catches the deleted planter_transform()'s reversal", {
  # planter_transform() (formerly R/utils_names_layout.R) hand-rolled the
  # serpentine reversal twice, once per branch:
  old_planter_transform_body <- quote({
    PLOTS <- plots
    n_Reps <- reps
    if (!is.null(mode)) {
      if (mode == "Grid") {
        repCols <- units
      } else repCols <- cols / n_Reps
      nt <- length(PLOTS) / n_Reps
      RPLOTS <- split_vectors(x = PLOTS, len_cuts = rep(rep(nt, n_Reps), each = 1))
      rep_breaks <- vector(mode = "list", length = n_Reps)
      for (nreps in 1:n_Reps) {
        nCuts <- length(RPLOTS[[nreps]]) / repCols
        rep_breaks[[nreps]] <- split_vectors(x = RPLOTS[[nreps]], len_cuts = rep(repCols, each = nCuts))
      }
      lngt1 <- 1:length(rep_breaks)
      lngt2 <- 1:length(rep_breaks[[1]])
      new_breaks <- list()
      k <- 1
      for (n in lngt1) {
        for (m in lngt2) {
          if (m %% 2 == 0) {
            new_breaks[[k]] <- rev(unlist(rep_breaks[[n]][m]))
          } else new_breaks[[k]] <- rep_breaks[[n]][m]
          k <- k + 1
        }
      }
    } else {
      nCuts <- length(PLOTS) / cols
      breaks <- split_vectors(x = PLOTS, len_cuts = rep(cols, each = nCuts))
      lngt <- 1:length(breaks)
      new_breaks <- vector(mode = "list", length = nCuts)
      for (n in lngt) {
        if (n %% 2 == 0) {
          new_breaks[[n]] <- rev(breaks[[n]])
        } else new_breaks[[n]] <- breaks[[n]]
      }
    }
    PLOTS_serp <- as.vector(unlist(new_breaks))
    if (planter == "serpentine") {
      New_PLOTS <- as.vector(PLOTS_serp)
    } else New_PLOTS <- as.vector(PLOTS)
    return(PLOTS = New_PLOTS)
  })

  hits <- fielddhub_alternate_row_reversal_ifs(old_planter_transform_body)
  expect_length(hits, 2)
})

test_that("no core function other than planting_path() reverses alternate rows", {
  functions <- core_functions()
  offenders <- names(Filter(
    function(f) length(fielddhub_alternate_row_reversal_ifs(body(f))) > 0,
    functions
  ))
  expect_identical(offenders, character(0))
})
