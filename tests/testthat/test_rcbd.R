# RCBD(): randomized complete blocks, with and without repeated checks, and
# the field layouts built from them.

# One row of `fb` per plot; the block of a plot is its LOCATION and REP.
rcbd_blocks <- function(fb) {
  fb <- fb[order(fb$LOCATION, fb$REP, fb$PLOT), ]
  split(fb, interaction(fb$LOCATION, fb$REP, drop = TRUE, lex.order = TRUE))
}

# Near-equal contiguous strata of a block of `n` plots for `r` copies of a
# check: the first n %% r strata hold one plot more.
stratum_of <- function(n, r) {
  sizes <- rep(n %/% r, r) + c(rep(1L, n %% r), rep(0L, r - n %% r))
  rep(seq_len(r), times = sizes)
}

# ---- Field book without checks ---------------------------------------------

test_that("RCBD() without checks reproduces the previous release's field book", {
  # Backward-compatibility pin from the released package: the checks feature
  # must not alter the random stream when `checks` is NULL.
  d <- RCBD(t = 4, reps = 3, seed = 89076)
  expected <- data.frame(
    ID        = 1:12,
    LOCATION  = factor(rep("loc1", 12), levels = "loc1"),
    PLOT      = c(101, 102, 103, 104, 201, 202, 203, 204, 301, 302, 303, 304),
    REP       = rep(1:3, each = 4),
    TREATMENT = c("T3", "T4", "T2", "T1",
                  "T4", "T3", "T1", "T2",
                  "T1", "T2", "T4", "T3"),
    stringsAsFactors = FALSE
  )
  fb <- d$fieldBook
  rownames(fb) <- NULL
  expect_equal(fb, expected)
})

test_that("RCBD() blocks hold every treatment once and layoutRandom shows each block", {
  cases <- list(
    single_location = list(
      args = list(t = 4, reps = 3, seed = 89076), serpentine = TRUE,
      treatments = paste0("T", 1:4),
      plots = list(loc1 = c(101:104, 201:204, 301:304)),
      info = list(blocks = 3, number.of.treatments = 4, treatments = paste0("T", 1:4),
                  locations = 1, plotNumber = c(101, 201, 301),
                  locationNames = "loc1", seed = 89076)),
    continuous_cartesian_two_locations = list(
      args = list(reps = 2, l = 2, plotNumber = c(101, 1001), continuous = TRUE,
                  planter = "cartesian", seed = 555, locationNames = c("A", "B"),
                  data = data.frame(TREATMENT = paste0("G-", 1:4))), serpentine = FALSE,
      treatments = paste0("G-", 1:4),
      plots = list(A = 101:108, B = 1001:1008),
      info = NULL)
  )
  for (nm in names(cases)) {
    case <- cases[[nm]]
    d <- do.call(RCBD, case$args)
    fb <- d$fieldBook
    blocks <- rcbd_blocks(fb)
    complete <- vapply(blocks, function(b) setequal(b$TREATMENT, case$treatments) &&
                         !anyDuplicated(b$TREATMENT), logical(1))
    expect(all(complete), sprintf("case %s: block %s is not complete", nm,
                                  names(blocks)[which(!complete)[1]]))
    plots <- lapply(split(fb$PLOT, fb$LOCATION), sort)
    expect_equal(plots, case$plots, ignore_attr = TRUE, info = nm)
    # layoutRandom: one row per block, the treatments in planting order; the
    # serpentine planter numbers every second block backwards.
    in_order <- vapply(blocks, function(b) {
      trt <- b$TREATMENT
      if (case$serpentine && b$REP[1] %% 2 == 0) trt <- rev(trt)
      paste(trt, collapse = " ")
    }, character(1))
    shown <- unlist(lapply(d$layoutRandom, function(m) m[, 2]), use.names = FALSE)
    expect_equal(shown, unname(in_order), info = nm)
    if (!is.null(case$info)) {
      expect_equal(d$infoDesign[names(case$info)], case$info, info = nm)
    }
  }
})

# ---- Repeated checks: field book and entries -------------------------------

test_that("RCBD() with checks produces the wide field book", {
  d <- RCBD(t = 6, reps = 3, checks = c("CK1", "CK2"), rep_checks = c(2, 2), seed = 77)
  expect_named(d$fieldBook,
               c("ID", "LOCATION", "PLOT", "REP", "ENTRY", "CHECKS", "TREATMENT"))
  expect_equal(nrow(d$fieldBook), 10 * 3)
  expect_equal(
    d$infoDesign[c("plots_per_block", "checks", "check_names", "rep_checks",
                   "spread_checks", "number.of.treatments")],
    list(plots_per_block = 10, checks = 2, check_names = c("CK1", "CK2"),
         rep_checks = c(2, 2), spread_checks = TRUE, number.of.treatments = 6))
})

test_that("RCBD() resolves check and test entries and repeats checks in every block", {
  # want: one row per entry (ENTRY id, CHECKS id, label, copies per block);
  # every block of every location must hold exactly `per_block` copies, and a
  # label keeps one ENTRY/CHECKS id everywhere.
  entry_table <- function(labels, n_checks, per_block) {
    data.frame(ENTRY = seq_along(labels),
               CHECKS = c(seq_len(n_checks), rep(0L, length(labels) - n_checks)),
               TREATMENT = labels, per_block = per_block, stringsAsFactors = FALSE)
  }
  cases <- list(
    labels_beside_generated_tests = list(
      args = list(t = 6, reps = 3, checks = c("CK1", "CK2"), rep_checks = c(2, 3), seed = 78),
      want = entry_table(c("CK1", "CK2", paste0("T", 1:6)), 2, c(2, 3, rep(1, 6))), blocks = 3),
    unspread_checks = list(
      args = list(t = 6, reps = 3, checks = c("CK1", "CK2"), rep_checks = c(2, 3),
                  spread_checks = FALSE, seed = 5),
      want = entry_table(c("CK1", "CK2", paste0("T", 1:6)), 2, c(2, 3, rep(1, 6))), blocks = 3),
    label_found_in_data = list(
      args = list(reps = 2, checks = "CK1", rep_checks = 3, seed = 3,
                  data = data.frame(TREATMENT = c("G-1", "CK1", "G-2", "G-3"))),
      want = entry_table(c("CK1", "G-1", "G-2", "G-3"), 1, c(3, 1, 1, 1)), blocks = 2),
    label_pool_from_character_t = list(
      args = list(t = c("CK1", "A", "B", "C"), reps = 2, checks = "CK1", rep_checks = 2, seed = 4),
      want = entry_table(c("CK1", "A", "B", "C"), 1, c(2, 1, 1, 1)), blocks = 2),
    count_over_character_t = list(
      args = list(t = c("CK1", "CK2", paste0("G", 1:6)), reps = 2, checks = 2,
                  rep_checks = 2, seed = 6),
      want = entry_table(c("CK1", "CK2", paste0("G", 1:6)), 2, c(2, 2, rep(1, 6))), blocks = 2),
    count_over_data = list(
      args = list(reps = 2, checks = 2, rep_checks = 2, seed = 80,
                  data = data.frame(TREATMENT = c("CK1", "CK2", paste0("G-", 1:8)))),
      want = entry_table(c("CK1", "CK2", paste0("G-", 1:8)), 2, c(2, 2, rep(1, 8))), blocks = 2),
    generated_check_labels = list(
      args = list(t = 6, reps = 3, checks = 2, rep_checks = c(2, 2), seed = 5),
      want = entry_table(c("CH1", "CH2", paste0("T", 1:6)), 2, c(2, 2, rep(1, 6))), blocks = 3),
    generated_labels_two_locations = list(
      args = list(t = 6, reps = 2, l = 2, checks = 2, rep_checks = 2,
                  plotNumber = c(101, 1001), locationNames = c("A", "B"), seed = 82),
      want = entry_table(c("CH1", "CH2", paste0("T", 1:6)), 2, c(2, 2, rep(1, 6))), blocks = 4),
    one_check_two_locations = list(
      args = list(t = 5, reps = 2, l = 2, checks = "CK1", rep_checks = 2,
                  plotNumber = c(1001, 2001), locationNames = c("FARGO", "MINOT"), seed = 79),
      want = entry_table(c("CK1", paste0("T", 1:5)), 1, c(2, rep(1, 5))), blocks = 4)
  )
  for (nm in names(cases)) {
    case <- cases[[nm]]
    fb <- do.call(RCBD, case$args)$fieldBook
    ids <- unique(fb[c("ENTRY", "CHECKS", "TREATMENT")])
    ids <- ids[order(ids$ENTRY), ]
    counts <- table(interaction(fb$LOCATION, fb$REP, drop = TRUE),
                    factor(fb$TREATMENT, levels = ids$TREATMENT))
    same <- apply(counts, 2, function(n) if (all(n == n[1])) n[[1]] else NA_integer_)
    got <- data.frame(ENTRY = ids$ENTRY, CHECKS = ids$CHECKS, TREATMENT = ids$TREATMENT,
                      per_block = as.numeric(same), stringsAsFactors = FALSE)
    rownames(got) <- NULL
    expect_equal(list(entries = got, blocks = nrow(counts)),
                 list(entries = case$want, blocks = case$blocks),
                 ignore_attr = TRUE, info = nm)
  }
})

test_that("spread_checks places the copies of each check in distinct strata of the block", {
  # Many blocks in one call: each block is randomized on its own, so `reps`
  # plays the role of repeated draws. `crowded` (copies c(6, 2) in a block of
  # 9) often exhausts a stratum on a single attempt, so it only stays spread
  # if the placement retries instead of falling back to a plain shuffle.
  # The cartesian planter keeps plot order equal to the order within a block.
  cases <- list(
    one_check = list(args = list(t = 10, checks = "CK1", rep_checks = 3, reps = 50, seed = 1)),
    two_checks = list(args = list(t = 6, checks = c("CK1", "CK2"), rep_checks = c(2, 2),
                                  reps = 30, seed = 11)),
    crowded = list(args = list(t = 1, checks = c("CK1", "CK2"), rep_checks = c(6, 2),
                               reps = 40, seed = 2), density_warning = TRUE)
  )
  for (nm in names(cases)) {
    case <- cases[[nm]]
    case$args$planter <- "cartesian"
    if (isTRUE(case$density_warning)) {
      expect_warning(d <- do.call(RCBD, case$args), class = "fieldhub_design_warning")
    } else {
      d <- do.call(RCBD, case$args)
    }
    reps_per_check <- stats::setNames(case$args$rep_checks, case$args$checks)
    spread <- vapply(rcbd_blocks(d$fieldBook), function(b) {
      all(vapply(names(reps_per_check), function(ck) {
        r <- reps_per_check[[ck]]
        hit <- stratum_of(nrow(b), r)[b$TREATMENT == ck]
        identical(sort(hit), seq_len(r))
      }, logical(1)))
    }, logical(1))
    expect(all(spread), sprintf("case %s: block %s has two copies of a check in one stratum",
                                nm, names(spread)[which(!spread)[1]]))
  }
})

test_that("RCBD() plot numbers step by the block size when checks are present", {
  d <- RCBD(t = 6, reps = 3, checks = "CK1", rep_checks = 2,
            plotNumber = 101, continuous = TRUE, seed = 81)
  expect_equal(sort(d$fieldBook$PLOT), 101:124)
})

test_that("RCBD() keeps data in its 9th positional slot", {
  pool <- data.frame(TREATMENT = paste0("G-", 1:4))
  # 9 positional arguments, the 9th being `data`: the older call shape.
  d <- RCBD(NULL, 3, 1, 101, FALSE, "serpentine", 89076, "FARGO", pool)
  expect_equal(list(sort(unique(d$fieldBook$TREATMENT)), nrow(d$fieldBook)),
               list(paste0("G-", 1:4), 12L))
})

# ---- Warnings ---------------------------------------------------------------

test_that("RCBD() warns about defaulted check replication and check-heavy blocks", {
  cases <- list(
    rep_checks_missing = list(
      args = list(t = 5, reps = 2, checks = "CK1", seed = 1),
      class = "fieldhub_default_warning", ck1_per_block = 1),
    check_heavy_spread = list(
      args = list(t = 2, reps = 2, checks = "CK1", rep_checks = 4, seed = 1),
      class = "fieldhub_design_warning", ck1_per_block = 4),
    # Regression: with spread_checks = FALSE no stratification happens, so the
    # warning must not describe a stratified-placement constraint.
    check_heavy_unspread = list(
      args = list(t = 2, reps = 2, checks = "CK1", rep_checks = 4, spread_checks = FALSE, seed = 1),
      class = "fieldhub_design_warning", ck1_per_block = 4, not_said = "stratified"),
    # No block of 78 plots holds copies 20, 19, 19, 19 one per stratum: the
    # block falls back to an unrestricted shuffle with its own warning.
    strata_impossible = list(
      args = list(t = 1, reps = 2, checks = paste0("CK", 1:4), rep_checks = c(20, 19, 19, 19), seed = 1),
      class = "fieldhub_design_warning", ck1_per_block = 20, warnings = 3L)
  )
  for (nm in names(cases)) {
    case <- cases[[nm]]
    warnings <- list()
    d <- withCallingHandlers(do.call(RCBD, case$args), warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    })
    expect_identical(vapply(warnings, inherits, logical(1), what = case$class),
                     rep(TRUE, if (is.null(case$warnings)) 1L else case$warnings), info = nm)
    w <- warnings[[1L]]
    fb <- d$fieldBook
    expect_equal(as.vector(table(fb$REP[fb$TREATMENT == "CK1"])),
                 rep(case$ck1_per_block, case$args$reps), info = nm)
    if (!is.null(case$not_said)) {
      expect_false(grepl(case$not_said, conditionMessage(w), fixed = TRUE), info = nm)
    }
  }
})

# ---- Input errors -----------------------------------------------------------

test_that("RCBD() rejects invalid treatments, checks and check replication", {
  pool <- data.frame(TREATMENT = c("CK1", "CK2", "G-1"))
  cases <- list(
    single_character_treatment = list(t = "Wheat", reps = 3),
    check_count_without_pool = list(reps = 2, checks = 2),
    no_test_entry_left = list(reps = 2, checks = 3, rep_checks = 2, data = pool),
    duplicated_check_labels = list(t = 5, reps = 2, checks = c("CK1", "CK1"), rep_checks = 2),
    rep_checks_zero = list(t = 5, reps = 2, checks = "CK1", rep_checks = 0),
    rep_checks_fractional = list(t = 5, reps = 2, checks = "CK1", rep_checks = 2.5),
    rep_checks_wrong_length = list(t = 5, reps = 2, checks = c("A", "B"), rep_checks = c(2, 2, 2)),
    rep_checks_infinite = list(t = 5, reps = 2, checks = "CK1", rep_checks = Inf),
    rep_checks_missing_value = list(t = 5, reps = 2, checks = "CK1", rep_checks = NA),
    checks_list = list(t = 5, reps = 2, checks = list(1), rep_checks = 2),
    checks_missing_value = list(t = 5, reps = 2, checks = NA, rep_checks = 2),
    check_count_zero = list(t = 5, reps = 2, checks = 0),
    check_count_negative = list(t = 5, reps = 2, checks = -2),
    check_count_fractional = list(t = 5, reps = 2, checks = 2.5),
    negative_t = list(t = -3, reps = 2, checks = "CK1", rep_checks = 2),
    check_collides_with_generated_label = list(t = 5, reps = 2, checks = "T2", rep_checks = 2),
    duplicated_pool_labels = list(reps = 2, checks = 1, rep_checks = 2,
                                  data = data.frame(TREATMENT = c("CK1", "G-1", "G-1"))),
    check_not_in_pool = list(reps = 2, checks = "XYZ", rep_checks = 2, data = pool)
  )
  for (nm in names(cases)) {
    expect_error(do.call(RCBD, c(cases[[nm]], seed = 1)), class = "fieldhub_input_error",
                 info = nm)
  }
  # The block-size limit fires before (and instead of) the density warning.
  expect_no_warning(expect_error(
    RCBD(t = 10, reps = 2, checks = c("A", "B"), rep_checks = c(1e9, 1e9), seed = 1),
    class = "fieldhub_input_error"))
  # The near-miss error tells the user which label they probably meant.
  expect_error(RCBD(reps = 2, checks = "T2", rep_checks = 2, seed = 1,
                    data = data.frame(TREATMENT = c("t2", "B", "C", "D"))),
               "Did you mean \"t2\"", class = "fieldhub_input_error")
})

# ---- Field layouts ----------------------------------------------------------

test_that("RCBD layouts place the blocks as rows, columns or one column", {
  d <- RCBD(t = 4, reps = 3, seed = 89076)
  cases <- list(
    vertical_blocks_as_rows = list(stacked = "vertical", layout = 1,
      ROW = rep(1:3, each = 4), COLUMN = rep(1:4, times = 3)),
    vertical_single_column = list(stacked = "vertical", layout = 3,
      ROW = 1:12, COLUMN = rep(1L, 12)),
    horizontal_blocks_as_columns = list(stacked = "horizontal", layout = 1,
      ROW = rep(1:4, times = 3), COLUMN = rep(1:3, each = 4))
  )
  for (nm in names(cases)) {
    case <- cases[[nm]]
    book <- field_layout(d, layout = case$layout, stacked = case$stacked)
    book <- book[order(book$PLOT), ]
    expect_equal(list(ROW = book$ROW, COLUMN = book$COLUMN),
                 list(ROW = case$ROW, COLUMN = case$COLUMN), ignore_attr = TRUE, info = nm)
  }
})

test_that("RCBD layouts size themselves by plots per block and place every plot once", {
  d <- RCBD(t = 6, reps = 3, checks = c("CK1", "CK2"), rep_checks = c(2, 2), seed = 91)
  book <- plot_layout(x = d, layout = 1, stacked = "vertical")$fieldBookXY
  # Block size is 10 (6 tests + 2 x 2 checks), three blocks.
  expect_equal(c(nrow(book), max(book$ROW) * max(book$COLUMN)), c(30, 30))
  expect_equal(anyDuplicated(paste(book$ROW, book$COLUMN)), 0)
  expect_setequal(book$PLOT, d$fieldBook$PLOT)
})

test_that("the RCBD layout field book keeps a name-based column order", {
  cases <- list(
    plain = list(args = list(t = 4, reps = 3, seed = 92),
      cols = c("ID", "LOCATION", "PLOT", "ROW", "COLUMN", "REP", "TREATMENT")),
    checks = list(args = list(t = 6, reps = 3, checks = "CK1", rep_checks = 2, seed = 93),
      cols = c("ID", "LOCATION", "PLOT", "ROW", "COLUMN", "REP", "ENTRY", "CHECKS", "TREATMENT"))
  )
  for (nm in names(cases)) {
    out <- plot_layout(x = do.call(RCBD, cases[[nm]]$args), layout = 1, stacked = "vertical")
    expect_equal(names(out$fieldBookXY), cases[[nm]]$cols, info = nm)
  }
})

test_that("the RCBD field map builds without a ggplot2 warning for both schemas", {
  # A ggplot spec is built lazily, so force the build: a bad `gpar` (an empty
  # `linewidth` aesthetic) only warns when the plot is drawn.
  cases <- list(plain = list(t = 4, reps = 3, seed = 990),
                checks = list(t = 6, reps = 3, checks = c("CK1", "CK2"),
                              rep_checks = c(2, 2), seed = 991))
  for (nm in names(cases)) {
    p <- plot_layout(x = do.call(RCBD, cases[[nm]]), layout = 1, stacked = "vertical")
    expect_no_warning({
      ggplot2::ggplot_build(p$out_layout)
      ggplot2::ggplot_build(p$out_layoutPlots)
    }, message = nm)
  }
})

test_that("export_layout() shows TREATMENT labels like the rendered checks map", {
  # The checks map always labels plots by TREATMENT, so the RCBD export
  # (type_pref = "TREATMENT") must show the same labels, while the default
  # export keeps preferring ENTRY when it exists.
  d <- RCBD(t = 6, reps = 3, checks = c("CK1", "CK2"), rep_checks = c(2, 2), seed = 993)
  fb <- plot_layout(x = d, layout = 1, stacked = "vertical")$fieldBookXY
  cells <- function(export) unlist(export$file[-(1:2), -1], use.names = FALSE)
  expect_true(all(cells(export_layout(fb, 1)) %in% as.character(fb$ENTRY)))
  rcbd_cells <- cells(export_layout(fb, 1, type_pref = "TREATMENT"))
  expect_true(all(rcbd_cells %in% fb$TREATMENT))
  expect_false(all(rcbd_cells %in% as.character(fb$ENTRY)))
})

test_that("simulated data works on an RCBD layout with checks", {
  d <- RCBD(t = 6, reps = 3, checks = c("CK1", "CK2"), rep_checks = c(2, 2), seed = 96)
  lay <- plot_layout(x = d, layout = 1, stacked = "vertical")$allSitesFieldbook
  sim <- norm_trunc(a = 1, b = 10, data = lay, seed = 1)
  expect_s3_class(sim, "data.frame")
  expect_equal(nrow(sim), nrow(lay))
})
