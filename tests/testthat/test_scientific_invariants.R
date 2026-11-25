# Scientific invariants of every catalogue design (helper-catalogue.R): field
# books are reproducible from their seed, place each plot once, number plots
# from the requested start, and hold each treatment as often as the design
# requires. Checks use plain counts (helper-invariants.R), not the package's
# validators or its earlier output.

# Builds a catalogue entry again, as catalogue_design() does, without the cache.
rebuild <- function(name) {
  suppressWarnings(suppressMessages({
    output <- NULL
    utils::capture.output(output <- catalogue[[name]]$build())
    output
  }))
}
# Wall-clock timings are not design content.
without_timing <- function(x) {
  if (is.list(x)) x$timing <- NULL
  x
}
field_book_entries <- function() {
  names(catalogue)[vapply(names(catalogue), function(name) {
    !is.null(catalogue_design(name)$fieldBook)
  }, logical(1))]
}

# Every catalogue design ------------------------------------------------------

test_that("every catalogue entry builds a design and its seed rebuilds it", {
  for (name in names(catalogue)) {
    first <- catalogue_design(name)
    expect(!is.null(first) && isTRUE(all.equal(without_timing(rebuild(name)), without_timing(first))),
           sprintf("%s: %s", name, if (is.null(first)) "no design" else "a second build differs"))
  }
})

test_that("every field design has distinct final physical coordinates", {
  for (name in field_book_entries()) {
    book <- field_layout(catalogue_design(name))
    ok <- has_unique_units(book, c("LOCATION", "ROW", "COLUMN")) && all(book$ROW >= 1 & book$COLUMN >= 1)
    expect(ok, sprintf("%s: repeated or non-positive LOCATION/ROW/COLUMN", name))
  }
})

test_that("every field book numbers its plots from the requested start at each location", {
  # Filler plots are numbered 0; split-unit designs share a plot number
  # across the sub-plots of one whole plot.
  for (name in field_book_entries()) {
    design <- catalogue_design(name)
    book <- design$fieldBook
    start <- design$metadata$parameters$plotNumber
    sites <- unique(book$LOCATION)
    units <- intersect(c("PLOT", "SUB_PLOT", "SUB_SUB_PLOT"), names(book))
    problems <- unlist(lapply(seq_along(sites), function(i) {
      plots <- book[book$LOCATION == sites[i] & book$PLOT > 0, units, drop = FALSE]
      c(if (min(plots$PLOT) != rep_len(start, length(sites))[i])
          sprintf("location %s starts at %d", sites[i], min(plots$PLOT)),
        if (anyDuplicated(plots)) sprintf("location %s repeats a plot", sites[i]))
    }))
    expect(length(problems) == 0L, sprintf("%s (plotNumber %s): %s", name,
                                           paste(start, collapse = ", "), paste(problems, collapse = "; ")))
  }
})

# Classic designs ---------------------------------------------------------------

test_that("CRD and complete blocks preserve requested treatment multiplicities", {
  cases <- list(
    CRD_count = list(list(TREATMENT = paste0("T", 1:5), LOCATION = "FARGO"), 3),
    CRD_labels = list(list(TREATMENT = LETTERS[1:4], LOCATION = 1), 2),
    CRD_data = list(list(TREATMENT = paste0("T", 1:4), LOCATION = 1), c(2, 3, 2, 3)),
    RCBD_two_locations = list(list(TREATMENT = paste0("T", 1:6), REP = 1:3, LOCATION = c("A", "B")), 1),
    RCBD_checks = list(list(TREATMENT = c("CK1", "CK2", paste0("T", 1:8)), REP = 1:3, LOCATION = "loc1"),
                       c(2, 2, rep(1, 8))),
    # A bare check count generates "CH1".."CHk" checks and "T1".."Tt" tests
    RCBD_check_count = list(list(TREATMENT = c("CH1", "CH2", paste0("T", 1:6)), REP = 1:3,
                                 LOCATION = "loc1"), c(2, 2, rep(1, 6))),
    RCBD_cartesian = list(list(TREATMENT = paste0("T", 1:5), REP = 1:3, LOCATION = "loc1"), 1)
  )
  for (name in names(cases)) {
    book <- catalogue_design(name)$fieldBook
    expect_true(has_invariant_counts(book, cases[[name]][[1]], cases[[name]][[2]]), info = name)
  }
})

test_that("Latin squares and rectangles have each treatment once per row and at most once per column", {
  treatments <- function(book) sort(unique(book$TREATMENT))
  cases <- list(
    latin_square = function(book) list(
      list(TREATMENT = paste0("T", 1:4), SQUARE = 1:2, ROW = paste("Row", 1:4)),
      list(TREATMENT = paste0("T", 1:4), SQUARE = 1:2, COLUMN = paste("Column", 1:4))),
    latin_square_cartesian = function(book) list(
      list(TREATMENT = paste0("T", 1:5), SQUARE = 1, ROW = paste("Row", 1:5)),
      list(TREATMENT = paste0("T", 1:5), SQUARE = 1, COLUMN = paste("Column", 1:5))),
    # Three complete rows of five treatments: each column then holds three
    # distinct treatments.
    latin_rectangle = function(book) list(
      list(TREATMENT = c("No nitrogen", "A*B", "C", "D", "E"), ROW = 1:3, LOCATION = c("A", "B")))
  )
  for (name in names(cases)) {
    book <- catalogue_design(name)$fieldBook
    ok <- all(vapply(cases[[name]](book), function(levels) has_invariant_counts(book, levels),
                     logical(1)))
    square <- if ("SQUARE" %in% names(book)) book$SQUARE else 1
    columns <- table(book$TREATMENT, interaction(book$LOCATION, square, book$COLUMN, drop = TRUE))
    expect(ok && all(columns <= 1), sprintf("%s: a treatment repeats in a row or column", name))
  }
})

test_that("factorial and split-unit designs contain every requested combination", {
  cases <- list(
    full_factorial_rcbd = list(FACTOR_A = 0:1, FACTOR_B = 0:2, REP = 1:2, LOCATION = 1:2),
    full_factorial_crd_data = list(FACTOR_N = 0:1, FACTOR_P = 0:2, REP = 1:2, LOCATION = 1),
    split_plot_rcbd = list(WHOLE_PLOT = 1:3, SUB_PLOT = 1:2, REP = 1:2, LOCATION = 1:2),
    split_plot_crd = list(WHOLE_PLOT = c("W1", "W2"), SUB_PLOT = letters[1:3], REP = 1:2, LOCATION = 1),
    split_split_plot_rcbd = list(WHOLE_PLOT = 1:2, SUB_PLOT = 1:2, SUB_SUB_PLOT = 1:2, REP = 1:2, LOCATION = 1),
    split_split_plot_crd = list(WHOLE_PLOT = 1:2, SUB_PLOT = 1:3, SUB_SUB_PLOT = 1:2, REP = 1:2, LOCATION = 1),
    strip_plot = list(HSTRIP = paste0("b", 0:2), VSTRIP = paste0("a", 0:1), REP = 1:2, LOCATION = 1:2),
    strip_plot_labels = list(HSTRIP = c("H1", "H2"), VSTRIP = c("V1", "V2", "V3"), REP = 1:3, LOCATION = 1)
  )
  for (name in names(cases)) {
    book <- catalogue_design(name)$fieldBook
    # A whole plot carries one whole-plot treatment.
    whole <- if ("WHOLE_PLOT" %in% names(book)) {
      groups <- split(book$WHOLE_PLOT, interaction(book$LOCATION, book$PLOT, drop = TRUE))
      all(lengths(lapply(groups, unique)) == 1L)
    } else TRUE
    expect(has_invariant_counts(book, cases[[name]]) && whole,
           sprintf("%s: a combination is missing or repeated, or a whole plot is mixed", name))
  }
})

# Incomplete-block and row-column designs ---------------------------------------

test_that("incomplete blocks and lattices resolve into complete treatment replicates", {
  # Rows: entries, block size, replicates, locations.
  cases <- list(incomplete_blocks = c(12, 4, 2, 1), incomplete_blocks_labels = c(10, 5, 2, 2),
                alpha_lattice = c(12, 4, 2, 1), alpha_lattice_six_reps = c(12, 3, 6, 1),
                square_lattice = c(16, 4, 2, 1), rectangular_lattice = c(12, 3, 2, 1))
  expect_setequal(names(cases), setdiff(catalogue_family("incomplete"), c("row_column", "row_column_twostage")))
  for (name in names(cases)) {
    spec <- cases[[name]]
    book <- catalogue_design(name)$fieldBook
    replication <- list(ENTRY = seq_len(spec[1]), REP = seq_len(spec[3]), LOCATION = seq_len(spec[4]))
    blocks <- list(IBLOCK = seq_len(spec[1] / spec[2]), REP = seq_len(spec[3]), LOCATION = seq_len(spec[4]))
    ok <- has_invariant_counts(book, replication) && has_invariant_counts(book, blocks, spec[2]) &&
      has_unique_units(book, c("LOCATION", "REP", "IBLOCK", "UNIT"))
    expect(ok, sprintf("%s: not resolvable into replicates of blocks of %d", name, spec[2]))
  }
})

test_that("row-column designs resolve and fill each replicate grid", {
  for (name in c("row_column", "row_column_twostage")) {
    locations <- if (name == "row_column") 1L else 1:2
    book <- catalogue_design(name)$fieldBook
    ok <- has_invariant_counts(book, list(ENTRY = 1:12, REP = 1:2, LOCATION = locations)) &&
      has_invariant_counts(book, list(ROW = 1:3, COLUMN = 1:4, REP = 1:2, LOCATION = locations))
    expect(ok, sprintf("%s: an entry or a grid cell is not once per replicate", name))
  }
})

test_that("block-design efficiencies match incidence algebra and stay within their bounds", {
  # The reported A-efficiency (7 decimals) equals the eigenvalue reference of
  # the first location's field book (12 to 16 treatments, kappa < 10, so the
  # reference is good to 1e-13), and no reported efficiency exceeds its
  # reported A-Bound.
  tol <- 5e-8 + 1e-10
  for (name in catalogue_family("incomplete")) {
    design <- catalogue_design(name)
    models <- if (is.data.frame(design$blocksModel)) list(design$blocksModel) else design$blocksModel
    within <- all(vapply(models, function(bm) {
      bounded <- !is.na(bm[["A-Bound"]])
      all(bm[["A-Efficiency"]][bounded] <= bm[["A-Bound"]][bounded] + tol)
    }, logical(1)))
    match_oracle <- TRUE
    if (!grepl("^row_column", name)) {
      first <- design$fieldBook[design$fieldBook$LOCATION == design$fieldBook$LOCATION[1L], ]
      observed <- tail(design$blocksModel[["A-Efficiency"]], 1L)
      match_oracle <- abs(block_efficiency_oracle(first) - observed) <= tol
    }
    expect(within && match_oracle,
           sprintf("%s: %s", name, if (!within) "an efficiency exceeds its A-Bound" else
             "reported A-efficiency differs from the incidence algebra"))
  }
})

# Unreplicated and augmented designs --------------------------------------------

test_that("unreplicated and augmented designs preserve entries, checks, and fillers", {
  # Rows: entries, checks, check plots, fillers (per location).
  cases <- list(diagonal_single = c(270, 4, 30, 0), diagonal_single_cartesian = c(287, 4, 31, 2),
                diagonal_blocks_row = c(720, 5, 60, 0), optimized_arrangement = c(100, 5, 20, 0),
                RCBD_augmented = c(50, 3, 15, 0), RCBD_augmented_fixed = c(122, 4, 20, 3),
                RCBD_augmented_two_locations = c(40, 4, 16, 0))
  for (name in names(cases)) {
    spec <- cases[[name]]
    book <- catalogue_design(name)$fieldBook
    locations <- if (name == "RCBD_augmented_two_locations") c("A", "B") else 1
    # Fillers have ENTRY 0 (or missing values in the legacy augmented schema).
    active <- book[!is.na(book$ENTRY) & book$ENTRY > 0, ]
    entries <- active[active$CHECKS == 0, ]
    checks <- active[active$CHECKS > 0, ]
    ok <- has_invariant_counts(entries, list(ENTRY = spec[2] + seq_len(spec[1]), LOCATION = locations)) &&
      has_invariant_counts(checks, list(LOCATION = locations), spec[3]) &&
      nrow(book) - nrow(active) == spec[4] * length(locations)
    if ("BLOCK" %in% names(checks)) {
      # Augmented blocks hold each check once.
      ok <- ok && has_invariant_counts(checks, list(ENTRY = seq_len(spec[2]),
                                                    BLOCK = seq_len(spec[3] / spec[2]), LOCATION = locations))
    }
    expect(ok, sprintf("%s: entries, checks or fillers are not as requested", name))
  }
  book <- catalogue_design("diagonal_blocks_column_same")$fieldBook
  expect(has_invariant_counts(subset(book, CHECKS == 0), list(ENTRY = 5:44, EXPT = paste0("Block", 1:10))) &&
           has_invariant_counts(subset(book, CHECKS > 0), list(ENTRY = 1:4), 25),
         "diagonal_blocks_column_same: entries are not once per block or checks not 25 times")
})

test_that("augmented row-column designs hold each test once and each check once per column", {
  # 12 x 8 field with 4 checks along columns: 32 check plots, 64 tests.
  book <- catalogue_design("augmented_row_column")$fieldBook
  tests <- book[book$CHECKS == 0, ]
  checks <- book[book$CHECKS > 0, ]
  expect(has_invariant_counts(tests, list(ENTRY = sort(unique(tests$ENTRY)))) && nrow(tests) == 64L &&
           has_invariant_counts(checks, list(ENTRY = 1:4, COLUMN = 1:8)),
         "augmented_row_column: a test repeats or a check is not once per column")
})

# Allocations ---------------------------------------------------------------------

test_that("partial replication and multi-location allocation conserve the entry budget", {
  for (name in c("partially_replicated", "partially_replicated_fillers")) {
    book <- catalogue_design(name)$fieldBook
    expect(has_invariant_counts(subset(book, ENTRY > 0), list(ENTRY = 1:57), c(rep(1, 50), rep(2, 7))) &&
             sum(book$ENTRY == 0) == if (name == "partially_replicated") 0 else 8,
           sprintf("%s: entry copies or fillers are not as requested", name))
  }
  prep <- catalogue_design("multi_location_prep")$fieldBook
  expect(has_invariant_counts(subset(prep, ENTRY <= 80), list(ENTRY = 1:80), 5) &&
           has_invariant_counts(subset(prep, ENTRY > 80), list(ENTRY = 81:82, LOCATION = paste0("LOC", 1:4)), 4),
         "multi_location_prep: entry or check copies are not as requested")
  sparse <- catalogue_design("sparse_allocation")$fieldBook
  expect(has_invariant_counts(subset(sparse, CHECKS == 0), list(ENTRY = 1:120), 3) &&
           has_unique_units(subset(sparse, CHECKS == 0), c("LOCATION", "ENTRY")) &&
           has_invariant_counts(subset(sparse, CHECKS > 0), list(LOCATION = paste0("LOC", 1:4)), 10),
         "sparse_allocation: entry or check copies are not as requested")
})

test_that("do_optim() allocations give each entry its copies, evenly over locations", {
  # Rows: copies per entry, allowed copies per location, test-entry copies
  # per location. force_balance (the default) balances the locations: when
  # lines * copies is not a multiple of l (400 * 2 in 3 locations), fewer
  # than l entries get one extra copy, so every location holds 267.
  cases <- list(do_optim_sparse = list(3, 0:1, 90), do_optim_sparse_sampled = list(2, 0:1, 267),
                do_optim_sparse_lattice_start = list(2, 0:1, 3), do_optim_prep = list(5, 1:2, 100))
  expect_setequal(names(cases), grep("^do_optim", names(catalogue), value = TRUE))
  for (name in names(cases)) {
    spec <- cases[[name]]
    design <- catalogue_design(name)
    allocation <- design$allocation
    entries <- design$multi_location_data
    if ("REPS" %in% names(entries)) entries <- entries[rep(seq_len(nrow(entries)), entries$REPS), ]
    test_entries <- entries[entries$ENTRY <= nrow(allocation), ]
    extra <- rowSums(allocation) - spec[[1]]
    ok <- all(extra %in% 0:1) && sum(extra) < ncol(allocation) &&
      all(as.matrix(allocation) %in% spec[[2]]) && all(colSums(allocation) == spec[[3]]) &&
      has_invariant_counts(test_entries, list(ENTRY = seq_len(nrow(allocation)), LOCATION = names(allocation)),
                           as.numeric(as.matrix(allocation)))
    expect(ok, sprintf("%s: allocation or location lists do not match the requested copies", name))
  }
})

test_that("family splits retain every input row and balance within each family", {
  result <- catalogue_design("split_families")
  counts <- table(result$data_locations$FAMILY, factor(result$data_locations$LOCATION, levels = paste("Location", 1:3)))
  expect(has_same_units(result$data_locations, result$metadata$parameters$data, c("ENTRY", "NAME", "FAMILY")) &&
           all(apply(counts, 1, function(x) max(x) - min(x)) <= 1L) &&
           all(colSums(counts) == result$rowsEachlist$n),
         "split_families: rows lost or a family unbalanced across locations")
})

test_that("pair swapping conserves entries and reports independently computed distances", {
  result <- catalogue_design("swap_pairs")
  input <- result$metadata$parameters$X
  output <- result$optim_design
  expect_identical(table(input, dnn = NULL), table(output, dnn = NULL))
  expect_identical(dim(output), dim(input))
  repeated <- names(which(table(output) > 1))
  distances <- unlist(lapply(repeated, function(entry) {
    as.numeric(stats::dist(which(output == as.numeric(entry), arr.ind = TRUE)))
  }))
  # Distances between integer grid positions: exact up to sqrt rounding.
  expect_equal(sort(result$pairwise_distance$DIST), sort(distances), tolerance = 1e-12)
  expect_equal(result$min_distance, min(distances), tolerance = 1e-12)
})
