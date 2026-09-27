test_that("every field design has distinct final physical coordinates", {
  entries <- names(catalogue)[vapply(names(catalogue), function(name) {
    !is.null(catalogue_design(name)$fieldBook)
  }, logical(1))]
  # Task 8 added RCBD_check_count to the catalogue (RCBD() generating its own
  # check labels from a bare check count).
  expect_length(entries, 37L)
  for (name in entries) {
    book <- field_layout(catalogue_design(name))
    expect_true(has_unique_units(book, c("LOCATION", "ROW", "COLUMN")), info = name)
    expect_true(all(book$ROW >= 1 & book$COLUMN >= 1), info = name)
  }
})

test_that("CRD and complete blocks preserve requested treatment multiplicities", {
  cases <- list(
    CRD_count = list(TREATMENT = paste0("T", 1:5), LOCATION = "FARGO"),
    CRD_labels = list(TREATMENT = LETTERS[1:4], LOCATION = 1),
    CRD_data = list(TREATMENT = paste0("T", 1:4), LOCATION = 1),
    RCBD_two_locations = list(TREATMENT = paste0("T", 1:6), REP = 1:3, LOCATION = c("A", "B")),
    RCBD_checks = list(TREATMENT = c("CK1", "CK2", paste0("T", 1:8)), REP = 1:3, LOCATION = "loc1"),
    RCBD_cartesian = list(TREATMENT = paste0("T", 1:5), REP = 1:3, LOCATION = "loc1")
  )
  counts <- list(3, 2, c(2, 3, 2, 3), 1, c(2, 2, rep(1, 8)), 1)
  for (i in seq_along(cases)) {
    name <- names(cases)[i]
    book <- catalogue_design(name)$fieldBook
    expect_true(has_invariant_counts(book, cases[[i]], counts[[i]]), info = name)
    expect_false(has_invariant_counts(book[-1, ], cases[[i]], counts[[i]]), info = name)
  }
})

test_that("Latin squares have one treatment per row and column in each square", {
  for (name in c("latin_square", "latin_square_cartesian")) {
    n <- if (name == "latin_square") 4L else 5L
    squares <- if (name == "latin_square") 1:2 else 1L
    book <- catalogue_design(name)$fieldBook
    for (axis in c("ROW", "COLUMN")) {
      values <- paste(if (axis == "ROW") "Row" else "Column", seq_len(n))
      levels <- c(list(TREATMENT = paste0("T", seq_len(n)), SQUARE = squares), setNames(list(values), axis))
      expect_true(has_invariant_counts(book, levels), info = paste(name, axis))
      bad <- book
      bad$TREATMENT[1] <- bad$TREATMENT[2]
      expect_false(has_invariant_counts(bad, levels), info = paste(name, axis))
    }
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
    expect_true(has_invariant_counts(book, cases[[name]]), info = name)
    expect_false(has_invariant_counts(book[-1, ], cases[[name]]), info = name)
    if ("WHOLE_PLOT" %in% names(book)) {
      groups <- split(book$WHOLE_PLOT, interaction(book$LOCATION, book$PLOT, drop = TRUE))
      expect_true(all(vapply(groups, function(x) length(unique(x)) == 1L, logical(1))), info = name)
    }
  }
})

test_that("incomplete blocks and lattices resolve into complete treatment replicates", {
  cases <- list(incomplete_blocks = c(12, 4, 2, 1), incomplete_blocks_labels = c(10, 5, 2, 2),
                alpha_lattice = c(12, 4, 2, 1), alpha_lattice_six_reps = c(12, 3, 6, 1),
                square_lattice = c(16, 4, 2, 1), rectangular_lattice = c(12, 3, 2, 1))
  for (name in names(cases)) {
    spec <- cases[[name]]
    book <- catalogue_design(name)$fieldBook
    replication <- list(ENTRY = seq_len(spec[1]), REP = seq_len(spec[3]), LOCATION = seq_len(spec[4]))
    blocks <- list(IBLOCK = seq_len(spec[1] / spec[2]), REP = seq_len(spec[3]), LOCATION = seq_len(spec[4]))
    expect_true(has_invariant_counts(book, replication), info = name)
    expect_true(has_invariant_counts(book, blocks, spec[2]), info = name)
    expect_true(has_unique_units(book, c("LOCATION", "REP", "IBLOCK", "UNIT")), info = name)
    expect_false(has_invariant_counts(book[-1, ], replication), info = name)
  }
})

test_that("row-column designs resolve and fill each replicate grid", {
  for (name in c("row_column", "row_column_twostage")) {
    locations <- if (name == "row_column") 1L else 1:2
    book <- catalogue_design(name)$fieldBook
    expect_true(has_invariant_counts(book, list(ENTRY = 1:12, REP = 1:2, LOCATION = locations)))
    expect_true(has_invariant_counts(book, list(ROW = 1:3, COLUMN = 1:4, REP = 1:2, LOCATION = locations)))
  }
})

test_that("incomplete-block and lattice efficiencies match independent incidence algebra", {
  cases <- c("incomplete_blocks", "incomplete_blocks_labels", "alpha_lattice",
             "alpha_lattice_six_reps", "square_lattice", "rectangular_lattice")
  for (name in cases) {
    design <- catalogue_design(name)
    # The legacy blocksModel table describes the first location.
    first_location <- unique(design$fieldBook$LOCATION)[1L]
    book <- design$fieldBook[design$fieldBook$LOCATION == first_location, ]
    observed <- tail(design$blocksModel$`A-Efficiency`, 1L)
    expect_equal(block_efficiency_oracle(book), observed, tolerance = 1e-6, info = name)
  }
})

test_that("unreplicated and augmented designs preserve entries, checks, and fillers", {
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
    expect_true(has_invariant_counts(entries, list(ENTRY = spec[2] + seq_len(spec[1]), LOCATION = locations)), info = name)
    expect_true(has_invariant_counts(checks, list(LOCATION = locations), spec[3]), info = name)
    expect_equal(nrow(book) - nrow(active), spec[4] * length(locations), info = name)
    if ("BLOCK" %in% names(checks)) {
      blocks <- spec[3] / spec[2]
      expect_true(has_invariant_counts(checks, list(ENTRY = seq_len(spec[2]), BLOCK = seq_len(blocks), LOCATION = locations)), info = name)
    }
  }
  book <- catalogue_design("diagonal_blocks_column_same")$fieldBook
  expect_true(has_invariant_counts(subset(book, CHECKS == 0), list(ENTRY = 5:44, EXPT = paste0("Block", 1:10))))
  expect_true(has_invariant_counts(subset(book, CHECKS > 0), list(ENTRY = 1:4), 25))
})

test_that("partial replication and multi-location allocation conserve the entry budget", {
  for (name in c("partially_replicated", "partially_replicated_fillers")) {
    book <- catalogue_design(name)$fieldBook
    expect_true(has_invariant_counts(subset(book, ENTRY > 0), list(ENTRY = 1:57), c(rep(1, 50), rep(2, 7))))
    expect_equal(sum(book$ENTRY == 0), if (name == "partially_replicated") 0 else 8)
  }
  prep <- catalogue_design("multi_location_prep")$fieldBook
  expect_true(has_invariant_counts(subset(prep, ENTRY <= 80), list(ENTRY = 1:80), 5))
  expect_true(has_invariant_counts(subset(prep, ENTRY > 80), list(ENTRY = 81:82, LOCATION = paste0("LOC", 1:4)), 4))
  sparse <- catalogue_design("sparse_allocation")$fieldBook
  expect_true(has_invariant_counts(subset(sparse, CHECKS == 0), list(ENTRY = 1:120), 3))
  expect_true(has_unique_units(subset(sparse, CHECKS == 0), c("LOCATION", "ENTRY")))
  expect_true(has_invariant_counts(subset(sparse, CHECKS > 0), list(LOCATION = paste0("LOC", 1:4)), 10))
  for (name in c("do_optim_sparse", "do_optim_prep")) {
    design <- catalogue_design(name)
    allocation <- design$allocation
    expect_true(all(rowSums(allocation) == if (name == "do_optim_sparse") 3 else 5))
    expect_true(all(as.matrix(allocation) %in% if (name == "do_optim_sparse") 0:1 else 1:2))
    expect_true(all(colSums(allocation) == if (name == "do_optim_sparse") 90 else 100))
    entries <- design$multi_location_data
    if ("REPS" %in% names(entries)) entries <- entries[rep(seq_len(nrow(entries)), entries$REPS), ]
    test_entries <- entries[entries$ENTRY <= nrow(allocation), ]
    expect_true(has_invariant_counts(test_entries,
      list(ENTRY = seq_len(nrow(allocation)), LOCATION = names(allocation)), as.numeric(as.matrix(allocation))))
  }
})

test_that("family splits retain every input row and balance within each family", {
  result <- catalogue_design("split_families")
  expect_true(has_same_units(result$data_locations, result$metadata$parameters$data, c("ENTRY", "NAME", "FAMILY")))
  counts <- table(result$data_locations$FAMILY, factor(result$data_locations$LOCATION, levels = paste("Location", 1:3)))
  expect_true(all(apply(counts, 1, function(x) max(x) - min(x)) <= 1L))
  expect_equal(as.numeric(colSums(counts)), result$rowsEachlist$n)
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
  expect_equal(sort(result$pairwise_distance$DIST), sort(distances))
  expect_equal(result$min_distance, min(distances))
})
