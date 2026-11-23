test_that("dimensioned constructors preserve assignments and tile experimental units", {
  cases <- list(
    CRD_data = list(grid = c(2, 5)),
    full_factorial_crd_data = list(grid = c(3, 4)),
    full_factorial_rcbd = list(grid = c(3, 4), groups = "REP"),
    incomplete_blocks_labels = list(grid = c(5, 4), groups = c("REP", "IBLOCK")),
    alpha_lattice = list(grid = c(4, 6), groups = c("REP", "IBLOCK")),
    square_lattice = list(grid = c(4, 8), groups = c("REP", "IBLOCK")),
    rectangular_lattice = list(grid = c(6, 4), groups = c("REP", "IBLOCK")),
    split_plot_rcbd = list(grid = c(3, 4), groups = c("REP", "WHOLE_PLOT")),
    split_plot_crd = list(grid = c(3, 4), groups = "WHOLE_PLOT"),
    split_split_plot_rcbd = list(grid = c(4, 4),
                                groups = c("REP", "WHOLE_PLOT", "SUB_PLOT")),
    split_split_plot_crd = list(grid = c(4, 6), groups = c("WHOLE_PLOT", "SUB_PLOT")),
    latin_square = list(grid = c(4, 8), groups = "SQUARE",
                        axes = c("ROW_SQ", "COLUMN_SQ")),
    strip_plot = list(grid = c(3, 4), groups = "REP", axes = c("HSTRIP", "VSTRIP"))
  )
  for (name in names(cases)) {
    case <- cases[[name]]
    original <- catalogue_design(name)
    constructor <- get(catalogue[[name]]$fun, asNamespace("FielDHub"))
    dims <- setNames(as.list(case$grid), c("nrows", "ncols"))
    design <- do.call(constructor, c(original$metadata$parameters, dims))
    book <- design$fieldBook
    expect_identical(book, do.call(field_layout, c(list(original), dims)), info = name)
    expect_identical(book, field_layout(original, ncols = dims$ncols), info = name)
    expect_identical(book, field_layout(original, nrows = dims$nrows), info = name)
    expect_identical(book, field_layout(design), info = name)
    expect_identical(book, plot_layout(design)$allSitesFieldbook, info = name)
    expect_identical(design, reproduce_design(design), info = name)
    expect_equal(unname(dim(table(book$ROW, book$COLUMN))), case$grid, info = name)
    expect_true(all(table(book$LOCATION, book$ROW, book$COLUMN) == 1L), info = name)

    assignments <- book[setdiff(names(book), c("ROW", "COLUMN"))]
    if (name == "latin_square") {
      names(assignments)[match(case$axes, names(assignments))] <- c("ROW", "COLUMN")
    }
    expect_identical(assignments[names(original$fieldBook)], original$fieldBook, info = name)
    if (name %in% c("split_plot_crd", "split_split_plot_crd")) {
      book$WHOLE_PLOT <- interaction(book$REP, book$WHOLE_PLOT, drop = TRUE)
    }
    groups <- c("LOCATION", case$groups)
    for (level in seq_along(groups)) {
      units <- split(book, as.list(book[groups[seq_len(level)]]), drop = TRUE)
      for (unit in units) {
        expect_equal((diff(range(unit$ROW)) + 1) * (diff(range(unit$COLUMN)) + 1),
                     nrow(unit), info = name)
      }
    }
    if (length(case$axes)) {
      for (unit in units) {
        expect_equal(nrow(unique(unit[c("ROW", case$axes[1L])])), length(unique(unit$ROW)))
        expect_equal(nrow(unique(unit[c("COLUMN", case$axes[2L])])), length(unique(unit$COLUMN)))
      }
    }
  }
})

test_that("dimension errors suggest grids that preserve fixed layouts", {
  expect_error(latin_square(4, reps = 2, nrows = 2, ncols = 16, seed = 1),
               "Valid dimensions.*4 x 8.*8 x 4", class = "fieldhub_input_error")
  expect_error(strip_plot(3, 2, reps = 2, nrows = 2, ncols = 6, seed = 1),
               "Valid dimensions.*3 x 4", class = "fieldhub_input_error")
  expect_error(CRD(6, 2, nrows = 5, seed = 1),
               "Valid dimensions.*4 x 3", class = "fieldhub_input_error")
  expect_error(field_layout(row_column(12, nrows = 3, reps = 2, seed = 1), nrows = 4),
               "not supported", class = "fieldhub_input_error")
})
