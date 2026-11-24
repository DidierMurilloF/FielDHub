library(FielDHub)

test_that("the planting path goes along the rows, every second row back for a serpentine", {
  expect_identical(
    planting_path(3, 4, "serpentine"),
    cbind(ROW = rep(1:3, each = 4), COLUMN = c(1:4, 4:1, 1:4))
  )
  expect_identical(
    planting_path(3, 4, "cartesian"),
    cbind(ROW = rep(1:3, each = 4), COLUMN = rep(1:4, times = 3))
  )
})

test_that("field-map helpers use the planting path", {
  for (planter in c("serpentine", "cartesian")) {
    path <- field_path(3, 4, planter)
    planted <- planting_path(3, 4, planter)
    expect_identical(path[, "row"], 4L - planted[, "ROW"])
    expect_identical(path[, "col"], planted[, "COLUMN"])

    map <- matrix(0L, nrow = 3, ncol = 4)
    map[3, 2] <- -1L
    free <- path[map[path] == 0L, , drop = FALSE]
    values <- seq_len(nrow(free))
    filled <- fill_along_path(map, values, planter)
    expect_identical(filled[free], values)
    expect_identical(filled[3, 2], -1L)

    for (n in 0:4) {
      at_end <- path_end(3, 4, planter, n)
      expected <- if (n == 0) path[integer(), , drop = FALSE] else tail(path, n)[n:1, , drop = FALSE]
      rownames(expected) <- NULL
      expect_identical(at_end, expected)
      expect_identical(filler_columns(3, 4, planter, n), sort(at_end[, "col"]))
    }
  }
})

test_that("along_rows matches the former serpentine layout", {
  for (nrows in 1:4) {
    M <- matrix(seq_len(nrows * 5), nrow = nrows, byrow = TRUE)
    expected <- M
    for (row in seq_len(nrows)) {
      if (row %% 2 == 0) expected[row, ] <- rev(expected[row, ])
    }
    expect_identical(along_rows(M, "serpentine"), expected)
    expect_identical(along_rows(M, "cartesian"), M)
  }
})

test_that("top-row fillers start at the end of the planting path", {
  map <- matrix(0, nrow = 3, ncol = 4)
  map[1, 3] <- 1
  filled <- top_row_fillers(map, n = 2, planter = "serpentine")
  expect_identical(as.character(filled[1, ]), c("0", "Filler", "Filler", "Filler"))

  for (nrows in 1:4) {
    for (planter in c("serpentine", "cartesian")) {
      map <- matrix(0, nrow = nrows, ncol = 5)
      filled <- top_row_fillers(map, n = 3, planter = planter)
      expect_identical(which(filled[1, ] == "Filler"),
                       filler_columns(nrows, 5, planter, 3))
    }
  }
})

test_that("field_layout() returns the field book that plot() draws", {
  rcbd <- RCBD(t = 6, reps = 3, l = 2, plotNumber = c(101, 1001), seed = 1)
  for (stacked in c("vertical", "horizontal")) {
    for (planter in c("serpentine", "cartesian")) {
      drawn <- suppressWarnings(plot_layout(rcbd, layout = 2, planter = planter,
                                            stacked = stacked))
      expect_identical(
        field_layout(rcbd, layout = 2, planter = planter, stacked = stacked),
        drawn$allSitesFieldbook
      )
    }
  }
})

test_that("field_layout() puts the identifiers and coordinates first", {
  book <- field_layout(alpha_lattice(t = 12, k = 4, reps = 2, seed = 1))
  expect_identical(names(book)[1:5], c("ID", "LOCATION", "PLOT", "ROW", "COLUMN"))
})

test_that("field_layout() returns the field book of designs placed when they are built", {
  prep <- partially_replicated(nrows = 8, ncols = 5, repGens = c(10, 20),
                               repUnits = c(2, 1), seed = 1)
  expect_identical(field_layout(prep), prep$fieldBook)
})

test_that("every layout option gives each plot its own coordinates", {
  # Runs on every platform, unlike the golden snapshots of the layouts
  for (name in names(catalogue)) {
    design <- catalogue_design(name)
    if (!inherits(design, "FielDHub") || inherits(design, "fieldhub_split_families")) next
    for (stacked in c("vertical", "horizontal", "grid_panel")) {
      for (planter in c("serpentine", "cartesian")) {
        options <- layout_options(design, planter = planter, stacked = stacked)
        for (books in options) {
          for (book in books) {
            expect_false(anyDuplicated(book[c("ROW", "COLUMN")]) > 0,
                         info = paste(name, stacked, planter))
            expect_setequal(book$ID, book$ID[!duplicated(book$ID)])
          }
        }
      }
    }
  }
})

test_that("field_layout() lists the layout options when one is not available", {
  rcbd <- RCBD(t = 6, reps = 3, seed = 1)
  err <- tryCatch(field_layout(rcbd, layout = 99), fieldhub_input_error = function(e) e)
  expect_s3_class(err, "fieldhub_input_error")
  expect_match(conditionMessage(err), "Layout option 99 is not available")
  expect_identical(err$options, seq_along(layout_options(rcbd)[[1]]))
})

test_that("field_layout() checks the planter and the stacking", {
  rcbd <- RCBD(t = 6, reps = 3, seed = 1)
  expect_error(field_layout(rcbd, planter = "zigzag"), class = "fieldhub_input_error")
  expect_error(field_layout(rcbd, stacked = "diagonal"), class = "fieldhub_input_error")
  expect_error(field_layout(rcbd, stacked = "grid_panel"), "not available",
               class = "fieldhub_input_error")
  expect_error(field_layout(rcbd$fieldBook), class = "fieldhub_input_error")
})

test_that("field_layout() explains that split_families() results have no layout", {
  set.seed(77)
  families <- split_families(l = 2, data = data.frame(
    ENTRY = 1:40, NAME = paste0("SB-", 1:40), FAMILY = rep(1:8, each = 5)
  ))
  expect_error(field_layout(families), "no field layout", class = "fieldhub_input_error")
})

test_that("a CRD with unequal replication has layouts", {
  # Regression test: plot_layout() assumed every treatment had every rep and
  # failed with "`ROW` must be size 10 or 1, not 12".
  crd <- CRD(data = data.frame(Treatment = paste0("T", 1:4), Reps = c(2, 3, 2, 3)),
             seed = 3)
  options <- layout_options(crd)[[1]]
  expect_length(options, 2)
  expect_identical(c(max(options[[1]]$ROW), max(options[[1]]$COLUMN)), c(2L, 5L))
  expect_identical(c(max(options[[2]]$ROW), max(options[[2]]$COLUMN)), c(5L, 2L))
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_s3_class(plot(crd)$p, "ggplot")
})

test_that("split-split plots in complete blocks follow the stacking", {
  # Regression test: plot_layout() did not pass `stacked` on, so every
  # stacking gave the vertical layouts.
  sspd <- split_split_plot(wp = 2, sp = 2, ssp = 2, reps = 2, plotNumber = 101, seed = 13)
  vertical <- field_layout(sspd, stacked = "vertical")
  horizontal <- field_layout(sspd, stacked = "horizontal")
  # Vertically the reps are stacked in rows, horizontally side by side
  expect_true(all(vertical$ROW[vertical$REP == 1] <= 2))
  expect_true(all(horizontal$COLUMN[horizontal$REP == 1] <= 2))
  expect_false(all(horizontal$ROW[horizontal$REP == 1] <= 2))
})

test_that("plot() explains that a stacking is not available", {
  # Regression test: grid_panel failed with "undefined columns selected" for
  # designs with two reps, and RCBD() designs rejected it with a plain error.
  # plot_layout() used to warn (fieldhub_layout_warning) and return NULL
  # instead of raising the classed error directly.
  alpha <- alpha_lattice(t = 12, k = 4, reps = 2, seed = 1)
  expect_warning(
    expect_error(plot(alpha, stacked = "grid_panel"), "Stacking 'grid_panel' is not available",
                 class = "fieldhub_input_error"),
    NA
  )
  expect_warning(
    expect_error(plot(RCBD(t = 6, reps = 3, seed = 1), stacked = "grid_panel"),
                 "not available", class = "fieldhub_input_error"),
    NA
  )
})

# Field dimensions (nrows, ncols) ---------------------------------------

# Step of the planting path at which each row of a one-location field book
# is planted
grid_path_step <- function(book, planter) {
  path <- planting_path(max(book$ROW), max(book$COLUMN), planter)
  match(paste(book$ROW, book$COLUMN), paste(path[, "ROW"], path[, "COLUMN"]))
}

grid_cases <- list(
  CRD_data = list(grid = c(2, 5)),
  RCBD_two_locations = list(grid = c(6, 3), groups = "REP"),
  RCBD_checks = list(grid = c(4, 9), groups = "REP"),
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

test_that("field dimensions keep the randomization and tile the experimental units", {
  for (name in names(grid_cases)) {
    case <- grid_cases[[name]]
    original <- catalogue_design(name)
    dims <- setNames(as.list(case$grid), c("nrows", "ncols"))
    planter <- original$metadata$parameters$planter
    if (is.null(planter)) planter <- "serpentine"
    layout <- function(...) field_layout(original, planter = planter, ...)
    design <- do.call(catalogue[[name]]$fun, c(original$metadata$parameters, dims))
    book <- design$fieldBook
    expect_identical(book, layout(nrows = dims$nrows, ncols = dims$ncols), info = name)
    expect_identical(book, layout(ncols = dims$ncols), info = name)
    expect_identical(book, layout(nrows = dims$nrows), info = name)
    expect_identical(book, field_layout(design), info = name)
    expect_identical(book, plot_layout(design)$allSitesFieldbook, info = name)
    expect_identical(design, reproduce_design(design), info = name)
    expect_type(book$ROW, "integer")
    expect_type(book$COLUMN, "integer")
    expect_identical(dim(table(book$ROW, book$COLUMN)), as.integer(case$grid), info = name)
    expect_true(all(table(book$LOCATION, book$ROW, book$COLUMN) == 1L), info = name)

    # The randomization is the default design's: only PLOT, ROW and COLUMN differ
    assignments <- book[setdiff(names(book), c("PLOT", "ROW", "COLUMN"))]
    if (name == "latin_square") {
      names(assignments)[match(case$axes, names(assignments))] <- c("ROW", "COLUMN")
    }
    kept <- setdiff(names(original$fieldBook), "PLOT")
    expect_identical(assignments[kept], original$fieldBook[kept], info = name)
    expect_identical(sort(book$PLOT), sort(original$fieldBook$PLOT), info = name)

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

test_that("one field dimension builds and reproduces the same design as both", {
  for (name in names(grid_cases)) {
    original <- catalogue_design(name)
    dims <- setNames(as.list(grid_cases[[name]]$grid), c("nrows", "ncols"))
    fun <- catalogue[[name]]$fun
    both <- do.call(fun, c(original$metadata$parameters, dims))
    for (one in names(dims)) {
      design <- do.call(fun, c(original$metadata$parameters, dims[one]))
      expect_identical(design$fieldBook, both$fieldBook, info = paste(name, one))
      expect_identical(design$metadata$parameters[[one]], dims[[one]], info = paste(name, one))
      expect_identical(design, reproduce_design(design), info = paste(name, one))
    }
  }
})

test_that("plot numbers follow the planter's path across the whole field", {
  for (name in names(grid_cases)) {
    original <- catalogue_design(name)
    dims <- grid_cases[[name]]$grid
    for (planter in c("serpentine", "cartesian")) {
      book <- field_layout(original, planter = planter, nrows = dims[1L], ncols = dims[2L])
      info <- paste(name, planter)
      expect_identical(dim(table(book$ROW, book$COLUMN)), as.integer(dims), info = info)
      for (site in split(book, book$LOCATION, drop = TRUE)) {
        # A plot is numbered when the planter first reaches it; a whole plot
        # of a split plot holds several cells under one number
        reached <- tapply(grid_path_step(site, planter), site$PLOT, min)
        expect_false(is.unsorted(reached, strictly = TRUE), info = info)
        if (!anyDuplicated(site$PLOT)) {
          expect_identical(order(site$PLOT), order(grid_path_step(site, planter)), info = info)
        }
      }
    }
  }
})

test_that("consecutive plot numbers are neighbours in a serpentine field", {
  designs <- list(
    RCBD(t = 6, reps = 6, seed = 1, nrows = 4, ncols = 9),
    RCBD(t = 5, reps = 3, l = 2, continuous = TRUE, seed = 6, ncols = 3),
    incomplete_blocks(t = 12, k = 4, reps = 2, seed = 17, nrows = 6, ncols = 4),
    alpha_lattice(t = 12, k = 3, reps = 6, seed = 36, nrows = 4),
    full_factorial(setfactors = c(2, 3), reps = 2, type = 2, seed = 9, nrows = 4),
    latin_square(t = 4, reps = 2, seed = 7, nrows = 4, ncols = 8),
    strip_plot(Hplots = 3, Vplots = 2, reps = 2, seed = 15, nrows = 6),
    CRD(t = 5, reps = 3, seed = 1, nrows = 3)
  )
  for (design in designs) {
    for (site in split(design$fieldBook, design$fieldBook$LOCATION, drop = TRUE)) {
      site <- site[order(site$PLOT), ]
      expect_true(all(abs(diff(site$ROW)) + abs(diff(site$COLUMN)) == 1L),
                  info = class(design)[1L])
    }
  }
  # The numbering does not restart in each block of an RCBD: three blocks of
  # two rows lie side by side, and the planter crosses all three in each row
  book <- designs[[1L]]$fieldBook
  expect_identical(book$PLOT[order(book$ROW, book$COLUMN)][1:18],
                   c(101:106, 201:203, 306:301, 206:204))
  expect_identical(book$REP[order(book$ROW, book$COLUMN)][1:18], rep(rep(1:3, each = 3), 2))
})

test_that("consecutive whole plots are neighbours in a serpentine field", {
  # Whole plots two rows tall: the planter follows the grid of whole plots,
  # so the whole plot after 103 is the one beside it, not across the field
  designs <- list(
    split_plot(wp = 3, sp = 2, reps = 2, seed = 11, nrows = 4),
    split_plot(wp = 2, sp = 3, reps = 3, l = 2, seed = 4, ncols = 2),
    split_split_plot(wp = 2, sp = 2, ssp = 2, reps = 2, seed = 1, nrows = 4),
    split_split_plot(wp = 2, sp = 2, ssp = 2, reps = 2, seed = 1, nrows = 2)
  )
  for (design in designs) {
    for (site in split(design$fieldBook, design$fieldBook$LOCATION, drop = TRUE)) {
      top <- tapply(site$ROW, site$PLOT, min)
      left <- tapply(site$COLUMN, site$PLOT, min)
      height <- tapply(site$ROW, site$PLOT, max) - top + 1L
      width <- tapply(site$COLUMN, site$PLOT, max) - left + 1L
      step_row <- abs(diff(top))
      step_col <- abs(diff(left))
      beside <- (step_row == 0L & step_col == width[-length(width)]) |
        (step_col == 0L & step_row == height[-length(height)])
      expect_true(all(beside), info = class(design)[1L])
    }
  }
  book <- designs[[1L]]$fieldBook
  map <- tapply(book$PLOT, list(book$ROW, book$COLUMN), unique)
  expect_equal(unname(map[3L, ]), c(203, 202, 201))
})

test_that("plot() follows the rules of field_layout() for field grids", {
  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  stored <- RCBD(t = 6, reps = 6, seed = 1, nrows = 4)
  fixed <- "built on a fixed 4 x 9 field with a serpentine planter"
  expect_error(plot(stored, planter = "cartesian"), fixed, class = "fieldhub_input_error")
  expect_error(plot(stored, stacked = "horizontal"), fixed, class = "fieldhub_input_error")
  expect_identical(plot(stored)$field_book, stored$fieldBook)
  expect_identical(plot(stored, planter = "serpentine", stacked = "vertical")$field_book,
                   stored$fieldBook)
  x <- RCBD(t = 6, reps = 6, seed = 1)
  expect_error(plot(x, layout = 2, nrows = 4), "'layout'.*'stacked'",
               class = "fieldhub_input_error")
  expect_error(plot(x, stacked = "horizontal", ncols = 9), "'layout'.*'stacked'",
               class = "fieldhub_input_error")
  expect_identical(plot(x, nrows = 4)$field_book, field_layout(x, nrows = 4))
})

test_that("fields of one row and of one column are valid grids", {
  designs <- list(CRD(t = 4, reps = 3, seed = 1),
                  RCBD(t = 3, reps = 2, seed = 1),
                  incomplete_blocks(t = 4, k = 2, reps = 2, seed = 1),
                  split_plot(wp = 2, sp = 2, reps = 2, seed = 1))
  for (design in designs) {
    n <- nrow(design$fieldBook)
    row <- field_layout(design, nrows = 1)
    expect_identical(row$ROW, rep(1L, n))
    expect_identical(sort(row$COLUMN), seq_len(n))
    expect_identical(row$PLOT[order(row$COLUMN)], sort(design$fieldBook$PLOT))
    column <- field_layout(design, ncols = 1)
    expect_identical(column$COLUMN, rep(1L, n))
    expect_identical(sort(column$ROW), seq_len(n))
    expect_identical(column$PLOT[order(column$ROW)], sort(design$fieldBook$PLOT))
  }
})

test_that("RCBD with dimensions keeps plotNumber by blocks and adds the field map", {
  args <- list(t = 4, checks = 1, rep_checks = 2, reps = 6, l = 2,
               plotNumber = c(101, 1001), seed = 42)
  original <- do.call(RCBD, args)
  spatial <- do.call(RCBD, c(args, list(nrows = 4, ncols = 9)))
  book <- spatial$fieldBook
  expect_identical(names(original), c("infoDesign", "layoutRandom", "plotNumber",
                                      "fieldBook", "metadata"))
  expect_null(original$infoDesign$nrows)
  expect_identical(spatial$layoutRandom, original$layoutRandom)
  expect_identical(names(spatial$plotNumber), names(original$plotNumber))
  expect_identical(names(spatial$plotNumberField), names(original$plotNumber))
  expect_equal(as.numeric(table(book$LOCATION, book$REP, book$CHECKS)[, , "1"]), rep(2, 12))
  sites <- split(book, book$LOCATION, drop = TRUE)
  for (i in seq_along(sites)) {
    site <- sites[[i]]
    plots <- spatial$plotNumber[[i]]
    expect_identical(dim(plots), dim(original$plotNumber[[i]]))
    expect_type(plots, "integer")
    # plotNumber still lines up cell for cell with layoutRandom
    treatments <- do.call(rbind, strsplit(spatial$layoutRandom[[i]][, 2L], " "))
    expect_identical(site$TREATMENT[match(plots, site$PLOT)], as.vector(treatments))
    expect_identical(site$REP[match(plots, site$PLOT)], as.vector(row(plots)))
    # The field map has field row 1 in its last row, as other field maps
    map <- spatial$plotNumberField[[i]]
    expect_identical(dim(map), c(4L, 9L))
    expect_identical(map[cbind(5L - site$ROW, site$COLUMN)], site$PLOT)
  }
  expect_identical(spatial$infoDesign[c("nrows", "ncols")], list(nrows = 4L, ncols = 9L))
  expect_identical(RCBD(t = 6, reps = 6, seed = 1, ncols = 9)$infoDesign[c("nrows", "ncols")],
                   list(nrows = 4L, ncols = 9L))
  expect_true(any(grepl("nrows", capture.output(print(spatial)))))
  expect_true(any(grepl("ncols", capture.output(summary(spatial)))))
  expect_true(any(grepl("Field map of plot numbers", capture.output(summary(spatial)))))
})

test_that("invalid field dimensions suggest grids that preserve the design", {
  x <- RCBD(t = 6, reps = 6, seed = 1)
  expect_error(field_layout(x, nrows = 5, ncols = 7), "36 plots.*4 x 9",
               class = "fieldhub_input_error")
  expect_error(RCBD(t = 6, reps = 6, nrows = 5), "Valid dimensions",
               class = "fieldhub_input_error")
  expect_error(latin_square(4, reps = 2, nrows = 2, ncols = 16, seed = 1),
               "Valid dimensions.*4 x 8.*8 x 4", class = "fieldhub_input_error")
  expect_error(strip_plot(3, 2, reps = 2, nrows = 2, ncols = 6, seed = 1),
               "Valid dimensions.*3 x 4", class = "fieldhub_input_error")
  expect_error(CRD(6, 2, nrows = 5, seed = 1),
               "Valid dimensions.*4 x 3", class = "fieldhub_input_error")
  expect_error(field_layout(row_column(12, nrows = 3, reps = 2, seed = 1), nrows = 4),
               "not supported", class = "fieldhub_input_error")
})

test_that("field dimensions must be single positive whole numbers", {
  designs <- list(RCBD(t = 6, reps = 6, seed = 1), CRD(t = 6, reps = 6, seed = 1))
  for (x in designs) {
    for (value in list("4", c(4, 9), NA, NA_real_, 0, -4, TRUE, Inf, 1.5, list(4))) {
      expect_error(field_layout(x, nrows = value), "`nrows` must be one positive whole number",
                   class = "fieldhub_input_error")
      expect_error(field_layout(x, nrows = 4, ncols = value),
                   "`ncols` must be one positive whole number", class = "fieldhub_input_error")
    }
  }
  expect_error(CRD(t = 6, reps = 6, seed = 1, nrows = "4"), "`nrows`",
               class = "fieldhub_input_error")
  expect_error(RCBD(t = 6, reps = 6, seed = 1, ncols = 0), "`ncols`",
               class = "fieldhub_input_error")
})

test_that("a design built on a field grid returns that grid only", {
  designs <- list(
    serpentine = RCBD(t = 6, reps = 6, seed = 1, nrows = 4),
    cartesian = RCBD(t = 6, reps = 6, seed = 1, nrows = 4, planter = "cartesian"),
    serpentine = incomplete_blocks(t = 12, k = 4, reps = 3, seed = 1, nrows = 4),
    cartesian = latin_square(t = 3, reps = 3, seed = 1, ncols = 9, planter = "cartesian")
  )
  for (i in seq_along(designs)) {
    stored <- designs[[i]]
    built <- names(designs)[i]
    rows <- max(stored$fieldBook$ROW)
    other <- setdiff(c("serpentine", "cartesian"), built)
    # Defaults, the planter of the design and its own dimensions give the grid
    expect_identical(field_layout(stored), stored$fieldBook)
    expect_identical(field_layout(stored, planter = built), stored$fieldBook)
    expect_identical(field_layout(stored, stacked = "vertical"), stored$fieldBook)
    expect_identical(field_layout(stored, nrows = rows, ncols = 9), stored$fieldBook)
    expect_identical(field_layout(stored, ncols = 9, planter = built), stored$fieldBook)
    fixed <- paste0("built on a fixed ", rows, " x 9 field with a ", built,
                    " planter.*again with other 'nrows' and 'ncols', or without them")
    expect_error(field_layout(stored, planter = other), fixed, class = "fieldhub_input_error")
    expect_error(field_layout(stored, ncols = 9, planter = other), fixed,
                 class = "fieldhub_input_error")
    expect_error(field_layout(stored, stacked = "horizontal"), fixed,
                 class = "fieldhub_input_error")
    expect_error(field_layout(stored, nrows = 6), fixed, class = "fieldhub_input_error")
    expect_error(field_layout(stored, nrows = rows, ncols = 6), fixed,
                 class = "fieldhub_input_error")
  }
})

test_that("field dimensions cannot be combined with a numbered layout or a stacking", {
  x <- RCBD(t = 6, reps = 6, seed = 1)
  expect_error(field_layout(x, layout = 2, nrows = 4), "'layout'.*'stacked'",
               class = "fieldhub_input_error")
  expect_error(field_layout(x, stacked = "horizontal", ncols = 9), "'layout'.*'stacked'",
               class = "fieldhub_input_error")
})

test_that("plot-number maps of a design follow the plots to the grid", {
  squares <- latin_square(t = 3, reps = 2, seed = 1, nrows = 3, ncols = 6)
  book <- squares$fieldBook
  for (i in 1:2) {
    square <- book[book$SQUARE == i, ]
    at <- cbind(match(square$ROW_SQ, unique(square$ROW_SQ)),
                match(square$COLUMN_SQ, unique(square$COLUMN_SQ)))
    expect_identical(squares$plotSquares[[i]][at], square$PLOT)
  }
  expect_identical(lapply(squares$plotSquares, dim),
                   lapply(latin_square(t = 3, reps = 2, seed = 1)$plotSquares, dim))

  for (type in 1:2) {
    split <- split_plot(wp = 3, sp = 2, reps = 2, l = 2, type = type, seed = 11, nrows = 3)
    for (loc in 1:2) {
      site <- split$fieldBook[split$fieldBook$LOCATION == loc, ]
      whole <- site[!duplicated(site$PLOT), ]
      expect_identical(unname(split$layoutlocations[[loc]][, "PLOT"]), as.character(whole$PLOT))
      expect_identical(unname(split$layoutlocations[[loc]][, "Whole-plot"]),
                       as.character(whole$WHOLE_PLOT))
    }
  }

  strips <- strip_plot(Hplots = 3, Vplots = 2, reps = 2, l = 2, seed = 1, nrows = 3)
  for (loc in 1:2) {
    site <- strips$fieldBook[strips$fieldBook$LOCATION == loc, ]
    for (rep in 1:2) {
      plots <- site$PLOT[site$REP == rep]
      expect_setequal(strips$plotLayouts[[loc]][[rep]], plots)
      expect_identical(as.vector(t(strips$plotLayouts[[loc]][[rep]])), plots)
    }
  }
})
