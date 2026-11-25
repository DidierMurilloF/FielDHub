library(FielDHub)

# Field layouts: field_layout(), plot() and the field dimensions, planters
# and stackings of the designs built on a field.

# One expectation for a case that checks several properties: it names the
# case and every property that fails.
expect_case <- function(ok, case) {
  bad <- names(ok)[!vapply(ok, isTRUE, logical(1))]
  expect(length(bad) == 0L, sprintf("case %s: %s", case, paste(bad, collapse = ", ")))
}

# Whether `expr` signals an error of `class` whose message matches `regexp`
signals <- function(expr, class = "fieldhub_input_error", regexp = NULL) {
  tryCatch({
    force(expr)
    FALSE
  }, error = function(e) {
    inherits(e, class) && (is.null(regexp) || grepl(regexp, conditionMessage(e)))
  })
}

# Independent reference for the planter: the cells "ROW COLUMN" in the order
# it plants them, line by line (rows, or columns when `by_column`). A
# serpentine planter turns back on every second line; `reverse_first` starts
# it on the way back.
planter_path <- function(rows, cols, planter, by_column = FALSE, reverse_first = FALSE) {
  lines <- if (by_column) cols else rows
  along <- if (by_column) rows else cols
  unlist(lapply(seq_along(lines), function(i) {
    back <- planter == "serpentine" && xor(i %% 2 == 0, reverse_first)
    cells <- if (back) rev(along) else along
    if (by_column) paste(cells, lines[i]) else paste(lines[i], cells)
  }))
}

# Whether the plot numbers of one field unit (a replicate, a square or a
# location) never decrease along the planter's path over that unit, which
# must be a full rectangle. The planter may go along rows or along columns,
# and a serpentine one may enter the unit on its way back.
follows_planter <- function(unit, planter) {
  rows <- sort(unique(unit$ROW))
  cols <- sort(unique(unit$COLUMN))
  plots <- stats::setNames(unit$PLOT, paste(unit$ROW, unit$COLUMN))
  for (by_column in c(FALSE, TRUE)) {
    for (reverse_first in if (planter == "serpentine") c(FALSE, TRUE) else FALSE) {
      along <- plots[planter_path(rows, cols, planter, by_column, reverse_first)]
      if (!anyNA(along) && !is.unsorted(along)) return(TRUE)
    }
  }
  FALSE
}

# Step of the planter's path across the whole field at which each row of a
# one-location field book is planted
grid_path_step <- function(book, planter) {
  path <- planter_path(seq_len(max(book$ROW)), seq_len(max(book$COLUMN)), planter)
  match(paste(book$ROW, book$COLUMN), path)
}

# Map of one column of a one-location field book, field row 1 first
field_map <- function(book, column = "PLOT") {
  map <- matrix(NA, max(book$ROW), max(book$COLUMN))
  map[cbind(book$ROW, book$COLUMN)] <- book[[column]]
  map
}

# Planting path -------------------------------------------------------------

test_that("plot numbers follow the requested planter path", {
  # Hand-derived maps, field row 1 first
  cases <- list(
    crd_serpentine = list(book = field_layout(CRD(t = 4, reps = 3, plotNumber = 1, seed = 1),
                                               planter = "serpentine", nrows = 3, ncols = 4),
                          map = rbind(1:4, 8:5, 9:12)),
    crd_cartesian = list(book = field_layout(CRD(t = 4, reps = 3, plotNumber = 1, seed = 1),
                                              planter = "cartesian", nrows = 3, ncols = 4),
                         map = rbind(1:4, 5:8, 9:12)),
    # Blocks side by side: the serpentine turns back inside each block
    rcbd_horizontal_serpentine = list(
      book = field_layout(RCBD(t = 6, reps = 3, seed = 1), layout = 2,
                          planter = "serpentine", stacked = "horizontal"),
      map = rbind(c(101:103, 201:203, 301:303), c(106:104, 206:204, 306:304))),
    rcbd_horizontal_cartesian = list(
      book = field_layout(RCBD(t = 6, reps = 3, seed = 1), layout = 2,
                          planter = "cartesian", stacked = "horizontal"),
      map = rbind(c(101:103, 201:203, 301:303), c(104:106, 204:206, 304:306)))
  )
  for (name in names(cases)) {
    expect_equal(field_map(cases[[name]]$book), cases[[name]]$map, info = name)
  }
})

# Catalogue designs whose REP column is not a block of the field
unblocked <- c("CRD_count", "CRD_labels", "CRD_data", "full_factorial_crd_data",
               "split_plot_crd", "split_split_plot_crd")

test_that("every layout option places each plot once and plants it along the planter's path", {
  # For every stacking, planter and layout option: each plot has its own
  # coordinates; designs laid out at plot time also number the plots along
  # the planter's path in each replicate (square, or location when there are
  # no blocks), and lie their replicates as 'stacked' says.
  laid_out <- layout_entries()
  for (name in names(catalogue)) {
    design <- catalogue_design(name)
    if (!inherits(design, "FielDHub") || inherits(design, "fieldhub_split_families")) next
    unit <- if (name %in% unblocked) NULL else if ("SQUARE" %in% names(design$fieldBook)) "SQUARE" else "REP"
    failures <- character()
    for (stacked in c("vertical", "horizontal", "grid_panel")) {
      for (planter in c("serpentine", "cartesian")) {
        options <- tryCatch(layout_options(design, planter = planter, stacked = stacked),
                            fieldhub_input_error = function(e) NULL)
        for (site in options) {
          for (k in seq_along(site)) {
            book <- site[[k]]
            where <- sprintf("%s/%s option %d", stacked, planter, k)
            if (anyDuplicated(book[c("LOCATION", "ROW", "COLUMN")]) || anyDuplicated(book$ID)) {
              failures <- c(failures, paste(where, "repeats a plot"))
            }
            if (!name %in% laid_out) next
            groups <- if (is.null(unit)) list(book$LOCATION) else list(book$LOCATION, book[[unit]])
            units <- split(book, groups, drop = TRUE)
            if (!all(vapply(units, follows_planter, logical(1), planter = planter))) {
              failures <- c(failures, paste(where, "leaves the planter's path"))
            }
            if (!is.null(unit) && length(unique(book[[unit]])) > 1L &&
                !identical(replicates_lie(book, unit), stacked)) {
              failures <- c(failures, paste(where, "does not stack the replicates", stacked))
            }
          }
        }
      }
    }
    expect(length(failures) == 0L, sprintf("case %s: %s", name, paste(failures, collapse = "; ")))
  }
})

# field_layout() and plot() -------------------------------------------------

test_that("field_layout() returns the field book that plot() draws", {
  rcbd <- RCBD(t = 6, reps = 3, l = 2, plotNumber = c(101, 1001), seed = 1)
  for (stacked in c("vertical", "horizontal")) {
    for (planter in c("serpentine", "cartesian")) {
      drawn <- suppressWarnings(plot_layout(rcbd, layout = 2, planter = planter,
                                            stacked = stacked))
      expect_identical(
        field_layout(rcbd, layout = 2, planter = planter, stacked = stacked),
        drawn$allSitesFieldbook, info = paste(stacked, planter)
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

test_that("field_layout() lists the layout options when one is not available", {
  rcbd <- RCBD(t = 6, reps = 3, seed = 1)
  err <- expect_error(field_layout(rcbd, layout = 99), "Layout option 99 is not available",
                      class = "fieldhub_input_error")
  expect_identical(err$options, seq_along(layout_options(rcbd)[[1]]))
})

test_that("field_layout() checks the planter, the stacking and its input", {
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
  expect_identical(lapply(options, function(b) c(max(b$ROW), max(b$COLUMN))),
                   list(c(2L, 5L), c(5L, 2L)))
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
  expect_true(all(horizontal$COLUMN[horizontal$REP == 1] <= 2) &&
                !all(horizontal$ROW[horizontal$REP == 1] <= 2))
})

test_that("plot() explains that a stacking is not available", {
  # Regression test: grid_panel failed with "undefined columns selected" for
  # designs with two reps, and RCBD() designs rejected it with a plain error;
  # plot() must raise the classed error without a warning.
  cases <- list(alpha = alpha_lattice(t = 12, k = 4, reps = 2, seed = 1),
                rcbd = RCBD(t = 6, reps = 3, seed = 1))
  for (name in names(cases)) {
    expect_warning(
      expect_error(plot(cases[[name]], stacked = "grid_panel"),
                   "Stacking 'grid_panel' is not available", class = "fieldhub_input_error"),
      NA
    )
  }
})

# Field dimensions (nrows, ncols) ---------------------------------------

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

# Whether every unit of a field book fills the rectangle it spans
units_tile <- function(book, groups) {
  all(vapply(split(book, as.list(book[groups]), drop = TRUE), function(unit) {
    (diff(range(unit$ROW)) + 1) * (diff(range(unit$COLUMN)) + 1) == nrow(unit)
  }, logical(1)))
}

test_that("field dimensions keep the randomization and tile the experimental units", {
  for (name in names(grid_cases)) {
    case <- grid_cases[[name]]
    original <- catalogue_design(name)
    planter <- original$metadata$parameters$planter
    if (is.null(planter)) planter <- "serpentine"
    layout <- function(...) field_layout(original, planter = planter, ...)
    design <- do.call(catalogue[[name]]$fun,
                      c(original$metadata$parameters, list(nrows = case$grid[1], ncols = case$grid[2])))
    book <- design$fieldBook

    # The randomization is the default design's: only PLOT, ROW and COLUMN differ
    assignments <- book[setdiff(names(book), c("PLOT", "ROW", "COLUMN"))]
    if (name == "latin_square") {
      names(assignments)[match(case$axes, names(assignments))] <- c("ROW", "COLUMN")
    }
    kept <- setdiff(names(original$fieldBook), "PLOT")
    tiled <- book
    if (name %in% c("split_plot_crd", "split_split_plot_crd")) {
      tiled$WHOLE_PLOT <- interaction(tiled$REP, tiled$WHOLE_PLOT, drop = TRUE)
    }
    groups <- c("LOCATION", case$groups)
    deepest <- split(tiled, as.list(tiled[groups]), drop = TRUE)
    # A strip or a square's row or column is one field row or column
    strips <- length(case$axes) == 0L || all(vapply(deepest, function(unit) {
      nrow(unique(unit[c("ROW", case$axes[1L])])) == length(unique(unit$ROW)) &&
        nrow(unique(unit[c("COLUMN", case$axes[2L])])) == length(unique(unit$COLUMN))
    }, logical(1)))

    expect_case(list(
      "same as field_layout() of the design" =
        identical(book, layout(nrows = case$grid[1], ncols = case$grid[2])) &&
        identical(book, layout(ncols = case$grid[2])) && identical(book, layout(nrows = case$grid[1])),
      "field_layout() and plot_layout() return the built grid" =
        identical(book, field_layout(design)) && identical(book, plot_layout(design)$allSitesFieldbook),
      "reproduces" = identical(design, reproduce_design(design)),
      "integer coordinates" = is.integer(book$ROW) && is.integer(book$COLUMN),
      "fills the grid once" = identical(dim(table(book$ROW, book$COLUMN)), as.integer(case$grid)) &&
        all(table(book$LOCATION, book$ROW, book$COLUMN) == 1L),
      "keeps the randomization" = identical(assignments[kept], original$fieldBook[kept]) &&
        identical(sort(book$PLOT), sort(original$fieldBook$PLOT)),
      "units tile rectangles" = all(vapply(seq_along(groups), function(level) {
        units_tile(tiled, groups[seq_len(level)])
      }, logical(1))),
      "strips are field rows and columns" = strips
    ), name)
  }
})

test_that("one field dimension builds and reproduces the same design as both", {
  for (name in names(grid_cases)) {
    original <- catalogue_design(name)
    dims <- setNames(as.list(grid_cases[[name]]$grid), c("nrows", "ncols"))
    fun <- catalogue[[name]]$fun
    both <- do.call(fun, c(original$metadata$parameters, dims))
    ok <- lapply(names(dims), function(one) {
      design <- do.call(fun, c(original$metadata$parameters, dims[one]))
      identical(design$fieldBook, both$fieldBook) &&
        identical(design$metadata$parameters[[one]], dims[[one]]) &&
        identical(design, reproduce_design(design))
    })
    expect_case(setNames(ok, paste("only", names(dims))), name)
  }
})

test_that("plot numbers follow the planter's path across the whole field", {
  for (name in names(grid_cases)) {
    original <- catalogue_design(name)
    dims <- grid_cases[[name]]$grid
    ok <- lapply(c("serpentine", "cartesian"), function(planter) {
      book <- field_layout(original, planter = planter, nrows = dims[1L], ncols = dims[2L])
      identical(dim(table(book$ROW, book$COLUMN)), as.integer(dims)) &&
        all(vapply(split(book, book$LOCATION, drop = TRUE), function(site) {
          # A plot is numbered when the planter first reaches it; a whole plot
          # of a split plot holds several cells under one number
          step <- grid_path_step(site, planter)
          reached <- tapply(step, site$PLOT, min)
          !is.unsorted(reached, strictly = TRUE) &&
            (anyDuplicated(site$PLOT) > 0L || identical(order(site$PLOT), order(step)))
        }, logical(1)))
    })
    expect_case(setNames(ok, c("serpentine", "cartesian")), name)
  }
})

test_that("consecutive plot numbers are neighbours in a serpentine field", {
  designs <- list(
    RCBD = RCBD(t = 6, reps = 6, seed = 1, nrows = 4, ncols = 9),
    RCBD_two_locations = RCBD(t = 5, reps = 3, l = 2, continuous = TRUE, seed = 6, ncols = 3),
    incomplete_blocks = incomplete_blocks(t = 12, k = 4, reps = 2, seed = 17, nrows = 6, ncols = 4),
    alpha_lattice = alpha_lattice(t = 12, k = 3, reps = 6, seed = 36, nrows = 4),
    full_factorial = full_factorial(setfactors = c(2, 3), reps = 2, type = 2, seed = 9, nrows = 4),
    latin_square = latin_square(t = 4, reps = 2, seed = 7, nrows = 4, ncols = 8),
    strip_plot = strip_plot(Hplots = 3, Vplots = 2, reps = 2, seed = 15, nrows = 6),
    CRD = CRD(t = 5, reps = 3, seed = 1, nrows = 3)
  )
  for (name in names(designs)) {
    sites <- split(designs[[name]]$fieldBook, designs[[name]]$fieldBook$LOCATION, drop = TRUE)
    adjacent <- vapply(sites, function(site) {
      site <- site[order(site$PLOT), ]
      all(abs(diff(site$ROW)) + abs(diff(site$COLUMN)) == 1L)
    }, logical(1))
    expect(all(adjacent), sprintf("case %s: location %s jumps", name, names(adjacent)[!adjacent][1]))
  }
  # The numbering does not restart in each block of an RCBD: three blocks of
  # two rows lie side by side, and the planter crosses all three in each row
  book <- designs$RCBD$fieldBook
  first_rows <- order(book$ROW, book$COLUMN)[1:18]
  expect_identical(list(book$PLOT[first_rows], book$REP[first_rows]),
                   list(c(101:106, 201:203, 306:301, 206:204), rep(rep(1:3, each = 3), 2)))
})

test_that("consecutive whole plots are neighbours in a serpentine field", {
  # Whole plots two rows tall: the planter follows the grid of whole plots,
  # so the whole plot after 103 is the one beside it, not across the field
  designs <- list(
    split_plot = split_plot(wp = 3, sp = 2, reps = 2, seed = 11, nrows = 4),
    split_plot_two_locations = split_plot(wp = 2, sp = 3, reps = 3, l = 2, seed = 4, ncols = 2),
    split_split_plot_tall = split_split_plot(wp = 2, sp = 2, ssp = 2, reps = 2, seed = 1, nrows = 4),
    split_split_plot_wide = split_split_plot(wp = 2, sp = 2, ssp = 2, reps = 2, seed = 1, nrows = 2)
  )
  for (name in names(designs)) {
    sites <- split(designs[[name]]$fieldBook, designs[[name]]$fieldBook$LOCATION, drop = TRUE)
    beside <- vapply(sites, function(site) {
      top <- tapply(site$ROW, site$PLOT, min)
      left <- tapply(site$COLUMN, site$PLOT, min)
      height <- tapply(site$ROW, site$PLOT, max) - top + 1L
      width <- tapply(site$COLUMN, site$PLOT, max) - left + 1L
      step_row <- abs(diff(top))
      step_col <- abs(diff(left))
      all((step_row == 0L & step_col == width[-length(width)]) |
            (step_col == 0L & step_row == height[-length(height)]))
    }, logical(1))
    expect(all(beside), sprintf("case %s: location %s jumps", name, names(beside)[!beside][1]))
  }
  book <- designs$split_plot$fieldBook
  map <- tapply(book$PLOT, list(book$ROW, book$COLUMN), unique)
  expect_equal(unname(map[3L, ]), c(203, 202, 201))
})

test_that("plot() follows the rules of field_layout() for field grids", {
  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  stored <- RCBD(t = 6, reps = 6, seed = 1, nrows = 4)
  fixed <- "built on a fixed 4 x 9 field with a serpentine planter"
  expect_error(plot(stored, planter = "cartesian"), fixed, class = "fieldhub_input_error")
  # Six blocks in a 4 x 9 field lie two down and three across: a grid panel
  expect_error(plot(stored, stacked = "horizontal"), fixed, class = "fieldhub_input_error")
  expect_error(plot(stored, stacked = "vertical"), fixed, class = "fieldhub_input_error")
  expect_identical(
    list(plot(stored)$field_book,
         plot(stored, planter = "serpentine", stacked = "grid_panel")$field_book),
    list(stored$fieldBook, stored$fieldBook))
  x <- RCBD(t = 6, reps = 6, seed = 1)
  expect_error(plot(x, layout = 2, nrows = 4), "'layout'.*cannot be combined",
               class = "fieldhub_input_error")
  # An abbreviated dimension is a dimension, as in field_layout()
  expect_identical(list(plot(x, nrows = 4)$field_book, plot(x, nrow = 4)$field_book),
                   list(field_layout(x, nrows = 4), field_layout(x, nrow = 4)))
  expect_error(plot(x, layout = 2, nrow = 4), "'layout'.*cannot be combined",
               class = "fieldhub_input_error")
  # A design built with a cartesian planter draws its grid for its own
  # dimensions, stacking and planter, or for none of them
  built <- list(RCBD = RCBD(t = 6, reps = 4, seed = 1, nrows = 4, planter = "cartesian"),
                full_factorial = full_factorial(setfactors = c(2, 3), reps = 4, type = 2, seed = 1,
                                                nrows = 4, planter = "cartesian"),
                latin_square = latin_square(t = 3, reps = 4, seed = 1, nrows = 6, planter = "cartesian"),
                strip_plot = strip_plot(Hplots = 3, Vplots = 2, reps = 4, seed = 1, nrows = 6,
                                        planter = "cartesian"))
  for (name in names(built)) {
    design <- built[[name]]
    rows <- max(design$fieldBook$ROW)
    cols <- max(design$fieldBook$COLUMN)
    own <- design$metadata$parameters$stacked
    expect_case(list(
      "defaults" = identical(plot(design)$field_book, design$fieldBook),
      "own rows" = identical(plot(design, nrows = rows)$field_book, design$fieldBook),
      "own stacking and grid" = identical(plot(design, stacked = own, nrows = rows,
                                               ncols = cols)$field_book, design$fieldBook),
      "own planter" = identical(plot(design, planter = "cartesian", ncols = cols)$field_book,
                                design$fieldBook),
      "other planter refused" = signals(plot(design, planter = "serpentine", nrows = rows),
                                        regexp = "with a cartesian planter.*or another 'stacked'")
    ), name)
  }
})

test_that("fields of one row and of one column are valid grids", {
  designs <- list(CRD = CRD(t = 4, reps = 3, seed = 1),
                  RCBD = RCBD(t = 3, reps = 2, seed = 1),
                  incomplete_blocks = incomplete_blocks(t = 4, k = 2, reps = 2, seed = 1),
                  split_plot = split_plot(wp = 2, sp = 2, reps = 2, seed = 1))
  for (name in names(designs)) {
    design <- designs[[name]]
    n <- nrow(design$fieldBook)
    row <- field_layout(design, nrows = 1)
    column <- field_layout(design, ncols = 1)
    expect_case(list(
      "one row" = identical(row$ROW, rep(1L, n)) && identical(sort(row$COLUMN), seq_len(n)) &&
        identical(row$PLOT[order(row$COLUMN)], sort(design$fieldBook$PLOT)),
      "one column" = identical(column$COLUMN, rep(1L, n)) && identical(sort(column$ROW), seq_len(n)) &&
        identical(column$PLOT[order(column$ROW)], sort(design$fieldBook$PLOT))
    ), name)
  }
})

test_that("RCBD with dimensions keeps plotNumber by blocks and adds the field map", {
  args <- list(t = 4, checks = 1, rep_checks = 2, reps = 6, l = 2,
               plotNumber = c(101, 1001), seed = 42)
  original <- do.call(RCBD, args)
  spatial <- do.call(RCBD, c(args, list(nrows = 4, ncols = 9)))
  book <- spatial$fieldBook
  expect_null(original$infoDesign$nrows)
  expect_identical(spatial$layoutRandom, original$layoutRandom)
  expect_identical(list(names(spatial$plotNumber), names(spatial$plotNumberField)),
                   list(names(original$plotNumber), names(original$plotNumber)))
  expect_equal(as.numeric(table(book$LOCATION, book$REP, book$CHECKS)[, , "1"]), rep(2, 12))
  sites <- split(book, book$LOCATION, drop = TRUE)
  for (i in seq_along(sites)) {
    site <- sites[[i]]
    plots <- spatial$plotNumber[[i]]
    treatments <- do.call(rbind, strsplit(spatial$layoutRandom[[i]][, 2L], " "))
    map <- spatial$plotNumberField[[i]]
    expect_case(list(
      "plotNumber keeps its shape" = identical(dim(plots), dim(original$plotNumber[[i]])) &&
        is.integer(plots),
      # plotNumber still lines up cell for cell with layoutRandom
      "plotNumber lines up with layoutRandom" =
        identical(site$TREATMENT[match(plots, site$PLOT)], as.vector(treatments)) &&
        identical(site$REP[match(plots, site$PLOT)], as.vector(row(plots))),
      # The field map has field row 1 in its last row, as other field maps
      "field map" = identical(dim(map), c(4L, 9L)) &&
        identical(map[cbind(5L - site$ROW, site$COLUMN)], site$PLOT)
    ), paste("location", i))
  }
  expect_identical(list(spatial$infoDesign[c("nrows", "ncols")],
                        RCBD(t = 6, reps = 6, seed = 1, ncols = 9)$infoDesign[c("nrows", "ncols")]),
                   rep(list(list(nrows = 4L, ncols = 9L)), 2))
  shown <- c(capture.output(print(spatial)), capture.output(summary(spatial)))
  expect_true(any(grepl("nrows", shown)) && any(grepl("ncols", shown)) &&
                any(grepl("Field map of plot numbers", shown)))
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
  designs <- list(RCBD = RCBD(t = 6, reps = 6, seed = 1), CRD = CRD(t = 6, reps = 6, seed = 1))
  values <- list(character = "4", two = c(4, 9), logical_na = NA, na = NA_real_, zero = 0,
                 negative = -4, logical = TRUE, infinite = Inf, fraction = 1.5, list = list(4))
  for (name in names(values)) {
    value <- values[[name]]
    ok <- unlist(lapply(names(designs), function(d) {
      x <- designs[[d]]
      setNames(list(
        signals(field_layout(x, nrows = value), regexp = "`nrows` must be one positive whole number"),
        signals(field_layout(x, nrows = 4, ncols = value),
                regexp = "`ncols` must be one positive whole number")
      ), paste(d, c("nrows", "ncols")))
    }), recursive = FALSE)
    expect_case(ok, name)
  }
  expect_error(CRD(t = 6, reps = 6, seed = 1, nrows = "4"), "`nrows`",
               class = "fieldhub_input_error")
  expect_error(RCBD(t = 6, reps = 6, seed = 1, ncols = 0), "`ncols`",
               class = "fieldhub_input_error")
})

test_that("a design built on a field grid returns that grid only", {
  designs <- list(
    list(RCBD(t = 6, reps = 6, seed = 1, nrows = 4), "serpentine"),
    list(RCBD(t = 6, reps = 6, seed = 1, nrows = 4, planter = "cartesian"), "cartesian"),
    list(incomplete_blocks(t = 12, k = 4, reps = 3, seed = 1, nrows = 4), "serpentine"),
    list(latin_square(t = 3, reps = 3, seed = 1, ncols = 9, planter = "cartesian"), "cartesian")
  )
  for (case in designs) {
    stored <- case[[1L]]
    built <- case[[2L]]
    name <- paste(class(stored)[1L], built)
    rows <- max(stored$fieldBook$ROW)
    other <- setdiff(c("serpentine", "cartesian"), built)
    own <- stored$metadata$parameters$stacked
    same <- function(book) identical(book, stored$fieldBook)
    # Defaults, the planter of the design, its own dimensions and its own
    # stacking give the grid
    expect_case(list(
      "defaults" = same(field_layout(stored)),
      "own planter" = same(field_layout(stored, planter = built)),
      "own stacking" = same(field_layout(stored, stacked = own)) &&
        same(field_layout(stored, stacked = own, nrows = rows, planter = built)),
      "own dimensions" = same(field_layout(stored, nrows = rows, ncols = 9)) &&
        same(field_layout(stored, ncols = 9, planter = built))
    ), name)
    fixed <- paste0("built on a fixed ", rows, " x 9 field with a ", built,
                    " planter.*again with other 'nrows' and 'ncols' or another 'stacked', ",
                    "or without them")
    refused <- list(
      "other planter" = signals(field_layout(stored, planter = other), regexp = fixed),
      "other planter with own columns" = signals(field_layout(stored, ncols = 9, planter = other),
                                                 regexp = fixed),
      "other rows" = signals(field_layout(stored, nrows = 6), regexp = fixed),
      "other columns" = signals(field_layout(stored, nrows = rows, ncols = 6), regexp = fixed)
    )
    for (another in setdiff(c("vertical", "horizontal", "grid_panel"), own)) {
      refused[[paste("stacking", another)]] <- signals(field_layout(stored, stacked = another),
                                                       regexp = fixed) &&
        signals(field_layout(stored, stacked = another, nrows = rows, ncols = 9), regexp = fixed)
    }
    expect_case(refused, paste(name, "refuses"))
  }
})

test_that("field dimensions cannot be combined with a numbered layout", {
  x <- RCBD(t = 6, reps = 6, seed = 1)
  expect_error(field_layout(x, layout = 2, nrows = 4), "'layout'.*cannot be combined",
               class = "fieldhub_input_error")
  expect_error(field_layout(x, layout = 1, stacked = "grid_panel", ncols = 9),
               "'layout'.*cannot be combined", class = "fieldhub_input_error")
})

# Stacking of the replicates in a field of given dimensions -----------------

# Rows and columns of each replicate
replicate_shape <- function(book, unit = "REP") {
  unique(lapply(split(book[c("ROW", "COLUMN")], book[[unit]]), function(u) {
    c(diff(range(u$ROW)), diff(range(u$COLUMN))) + 1
  }))
}

# Designs of four replicates, with a field for each stacking
stacking_cases <- list(
  RCBD = list(fun = RCBD, args = list(t = 6, reps = 4, seed = 1),
              vertical = c(4, 6), horizontal = c(6, 4), grid_panel = c(4, 6)),
  full_factorial = list(fun = full_factorial,
                        args = list(setfactors = c(2, 3), reps = 4, type = 2, seed = 1),
                        vertical = c(4, 6), horizontal = c(6, 4), grid_panel = c(4, 6)),
  split_plot = list(fun = split_plot, args = list(wp = 3, sp = 2, reps = 4, seed = 1),
                    vertical = c(4, 6), horizontal = c(6, 4), grid_panel = c(4, 6)),
  split_split_plot = list(fun = split_split_plot,
                          args = list(wp = 2, sp = 2, ssp = 2, reps = 4, seed = 1),
                          vertical = c(8, 4), horizontal = c(4, 8), grid_panel = c(4, 8)),
  incomplete_blocks = list(fun = incomplete_blocks, args = list(t = 12, k = 4, reps = 4, seed = 1),
                           vertical = c(8, 6), horizontal = c(6, 8), grid_panel = c(8, 6)),
  alpha_lattice = list(fun = alpha_lattice, args = list(t = 12, k = 3, reps = 4, seed = 36),
                       vertical = c(8, 6), horizontal = c(6, 8), grid_panel = c(8, 6)),
  square_lattice = list(fun = square_lattice, args = list(t = 16, k = 4, reps = 4, seed = 1),
                        vertical = c(16, 4), horizontal = c(4, 16), grid_panel = c(8, 8)),
  rectangular_lattice = list(fun = rectangular_lattice,
                             args = list(t = 12, k = 3, reps = 4, seed = 1),
                             vertical = c(8, 6), horizontal = c(6, 8), grid_panel = c(8, 6)),
  latin_square = list(fun = latin_square, args = list(t = 4, reps = 4, seed = 7), unit = "SQUARE",
                      vertical = c(16, 4), horizontal = c(4, 16), grid_panel = c(8, 8)),
  strip_plot = list(fun = strip_plot, args = list(Hplots = 3, Vplots = 2, reps = 4, seed = 1),
                    vertical = c(12, 2), horizontal = c(3, 8), grid_panel = c(6, 4))
)
stackings <- c("vertical", "horizontal", "grid_panel")

test_that("field_layout() arranges the replicates of a field as 'stacked' says", {
  for (name in names(stacking_cases)) {
    case <- stacking_cases[[name]]
    unit <- if (is.null(case$unit)) "REP" else case$unit
    x <- do.call(case$fun, case$args)
    kept <- setdiff(names(x$fieldBook), c("PLOT", if (name == "latin_square") c("ROW", "COLUMN")))
    ok <- lapply(stackings, function(stacked) {
      dims <- case[[stacked]]
      book <- field_layout(x, stacked = stacked, nrows = dims[1L], ncols = dims[2L])
      # One above another, each replicate spans all the columns; side by
      # side, all the rows; in a panel of two by two, half of each
      spans <- switch(stacked, vertical = dims / c(4, 1), horizontal = dims / c(1, 4),
                      grid_panel = dims / 2)
      identical(c(max(book$ROW), max(book$COLUMN)), as.integer(dims)) &&
        identical(replicates_lie(book, unit), stacked) &&
        identical(replicate_shape(book, unit), list(spans)) &&
        all(table(book$ROW, book$COLUMN) == 1L) &&
        # One dimension and the stacking are enough
        identical(field_layout(x, stacked = stacked, nrows = dims[1L]), book) &&
        identical(field_layout(x, stacked = stacked, ncols = dims[2L]), book) &&
        # The stacking moves the replicates only: the randomization is the design's
        identical(book[kept], x$fieldBook[kept])
    })
    expect_case(setNames(ok, stackings), name)
  }
})

test_that("a design built with dimensions and a stacking stores, prints and reproduces it", {
  for (name in names(stacking_cases)) {
    case <- stacking_cases[[name]]
    x <- do.call(case$fun, case$args)
    ok <- lapply(stackings, function(stacked) {
      dims <- case[[stacked]]
      built <- do.call(case$fun, c(case$args, list(nrows = dims[1L], ncols = dims[2L],
                                                   stacked = stacked)))
      shown <- list(capture.output(print(built)), capture.output(summary(built)))
      list(
        "same as field_layout()" = identical(
          built$fieldBook, field_layout(x, stacked = stacked, nrows = dims[1L], ncols = dims[2L])),
        "stores the stacking" = identical(built$metadata$parameters$stacked, stacked) &&
          identical(built$infoDesign$stacked, stacked) &&
          identical(built$infoDesign[c("nrows", "ncols")],
                    list(nrows = as.integer(dims[1L]), ncols = as.integer(dims[2L]))),
        # print() and summary() show the field and the stacking of every design
        "prints it" = all(vapply(shown, function(s) {
          any(grepl(paste0("nrows +: int ", dims[1L], "$"), s)) &&
            any(grepl(paste0("ncols +: int ", dims[2L], "$"), s)) &&
            any(grepl(paste0("stacked.*", stacked), s))
        }, logical(1))),
        "reproduces" = identical(reproduce_design(built), built) &&
          identical(do.call(case$fun, built$metadata$parameters), built),
        # Its own stacking or none returns the grid; another is refused
        "returns its grid" = identical(field_layout(built), built$fieldBook) &&
          identical(field_layout(built, stacked = stacked), built$fieldBook),
        "refuses another" = all(vapply(setdiff(stackings, stacked), function(another) {
          signals(field_layout(built, stacked = another), regexp = "built on a fixed")
        }, logical(1))) && signals(field_layout(built, planter = "cartesian"), regexp = "built on a fixed")
      )
    })
    ok <- unlist(lapply(seq_along(stackings), function(i) {
      setNames(ok[[i]], paste(stackings[i], names(ok[[i]])))
    }), recursive = FALSE)
    expect_case(ok, name)
  }
})

test_that("dimensions that do not fit the stacking suggest dimensions that do", {
  x <- RCBD(t = 6, reps = 4, seed = 1)
  # Four blocks one above another need a multiple of four rows
  e <- expect_error(field_layout(x, stacked = "vertical", nrows = 6, ncols = 4),
                    "one above another.*Valid dimensions.*8 x 3, 4 x 6, 12 x 2, 24 x 1",
                    class = "fieldhub_input_error")
  expect_false(grepl("6 x 4", conditionMessage(e), fixed = TRUE))
  expect_error(field_layout(x, stacked = "horizontal", nrows = 4, ncols = 6),
               "side by side.*Valid dimensions.*3 x 8, 6 x 4, 2 x 12, 1 x 24",
               class = "fieldhub_input_error")
  e <- expect_error(RCBD(t = 6, reps = 4, seed = 1, nrows = 8, ncols = 3, stacked = "grid_panel"),
                    "rows and columns of replicates.*Valid dimensions.*6 x 4.*4 x 6",
                    class = "fieldhub_input_error")
  expect_false(grepl("8 x 3", conditionMessage(e), fixed = TRUE))
  expect_error(incomplete_blocks(t = 12, k = 4, reps = 4, seed = 1, nrows = 6, stacked = "vertical"),
               "one above another.*Valid dimensions.*8 x 6", class = "fieldhub_input_error")
  expect_error(latin_square(t = 4, reps = 4, seed = 7, nrows = 8, ncols = 8, stacked = "horizontal"),
               "side by side.*Valid dimensions.*4 x 16", class = "fieldhub_input_error")
  # Three replicates cannot lie in rows and columns of replicates
  expect_error(field_layout(RCBD(t = 6, reps = 3, seed = 1), stacked = "grid_panel", nrows = 3),
               "grid_panel", class = "fieldhub_input_error")
  expect_error(RCBD(t = 6, reps = 4, seed = 1, nrows = 4, stacked = "diagonal"), "'stacked'",
               class = "fieldhub_input_error")
})

test_that("a stacking is refused where there are no replicates to stack, or no field", {
  no_replicates <- list(
    CRD = list(fun = CRD, args = list(t = 6, reps = 4, seed = 1)),
    full_factorial_crd = list(fun = full_factorial,
                              args = list(setfactors = c(2, 3), reps = 4, type = 1, seed = 1)),
    split_plot_crd = list(fun = split_plot, args = list(wp = 3, sp = 2, reps = 4, type = 1, seed = 1)),
    split_split_plot_crd = list(fun = split_split_plot,
                                args = list(wp = 2, sp = 2, ssp = 2, reps = 3, type = 1, seed = 1))
  )
  for (name in names(no_replicates)) {
    case <- no_replicates[[name]]
    x <- do.call(case$fun, case$args)
    built <- do.call(case$fun, c(case$args, list(nrows = 4)))
    shown <- capture.output(print(built))
    expect_case(list(
      "field_layout() refuses" = signals(field_layout(x, stacked = "vertical", nrows = 4),
                                         regexp = "no replicates"),
      "building refuses" = signals(do.call(case$fun, c(case$args, list(nrows = 4, stacked = "horizontal"))),
                                   regexp = "no replicates"),
      "stores no stacking" = is.null(built$metadata$parameters$stacked) &&
        is.null(built$infoDesign$stacked) && identical(built$infoDesign$nrows, 4L),
      "prints the field" = any(grepl("nrows +: int 4$", shown)) && any(grepl("ncols +: int ", shown)),
      "built design refuses" = signals(field_layout(built, stacked = "vertical"), regexp = "no replicates")
    ), name)
  }
  # A stacking arranges a field of given dimensions: alone it is refused
  alone <- lapply(stacking_cases, function(case) {
    signals(do.call(case$fun, c(case$args, list(stacked = "vertical"))),
            regexp = "'stacked'.*'nrows'.*'ncols'")
  })
  expect_case(alone, "stacking without a field")
  expect_error(CRD(t = 6, reps = 4, seed = 1, stacked = "vertical"), "'stacked'",
               class = "fieldhub_input_error")
  # A stacking that does not exist is named as such, with or without a field
  expect_error(RCBD(t = 6, reps = 4, seed = 1, stacked = "diagonal"),
               "'stacked' must be \"vertical\", \"horizontal\" or \"grid_panel\"",
               class = "fieldhub_input_error")
})

test_that("without a stacking the most square replicate is chosen", {
  # Hand-derived choices: the stacking and the shape of each replicate
  rcbd <- list(
    # Two blocks of twelve plots: 3 x 4 blocks one above another are more
    # square than 6 x 2 blocks side by side, and the other way round
    tall = list(RCBD(t = 12, reps = 2, seed = 1, nrows = 6, ncols = 4), "vertical", c(3, 4)),
    wide = list(RCBD(t = 12, reps = 2, seed = 1, nrows = 4, ncols = 6), "horizontal", c(4, 3)),
    # A panel of 2 x 3 blocks is more square than a stack of 1 x 6 blocks
    panel = list(RCBD(t = 6, reps = 4, seed = 1, nrows = 4, ncols = 6), "grid_panel", c(2, 3)),
    # A tie goes to the vertical stack: 2 x 4 blocks above, 4 x 2 beside
    tie = list(RCBD(t = 8, reps = 2, seed = 1, nrows = 4, ncols = 4), "vertical", c(2, 4)),
    # then to the horizontal one: 2 x 1 blocks side by side, or 1 x 2 in a panel
    second_tie = list(RCBD(t = 2, reps = 4, seed = 1, nrows = 2, ncols = 4), "horizontal", c(2, 1)),
    # Among the panels that fit, the most square replicate too: twelve 4 x 3
    # blocks (six rows of two) rather than 6 x 2 blocks (four rows of three)
    panels = list(RCBD(t = 12, reps = 12, seed = 1, nrows = 24, ncols = 6), "grid_panel", c(4, 3))
  )
  for (name in names(rcbd)) {
    x <- rcbd[[name]][[1L]]
    expect_identical(list(x$metadata$parameters$stacked, replicates_lie(x$fieldBook),
                          replicate_shape(x$fieldBook)),
                     list(rcbd[[name]][[2L]], rcbd[[name]][[2L]], list(rcbd[[name]][[3L]])),
                     info = name)
  }

  others <- list(
    split_plot_tall = list(split_plot(wp = 3, sp = 2, reps = 2, seed = 11, nrows = 4, ncols = 3), "vertical"),
    split_plot_wide = list(split_plot(wp = 3, sp = 2, reps = 2, seed = 11, nrows = 3, ncols = 4), "horizontal"),
    incomplete_blocks = list(incomplete_blocks(t = 12, k = 4, reps = 4, seed = 1, nrows = 8, ncols = 6),
                             "grid_panel"),
    alpha_lattice = list(alpha_lattice(t = 12, k = 3, reps = 2, seed = 1, nrows = 6, ncols = 4), "vertical"),
    latin_square = list(latin_square(t = 3, reps = 3, seed = 1, ncols = 9), "horizontal"),
    strip_plot = list(strip_plot(Hplots = 3, Vplots = 2, reps = 2, seed = 15, nrows = 6), "vertical"),
    full_factorial = list(full_factorial(setfactors = c(2, 3), reps = 2, type = 2, seed = 9, nrows = 2),
                          "horizontal")
  )
  for (name in names(others)) {
    x <- others[[name]][[1L]]
    want <- others[[name]][[2L]]
    unit <- if (inherits(x, "fieldhub_latin_square")) "SQUARE" else "REP"
    expect_case(list(
      "stores the choice" = identical(x$metadata$parameters$stacked, want) &&
        identical(x$infoDesign$stacked, want),
      "lies so" = identical(replicates_lie(x$fieldBook, unit), want),
      "reproduces" = identical(reproduce_design(x), x),
      "its stacking returns the grid" = identical(field_layout(x, stacked = want), x$fieldBook)
    ), name)
  }
})

test_that("plot() arranges the replicates of a field as 'stacked' says", {
  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (name in c("RCBD", "split_plot", "incomplete_blocks", "latin_square")) {
    case <- stacking_cases[[name]]
    unit <- if (is.null(case$unit)) "REP" else case$unit
    x <- do.call(case$fun, case$args)
    ok <- lapply(stackings, function(stacked) {
      dims <- case[[stacked]]
      book <- plot(x, stacked = stacked, nrows = dims[1L], ncols = dims[2L])$field_book
      built <- do.call(case$fun, c(case$args, list(nrows = dims[1L], stacked = stacked)))
      identical(book, field_layout(x, stacked = stacked, nrows = dims[1L], ncols = dims[2L])) &&
        identical(replicates_lie(book, unit), stacked) &&
        identical(plot(built, stacked = stacked)$field_book, built$fieldBook) &&
        identical(plot(built)$field_book, built$fieldBook)
    })
    expect_case(setNames(ok, stackings), name)
  }
  x <- RCBD(t = 6, reps = 4, seed = 1)
  # Without a stacking, the most square block: a panel of 2 x 3 blocks
  expect_identical(replicates_lie(plot(x, nrows = 4)$field_book), "grid_panel")
  expect_error(plot(x, stacked = "vertical", nrows = 6), "one above another.*Valid dimensions",
               class = "fieldhub_input_error")
  expect_error(plot(CRD(t = 6, reps = 4, seed = 1), stacked = "vertical", nrows = 4),
               "no replicates", class = "fieldhub_input_error")
  built <- RCBD(t = 6, reps = 4, seed = 1, nrows = 4, stacked = "vertical")
  expect_error(plot(built, stacked = "grid_panel"), "built on a fixed",
               class = "fieldhub_input_error")
})

test_that("plot-number maps of a design follow the plots to the grid", {
  squares <- latin_square(t = 3, reps = 2, seed = 1, nrows = 3, ncols = 6)
  book <- squares$fieldBook
  expect_case(list(
    "square maps hold the plots" = all(vapply(1:2, function(i) {
      square <- book[book$SQUARE == i, ]
      at <- cbind(match(square$ROW_SQ, unique(square$ROW_SQ)),
                  match(square$COLUMN_SQ, unique(square$COLUMN_SQ)))
      identical(squares$plotSquares[[i]][at], square$PLOT)
    }, logical(1))),
    "square maps keep their shape" = identical(
      lapply(squares$plotSquares, dim), lapply(latin_square(t = 3, reps = 2, seed = 1)$plotSquares, dim))
  ), "latin_square")

  for (type in 1:2) {
    split <- split_plot(wp = 3, sp = 2, reps = 2, l = 2, type = type, seed = 11, nrows = 3)
    ok <- vapply(1:2, function(loc) {
      site <- split$fieldBook[split$fieldBook$LOCATION == loc, ]
      whole <- site[!duplicated(site$PLOT), ]
      identical(unname(split$layoutlocations[[loc]][, "PLOT"]), as.character(whole$PLOT)) &&
        identical(unname(split$layoutlocations[[loc]][, "Whole-plot"]), as.character(whole$WHOLE_PLOT))
    }, logical(1))
    expect_case(as.list(setNames(ok, paste("location", 1:2))), paste("split_plot type", type))
  }

  strips <- strip_plot(Hplots = 3, Vplots = 2, reps = 2, l = 2, seed = 1, nrows = 3)
  ok <- list()
  for (loc in 1:2) {
    site <- strips$fieldBook[strips$fieldBook$LOCATION == loc, ]
    for (rep in 1:2) {
      plots <- site$PLOT[site$REP == rep]
      ok[[paste("location", loc, "rep", rep)]] <-
        identical(as.vector(t(strips$plotLayouts[[loc]][[rep]])), plots)
    }
  }
  expect_case(ok, "strip_plot")
})

# Plot numbers --------------------------------------------------------------

test_that("physical design engines reject malformed plot starts with input conditions", {
  # Each malformed start is refused by class before the design draws a
  # random number
  cases <- c("RCBD_two_locations", "latin_square", "full_factorial_rcbd",
             "split_plot_rcbd", "split_split_plot_rcbd", "strip_plot",
             "incomplete_blocks", "alpha_lattice", "square_lattice",
             "rectangular_lattice", "row_column", "diagonal_single",
             "optimized_arrangement", "RCBD_augmented", "partially_replicated",
             "multi_location_prep", "sparse_allocation")
  for (name in cases) {
    args <- catalogue_design(name)$metadata$parameters
    sites <- if (is.null(args[["l"]])) 1L else args[["l"]]
    invalid <- list(na = rep(NA_real_, sites), nan = rep(NaN, sites), inf = rep(Inf, sites),
                    minus_inf = rep(-Inf, sites), matrix = matrix(rep(101, sites)),
                    complex = rep(101 + 1i, sites), character = rep("101", sites),
                    logical = rep(TRUE, sites))
    ok <- lapply(invalid, function(bad) {
      args["plotNumber"] <- list(bad)
      set.seed(1941)
      before <- .Random.seed
      signals(suppressWarnings(do.call(catalogue[[name]]$fun, args))) &&
        identical(.Random.seed, before)
    })
    expect_case(ok, name)
  }
})

test_that("diagonal experiment-specific plot starts are validated before coercion", {
  args <- catalogue_design("diagonal_blocks_row")$metadata$parameters
  invalid <- list(na = NA_real_, nan = NaN, inf = Inf, minus_inf = -Inf, character = "101",
                  logical = TRUE, complex = 101 + 1i, matrix = matrix(101))
  ok <- lapply(invalid, function(bad) {
    values <- rep(bad[1L], length(args$blocks))
    if (is.matrix(bad)) dim(values) <- c(1L, length(values))
    signals(diagonal_arrangement(
      nrows = args$nrows, ncols = args$ncols, lines = args$lines,
      checks = args$checks, kindExpt = args$kindExpt, splitBy = args$splitBy,
      blocks = args$blocks, plotNumber = list(values), seed = args$seed,
      year = args$year))
  })
  expect_case(ok, "diagonal_blocks_row")
})

test_that("filler plots keep their label at the end of the planting path", {
  # 287 lines and 4 checks leave one plot of an 18 x 18 field empty: it is
  # the last plot the planter reaches, top row
  cases <- list(
    serpentine = list(planter = "serpentine", at = c(18L, 1L)),
    cartesian = list(planter = "cartesian", at = c(18L, 18L))
  )
  for (name in names(cases)) {
    book <- diagonal_arrangement(nrows = 18, ncols = 18, lines = 287, checks = 4, seed = 27,
                                 planter = cases[[name]]$planter)$fieldBook
    filler <- book[book$TREATMENT == "Filler", ]
    expect_equal(list(nrow(filler), filler$EXPT, filler$ENTRY, c(filler$ROW, filler$COLUMN)),
                 list(1L, "Filler", 0, cases[[name]]$at), info = name)
  }
})
