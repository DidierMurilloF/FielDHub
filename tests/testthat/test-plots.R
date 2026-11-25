library(FielDHub)

# Field maps drawn by plot(): what a reader of the map sees. Each replication
# (or group) has one background of its own, each check its own colour, filler
# plots are white, labels are the field book's values verbatim and dark
# enough to read, and a white grid separates the plots unless switched off.
# Exact palettes, line widths and layer order are styling, not checked here.

# One expectation for a case that checks several properties: it names the
# case and every property that fails.
expect_case <- function(ok, case) {
  bad <- names(ok)[!vapply(ok, isTRUE, logical(1))]
  expect(length(bad) == 0L, sprintf("case %s: %s", case, paste(bad, collapse = ", ")))
}

# A drawn map: the geoms of its layers and their built data. Building a
# ggplot is slow, so helpers accept a map built once.
map_of <- function(p) {
  if (!inherits(p, "ggplot")) return(p)
  list(geoms = vapply(p$layers, function(l) class(l$geom)[1], ""),
       data = ggplot2::ggplot_build(p)$data, title = p$labels$title, source = p$data)
}

# The layout view of a design with its maps built
built_view <- function(...) {
  view <- checked_layout_view(...)
  for (map in c("out_layout", "out_layoutPlots")) {
    if (!is.null(view[[map]])) view[[map]] <- map_of(view[[map]])
  }
  view
}

# The fill of the map: desplot's first tile layer
fill_data <- function(m) {
  m <- map_of(m)
  m$data[[which(m$geoms == "GeomTile")[1]]]
}

fill_at <- function(m, book) {
  tiles <- fill_data(m)
  tiles$fill[match(paste(book$COLUMN, book$ROW), paste(tiles$x, tiles$y))]
}

# The visible labels (desplot may add a text layer of size 0)
drawn_text <- function(m) {
  m <- map_of(m)
  Filter(function(d) max(as.numeric(d$size)) > 0, m$data[m$geoms == "GeomText"])
}

text_at <- function(m, book) {
  d <- drawn_text(m)
  d <- d[[length(d)]]
  key <- match(paste(book$COLUMN, book$ROW), paste(d$x, d$y))
  list(label = as.character(d$label[key]), colour = as.character(d$colour[key]))
}

same_colour <- function(a, b) identical(grDevices::col2rgb(a), grDevices::col2rgb(b))

# Unit edges "x1 y1 x2 y2" between the cells of a set of line segments
unit_edges <- function(s) {
  unlist(lapply(seq_len(nrow(s)), function(i) {
    if (s$y[i] == s$yend[i]) {
      x <- seq(min(s$x[i], s$xend[i]), max(s$x[i], s$xend[i]) - 1)
      sprintf("%g %g %g %g", x, s$y[i], x + 1, s$y[i])
    } else {
      y <- seq(min(s$y[i], s$yend[i]), max(s$y[i], s$yend[i]) - 1)
      sprintf("%g %g %g %g", s$x[i], y, s$x[i], y + 1)
    }
  }))
}

# Every edge the map draws in `colour`: line segments, and outlines of
# unfilled tiles
edges_drawn <- function(m, colour = "white") {
  m <- map_of(m)
  out <- character()
  for (i in seq_along(m$data)) {
    d <- m$data[[i]]
    if (!"colour" %in% names(d)) next
    d <- d[!is.na(d$colour) & d$colour == colour, , drop = FALSE]
    if (!nrow(d)) next
    if (m$geoms[i] == "GeomSegment") out <- c(out, unit_edges(d))
    if (m$geoms[i] == "GeomTile" && all(is.na(d$fill))) {
      out <- c(out,
               sprintf("%g %g %g %g", d$x - 0.5, d$y - 0.5, d$x + 0.5, d$y - 0.5),
               sprintf("%g %g %g %g", d$x - 0.5, d$y + 0.5, d$x + 0.5, d$y + 0.5),
               sprintf("%g %g %g %g", d$x - 0.5, d$y - 0.5, d$x - 0.5, d$y + 0.5),
               sprintf("%g %g %g %g", d$x + 0.5, d$y - 0.5, d$x + 0.5, d$y + 0.5))
    }
  }
  unique(out)
}

# Edges between neighbouring drawn plots
interior_edges <- function(m) {
  tiles <- fill_data(m)
  key <- paste(tiles$x, tiles$y)
  right <- paste(tiles$x + 1, tiles$y) %in% key
  up <- paste(tiles$x, tiles$y + 1) %in% key
  c(sprintf("%g %g %g %g", tiles$x + 0.5, tiles$y - 0.5, tiles$x + 0.5, tiles$y + 0.5)[right],
    sprintf("%g %g %g %g", tiles$x - 0.5, tiles$y + 0.5, tiles$x + 0.5, tiles$y + 0.5)[up])
}

# Whether a white line separates every pair of neighbouring plots
draws_plot_grid <- function(m) {
  m <- map_of(m)
  all(interior_edges(m) %in% edges_drawn(m, "white"))
}

# One distinct colour per group: every group has a single fill, and no two
# groups share one
one_fill_per_group <- function(fill, group) {
  by_group <- lapply(split(fill, group), unique)
  all(lengths(by_group) == 1L) && !anyDuplicated(unlist(by_group))
}

# WCAG relative luminance and contrast ratio
relative_luminance <- function(col) {
  rgb <- grDevices::col2rgb(col) / 255
  rgb <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  colSums(rgb * c(0.2126, 0.7152, 0.0722))
}
contrast_ratio <- function(a, b) {
  la <- relative_luminance(a)
  lb <- relative_luminance(b)
  (pmax(la, lb) + 0.05) / (pmin(la, lb) + 0.05)
}
lab <- function(colours) {
  grDevices::convertColor(t(grDevices::col2rgb(colours) / 255), from = "sRGB", to = "Lab")
}

# Palettes --------------------------------------------------------------------

test_that("replication tints are distinct, one per replication", {
  # Up to six replications keep their tints as replications are added; the
  # first is always the neutral background of unreplicated maps
  for (n in c(0, 2, 6, 9)) {
    tints <- fieldhub_field_tints(n)
    expect_true(is.character(tints) && length(tints) == n && !anyDuplicated(tints) &&
                  (n > 6 || identical(tints, fieldhub_field_tints(6)[seq_len(n)])) &&
                  (n == 0 || identical(tints[1], fieldhub_field_tints(1))),
                info = paste(n, "replications"))
  }
})

test_that("consecutive replications never get near-identical tints", {
  # The tints are deliberately subtle; the white alleys separate replications,
  # but neighbouring replications must still differ visibly (CIE76 delta E of
  # at least 4, above the just-noticeable difference of about 2.3)
  expect_gte(min(sqrt(rowSums(diff(lab(fieldhub_field_tints(6)))^2))), 4)
})

test_that("dark labels keep AAA contrast on every replication background", {
  # WCAG AAA asks for a contrast ratio of at least 7 for normal text
  ratios <- contrast_ratio("gray10", fieldhub_field_tints(12))
  expect(all(ratios >= 7), sprintf("tint %d has contrast %.2f", which.min(ratios), min(ratios)))
})

test_that("check colours are distinct, readable and visibly different", {
  ratios <- contrast_ratio("gray10", fieldhub_check_palette(12))
  expect_case(list(
    "twelve distinct colours" = length(unique(fieldhub_check_palette(12))) == 12L,
    "none for no checks" = identical(fieldhub_check_palette(0), character()),
    "AAA contrast under dark labels" = all(ratios >= 7),
    "delta E of at least 4" = min(stats::dist(lab(fieldhub_check_palette(8)))) >= 4
  ), "fieldhub_check_palette()")
})

test_that("the check fill is a neutral grey that dark labels read on", {
  rgb <- grDevices::col2rgb(fieldhub_check_fill())
  expect_case(list(
    # channels within 12 of 255 of each other: no visible hue
    "neutral" = max(rgb) - min(rgb) <= 12,
    "AAA contrast" = contrast_ratio("gray10", fieldhub_check_fill()) >= 7,
    "not a replication tint" = !fieldhub_check_fill() %in% fieldhub_field_tints(12)
  ), "fieldhub_check_fill()")
})

test_that("the replicated-plot fill is readable under dark labels and distinct", {
  expect_case(list(
    "AAA contrast" = contrast_ratio("gray10", fieldhub_replicated_fill()) >= 7,
    "distinct" = !fieldhub_replicated_fill() %in% c(fieldhub_check_fill(), fieldhub_field_tints(6))
  ), "fieldhub_replicated_fill()")
})

# Incomplete-block maps -------------------------------------------------------

iblock_designs <- c("alpha_lattice", "alpha_lattice_six_reps", "incomplete_blocks",
                    "incomplete_blocks_labels", "square_lattice", "rectangular_lattice")

test_that("every replication has one background in any drawn layout", {
  skip_on_cran()
  for (name in iblock_designs) {
    design <- catalogue_design(name)
    failures <- character()
    for (stacked in c("vertical", "horizontal", "grid_panel")) {
      for (planter in c("serpentine", "cartesian")) {
        options <- tryCatch(layout_options(design, planter = planter, stacked = stacked),
                            fieldhub_input_error = function(e) NULL)
        for (site in options) for (book in site) {
          p <- plot_field_alleys(book, "ENTRY", "map", group = "REP", block = "IBLOCK")
          tiles <- fill_data(p)
          cell <- match(paste(tiles$x, tiles$y), paste(book$COLUMN, book$ROW))
          if (!one_fill_per_group(tiles$fill, book$REP[cell])) {
            failures <- c(failures, paste(stacked, planter))
          }
        }
      }
    }
    # The plot-number map draws the same backgrounds as the entry map
    view <- built_view(design)
    if (!identical(fill_data(view$out_layoutPlots)$fill, fill_data(view$out_layout)$fill)) {
      failures <- c(failures, "plot-number map has other backgrounds")
    }
    expect(length(failures) == 0L, sprintf("case %s: %s", name, paste(unique(failures), collapse = "; ")))
  }
})

test_that("the lattice plot grid follows flips and can be switched off", {
  design <- catalogue_design("alpha_lattice")
  flipped <- built_view(design, flip = TRUE)
  plain <- built_view(design, field_grid = FALSE)
  expect_case(list(
    "flipped maps keep the grid" = draws_plot_grid(flipped$out_layout) &&
      draws_plot_grid(flipped$out_layoutPlots),
    "field_grid = FALSE removes it" = !draws_plot_grid(plain$out_layout) &&
      !draws_plot_grid(plain$out_layoutPlots)
  ), "alpha_lattice")
})

test_that("incomplete-block maps keep entry and plot-number labels", {
  for (name in iblock_designs) {
    view <- built_view(catalogue_design(name))
    book <- view$fieldBookXY
    entries <- text_at(view$out_layout, book)
    expect_case(list(
      "entries" = identical(entries$label, as.character(book$ENTRY)),
      "dark labels" = same_colour(unique(entries$colour), "gray10"),
      "plot numbers" = identical(text_at(view$out_layoutPlots, book)$label, as.character(book$PLOT)),
      "plot grid" = draws_plot_grid(view$out_layout)
    ), name)
  }
})

test_that("plot() arguments override the map styling", {
  design <- catalogue_design("alpha_lattice")
  view <- built_view(design, main = "CUSTOM", col.regions = rep("white", 8))
  expect_identical(list(view$out_layout$title, unique(fill_data(view$out_layout)$fill)),
                   list("CUSTOM", "white"))
  augmented <- built_view(RCBD_augmented(lines = 50, checks = 3, b = 6, seed = 23),
                          flip = TRUE, main = "CUSTOM")$out_layout
  expect_identical(augmented$title, "CUSTOM")
})

test_that("a short user palette is recycled over the map's fill levels", {
  map <- built_view(catalogue_design("partially_replicated_fillers"),
                             col.regions = c("white", "navy"))$out_layout
  expect_true(all(fill_data(map)$fill %in% c("white", "navy")))
})

# Augmented designs -------------------------------------------------------

test_that("augmented RCBD maps tint blocks and colour each check", {
  designs <- list(
    single = RCBD_augmented(lines = 50, checks = 3, b = 6, seed = 23),
    two_experiments = RCBD_augmented(lines = 40, checks = 3, b = 3, seed = 5, repsExpt = 2)
  )
  for (name in names(designs)) {
    view <- built_view(designs[[name]])
    book <- view$fieldBookXY
    is_check <- !is.na(book$CHECKS) & book$CHECKS == "1"
    filler <- is.na(book$CHECKS)
    plain <- !is_check & !filler
    entries <- fill_at(view$out_layout, book)
    numbers <- fill_at(view$out_layoutPlots, book)
    text <- text_at(view$out_layout, book)
    expect_case(list(
      "has checks" = any(is_check),
      # each check in its own colour in the entry map; none in the plot-number map
      "checks coloured" = identical(entries[is_check], expected_check_fills(book$ENTRY[is_check])) &&
        length(unique(entries[is_check])) == 3L,
      "plot-number map has no check colours" = !any(numbers %in% fieldhub_check_palette(3)),
      "fillers white" = all(c(entries[filler], numbers[filler]) == "#FFFFFF"),
      # every other plot has its block's tint, a different one per block
      "one tint per block" = one_fill_per_group(entries[plain], paste(book$EXPT, book$BLOCK)[plain]) &&
        identical(numbers[plain], entries[plain]),
      "entry labels" = identical(text$label, as.character(book$ENTRY)) &&
        all(text$colour[!filler] == "gray10") && all(text$colour[filler] == "gray60"),
      "plot labels" = identical(text_at(view$out_layoutPlots, book)$label,
                                sprintf("%d", as.integer(book$PLOT))),
      "plot grid" = draws_plot_grid(view$out_layout) && draws_plot_grid(view$out_layoutPlots)
    ), name)
  }
})

test_that("augmented RCBD colours only check cells without changing block backgrounds", {
  design <- RCBD_augmented(lines = 110, checks = 4, b = 6, l = 2, seed = 1)
  before <- serialize(design, NULL)
  for (location in 1:2) for (flip in c(FALSE, TRUE)) {
    view <- built_view(design, location = location, flip = flip)
    e <- fill_data(view$out_layout)
    n <- fill_data(view$out_layoutPlots)
    is_check <- view$out_layout$source$CHECK_TEXT == "check"
    expect_case(list(
      "checks coloured" = identical(e$fill[is_check],
                                    expected_check_fills(view$out_layout$source$ENTRY[is_check])),
      # outside the checks, both maps draw the same block backgrounds
      "same block backgrounds" = identical(e$fill[!is_check], n$fill[!is_check]),
      "plot-number map has no check colours" = !any(n$fill %in% fieldhub_check_palette(4))
    ), paste("location", location, if (flip) "flipped"))
  }
  expect_identical(serialize(design, NULL), before)
})

test_that("augmented row-column maps label entries and colour each check", {
  view <- built_view(catalogue_design("augmented_row_column"))
  book <- view$fieldBookXY
  is_check <- !is.na(book$CHECKS) & book$CHECKS != 0
  text <- text_at(view$out_layout, book)
  entries <- fill_at(view$out_layout, book)
  numbers <- fill_at(view$out_layoutPlots, book)
  expect_case(list(
    # a white plot grid: every interior edge drawn
    "plot grid" = draws_plot_grid(view$out_layout) && draws_plot_grid(view$out_layoutPlots),
    # entry numbers, not treatment names
    "entry labels" = identical(text$label, as.character(book$ENTRY)),
    "checks coloured" = identical(entries[is_check], expected_check_fills(book$CHECKS[is_check])) &&
      length(unique(entries[is_check])) == 4L,
    "entries on the neutral" = all(entries[!is_check] == fieldhub_field_tints(1)) &&
      all(numbers == fieldhub_field_tints(1)),
    "plot labels" = identical(text_at(view$out_layoutPlots, book)$label,
                              sprintf("%d", as.integer(book$PLOT)))
  ), "augmented_row_column")
})

test_that("augmented RCBD layout draws entries natively with dark labels", {
  a <- RCBD_augmented(lines = 110, checks = 4, b = 6, l = 1, plotNumber = 101, seed = 1)
  p <- plot_layout(a, l = 1)
  d1 <- drawn_text(p$out_layout)[[1]]
  d2 <- drawn_text(p$out_layoutPlots)[[1]]
  expect_case(list(
    "dark labels, grey fillers" = all(as.character(d1$colour) %in% c("gray10", "gray60")) &&
      "gray10" %in% d1$colour,
    "plot numbers drawn alike" = identical(sort(as.character(d2$colour)), sort(as.character(d1$colour))) &&
      identical(unique(as.numeric(d1$size)), unique(as.numeric(d2$size)))
  ), "RCBD_augmented")
})

# Complete-block and other designs -------------------------------------------

# Designs in complete blocks or replications without incomplete blocks: one
# tint per replication, each check its own colour, a white grid around every
# plot (the strips of a strip-plot are its rows and columns).
complete_designs <- list(
  RCBD_two_locations = "TREATMENT", RCBD_checks = "TREATMENT", RCBD_cartesian = "TREATMENT",
  row_column = "ENTRY", row_column_twostage = "ENTRY",
  strip_plot = "TRT_COMB", strip_plot_labels = "TRT_COMB"
)

test_that("RCBD, row-column and strip-plot maps tint reps, colour checks and draw a plot grid", {
  for (name in names(complete_designs)) {
    design <- catalogue_design(name)
    for (location in seq_along(unique(design$fieldBook$LOCATION))) {
      view <- built_view(design, location = location)
      book <- view$fieldBookXY
      is_check <- if ("CHECKS" %in% names(book)) !is.na(book$CHECKS) & book$CHECKS != 0 else
        rep(FALSE, nrow(book))
      entries <- fill_at(view$out_layout, book)
      numbers <- fill_at(view$out_layoutPlots, book)
      text <- text_at(view$out_layout, book)
      expect_case(list(
        "one tint per replication" = one_fill_per_group(entries[!is_check], book$REP[!is_check]) &&
          identical(numbers[!is_check], entries[!is_check]),
        # each check in its own colour, in the entry map only
        "checks coloured" = identical(entries[is_check], expected_check_fills(book$CHECKS[is_check])) &&
          one_fill_per_group(entries[is_check], book$CHECKS[is_check]) &&
          !any(numbers %in% fieldhub_check_palette(8)),
        "entry labels" = identical(text$label, as.character(book[[complete_designs[[name]]]])) &&
          all(text$colour == "gray10"),
        "plot labels" = identical(text_at(view$out_layoutPlots, book)$label, as.character(book$PLOT)),
        "plot grid" = draws_plot_grid(view$out_layout) && draws_plot_grid(view$out_layoutPlots)
      ), paste(name, "location", location))
    }
  }
})

test_that("the complete-block plot grid can be switched off", {
  plain <- built_view(catalogue_design("RCBD_checks"), field_grid = FALSE)
  expect_false(draws_plot_grid(plain$out_layout) || draws_plot_grid(plain$out_layoutPlots))
})

# CRD, Latin square and rectangle, factorial, split-plot and split-split-plot:
# groups (reps or squares) get one tint each; labels keep their columns.
other_designs <- list(
  CRD_count = list(text = "TREATMENT"), CRD_labels = list(text = "TREATMENT"),
  CRD_data = list(text = "TREATMENT"),
  latin_square = list(text = "TREATMENT", group = "SQUARE"),
  latin_square_cartesian = list(text = "TREATMENT"),
  latin_rectangle = list(text = "TREATMENT"),
  full_factorial_rcbd = list(text = "TRT_COMB", group = "REP"),
  full_factorial_crd_data = list(text = "TRT_COMB"),
  split_plot_rcbd = list(text = "TRT_COMB", group = "REP"),
  split_plot_crd = list(text = "TRT_COMB"),
  split_split_plot_rcbd = list(text = "TRT_COMB", group = "REP"),
  split_split_plot_crd = list(text = "TRT_COMB")
)

test_that("the other designs' maps tint each group and keep their labels", {
  for (name in names(other_designs)) {
    spec <- other_designs[[name]]
    design <- catalogue_design(name)
    for (location in seq_along(unique(design$fieldBook$LOCATION))) {
      view <- built_view(design, location = location)
      book <- view$fieldBookXY
      grouped <- !is.null(spec$group) && length(unique(book[[spec$group]])) > 1L
      entries <- fill_at(view$out_layout, book)
      numbers <- fill_at(view$out_layoutPlots, book)
      text <- text_at(view$out_layout, book)
      expect_case(list(
        "same backgrounds in both maps" = identical(numbers, entries),
        "one tint per group" = if (grouped) one_fill_per_group(entries, book[[spec$group]]) else
          all(entries == fieldhub_field_tints(1)),
        "entry labels" = identical(text$label, as.character(book[[spec$text]])) &&
          all(text$colour == "gray10"),
        "plot labels" = identical(text_at(view$out_layoutPlots, book)$label, as.character(book$PLOT)),
        "plot grid" = draws_plot_grid(view$out_layout)
      ), paste(name, "location", location))
    }
  }
})

test_that("split-split-plot sub-plot lines separate sub-plots inside a whole plot only", {
  view <- built_view(catalogue_design("split_split_plot_rcbd"))
  book <- view$fieldBookXY
  m <- view$out_layout
  # the sub-plot lines: the map's lighter grey lines
  seg <- do.call(rbind, lapply(m$data[m$geoms == "GeomSegment"], function(d) {
    d[d$colour == "gray70", c("x", "y", "xend", "yend")]
  }))
  vertical <- seg$x == seg$xend
  # the two plots on either side of each line
  a <- ifelse(vertical, paste(seg$x - 0.5, (seg$y + seg$yend) / 2), paste((seg$x + seg$xend) / 2, seg$y - 0.5))
  b <- ifelse(vertical, paste(seg$x + 0.5, (seg$y + seg$yend) / 2), paste((seg$x + seg$xend) / 2, seg$y + 0.5))
  key <- paste(book$COLUMN, book$ROW)
  ia <- match(a, key)
  ib <- match(b, key)
  # and every such pair of neighbours gets a line
  right <- match(paste(book$COLUMN + 1, book$ROW), key)
  up <- match(paste(book$COLUMN, book$ROW + 1), key)
  pairs <- data.frame(from = rep(seq_len(nrow(book)), 2), to = c(right, up))
  pairs <- pairs[!is.na(pairs$to), ]
  pairs <- pairs[book$PLOT[pairs$from] == book$PLOT[pairs$to] &
                   book$SUB_PLOT[pairs$from] != book$SUB_PLOT[pairs$to], ]
  expect_case(list(
    "each line between two plots" = !anyNA(c(ia, ib)),
    "inside a whole plot, between sub-plots" = !anyNA(c(ia, ib)) &&
      all(book$PLOT[ia] == book$PLOT[ib]) && all(book$SUB_PLOT[ia] != book$SUB_PLOT[ib]),
    "every such pair" = nrow(seg) == nrow(pairs)
  ), "split_split_plot_rcbd")
})

# p-rep and unreplicated check maps -------------------------------------------

prep_designs <- c("partially_replicated", "partially_replicated_fillers", "multi_location_prep")

test_that("p-rep maps show replicated plots in their own fill on a plot grid", {
  for (name in prep_designs) {
    design <- catalogue_design(name)
    for (location in seq_along(location_books(design))) {
      entries_view <- built_view(design, location = location)
      numbers_view <- built_view(design, location = location, text.string = "PLOT")
      book <- entries_view$fieldBookXY
      filler <- book$TREATMENT == "Filler"
      replicated <- book$CHECKS != 0 & !filler
      entries <- fill_at(entries_view$out_layout, book)
      numbers <- fill_at(numbers_view$out_layout, book)
      text <- text_at(entries_view$out_layout, book)
      expect_case(list(
        "has replicated plots" = any(replicated),
        "replicated plots highlighted" = all(entries[replicated] == fieldhub_replicated_fill()),
        "other plots on the neutral" = all(entries[!replicated & !filler] == fieldhub_field_tints(1)) &&
          all(numbers[!filler] == fieldhub_field_tints(1)),
        "fillers white" = all(c(entries[filler], numbers[filler]) == "#FFFFFF"),
        "entry labels" = identical(text$label, as.character(book$ENTRY)) &&
          all(text$colour[!filler] == "gray10") && all(text$colour[filler] == "gray60"),
        "plot labels" = identical(text_at(numbers_view$out_layout, book)$label, as.character(book$PLOT)),
        "one map per request" = is.null(entries_view$out_layoutPlots),
        "plot grid" = draws_plot_grid(entries_view$out_layout) && draws_plot_grid(numbers_view$out_layout)
      ), paste(name, "location", location))
    }
  }
})

check_designs <- c("diagonal_single", "diagonal_single_cartesian", "diagonal_blocks_row",
                   "diagonal_blocks_column_same", "optimized_arrangement", "sparse_allocation")

test_that("check maps colour each check on a plot grid", {
  for (name in check_designs) {
    design <- catalogue_design(name)
    for (location in seq_along(location_books(design))) {
      view <- built_view(design, location = location)
      numbers <- built_view(design, location = location, text.string = "PLOT")$out_layout
      book <- view$fieldBookXY
      filler <- book$TREATMENT == "Filler"
      is_check <- !is.na(book$CHECKS) & as.character(book$CHECKS) != "0" & !filler
      entries <- fill_at(view$out_layout, book)
      plain <- fill_at(numbers, book)
      text <- text_at(view$out_layout, book)
      expect_case(list(
        "checks coloured" = identical(entries[is_check], expected_check_fills(book$CHECKS[is_check])) &&
          one_fill_per_group(entries[is_check], book$CHECKS[is_check]),
        "tests on the neutral" = all(entries[!is_check & !filler] == fieldhub_field_tints(1)) &&
          all(plain[!filler] == fieldhub_field_tints(1)),
        "fillers white" = all(entries[filler] == "#FFFFFF"),
        "entry labels" = identical(text$label, as.character(book$ENTRY)) &&
          all(text$colour[!filler] == "gray10"),
        "plot labels" = identical(text_at(numbers, book)$label, as.character(book$PLOT)),
        "one map per request" = is.null(view$out_layoutPlots),
        "plot grid" = draws_plot_grid(view$out_layout) && draws_plot_grid(numbers)
      ), paste(name, "location", location))
    }
  }
})

test_that("the multiple-diagonal experiment map tints and labels each experiment", {
  for (name in c("diagonal_blocks_row", "diagonal_blocks_column_same")) {
    design <- catalogue_design(name)
    map <- built_view(design, text.string = "EXPT")$out_layout
    book <- built_view(design)$fieldBookXY
    expect_case(list(
      "one tint per experiment" = one_fill_per_group(fill_at(map, book), book$EXPT),
      "experiment labels" = identical(text_at(map, book)$label, as.character(book$EXPT))
    ), name)
  }
})

test_that("multiple-diagonal maps colour experiments only in the experiment map", {
  design <- catalogue_design("diagonal_blocks_row")
  book <- design$fieldBook
  for (label in c("PLOT", "EXPT")) {
    map <- built_view(design, text.string = label)$out_layout
    text <- drawn_text(map)[[1L]]
    tile <- fill_data(map)
    experiment <- book$EXPT[match(paste(tile$x, tile$y), paste(book$COLUMN, book$ROW))]
    expect_case(list(
      "labels" = identical(as.character(text$label), as.character(book[[label]])),
      "one label colour" = length(unique(text$colour)) == 1L,
      # plot numbers: experiments are outlined, not coloured
      "fills" = if (label == "EXPT") one_fill_per_group(tile$fill, experiment) else
        identical(unique(tile$fill), fieldhub_field_tints(1))
    ), label)
  }
})

test_that("unreplicated entry maps fill check cells and keep test lines neutral", {
  designs <- c(
    lapply(c(diagonal_single = "diagonal_single", diagonal_blocks_row = "diagonal_blocks_row",
             sparse_allocation = "sparse_allocation"), catalogue_design),
    list(diagonal_fillers = diagonal_arrangement(nrows = 18, ncols = 18, lines = 287, checks = 4, seed = 27),
         optimized = optimized_arrangement(nrows = 12, ncols = 10, lines = 100, checks = 4,
                                           rep_checks = c(5, 5, 5, 5), seed = 27)))
  for (name in names(designs)) {
    design <- designs[[name]]
    before <- serialize(design, NULL)
    ok <- list()
    for (location in seq_along(location_books(design))) {
      book <- location_books(design)[[location]]
      view <- built_view(design, location = location)
      tile <- fill_data(view$out_layout)
      cells <- match(paste(tile$x, tile$y), paste(book$COLUMN, book$ROW))
      checks <- as.character(book$CHECKS[cells])
      filler <- book$TREATMENT[cells] == "Filler"
      check <- checks != "0" & !filler
      text <- drawn_text(view$out_layout)[[1L]]
      ok[[paste("location", location)]] <- !anyNA(cells) && any(checks == "0") && any(check) &&
        # each check has its own muted colour; test lines stay on the neutral
        one_fill_per_group(tile$fill[check], checks[check]) &&
        all(tile$fill[check] %in% fieldhub_check_palette(length(unique(checks[check])))) &&
        all(tile$fill[checks == "0" & !filler] == fieldhub_field_tints(1)) &&
        length(unique(text$colour[text$label != "Filler"])) == 1L &&
        identical(as.character(view$fieldBookXY$CHECKS), as.character(book$CHECKS))
    }
    ok[["design unchanged"]] <- identical(serialize(design, NULL), before)
    expect_case(ok, name)
  }
})

test_that("plot-number maps do not inherit replication or check colours", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  designs <- c(
    lapply(c(partially_replicated_fillers = "partially_replicated_fillers",
             multi_location_prep = "multi_location_prep", optimized_arrangement = "optimized_arrangement",
             diagonal_single = "diagonal_single", sparse_allocation = "sparse_allocation"),
           catalogue_design),
    list(diagonal_fillers = diagonal_arrangement(nrows = 18, ncols = 18, lines = 287, checks = 4, seed = 27)))
  for (name in names(designs)) {
    design <- designs[[name]]
    prep <- name %in% c("partially_replicated_fillers", "multi_location_prep")
    ok <- list()
    if (name == "diagonal_fillers") {
      labels <- drawn_text(built_view(design)$out_layout)[[1L]]$label
      ok[["fillers labelled Filler"]] <- "Filler" %in% labels && !"0" %in% labels
    }
    for (location in seq_along(location_books(design))) {
      book <- location_books(design)[[location]]
      numbers <- map_of(plot(design, l = location, text.string = "PLOT")$p)
      filler <- book$TREATMENT == "Filler"
      fill <- fill_at(numbers, book)
      text <- text_at(numbers, book)
      ok[[paste("location", location)]] <-
        identical(as.character(drawn_text(numbers)[[1L]]$label), as.character(book$PLOT)) &&
        # plots on the neutral, filler plots white
        all(fill[!filler] == fieldhub_field_tints(1)) && all(fill[filler] == "#FFFFFF") &&
        # p-rep entry maps highlight replicated plots; plot numbers in one dark colour
        if (prep) fieldhub_replicated_fill() %in% fill_data(plot(design, l = location)$p)$fill else
          length(unique(text$colour[!filler])) == 1L
    }
    expect_case(ok, name)
  }
})

test_that("custom palettes remain caller-controlled", {
  book <- data.frame(ROW = c(1, 1), COLUMN = c(1, 2), ENTRY = c("A", "B"))
  custom <- plot_desplot(ENTRY ~ COLUMN + ROW, book,
                         extra_args = list(col.regions = c("white", "navy")))
  expect_setequal(fill_data(custom)$fill, c("white", "navy"))
  for (name in c("diagonal_single", "diagonal_blocks_row", "sparse_allocation",
                 "optimized_arrangement", "partially_replicated_fillers", "multi_location_prep")) {
    design <- catalogue_design(name)
    ok <- lapply(c(ENTRY = "ENTRY", PLOT = "PLOT"), function(label) {
      map <- built_view(design, text.string = label, col.regions = c("white", "white"))$out_layout
      all(vapply(unique(fill_data(map)$fill), same_colour, logical(1), b = "white"))
    })
    expect_case(ok, name)
  }
})

test_that("optimized maps draw a white grid around every plot instead of check borders", {
  design <- optimized_arrangement(nrows = 12, ncols = 10, lines = 100, checks = 4,
    rep_checks = c(5, 5, 5, 5), l = 2, seed = 27)
  before <- serialize(design, NULL)
  for (location in 1:2) for (flip in c(FALSE, TRUE)) {
    entries <- built_view(design, location = location, flip = flip)$out_layout
    numbers <- built_view(design, location = location, flip = flip, text.string = "PLOT")$out_layout
    plain <- built_view(design, location = location, flip = flip, field_grid = FALSE)$out_layout
    expect_case(list(
      # every internal edge between two plots is drawn in white
      "plot grid" = draws_plot_grid(entries) && draws_plot_grid(numbers),
      # the grid is the only addition: switching it off keeps the same fills and labels
      "field_grid = FALSE" = !draws_plot_grid(plain) &&
        identical(sort(unique(fill_data(entries)$fill)), sort(unique(fill_data(plain)$fill))) &&
        isTRUE(all.equal(drawn_text(entries), drawn_text(plain))),
      "plot numbers on the neutral" = identical(unique(fill_data(numbers)$fill), fieldhub_field_tints(1))
    ), paste("location", location, if (flip) "flipped"))
  }
  # out1.gpar restyles the grid as the caller asks
  custom <- built_view(design, out1.gpar = list(col = "navy", lwd = 2, lty = 2))$out_layout
  navy <- do.call(rbind, lapply(custom$data[custom$geoms == "GeomSegment"], function(d) {
    d[d$colour == "navy", c("linewidth", "linetype")]
  }))
  expect_true(all(interior_edges(custom) %in% edges_drawn(custom, "navy")) &&
                all(navy$linewidth == 2 & navy$linetype == 2))
  expect_identical(serialize(design, NULL), before)
})

# Labels drawn verbatim ------------------------------------------------------

# desplot's default shorten = "abb" runs abbreviate() over the labels and
# turns three-digit entry numbers into two-digit ones (100 -> "00"): a
# different, equally plausible entry number.
test_that("maps draw entry and plot numbers verbatim", {
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  diagonal <- diagonal_arrangement(nrows = 15, ncols = 10, lines = 120, checks = 4,
                                   plotNumber = 101, seed = 1)
  prep <- partially_replicated(nrows = 14, ncols = 10, repGens = c(20, 100), repUnits = c(2, 1),
                               plotNumber = 101, seed = 1)
  optimized <- optimized_arrangement(nrows = 12, ncols = 10, lines = 110, rep_checks = 10,
                                     checks = 1, plotNumber = 101, seed = 1)
  augmented <- RCBD_augmented(lines = 110, checks = 4, b = 6, l = 1, plotNumber = 101, seed = 1)
  augmented_maps <- plot_layout(augmented, l = 1)
  cases <- list(
    diagonal_plot = list(map = plot(diagonal)$p, labels = as.numeric(diagonal$fieldBook$ENTRY)),
    prep = list(map = plot_layout(prep, l = 1)$out_layout, labels = prep$fieldBook$ENTRY),
    optimized = list(map = plot_layout(optimized, l = 1)$out_layout, labels = optimized$fieldBook$ENTRY),
    augmented_entries = list(map = augmented_maps$out_layout, labels = augmented$fieldBook$ENTRY),
    augmented_plots = list(map = augmented_maps$out_layoutPlots,
                           labels = sprintf("%d", as.integer(augmented$fieldBook$PLOT)))
  )
  for (name in names(cases)) {
    expected <- as.character(cases[[name]]$labels)
    layers <- drawn_text(cases[[name]]$map)
    u <- unique(expected)
    expect_case(list(
      # Guard against a vacuous case: abbreviation would change these labels
      "abbreviation would bite" = !all(unname(abbreviate(u, 2, method = "both")) == u),
      "one text layer" = length(layers) == 1L,
      "verbatim" = length(layers) == 1L &&
        identical(sort(as.character(layers[[1]]$label)), sort(expected))
    ), name)
  }
})

test_that("partially replicated design labels filler plots", {
  p_rep <- partially_replicated(nrows = 6, ncols = 7, repGens = c(5, 31), repUnits = c(2, 1),
                                plotNumber = 101, seed = 1, allow_fillers = TRUE)
  labels <- as.character(drawn_text(plot_layout(p_rep, l = 1)$out_layout)[[1]]$label)
  expect_true(sum(labels == "Filler") == 1 && !"0" %in% labels)
})

# plot() and desplot ----------------------------------------------------------

skip_if_layout_refactor_unavailable <- function() {
  skip_if_not_installed("desplot")
  if (utils::packageVersion("desplot") < "1.11") {
    skip("plot layout refactor requires desplot >= 1.11")
  }
}

test_that("plot() forwards extra arguments to desplot", {
  skip_if_layout_refactor_unavailable()
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  cases <- list(
    rcbd = RCBD(t = 12, reps = 3, plotNumber = 101, seed = 1),
    prep = partially_replicated(nrows = 8, ncols = 5, repGens = c(10, 20), repUnits = c(2, 1),
                                seed = 65784)
  )
  for (name in names(cases)) {
    # extra arguments win over plot()'s own xlab = "COLUMNS" default
    p <- plot(cases[[name]], xlab = "CUSTOM_X")$p
    expect_true(inherits(p, "ggplot") && identical(p$labels$x, "CUSTOM_X"), info = name)
  }
})

test_that("drawing every catalogue design is silent", {
  skip_on_cran()
  skip_if_layout_refactor_unavailable()
  for (name in setdiff(names(catalogue), "split_families")) {
    design <- catalogue_design(name)
    if (!inherits(design, "FielDHub")) next
    layout <- plot_layout(design)
    expect_silent(for (p in Filter(function(z) inherits(z, "ggplot"), layout)) ggplot2::ggplot_build(p))
  }
})
