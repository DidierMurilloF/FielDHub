library(FielDHub)

# Field-alleys maps of the incomplete-block family (alpha, square and
# rectangular lattices, incomplete blocks): one desplot call that gives each
# replication a single pastel background, separates replications with white
# alleys and outlines the incomplete blocks in grey. Blocks never get a
# background of their own, so no colour repeats inside a replication.

iblock_designs <- c("alpha_lattice", "alpha_lattice_six_reps", "incomplete_blocks",
                    "incomplete_blocks_labels", "square_lattice", "rectangular_lattice")

layer_geoms <- function(p) vapply(p$layers, function(l) class(l$geom)[1], "")

fill_data <- function(p) ggplot2::layer_data(p, which(layer_geoms(p) == "GeomTile")[1])

outline_colour <- function(p, colour) {
  any(vapply(p$layers, function(l) {
    inherits(l$geom, "GeomSegment") && identical(l$aes_params$colour, colour)
  }, TRUE))
}

# Every drawn layout of a design: each stacking, planter, location and option
each_drawn_layout <- function(design, fun) {
  for (stacked in c("vertical", "horizontal", "grid_panel")) {
    for (planter in c("serpentine", "cartesian")) {
      options <- tryCatch(layout_options(design, planter = planter, stacked = stacked),
                          error = function(e) NULL)
      for (site in options) for (book in site) fun(book)
    }
  }
}

test_that("fieldhub_field_tints() returns the muted replication palette", {
  tints <- fieldhub_field_tints(6)
  expect_identical(tints, c("#F2F2F2", "#E6EEF5", "#E9F2EC", "#F3EEE6", "#EDE7F2", "#F1E9E9"))
  expect_identical(fieldhub_field_tints(2), tints[1:2])
  expect_length(fieldhub_field_tints(9), 9L)
  expect_length(unique(fieldhub_field_tints(9)), 9L)
  expect_identical(fieldhub_field_tints(9)[1], "#F2F2F2")
  expect_identical(fieldhub_field_tints(0), character())
})

test_that("consecutive replications never get near-identical tints", {
  # The tints are deliberately subtle; the white alleys separate replications,
  # but neighbouring replications must still differ visibly.
  lab <- grDevices::convertColor(t(grDevices::col2rgb(fieldhub_field_tints(6)) / 255),
                                 from = "sRGB", to = "Lab")
  expect_gte(min(sqrt(rowSums(diff(lab)^2))), 4)
})

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

test_that("dark labels keep AAA contrast on every replication background", {
  for (reps in 1:12) {
    expect_true(all(contrast_ratio("gray10", fieldhub_field_tints(reps)) >= 7),
                info = paste(reps, "reps"))
  }
})

test_that("every replication has one background in any drawn layout", {
  for (name in iblock_designs) {
    design <- catalogue_design(name)
    each_drawn_layout(design, function(book) {
      p <- plot_field_alleys(book, "ENTRY", "map", group = "REP", block = "IBLOCK")
      tiles <- fill_data(p)
      cell <- match(paste(tiles$x, tiles$y), paste(book$COLUMN, book$ROW))
      by_rep <- lapply(split(tiles$fill, book$REP[cell]), unique)
      # one colour per replication, shared by all of its incomplete blocks
      expect_true(all(lengths(by_rep) == 1L), info = name)
      # each replication has its own colour
      expect_length(unique(unlist(by_rep)), length(by_rep))
    })
  }
})

test_that("incomplete-block maps are desplot plus one white plot-grid layer", {
  for (name in iblock_designs) {
    view <- checked_layout_view(catalogue_design(name))
    for (p in list(view$out_layout, view$out_layoutPlots)) {
      geoms <- layer_geoms(p)
      # desplot's fill, then the plot grid right above it, below outlines and text
      expect_identical(unname(which(geoms == "GeomTile")), 1:2, info = name)
      grid <- p$layers[[2]]
      expect_true(is.na(grid$aes_params$fill), info = name)
      expect_identical(grid$aes_params$colour, "white", info = name)
      expect_identical(grid$aes_params$linewidth, 0.8, info = name)
      expect_identical(sum(geoms == "GeomSegment"), 2L, info = name)
      expect_true(all(geoms %in% c("GeomTile", "GeomSegment", "GeomText")), info = name)
      # white alleys between replications, grey lines between incomplete blocks
      expect_true(outline_colour(p, "white"), info = name)
      expect_true(outline_colour(p, "gray50"), info = name)
      # the alley is a narrow white strip, still wider than the block lines
      widths <- vapply(p$layers[geoms == "GeomSegment"], function(l) l$aes_params$linewidth, 0)
      colours <- vapply(p$layers[geoms == "GeomSegment"], function(l) l$aes_params$colour, "")
      expect_identical(unname(widths[colours == "white"]), 2.2, info = name)
      expect_lt(widths[colours == "gray50"], widths[colours == "white"])
      # the grid covers exactly the drawn plots
      expect_identical(ggplot2::layer_data(p, 2)[c("x", "y")], ggplot2::layer_data(p, 1)[c("x", "y")])
    }
  }
})

test_that("the lattice plot grid follows flips and can be switched off", {
  design <- catalogue_design("alpha_lattice")
  flipped <- checked_layout_view(design, flip = TRUE)$out_layout
  expect_identical(ggplot2::layer_data(flipped, 2)[c("x", "y")], ggplot2::layer_data(flipped, 1)[c("x", "y")])
  plain <- checked_layout_view(design, field_grid = FALSE)
  for (p in list(plain$out_layout, plain$out_layoutPlots)) {
    expect_identical(sum(layer_geoms(p) == "GeomTile"), 1L)
  }
})

test_that("incomplete-block maps tint replications instead of colouring entries", {
  for (name in iblock_designs) {
    view <- checked_layout_view(catalogue_design(name))
    book <- view$fieldBookXY
    tiles <- fill_data(view$out_layout)
    cell <- match(paste(tiles$x, tiles$y), paste(book$COLUMN, book$ROW))
    fill <- tiles$fill
    # the replication's pastel tint, the same for all of its plots
    reps <- levels(factor(book$REP))
    expected <- fieldhub_field_tints(length(reps))[match(as.character(book$REP[cell]), reps)]
    expect_identical(fill, expected, info = name)
    # the plot-number map draws the same backgrounds
    numbers <- fill_data(view$out_layoutPlots)
    expect_identical(numbers$fill, tiles$fill, info = name)
  }
})

test_that("incomplete-block maps keep entry and plot-number labels", {
  for (name in iblock_designs) {
    view <- checked_layout_view(catalogue_design(name))
    book <- view$fieldBookXY
    text_of <- function(p) {
      ix <- which(layer_geoms(p) == "GeomText")
      d <- ggplot2::layer_data(p, ix[length(ix)])
      d[order(d$y, d$x), "label"]
    }
    ordered <- book[order(book$ROW, book$COLUMN), ]
    # dark labels
    ix <- which(layer_geoms(view$out_layout) == "GeomText")
    colour <- unique(ggplot2::layer_data(view$out_layout, ix[length(ix)])$colour)
    expect_identical(grDevices::col2rgb(colour), grDevices::col2rgb("gray10"), info = name)
    expect_identical(as.character(text_of(view$out_layout)), as.character(ordered$ENTRY), info = name)
    expect_identical(as.character(text_of(view$out_layoutPlots)), as.character(ordered$PLOT), info = name)
  }
})

test_that("plot() arguments still override the incomplete-block map styling", {
  design <- catalogue_design("alpha_lattice")
  view <- checked_layout_view(design, main = "CUSTOM", col.regions = rep("white", 8))
  expect_identical(view$out_layout$labels$title, "CUSTOM")
  expect_identical(unique(fill_data(view$out_layout)$fill), "white")
})

# Augmented designs: the same style, with every check plot on one neutral grey
# (fieldhub_check_fill()) and filler plots white with grey numbers.

fill_at <- function(p, book) {
  tiles <- fill_data(p)
  tiles$fill[match(paste(book$COLUMN, book$ROW), paste(tiles$x, tiles$y))]
}
text_at <- function(p, book) {
  ix <- which(layer_geoms(p) == "GeomText")
  d <- ggplot2::layer_data(p, ix[length(ix)])
  key <- match(paste(book$COLUMN, book$ROW), paste(d$x, d$y))
  list(label = as.character(d$label[key]), colour = as.character(d$colour[key]))
}

test_that("augmented RCBD maps tint blocks and show every check on one grey", {
  designs <- list(
    single = RCBD_augmented(lines = 50, checks = 3, b = 6, seed = 23),
    two_experiments = RCBD_augmented(lines = 40, checks = 3, b = 3, seed = 5, repsExpt = 2)
  )
  for (name in names(designs)) {
    view <- checked_layout_view(designs[[name]])
    book <- view$fieldBookXY
    is_check <- !is.na(book$CHECKS) & book$CHECKS == "1"
    filler <- is.na(book$CHECKS)
    expect_true(any(is_check), info = name)
    for (p in list(view$out_layout, view$out_layoutPlots)) {
      geoms <- layer_geoms(p)
      expect_identical(sum(geoms == "GeomTile"), 1L, info = name)
      expect_true(all(geoms %in% c("GeomTile", "GeomSegment", "GeomText")), info = name)
      # two white outlines: a thin plot grid and the wider alleys between blocks
      segments <- p$layers[geoms == "GeomSegment"]
      expect_length(segments, 2L)
      expect_true(all(vapply(segments, function(l) l$aes_params$colour, "") == "white"), info = name)
      widths <- sort(unname(vapply(segments, function(l) l$aes_params$linewidth, 0)))
      expect_identical(widths, c(0.8, 2.2), info = name)
    }
    entries <- fill_at(view$out_layout, book)
    numbers <- fill_at(view$out_layoutPlots, book)
    # one grey for all checks in the entry map; none in the plot-number map
    expect_true(all(entries[is_check] == fieldhub_check_fill()), info = name)
    expect_false(any(numbers == fieldhub_check_fill()), info = name)
    # fillers are white in both maps
    expect_true(all(entries[filler] == "#FFFFFF") && all(numbers[filler] == "#FFFFFF"), info = name)
    # every other plot has its block's tint: one colour per block, a different one per block
    group <- paste(book$EXPT, book$BLOCK)
    plain <- !is_check & !filler
    by_block <- lapply(split(entries[plain], group[plain]), unique)
    expect_true(all(lengths(by_block) == 1L), info = name)
    expect_length(unique(unlist(by_block)), length(by_block))
    expect_identical(numbers[plain], entries[plain], info = name)
    # labels: entries and plot numbers verbatim, dark; filler numbers grey
    text <- text_at(view$out_layout, book)
    expect_identical(text$label, as.character(book$ENTRY), info = name)
    expect_true(all(text$colour[!filler] == "gray10"), info = name)
    expect_true(all(text$colour[filler] == "gray60"), info = name)
    plots <- text_at(view$out_layoutPlots, book)
    expect_identical(plots$label, sprintf("%d", as.integer(book$PLOT)), info = name)
  }
})

test_that("augmented RCBD maps keep their returned field book and accept desplot options", {
  design <- RCBD_augmented(lines = 50, checks = 3, b = 6, seed = 23)
  view <- checked_layout_view(design)
  expect_true(all(c("PLOT_TXT", "CHECK_TEXT") %in% names(view$fieldBookXY)))
  flipped <- checked_layout_view(design, flip = TRUE, main = "CUSTOM")$out_layout
  expect_identical(flipped$labels$title, "CUSTOM")
  expect_s3_class(flipped, "ggplot")
})

test_that("augmented row-column maps label entries and show every check on one grey", {
  design <- augmented_row_column(nrows = 10, ncols = 10, checks = 4, check_direction = "rows",
                                 entries = 60, seed = 3)
  view <- checked_layout_view(design)
  book <- view$fieldBookXY
  is_check <- !is.na(book$CHECKS) & book$CHECKS != 0
  # interior edges between neighbouring plots, as "x y xend yend"
  interior <- with(book, c(
    paste(COLUMN + 0.5, ROW - 0.5, COLUMN + 0.5, ROW + 0.5)[COLUMN < max(COLUMN)],
    paste(COLUMN - 0.5, ROW + 0.5, COLUMN + 0.5, ROW + 0.5)[ROW < max(ROW)]))
  for (p in list(view$out_layout, view$out_layoutPlots)) {
    geoms <- layer_geoms(p)
    expect_identical(sum(geoms == "GeomTile"), 1L)
    expect_true(all(geoms %in% c("GeomTile", "GeomSegment", "GeomText")))
    # a white plot grid: every interior edge drawn once, as one white line
    expect_identical(sum(geoms == "GeomSegment"), 1L)
    expect_true(outline_colour(p, "white"))
    segments <- ggplot2::layer_data(p, which(geoms == "GeomSegment"))
    expect_true(all(segments$linewidth == 1.2))
    unit <- function(s) {
      out <- character()
      for (i in seq_len(nrow(s))) {
        if (s$y[i] == s$yend[i]) {
          x <- seq(min(s$x[i], s$xend[i]), max(s$x[i], s$xend[i]) - 1)
          out <- c(out, paste(x, s$y[i], x + 1, s$yend[i]))
        } else {
          y <- seq(min(s$y[i], s$yend[i]), max(s$y[i], s$yend[i]) - 1)
          out <- c(out, paste(s$x[i], y, s$xend[i], y + 1))
        }
      }
      out
    }
    expect_setequal(unit(segments), interior)
  }
  # entry numbers, not treatment names
  text <- text_at(view$out_layout, book)
  expect_identical(text$label, as.character(book$ENTRY))
  expect_false(any(text$label %in% book$TREATMENT[book$TREATMENT != as.character(book$ENTRY)]))
  entries <- fill_at(view$out_layout, book)
  numbers <- fill_at(view$out_layoutPlots, book)
  expect_true(all(entries[is_check] == fieldhub_check_fill()))
  expect_true(all(entries[!is_check] == fieldhub_field_tints(1)))
  expect_true(all(numbers == fieldhub_field_tints(1)))
  plots <- text_at(view$out_layoutPlots, book)
  expect_identical(plots$label, sprintf("%d", as.integer(book$PLOT)))
})

test_that("the check fill is a neutral grey that dark labels read on", {
  rgb <- grDevices::col2rgb(fieldhub_check_fill())
  expect_lte(max(rgb) - min(rgb), 12)
  expect_gte(contrast_ratio("gray10", fieldhub_check_fill()), 7)
  expect_false(fieldhub_check_fill() %in% fieldhub_field_tints(12))
})

# Designs in complete blocks or replications without incomplete blocks: one
# muted tint per replication, white alleys, and a desplot-drawn white grid
# around every plot (the strips of a strip-plot are its rows and columns).
complete_designs <- list(
  RCBD_two_locations = "TREATMENT", RCBD_checks = "TREATMENT", RCBD_cartesian = "TREATMENT",
  row_column = "ENTRY", row_column_twostage = "ENTRY",
  strip_plot = "TRT_COMB", strip_plot_labels = "TRT_COMB"
)

test_that("RCBD, row-column and strip-plot maps tint reps and draw a plot grid with desplot", {
  for (name in names(complete_designs)) {
    design <- catalogue_design(name)
    sites <- length(unique(design$fieldBook$LOCATION))
    for (location in seq_len(sites)) {
      view <- checked_layout_view(design, location = location)
      book <- view$fieldBookXY
      is_check <- if ("CHECKS" %in% names(book)) !is.na(book$CHECKS) & book$CHECKS != 0 else rep(FALSE, nrow(book))
      for (p in list(view$out_layout, view$out_layoutPlots)) {
        geoms <- layer_geoms(p)
        expect_identical(sum(geoms == "GeomTile"), 1L, info = name)
        expect_true(all(geoms %in% c("GeomTile", "GeomSegment", "GeomText")), info = name)
        segments <- p$layers[geoms == "GeomSegment"]
        colours <- vapply(segments, function(l) l$aes_params$colour, "")
        widths <- sort(unname(vapply(segments, function(l) l$aes_params$linewidth, 0)))
        expect_true(all(colours == "white"), info = name)
        expect_identical(widths, c(0.8, 2.2), info = name)
      }
      entries <- fill_at(view$out_layout, book)
      numbers <- fill_at(view$out_layoutPlots, book)
      # one tint per replication, a different one for each
      by_rep <- lapply(split(entries[!is_check], book$REP[!is_check]), unique)
      expect_true(all(lengths(by_rep) == 1L), info = name)
      expect_length(unique(unlist(by_rep)), length(by_rep))
      expect_identical(numbers[!is_check], entries[!is_check], info = name)
      # checks share the grey in the entry map only
      expect_true(all(entries[is_check] == fieldhub_check_fill()), info = name)
      expect_false(any(numbers == fieldhub_check_fill()), info = name)
      # labels keep their columns
      text <- text_at(view$out_layout, book)
      expect_identical(text$label, as.character(book[[complete_designs[[name]]]]), info = name)
      expect_true(all(text$colour == "gray10"), info = name)
      plots <- text_at(view$out_layoutPlots, book)
      expect_identical(plots$label, as.character(book$PLOT), info = name)
    }
  }
})

test_that("the complete-block plot grid can be switched off", {
  plain <- checked_layout_view(catalogue_design("RCBD_checks"), field_grid = FALSE)
  for (p in list(plain$out_layout, plain$out_layoutPlots)) {
    segments <- p$layers[layer_geoms(p) == "GeomSegment"]
    expect_length(segments, 1L)
    expect_identical(segments[[1]]$aes_params$linewidth, 2.2)
  }
})

# The remaining "Other Designs": CRD, Latin square and rectangle, factorial,
# split-plot and split-split-plot. Groups (reps or squares) get a tint and
# alleys, whole plots grey lines, sub-plots lighter grey lines, every plot a
# white grid; labels keep their columns.
other_designs <- list(
  CRD_count = list(text = "TREATMENT"), CRD_labels = list(text = "TREATMENT"),
  CRD_data = list(text = "TREATMENT"),
  latin_square = list(text = "TREATMENT", group = "SQUARE"),
  latin_square_cartesian = list(text = "TREATMENT"),
  latin_rectangle = list(text = "TREATMENT"),
  full_factorial_rcbd = list(text = "TRT_COMB", group = "REP"),
  full_factorial_crd_data = list(text = "TRT_COMB"),
  split_plot_rcbd = list(text = "TRT_COMB", group = "REP", block = TRUE),
  split_plot_crd = list(text = "TRT_COMB", block = TRUE),
  split_split_plot_rcbd = list(text = "TRT_COMB", group = "REP", block = TRUE, sub = TRUE),
  split_split_plot_crd = list(text = "TRT_COMB", block = TRUE, sub = TRUE)
)

segment_style <- function(p) {
  layers <- p$layers[layer_geoms(p) == "GeomSegment"]
  data.frame(colour = vapply(layers, function(l) l$aes_params$colour, ""),
             width = unname(vapply(layers, function(l) l$aes_params$linewidth, 0)))
}

test_that("the other designs' maps follow the field-alleys style", {
  for (name in names(other_designs)) {
    spec <- other_designs[[name]]
    design <- catalogue_design(name)
    sites <- length(unique(design$fieldBook$LOCATION))
    for (location in seq_len(sites)) {
      view <- checked_layout_view(design, location = location)
      book <- view$fieldBookXY
      grouped <- !is.null(spec$group) && length(unique(book[[spec$group]])) > 1L
      for (p in list(view$out_layout, view$out_layoutPlots)) {
        geoms <- layer_geoms(p)
        expect_true(all(geoms %in% c("GeomTile", "GeomSegment", "GeomText")), info = name)
        style <- segment_style(p)
        # alleys between groups
        expect_identical(any(style$colour == "white" & style$width == 2.2), grouped, info = name)
        if (isTRUE(spec$block)) {
          # whole plots: grey lines; plots: a white grid layer above the fill
          expect_true(any(style$colour == "gray50"), info = name)
          expect_identical(unname(which(geoms == "GeomTile")), 1:2, info = name)
          expect_identical(p$layers[[2]]$aes_params$colour, "white", info = name)
        } else {
          # plots: a desplot-drawn white grid, thinner when there are alleys
          expect_identical(sum(geoms == "GeomTile"), 1L, info = name)
          expect_true(any(style$colour == "white" & style$width == if (grouped) 0.8 else 1.2), info = name)
        }
        # sub-plots: lighter grey lines inside whole plots
        expect_identical(any(style$colour == "gray70"), isTRUE(spec$sub), info = name)
      }
      entries <- fill_at(view$out_layout, book)
      numbers <- fill_at(view$out_layoutPlots, book)
      expect_identical(numbers, entries, info = name)
      if (grouped) {
        by_group <- lapply(split(entries, book[[spec$group]]), unique)
        expect_true(all(lengths(by_group) == 1L), info = name)
        expect_length(unique(unlist(by_group)), length(by_group))
      } else {
        expect_true(all(entries == fieldhub_field_tints(1)), info = name)
      }
      text <- text_at(view$out_layout, book)
      expect_identical(text$label, as.character(book[[spec$text]]), info = name)
      expect_true(all(text$colour == "gray10"), info = name)
      expect_identical(text_at(view$out_layoutPlots, book)$label, as.character(book$PLOT), info = name)
    }
  }
})

test_that("split-split-plot sub-plot lines separate sub-plots inside a whole plot only", {
  view <- checked_layout_view(catalogue_design("split_split_plot_rcbd"))
  book <- view$fieldBookXY
  p <- view$out_layout
  ix <- which(layer_geoms(p) == "GeomSegment")
  lines <- ix[vapply(p$layers[ix], function(l) identical(l$aes_params$colour, "gray70"), TRUE)]
  seg <- ggplot2::layer_data(p, lines)
  vertical <- seg$x == seg$xend
  # the two plots on either side of each line
  a <- ifelse(vertical, paste(seg$x - 0.5, (seg$y + seg$yend) / 2), paste((seg$x + seg$xend) / 2, seg$y - 0.5))
  b <- ifelse(vertical, paste(seg$x + 0.5, (seg$y + seg$yend) / 2), paste((seg$x + seg$xend) / 2, seg$y + 0.5))
  key <- paste(book$COLUMN, book$ROW)
  ia <- match(a, key); ib <- match(b, key)
  expect_false(anyNA(c(ia, ib)))
  expect_true(all(book$PLOT[ia] == book$PLOT[ib]))
  expect_true(all(book$SUB_PLOT[ia] != book$SUB_PLOT[ib]))
  # and every such pair of neighbours gets a line
  right <- match(paste(book$COLUMN + 1, book$ROW), key); up <- match(paste(book$COLUMN, book$ROW + 1), key)
  from <- rep(seq_len(nrow(book)), 2); to <- c(right, up)
  pairs <- data.frame(from = from, to = to)[!is.na(to), ]
  pairs <- pairs[book$PLOT[pairs$from] == book$PLOT[pairs$to] & book$SUB_PLOT[pairs$from] != book$SUB_PLOT[pairs$to], ]
  expect_identical(nrow(seg), nrow(pairs))
})
