library(FielDHub)

# Cell text on a field map must be drawn exactly as it is stored. desplot's
# default is shorten = "abb", which runs abbreviate() over the labels and turns
# three-digit entry numbers into two-digit ones (100 -> "00", 108 -> "08"),
# i.e. it silently shows a different, equally plausible entry number.

# All text layers that are actually visible (the layer that desplot adds when
# only 'col' is given carries size 0 and no label of interest).
drawn_text <- function(p) {
  out <- list()
  for (i in seq_along(p$layers)) {
    if (class(p$layers[[i]]$geom)[1] != "GeomText") next
    d <- ggplot2::layer_data(p, i)
    if (max(as.numeric(d$size)) == 0) next
    out[[length(out) + 1L]] <- d
  }
  out
}

tile_fills <- function(p) {
  ix <- which(vapply(p$layers, function(l) class(l$geom)[1] == "GeomTile", TRUE))
  sort(unique(as.character(ggplot2::layer_data(p, ix[1])$fill)))
}

# Guard against a vacuous test: the labels must be such that abbreviation
# would in fact change them.
expect_abbreviation_would_bite <- function(labels) {
  u <- unique(as.character(labels))
  expect_false(all(unname(abbreviate(u, 2, method = "both")) == u))
}

# One visible text layer whose labels are the expected ones, verbatim.
expect_labels_verbatim <- function(p, expected) {
  expected <- as.character(expected)
  expect_abbreviation_would_bite(expected)
  layers <- drawn_text(p)
  expect_length(layers, 1L)
  expect_equal(sort(as.character(layers[[1]]$label)), sort(expected))
}

test_that("diagonal arrangement draws entry numbers unabbreviated", {
  d <- diagonal_arrangement(
    nrows = 15, ncols = 10, lines = 120, checks = 4,
    plotNumber = 101, seed = 1
  )
  p <- plot_layout(d, l = 1)
  expect_labels_verbatim(p$out_layout, as.numeric(d$fieldBook$ENTRY))
})

test_that("plot() reaches the same labels through the public API", {
  d <- diagonal_arrangement(
    nrows = 15, ncols = 10, lines = 120, checks = 4,
    plotNumber = 101, seed = 1
  )
  # plot() prints its layout, so send that to a null device
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_labels_verbatim(plot(d)$p, as.numeric(d$fieldBook$ENTRY))
})

test_that("partially replicated design draws entry numbers unabbreviated", {
  p_rep <- partially_replicated(
    nrows = 14, ncols = 10, repGens = c(20, 100), repUnits = c(2, 1),
    plotNumber = 101, seed = 1
  )
  p <- plot_layout(p_rep, l = 1)
  expect_labels_verbatim(p$out_layout, p_rep$fieldBook$ENTRY)
})

test_that("partially replicated design labels filler plots", {
  p_rep <- partially_replicated(
    nrows = 6, ncols = 7, repGens = c(5, 31), repUnits = c(2, 1),
    plotNumber = 101, seed = 1, allow_fillers = TRUE
  )
  p <- plot_layout(p_rep, l = 1)
  labels <- as.character(drawn_text(p$out_layout)[[1]]$label)

  expect_equal(sum(labels == "Filler"), 1)
  expect_false("0" %in% labels)
})

test_that("plot-number maps do not inherit replication or check colours", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (name in c("partially_replicated_fillers", "multi_location_prep", "optimized_arrangement",
                 "diagonal_single", "diagonal_fillers", "sparse_allocation")) {
    design <- if (name == "diagonal_fillers") {
      diagonal_arrangement(nrows = 18, ncols = 18, lines = 287, checks = 4, seed = 27)
    } else catalogue_design(name)
    if (name == "diagonal_fillers") {
      entries <- checked_layout_view(design)$out_layout
      expect_true("Filler" %in% drawn_text(entries)[[1L]]$label)
      expect_false("0" %in% drawn_text(entries)[[1L]]$label)
    }
    for (location in seq_along(location_books(design))) {
      book <- location_books(design)[[location]]
      numbers <- plot(design, l = location, text.string = "PLOT")$p
      expect_equal(as.character(drawn_text(numbers)[[1L]]$label), as.character(book$PLOT), info = name)
      expect_length(tile_fills(numbers), 1L)
      expect_equal(unname(grDevices::col2rgb(tile_fills(numbers))), unname(grDevices::col2rgb("#F2F2F2")),
                    info = name)
      expect_length(unique(drawn_text(numbers)[[1L]]$colour), 1L)
      if (name %in% c("partially_replicated_fillers", "multi_location_prep")) {
        entries <- plot(design, l = location)$p
        colours <- apply(grDevices::col2rgb(tile_fills(entries)), 2L, paste, collapse = ",")
        expect_true("46,139,87" %in% colours, info = name) # seagreen
      }
    }
  }
})

test_that("multiple-diagonal plot numbers and experiment maps colour only experiments", {
  design <- catalogue_design("diagonal_blocks_row")
  book <- design$fieldBook
  for (label in c("PLOT", "EXPT")) {
    map <- checked_layout_view(design, text.string = label)$out_layout
    text <- drawn_text(map)[[1L]]
    expect_equal(as.character(text$label), as.character(book[[label]]))
    expect_length(unique(text$colour), 1L)
    tile <- ggplot2::layer_data(map, which(vapply(map$layers, function(layer) inherits(layer$geom, "GeomTile"), TRUE))[1])
    experiment <- book$EXPT[match(paste(tile$x, tile$y), paste(book$COLUMN, book$ROW))]
    expect_true(all(vapply(split(tile$fill, experiment), function(fill) length(unique(fill)) == 1L, TRUE)))
    expect_length(unique(tile$fill), length(unique(book$EXPT)))
  }
})

test_that("unreplicated entry maps fill check cells and keep test lines light gray", {
  designs <- lapply(c("diagonal_single", "diagonal_blocks_row", "sparse_allocation"), catalogue_design)
  designs <- c(designs, list(
    diagonal_arrangement(nrows = 18, ncols = 18, lines = 287, checks = 4, seed = 27),
    optimized_arrangement(nrows = 12, ncols = 10, lines = 100, checks = 4,
                          rep_checks = c(5, 5, 5, 5), seed = 27)))
  for (design in designs) {
    before <- serialize(design, NULL)
    for (location in seq_along(location_books(design))) {
      book <- location_books(design)[[location]]
      view <- checked_layout_view(design, location = location)
      map <- view$out_layout
      tile <- ggplot2::layer_data(map,
        which(vapply(map$layers, function(layer) inherits(layer$geom, "GeomTile"), TRUE))[1L])
      cells <- match(paste(tile$x, tile$y), paste(book$COLUMN, book$ROW))
      expect_false(anyNA(cells))
      checks <- as.character(book$CHECKS[cells])
      expect_true(any(checks == "0"))
      expect_true(any(checks != "0"))
      expect_false(any(tile$fill[checks != "0"] == "#F2F2F2"))
      groups <- split(tile$fill[checks != "0"], checks[checks != "0"])
      expect_true(all(lengths(lapply(groups, unique)) == 1L))
      expect_length(unique(tile$fill[checks != "0"]), length(unique(checks[checks != "0"])))
      if (length(setdiff(unique(book$EXPT), "Filler")) > 1L) {
        # Adding check fills must not discard the existing experiment backgrounds.
        experiments <- checked_layout_view(design, location = location, text.string = "EXPT")$out_layout
        base <- ggplot2::layer_data(experiments,
          which(vapply(experiments$layers, function(layer) inherits(layer$geom, "GeomTile"), TRUE))[1L])
        expect_identical(tile$fill[checks == "0"], base$fill[checks == "0"])
      } else {
        expect_true(all(tile$fill[checks == "0"] == "#F2F2F2"))
        # Single-experiment maps use the same check colours as optimized arrangements.
        levels <- sort(unique(checks))
        expected <- stats::setNames(fieldhub_layout_palette()[seq_along(levels)], levels)
        expect_identical(tile$fill, unname(expected[checks]))
      }
      expect_length(unique(drawn_text(map)[[1L]]$colour), 1L)
      expect_identical(as.character(view$fieldBookXY$CHECKS), as.character(book$CHECKS))
    }
    expect_identical(serialize(design, NULL), before)
  }
})

test_that("shared neutral fills are lighter while custom palettes remain caller-controlled", {
  book <- data.frame(ROW = c(1, 1), COLUMN = c(1, 2), ENTRY = c("A", "B"))
  map <- plot_desplot(ENTRY ~ COLUMN + ROW, book)
  expect_setequal(tile_fills(map), c("#F2F2F2", "#FFD9D9"))
  custom <- plot_desplot(ENTRY ~ COLUMN + ROW, book,
                         extra_args = list(col.regions = c("white", "navy")))
  expect_setequal(tile_fills(custom), c("white", "navy"))
  for (name in c("diagonal_single", "diagonal_blocks_row", "sparse_allocation",
                 "optimized_arrangement", "partially_replicated_fillers", "multi_location_prep")) {
    design <- catalogue_design(name)
    for (label in c("ENTRY", "PLOT")) {
      map <- checked_layout_view(design, text.string = label, col.regions = c("white", "white"))$out_layout
      expect_equal(unname(grDevices::col2rgb(tile_fills(map))), unname(grDevices::col2rgb("white")),
                    info = paste(name, label))
    }
  }
})

test_that("optimized arrangement draws entry numbers unabbreviated", {
  o <- optimized_arrangement(
    nrows = 12, ncols = 10, lines = 110, rep_checks = 10, checks = 1,
    plotNumber = 101, seed = 1
  )
  p <- plot_layout(o, l = 1)
  expect_labels_verbatim(p$out_layout, o$fieldBook$ENTRY)
})

test_that("optimized entry maps outline checks without adding outlines to plot-number maps", {
  design <- optimized_arrangement(nrows = 12, ncols = 10, lines = 100, checks = 4,
    rep_checks = c(5, 5, 5, 5), l = 2, seed = 27)
  before <- serialize(design, NULL)
  borders <- function(p) which(vapply(p$layers, function(layer) inherits(layer$stat, "StatTileBorder"), TRUE))
  segment_keys <- function(d) apply(d[c("x", "y", "xend", "yend")], 1L, paste, collapse = ":")
  for (location in 1:2) for (flip in c(FALSE, TRUE)) {
    entries <- checked_layout_view(design, location = location, flip = flip)$out_layout
    numbers <- checked_layout_view(design, location = location, flip = flip, text.string = "PLOT")$out_layout
    expect_length(borders(entries), 1L)
    expect_length(borders(numbers), 0L)
    border <- ggplot2::layer_data(entries, borders(entries))
    background <- ggplot2::layer_data(entries, 1L)
    # Every internal edge touching a check is outlined, even between two copies
    # of the same check. Ordinary test-to-test edges must not become a grid.
    cells <- background[entries$data$CHECKS != "0", ]
    expected <- data.frame(x = numeric(), y = numeric(), xend = numeric(), yend = numeric())
    add_edge <- function(x, y, xend, yend) data.frame(x = x, y = y, xend = xend, yend = yend)
    for (i in seq_len(nrow(cells))) {
      x <- cells$x[i]
      y <- cells$y[i]
      if (x > min(background$x)) expected <- rbind(expected, add_edge(x - 0.5, y - 0.5, x - 0.5, y + 0.5))
      if (x < max(background$x)) expected <- rbind(expected, add_edge(x + 0.5, y - 0.5, x + 0.5, y + 0.5))
      if (y > min(background$y)) expected <- rbind(expected, add_edge(x - 0.5, y - 0.5, x + 0.5, y - 0.5))
      if (y < max(background$y)) expected <- rbind(expected, add_edge(x - 0.5, y + 0.5, x + 0.5, y + 0.5))
    }
    expect_setequal(segment_keys(border), segment_keys(expected))
    expect_true(all(border$colour == "gray50"))
    expect_true(all(border$linewidth == 1))
    expect_true(all(border$linetype == 1))
    # Borders are the only change: disabling them restores the same fills and labels.
    plain <- checked_layout_view(design, location = location, flip = flip, out2.string = NULL)$out_layout
    expect_length(borders(plain), 0L)
    expect_identical(tile_fills(entries), tile_fills(plain))
    expect_equal(drawn_text(entries), drawn_text(plain))
    expect_identical(tile_fills(numbers), "#F2F2F2")
  }
  custom <- checked_layout_view(design, out2.gpar = list(col = "navy", lwd = 2, lty = 2))$out_layout
  border <- ggplot2::layer_data(custom, borders(custom))
  expect_true(all(border$colour == "navy" & border$linewidth == 2 & border$linetype == 2))
  expect_identical(serialize(design, NULL), before)
})

# The augmented RCBD layout used to draw its cell text with ggplot2::geom_text()
# layers on top of a desplot plot whose own text was switched off. desplot draws
# the same thing natively, and the checks keep their own text colour.
test_that("augmented RCBD layout draws entries natively, checks in red", {
  a <- RCBD_augmented(
    lines = 110, checks = 4, b = 6, l = 1,
    plotNumber = 101, seed = 1
  )
  fb <- a$fieldBook
  p <- plot_layout(a, l = 1)

  # p1: entry numbers, verbatim, in a single text layer
  expect_labels_verbatim(p$out_layout, fb$ENTRY)
  d1 <- drawn_text(p$out_layout)[[1]]

  # the checks - and only the checks - are red
  is_check <- as.character(fb$CHECKS) == "1"
  expect_true(any(is_check))
  expect_setequal(unique(as.character(d1$colour)), c("gray10", "red3"))
  expect_equal(
    sort(as.character(d1$label[d1$colour == "red3"])),
    sort(as.character(fb$ENTRY[is_check]))
  )

  # text size and the muted block background are the ones the overlay produced
  expect_equal(unique(as.numeric(d1$size)), 3.2)
  expect_length(tile_fills(p$out_layout), length(unique(fb$BLOCK)))

  # p2: plot numbers, verbatim, one colour, same size
  expect_labels_verbatim(p$out_layoutPlots, sprintf("%d", as.integer(fb$PLOT)))
  d2 <- drawn_text(p$out_layoutPlots)[[1]]
  expect_equal(unique(as.character(d2$colour)), "gray10")
  expect_equal(unique(as.numeric(d2$size)), 3.2)
})

test_that("augmented RCBD fills and borders only check cells without changing block backgrounds", {
  design <- RCBD_augmented(lines = 110, checks = 4, b = 6, l = 2, seed = 1)
  before <- serialize(design, NULL)
  for (location in 1:2) for (flip in c(FALSE, TRUE)) {
    view <- checked_layout_view(design, location = location, flip = flip)
    entries <- view$out_layout
    numbers <- view$out_layoutPlots
    tile_layers <- function(p) which(vapply(p$layers, function(layer) inherits(layer$geom, "GeomTile"), TRUE))
    expect_length(tile_layers(entries), 2L)
    expect_length(tile_layers(numbers), 1L)
    # The original muted block fills remain exactly the same as in the number map.
    background <- ggplot2::layer_data(entries, tile_layers(entries)[1L])
    expect_equal(background, ggplot2::layer_data(numbers, tile_layers(numbers)[1L]))
    checks <- ggplot2::layer_data(entries, tile_layers(entries)[2L])
    data <- entries$data[entries$data$CHECK_TEXT == "check", , drop = FALSE]
    expect_equal(nrow(checks), nrow(data))
    is_check <- entries$data$CHECK_TEXT == "check"
    expect_equal(checks$x, background$x[is_check])
    expect_equal(checks$y, background$y[is_check])
    expect_true(all(checks$colour == "gray50"))
    expect_true(all(checks$linewidth == 1))
    expected <- fieldhub_layout_palette()[-1L][as.integer(factor(data$ENTRY))]
    expect_identical(checks$fill, expected)
    expect_length(unique(checks$fill), 4L)
    # Filled tiles precede both border layers and the native text: labels stay visible.
    expect_lt(tile_layers(entries)[2L], min(which(vapply(entries$layers,
      function(layer) inherits(layer$geom, "GeomText"), TRUE))))
    expect_equal(sum(vapply(entries$layers, function(layer) inherits(layer$stat, "StatTileBorder"), TRUE)), 2L)
    expect_equal(sum(vapply(numbers$layers, function(layer) inherits(layer$stat, "StatTileBorder"), TRUE)), 2L)
  }
  expect_identical(serialize(design, NULL), before)
})
