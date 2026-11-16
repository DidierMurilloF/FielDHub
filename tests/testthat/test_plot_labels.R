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
      if (name %in% c("partially_replicated_fillers", "multi_location_prep")) {
        # p-rep maps: plots on the neutral, filler plots white with grey labels
        filler <- book$TREATMENT == "Filler"
        tile <- ggplot2::layer_data(numbers, 1L)
        fill <- tile$fill[match(paste(book$COLUMN, book$ROW), paste(tile$x, tile$y))]
        expect_true(all(fill[!filler] == "#F2F2F2"), info = name)
        expect_true(all(fill[filler] == "#FFFFFF"), info = name)
        entries <- plot(design, l = location)$p
        expect_true(fieldhub_replicated_fill() %in% tile_fills(entries), info = name)
      } else {
        # plots on the neutral, filler plots white; plot numbers in one dark colour
        filler <- book$TREATMENT == "Filler"
        tile <- ggplot2::layer_data(numbers, 1L)
        fill <- tile$fill[match(paste(book$COLUMN, book$ROW), paste(tile$x, tile$y))]
        expect_true(all(fill[!filler] == "#F2F2F2"), info = name)
        expect_true(all(fill[filler] == "#FFFFFF"), info = name)
        text <- drawn_text(numbers)[[1L]]
        colour <- text$colour[match(paste(book$COLUMN, book$ROW), paste(text$x, text$y))]
        expect_length(unique(colour[!filler]), 1L)
      }
    }
  }
})

test_that("multiple-diagonal maps colour experiments only in the experiment map", {
  design <- catalogue_design("diagonal_blocks_row")
  book <- design$fieldBook
  for (label in c("PLOT", "EXPT")) {
    map <- checked_layout_view(design, text.string = label)$out_layout
    text <- drawn_text(map)[[1L]]
    expect_equal(as.character(text$label), as.character(book[[label]]))
    expect_length(unique(text$colour), 1L)
    tile <- ggplot2::layer_data(map, which(vapply(map$layers, function(layer) inherits(layer$geom, "GeomTile"), TRUE))[1])
    experiment <- book$EXPT[match(paste(tile$x, tile$y), paste(book$COLUMN, book$ROW))]
    if (label == "EXPT") {
      expect_true(all(vapply(split(tile$fill, experiment), function(fill) length(unique(fill)) == 1L, TRUE)))
      expect_length(unique(tile$fill), length(unique(book$EXPT)))
    } else {
      # plot numbers: experiments are outlined, not coloured
      expect_identical(unique(tile$fill), "#F2F2F2")
    }
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
      filler <- book$TREATMENT[cells] == "Filler"
      expect_true(any(checks == "0"))
      expect_true(any(checks != "0"))
      # each check has its own muted colour; test lines stay on the neutral
      expect_false(any(tile$fill[checks != "0" & !filler] == "#F2F2F2"))
      groups <- split(tile$fill[checks != "0" & !filler], checks[checks != "0" & !filler])
      expect_true(all(lengths(lapply(groups, unique)) == 1L))
      expect_length(unique(tile$fill[checks != "0" & !filler]), length(groups))
      expect_true(all(tile$fill[checks != "0" & !filler] %in% fieldhub_check_palette(length(groups))))
      expect_true(all(tile$fill[checks == "0" & !filler] == "#F2F2F2"))
      text <- drawn_text(map)[[1L]]
      expect_length(unique(text$colour[!(text$label == "Filler")]), 1L)
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

test_that("optimized maps draw a white grid around every plot instead of check borders", {
  design <- optimized_arrangement(nrows = 12, ncols = 10, lines = 100, checks = 4,
    rep_checks = c(5, 5, 5, 5), l = 2, seed = 27)
  before <- serialize(design, NULL)
  borders <- function(p) which(vapply(p$layers, function(layer) inherits(layer$stat, "StatTileBorder"), TRUE))
  segment_keys <- function(d) {
    out <- character()
    for (i in seq_len(nrow(d))) {
      if (d$y[i] == d$yend[i]) {
        x <- seq(min(d$x[i], d$xend[i]), max(d$x[i], d$xend[i]) - 1)
        out <- c(out, paste(x, d$y[i], x + 1, d$yend[i], sep = ":"))
      } else {
        y <- seq(min(d$y[i], d$yend[i]), max(d$y[i], d$yend[i]) - 1)
        out <- c(out, paste(d$x[i], y, d$xend[i], y + 1, sep = ":"))
      }
    }
    out
  }
  for (location in 1:2) for (flip in c(FALSE, TRUE)) {
    entries <- checked_layout_view(design, location = location, flip = flip)$out_layout
    numbers <- checked_layout_view(design, location = location, flip = flip, text.string = "PLOT")$out_layout
    for (map in list(entries, numbers)) {
      expect_length(borders(map), 1L)
      grid <- ggplot2::layer_data(map, borders(map))
      background <- ggplot2::layer_data(map, 1L)
      # every internal edge between two plots is drawn once, in white
      x <- background$x; y <- background$y
      expected <- c(paste(x + 0.5, y - 0.5, x + 0.5, y + 0.5, sep = ":")[x < max(x)],
                    paste(x - 0.5, y + 0.5, x + 0.5, y + 0.5, sep = ":")[y < max(y)])
      expect_setequal(segment_keys(grid), expected)
      expect_true(all(grid$colour == "white" & grid$linewidth == 1.2 & grid$linetype == 1))
    }
    # the grid is the only addition: switching it off keeps the same fills and labels
    plain <- checked_layout_view(design, location = location, flip = flip, field_grid = FALSE)$out_layout
    expect_length(borders(plain), 0L)
    expect_identical(tile_fills(entries), tile_fills(plain))
    expect_equal(drawn_text(entries), drawn_text(plain))
    expect_identical(tile_fills(numbers), "#F2F2F2")
  }
  custom <- checked_layout_view(design, out1.gpar = list(col = "navy", lwd = 2, lty = 2))$out_layout
  border <- ggplot2::layer_data(custom, borders(custom))
  expect_true(all(border$colour == "navy" & border$linewidth == 2 & border$linetype == 2))
  expect_identical(serialize(design, NULL), before)
})

# The augmented RCBD layout used to draw its cell text with ggplot2::geom_text()
# layers on top of a desplot plot whose own text was switched off. desplot draws
# the same thing natively; checks are shown by a grey background, not by colour.
test_that("augmented RCBD layout draws entries natively with dark labels", {
  a <- RCBD_augmented(
    lines = 110, checks = 4, b = 6, l = 1,
    plotNumber = 101, seed = 1
  )
  fb <- a$fieldBook
  p <- plot_layout(a, l = 1)

  # p1: entry numbers, verbatim, in a single text layer: dark, fillers grey
  expect_labels_verbatim(p$out_layout, fb$ENTRY)
  d1 <- drawn_text(p$out_layout)[[1]]
  expect_true(all(as.character(d1$colour) %in% c("gray10", "gray60")))
  expect_true("gray10" %in% d1$colour)

  # p2: plot numbers, verbatim, with the same colours
  expect_labels_verbatim(p$out_layoutPlots, sprintf("%d", as.integer(fb$PLOT)))
  d2 <- drawn_text(p$out_layoutPlots)[[1]]
  expect_identical(sort(as.character(d2$colour)), sort(as.character(d1$colour)))
  expect_equal(unique(as.numeric(d1$size)), unique(as.numeric(d2$size)))
})

test_that("augmented RCBD colours only check cells without changing block backgrounds", {
  design <- RCBD_augmented(lines = 110, checks = 4, b = 6, l = 2, seed = 1)
  before <- serialize(design, NULL)
  for (location in 1:2) for (flip in c(FALSE, TRUE)) {
    view <- checked_layout_view(design, location = location, flip = flip)
    entries <- view$out_layout
    numbers <- view$out_layoutPlots
    tile_layers <- function(p) which(vapply(p$layers, function(layer) inherits(layer$geom, "GeomTile"), TRUE))
    # a single desplot fill layer in both maps
    expect_length(tile_layers(entries), 1L)
    expect_length(tile_layers(numbers), 1L)
    e <- ggplot2::layer_data(entries, tile_layers(entries))
    n <- ggplot2::layer_data(numbers, tile_layers(numbers))
    is_check <- entries$data$CHECK_TEXT == "check"
    expect_identical(e$fill[is_check], expected_check_fills(entries$data$ENTRY[is_check]))
    # outside the checks, both maps draw the same block backgrounds
    expect_identical(e$fill[!is_check], n$fill[!is_check])
    expect_false(any(n$fill %in% fieldhub_check_palette(4)))
  }
  expect_identical(serialize(design, NULL), before)
})
