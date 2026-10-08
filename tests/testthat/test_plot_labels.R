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
      expect_equal(unname(grDevices::col2rgb(tile_fills(numbers))), unname(grDevices::col2rgb("gray")),
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

test_that("optimized arrangement draws entry numbers unabbreviated", {
  o <- optimized_arrangement(
    nrows = 12, ncols = 10, lines = 110, rep_checks = 10, checks = 1,
    plotNumber = 101, seed = 1
  )
  p <- plot_layout(o, l = 1)
  expect_labels_verbatim(p$out_layout, o$fieldBook$ENTRY)
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
