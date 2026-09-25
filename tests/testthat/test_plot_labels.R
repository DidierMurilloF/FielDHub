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
  p <- plot_diagonal_arrangement(d, l = 1)
  expect_labels_verbatim(p$p1, as.numeric(d$fieldBook$ENTRY))
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
  p <- plot_prep(p_rep, l = 1)
  expect_labels_verbatim(p$p1, p_rep$fieldBook$ENTRY)
})

test_that("partially replicated design labels filler plots", {
  p_rep <- partially_replicated(
    nrows = 6, ncols = 7, repGens = c(5, 31), repUnits = c(2, 1),
    plotNumber = 101, seed = 1, allow_fillers = TRUE
  )
  p <- plot_prep(p_rep, l = 1)
  labels <- as.character(drawn_text(p$p1)[[1]]$label)

  expect_equal(sum(labels == "Filler"), 1)
  expect_false("0" %in% labels)
})

test_that("optimized arrangement draws entry numbers unabbreviated", {
  o <- optimized_arrangement(
    nrows = 12, ncols = 10, lines = 110, amountChecks = 10, checks = 1,
    plotNumber = 101, seed = 1
  )
  p <- plot_optim(o, l = 1)
  expect_labels_verbatim(p$p1, o$fieldBook$ENTRY)
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
  p <- plot_augmented_RCBD(a, l = 1)

  # p1: entry numbers, verbatim, in a single text layer
  expect_labels_verbatim(p$p1, fb$ENTRY)
  d1 <- drawn_text(p$p1)[[1]]

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
  expect_length(tile_fills(p$p1), length(unique(fb$BLOCK)))

  # p2: plot numbers, verbatim, one colour, same size
  expect_labels_verbatim(p$p2, sprintf("%d", as.integer(fb$PLOT)))
  d2 <- drawn_text(p$p2)[[1]]
  expect_equal(unique(as.character(d2$colour)), "gray10")
  expect_equal(unique(as.numeric(d2$size)), 3.2)
})
