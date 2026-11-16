# Renders the generated images and the field-book excerpt of the Welcome page
# into inst/app/www, all from real FielDHub output with fixed seeds:
#   map-diagonal.png      field map of the "What you get" example
#   heatmap-yield.svg     its simulated yield, one square per plot
#   home-fieldbook.html   the first plots of row 1 of its field book
#   home-heat-scale.html  the yield range under the heatmap
#   thumb-*.svg           one small layout per design menu
# Re-run after changing the field map style or the example:
#   Rscript dev/page_images.R
pkgload::load_all(".", compile = FALSE, quiet = TRUE)
grDevices::pdf(NULL)  # plot() draws; keep it from writing Rplots.pdf
www <- "inst/app/www"
grey <- "#dde1e0"
checks <- c("#B9CADB", "#C6D7BF", "#E3D3B6", "#DEC4C4")

svg <- function(file, rects, width, height) {
  body <- sprintf('<rect x="%d" y="%d" width="%d" height="%d" fill="%s"/>',
                  rects$x, rects$y, rects$w, rects$h, rects$fill)
  writeLines(paste0('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ', width, " ", height,
                    '" width="', width, '" height="', height, '"><rect width="', width, '" height="',
                    height, '" fill="#fff"/>', paste(body, collapse = ""), "</svg>"),
             file.path(www, file), sep = "")
}

# The "What you get" example: one design, its map, field book and simulated yield
design <- diagonal_arrangement(nrows = 10, ncols = 14, lines = 126, checks = 4, seed = 21)
map <- plot(design)$p +
  ggplot2::theme(plot.title = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
                 plot.background = ggplot2::element_rect(fill = "white", colour = NA))
ggplot2::ggsave(file.path(www, "map-diagonal.png"), map, width = 1120 / 192, height = 760 / 192, dpi = 192)
book <- simulate_spatial_field_book(design$fieldBook, nrows = 10, ncols = 14, correlation_x = 0.6,
                                    correlation_y = 0.6, min_value = 40, max_value = 90,
                                    response_name = "YIELD", seed = 21)$field_book
low <- min(book$YIELD)
high <- max(book$YIELD)
shade <- function(v) {
  t <- (v - low) / (high - low)
  ramp <- round(outer(1 - t, c(232, 239, 234)) + outer(t, c(10, 86, 64)))
  sprintf("#%02x%02x%02x", ramp[, 1], ramp[, 2], ramp[, 3])
}
step <- 28
svg("heatmap-yield.svg",
    data.frame(x = (book$COLUMN - 1) * step, y = (10 - book$ROW) * step, w = 26, h = 26, fill = shade(book$YIELD)),
    14 * step - 2, 10 * step - 2)
columns <- c("PLOT", "ROW", "COLUMN", "CHECKS", "ENTRY", "TREATMENT", "YIELD")
rows <- utils::head(book[book$ROW == 1, columns], 7)
cell <- function(column, value) {
  if (column == "YIELD") return(sprintf("<td>%.1f</td>", value))
  if (column == "CHECKS" && value != 0) {
    return(sprintf('<td><span class="fh-chk fh-chk-%d">%d</span></td>', as.integer(value), as.integer(value)))
  }
  paste0("<td>", htmltools::htmlEscape(as.character(value)), "</td>")
}
body <- vapply(seq_len(nrow(rows)), function(i) {
  paste0("<tr>", paste(vapply(columns, function(column) cell(column, rows[[column]][i]), ""), collapse = ""), "</tr>")
}, "")
writeLines(c('<table class="fh-book"><caption class="sr-only">First rows of the field book for the same design, with a simulated yield column</caption>',
             paste0("<thead><tr>", paste0('<th scope="col">', columns, "</th>", collapse = ""), "</tr></thead>"),
             "<tbody>", body, "</tbody></table>"),
           file.path(www, "home-fieldbook.html"))
writeLines(sprintf('<p class="fh-scale"><span>%.0f</span><i></i><span>%.0f</span><span class="fh-scale-name">Simulated yield</span></p>',
                   low, high),
           file.path(www, "home-heat-scale.html"))

# One small layout per design menu
size <- 10
gap <- 2
pad <- 6
width <- 2 * pad + 14 * (size + gap) - gap
height <- 2 * pad + 8 * (size + gap) - gap
grid_cells <- function(book, fill) {
  data.frame(x = pad + (book$COLUMN - 1) * (size + gap), y = pad + (max(book$ROW) - book$ROW) * (size + gap),
             w = size, h = size, fill = fill)
}
unreplicated <- diagonal_arrangement(nrows = 8, ncols = 14, lines = 100, checks = 4, seed = 4)$fieldBook
svg("thumb-unreplicated.svg",
    grid_cells(unreplicated, ifelse(unreplicated$CHECKS == 0, grey, checks[(unreplicated$CHECKS - 1) %% 4 + 1])),
    width, height)
prep <- partially_replicated(nrows = 8, ncols = 14, repGens = c(30, 52), repUnits = c(2, 1), seed = 7)$fieldBook
svg("thumb-prep.svg", grid_cells(prep, ifelse(prep$CHECKS == 0, grey, checks[2])), width, height)
lattice <- alpha_lattice(t = 28, k = 7, reps = 4, seed = 3)$fieldBook
cells <- do.call(rbind, lapply(sort(unique(lattice$REP)), function(rep) {
  plots <- lattice[lattice$REP == rep, ]
  block <- match(plots$IBLOCK, sort(unique(plots$IBLOCK))) - 1
  x0 <- ((rep - 1) %% 2) * 7
  y0 <- ((rep - 1) %/% 2) * 4
  data.frame(x = pad + (x0 + plots$UNIT - 1) * (size + gap) + if (x0 > 0) 4 else 0,
             y = pad + (y0 + block) * (size + gap) + if (y0 > 0) 4 else 0,
             w = size, h = size, fill = ifelse(block %% 2 == 1, checks[1], grey))
}))
svg("thumb-lattice.svg", cells, width + 4, height + 4)
rcbd <- RCBD(t = 14, reps = 4, seed = 5)$fieldBook
position <- stats::ave(seq_len(nrow(rcbd)), rcbd$REP, FUN = seq_along) - 1
fills <- c(T1 = checks[3], T2 = checks[4], T3 = checks[1])
svg("thumb-other.svg",
    data.frame(x = pad + position * (size + gap), y = pad + (rcbd$REP - 1) * (2 * size + gap + 3),
               w = size, h = 2 * size,
               fill = ifelse(rcbd$TREATMENT %in% names(fills), fills[as.character(rcbd$TREATMENT)], grey)),
    width, 2 * pad + 4 * (2 * size + gap + 3) - gap - 3)
