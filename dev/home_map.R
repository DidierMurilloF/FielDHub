# Renders inst/app/www/home-map.png, the field map on the Welcome page.
# Re-run after changing the field map style:
#   Rscript dev/home_map.R
pkgload::load_all(".", compile = FALSE, quiet = TRUE)
grDevices::pdf(NULL)  # plot() draws; keep it from writing Rplots.pdf
design <- diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4, seed = 21)
map <- plot(design)$p +
  ggplot2::theme(plot.title = ggplot2::element_blank(), axis.title = ggplot2::element_blank(),
                 plot.background = ggplot2::element_rect(fill = "white", colour = NA))
ggplot2::ggsave("inst/app/www/home-map.png", map, width = 1320 / 192, height = 768 / 192, dpi = 192)
