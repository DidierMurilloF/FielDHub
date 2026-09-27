#' Shared classic heatmap rendering from named field-book columns
#' @importFrom rlang .data
#' @noRd
app_field_heatmap <- function(field_book, response_name, selected = 1L,
                               label_column = "TREATMENT", label_title = "Treatment",
                               include_site = TRUE, include_checks = FALSE,
                               height = 560) {
  data <- field_book_heatmap_data(
    field_book, response_name, selected, label_column, label_title,
    include_site, include_checks
  )
  app_tile_heatmap(data, response_name, height,
                   title = paste("Heatmap for ", response_name), classic = TRUE)
}

#' Shared renderer for existing spatial simulation outputs
#' @noRd
app_spatial_heatmap <- function(simulations, response_name, selected = 1L,
                                 height = 720, show_title = FALSE) {
  data <- spatial_heatmap_data(simulations, response_name, selected)
  title <- if (show_title) paste("Heat map for", response_name) else NULL
  app_tile_heatmap(data, response_name, height, title)
}

#' Draw named tile data with the established classic or spatial appearance
#' @noRd
app_tile_heatmap <- function(data, response_name, height, title = NULL,
                              classic = FALSE) {
  plot <- ggplot2::ggplot(
    data, ggplot2::aes(x = .data[["COLUMN"]], y = .data[["ROW"]],
                      fill = .data[[response_name]], text = .data[["text"]])
  ) +
    ggplot2::geom_tile() +
    ggplot2::xlab("COLUMN") +
    ggplot2::ylab("ROW") +
    ggplot2::labs(fill = response_name) +
    fieldhub_viridis_scale()
  if (!is.null(title)) plot <- plot + ggplot2::ggtitle(title)
  if (classic) {
    plot <- plot + ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(
        family = "Calibri", face = "bold", size = 13, hjust = 0.5
      ))
  }
  plotly::ggplotly(plot, tooltip = "text", height = height)
}
