#' Responsive image panel shared by every app plot
#' @noRd
app_plot_ui <- function(ns, panel, csv_id = paste0(panel$id, "_csv"), controls = NULL) {
  shiny::div(class = "fieldhub-plot-panel",
    shiny::div(class = "fieldhub-plot-header",
      # Downloads come first so they stay at the top right when controls wrap.
      # Keep their real height while loading, without hiding the live controls.
      shiny::div(id = ns(paste0(panel$id, "_tools")), class = "fieldhub-plot-toolbar",
        `aria-hidden` = "true", inert = NA,
        app_plot_tools(ns, panel$id, csv_id)),
      if (length(controls)) shiny::div(class = "fieldhub-plot-controls", controls)),
    shiny::div(class = "fieldhub-layout-image",
      role = "region", `aria-label` = paste(panel$title, "image"),
      app_output_feedback(shiny::imageOutput(ns(panel$id), width = NULL, height = NULL)))
  )
}

#' Base rendering dimensions for sharp images and downloads without huge bitmaps
#' @noRd
app_spatial_plot_size <- function(plot) {
  book <- plot$data
  extent <- function(x) max(as.numeric(as.character(x)))
  list(width = min(1800, max(800, extent(book$COLUMN) * 44 + 160)),
       height = min(1800, max(560, extent(book$ROW) * 36 + 160)))
}

#' Write the same plot for the screen or a publication-quality download
#' @noRd
app_spatial_plot_file <- function(plot, file, format = "png", dpi = 300,
                                  size = app_spatial_plot_size(plot)) {
  ggplot2::ggsave(file, plot = plot, device = format, width = size$width / 96,
                  height = size$height / 96, units = "in", dpi = dpi, bg = "white")
  invisible(size)
}

#' Validate the browser's drawing area and bound preview memory, preserving its ratio
#' @noRd
app_spatial_preview_size <- function(size) {
  if (!is.list(size)) return(NULL)
  valid <- function(value) is.numeric(value) && length(value) == 1L &&
    !is.complex(value) && is.null(dim(value)) && is.finite(value) && value > 0
  if (!valid(size$width) || !valid(size$height)) return(NULL)
  scale <- min(1, 1800 / max(size$width, size$height))
  list(width = max(1, floor(size$width * scale)), height = max(1, floor(size$height * scale)))
}

#' Fit preview text to dense fields without changing the source plot or downloads
#' @noRd
app_plot_preview <- function(plot, size) {
  columns <- length(unique(plot$data$COLUMN))
  rows <- length(unique(plot$data$ROW))
  cell_width <- max(1, size$width - 90) / columns
  cell_height <- max(1, size$height - 90) / rows
  # Reserve space between cells; width is estimated conservatively for full labels.
  fit_points <- function(pixels, characters) pixels * 0.8 * 72 / 96 / max(1, characters * 0.55)
  plot <- plot + ggplot2::theme(
    axis.text.x = ggplot2::element_text(size = min(10, fit_points(cell_width, nchar(columns)))),
    axis.text.y = ggplot2::element_text(size = min(10, cell_height * 0.6)))
  text_layers <- which(vapply(plot$layers, function(layer) inherits(layer$geom, "GeomText"), TRUE))
  if (length(text_layers)) {
    data <- ggplot2::ggplot_build(plot)$data
    for (i in text_layers) {
      label <- as.character(data[[i]]$label)
      characters <- max(1, nchar(label, type = "width"), na.rm = TRUE)
      # Very small text can disappear on raster devices; keep a visible minimum.
      limit <- max(1.1, min(fit_points(cell_width, characters), cell_height * 0.6) * 25.4 / 72)
      # Layers are reference objects: clone before modifying, so cached/public plots stay untouched.
      layer <- local({
        original <- plot$layers[[i]]
        ggplot2::ggproto(NULL, original)
      })
      layer$aes_params$size <- pmin(data[[i]]$size, limit)
      plot$layers[[i]] <- layer
    }
  }
  plot
}

#' Keep matrix CSV exports available after replacing a grid with an image
#' @noRd
app_spatial_plot_data <- function(panel, design, location, values) {
  data <- panel$grid(design, location, values)$data
  data.frame(ROW = as.integer(rownames(data)), data, row.names = NULL, check.names = FALSE)
}

#' A plot's values in their displayed row and column positions
#' @noRd
app_plot_grid_data <- function(plot, value) {
  grid <- field_book_export_grid(plot$data, value)
  rows <- rev(seq_len(nrow(grid)))
  data.frame(ROW = rows, grid[rows, , drop = FALSE], row.names = NULL, check.names = FALSE)
}

#' The same three download buttons for every plot
#' @noRd
app_plot_tools <- function(ns, id, csv_id = paste0(id, "_csv")) {
  shiny::div(class = "fieldhub-layout-downloads", role = "group", `aria-label` = "Plot downloads",
    shiny::downloadButton(ns(paste0(id, "_png")), "PNG (300 dpi)"),
    shiny::downloadButton(ns(paste0(id, "_pdf")), "PDF (vector)"),
    shiny::downloadButton(ns(csv_id), "Layout CSV"))
}

#' Shared image renderer and downloads for the currently selected plot
#' @noRd
app_plot_outputs <- function(input, output, session, panel, plot, design, location, ready,
                               csv_data, kind = function() panel$id, csv_id = paste0(panel$id, "_csv")) {
  id <- panel$id
  filename <- function(extension) {
    sub("[.]csv$", paste0(".", extension), csv_export_filename(design(), kind(), location()))
  }
  output[[id]] <- shiny::renderImage({
    current <- plot()
    size <- app_spatial_preview_size(input[[paste0(id, "_size")]])
    shiny::req(current, size)
    file <- tempfile(fileext = ".png")
    # Shiny removes successful temporary images after sending them; remove failures too.
    tryCatch({
      size <- validate_design(app_spatial_plot_file(app_plot_preview(current, size), file,
                                                    dpi = 192, size = size))
      list(src = file, contentType = "image/png", width = size$width, height = size$height,
           alt = paste(panel$title, "for location", location()))
    }, error = function(e) {
      unlink(file)
      stop(e)
    })
  }, deleteFile = TRUE)
  for (format in c("png", "pdf")) local({
    format <- format
    output[[paste0(id, "_", format)]] <- shiny::downloadHandler(
      filename = function() filename(format),
      contentType = if (format == "png") "image/png" else "application/pdf",
      content = function(file) {
        shiny::req(ready())
        validate_design(app_spatial_plot_file(plot(), file, format))
      }
    )
  })
  output[[csv_id]] <- app_csv_download(
    filename = function() filename("csv"),
    data = function() {
      shiny::req(ready())
      validate_design(csv_data())
    }
  )
}

#' Spatial layouts supply their selected-location CSV to the shared renderer
#' @noRd
app_spatial_plot_outputs <- function(input, output, session, panel, plot, design, location, values, ready) {
  app_plot_outputs(input, output, session, panel, plot, design, location, ready,
    csv_data = function() {
      if (is.null(panel$grid)) app_plot_grid_data(plot(), panel$label)
      else app_spatial_plot_data(panel, design(), location(), values())
    })
}
