test_that("every design layout and heatmap uses the shared image panel", {
  for (package in app_dependencies()) skip_if_not_installed(package)
  for (spec in fieldhub_design_specs()) {
    expect_true(all(vapply(spec$panels, function(panel) identical(panel$type, "image"), TRUE)))
    ui <- htmltools::tagQuery(mod_design_ui("page", spec))
    ids <- if (spec$kind == "classic") spec$workflow$ids[["plot"]] else {
      c(vapply(spec$panels, `[[`, "", "id"), spec$workflow$ids[["heatmap"]])
    }
    images <- ui$find(".shiny-image-output")
    output_ids <- vapply(images$selectedTags(), function(tag) tag$attribs$id, "")
    expect_setequal(output_ids, paste0("page-", ids))
    expect_equal(images$parents(".fieldhub-task-region")$length(), length(ids))
    expect_equal(ui$find(".fieldhub-layout-image")$length(), length(ids))
    expect_equal(ui$find(".fieldhub-plot-panel")$length(), length(ids))
    expect_equal(ui$find(".fieldhub-plot-toolbar")$length(), length(ids))
    for (id in ids) {
      toolbars <- ui$find(".fieldhub-plot-toolbar")$selectedTags()
      tag <- Filter(function(tag) identical(tag$attribs$id, paste0("page-", id, "_tools")), toolbars)
      expect_length(tag, 1L)
      toolbar <- htmltools::tagQuery(tag[[1L]])
      expect_equal(toolbar$find(".shiny-download-link")$length(), 3L)
      expect_equal(toolbar$find(".shiny-html-output")$length(), 0L)
      expect_identical(toolbar$selectedTags()[[1L]]$attribs[["aria-hidden"]], "true")
      csv_id <- if (spec$kind == "classic") spec$workflow$ids[["layout_download"]] else paste0(id, "_csv")
      buttons <- toolbar$find(".shiny-download-link")$selectedTags()
      expect_identical(unname(vapply(buttons, function(button) button$attribs$id, "")),
        paste0("page-", c(paste0(id, "_png"), paste0(id, "_pdf"), csv_id)))
    }
    expect_equal(ui$find(".plotly")$length(), 0L)
    expect_false(grepl("scroll to inspect", as.character(mod_design_ui("page", spec)), fixed = TRUE))
    if (spec$kind == "spatial") expect_equal(ui$find("#page-entries_1.datatables")$length(), 1L)
  }
  expect_false("plotly" %in% app_dependencies())
})

test_that("plot loading does not rebuild or collapse the download toolbar", {
  expect_false("renderUI" %in% all.names(body(app_plot_outputs)))
  css <- paste(readLines(system.file("app/www/style.css", package = "FielDHub")), collapse = "\n")
  expect_match(css, "[.]fieldhub-layout-page\\s*\\{\\s*scrollbar-gutter: stable;")
  expect_match(css, "[.]fieldhub-layout-page > body\\s*\\{\\s*padding-right: 0 !important;")
  expect_match(css, "[.]fieldhub-plot-toolbar\\s*\\{\\s*display: flow-root;\\s*visibility: hidden;")
  expect_match(css, "[.]has-plot > [.]fieldhub-plot-toolbar\\s*\\{\\s*visibility: visible;")
  js <- paste(readLines(system.file("app/www/layout-images.js", package = "FielDHub")), collapse = "\n")
  expect_match(js, 'pending[image.id] !== key', fixed = TRUE)
  expect_match(js, '"shiny:value shiny:error shiny:recalculating", plotState', fixed = TRUE)
  expect_false(grepl('shiny:value[^\\n]*scheduleFit', js))
})

test_that("layout previews fill a wide, bounded drawing area without internal scrolling", {
  css <- paste(readLines(system.file("app/www/style.css", package = "FielDHub")), collapse = "\n")
  rules <- function(selector) {
    pattern <- paste0("#fieldhub-app ", selector, " \\{[^}]*\\}")
    regmatches(css, regexpr(pattern, css))
  }
  container <- rules("[.]fieldhub-layout-image")
  image <- rules("[.]fieldhub-layout-image img")
  expect_match(container, "width: 98%;", fixed = TRUE)
  expect_match(container, "height: var(--fieldhub-preview-height, 520px);", fixed = TRUE)
  expect_match(container, "margin: 0 auto;", fixed = TRUE)
  expect_false(grepl("overflow:|min-height:", container))
  toolbar <- rules("[.]fieldhub-layout-downloads")
  for (declaration in c("justify-content: flex-end;", "width: 98%;", "flex-wrap: wrap;")) {
    expect_match(toolbar, declaration, fixed = TRUE)
  }
  for (declaration in c("width: 100%;", "height: 100%;", "object-fit: contain;", "margin: 0 auto;")) {
    expect_match(image, declaration, fixed = TRUE)
  }
  expect_match(paste(deparse(body(golem_add_external_resources)), collapse = " "),
    "layout-images.js", fixed = TRUE)
})

test_that("plot toolbars have exactly three consistent namespaced downloads", {
  for (csv_id in c("layout_csv", "downloadCsv.rcbd")) {
    tools <- htmltools::tagQuery(app_plot_tools(shiny::NS("page"), "layout", csv_id))
    buttons <- tools$find("a.shiny-download-link")$selectedTags()
    expect_length(buttons, 3L)
    expect_identical(unname(vapply(buttons, function(x) x$attribs$id, "")),
      paste0("page-", c("layout_png", "layout_pdf", csv_id)))
    html <- as.character(app_plot_tools(shiny::NS("page"), "layout", csv_id))
    for (label in c("PNG (300 dpi)", "PDF (vector)", "Layout CSV")) {
      expect_match(html, label, fixed = TRUE)
    }
  }
})

test_that("augmented RCBD images and CSVs match the selected native plot", {
  design <- catalogue_design("RCBD_augmented_two_locations")
  for (location in 1:2) {
    view <- checked_layout_view(design, location = location)
    for (panel in design_app_spec("RCBD_augmented")$panels) {
      plot <- panel$view(design, location, list())
      expected <- if (panel$label == "PLOT") view$out_layoutPlots else view$out_layout
      expect_equal(ggplot2::ggplot_build(plot)$data, ggplot2::ggplot_build(expected)$data)
      csv <- app_plot_grid_data(plot, panel$label)
      cells <- match(paste(rep(csv$ROW, ncol(csv) - 1L),
                          rep(seq_len(ncol(csv) - 1L), each = nrow(csv))),
                     paste(plot$data$ROW, plot$data$COLUMN))
      expect_false(anyNA(cells))
      expect_equal(as.character(as.matrix(csv[, -1L])), as.character(plot$data[[panel$label]][cells]))
    }
  }
})

test_that("heatmap images and CSVs keep the response at its field coordinates", {
  book <- data.frame(LOCATION = rep(c("West", "East"), each = 4),
    ROW = rep(c(1, 1, 2, 2), 2), COLUMN = rep(c(1, 2, 1, 2), 2),
    TREATMENT = letters[1:8], YIELD = c(1:7, NA_real_))
  plot <- app_field_heatmap(book, "YIELD", selected = 2)
  expect_s3_class(plot, "ggplot")
  expect_equal(plot$data$YIELD, book$YIELD[5:8])
  csv <- app_plot_grid_data(plot, "YIELD")
  expect_identical(csv$ROW, 2:1)
  expect_equal(unname(as.matrix(csv[, -1L])), matrix(c(7, 5, NA, 6), nrow = 2))
  expect_identical(app_spatial_plot_size(plot), list(width = 800, height = 560))
  spatial <- app_spatial_heatmap(list(plot$data), "YIELD")
  expect_s3_class(spatial, "ggplot")
  expect_equal(app_plot_grid_data(spatial, "YIELD"), csv)
  expect_warning(ggplot2::ggplot_build(spatial), NA)
})

test_that("dense preview labels fit without mutating native plots or download text", {
  design <- RCBD_augmented(lines = 180, checks = 4, b = 3, seed = 27)
  plot <- checked_layout_view(design)$out_layout
  original <- ggplot2::ggplot_build(plot)$data
  preview <- app_plot_preview(plot, list(width = 800, height = 500))
  fitted <- ggplot2::ggplot_build(preview)$data
  text <- which(vapply(original, function(layer) "label" %in% names(layer), TRUE))
  expect_length(text, 1L)
  expect_lt(max(fitted[[text]]$size), max(original[[text]]$size))
  expect_gte(min(fitted[[text]]$size), 1.1)
  expect_equal(fitted[[text]]$label, original[[text]]$label)
  expect_equal(fitted[[text]]$x, original[[text]]$x)
  expect_equal(fitted[[text]]$y, original[[text]]$y)
  expect_equal(fitted[[text]]$colour, original[[text]]$colour)
  expect_equal(ggplot2::ggplot_build(plot)$data, original)
  expect_lt(preview$theme$axis.text.x$size, 10)
})

test_that("preview dimensions are measured independently of the field's shape", {
  expect_identical(app_spatial_preview_size(list(width = 930, height = 540)),
                   list(width = 930, height = 540))
  expect_identical(app_spatial_preview_size(list(width = 930.8, height = 540.2)),
                   list(width = 930, height = 540))
  expect_identical(app_spatial_preview_size(list(width = 5000, height = 2500)),
                   list(width = 1800, height = 900))
  expect_null(app_spatial_preview_size(NULL))
  expect_null(app_spatial_preview_size(c(width = 930, height = 540)))
  for (value in list(NULL, 0, -1, NA_real_, Inf, "800", c(1, 2), matrix(1), 1 + 1i)) {
    expect_null(app_spatial_preview_size(list(width = value, height = 540)))
    expect_null(app_spatial_preview_size(list(width = 930, height = value)))
  }
})

test_that("all migrated images match plot() and CSV cells without mutating designs", {
  designs <- list(
    pREPS = partially_replicated(nrows = c(6, 7), ncols = c(7, 6), l = 2,
      repGens = c(5, 31), repUnits = c(2, 1), allow_fillers = TRUE,
      plotNumber = c(101, 1001), locationNames = c("West", "East"), seed = 16),
    multi_loc_preps = catalogue_design("multi_location_prep"),
    Optim = catalogue_design("optimized_arrangement"),
    Diagonal = diagonal_arrangement(nrows = 18, ncols = 18, lines = 287, checks = 4,
      l = 2, plotNumber = c(101, 1001), locationNames = c("West", "East"), seed = 1),
    diagonal_multiple = catalogue_design("diagonal_blocks_row"),
    sparse_allocation = catalogue_design("sparse_allocation")
  )
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (module in names(designs)) {
    design <- designs[[module]]
    before <- serialize(design, NULL)
    set.seed(93)
    rng <- .Random.seed
    panels <- design_app_spec(module)$panels
    for (location in seq_along(field_book_locations(design$fieldBook))) {
      book <- location_books(design)[[location]]
      for (i in seq_along(panels)) {
        panel <- panels[[i]]
        plot <- panel$view(design, location, list())
        expect_s3_class(plot, "ggplot")
        label <- switch(panel$id, field_layout = "ENTRY", plot_numbers = "PLOT", expt_layout = "EXPT")
        expected <- book[[label]]
        if (label == "ENTRY") {
          expected <- ifelse(book$TREATMENT == "Filler", "Filler", book$ENTRY)
        }
        text <- Filter(function(layer) "label" %in% names(layer) && any(layer$size > 0),
                       ggplot2::ggplot_build(plot)$data)[[1L]]
        expect_identical(as.character(text$label), as.character(expected))
        expect_equal(as.numeric(text$x), as.numeric(book$COLUMN))
        expect_equal(as.numeric(text$y), as.numeric(book$ROW))
        public <- plot(design, l = location, text.string = label)$p
        expect_equal(ggplot2::ggplot_build(plot)$data, ggplot2::ggplot_build(public)$data)
        csv <- app_spatial_plot_data(panel, design, location, list())
        old <- panel$grid(design, location, list())$data
        expect_identical(csv$ROW, as.integer(rownames(old)))
        expect_equal(as.matrix(csv[, -1, drop = FALSE]), unname(as.matrix(old)), ignore_attr = TRUE)
        cells <- match(paste(rep(csv$ROW, ncol(csv) - 1L),
                              rep(seq_len(ncol(csv) - 1L), each = nrow(csv))),
                        paste(book$ROW, book$COLUMN))
        expect_false(anyNA(cells))
        expect_equal(as.character(as.matrix(csv[, -1L, drop = FALSE])), as.character(expected[cells]))
      }
    }
    expect_identical(serialize(design, NULL), before)
    expect_identical(.Random.seed, rng)
  }
})

test_that("image sizes grow with the field but are bounded", {
  small <- list(data = data.frame(ROW = 1:5, COLUMN = 1:5))
  wide <- list(data = data.frame(ROW = 1:5, COLUMN = 36:40))
  tall <- list(data = data.frame(ROW = 36:40, COLUMN = 1:5))
  huge <- list(data = data.frame(ROW = 10000, COLUMN = 10000))
  expect_identical(app_spatial_plot_size(small), list(width = 800, height = 560))
  expect_gt(app_spatial_plot_size(wide)$width, app_spatial_plot_size(small)$width)
  expect_gt(app_spatial_plot_size(tall)$height, app_spatial_plot_size(small)$height)
  expect_identical(app_spatial_plot_size(huge), list(width = 1800, height = 1800))
})

test_that("preview PNGs use 192 dpi and downloads retain quality", {
  skip_if_not(capabilities("png"))
  design <- catalogue_design("partially_replicated_fillers")
  plot <- design_app_spec("pREPS")$panels[[1]]$view(design, 1, list())
  file <- tempfile(fileext = ".png")
  pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(file, pdf)), add = TRUE)
  size <- app_spatial_plot_file(plot, file, dpi = 192)
  png_size <- function(file) {
    con <- file(file, "rb")
    on.exit(close(con))
    readBin(con, "raw", n = 16L)
    readBin(con, "integer", n = 2L, size = 4L, endian = "big")
  }
  expect_equal(png_size(file), unlist(size, use.names = FALSE) * 2)
  preview <- app_spatial_preview_size(list(width = 930, height = 540))
  app_spatial_plot_file(plot, file, dpi = 192, size = preview)
  expect_equal(png_size(file), c(1860, 1080))
  app_spatial_plot_file(plot, file)
  expect_equal(png_size(file), floor(unlist(size, use.names = FALSE) * 300 / 96))
  app_spatial_plot_file(plot, pdf, "pdf")
  expect_identical(readChar(pdf, 4L, useBytes = TRUE), "%PDF")
})
