#' Original, design-specific input-panel presentation
#'
#' Rows follow the pre-refactor module UIs (v1.5.0), with the repeated-check
#' and filler controls retained from the modules immediately before migration.
#' Presentation order is independent of the controls' parser dependency order.
#' @noRd
app_sidebar_layout <- function(spec) {
  classic_end <- list("planter", c("plot_start", "location_names"), "seed")
  spatial_end <- list(c("plot_start", "expt_name"), c("seed", "location_names"))
  location_row <- list(c("l", "location_view"))
  rows <- switch(spec$module,
    CRD = c(list("t", "upload_file", "reps"), classic_end),
    RCBD = list("t", "upload_file", "reps", "use_checks", c("checks", "rep_checks"),
      "spread_checks", "checks_note", "l", "planter", c("plot_start", "continuous"),
      "location_names", "seed"),
    LSD = c(list("upload_file", "t", "reps"), classic_end),
    # Latin rectangles were introduced after the original modules.
    Latin_Rectangle = c(list("upload_file", "t", "nrows", "l"), classic_end),
    FD = list("type", "setfactors", "upload_file", c("reps", "l"),
      c("plot_start", "location_names"), "planter", "seed"),
    SPD = c(list("type", "upload_file", "wp", "sp", c("reps", "l")), classic_end),
    SSPD = c(list("type", "upload_file", "wp", "sp", "ssp", c("reps", "l")), classic_end),
    STRIPD = c(list("upload_file", c("Hplots", "Vplots"), "reps", "l"),
      classic_end[1:2], list("randomizeH", "randomizeV", "seed")),
    IBD =, Alpha_Lattice =, Square_Lattice =, Rectangular_Lattice =
      c(list("t", "upload_file", "reps", "k", "l"), classic_end),
    RowCol = c(list("upload_file", "t", c("nrows", "reps"), "l"), classic_end),
    Optim = c(list("upload_file", "checks", "rep_checks", "lines", "planter"),
      location_row, spatial_end),
    pREPS = c(list("upload_file", "repGens", "repUnits", "allow_fillers"),
      location_row, list("planter"), spatial_end),
    RCBD_augmented = c(list("upload_file", c("repsExpt", "random"), "random_note",
      "repsStack", "lines", c("checks", "b")), location_row, list("planter"), spatial_end),
    Diagonal = c(list("upload_file", "lines", "checks"), location_row, list("planter"), spatial_end),
    diagonal_multiple = c(list("sameEntries", "upload_file", "lines", "blocks", "checks"),
      location_row, list(c("stacked", "planter")), spatial_end),
    sparse_allocation = c(list("upload_file", "lines", "checks"), location_row,
      list("copies_per_entry", "planter"), spatial_end),
    multi_loc_preps = c(list("upload_file", "lines", "use_checks", c("checks", "rep_checks")),
      location_row, list("copies_per_entry", "planter", "allow_fillers"), spatial_end),
    c(list("upload_file"), as.list(vapply(spec$controls, `[[`, "", "id")))
  )
  if (is.null(spec$upload)) rows <- Filter(function(row) !identical(row, "upload_file"), rows)
  list(rows = rows, gutters = !spec$module %in% c("pREPS", "multi_loc_preps"),
    upload_width = if (identical(spec$kind, "classic") && spec$module != "CRD") 8L else 7L,
    upload_label = switch(spec$module, Optim = "Import Entries' List?",
      SSPD = "Do you have your own data?", "Import entries' list?"),
    file_label = if (spec$module %in% c("RowCol", "SPD", "SSPD", "STRIPD"))
      "Upload a csv File:" else "Upload a CSV File:")
}

#' Restore visible labels and widget types without changing validation or defaults
#' @noRd
app_sidebar_control <- function(control, module) {
  labels <- c(seed = "Random Seed:", plot_start = "Starting Plot Number:",
    location_names = "Input Location:", expt_name = "Input Experiment Name:")
  if (module == "RCBD") labels["plot_start"] <- "Starting Plot Number(s):"
  if (module == "LSD") labels["reps"] <- "Input # of Full Reps (Squares):"
  if (module == "FD") labels <- c(labels, type = "Select a Factorial Design Type:",
    setfactors = "Input # of Entries for Each Factor: (Separated by Comma)")
  if (module %in% c("SPD", "SSPD")) labels <- c(labels,
    type = paste0("Select ", module, " Type:"), wp = "Whole-plots:",
    sp = "Sub-plots Within Whole-plots:", ssp = "Sub-Sub-plots within Sub-plots:")
  if (module %in% c("SPD", "Diagonal", "diagonal_multiple", "sparse_allocation")) {
    labels["location_names"] <- "Input the Location:"
  }
  if (module %in% c("pREPS", "multi_loc_preps")) labels["location_names"] <- "Input Location Name:"
  if (module %in% c("Optim", "RCBD_augmented", "Diagonal", "diagonal_multiple")) {
    labels["location_view"] <- "Choose location to view:"
  }
  if (module %in% c("Optim", "multi_loc_preps")) labels["rep_checks"] <- "Input # Check's Reps:"
  if (module == "multi_loc_preps") labels["use_checks"] <- "Include checks?"
  if (module == "RCBD_augmented") labels["checks"] <- "Checks per Block:"
  if (control$id %in% names(labels)) control$label <- unname(labels[control$id])
  control
}

#' Render the original rows with Bootstrap's original six-column widths
#' @noRd
app_sidebar_ui <- function(spec, ns, toggle = NULL) {
  layout <- app_sidebar_layout(spec)
  controls <- stats::setNames(spec$controls, vapply(spec$controls, `[[`, "", "id"))
  render <- function(id) app_control_ui(app_sidebar_control(controls[[id]], spec$module), ns, toggle)
  shiny::div(class = "fieldhub-sidebar-controls",
    if (!is.null(spec$upload)) app_upload_ui(ns, spec$upload, part = "toggle",
      toggle_label = layout$upload_label),
    lapply(layout$rows, function(row) {
      if (identical(row, "upload_file")) return(app_upload_ui(ns, spec$upload, part = "file",
        file_width = layout$upload_width, gutters = layout$gutters,
        file_label = layout$file_label, separator_gutter = spec$module != "sparse_allocation"))
      if (length(row) == 1L) return(render(row))
      shiny::fluidRow(class = "fieldhub-input-row",
        shiny::column(6, class = if (layout$gutters) "fieldhub-input-left", render(row[1L])),
        shiny::column(6, class = if (layout$gutters) "fieldhub-input-right", render(row[2L])))
    }))
}
