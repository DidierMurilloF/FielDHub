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
    CRD = list("upload_file", c("t", "reps"), c("planter", "plot_start"), c("location_names", "seed")),
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
    pREPS = c(list("upload_file", c("repGens", "repUnits"), "allow_fillers", "rowcol_distinct"),
      location_row, list("planter"), spatial_end),
    RCBD_augmented = c(list("upload_file", c("repsExpt", "random"), "random_note",
      "repsStack", "lines", c("checks", "b")), location_row,
      list("planter", "plot_by_block", "plot_increment"), spatial_end),
    # The field and what goes in it in pairs; the search settings folded
    # into one group at the end. The year is not asked: the design takes
    # the current one, as it does for a blank year
    Augmented_RowCol = c(list("upload_file", c("nrows", "ncols"), c("lines", "checks"),
      "check_direction", "objective"), location_row, list("planter"), spatial_end,
      list(c("advanced", "budget", "time_limit"))),
    Diagonal = c(list("upload_file", "lines", "checks", "spread_checks", "dist_method"),
      location_row, list("planter"), spatial_end),
    diagonal_multiple = c(list("sameEntries", "upload_file", "lines", "blocks", "checks",
      "spread_checks", "dist_method"), location_row, list(c("stacked", "planter")), spatial_end),
    sparse_allocation = c(list("upload_file", "lines", "checks", "spread_checks", "dist_method"), location_row,
      list("copies_per_entry", "planter"), spatial_end),
    multi_loc_preps = c(list("upload_file", "lines", "use_checks", c("checks", "rep_checks")),
      location_row, list(c("copies_per_entry", "planter"), "allow_fillers", "rowcol_distinct"), spatial_end),
    c(list("upload_file"), as.list(vapply(spec$controls, `[[`, "", "id")))
  )
  if (is.null(spec$upload)) rows <- Filter(function(row) !identical(row, "upload_file"), rows)
  list(rows = rows, omitted = if (spec$module == "Augmented_RowCol") "year",
    gutters = !spec$module %in% c("pREPS", "multi_loc_preps"),
    upload_width = if (identical(spec$kind, "classic") && spec$module != "CRD") 8L else 7L,
    upload_label = "Import entries list", file_label = "CSV file")
}

#' The control as the sidebar shows it
#'
#' @description A control's label is the page's own (\code{design_app_spec()}),
#' the same in its field and in its messages, so there is nothing to change.
#' @noRd
app_sidebar_control <- function(control, module) {
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
      # More than a pair is a group that its first control (a flag) opens
      if (length(row) > 2L) return(shiny::div(class = "fh-advanced",
        shiny::span(class = "fh-tip fh-info", tabindex = "0", role = "img", `data-fh-tip` = NA,
          title = "How long the search for the design may run. The defaults suit most trials.",
          `aria-label` = "How long the search for the design may run. The defaults suit most trials.",
          shiny::icon("circle-info", verify_fa = FALSE)),
        lapply(row, render)))
      shiny::fluidRow(class = "fieldhub-input-row",
        shiny::column(6, class = if (layout$gutters) "fieldhub-input-left", render(row[1L])),
        shiny::column(6, class = if (layout$gutters) "fieldhub-input-right", render(row[2L])))
    }))
}
