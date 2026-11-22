#' Registry-driven classic layout controls without a reactive session
#'
#' @description The plot type, and the stacking, layout and location of the
#' design, as a toolbar on the plot (\code{app_layout_toolbar()}).
#' @noRd
app_classic_layout_panel <- function(ns, spec, x, planter = "serpentine", locations = NULL) {
  layout <- spec$layout
  # The menu holds plain selects: a selectize list would open a menu inside the menu
  select <- function(...) shiny::selectInput(..., selectize = FALSE)
  choices <- layout_choice_labels(x, planter = planter, stacked = "vertical")
  controls <- list(shiny::radioButtons(ns(spec$ids[["plot_type"]]), "Type of plot",
    c("Entries" = 1, "Plots" = 2, "Heatmap" = 3), inline = TRUE))
  if ("stacked" %in% names(layout$ids)) {
    stacking <- classic_stacking_choices(length(levels(as.factor(x$fieldBook$REP))), grid = layout$grid)
    controls <- c(controls, list(select(ns(layout$ids[["stacked"]]), "Reps layout", stacking)))
  }
  controls <- c(controls, list(select(ns(layout$ids[["layout"]]), "Layout option", choices,
    selected = if (layout$select_layout) 1 else NULL)))
  if ("location" %in% names(layout$ids)) {
    if (is.null(locations)) locations <- seq_len(length(levels(as.factor(x$fieldBook$LOCATION))))
    location <- select(ns(layout$ids[["location"]]), "Location", locations,
      selected = if (layout$select_location) 1 else NULL)
    # One location is nothing to choose from: the input stays (the server
    # reads it) but is not shown
    if (length(locations) < 2L) location <- shiny::div(hidden = NA, location)
    controls <- c(controls, list(location))
  }
  # The menu is worth a button while it offers a choice
  useful <- "stacked" %in% names(layout$ids) || length(choices) > 1L || length(locations) > 1L
  app_layout_toolbar(ns(paste0(layout$output, "_menu")), controls, menu = useful)
}

#' The view controls on the plot itself
#'
#' @description A segmented control for the plot type, and a button that
#' opens a small menu with the layout, stacking and location selects
#' (layout-menu.js opens and closes it). The inputs keep their ids.
#' @param menu_id The id of the menu, for \code{aria-controls}.
#' @param controls The plot type control, then the controls of the menu.
#' @param menu Whether the menu offers a choice; its inputs stay on the
#'   page either way.
#' @noRd
app_layout_toolbar <- function(menu_id, controls, menu = TRUE) {
  shiny::div(class = "fh-view-bar",
    shiny::div(class = "fh-segmented", controls[[1L]]),
    shiny::div(class = "fh-view-menu", hidden = if (!menu) NA,
      shiny::tags$button(type = "button", class = "btn fh-view-menu-toggle",
        `aria-haspopup` = "true", `aria-expanded` = "false", `aria-controls` = menu_id,
        shiny::icon("sliders", verify_fa = FALSE), shiny::span("Layout options"),
        shiny::icon("chevron-down", class = "fh-view-menu-caret", verify_fa = FALSE)),
      shiny::div(id = menu_id, class = "fh-view-menu-panel", role = "group",
        `aria-label` = "Layout options", hidden = NA, controls[-1L])))
}

#' Shared panel and selection lifecycle for classic design modules
#' @noRd
app_classic_layout <- function(input, output, session, design, planter, spec,
                                locations = function() NULL) {
  output[[spec$layout$output]] <- shiny::renderUI({
    # A design that failed says so where the plot would be, not here too
    state <- app_design_state(design)
    shiny::req(!inherits(state, "condition"), state$fieldBook)
    validate_design(app_classic_layout_panel(session$ns, spec, design(),
      planter = if (spec$layout$current_planter) planter() else "serpentine", locations = locations()))
  })
  # The field book and the summary read the selection before the plot is seen
  shiny::outputOptions(output, spec$layout$output, suspendWhenHidden = FALSE)
  app_layout_selection(input, session, design = design, planter = planter, ids = spec$layout$ids)
}
