#' Shared classic layout-selection lifecycle
#'
#' Keep module-specific input identifiers and the existing reset-to-first
#' behavior when stacking changes. Coordinates and rendering remain core work.
#' @noRd
app_layout_selection <- function(input, session, design, planter, ids) {
  if (!is.character(ids) || !is.null(dim(ids)) || is.null(names(ids)) ||
      !"layout" %in% names(ids) || !all(names(ids) %in% c("layout", "stacked", "location")) ||
      anyNA(ids) || anyDuplicated(ids) || anyDuplicated(names(ids)) ||
      !all(grepl("^[A-Za-z][A-Za-z0-9_.]*$", ids))) {
    fieldhub_abort("Layout controls need distinct named layout, stacking, and location identifiers.")
  }
  # The layout a new stacking opens on (its first choice), until the select says otherwise
  reset <- shiny::reactiveVal(NULL)
  if ("stacked" %in% names(ids)) {
    shiny::observeEvent(input[[ids[["stacked"]]]], {
      stacking <- input[[ids[["stacked"]]]]
      shiny::req(stacking, design(), planter())
      choices <- validate_design(layout_choice_labels(design(), planter = planter(), stacked = stacking), report = TRUE)
      first <- if (length(choices)) choices[[1L]] else 1
      reset(first)
      shiny::updateSelectInput(session, inputId = ids[["layout"]], label = "Layout option",
                               choices = choices, selected = first)
    })
    shiny::observeEvent(input[[ids[["layout"]]]], reset(NULL))
  }
  shiny::reactive({
    selected <- input[[ids[["layout"]]]]
    stacking <- if ("stacked" %in% names(ids)) input[[ids[["stacked"]]]] else "vertical"
    location <- if ("location" %in% names(ids)) input[[ids[["location"]]]] else 1
    shiny::req(selected, stacking, location, design(), planter())
    validate_design(checked_layout_view(
      design(), layout = if (!is.null(reset())) reset() else suppressWarnings(as.numeric(selected)),
      planter = planter(), location = suppressWarnings(as.numeric(location)), stacked = stacking
    ))
  })
}
