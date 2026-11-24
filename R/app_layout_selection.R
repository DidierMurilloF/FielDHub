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
      choices <- validate_design(layout_choice_labels(design(), planter = planter(),
                                                      stacked = stacking, include_grids = TRUE),
                                 report = TRUE)
      first <- if (length(choices)) app_first_layout_choice(choices) else 1
      reset(first)
      shiny::updateSelectInput(session, inputId = ids[["layout"]], label = "Field dimensions",
                               choices = choices, selected = first)
    })
    shiny::observeEvent(input[[ids[["layout"]]]], reset(NULL))
  }
  shiny::reactive({
    selected <- input[[ids[["layout"]]]]
    stacking <- if ("stacked" %in% names(ids)) input[[ids[["stacked"]]]] else "vertical"
    location <- if ("location" %in% names(ids)) input[[ids[["location"]]]] else 1
    shiny::req(selected, stacking, location, design(), planter())
    selected <- if (!is.null(reset())) reset() else selected
    validate_design(app_selected_layout_view(design(), selected, planter(),
                                              suppressWarnings(as.numeric(location)), stacking))
  })
}

app_selected_layout_view <- function(design, selected, planter, location, stacking) {
  grid <- regmatches(selected, regexec("^grid:([a-z_]+):([0-9]+):([0-9]+)$", selected))[[1L]]
  if (length(grid)) {
    # A grid is drawn with its own stacking, the one its name states, not
    # with that of the menu that lists it; a design without replicates has
    # one field for its dimensions ("none")
    return(checked_layout_view(design, planter = planter, location = location,
                               stacked = if (!identical(grid[2L], "none")) grid[2L],
                               nrows = as.numeric(grid[3L]), ncols = as.numeric(grid[4L])))
  }
  checked_layout_view(design, layout = suppressWarnings(as.numeric(selected)),
                      planter = planter, location = location, stacked = stacking)
}

#' The first option of a layout menu: the one a page or a stacking opens on
#' @param choices The value of \code{layout_choice_labels()}.
#' @noRd
app_first_layout_choice <- function(choices) {
  if (!length(choices)) return(NULL)
  choices[[1L]]
}
