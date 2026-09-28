#' One label, default and minimum per control concept
#'
#' @description Every design page builds its sidebar from the constructors
#' below, and every constructor takes its label (and, where the concept has
#' one, its default and minimum) from this table, so the same concept reads
#' the same on every page. The input id of a control is its concept, which
#' is also the name its value has in the \code{values} the argument
#' builders read (\code{design_args_<Module>()}).
#'
#' Minimums are the smallest value the engines accept (the engines
#' validate; a minimum is only a hint in the browser). A page overrides the
#' minimum only where its engine accepts a smaller value (see
#' \code{design_app_spec()}). Counts that describe a design (treatments,
#' replicates, whole plots, ...) keep their design-specific defaults.
#' @noRd
fieldhub_control_concepts <- function() {
  list(
    type = list(label = "Select Design Type:"),
    t = list(label = "Input # of Treatments:", min = 2),
    setfactors = list(label = "Input # of Entries for Each Factor (comma separated):"),
    wp = list(label = "Input # of Whole Plots:", min = 2),
    sp = list(label = "Input # of Sub-plots Within Whole Plots:", min = 2),
    ssp = list(label = "Input # of Sub-sub-plots Within Sub-plots:", min = 2),
    Hplots = list(label = "Input # of Horizontal Strips:", min = 2),
    Vplots = list(label = "Input # of Vertical Strips:", min = 2),
    reps = list(label = "Input # of Full Reps:", min = 2),
    k = list(label = "Input # of Plots per IBlock:"),
    nrows = list(label = "Input # of Rows:"),
    use_checks = list(label = "Add repeated checks?", value = FALSE),
    checks = list(label = "Input # of Checks:", min = 1),
    rep_checks = list(label = "Reps per Check:", value = "2"),
    spread_checks = list(label = "Spread checks within each block", value = TRUE),
    checks_note = list(label = NULL),
    l = list(label = "Input # of Locations:", value = 1, min = 1),
    planter = list(label = "Plot Order Layout:", value = "serpentine",
                   choices = c("serpentine", "cartesian")),
    plot_start = list(label = "Starting Plot Number(s):", value = "101"),
    continuous = list(label = "Continuous Plot", value = TRUE),
    location_names = list(label = "Location Name(s):", value = "FARGO"),
    randomizeH = list(label = "Randomize Horizontal Strips (Across reps)", value = TRUE),
    randomizeV = list(label = "Randomize Vertical Strips (Across reps)", value = TRUE),
    seed = list(label = "Random Seed (blank = automatic):", value = NULL)
  )
}

#' A control of the design sidebar
#'
#' @description The one constructor behind every \code{ctl_*()} helper. The
#' label comes from \code{fieldhub_control_concepts()}; \code{parse} is a
#' plain function \code{function(value, values)} (see
#' \code{read_design_controls()}).
#' @noRd
design_control <- function(id, type, parse, value = NULL, min = NULL, max = NULL,
                           choices = NULL, generated_only = FALSE, show_if = NULL,
                           enabled_by = NULL, disabled_value = NULL,
                           depends_on = NULL, options = NULL, preview = NULL) {
  concept <- fieldhub_control_concepts()[[id]]
  if (is.null(concept)) fieldhub_abort("Unknown control concept: ", id, class = "fieldhub_internal_error")
  list(id = id, type = type, label = concept$label, value = value, min = min, max = max,
       choices = choices, parse = parse, generated_only = generated_only, show_if = show_if,
       enabled_by = enabled_by, disabled_value = disabled_value, depends_on = depends_on,
       options = options, preview = preview)
}

#' Default of a concept, unless the design overrides it
#' @noRd
concept_value <- function(id, value = NULL) {
  if (!is.null(value)) return(value)
  fieldhub_control_concepts()[[id]]$value
}

#' Minimum of a concept, unless the design's engine accepts a smaller one
#' @noRd
concept_min <- function(id, min = NULL) {
  if (!is.null(min)) return(min)
  fieldhub_control_concepts()[[id]]$min
}

#' A whole-number count (treatments, whole plots, strips, checks, ...)
#' @param id The concept.
#' @param value The design's default.
#' @param min Only where the engine accepts less than the concept's minimum.
#' @param max Only where the engine accepts no more.
#' @param generated_only Whether the count is not read when entries are
#'   uploaded (the file gives it).
#' @param parse Parser \code{function(value, label)}.
#' @noRd
ctl_count <- function(id, value, min = NULL, max = NULL, generated_only = FALSE,
                      parse = parse_control_number, ...) {
  label <- fieldhub_control_concepts()[[id]]$label
  design_control(id, "number", value = value, min = concept_min(id, min), max = max,
                 generated_only = generated_only, ...,
                 parse = function(value, values) parse(value, label))
}

#' Number of full replicates
#' @noRd
ctl_reps <- function(value, min = NULL) ctl_count("reps", value, min = min)

#' Number of locations
#' @noRd
ctl_locations <- function() ctl_count("l", concept_value("l"))

#' Location names, comma separated
#' @noRd
ctl_location_names <- function() {
  label <- fieldhub_control_concepts()$location_names$label
  design_control("location_names", "text", value = concept_value("location_names"),
                 parse = function(value, values) parse_control_names(value, label))
}

#' Starting plot number(s), comma separated
#' @noRd
ctl_plot_start <- function() ctl_text_list("plot_start")

#' Plot order (serpentine or cartesian)
#' @param show_if Optional JavaScript condition showing the control.
#' @noRd
ctl_planter <- function(show_if = NULL) {
  concept <- fieldhub_control_concepts()$planter
  ctl_select("planter", concept$choices, selected = concept$value, show_if = show_if)
}

#' Optional random seed; blank means an automatic seed
#' @noRd
ctl_seed <- function() {
  label <- fieldhub_control_concepts()$seed$label
  design_control("seed", "seed", value = concept_value("seed"),
                 parse = function(value, values) parse_control_seed(value, label))
}

#' Comma-separated whole numbers (plot starts, levels per factor)
#' @param parse Parser \code{function(value, label, values)}; \code{values}
#'   holds the controls read before this one.
#' @noRd
ctl_text_list <- function(id, value = NULL, generated_only = FALSE,
                          parse = function(value, label, values) parse_control_whole_numbers(value, label),
                          ...) {
  label <- fieldhub_control_concepts()[[id]]$label
  design_control(id, "text", value = concept_value(id, value), generated_only = generated_only,
                 ..., parse = function(value, values) parse(value, label, values))
}

#' A select with fixed choices
#' @param choices The choices (named for display).
#' @param selected The default choice (the first when \code{NULL}).
#' @param convert Applied to the chosen string (e.g. \code{as.numeric}).
#' @noRd
ctl_select <- function(id, choices, selected = NULL, convert = identity, show_if = NULL) {
  label <- fieldhub_control_concepts()[[id]]$label
  selected <- if (is.null(selected)) unname(choices)[1L] else selected
  design_control(id, "select", value = selected, choices = choices, show_if = show_if,
                 parse = function(value, values) convert(parse_control_choice(value, label, choices)))
}

#' A select whose choices are computed from other controls or the upload
#' @param depends_on Ids of the controls the choices are computed from.
#' @param options Plain function \code{function(values, data)} returning
#'   \code{list(choices = , selected = )}; \code{data} is the shaped upload
#'   or \code{NULL}.
#' @noRd
ctl_dependent_select <- function(id, depends_on, options) {
  label <- fieldhub_control_concepts()[[id]]$label
  design_control(id, "dependent_select", depends_on = depends_on, options = options,
                 parse = function(value, values) parse_control_option(value, label))
}

#' A checkbox
#' @param enabled_by Id of a flag that must be on for this control to be
#'   read; while it is off the control reads as \code{disabled_value}.
#' @noRd
ctl_flag <- function(id, value = NULL, show_if = NULL, enabled_by = NULL, disabled_value = NULL) {
  label <- fieldhub_control_concepts()[[id]]$label
  default <- concept_value(id, value)
  design_control(id, "flag", value = default, show_if = show_if, enabled_by = enabled_by,
                 disabled_value = disabled_value,
                 parse = function(value, values) parse_control_flag(value, label, default))
}

#' A note computed from the other controls (no value of its own)
#' @param text Plain function \code{function(raw, uploaded)} returning the
#'   note or \code{NULL}.
#' @noRd
ctl_preview <- function(id, text, show_if = NULL) {
  design_control(id, "preview", parse = NULL, preview = text, show_if = show_if)
}

#' Render one sidebar control
#'
#' @param control A control built by a \code{ctl_*()} constructor.
#' @param ns The page's namespace function.
#' @param toggle The id of the page's "Import entries' list?" toggle, or
#'   \code{NULL}; controls read only for generated entries are hidden while
#'   it is "Yes".
#' @noRd
app_control_ui <- function(control, ns, toggle = NULL) {
  id <- ns(control$id)
  widget <- switch(control$type,
    number = shiny::numericInput(id, control$label, value = control$value,
                                 min = if (is.null(control$min)) NA else control$min,
                                 max = if (is.null(control$max)) NA else control$max),
    text = shiny::textInput(id, control$label, value = control$value),
    select = shiny::selectInput(id, control$label, choices = control$choices,
                                selected = control$value, multiple = FALSE),
    dependent_select = shiny::selectInput(id, control$label, choices = ""),
    flag = shiny::checkboxInput(id, control$label, value = control$value),
    seed = app_seed_input(id, value = control$value),
    preview = shiny::uiOutput(id),
    fieldhub_abort("Unknown control type: ", control$type, class = "fieldhub_internal_error")
  )
  conditions <- c(
    if (isTRUE(control$generated_only) && !is.null(toggle)) paste0("input.", toggle, " != 'Yes'"),
    control$show_if
  )
  if (length(conditions) == 0L) return(widget)
  shiny::conditionalPanel(paste(conditions, collapse = " && "), ns = ns, widget)
}
