#' One label, default and minimum per control concept
#'
#' @description Every design page builds its sidebar from the constructors
#' below, and every constructor takes its label (and, where the concept has
#' one, its default and minimum) from this table, so the same concept reads
#' the same in validation on every page. Original visible labels and row
#' layouts are applied separately by \code{app_sidebar_ui()}.
#' The input id of a control is its concept, which
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
    seed = list(label = "Random Seed (blank = automatic):", value = NULL),
    # Spatial designs
    lines = list(label = "Input # of Entries:", min = 1),
    repGens = list(label = "# of Entries Per Rep Group:"),
    repUnits = list(label = "# of Rep Per Group:"),
    blocks = list(label = "Input # Entries per Expt:"),
    b = list(label = "Input # of Blocks:"),
    repsExpt = list(label = "Input # of Stacked Expts:", min = 1),
    repsStack = list(label = "Stack experiments:", choices = c("vertical", "horizontal")),
    stacked = list(label = "Blocks Layout:", choices = c("By Column", "By Row")),
    copies_per_entry = list(label = "# of Copies Per Entry:"),
    location_view = list(label = "Choose Location to View:"),
    expt_name = list(label = "Experiment Name(s):", value = "Expt1"),
    random = list(label = "Randomize Entries?", value = TRUE),
    random_note = list(label = NULL),
    sameEntries = list(label = "Repeat entries across experiments", value = FALSE),
    allow_fillers = list(label = "Allow filler plots", value = FALSE),
    # Steps of the spatial pages, offered after Run! or Randomize!
    dimensions = list(label = "Select dimensions of field:"),
    multi_dimension = list(label = "Set different dimensions across locations", value = FALSE),
    location_dimensions = list(label = "Select dimension for location"),
    checks_percent = list(label = "Choose % of Checks:")
  )
}

#' A control of the design sidebar
#'
#' @description The one constructor behind every \code{ctl_*()} helper. The
#' label comes from \code{fieldhub_control_concepts()}. Every constructor
#' takes its parser as \code{parse(value, label, values)} (the raw value,
#' the control's label and the values read before it) and hands the reader
#' \code{parse(value, values)} (see \code{read_design_controls()}).
#'
#' \code{stage} says when the choices of a computed select are computed:
#' \code{"live"} from the controls it depends on as they change,
#' \code{"run"} from the values of each Run!, \code{"randomize"} from the
#' values of each Randomize! (spatial pages).
#' @noRd
design_control <- function(id, type, parse, value = NULL, min = NULL, max = NULL,
                           choices = NULL, generated_only = FALSE, show_if = NULL,
                           enabled_by = NULL, disabled_value = NULL,
                           depends_on = NULL, options = NULL, preview = NULL,
                           stage = "live") {
  concept <- fieldhub_control_concepts()[[id]]
  if (is.null(concept)) fieldhub_abort("Unknown control concept: ", id, class = "fieldhub_internal_error")
  label <- concept$label
  list(id = id, type = type, label = label, value = value, min = min, max = max,
       choices = choices,
       parse = if (!is.null(parse)) function(value, values) parse(value, label, values),
       generated_only = generated_only, show_if = show_if,
       enabled_by = enabled_by, disabled_value = disabled_value, depends_on = depends_on,
       options = options, preview = preview, stage = stage)
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
#' @param parse Parser \code{function(value, label, values)}.
#' @noRd
ctl_count <- function(id, value, min = NULL, max = NULL, generated_only = FALSE,
                      parse = function(value, label, values) parse_control_number(value, label),
                      ...) {
  design_control(id, "number", value = value, min = concept_min(id, min), max = max,
                 generated_only = generated_only, ..., parse = parse)
}

#' Number of checks
#' @param max Only where the page offers no more.
#' @param choices Allowed counts for an original dropdown; NULL for a number box.
#' @noRd
ctl_checks <- function(value, max = NULL, generated_only = FALSE, choices = NULL, ...) {
  control <- ctl_count("checks", value, max = max, generated_only = generated_only, ...,
    parse = function(value, label, values) {
      if (!is.null(choices) && is.character(value)) {
        value <- as.numeric(parse_control_choice(value, label, choices))
      }
      parse_control_checks(value, label)
    })
  if (!is.null(choices)) {
    control$type <- "select"
    control$choices <- choices
  }
  control
}

#' Replicates of each check (one value is used for every check)
#' @param value Default text.
#' @noRd
ctl_rep_checks <- function(value = NULL, generated_only = FALSE, ...) {
  ctl_text_list("rep_checks", value, generated_only = generated_only, ...,
                parse = function(value, label, values) {
                  parse_control_rep_checks(value, label, values$checks)
                })
}

#' Number of full replicates
#' @noRd
ctl_reps <- function(value, min = NULL) ctl_count("reps", value, min = min)

#' Number of locations
#' @noRd
ctl_locations <- function() ctl_count("l", concept_value("l"))

#' Location names, comma separated
#' @param trim Whether to drop the spaces around each name.
#' @noRd
ctl_location_names <- function(trim = FALSE) {
  design_control("location_names", "text", value = concept_value("location_names"),
                 parse = function(value, label, values) parse_control_names(value, label, trim = trim))
}

#' Experiment name(s), comma separated
#' @param value The page's default.
#' @param split Whether the page takes several names (comma separated).
#' @param trim Whether to drop the spaces around each name.
#' @noRd
ctl_expt_name <- function(value = NULL, split = TRUE, trim = FALSE) {
  design_control("expt_name", "text", value = concept_value("expt_name", value),
                 parse = function(value, label, values) {
                   parse_control_names(value, label, split = split, trim = trim)
                 })
}

#' Starting plot number(s), comma separated
#' @param value The page's default.
#' @noRd
ctl_plot_start <- function(value = NULL) ctl_text_list("plot_start", value)

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
  design_control("seed", "seed", value = concept_value("seed"),
                 parse = function(value, label, values) parse_control_seed(value, label))
}

#' Comma-separated whole numbers (plot starts, levels per factor)
#' @param parse Parser \code{function(value, label, values)}; \code{values}
#'   holds the controls read before this one.
#' @noRd
ctl_text_list <- function(id, value = NULL, generated_only = FALSE,
                          parse = function(value, label, values) parse_control_whole_numbers(value, label),
                          ...) {
  design_control(id, "text", value = concept_value(id, value), generated_only = generated_only,
                 ..., parse = parse)
}

#' A select with fixed choices
#' @param choices The choices (named for display).
#' @param selected The default choice (the first when \code{NULL}).
#' @param convert Applied to the chosen string (e.g. \code{as.numeric}).
#' @noRd
ctl_select <- function(id, choices = NULL, selected = NULL, convert = identity, show_if = NULL) {
  choices <- if (is.null(choices)) fieldhub_control_concepts()[[id]]$choices else choices
  selected <- if (is.null(selected)) unname(choices)[1L] else selected
  design_control(id, "select", value = selected, choices = choices, show_if = show_if,
                 parse = function(value, label, values) convert(parse_control_choice(value, label, choices)))
}

#' A select whose choices are computed from other controls or the upload
#'
#' @param depends_on Ids of the controls the choices are computed from:
#'   they are read (\code{read_design_controls()}) into the \code{values}
#'   \code{options} receives.
#' @param options Plain function \code{function(values, data)} returning
#'   \code{list(choices = , selected = )}; \code{data} is the shaped upload
#'   or \code{NULL}. An input error it raises explains why nothing fits.
#' @param stage \code{"live"} (a sidebar select following
#'   \code{depends_on}), \code{"run"} or \code{"randomize"} (computed from
#'   the values of each Run! or Randomize!, see \code{design_control()}).
#' @param parse Parser \code{function(value, label, values)}; a whole number
#'   by default. A step's parser may return a named list of the values it
#'   sets (\code{read_design_steps()}). \code{NULL} for a select that only
#'   chooses what is shown (the location to view).
#' @noRd
ctl_dependent_select <- function(id, depends_on = NULL, options, stage = "live",
                                 parse = function(value, label, values) parse_control_option(value, label),
                                 show_if = NULL, enabled_by = NULL) {
  design_control(id, "dependent_select", depends_on = depends_on, options = options,
                 stage = stage, parse = parse, show_if = show_if, enabled_by = enabled_by)
}

#' The location a spatial page shows, from the locations of the run
#' @noRd
ctl_location_view <- function() {
  ctl_dependent_select("location_view", stage = "run", parse = NULL,
                       options = function(values, data) {
                         choices <- location_view_choices(values$l)
                         list(choices = choices, selected = utils::head(choices, 1L))
                       })
}

#' The field size a spatial page offers after Run!
#' @param options Plain function \code{function(values, data)} (see
#'   \code{ctl_dependent_select()}).
#' @param depends_on Controls read again as they change (a new choice of
#'   filler plots offers other sizes without a new Run!).
#' @param per_location Whether each location gets the size (the one size
#'   repeated \code{l} times).
#' @noRd
ctl_dimensions <- function(options, depends_on = NULL, per_location = FALSE, show_if = NULL) {
  ctl_dependent_select("dimensions", depends_on = depends_on, options = options, stage = "run",
                       show_if = show_if, parse = function(value, label, values) {
                         size <- parse_control_dimensions(value, label)
                         times <- if (per_location) values$l else 1L
                         list(nrows = rep(size[1], times), ncols = rep(size[2], times))
                       })
}

#' One field size per location (multi-location p-rep)
#' @param options Plain function \code{function(values, data)} returning
#'   \code{list(choices = , selected = )}, each a list with one entry per
#'   location.
#' @param enabled_by The flag step that turns the per-location sizes on.
#' @noRd
ctl_location_dimensions <- function(options, enabled_by, show_if = NULL) {
  design_control("location_dimensions", "location_selects", options = options, stage = "run",
                 enabled_by = enabled_by, show_if = show_if,
                 parse = function(value, label, values) {
                   sizes <- lapply(value, parse_control_dimensions, label = label)
                   list(nrows = vapply(sizes, `[[`, numeric(1), 1L),
                        ncols = vapply(sizes, `[[`, numeric(1), 2L))
                 })
}

#' The percentage of checks a diagonal page offers after Randomize!
#' @param options Plain function \code{function(values, data)} returning
#'   \code{list(choices = , selected = , table = )}.
#' @noRd
ctl_checks_percent <- function(options) {
  ctl_dependent_select("checks_percent", options = options, stage = "randomize",
                       parse = function(value, label, values) {
                         list(checksPercent = as.numeric(value))
                       })
}

#' A boolean control, optionally shown as inline Yes/No choices
#' @param choices Named logical choices, or NULL for a checkbox.
#' @param enabled_by Id of a flag that must be on for this control to be
#'   read; while it is off the control reads as \code{disabled_value}.
#' @param stage \code{"run"} for a flag offered with the steps of a spatial
#'   page.
#' @noRd
ctl_flag <- function(id, value = NULL, show_if = NULL, enabled_by = NULL, disabled_value = NULL,
                     stage = "live", choices = NULL) {
  default <- concept_value(id, value)
  design_control(id, "flag", value = default, show_if = show_if, enabled_by = enabled_by,
                 disabled_value = disabled_value, stage = stage, choices = choices,
                 parse = function(value, label, values) {
                   if (!is.null(choices) && is.character(value) && length(value) == 1L &&
                       !is.na(value) && value %in% as.character(choices)) value <- as.logical(value)
                   parse_control_flag(value, label, default)
                 })
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
    location_selects = shiny::uiOutput(id),
    flag = if (is.null(control$choices)) shiny::checkboxInput(id, control$label, value = control$value)
      else shiny::radioButtons(id, control$label, choices = control$choices,
                               selected = as.character(control$value), inline = TRUE),
    seed = app_seed_input(id, value = control$value, label = control$label),
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
