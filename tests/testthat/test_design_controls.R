library(testthat)
library(FielDHub)

# Plain tests of the shared design-page controls (R/app_controls.R) and
# their reader (R/validate_design_controls.R). Raw values use Shiny's own
# encodings: NULL before an input renders, logical NA for a cleared
# numericInput, "" for a cleared textInput.

blanks <- list(NULL, NA, "", "   ", NA_real_, character())

label_of <- function(id) control_label_name(fieldhub_control_concepts()[[id]]$label)

expect_control_error <- function(expr, pattern) {
  err <- expect_error(expr, class = "fieldhub_input_error")
  expect_match(conditionMessage(err), pattern, fixed = TRUE)
  invisible(err)
}

page <- function(...) list(controls = list(...))

test_that("control_label_name() drops the trailing colon of a label", {
  expect_identical(control_label_name("Input # of Treatments:"), "Input # of Treatments")
  expect_identical(control_label_name("Random Seed (blank = automatic):"),
                   "Random Seed (blank = automatic)")
  expect_identical(control_label_name("Add repeated checks?"), "Add repeated checks?")
})

test_that("is_blank_control_value() recognises every Shiny blank", {
  for (value in blanks) expect_true(is_blank_control_value(value))
  for (value in list(0, 0L, "0", FALSE, "a", c(1, 2), list(NA))) {
    expect_false(is_blank_control_value(value))
  }
})

test_that("count controls read one whole number and name their label otherwise", {
  spec <- page(ctl_count("t", 15), ctl_reps(3), ctl_locations())
  expect_identical(read_design_controls(spec, list(t = 15L, reps = 3L, l = 2L)),
                   list(t = 15L, reps = 3L, l = 2L))
  for (value in blanks) {
    expect_control_error(read_design_controls(spec, list(t = value, reps = 3L, l = 1L)),
                         paste(label_of("t"), "cannot be blank."))
  }
  for (value in list(2.5, "12", TRUE, c(1, 2), Inf)) {
    expect_control_error(read_design_controls(spec, list(t = 4L, reps = value, l = 1L)),
                         paste(label_of("reps"), "must be one whole number."))
  }
  # A missing entry is an input that has not rendered: blank
  expect_control_error(read_design_controls(spec, list(t = 4L, reps = 2L)),
                       paste(label_of("l"), "cannot be blank."))
})

test_that("the error names the control it came from", {
  err <- expect_control_error(read_design_controls(page(ctl_locations()), list(l = NA)),
                              "Input # of Locations cannot be blank.")
  expect_identical(err$control, "Input # of Locations:")
})

test_that("starting plots read comma-separated whole numbers", {
  spec <- page(ctl_plot_start())
  expect_identical(read_design_controls(spec, list(plot_start = "101"))$plot_start, 101)
  expect_identical(read_design_controls(spec, list(plot_start = "101, 1001"))$plot_start, c(101, 1001))
  for (value in blanks) {
    expect_control_error(read_design_controls(spec, list(plot_start = value)),
                         "Starting Plot Number(s) cannot be blank.")
  }
  expect_control_error(read_design_controls(spec, list(plot_start = "101,abc")),
                       "Starting Plot Number(s) could not read \"abc\"")
})

test_that("factor level counts need two or more factors", {
  spec <- page(ctl_text_list("setfactors", "2,2,3",
                             parse = function(value, label, values) parse_control_factor_counts(value, label)))
  expect_identical(read_design_controls(spec, list(setfactors = "2,3"))$setfactors, c(2, 3))
  expect_control_error(read_design_controls(spec, list(setfactors = "4")),
                       "more than one factor needs to be specified")
  for (value in blanks) {
    expect_control_error(read_design_controls(spec, list(setfactors = value)), "cannot be blank.")
  }
})

test_that("location names split on commas as the design pages always have", {
  spec <- page(ctl_location_names())
  expect_identical(read_design_controls(spec, list(location_names = "FARGO"))$location_names, "FARGO")
  expect_identical(read_design_controls(spec, list(location_names = "A,B"))$location_names, c("A", "B"))
  expect_identical(read_design_controls(spec, list(location_names = "A, B"))$location_names, c("A", " B"))
  for (value in blanks) {
    expect_control_error(read_design_controls(spec, list(location_names = value)),
                         "Location Name(s) cannot be blank.")
  }
  expect_control_error(read_design_controls(spec, list(location_names = 3)),
                       "Location Name(s) must be one comma-separated text value.")
})

test_that("selects read one of their choices, converted as the builder needs", {
  spec <- page(ctl_planter(), ctl_select("type", c("In a RCBD" = "2", "In a CRD" = "1"), convert = as.numeric))
  values <- read_design_controls(spec, list(planter = "cartesian", type = "1"))
  expect_identical(values, list(planter = "cartesian", type = 1))
  for (value in blanks) {
    expect_control_error(read_design_controls(spec, list(planter = value, type = "2")),
                         "Plot Order Layout cannot be blank.")
  }
  expect_control_error(read_design_controls(spec, list(planter = "spiral", type = "2")),
                       "Plot Order Layout must be one of: serpentine, cartesian.")
  expect_control_error(read_design_controls(spec, list(planter = "serpentine", type = 2)),
                       "Select Design Type must be one of: 2, 1.")
})

test_that("dependent selects read a whole number, or report that nothing fits", {
  control <- ctl_dependent_select("k", depends_on = "t", options = function(values, data) {
    block_size_choices(if (is.null(data)) values$t else nrow(data), "incomplete_blocks")
  })
  spec <- page(ctl_count("t", 12, generated_only = TRUE), control)
  expect_identical(read_design_controls(spec, list(t = 12L, k = "4"))$k, 4)
  for (value in blanks) {
    expect_control_error(read_design_controls(spec, list(t = 12L, k = value)),
                         "Input # of Plots per IBlock cannot be blank.")
  }
  expect_control_error(read_design_controls(spec, list(t = 13L, k = "No Options Available")),
                       "No options for this combination of treatments!")
  expect_control_error(read_design_controls(spec, list(t = 12L, k = "four")),
                       "Input # of Plots per IBlock must be one whole number.")
  # choices follow t, or the uploaded entries
  expect_identical(design_control_choices(spec, control, list(t = 12L)),
                   list(choices = c(2L, 3L, 4L, 6L), selected = 3L))
  expect_identical(design_control_choices(spec, control, list(t = NA), data = data.frame(ENTRY = 1:15)),
                   list(choices = c(3L, 5L), selected = 3L))
  expect_control_error(design_control_choices(spec, control, list(t = NA)),
                       "Input # of Treatments cannot be blank.")
})

test_that("flags read TRUE/FALSE and take their default before they render", {
  spec <- page(ctl_flag("continuous"), ctl_flag("use_checks"))
  expect_identical(read_design_controls(spec, list(continuous = FALSE, use_checks = TRUE)),
                   list(continuous = FALSE, use_checks = TRUE))
  for (value in list(NULL, NA)) {
    expect_identical(read_design_controls(spec, list(continuous = value, use_checks = value)),
                     list(continuous = TRUE, use_checks = FALSE))
  }
  expect_control_error(read_design_controls(spec, list(continuous = "yes", use_checks = FALSE)),
                       "Continuous Plot must be TRUE or FALSE.")
})

test_that("a blank seed means an automatic seed; anything else must be a number", {
  spec <- page(ctl_seed())
  for (value in blanks) {
    values <- read_design_controls(spec, list(seed = value))
    expect_identical(names(values), "seed")
    expect_null(values$seed)
  }
  expect_identical(read_design_controls(spec, list(seed = 17L))$seed, 17)
  expect_identical(read_design_controls(spec, list(seed = -3))$seed, -3)
  expect_control_error(read_design_controls(spec, list(seed = "abc")),
                       "Random Seed (blank = automatic) must be one number, or blank for an automatic seed.")
  expect_control_error(read_design_controls(spec, list(seed = c(1, 2))), "Random Seed")
})

test_that("controls for generated entries are not read when entries are uploaded", {
  spec <- page(ctl_count("t", 15, generated_only = TRUE), ctl_reps(3))
  expect_identical(read_design_controls(spec, list(t = NA, reps = 3L), uploaded = TRUE),
                   list(reps = 3L))
  expect_control_error(read_design_controls(spec, list(t = NA, reps = 3L)), "cannot be blank.")
})

test_that("controls enabled by a flag take their off value while it is off", {
  spec <- page(
    ctl_flag("use_checks"),
    ctl_checks(2, enabled_by = "use_checks"),
    ctl_rep_checks(enabled_by = "use_checks"),
    ctl_flag("spread_checks", enabled_by = "use_checks", disabled_value = TRUE)
  )
  off <- read_design_controls(spec, list(use_checks = FALSE, checks = NA, rep_checks = "", spread_checks = FALSE))
  expect_identical(off, list(use_checks = FALSE, checks = NULL, rep_checks = NULL, spread_checks = TRUE))
  on <- read_design_controls(spec, list(use_checks = TRUE, checks = 2L, rep_checks = "2,3", spread_checks = FALSE))
  expect_identical(on, list(use_checks = TRUE, checks = 2L, rep_checks = c(2, 3), spread_checks = FALSE))
  expect_identical(read_design_controls(spec, list(use_checks = TRUE, checks = 3L, rep_checks = "2"))$rep_checks,
                   c(2, 2, 2))
  err <- expect_control_error(read_design_controls(spec, list(use_checks = TRUE, checks = NA, rep_checks = "2")),
                              "Input # of Checks cannot be blank.")
  expect_identical(err$control, "Input # of Checks:")
  err <- expect_control_error(read_design_controls(spec, list(use_checks = TRUE, checks = 0L, rep_checks = "2")),
                              "Input # of Checks must be a whole number from 1 to")
  expect_identical(err$control, "Input # of Checks:")
  for (value in blanks) {
    expect_control_error(read_design_controls(spec, list(use_checks = TRUE, checks = 2L, rep_checks = value)),
                         "Reps per Check cannot be blank.")
  }
  err <- expect_control_error(read_design_controls(spec, list(use_checks = TRUE, checks = 2L, rep_checks = "1,2,3")),
                              "Reps per Check must have 1 value or 2 values")
  expect_identical(err$control, "Reps per Check:")
})

test_that("every parser takes (value, label, values) and names its control in errors", {
  # A parser reads the values read before it, and every error it raises
  # carries the label of its control
  spec <- page(ctl_text_list("repGens", "75,150"),
               ctl_text_list("repUnits", "2,1", parse = function(value, label, values) {
                 parse_control_rep_units(value, label, values$repGens)
               }),
               ctl_plot_start())
  expect_identical(read_design_controls(spec, list(repGens = "75,150", repUnits = "2,1", plot_start = "1")),
                   list(repGens = c(75, 150), repUnits = c(2, 1), plot_start = 1))
  cases <- list(
    list(raw = list(repGens = "75,x", repUnits = "2,1", plot_start = "1"), control = "# of Entries Per Rep Group:",
         message = "# of Entries Per Rep Group could not read"),
    list(raw = list(repGens = "75,150", repUnits = "2", plot_start = "1"), control = "# of Rep Per Group:",
         message = "# of Rep Per Group must have one value per group of entries (2); got 1."),
    list(raw = list(repGens = "75", repUnits = "2", plot_start = "0"), control = "Starting Plot Number(s):",
         message = "Starting Plot Number(s) could not read")
  )
  for (case in cases) {
    err <- expect_control_error(read_design_controls(spec, case$raw), case$message)
    expect_identical(err$control, case$control)
  }
  # the parser a constructor is given receives the label and earlier values
  seen <- NULL
  spec <- page(ctl_reps(3), ctl_count("t", 4, parse = function(value, label, values) {
    seen <<- list(label = label, values = values)
    value
  }))
  read_design_controls(spec, list(reps = 3L, t = 4L))
  expect_identical(seen, list(label = "Input # of Treatments:", values = list(reps = 3L)))
})

test_that("names split on commas, and may be read whole or trimmed", {
  expect_identical(read_design_controls(page(ctl_expt_name()), list(expt_name = "A, B"))$expt_name,
                   c("A", " B"))
  expect_identical(read_design_controls(page(ctl_expt_name(trim = TRUE)), list(expt_name = "A, B"))$expt_name,
                   c("A", "B"))
  expect_identical(read_design_controls(page(ctl_expt_name(split = FALSE)), list(expt_name = "A,B"))$expt_name,
                   "A,B")
  expect_identical(read_design_controls(page(ctl_location_names(trim = TRUE)),
                                        list(location_names = " X ,Y"))$location_names, c("X", "Y"))
  for (value in blanks) {
    expect_control_error(read_design_controls(page(ctl_expt_name()), list(expt_name = value)),
                         "Experiment Name(s) cannot be blank.")
  }
})

test_that("field sizes read as rows and columns", {
  expect_identical(parse_control_dimensions("15 x 20", "Size:"), c(15, 20))
  for (value in c(blanks, list("15x20", "15 x", "a x b", "0 x 3", "2.5 x 3", 15, "No Options Available"))) {
    expect_error(parse_control_dimensions(value, "Size:"), class = "fieldhub_input_error")
  }
  dims <- ctl_dimensions(function(values, data) NULL)
  expect_identical(dims$parse("15 x 20", list(l = 3)), list(nrows = 15, ncols = 20))
  dims <- ctl_dimensions(function(values, data) NULL, per_location = TRUE)
  expect_identical(dims$parse("15 x 20", list(l = 3)), list(nrows = c(15, 15, 15), ncols = c(20, 20, 20)))
  sizes <- ctl_location_dimensions(function(values, data) NULL, enabled_by = "multi_dimension")
  expect_identical(sizes$parse(list("8 x 8", "9 x 7"), list(l = 2)), list(nrows = c(8, 9), ncols = c(8, 7)))
  expect_identical(ctl_checks_percent(function(values, data) NULL)$parse("9.6", list()),
                   list(checksPercent = 9.6))
})

test_that("a step is read once it holds one of its current choices", {
  expect_true(step_choice_ready("15 x 20", c("15 x 20", "20 x 15")))
  expect_true(step_choice_ready("15 x 20", c(`15 x 20 (+1 filler)` = "15 x 20")))
  expect_false(step_choice_ready("1 x 300", c("15 x 20", "20 x 15")))
  expect_true(step_choice_ready("9.6", c(4.5, 9.6)))
  expect_true(step_choice_ready(9.6, c(4.5, 9.6)))
  expect_false(step_choice_ready("9.7", c(4.5, 9.6)))
  expect_true(step_choice_ready(list("8 x 8", "9 x 7"), list(c("8 x 8"), c("9 x 7", "7 x 9"))))
  expect_false(step_choice_ready(list("8 x 8", NULL), list(c("8 x 8"), c("9 x 7"))))
  expect_false(step_choice_ready(list("8 x 8"), list(c("8 x 8"), c("9 x 7"))))
  for (value in c(blanks, list(c("a", "b"), list("x")))) expect_false(step_choice_ready(value, c("a", "b")))

  steps <- list(
    ctl_dimensions(function(values, data) list(choices = c("2 x 3", "3 x 2"), selected = "2 x 3"),
                   per_location = TRUE),
    ctl_flag("multi_dimension", stage = "run"),
    ctl_location_dimensions(function(values, data) NULL, enabled_by = "multi_dimension")
  )
  choices <- list(dimensions = design_step_choices(steps[[1]], list(l = 2)),
                  location_dimensions = list(choices = list("2 x 3", c("1 x 6", "2 x 3"))))
  values <- read_design_steps(steps, list(dimensions = "3 x 2", multi_dimension = FALSE), list(l = 2), choices)
  expect_identical(values, list(l = 2, nrows = c(3, 3), ncols = c(2, 2), multi_dimension = FALSE))
  values <- read_design_steps(steps, list(dimensions = "3 x 2", multi_dimension = TRUE,
                                          location_dimensions = list("2 x 3", "1 x 6")), list(l = 2), choices)
  expect_identical(values[c("nrows", "ncols")], list(nrows = c(2, 1), ncols = c(3, 6)))
  # waits while a select holds a value of earlier choices
  expect_null(read_design_steps(steps, list(dimensions = "6 x 1"), list(l = 2), choices))
  expect_null(read_design_steps(steps, list(dimensions = "3 x 2", multi_dimension = TRUE,
                                            location_dimensions = list("2 x 3", "9 x 9")), list(l = 2), choices))
})

test_that("a step with nothing to choose from is explained instead of waited for", {
  sizes <- ctl_location_dimensions(function(values, data) NULL, enabled_by = "multi_dimension")
  steps <- list(ctl_flag("multi_dimension", stage = "run"), sizes)
  choices <- list(location_dimensions = list(choices = list("8 x 8", character(), character())))
  raw <- list(multi_dimension = TRUE, location_dimensions = list("8 x 8", NULL, NULL))
  err <- expect_error(read_design_steps(steps, raw, list(l = 3), choices), class = "fieldhub_input_error")
  expect_match(conditionMessage(err), "^Select dimension for location 2: no field size fits this location")
  expect_identical(err$control, "Select dimension for location 2")
  # the per-location sizes are not read while their flag is off
  expect_identical(read_design_steps(steps, list(multi_dimension = FALSE), list(l = 3), choices),
                   list(l = 3, multi_dimension = FALSE))
  empty <- ctl_dimensions(function(values, data) NULL)
  expect_error(read_design_steps(list(empty), list(dimensions = ""), list(),
                                 list(dimensions = list(choices = character()))),
               "Select dimensions of field has no option that fits.", class = "fieldhub_input_error")
})

test_that("each Randomize! starts its steps from the choices it selects, as the selects send them", {
  steps <- list(ctl_checks_percent(function(values, data) NULL))
  choices <- list(checks_percent = list(choices = c(6.98, 9.6, 11.11), selected = 11.11))
  raw <- selected_step_values(steps, choices)
  expect_identical(raw, list(checks_percent = "11.11"))
  expect_identical(read_design_steps(steps, raw, list(), choices), list(checksPercent = 11.11))
})

test_that("read_design_controls() reads only the controls asked for, and needs a named list", {
  spec <- page(ctl_count("t", 15), ctl_reps(3))
  expect_identical(read_design_controls(spec, list(t = 9L, reps = NA), only = "t"), list(t = 9L))
  expect_error(read_design_controls(spec, list(9L, 3L)), class = "fieldhub_internal_error")
  expect_error(read_design_controls(spec, "t"), class = "fieldhub_internal_error")
  expect_error(read_design_controls(spec, list(), uploaded = NA), class = "fieldhub_input_error")
  # a preview control has no value
  spec <- page(ctl_preview("checks_note", text = function(raw, uploaded) "note"), ctl_reps(3))
  expect_identical(read_design_controls(spec, list(reps = 2L)), list(reps = 2L))
})

test_that("design_control_defaults() is what the page shows before any change", {
  control <- ctl_dependent_select("k", depends_on = "t", options = function(values, data) {
    block_size_choices(values$t, "alpha_lattice")
  })
  spec <- page(ctl_count("t", 36), control, ctl_planter(), ctl_plot_start(), ctl_flag("continuous"),
               ctl_preview("checks_note", text = function(raw, uploaded) NULL), ctl_seed())
  expect_identical(design_control_defaults(spec),
                   list(t = 36, k = 6L, planter = "serpentine", plot_start = "101",
                        continuous = TRUE, seed = NULL))
})

test_that("control constructors take labels, defaults and minimums from the concept table", {
  concepts <- fieldhub_control_concepts()
  expect_identical(ctl_locations()[c("label", "value", "min")],
                   list(label = "Input # of Locations:", value = 1, min = 1))
  expect_identical(ctl_reps(3)$min, 2)
  expect_identical(ctl_reps(3, min = 1)$min, 1)
  expect_identical(ctl_plot_start()$value, "101")
  expect_identical(ctl_location_names()$value, "FARGO")
  expect_identical(ctl_planter()$value, "serpentine")
  expect_null(ctl_seed()$value)
  expect_identical(ctl_seed()$label, "Random Seed (blank = automatic):")
  expect_identical(ctl_flag("use_checks")$value, FALSE)
  expect_identical(ctl_flag("randomizeV", value = FALSE)$value, FALSE)
  for (id in names(concepts)) {
    if (!is.null(concepts[[id]]$label)) expect_true(endsWith(concepts[[id]]$label, ":") ||
                                                      !grepl(":", concepts[[id]]$label), info = id)
  }
  expect_error(ctl_count("unknown", 1), class = "fieldhub_internal_error")
  expect_identical(ctl_select("stacked", selected = "By Row")$choices, c("By Column", "By Row"))
  expect_identical(ctl_plot_start("1")$value, "1")
  expect_identical(ctl_expt_name()$value, "Expt1")
  expect_identical(ctl_location_view()$stage, "run")
  expect_null(ctl_location_view()$parse)
  expect_identical(ctl_checks_percent(function(values, data) NULL)$stage, "randomize")
})

test_that("computed selects of the sidebar can depend on controls read with an upload", {
  # checks are read on the upload path too; entries typed only on the other
  control <- ctl_dependent_select("b", depends_on = c("lines", "checks"), options = function(values, data) {
    list(choices = values, selected = if (is.null(data)) "typed" else nrow(data))
  })
  spec <- page(ctl_count("lines", 50, generated_only = TRUE), ctl_checks(4), control)
  expect_identical(design_control_choices(spec, control, list(lines = 50L, checks = 3L))$choices,
                   list(lines = 50L, checks = 3L))
  offered <- design_control_choices(spec, control, list(lines = NA, checks = 3L), data.frame(ENTRY = 1:9))
  expect_identical(offered, list(choices = list(checks = 3L), selected = 9L))
})

test_that("app_control_ui() renders each control type with its namespaced id", {
  ns <- shiny::NS("page")
  html <- function(control, toggle = NULL) as.character(app_control_ui(control, ns, toggle))
  expect_match(html(ctl_count("t", 15)), 'id="page-t" type="number"', fixed = TRUE)
  expect_match(html(ctl_count("t", 15)), 'min="2"', fixed = TRUE)
  expect_match(html(ctl_count("t", 5, max = 10)), 'max="10"', fixed = TRUE)
  expect_match(html(ctl_plot_start()), 'id="page-plot_start" type="text" class="shiny-input-text form-control" value="101"',
               fixed = TRUE)
  expect_match(html(ctl_planter()), '<option value="serpentine" selected>serpentine</option>', fixed = TRUE)
  expect_match(html(ctl_flag("continuous")), 'id="page-continuous" type="checkbox" class="shiny-input-checkbox" checked',
               fixed = TRUE)
  expect_match(html(ctl_seed()), 'id="page-seed"', fixed = TRUE)
  expect_false(grepl("value=", html(ctl_seed()), fixed = TRUE))
  expect_match(html(ctl_dependent_select("k", "t", function(values, data) NULL)), 'id="page-k"', fixed = TRUE)
  expect_match(html(ctl_location_view()), 'id="page-location_view"', fixed = TRUE)
  expect_match(html(ctl_location_dimensions(function(values, data) NULL, "multi_dimension")),
               'id="page-location_dimensions" class="shiny-html-output"', fixed = TRUE)
  expect_match(html(ctl_preview("checks_note", function(raw, uploaded) NULL)),
               'id="page-checks_note" class="shiny-html-output"', fixed = TRUE)
  hidden <- html(ctl_count("t", 15, generated_only = TRUE), toggle = "owndata")
  expect_match(hidden, "data-display-if=\"input.owndata != &#39;Yes&#39;\"", fixed = TRUE)
  expect_match(hidden, 'data-ns-prefix="page-"', fixed = TRUE)
  both <- html(ctl_planter(show_if = "input.type == '1'"))
  expect_match(both, "input.type == &#39;1&#39;", fixed = TRUE)
  expect_false(grepl("data-display-if", html(ctl_count("t", 15, generated_only = TRUE))))
})

test_that("the seed widget shows the concept's label", {
  html <- as.character(app_seed_input("s"))
  expect_match(html, "Random Seed (blank = automatic):", fixed = TRUE)
  expect_false(grepl("value=", html, fixed = TRUE))
})

test_that("block_size_choices() offers feasible sizes and selects the middle one", {
  expect_identical(block_size_choices(12, "incomplete_blocks"),
                   list(choices = c(2L, 3L, 4L, 6L), selected = 3L))
  expect_identical(block_size_choices(15, "incomplete_blocks"), list(choices = c(3L, 5L), selected = 3L))
  expect_identical(block_size_choices(49, "square_lattice"), list(choices = 7L, selected = 7L))
  expect_identical(block_size_choices(13, "row_column"),
                   list(choices = "No Options Available", selected = "No Options Available"))
  expect_error(block_size_choices(NA, "row_column"), class = "fieldhub_input_error")
})

test_that("rcbd_checks_note() previews the block size only when it can", {
  raw <- list(use_checks = TRUE, t = 18L, reps = 3L, checks = 2L, rep_checks = "2")
  expect_identical(rcbd_checks_note(raw, FALSE), rcbd_size_preview(18L, 3L, 2L, "2"))
  expect_null(rcbd_checks_note(replace(raw, "use_checks", list(FALSE)), FALSE))
  expect_null(rcbd_checks_note(replace(raw, "use_checks", list(NULL)), FALSE))
  expect_match(rcbd_checks_note(raw, TRUE), "depends on the uploaded list", fixed = TRUE)
  for (id in c("t", "reps", "checks", "rep_checks")) {
    for (value in list(NULL, NA, "")) expect_null(rcbd_checks_note(replace(raw, id, list(value)), FALSE))
  }
  expect_error(rcbd_checks_note(replace(raw, "rep_checks", "2,x"), FALSE), class = "fieldhub_input_error")
})

test_that("shape_design_upload() keeps, names and completes the design's columns", {
  file <- data.frame(a = c("x", "y", NA), b = c(1, NA, 3), extra = 1:3)
  expect_identical(shape_design_upload(file, "TREATMENT"),
                   stats::na.omit(data.frame(TREATMENT = c("x", "y", NA))))
  expect_identical(nrow(shape_design_upload(file, c("ENTRY", "NAME"))), 1L)
  kept <- shape_design_upload(file, c("Hplot", "Vplot"), omit_na = FALSE)
  expect_identical(names(kept), c("Hplot", "Vplot"))
  expect_identical(nrow(kept), 3L)
  expect_identical(upload_level_counts(kept), c(2, 2))
  expect_error(shape_design_upload(file, c("A", "B", "C", "D")), class = "fieldhub_input_error")
  expect_error(shape_design_upload("file", "A"), class = "fieldhub_input_error")
})

test_that("uploads of a factorial design need two factors, and a failed upload says so on Run", {
  two <- data.frame(FACTOR = c("A", "A", "B"), LEVEL = c("a0", "a1", "b0"))
  expect_identical(check_factorial_upload(two), two)
  expect_error(check_factorial_upload(two[1:2, ]), "More than one factor", class = "fieldhub_input_error")
  expect_identical(design_upload_data(two), two)
  expect_error(design_upload_data(NULL), "Check the input file", class = "fieldhub_input_error")
})
