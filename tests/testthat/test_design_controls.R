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
    ctl_count("checks", 2, enabled_by = "use_checks", parse = function(value, label) parse_n_checks(value)),
    ctl_text_list("rep_checks", enabled_by = "use_checks", parse = function(value, label, values) {
      parse_control_rep_checks(value, label, values$checks)
    }),
    ctl_flag("spread_checks", enabled_by = "use_checks", disabled_value = TRUE)
  )
  off <- read_design_controls(spec, list(use_checks = FALSE, checks = NA, rep_checks = "", spread_checks = FALSE))
  expect_identical(off, list(use_checks = FALSE, checks = NULL, rep_checks = NULL, spread_checks = TRUE))
  on <- read_design_controls(spec, list(use_checks = TRUE, checks = 2L, rep_checks = "2,3", spread_checks = FALSE))
  expect_identical(on, list(use_checks = TRUE, checks = 2L, rep_checks = c(2, 3), spread_checks = FALSE))
  expect_identical(read_design_controls(spec, list(use_checks = TRUE, checks = 3L, rep_checks = "2"))$rep_checks,
                   c(2, 2, 2))
  expect_control_error(read_design_controls(spec, list(use_checks = TRUE, checks = NA, rep_checks = "2")),
                       "Input # of Checks cannot be blank.")
  for (value in blanks) {
    expect_control_error(read_design_controls(spec, list(use_checks = TRUE, checks = 2L, rep_checks = value)),
                         "Reps per Check cannot be blank.")
  }
  expect_control_error(read_design_controls(spec, list(use_checks = TRUE, checks = 2L, rep_checks = "1,2,3")),
                       "Reps per Check must have 1 value or 2 values")
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
