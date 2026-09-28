library(testthat)
library(FielDHub)

# Plain tests of the generic design page (R/app_design_module.R) and the
# specs it renders (R/app_design_specs.R): what a page sends its engine,
# the HTML it builds, and the consistency of its controls. No Shiny server
# is started.

classic_modules <- c("CRD", "RCBD", "LSD", "FD", "SPD", "SSPD", "STRIPD", "IBD", "RowCol",
                     "Alpha_Lattice", "Square_Lattice", "Rectangular_Lattice")
spatial_modules <- c("Optim", "pREPS", "RCBD_augmented", "Diagonal")
all_modules <- c(classic_modules, spatial_modules)

# The one label of each concept, written out here so a label changed in
# fieldhub_control_concepts() fails this test instead of passing by
# construction
canonical_labels <- c(
  type = "Select Design Type:",
  t = "Input # of Treatments:",
  setfactors = "Input # of Entries for Each Factor (comma separated):",
  wp = "Input # of Whole Plots:",
  sp = "Input # of Sub-plots Within Whole Plots:",
  ssp = "Input # of Sub-sub-plots Within Sub-plots:",
  Hplots = "Input # of Horizontal Strips:",
  Vplots = "Input # of Vertical Strips:",
  reps = "Input # of Full Reps:",
  k = "Input # of Plots per IBlock:",
  nrows = "Input # of Rows:",
  use_checks = "Add repeated checks?",
  checks = "Input # of Checks:",
  rep_checks = "Reps per Check:",
  spread_checks = "Spread checks within each block",
  checks_note = NA,
  l = "Input # of Locations:",
  planter = "Plot Order Layout:",
  plot_start = "Starting Plot Number(s):",
  continuous = "Continuous Plot",
  location_names = "Location Name(s):",
  randomizeH = "Randomize Horizontal Strips (Across reps)",
  randomizeV = "Randomize Vertical Strips (Across reps)",
  seed = "Random Seed (blank = automatic):",
  lines = "Input # of Entries:",
  repGens = "# of Entries Per Rep Group:",
  repUnits = "# of Rep Per Group:",
  blocks = "Input # Entries per Expt:",
  b = "Input # of Blocks:",
  repsExpt = "Input # of Stacked Expts:",
  repsStack = "Stack experiments:",
  stacked = "Blocks Layout:",
  copies_per_entry = "# of Copies Per Entry:",
  location_view = "Choose Location to View:",
  expt_name = "Experiment Name(s):",
  random = "Randomize Entries?",
  random_note = NA,
  sameEntries = "Repeat entries across experiments",
  allow_fillers = "Allow filler plots",
  dimensions = "Select dimensions of field:",
  multi_dimension = "Set different dimensions across locations",
  location_dimensions = "Select dimension for location",
  checks_percent = "Choose % of Checks:"
)

#' Muffle only the onestage->twostage fallback row_column() may report
quiet_design <- function(expr) {
  withCallingHandlers(expr, fieldhub_design_warning = function(w) invokeRestart("muffleWarning"))
}

#' The design a page builds from raw control values (and an uploaded file),
#' as mod_design_server() does on Run! (and, on a spatial page,
#' app_spatial_page() on Randomize!). `steps` gives raw step values; a step
#' left out takes the choice the page selects.
page_design <- function(spec, raw, file = NULL, steps = list()) {
  shaped <- if (!is.null(file)) spec$upload_shape(file)
  controls <- read_design_controls(spec, shiny_shaped(raw), uploaded = !is.null(shaped))
  data <- spec$data(shaped, controls)
  values <- spec$values(controls, data)
  if (identical(spec$kind, "spatial")) values <- page_step_values(spec, values, data, steps)
  quiet_design(do.call(spec$engine, spec$args(values, data)))
}

#' The values a spatial page sends its engine after its steps
page_step_values <- function(spec, values, data, steps = list()) {
  if (!is.null(spec$optim)) {
    values[[spec$optim$into]] <- do.call(spec$optim$engine, spec$optim$args(values, data))
  }
  for (stage in c("run", "randomize")) {
    stage_steps <- Filter(function(step) identical(step$stage, stage), spec$steps)
    offered <- Filter(function(step) !is.null(step$options), stage_steps)
    choices <- stats::setNames(lapply(offered, design_step_choices, values = values, data = data),
                               vapply(offered, `[[`, character(1), "id"))
    raw <- lapply(stage_steps, function(step) {
      if (step$id %in% names(steps)) steps[[step$id]] else choices[[step$id]]$selected
    })
    names(raw) <- vapply(stage_steps, `[[`, character(1), "id")
    values <- read_design_steps(stage_steps, raw, values, choices)
    stopifnot(!is.null(values))
  }
  values
}

expect_same_design <- function(via_page, direct, info = NULL) {
  expect_identical(via_page$fieldBook, direct$fieldBook, info = info)
  expect_identical(via_page$metadata$parameters, direct$metadata$parameters, info = info)
}

page_defaults <- function(module, seed = 7) {
  raw <- design_control_defaults(design_app_spec(module))
  raw$seed <- seed
  raw
}

# Spatial pages whose runs take long: the diagonal searches, the
# allocations and the p-rep optimizations. optimized_arrangement() and
# RCBD_augmented() run in well under a second.
long_running <- c(Optim = FALSE, pREPS = TRUE, RCBD_augmented = FALSE, Diagonal = TRUE)

test_that("there is one page spec per design, in the registry's workflow order", {
  specs <- fieldhub_design_specs()
  expect_identical(names(specs), all_modules)
  expect_identical(names(specs)[seq_along(classic_modules)], names(fieldhub_classic_workflows()))
  registry <- fieldhub_app_registry()
  for (entry in registry) {
    if (is.null(entry$spec)) next
    spec <- design_app_spec(entry$workflow)
    expect_identical(entry$ui, "mod_design_ui")
    expect_identical(entry$server, "mod_design_server")
    expect_identical(spec$module, entry$workflow)
    expect_identical(spec$engine, getExportedValue("FielDHub", entry$engine), info = entry$id)
    expect_identical(spec$args, get(paste0("design_args_", spec$module), asNamespace("FielDHub")),
                     info = entry$id)
    expect_identical(spec$kind, entry$workflow_family, info = entry$id)
    if (identical(spec$kind, "classic")) {
      expect_identical(spec$workflow, classic_workflow_spec(spec$module))
      expect_identical(spec$layout, spec$workflow$layout)
      expect_false(spec$long_running)
    } else {
      expect_identical(spec$workflow, spatial_workflow_spec(spec$module))
      expect_null(spec$layout)
      expect_identical(spec$long_running, long_running[[spec$module]], info = entry$id)
      expect_true(all(vapply(spec$steps, function(step) step$stage %in% c("run", "randomize"), TRUE)))
      expect_true(spec$setup$type %in% c("summary", "table"), info = entry$id)
      expect_true(length(spec$entries) %in% 1:2, info = entry$id)
      expect_true(all(c("field_layout", "plot_numbers") %in% vapply(spec$panels, `[[`, "", "id")))
      if (!is.null(spec$optim)) {
        expect_identical(spec$optim$engine, do_optim)
        expect_identical(spec$optim$args, get(paste0("design_args_", spec$module, "_optim"),
                                              asNamespace("FielDHub")))
      }
    }
    expect_true(is.function(spec$values) && is.function(spec$data) && is.function(spec$upload_shape))
    expect_no_error(app_upload_spec(spec$upload))
  }
  # every design with a spec is a page of the generic module
  expect_setequal(names(specs), unlist(lapply(registry, `[[`, "spec")))
  for (bad in list(NULL, NA_character_, "unknown", 1, c("CRD", "RCBD"))) {
    expect_error(design_app_spec(bad), class = "fieldhub_input_error")
  }
  # built once per session
  expect_identical(fieldhub_design_specs(), specs)
  expect_true(identical(design_app_spec("CRD")$values, design_app_spec("CRD")$values))
})

test_that("each page's defaults build the design a direct API call with the documented defaults builds", {
  direct <- list(
    CRD = CRD(t = 15, reps = 4, plotNumber = 101, locationNames = "FARGO", seed = 7),
    RCBD = RCBD(t = 18, reps = 3, l = 1, plotNumber = 101, continuous = TRUE,
                planter = "serpentine", locationNames = "FARGO", seed = 7),
    LSD = latin_square(t = 5, reps = 1, plotNumber = 101, planter = "serpentine",
                       locationNames = "FARGO", seed = 7),
    FD = full_factorial(setfactors = c(2, 2, 3), reps = 3, l = 1, type = 2, plotNumber = 101,
                        planter = "serpentine", locationNames = "FARGO", seed = 7),
    SPD = split_plot(wp = 4, sp = 3, reps = 3, l = 1, type = 2, plotNumber = 101,
                     locationNames = "FARGO", seed = 7),
    SSPD = split_split_plot(wp = 2, sp = 2, ssp = 5, reps = 3, l = 1, type = 2, plotNumber = 101,
                            locationNames = "FARGO", seed = 7),
    STRIPD = strip_plot(Hplots = 5, Vplots = 5, reps = 3, l = 1, planter = "serpentine",
                        plotNumber = 101, locationNames = "FARGO", seed = 7,
                        randomizeH = TRUE, randomizeV = TRUE),
    IBD = incomplete_blocks(t = 15, k = 3, reps = 4, l = 1, plotNumber = 101,
                            locationNames = "FARGO", seed = 7),
    RowCol = quiet_design(row_column(t = 42, nrows = 6, reps = 2, l = 1, plotNumber = 101,
                                     locationNames = "FARGO", seed = 7)),
    Alpha_Lattice = alpha_lattice(t = 36, k = 6, reps = 3, l = 1, plotNumber = 101,
                                  locationNames = "FARGO", seed = 7),
    Square_Lattice = square_lattice(t = 49, k = 7, reps = 3, l = 1, plotNumber = 101,
                                    locationNames = "FARGO", seed = 7),
    Rectangular_Lattice = rectangular_lattice(t = 30, k = 5, reps = 3, l = 1, plotNumber = 101,
                                              locationNames = "FARGO", seed = 7)
  )
  expect_identical(names(direct), classic_modules)
  for (module in classic_modules) {
    expect_same_design(page_design(design_app_spec(module), page_defaults(module)),
                       direct[[module]], info = module)
  }
})

test_that("pages with changed controls still build the API's design", {
  raw <- utils::modifyList(page_defaults("RCBD", seed = 4), list(
    t = 6, l = 2, planter = "cartesian", plot_start = "101,1001", location_names = "A,B",
    continuous = FALSE, use_checks = TRUE, checks = 2, rep_checks = "2", spread_checks = TRUE))
  expect_same_design(page_design(design_app_spec("RCBD"), raw),
    RCBD(t = 6, reps = 3, l = 2, planter = "cartesian", plotNumber = c(101, 1001),
         locationNames = c("A", "B"), continuous = FALSE, seed = 4,
         checks = 2, rep_checks = c(2, 2), spread_checks = TRUE))
  raw <- utils::modifyList(page_defaults("SPD", seed = 3), list(type = "1", planter = "cartesian", l = 2,
                                                                    plot_start = "101,1001",
                                                                    location_names = "A,B"))
  expect_same_design(page_design(design_app_spec("SPD"), raw),
    split_plot(wp = 4, sp = 3, reps = 3, l = 2, type = 1, plotNumber = c(101, 1001),
               locationNames = c("A", "B"), seed = 3))
  raw <- utils::modifyList(page_defaults("STRIPD"), list(randomizeV = FALSE, reps = 1))
  expect_same_design(page_design(design_app_spec("STRIPD"), raw),
    strip_plot(Hplots = 5, Vplots = 5, reps = 1, l = 1, plotNumber = 101, locationNames = "FARGO",
               seed = 7, randomizeH = TRUE, randomizeV = FALSE))
})

test_that("pages with an uploaded file build the design the module always sent", {
  entries <- data.frame(ENTRY = 1:12, NAME = paste0("G", 1:12), NOTE = "x")
  raw <- utils::modifyList(page_defaults("IBD"), list(t = NA, k = "3"))
  expect_same_design(page_design(design_app_spec("IBD"), raw, entries),
    incomplete_blocks(t = 12, k = 3, reps = 4, l = 1, plotNumber = 101, locationNames = "FARGO",
                      seed = 7, data = data.frame(ENTRY = 1:12, NAME = paste0("G", 1:12))))
  treatments <- data.frame(TREATMENT = c(paste0("T", 1:5), NA))
  raw <- utils::modifyList(page_defaults("CRD"), list(t = NA))
  expect_same_design(page_design(design_app_spec("CRD"), raw, treatments),
    CRD(reps = 4, plotNumber = 101, locationNames = "FARGO", seed = 7,
        data = data.frame(TREATMENT = paste0("T", 1:5), REP = 4L)))
  strips <- data.frame(Hplot = c("H1", "H2", "H3"), Vplot = c("V1", "V2", NA))
  raw <- utils::modifyList(page_defaults("STRIPD"), list(Hplots = NA, Vplots = NA))
  expect_same_design(page_design(design_app_spec("STRIPD"), raw, strips),
    strip_plot(Hplots = 3, Vplots = 2, reps = 3, l = 1, plotNumber = 101, locationNames = "FARGO",
               seed = 7, randomizeH = TRUE, randomizeV = TRUE, data = strips))
  levels <- data.frame(WHOLEPLOT = c("W1", "W2", "W3"), SUBPLOT = c("S1", "S2", NA))
  raw <- utils::modifyList(page_defaults("SPD"), list(wp = NA, sp = NA))
  expect_same_design(page_design(design_app_spec("SPD"), raw, levels),
    split_plot(reps = 3, l = 1, type = 2, plotNumber = 101, locationNames = "FARGO", seed = 7,
               data = levels))
  factors <- data.frame(FACTOR = c("A", "A", "B"), LEVEL = c("a0", "a1", "b0"))
  expect_error(page_design(design_app_spec("FD"), page_defaults("FD"), factors[1:2, ]),
               "More than one factor", class = "fieldhub_input_error")
})

# --- Spatial pages: controls -> values -> (allocation) -> steps -> engine ---

# What each spatial page builds from its defaults (seed 7), as a direct call
# with the field size (and percentage of checks) the page selects first
spatial_defaults <- function(module) {
  switch(module,
    Optim = optimized_arrangement(nrows = 13, ncols = 24, lines = 280, checks = 4,
                                  rep_checks = c(8, 8, 8, 8), l = 1, plotNumber = 1, seed = 7,
                                  exptName = "Expt1", locationNames = "FARGO"),
    pREPS = partially_replicated(nrows = 15, ncols = 20, repGens = c(75, 150), repUnits = c(2, 1),
                                 l = 1, plotNumber = 1, seed = 7, exptName = "Expt1",
                                 locationNames = "FARGO"),
    RCBD_augmented = RCBD_augmented(lines = 180, checks = 4, b = 3, nrows = 3, ncols = 64, l = 1,
                                    plotNumber = 1, seed = 7, exptName = "Expt1",
                                    locationNames = "FARGO"),
    Diagonal = diagonal_arrangement(nrows = 18, ncols = 18, lines = 287, checks = 4, l = 1,
                                    plotNumber = 1, kindExpt = "SUDC", seed = 7, exptName = "Expt1",
                                    locationNames = "FARGO", checksPercent = 11.11)
  )
}

test_that("each spatial page's defaults build the design of a direct API call", {
  for (module in spatial_modules) {
    expect_same_design(page_design(design_app_spec(module), page_defaults(module)),
                       spatial_defaults(module), info = module)
  }
})

test_that("spatial pages with changed controls, steps or an uploaded file build the API's design", {
  spec <- design_app_spec("Optim")
  raw <- utils::modifyList(page_defaults("Optim", seed = 5), list(
    lines = 100, rep_checks = "5", l = 2, planter = "cartesian", plot_start = "1,1001",
    location_names = "A,B", expt_name = "Trial 1"))
  expect_same_design(page_design(spec, raw, steps = list(dimensions = "12 x 10")),
    optimized_arrangement(nrows = 12, ncols = 10, lines = 100, checks = 4, rep_checks = c(5, 5, 5, 5),
                          l = 2, planter = "cartesian", plotNumber = c(1, 1001), seed = 5,
                          exptName = "Trial 1", locationNames = c("A", "B")))
  entries <- data.frame(ENTRY = 1:104, NAME = c(paste0("CHECK", 1:4), paste0("SB-", 5:104)),
                        REPS = c(4L, 4L, 6L, 6L, rep(1L, 100)))
  raw <- utils::modifyList(page_defaults("Optim"), list(checks = NA, rep_checks = "", lines = NA))
  expect_same_design(page_design(spec, raw, entries, steps = list(dimensions = "12 x 10")),
    optimized_arrangement(nrows = 12, ncols = 10, plotNumber = 1, seed = 7, exptName = "Expt1",
                          locationNames = "FARGO", data = entries))
  expect_error(page_design(spec, raw, transform(entries, REPS = as.numeric(REPS))), "'REPS' must be numeric",
               class = "fieldhub_input_error")
  expect_error(page_design(spec, utils::modifyList(raw, list(checks = 4, rep_checks = "1,2", lines = 280))),
               "Reps per Check must have 1 value or 4 values", class = "fieldhub_input_error")
  expect_error(page_design(spec, utils::modifyList(raw, list(checks = 4, rep_checks = "80", lines = 280))),
               "Number of lines should be greater", class = "fieldhub_input_error")
})

test_that("the p-rep page offers filler plots and builds the API's design", {
  spec <- design_app_spec("pREPS")
  # 301 plots only fit a 7 x 43 field; filler plots offer squarer ones
  raw <- utils::modifyList(page_defaults("pREPS", seed = 11), list(
    repGens = "75,151", repUnits = "2,1", allow_fillers = TRUE, l = 2, planter = "cartesian",
    plot_start = "1,501", location_names = "A,B", expt_name = "P1"))
  expect_same_design(page_design(spec, raw, steps = list(dimensions = "16 x 19")),
    partially_replicated(nrows = 16, ncols = 19, repGens = c(75, 151), repUnits = c(2, 1), l = 2,
                         planter = "cartesian", plotNumber = c(1, 501), seed = 11, exptName = "P1",
                         locationNames = c("A", "B"), allow_fillers = TRUE))
  entries <- data.frame(ENTRY = 1:120, NAME = paste0("E", 1:120), REPS = rep(c(2L, 1L), c(40, 80)))
  raw <- utils::modifyList(page_defaults("pREPS"), list(repGens = "", repUnits = NA))
  expect_same_design(page_design(spec, raw, entries, steps = list(dimensions = "16 x 10")),
    partially_replicated(nrows = 16, ncols = 10, plotNumber = 1, seed = 7, exptName = "Expt1",
                         locationNames = "FARGO", data = entries))
  expect_error(page_design(spec, utils::modifyList(page_defaults("pREPS"), list(repUnits = "2"))),
               "# of Rep Per Group must have one value per group of entries", class = "fieldhub_input_error")
  expect_error(page_design(spec, utils::modifyList(page_defaults("pREPS"), list(repGens = "75,157"))),
               "Select 'Allow filler plots'", class = "fieldhub_input_error")
})

test_that("the augmented RCBD page offers the blocks and fields of its entries", {
  spec <- design_app_spec("RCBD_augmented")
  blocks <- Filter(function(control) identical(control$id, "b"), spec$controls)[[1]]
  typed <- design_control_choices(spec, blocks, shiny_shaped(list(lines = 180, checks = 4)))
  expect_identical(typed, augmented_block_choices(180, 4))
  entries <- data.frame(ENTRY = 1:64, NAME = c(paste0("CH", 1:4), paste0("G", 5:64)))
  uploaded <- design_control_choices(spec, blocks, shiny_shaped(list(lines = NA, checks = 4)), entries)
  expect_identical(uploaded, augmented_block_choices(60, 4))
  # changed controls, stacked experiments and a second location
  raw <- utils::modifyList(page_defaults("RCBD_augmented", seed = 3), list(
    lines = 60, checks = 3, b = "5", repsExpt = 2, repsStack = "horizontal", random = FALSE, l = 2,
    planter = "cartesian", plot_start = "1,1001", expt_name = "E1,E2", location_names = "A,B"))
  fields <- augmented_field_choices(60, 3, 5)$choices
  expect_same_design(page_design(spec, raw, steps = list(dimensions = fields[[2]])),
    RCBD_augmented(lines = 60, checks = 3, b = 5, repsExpt = 2, repsStack = "horizontal",
                   random = FALSE, l = 2, planter = "cartesian", plotNumber = c(1, 1001),
                   exptName = c("E1", "E2"), locationNames = c("A", "B"), seed = 3,
                   nrows = as.numeric(strsplit(fields[[2]], " x ")[[1]][1]),
                   ncols = as.numeric(strsplit(fields[[2]], " x ")[[1]][2])))
  # the stacking is only read for stacked experiments
  one <- utils::modifyList(page_defaults("RCBD_augmented"), list(repsStack = "horizontal"))
  expect_null(spec$values(read_design_controls(spec, shiny_shaped(one)), NULL)$repsStack)
  # an uploaded list: its rows after the checks are the entries
  raw <- utils::modifyList(page_defaults("RCBD_augmented"), list(lines = NA, b = "4"))
  expect_same_design(page_design(spec, raw, entries, steps = list(dimensions = "4 x 19")),
    RCBD_augmented(lines = 60, checks = 4, b = 4, nrows = 4, ncols = 19, l = 1, plotNumber = 1,
                   seed = 7, exptName = "Expt1", locationNames = "FARGO", data = entries))
  expect_error(page_design(spec, utils::modifyList(page_defaults("RCBD_augmented"), list(lines = 7))),
               "At least ten treatments", class = "fieldhub_input_error")
  expect_error(page_design(spec, utils::modifyList(page_defaults("RCBD_augmented"), list(b = "No Options Available"))),
               "No options for this combination", class = "fieldhub_input_error")
  expect_identical(augmented_random_note(FALSE), "By unchecking this option only the check plots are randomized.")
  expect_null(augmented_random_note(TRUE))
})

test_that("the diagonal page offers fields and percentages of checks and builds the API's design", {
  spec <- design_app_spec("Diagonal")
  raw <- utils::modifyList(page_defaults("Diagonal", seed = 21), list(
    lines = 150, checks = 3, l = 2, planter = "cartesian", plot_start = "1,1001",
    location_names = "A,B", expt_name = "D1"))
  fields <- diagonal_field_choices(150, 150, 1:3, planter = "cartesian")$choices
  size <- as.numeric(strsplit(fields[[2]], " x ")[[1]])
  percents <- diagonal_percent_choices(size[1], size[2], 1:3, 153, planter = "cartesian")$choices
  expect_same_design(page_design(spec, raw, steps = list(dimensions = fields[[2]],
                                                         checks_percent = as.character(percents[[1]]))),
    diagonal_arrangement(nrows = size[1], ncols = size[2], lines = 150, checks = 3, l = 2,
                         planter = "cartesian", plotNumber = c(1, 1001), kindExpt = "SUDC", seed = 21,
                         exptName = "D1", locationNames = c("A", "B"), checksPercent = percents[[1]]))
  # an uploaded list, its checks first
  entries <- data.frame(ENTRY = 1:154, NAME = c(paste0("CH", 1:4), paste0("G", 5:154)))
  raw <- utils::modifyList(page_defaults("Diagonal"), list(lines = NA))
  percents <- diagonal_percent_choices(13, 13, 1:4, 154, data = entries)$choices
  expect_same_design(page_design(spec, raw, entries, steps = list(dimensions = "13 x 13")),
    diagonal_arrangement(nrows = 13, ncols = 13, checks = 4, l = 1, plotNumber = 1, kindExpt = "SUDC",
                         seed = 7, exptName = "Expt1", locationNames = "FARGO", data = entries,
                         checksPercent = utils::tail(percents, 1L)))
  shuffled <- entries[c(2, 1, 5, 3, 4, 6:154), ]
  expect_error(page_design(spec, raw, shuffled), "must have consecutive ENTRY numbers",
               class = "fieldhub_input_error")
  expect_error(page_design(spec, utils::modifyList(raw, list(lines = 2))),
               "Insufficient number of entries", class = "fieldhub_input_error")
})

test_that("choices of computed selects follow the entries, typed or uploaded", {
  spec <- design_app_spec("RowCol")
  nrows <- Filter(function(control) identical(control$id, "nrows"), spec$controls)[[1]]
  expect_identical(design_control_choices(spec, nrows, list(t = 42L))$selected, 6L)
  expect_identical(design_control_choices(spec, nrows, list(t = NA), data.frame(ENTRY = 1:12))$choices,
                   c(2L, 3L, 4L, 6L))
  spec <- design_app_spec("Square_Lattice")
  k <- Filter(function(control) identical(control$id, "k"), spec$controls)[[1]]
  expect_identical(design_control_choices(spec, k, list(t = 50L))$choices, "No Options Available")
  raw <- utils::modifyList(page_defaults("Square_Lattice"), list(t = 50, k = "No Options Available"))
  expect_error(page_design(spec, raw), "No options for this combination", class = "fieldhub_input_error")
})

test_that("the page HTML has every control, the shared buttons and no inline style", {
  # Styles the libraries write themselves: shiny's hidden file input and the
  # spinner's placeholder
  library_styles <- c('<input[^>]*class="shiny-input-file"[^>]*>', '<div style="height:400px" class="shiny-spinner-placeholder">')
  for (module in all_modules) {
    spec <- design_app_spec(module)
    html <- as.character(mod_design_ui("x", spec))
    has_id <- function(id) grepl(paste0('id="x-', id, '"'), html, fixed = TRUE)
    for (control in c(spec$controls, spec$steps)) {
      expect_true(has_id(control$id), info = paste(module, control$id))
      if (!is.null(control$label)) {
        expect_true(grepl(htmltools::htmlEscape(control$label), html, fixed = TRUE),
                    info = paste(module, control$id))
      }
    }
    upload <- app_upload_spec(spec$upload)
    ids <- if (identical(spec$kind, "classic")) {
      c(spec$workflow$ids[c("simulate", "field_book_download", "layout_download", "plot", "table")],
        spec$layout$output)
    } else {
      c(spec$workflow$ids[c("simulate", "download", "table", "heatmap", "tabset")], "randomize",
        "status", "setup", paste0("entries_", seq_along(spec$entries)),
        vapply(spec$panels, `[[`, "", "id"), paste0(vapply(spec$steps, `[[`, "", "id"), "_step"))
    }
    for (id in c("run", ids, upload$toggle, upload$file, upload$sep)) {
      expect_true(has_id(id), info = paste(module, id))
    }
    texts <- if (identical(spec$kind, "classic")) {
      c("CSV + metadata (ZIP)", "Field Layout", "Field Book")
    } else {
      c("Get Random", "Randomize!", "Data Input", vapply(spec$panels, `[[`, "", "title"), "Field Book",
        "Heatmap")
    }
    for (text in c(spec$title, "Run!", "Simulate!", "Save experiment (ZIP)", "Import entries' list?",
                   "Upload a CSV File:", texts)) {
      expect_true(grepl(text, html, fixed = TRUE), info = paste(module, text))
    }
    expect_identical(grepl("Summary Design", html, fixed = TRUE), spec$summary, info = module)
    expect_identical(has_id("summary"), spec$summary, info = module)
    stripped <- html
    for (pattern in library_styles) stripped <- gsub(pattern, "", stripped, perl = TRUE)
    expect_false(grepl("style=", stripped, fixed = TRUE), info = module)
    # controls read only for generated entries hide while a file is used
    for (control in Filter(function(control) isTRUE(control$generated_only), spec$controls)) {
      expect_true(grepl(paste0("input.", upload$toggle, " != &#39;Yes&#39;"), html, fixed = TRUE), info = module)
    }
  }
})

test_that("the same concept has the same label, default and minimum on every page", {
  concepts <- fieldhub_control_concepts()
  expect_identical(vapply(concepts, function(concept) {
    if (is.null(concept$label)) NA_character_ else concept$label
  }, character(1)), canonical_labels)
  # Where an engine accepts less than the concept's minimum, the page offers
  # the engine's minimum (checked against the engines below). FD offers the
  # minimum of its CRD type (see design_app_spec()).
  engine_minimums <- c(CRD.t = 1, CRD.reps = 1, LSD.reps = 1, SPD.reps = 1, SSPD.reps = 1,
                       STRIPD.reps = 1, SPD.wp = 1, SSPD.wp = 1, FD.reps = 1)
  # The spatial pages keep the smallest counts they have always offered,
  # and the defaults of their own
  page_minimums <- c(Optim.lines = 5, Diagonal.lines = 50)
  page_values <- c(Optim.plot_start = "1", Optim.rep_checks = "8,8,8,8", pREPS.plot_start = "1",
                   pREPS.repGens = "75,150", pREPS.repUnits = "2,1",
                   RCBD_augmented.plot_start = "1", Diagonal.plot_start = "1")
  seen <- character()
  for (module in all_modules) {
    spec <- design_app_spec(module)
    ids <- vapply(spec$controls, `[[`, character(1), "id")
    expect_identical(anyDuplicated(c(ids, vapply(spec$steps, `[[`, "", "id"))), 0L, info = module)
    # every page has the shared controls, the seed last
    expect_true(all(c("planter", "plot_start", "location_names", "seed") %in% ids), info = module)
    expect_identical(ids[[length(ids)]], "seed", info = module)
    expect_identical("l" %in% ids, "l" %in% names(formals(spec$engine)), info = module)
    expect_identical("location_view" %in% ids, identical(spec$kind, "spatial"), info = module)
    for (control in c(spec$controls, spec$steps)) {
      concept <- concepts[[control$id]]
      info <- paste(module, control$id)
      key <- paste(module, control$id, sep = ".")
      expect_false(is.null(concept), info = info)
      expect_identical(control$label, if (is.na(canonical_labels[[control$id]])) NULL else canonical_labels[[control$id]],
                       info = info)
      if (key %in% names(page_values)) {
        expect_identical(control$value, page_values[[key]], info = info)
        seen <- c(seen, key)
      } else if ("value" %in% names(concept)) {
        expect_identical(control$value, concept$value, info = info)
      }
      if (!is.null(concept$choices)) expect_identical(unname(control$choices), concept$choices, info = info)
      minimums <- c(engine_minimums, page_minimums)
      if (key %in% names(minimums)) {
        expect_identical(control$min, minimums[[key]], info = info)
        seen <- c(seen, key)
      } else {
        expect_identical(control$min, concept$min, info = info)
      }
    }
  }
  expect_setequal(seen, c(names(engine_minimums), names(page_minimums), names(page_values)))
})

test_that("every minimum a page offers is the smallest value its engine accepts", {
  # FD's single replicate is for its CRD type
  at_minimum <- list(FD.reps = list(type = "1"))
  for (module in classic_modules) {
    spec <- design_app_spec(module)
    computed <- any(vapply(spec$controls, function(control) !is.null(control$options), logical(1)))
    for (control in spec$controls) {
      if (!identical(control$type, "number") || is.null(control$min)) next
      info <- paste(module, control$id)
      raw <- utils::modifyList(page_defaults(module), c(list(), at_minimum[[paste(module, control$id, sep = ".")]]))
      if (!is.null(control$enabled_by)) raw[[control$enabled_by]] <- TRUE
      below <- replace(raw, control$id, list(control$min - 1))
      expect_error(page_design(spec, below), class = "fieldhub_input_error", info = info)
      # a treatment count at its minimum leaves no block size to choose
      if (computed && identical(control$id, "t")) next
      at <- replace(raw, control$id, list(control$min))
      built <- tryCatch(page_design(spec, at), error = conditionMessage)
      expect_true(is.data.frame(built$fieldBook), info = paste(info, built))
    }
    maximum <- Filter(function(control) !is.null(control$max), spec$controls)
    for (control in maximum) {
      above <- replace(page_defaults(module), control$id, list(control$max + 1))
      expect_error(page_design(spec, above), class = "fieldhub_input_error", info = module)
    }
  }
})

test_that("only a numeric locations control and whole-number defaults are offered", {
  for (module in all_modules) {
    for (control in design_app_spec(module)$controls) {
      if (identical(control$type, "number")) {
        expect_true(is.numeric(control$value) && control$value >= control$min, info = paste(module, control$id))
      }
    }
  }
})
