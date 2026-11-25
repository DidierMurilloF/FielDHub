library(testthat)
library(FielDHub)

# The generic design page of the app (mod_design_ui(), mod_design_server()
# and the page specs): the controls it reads and the messages it shows, the
# design it builds compared with a direct API call, its registry, seeds,
# sidebar, background jobs and the cached percentages of the diagonal page.
# shiny_shaped() lives in helper-parity.R.

classic_modules <- c("CRD", "RCBD", "LSD", "Latin_Rectangle", "FD", "SPD", "SSPD", "STRIPD", "IBD", "RowCol",
                     "Alpha_Lattice", "Square_Lattice", "Rectangular_Lattice")
spatial_modules <- c("Optim", "pREPS", "RCBD_augmented", "Augmented_RowCol", "Diagonal",
                     "diagonal_multiple", "sparse_allocation", "multi_loc_preps")
all_modules <- c(classic_modules, spatial_modules)

# Shiny's blanks: NULL before an input renders, NA for a cleared
# numericInput, "" for a cleared textInput
blanks <- list(NULL, NA, "", "   ", NA_real_, character())

#' Muffle only the onestage->twostage fallback row_column() may report
quiet_design <- function(expr) {
  withCallingHandlers(expr, fieldhub_design_warning = function(w) invokeRestart("muffleWarning"))
}

#' The values a spatial page sends its engine after its steps (`steps` gives
#' raw step values; a step left out takes the choice the page selects)
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

#' The design a page builds from raw control values (and an uploaded file),
#' as mod_design_server() does on Run! (and, on a spatial page, Randomize!).
#' The values sent to the engine are kept as attribute "values", and the
#' number of fieldhub_default_warning the engine raised as "default_warnings".
page_design <- function(spec, raw, file = NULL, steps = list()) {
  shaped <- if (!is.null(file)) spec$upload_shape(file)
  controls <- read_design_controls(spec, shiny_shaped(raw), uploaded = !is.null(shaped))
  data <- spec$data(shaped, controls)
  values <- spec$values(controls, data)
  if (identical(spec$kind, "spatial")) values <- page_step_values(spec, values, data, steps)
  defaults <- 0L
  design <- withCallingHandlers(quiet_design(do.call(spec$engine, spec$args(values, data))),
    fieldhub_default_warning = function(w) {
      defaults <<- defaults + 1L
      invokeRestart("muffleWarning")
    })
  attr(design, "values") <- values
  attr(design, "default_warnings") <- defaults
  design
}

#' What a design records, with the storage type of every recorded argument:
#' Shiny sends whole numbers as integers, an R user types doubles, and the
#' two must record the same design
design_record <- function(design) {
  list(fieldBook = design$fieldBook, parameters = design$metadata$parameters,
       types = rapply(design$metadata$parameters, typeof, how = "list"))
}

page_defaults <- function(module, seed = 7) {
  raw <- design_control_defaults(design_app_spec(module))
  raw$seed <- seed
  raw
}

#' Raw page controls: the module's defaults changed by `...`
page_raw <- function(module, ..., seed = 7) utils::modifyList(page_defaults(module, seed), list(...))

#' The condition an expression signals, or NULL
problem_of <- function(expr) tryCatch({ force(expr); NULL }, error = function(e) e)

#' One expectation for a set of expressions that must each fail with an
#' input error whose message contains `pattern` (and names `control`)
expect_problems <- function(exprs, pattern, case, control = NULL) {
  problems <- lapply(exprs, function(f) problem_of(f()))
  ok <- vapply(problems, function(e) {
    inherits(e, "fieldhub_input_error") && grepl(pattern, conditionMessage(e), fixed = TRUE) &&
      (is.null(control) || identical(e$control, control))
  }, logical(1))
  bad <- which(!ok)[1]
  expect(all(ok), sprintf("case %s: input %d gave %s", case, bad,
                          if (is.null(problems[[bad]])) "no error" else
                            paste(class(problems[[bad]])[1], conditionMessage(problems[[bad]]),
                                  "for control", format(problems[[bad]]$control))))
}

# --- The app registry -------------------------------------------------------

test_that("the app registry registers every page server and an exported engine for each design", {
  registry <- fieldhub_app_registry()
  namespace <- asNamespace("FielDHub")
  ids <- vapply(registry, `[[`, character(1), "id")
  servers <- vapply(registry, `[[`, character(1), "server")
  ok <- vapply(registry, function(entry) {
    is.function(get(entry$ui, namespace)) && is.function(get(entry$server, namespace)) &&
      entry$engine %in% getNamespaceExports("FielDHub")
  }, logical(1))
  expect(all(ok) && anyDuplicated(ids) == 0L, sprintf("entry %s is not a usable page", ids[!ok][1]))
  expect_setequal(servers, ls(namespace, pattern = "^mod_.*_server$"))
  # every design with a page spec is in the registry
  expect_setequal(unlist(lapply(registry, `[[`, "spec")), all_modules)
})

test_that("Augmented Row-Column follows Augmented RCBD in the unreplicated designs menu", {
  registry <- fieldhub_app_registry()
  labels <- vapply(registry, `[[`, character(1), "label")
  position <- match("Augmented Row-Column", labels)
  expect_identical(c(labels[[position - 1L]], registry[[position]]$group),
                   c("Augmented RCBD", "Unreplicated Designs"))
})

test_that("the app title is derived from package metadata", {
  expect_identical(fieldhub_app_title(), paste0("FielDHub v", utils::packageVersion("FielDHub")))
})

# --- Classic pages: the design of a direct API call ---------------------------

labels5 <- c("No nitrogen", "A*B", "C", "D", "E")
entries9 <- data.frame(ENTRY = 1:9, NAME = paste0("ND-", 1:9))
entries6 <- data.frame(ENTRY = 1:6, NAME = paste0("ND-", 1:6))

classic_cases <- list(
  # each page's defaults, as a direct call with the documented defaults
  "CRD defaults" = list("CRD", page_defaults("CRD"), direct = function()
    CRD(t = 15, reps = 4, plotNumber = 101, locationNames = "FARGO", seed = 7)),
  "RCBD defaults" = list("RCBD", page_defaults("RCBD"), direct = function()
    RCBD(t = 18, reps = 3, l = 1, plotNumber = 101, continuous = TRUE, planter = "serpentine",
         locationNames = "FARGO", seed = 7)),
  "LSD defaults" = list("LSD", page_defaults("LSD"), direct = function()
    latin_square(t = 5, reps = 1, plotNumber = 101, planter = "serpentine", locationNames = "FARGO", seed = 7)),
  "Latin_Rectangle defaults" = list("Latin_Rectangle", page_defaults("Latin_Rectangle"), direct = function()
    latin_rectangle(t = 5, rows = 3, l = 1, plotNumber = 101, planter = "serpentine",
                    locationNames = "FARGO", seed = 7)),
  "FD defaults" = list("FD", page_defaults("FD"), direct = function()
    full_factorial(setfactors = c(2, 2, 3), reps = 3, l = 1, type = 2, plotNumber = 101,
                   planter = "serpentine", locationNames = "FARGO", seed = 7)),
  "SPD defaults" = list("SPD", page_defaults("SPD"), direct = function()
    split_plot(wp = 4, sp = 3, reps = 3, l = 1, type = 2, plotNumber = 101, locationNames = "FARGO", seed = 7)),
  "SSPD defaults" = list("SSPD", page_defaults("SSPD"), direct = function()
    split_split_plot(wp = 2, sp = 2, ssp = 5, reps = 3, l = 1, type = 2, plotNumber = 101,
                     locationNames = "FARGO", seed = 7)),
  "STRIPD defaults" = list("STRIPD", page_defaults("STRIPD"), direct = function()
    strip_plot(Hplots = 5, Vplots = 5, reps = 3, l = 1, planter = "serpentine", plotNumber = 101,
               locationNames = "FARGO", seed = 7, randomizeH = TRUE, randomizeV = TRUE)),
  "IBD defaults" = list("IBD", page_defaults("IBD"), direct = function()
    incomplete_blocks(t = 15, k = 3, reps = 4, l = 1, plotNumber = 101, locationNames = "FARGO", seed = 7)),
  "RowCol defaults" = list("RowCol", page_defaults("RowCol"), direct = function()
    quiet_design(row_column(t = 42, nrows = 6, reps = 2, l = 1, plotNumber = 101,
                            locationNames = "FARGO", seed = 7))),
  "Alpha_Lattice defaults" = list("Alpha_Lattice", page_defaults("Alpha_Lattice"), direct = function()
    alpha_lattice(t = 36, k = 6, reps = 3, l = 1, plotNumber = 101, locationNames = "FARGO", seed = 7)),
  "Square_Lattice defaults" = list("Square_Lattice", page_defaults("Square_Lattice"), direct = function()
    square_lattice(t = 49, k = 7, reps = 3, l = 1, plotNumber = 101, locationNames = "FARGO", seed = 7)),
  "Rectangular_Lattice defaults" = list("Rectangular_Lattice", page_defaults("Rectangular_Lattice"),
    direct = function() rectangular_lattice(t = 30, k = 5, reps = 3, l = 1, plotNumber = 101,
                                            locationNames = "FARGO", seed = 7)),
  # changed controls and two locations
  "Latin_Rectangle changed" = list("Latin_Rectangle", page_raw("Latin_Rectangle", nrows = 4, l = 2,
      plot_start = "1,101", location_names = "Fargo, CASSELTON", planter = "cartesian"),
    direct = function() latin_rectangle(5, rows = 4, l = 2, plotNumber = c(1, 101),
      locationNames = c("Fargo", " CASSELTON"), planter = "cartesian", seed = 7)),
  "RCBD checks, two locations" = list("RCBD", page_raw("RCBD", t = 6, l = 2, planter = "cartesian",
      plot_start = "101,1001", location_names = "A,B", continuous = FALSE, use_checks = TRUE, checks = 2,
      rep_checks = "2", spread_checks = TRUE, seed = 4),
    direct = function() RCBD(t = 6, reps = 3, l = 2, planter = "cartesian", plotNumber = c(101, 1001),
      locationNames = c("A", "B"), continuous = FALSE, seed = 4, checks = 2, rep_checks = c(2, 2),
      spread_checks = TRUE)),
  "SPD in a CRD, two locations" = list("SPD", page_raw("SPD", type = "1", planter = "cartesian", l = 2,
      plot_start = "101,1001", location_names = "A,B", seed = 3),
    direct = function() split_plot(wp = 4, sp = 3, reps = 3, l = 2, type = 1, plotNumber = c(101, 1001),
      locationNames = c("A", "B"), seed = 3)),
  "SPD in a RCBD, two locations" = list("SPD", page_raw("SPD", wp = 4, sp = 2, l = 2,
      plot_start = "101,1001", location_names = "A,B", seed = 8),
    direct = function() split_plot(wp = 4, sp = 2, reps = 3, l = 2, type = 2, plotNumber = c(101, 1001),
      locationNames = c("A", "B"), seed = 8)),
  "STRIPD one replicate" = list("STRIPD", page_raw("STRIPD", randomizeV = FALSE, reps = 1),
    direct = function() strip_plot(Hplots = 5, Vplots = 5, reps = 1, l = 1, plotNumber = 101,
      locationNames = "FARGO", seed = 7, randomizeH = TRUE, randomizeV = FALSE)),
  "STRIPD two locations" = list("STRIPD", page_raw("STRIPD", Hplots = 4, Vplots = 3, reps = 2, l = 2,
      planter = "cartesian", plot_start = "101,1001", location_names = "A,B", randomizeV = FALSE, seed = 12),
    direct = function() strip_plot(Hplots = 4, Vplots = 3, reps = 2, l = 2, planter = "cartesian",
      plotNumber = c(101, 1001), locationNames = c("A", "B"), seed = 12, randomizeH = TRUE,
      randomizeV = FALSE)),
  "LSD changed" = list("LSD", page_raw("LSD", reps = 2, planter = "cartesian", location_names = "Loc1"),
    direct = function() latin_square(t = 5, reps = 2, plotNumber = 101, planter = "cartesian",
      locationNames = "Loc1", seed = 7)),
  "FD two locations" = list("FD", page_raw("FD", setfactors = "2,3", reps = 2, l = 2,
      plot_start = "101,1001", location_names = "A,B", seed = 5),
    direct = function() full_factorial(setfactors = c(2, 3), reps = 2, l = 2, type = 2,
      plotNumber = c(101, 1001), planter = "serpentine", locationNames = c("A", "B"), seed = 5)),
  "SSPD two locations" = list("SSPD", page_raw("SSPD", ssp = 3, reps = 2, l = 2,
      plot_start = "101,1001", location_names = "A,B", seed = 6),
    direct = function() split_split_plot(wp = 2, sp = 2, ssp = 3, reps = 2, l = 2, type = 2,
      plotNumber = c(101, 1001), locationNames = c("A", "B"), seed = 6)),
  "IBD two locations" = list("IBD", page_raw("IBD", t = 12, k = "4", reps = 2, l = 2,
      plot_start = "101,1001", location_names = "A,B", seed = 9),
    direct = function() incomplete_blocks(t = 12, k = 4, reps = 2, l = 2, plotNumber = c(101, 1001),
      locationNames = c("A", "B"), seed = 9)),
  "RowCol two locations" = list("RowCol", page_raw("RowCol", t = 9, nrows = "3", reps = 2, l = 2,
      plot_start = "101,1001", location_names = "A,B", seed = 14),
    direct = function() quiet_design(row_column(t = 9, nrows = 3, reps = 2, l = 2,
      plotNumber = c(101, 1001), locationNames = c("A", "B"), seed = 14))),
  "Alpha_Lattice two locations" = list("Alpha_Lattice", page_raw("Alpha_Lattice", t = 9, k = "3", reps = 2,
      l = 2, plot_start = "101,1001", location_names = "A,B", seed = 1),
    direct = function() alpha_lattice(t = 9, k = 3, reps = 2, l = 2, plotNumber = c(101, 1001),
      locationNames = c("A", "B"), seed = 1)),
  "Square_Lattice two locations" = list("Square_Lattice", page_raw("Square_Lattice", t = 9, k = "3",
      reps = 2, l = 2, plot_start = "101,1001", location_names = "A,B", seed = 1),
    direct = function() square_lattice(t = 9, k = 3, reps = 2, l = 2, plotNumber = c(101, 1001),
      locationNames = c("A", "B"), seed = 1)),
  "Rectangular_Lattice two locations" = list("Rectangular_Lattice", page_raw("Rectangular_Lattice", t = 6,
      k = "2", reps = 2, l = 2, plot_start = "101,1001", location_names = "A,B", seed = 1),
    direct = function() rectangular_lattice(t = 6, k = 2, reps = 2, l = 2, plotNumber = c(101, 1001),
      locationNames = c("A", "B"), seed = 1)),
  # an uploaded file
  "Latin_Rectangle upload" = list("Latin_Rectangle", page_raw("Latin_Rectangle", t = NA),
    file = data.frame(TREATMENT = labels5),
    direct = function() latin_rectangle(labels5, rows = 3, locationNames = "FARGO", seed = 7)),
  "IBD upload with an extra column" = list("IBD", page_raw("IBD", t = NA, k = "3"),
    file = data.frame(ENTRY = 1:12, NAME = paste0("G", 1:12), NOTE = "x"),
    direct = function() incomplete_blocks(t = 12, k = 3, reps = 4, l = 1, plotNumber = 101,
      locationNames = "FARGO", seed = 7, data = data.frame(ENTRY = 1:12, NAME = paste0("G", 1:12)))),
  # CRD(data = ) records no treatment count
  "CRD upload" = list("CRD", page_raw("CRD", t = NA), file = data.frame(TREATMENT = c(paste0("T", 1:5), NA)),
    direct = function() CRD(reps = 4, plotNumber = 101, locationNames = "FARGO", seed = 7,
      data = data.frame(TREATMENT = paste0("T", 1:5), REP = 4L))),
  "STRIPD upload" = list("STRIPD", page_raw("STRIPD", Hplots = NA, Vplots = NA),
    file = data.frame(Hplot = c("H1", "H2", "H3"), Vplot = c("V1", "V2", NA)),
    direct = function() strip_plot(Hplots = 3, Vplots = 2, reps = 3, l = 1, plotNumber = 101,
      locationNames = "FARGO", seed = 7, randomizeH = TRUE, randomizeV = TRUE,
      data = data.frame(Hplot = c("H1", "H2", "H3"), Vplot = c("V1", "V2", NA)))),
  "SPD upload" = list("SPD", page_raw("SPD", wp = NA, sp = NA),
    file = data.frame(WHOLEPLOT = c("W1", "W2", "W3"), SUBPLOT = c("S1", "S2", NA)),
    direct = function() split_plot(reps = 3, l = 1, type = 2, plotNumber = 101, locationNames = "FARGO",
      seed = 7, data = data.frame(WHOLEPLOT = c("W1", "W2", "W3"), SUBPLOT = c("S1", "S2", NA)))),
  "RCBD upload" = list("RCBD", page_raw("RCBD", t = NA, continuous = FALSE, location_names = "IBAGUE",
      seed = 13), file = data.frame(TREATMENT = paste0("ND-", 1:6)),
    direct = function() RCBD(reps = 3, l = 1, plotNumber = 101, continuous = FALSE, planter = "serpentine",
      locationNames = "IBAGUE", seed = 13, data = data.frame(TREATMENT = paste0("ND-", 1:6)))),
  "LSD upload" = list("LSD", page_raw("LSD", t = NA, location_names = "A", seed = 9),
    file = data.frame(ROW = paste0("Period", 1:4), COLUMN = paste0("Cow", 1:4), TREATMENT = paste0("Diet", 1:4)),
    direct = function() latin_square(reps = 1, plotNumber = 101, planter = "serpentine", locationNames = "A",
      seed = 9, data = data.frame(ROW = paste0("Period", 1:4), COLUMN = paste0("Cow", 1:4),
                                  TREATMENT = paste0("Diet", 1:4)))),
  "FD upload in a CRD" = list("FD", page_raw("FD", type = "1", reps = 2, location_names = "Loc", seed = 3),
    file = data.frame(FACTOR = c("A", "A", "B", "B"), LEVEL = c("a0", "a1", "b0", "b1")),
    direct = function() full_factorial(reps = 2, l = 1, type = 1, plotNumber = 101, planter = "serpentine",
      locationNames = "Loc", seed = 3,
      data = data.frame(FACTOR = c("A", "A", "B", "B"), LEVEL = c("a0", "a1", "b0", "b1")))),
  "SSPD upload in a CRD" = list("SSPD", page_raw("SSPD", type = "1", wp = NA, sp = NA, ssp = NA, reps = 1,
      location_names = "Loc", seed = 2),
    file = data.frame(WHOLEPLOT = c("A", "B", NA), SUBPLOT = c("s1", "s2", NA), SUB_SUBPLOT = c("x1", "x2", "x3")),
    direct = function() split_split_plot(reps = 1, l = 1, type = 1, plotNumber = 101, locationNames = "Loc",
      seed = 2, data = data.frame(WHOLEPLOT = c("A", "B", NA), SUBPLOT = c("s1", "s2", NA),
                                  SUB_SUBPLOT = c("x1", "x2", "x3")))),
  "RowCol upload" = list("RowCol", page_raw("RowCol", t = NA, nrows = "3", location_names = "A", seed = 5),
    file = entries9, direct = function() quiet_design(row_column(t = 9, nrows = 3, reps = 2, l = 1,
      plotNumber = 101, locationNames = "A", seed = 5, data = entries9))),
  "Alpha_Lattice upload" = list("Alpha_Lattice", page_raw("Alpha_Lattice", t = NA, k = "3", reps = 2,
      location_names = "Loc", seed = 1), file = entries9,
    direct = function() alpha_lattice(t = 9, k = 3, reps = 2, l = 1, plotNumber = 101, locationNames = "Loc",
      seed = 1, data = entries9)),
  "Square_Lattice upload" = list("Square_Lattice", page_raw("Square_Lattice", t = NA, k = "3", reps = 2,
      location_names = "Loc", seed = 1), file = entries9,
    direct = function() square_lattice(t = 9, k = 3, reps = 2, l = 1, plotNumber = 101, locationNames = "Loc",
      seed = 1, data = entries9)),
  "Rectangular_Lattice upload" = list("Rectangular_Lattice", page_raw("Rectangular_Lattice", t = NA, k = "2",
      reps = 2, location_names = "Loc", seed = 1), file = entries6,
    direct = function() rectangular_lattice(t = 6, k = 2, reps = 2, l = 1, plotNumber = 101,
      locationNames = "Loc", seed = 1, data = entries6))
)

test_that("classic pages build the design of the direct API call with the same inputs", {
  for (name in names(classic_cases)) {
    case <- classic_cases[[name]]
    built <- page_design(design_app_spec(case[[1]]), case[[2]], case$file)
    expect_equal(c(design_record(built), default_warnings = attr(built, "default_warnings")),
                 c(design_record(case$direct()), default_warnings = 0L), info = name)
  }
})

# --- Spatial pages: controls -> (allocation) -> steps -> engine --------------

arc_upload <- data.frame(ENTRY = 1:52, NAME = c(paste0("CHECK", 1:4), paste0("SB-", 5:52)))
optim_upload <- data.frame(ENTRY = 1:104, NAME = c(paste0("CHECK", 1:4), paste0("SB-", 5:104)),
                           REPS = c(4L, 4L, 6L, 6L, rep(1L, 100)))
prep_upload <- data.frame(ENTRY = 1:120, NAME = paste0("E", 1:120), REPS = rep(c(2L, 1L), c(40, 80)))
arcbd_upload <- data.frame(ENTRY = 1:64, NAME = c(paste0("CH", 1:4), paste0("G", 5:64)))
arcbd_stacked_upload <- data.frame(ENTRY = 1:43, NAME = c(paste0("CHECK", 1:3), paste0("SB-", 4:43)))
diagonal_upload <- data.frame(ENTRY = 1:154, NAME = c(paste0("CH", 1:4), paste0("G", 5:154)))
multiple_upload <- data.frame(ENTRY = 1:304, NAME = c(paste0("CH", 1:4), paste0("G", 5:304)))
sparse_upload <- data.frame(ENTRY = 1:139, NAME = c(paste0("CHECK", 1:4), paste0("SB-", 5:139)))
mlp_upload <- data.frame(ENTRY = 101:140, NAME = paste0("SB-", 101:140))

# A direct call takes `v`, the values the page sent after its steps: rows
# that leave a step to the page read the chosen field size or percentage
# of checks from it (the choices themselves are checked further down)
spatial_cases <- list(
  # each page's defaults (the check direction has no default: the row
  # chooses it, and a small search budget)
  "Optim defaults" = list("Optim", page_defaults("Optim"), direct = function(v)
    optimized_arrangement(nrows = 13, ncols = 24, lines = 280, checks = 4, rep_checks = c(8, 8, 8, 8), l = 1,
                          plotNumber = 1, seed = 7, exptName = "Expt1", locationNames = "FARGO")),
  "pREPS defaults" = list("pREPS", page_defaults("pREPS"), direct = function(v)
    partially_replicated(nrows = 15, ncols = 20, repGens = c(75, 150), repUnits = c(2, 1), l = 1, plotNumber = 1,
                         seed = 7, exptName = "Expt1", locationNames = "FARGO", allow_fillers = TRUE,
                         rowcol_penalty = Inf)),
  "RCBD_augmented defaults" = list("RCBD_augmented", page_defaults("RCBD_augmented"), direct = function(v)
    RCBD_augmented(lines = 180, checks = 4, b = 3, nrows = 12, ncols = 16, l = 1, plotNumber = 1, seed = 7,
                   exptName = "Expt1", locationNames = "FARGO")),
  "Augmented_RowCol defaults" = list("Augmented_RowCol",
    page_raw("Augmented_RowCol", check_direction = "columns", advanced = TRUE, budget = "300"),
    direct = function(v) augmented_row_column(nrows = 10, ncols = 10, checks = 4, check_direction = "columns",
      entries = 60, objective = "V_tt", budget = 300, l = 1, plotNumber = 1, locationNames = "FARGO",
      exptName = "Expt1", seed = 7)),
  "Diagonal defaults" = list("Diagonal", page_defaults("Diagonal"), slow = TRUE, direct = function(v)
    diagonal_arrangement(nrows = 18, ncols = 18, lines = 287, checks = 4, l = 1, plotNumber = 1,
                         kindExpt = "SUDC", seed = 7, exptName = "Expt1", locationNames = "FARGO",
                         checksPercent = 11.11, spread_checks = TRUE)),
  "diagonal_multiple defaults" = list("diagonal_multiple", page_defaults("diagonal_multiple"),
    slow = TRUE, direct = function(v)
    diagonal_arrangement(nrows = 19, ncols = 18, lines = 300, checks = 4, l = 1, plotNumber = 1,
                         kindExpt = "DBUDC", splitBy = "row", seed = 7, blocks = c(100, 120, 80),
                         exptName = c("Expt1", "Expt2", "Expt3"), locationNames = "FARGO",
                         checksPercent = 12.3, spread_checks = TRUE)),
  # "FARGO" names one of five locations: the engine's LOC1, ... are used
  "sparse_allocation defaults" = list("sparse_allocation", page_defaults("sparse_allocation"),
    slow = TRUE, direct = function(v)
    sparse_allocation(lines = 380, nrows = 19, ncols = 19, l = 5, plotNumber = c(1, 1001, 2001, 3001, 4001),
                      copies_per_entry = 4, checks = 4, exptName = "Expt1",
                      sparse_list = do_optim(design = "sparse", lines = 380, l = 5, copies_per_entry = 4,
                                             add_checks = TRUE, checks = 4, seed = 7),
                      seed = 7, checksPercent = 14.13, spread_checks = TRUE)),
  "multi_loc_preps defaults" = list("multi_loc_preps", page_defaults("multi_loc_preps"),
    slow = TRUE, direct = function(v)
    multi_location_prep(lines = 312, nrows = rep(26, 6), ncols = rep(16, 6), l = 6,
                        plotNumber = c(1, 1001, 2001, 3001, 4001, 5001), copies_per_entry = 8, checks = NULL,
                        rep_checks = NULL, exptName = "Expt1",
                        optim_list = do_optim(design = "prep", lines = 312, l = 6, copies_per_entry = 8,
                                              add_checks = FALSE, seed = 7),
                        seed = 7, allow_fillers = TRUE, rowcol_penalty = Inf)),
  # changed controls, chosen steps, uploads
  "Optim two locations" = list("Optim", page_raw("Optim", lines = 100, rep_checks = "5", l = 2,
      planter = "cartesian", plot_start = "1,1001", location_names = "A,B", expt_name = "Trial 1", seed = 5),
    steps = list(dimensions = "12 x 10"),
    direct = function(v) optimized_arrangement(nrows = 12, ncols = 10, lines = 100, checks = 4,
      rep_checks = c(5, 5, 5, 5), l = 2, planter = "cartesian", plotNumber = c(1, 1001), seed = 5,
      exptName = "Trial 1", locationNames = c("A", "B"))),
  "Optim upload" = list("Optim", page_raw("Optim", checks = NA, rep_checks = "", lines = NA),
    file = optim_upload, steps = list(dimensions = "12 x 10"),
    direct = function(v) optimized_arrangement(nrows = 12, ncols = 10, plotNumber = 1, seed = 7,
      exptName = "Expt1", locationNames = "FARGO", data = optim_upload)),
  # 301 plots only fit a 7 x 43 field; filler plots offer squarer ones
  "pREPS filler plots, two locations" = list("pREPS", page_raw("pREPS", repGens = "75,151", repUnits = "2,1",
      allow_fillers = TRUE, l = 2, planter = "cartesian", plot_start = "1,501", location_names = "A,B",
      expt_name = "P1", seed = 11), steps = list(dimensions = "16 x 19"),
    direct = function(v) partially_replicated(nrows = 16, ncols = 19, repGens = c(75, 151), repUnits = c(2, 1),
      l = 2, planter = "cartesian", plotNumber = c(1, 501), seed = 11, exptName = "P1",
      locationNames = c("A", "B"), allow_fillers = TRUE, rowcol_penalty = Inf)),
  "pREPS upload" = list("pREPS", page_raw("pREPS", repGens = "", repUnits = NA), file = prep_upload,
    steps = list(dimensions = "16 x 10"),
    direct = function(v) partially_replicated(nrows = 16, ncols = 10, plotNumber = 1, seed = 7,
      exptName = "Expt1", locationNames = "FARGO", data = prep_upload, allow_fillers = TRUE,
      rowcol_penalty = Inf)),
  "RCBD_augmented stacked, two locations" = list("RCBD_augmented", page_raw("RCBD_augmented", lines = 60,
      checks = 3, b = "5", repsExpt = 2, repsStack = "horizontal", random = FALSE, l = 2, planter = "cartesian",
      plot_start = "1,1001", expt_name = "E1,E2", location_names = "A,B", seed = 3),
    direct = function(v) RCBD_augmented(lines = 60, checks = 3, b = 5, repsExpt = 2, repsStack = "horizontal",
      random = FALSE, l = 2, planter = "cartesian", plotNumber = c(1, 1001), exptName = c("E1", "E2"),
      locationNames = c("A", "B"), seed = 3, nrows = v$nrows, ncols = v$ncols)),
  # an uploaded list: its rows after the checks are the entries
  "RCBD_augmented upload" = list("RCBD_augmented", page_raw("RCBD_augmented", lines = NA, b = "4"),
    file = arcbd_upload, steps = list(dimensions = "4 x 19"),
    direct = function(v) RCBD_augmented(lines = 60, checks = 4, b = 4, nrows = 4, ncols = 19, l = 1,
      plotNumber = 1, seed = 7, exptName = "Expt1", locationNames = "FARGO", data = arcbd_upload)),
  "RCBD_augmented upload, two stacked experiments" = list("RCBD_augmented", page_raw("RCBD_augmented",
      lines = NA, checks = 3, b = "4", repsExpt = 2, repsStack = "horizontal", random = FALSE,
      planter = "cartesian", plot_start = "101,201", expt_name = "E1,E2", location_names = "CASSELTON",
      seed = 29), file = arcbd_stacked_upload,
    direct = function(v) RCBD_augmented(lines = 40, checks = 3, b = 4, l = 1, planter = "cartesian",
      plotNumber = c(101, 201), repsStack = "horizontal", exptName = c("E1", "E2"), seed = 29,
      locationNames = "CASSELTON", repsExpt = 2, random = FALSE, data = arcbd_stacked_upload,
      nrows = v$nrows, ncols = v$ncols)),
  "Augmented_RowCol two locations" = list("Augmented_RowCol", page_raw("Augmented_RowCol", nrows = 10,
      ncols = 8, lines = 48, check_direction = "columns", objective = "E_aug", advanced = TRUE, budget = "300",
      l = 2, planter = "cartesian", plot_start = "1,1001", expt_name = "Trial 1", year = "2030",
      location_names = "fargo,minot", seed = 3),
    direct = function(v) augmented_row_column(nrows = 10, ncols = 8, checks = 4, check_direction = "columns",
      entries = 48, objective = "E_aug", budget = 300, l = 2, plotNumber = c(1, 1001), planter = "cartesian",
      locationNames = c("fargo", "minot"), exptName = "Trial 1", year = 2030, seed = 3)),
  "Augmented_RowCol rows and a time limit" = list("Augmented_RowCol", page_raw("Augmented_RowCol", nrows = 8,
      ncols = 10, lines = 48, check_direction = "rows", objective = "V_tc", advanced = TRUE, budget = "300",
      time_limit = "600", seed = 5),
    direct = function(v) augmented_row_column(nrows = 8, ncols = 10, checks = 4, check_direction = "rows",
      entries = 48, objective = "V_tc", budget = 300, time_limit = 600, plotNumber = 1,
      locationNames = "FARGO", exptName = "Expt1", seed = 5)),
  # the entries come from the file, a typed count is not sent beside it
  "Augmented_RowCol upload" = list("Augmented_RowCol", page_raw("Augmented_RowCol", nrows = 10, ncols = 8,
      lines = 48, check_direction = "columns", advanced = TRUE, budget = "300", location_names = "CASSELTON",
      seed = 29), file = arc_upload,
    direct = function(v) augmented_row_column(nrows = 10, ncols = 8, checks = 4, check_direction = "columns",
      objective = "V_tt", budget = 300, plotNumber = 1, locationNames = "CASSELTON", exptName = "Expt1",
      seed = 29, data = arc_upload)),
  "Diagonal two locations" = list("Diagonal", page_raw("Diagonal", lines = 150, checks = 3, l = 2,
      planter = "cartesian", plot_start = "1,1001", location_names = "A,B", expt_name = "D1", seed = 21),
    direct = function(v) diagonal_arrangement(nrows = v$nrows, ncols = v$ncols, lines = 150, checks = 3, l = 2,
      planter = "cartesian", plotNumber = c(1, 1001), kindExpt = "SUDC", seed = 21, exptName = "D1",
      locationNames = c("A", "B"), checksPercent = v$checksPercent, spread_checks = TRUE)),
  "Diagonal upload" = list("Diagonal", page_raw("Diagonal", lines = NA), file = diagonal_upload,
    steps = list(dimensions = "13 x 13"),
    direct = function(v) diagonal_arrangement(nrows = 13, ncols = 13, checks = 4, l = 1, plotNumber = 1,
      kindExpt = "SUDC", seed = 7, exptName = "Expt1", locationNames = "FARGO", data = diagonal_upload,
      checksPercent = v$checksPercent, spread_checks = TRUE)),
  # one plot start for two locations: the engine's own starts, silently
  "Diagonal misfit plot starts and names" = list("Diagonal", page_raw("Diagonal", lines = 150, checks = 3, l = 2,
      plot_start = "5", expt_name = "E", location_names = "ONLY", seed = 3),
    direct = function(v) diagonal_arrangement(nrows = v$nrows, ncols = v$ncols, lines = 150, checks = 3, l = 2,
      plotNumber = c(1001, 2001), seed = 3, exptName = "E", checksPercent = v$checksPercent,
      spread_checks = TRUE)),
  "diagonal_multiple repeated entries, by column" = list("diagonal_multiple", page_raw("diagonal_multiple",
      lines = 240, blocks = "80,80,80", sameEntries = TRUE, checks = 3, stacked = "By Column", l = 2,
      planter = "cartesian", plot_start = "1,101,201", expt_name = "A, B ,C", location_names = "X, Y", seed = 5),
    direct = function(v) diagonal_arrangement(nrows = v$nrows, ncols = v$ncols, lines = 240, checks = 3, l = 2,
      planter = "cartesian", plotNumber = list(c(1, 101, 201), c(1, 101, 201)), kindExpt = "DBUDC",
      splitBy = "column", seed = 5, blocks = c(80, 80, 80), exptName = c("A", "B", "C"),
      locationNames = c("X", "Y"), checksPercent = v$checksPercent, sameEntries = TRUE, spread_checks = TRUE)),
  "diagonal_multiple upload" = list("diagonal_multiple", page_raw("diagonal_multiple", lines = NA), slow = TRUE,
    file = multiple_upload, steps = list(dimensions = "19 x 18"),
    direct = function(v) diagonal_arrangement(nrows = 19, ncols = 18, checks = 4, l = 1, plotNumber = 1,
      kindExpt = "DBUDC", splitBy = "row", seed = 7, blocks = c(100, 120, 80),
      exptName = c("Expt1", "Expt2", "Expt3"), locationNames = "FARGO", data = multiple_upload,
      checksPercent = v$checksPercent, spread_checks = TRUE)),
  "diagonal_multiple upload, by column" = list("diagonal_multiple", page_raw("diagonal_multiple", lines = NA,
      blocks = "50,50,50", planter = "cartesian", plot_start = "101", stacked = "By Column", expt_name = "ONE",
      location_names = "CASSELTON", seed = 11), file = diagonal_upload,
    direct = function(v) diagonal_arrangement(nrows = v$nrows, ncols = v$ncols, checks = 4, l = 1,
      planter = "cartesian", plotNumber = 101, kindExpt = "DBUDC", splitBy = "column", seed = 11,
      blocks = c(50, 50, 50), exptName = "ONE", locationNames = "CASSELTON", data = diagonal_upload,
      checksPercent = v$checksPercent, spread_checks = TRUE)),
  # one start: every location starts there
  "diagonal_multiple one plot start" = list("diagonal_multiple", page_raw("diagonal_multiple", lines = 150,
      blocks = "50,50,50", checks = 3, l = 2, plot_start = "5", location_names = "A,B", seed = 3),
    direct = function(v) diagonal_arrangement(nrows = v$nrows, ncols = v$ncols, lines = 150, checks = 3, l = 2,
      plotNumber = c(5, 5), kindExpt = "DBUDC", splitBy = "row", seed = 3, blocks = c(50, 50, 50),
      exptName = c("Expt1", "Expt2", "Expt3"), locationNames = c("A", "B"), checksPercent = v$checksPercent,
      spread_checks = TRUE)),
  # neither one start nor one per experiment: the engine's own starts, silently
  "diagonal_multiple misfit plot starts and names" = list("diagonal_multiple", page_raw("diagonal_multiple",
      lines = 150, blocks = "50,50,50", checks = 3, l = 2, plot_start = "1,2", location_names = "ONLY", seed = 3),
    direct = function(v) diagonal_arrangement(nrows = v$nrows, ncols = v$ncols, lines = 150, checks = 3, l = 2,
      plotNumber = c(1001, 2001), kindExpt = "DBUDC", splitBy = "row", seed = 3, blocks = c(50, 50, 50),
      exptName = c("Expt1", "Expt2", "Expt3"), checksPercent = v$checksPercent, spread_checks = TRUE)),
  "sparse_allocation three locations" = list("sparse_allocation", page_raw("sparse_allocation", lines = 120,
      checks = 2, l = 3, copies_per_entry = "2", planter = "cartesian", plot_start = "1,501,1001",
      location_names = "A,B,C", expt_name = "S1", seed = 9),
    direct = function(v) sparse_allocation(lines = 120, nrows = v$nrows, ncols = v$ncols, l = 3,
      planter = "cartesian", plotNumber = c(1, 501, 1001), copies_per_entry = 2, checks = 2, exptName = "S1",
      locationNames = c("A", "B", "C"),
      sparse_list = do_optim(design = "sparse", lines = 120, l = 3, copies_per_entry = 2, add_checks = TRUE,
                             checks = 2, seed = 9),
      seed = 9, checksPercent = v$checksPercent, spread_checks = TRUE)),
  "sparse_allocation upload" = list("sparse_allocation", page_raw("sparse_allocation", lines = 135, l = 3,
      copies_per_entry = "2", plot_start = "101,201,301", expt_name = "E", location_names = "X,Y,Z", seed = 8),
    file = sparse_upload,
    direct = function(v) sparse_allocation(lines = 135, nrows = v$nrows, ncols = v$ncols, l = 3,
      plotNumber = c(101, 201, 301), copies_per_entry = 2, checks = 4, exptName = "E",
      locationNames = c("X", "Y", "Z"),
      sparse_list = do_optim(design = "sparse", lines = 135, l = 3, copies_per_entry = 2, add_checks = TRUE,
                             checks = 4, seed = 8, data = sparse_upload),
      seed = 8, data = sparse_upload, checksPercent = v$checksPercent, spread_checks = TRUE)),
  "sparse_allocation misfit plot starts and names" = list("sparse_allocation", page_raw("sparse_allocation",
      lines = 135, l = 3, copies_per_entry = "2", plot_start = "1001", location_names = "ONLY", seed = 5),
    direct = function(v) sparse_allocation(lines = 135, nrows = v$nrows, ncols = v$ncols, l = 3,
      plotNumber = c(1, 1001, 2001), copies_per_entry = 2, checks = 4, exptName = "Expt1",
      sparse_list = do_optim(design = "sparse", lines = 135, l = 3, copies_per_entry = 2, add_checks = TRUE,
                             checks = 4, seed = 5),
      seed = 5, checksPercent = v$checksPercent, spread_checks = TRUE),
    property = function(d) identical(unique(d$fieldBook$LOCATION), paste0("LOC", 1:3))),
  "multi_loc_preps checks and a field per location" = list("multi_loc_preps", page_raw("multi_loc_preps",
      lines = 150, use_checks = TRUE, checks = 2, rep_checks = "6", l = 3, copies_per_entry = "4",
      allow_fillers = TRUE, planter = "cartesian", plot_start = "1,501,1001", location_names = "A,B,C",
      expt_name = "M1", seed = 13), steps = list(multi_dimension = TRUE),
    direct = function(v) multi_location_prep(lines = 150, nrows = v$nrows, ncols = v$ncols, l = 3,
      planter = "cartesian", plotNumber = c(1, 501, 1001), copies_per_entry = 4, checks = 2, rep_checks = c(6, 6),
      exptName = "M1", locationNames = c("A", "B", "C"),
      optim_list = do_optim(design = "prep", lines = 150, l = 3, copies_per_entry = 4, add_checks = TRUE,
                            checks = 2, rep_checks = c(6, 6), seed = 13),
      seed = 13, allow_fillers = TRUE, rowcol_penalty = Inf)),
  # the engine merges the uploaded names into the allocation
  "multi_loc_preps upload" = list("multi_loc_preps", page_raw("multi_loc_preps", lines = 40, l = 2,
      copies_per_entry = "3", plot_start = "1,1001", expt_name = "MET", location_names = "A,B", seed = 12),
    file = mlp_upload,
    direct = function(v) multi_location_prep(lines = 40, nrows = v$nrows, ncols = v$ncols, l = 2,
      plotNumber = c(1, 1001), copies_per_entry = 3, exptName = "MET", locationNames = c("A", "B"),
      optim_list = do_optim(design = "prep", lines = 40, l = 2, copies_per_entry = 3, add_checks = FALSE,
                            seed = 12, data = mlp_upload),
      seed = 12, data = mlp_upload, allow_fillers = TRUE, rowcol_penalty = Inf),
    property = function(d) all(d$fieldBook$TREATMENT[d$fieldBook$ENTRY > 0] %in% mlp_upload$NAME)),
  "multi_loc_preps misfit plot starts and names" = list("multi_loc_preps", page_raw("multi_loc_preps",
      lines = 40, l = 2, copies_per_entry = "3", plot_start = "1001", location_names = "ONLY", seed = 7),
    direct = function(v) multi_location_prep(lines = 40, nrows = v$nrows, ncols = v$ncols, l = 2,
      plotNumber = c(1, 1001), copies_per_entry = 3, exptName = "Expt1",
      optim_list = do_optim(design = "prep", lines = 40, l = 2, copies_per_entry = 3, add_checks = FALSE,
                            seed = 7),
      seed = 7, allow_fillers = TRUE, rowcol_penalty = Inf),
    property = function(d) identical(unique(d$fieldBook$LOCATION), paste0("LOC", 1:2)))
)

expect_spatial_parity <- function(cases) {
  for (name in names(cases)) {
    case <- cases[[name]]
    built <- page_design(design_app_spec(case[[1]]), case[[2]], case$file, steps = case$steps %||% list())
    direct <- case$direct(attr(built, "values"))
    expect_equal(c(design_record(built), default_warnings = attr(built, "default_warnings"),
                   property = is.null(case$property) || case$property(built)),
                 c(design_record(direct), default_warnings = 0L, property = TRUE), info = name)
  }
}
slow <- vapply(spatial_cases, function(case) isTRUE(case$slow), logical(1))

test_that("spatial pages build the design of the direct API call with the same inputs and steps", {
  expect_spatial_parity(spatial_cases[!slow])
})

test_that("the larger spatial pages' defaults build the design of the direct API call", {
  # their default fields take seconds to build
  skip_on_cran()
  expect_spatial_parity(spatial_cases[slow])
})

test_that("an integer from Shiny and a typed double record the same design", {
  # The engines record an argument as given: 270L and 270 record different
  # parameters, so the page must send doubles whichever Shiny delivered
  typed <- diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4, plotNumber = 101, seed = 24)
  from_integer <- diagonal_arrangement(nrows = 15, ncols = 20, lines = 270L, checks = 4, plotNumber = 101,
                                       seed = 24)
  expect_false(identical(from_integer$metadata$parameters, typed$metadata$parameters))
  values <- list(nrows = 15, ncols = 20, lines = 270, checks = 4, plot_start = 101, seed = 24)
  via_integer <- do.call(diagonal_arrangement, design_args_Diagonal(shiny_shaped(values)))
  expect_identical(rapply(via_integer$metadata$parameters, typeof, how = "list"),
                   rapply(typed$metadata$parameters, typeof, how = "list"))
})

# --- What the pages offer, and the problems they report ---------------------

test_that("computed selects offer the choices of the entries, typed or uploaded", {
  choices_of <- function(module, id, raw, data = NULL) {
    spec <- design_app_spec(module)
    control <- Filter(function(control) identical(control$id, id), spec$controls)[[1]]
    design_control_choices(spec, control, shiny_shaped(raw), data)
  }
  cases <- list(
    "RowCol typed" = list(choices_of("RowCol", "nrows", list(t = 42L))$selected, 6L),
    "RowCol uploaded" = list(choices_of("RowCol", "nrows", list(t = NA), data.frame(ENTRY = 1:12))$choices,
                             c(2L, 3L, 4L, 6L)),
    "Square_Lattice no fit" = list(choices_of("Square_Lattice", "k", list(t = 50L))$choices,
                                   "No Options Available"),
    "RCBD_augmented typed" = list(choices_of("RCBD_augmented", "b", list(lines = 180, checks = 4)),
                                  augmented_block_choices(180, 4)),
    "RCBD_augmented uploaded" = list(choices_of("RCBD_augmented", "b", list(lines = NA, checks = 4),
                                                arcbd_upload), augmented_block_choices(60, 4)),
    "sparse_allocation copies" = list(choices_of("sparse_allocation", "copies_per_entry", list(l = 4)),
                                      list(choices = 1:3, selected = 3L)),
    "multi_loc_preps copies" = list(choices_of("multi_loc_preps", "copies_per_entry", list(l = 4)),
                                    prep_copies_choices(4))
  )
  for (name in names(cases)) expect_equal(cases[[name]][[1]], cases[[name]][[2]], info = name)
})

test_that("allocation pages show one row per entry, named from an upload", {
  spec <- design_app_spec("sparse_allocation")
  raw <- page_raw("sparse_allocation", lines = 120, checks = 2, l = 3, copies_per_entry = "2", seed = 9)
  values <- spec$values(read_design_controls(spec, shiny_shaped(raw)), NULL)
  allocation <- do_optim(design = "sparse", lines = 120, l = 3, copies_per_entry = 2, add_checks = TRUE,
                         checks = 2, seed = 9)
  view <- spec$setup$view(c(values, list(sparse_list = allocation)), list())
  expect_equal(list(dim(view), rownames(view)[121]), list(c(121L, 4L), "Total"))
  upload <- data.frame(ENTRY = 1:122, NAME = c("CH1", "CH2", paste0("Line", 3:122)))
  values <- spec$values(read_design_controls(spec, shiny_shaped(raw), uploaded = TRUE), spec$upload_shape(upload))
  expect_identical(rownames(spec$setup$view(c(values, list(sparse_list = allocation)), list()))[1], "Line3")
  # the multi-location p-rep table averages the copies over the locations
  spec <- design_app_spec("multi_loc_preps")
  raw <- page_raw("multi_loc_preps", lines = 150, use_checks = TRUE, checks = 2, rep_checks = "6", l = 3,
                  copies_per_entry = "4", seed = 13)
  values <- spec$values(read_design_controls(spec, shiny_shaped(raw)), NULL)
  allocation <- do_optim(design = "prep", lines = 150, l = 3, copies_per_entry = 4, add_checks = TRUE,
                         checks = 2, rep_checks = c(6, 6), seed = 13)
  view <- spec$setup$view(c(values, list(optim_list = allocation)), list())
  expect_equal(list(names(view), is.na(view["Total", "Avg"])), list(c("LOC1", "LOC2", "LOC3", "Copies", "Avg"), TRUE))
  upload <- data.frame(ENTRY = 1:152, NAME = c("C1", "C2", paste0("L", 3:152)))
  values <- spec$values(read_design_controls(spec, shiny_shaped(raw), uploaded = TRUE), spec$upload_shape(upload))
  expect_identical(values$entries$names, paste0("L", 3:152))
})

test_that("the augmented RCBD page explains entries that are not randomized", {
  spec <- design_app_spec("RCBD_augmented")
  # the stacking is only read for stacked experiments
  one <- page_raw("RCBD_augmented", repsStack = "horizontal")
  expect_null(spec$values(read_design_controls(spec, shiny_shaped(one)), NULL)$repsStack)
  expect_identical(list(augmented_random_note(FALSE), augmented_random_note(TRUE)),
                   list("By unchecking this option only the check plots are randomized.", NULL))
})

test_that("pages report a problem with their controls or upload before building", {
  optim_raw <- page_raw("Optim", checks = NA, rep_checks = "", lines = NA)
  multiple_raw <- page_raw("diagonal_multiple", lines = NA)
  sparse_raw <- page_raw("sparse_allocation", lines = 120, checks = 2, l = 3, copies_per_entry = "2")
  mlp_raw <- page_raw("multi_loc_preps", lines = 150, use_checks = TRUE, checks = 2, rep_checks = "6", l = 3,
                      copies_per_entry = "4")
  cases <- list(
    "FD upload of one factor" = list("FD", page_defaults("FD"),
      data.frame(FACTOR = c("A", "A"), LEVEL = c("a0", "a1")), "More than one factor"),
    "Square_Lattice no block size" = list("Square_Lattice", page_raw("Square_Lattice", t = 50,
      k = "No Options Available"), NULL, "No options for this combination"),
    "Optim REPS stored as doubles" = list("Optim", optim_raw, transform(optim_upload, REPS = as.numeric(REPS)),
                                  "'REPS' must be numeric"),
    "Optim replicates per check" = list("Optim", utils::modifyList(optim_raw, list(checks = 4,
      rep_checks = "1,2", lines = 280)), NULL, "Replicates per check must have 1 value or 4 values"),
    "Optim too many check plots" = list("Optim", utils::modifyList(optim_raw, list(checks = 4,
      rep_checks = "80", lines = 280)), NULL, "Number of lines should be greater"),
    "pREPS replicates per group" = list("pREPS", page_raw("pREPS", repUnits = "2"), NULL,
      "Replicates per group must have one value per group of entries"),
    "pREPS needs filler plots" = list("pREPS", page_raw("pREPS", repGens = "75,157", allow_fillers = FALSE),
      NULL, "Select 'Allow filler plots'"),
    "RCBD_augmented too few entries" = list("RCBD_augmented", page_raw("RCBD_augmented", lines = 7), NULL,
      "At least ten treatments"),
    "RCBD_augmented no block count" = list("RCBD_augmented", page_raw("RCBD_augmented",
      b = "No Options Available"), NULL, "No options for this combination"),
    "Augmented_RowCol no check direction" = list("Augmented_RowCol", page_defaults("Augmented_RowCol"), NULL,
      "Choose the check direction"),
    "Diagonal shuffled entries" = list("Diagonal", page_raw("Diagonal", lines = NA),
      diagonal_upload[c(2, 1, 5, 3, 4, 6:154), ], "must have consecutive ENTRY numbers"),
    "Diagonal too few entries" = list("Diagonal", page_raw("Diagonal", lines = 2), NULL,
      "Insufficient number of entries"),
    "diagonal_multiple blocks sum" = list("diagonal_multiple", page_raw("diagonal_multiple", blocks = "100,120"),
      NULL, "must add up to the number of entries"),
    "diagonal_multiple small field" = list("diagonal_multiple", page_raw("diagonal_multiple", lines = 40,
      blocks = "20,20"), NULL, "Larger field size is recommended"),
    "diagonal_multiple repeated entries" = list("diagonal_multiple", page_raw("diagonal_multiple",
      sameEntries = TRUE), NULL, "Blocks should have the same size"),
    "diagonal_multiple upload mismatch" = list("diagonal_multiple", multiple_raw, multiple_upload[-5, ],
      "does not match with the data input file"),
    "sparse_allocation two locations" = list("sparse_allocation", page_raw("sparse_allocation", l = 2), NULL,
      "at least 3 locations"),
    "sparse_allocation too few entries" = list("sparse_allocation", page_raw("sparse_allocation", lines = 59),
      NULL, "at least 60 entries"),
    "sparse_allocation upload mismatch" = list("sparse_allocation", utils::modifyList(sparse_raw,
      list(lines = 100)), data.frame(ENTRY = 1:122, NAME = c("CH1", "CH2", paste0("Line", 3:122))),
      "does not match with the input value"),
    "multi_loc_preps upload mismatch" = list("multi_loc_preps", utils::modifyList(mlp_raw, list(lines = 149)),
      data.frame(ENTRY = 1:152, NAME = c("C1", "C2", paste0("L", 3:152))), "does not match with the input value"),
    "multi_loc_preps text entries" = list("multi_loc_preps", mlp_raw,
      data.frame(ENTRY = paste0("E", 1:152), NAME = c("C1", "C2", paste0("L", 3:152))), "ENTRY should be numeric"),
    "multi_loc_preps one location" = list("multi_loc_preps", page_raw("multi_loc_preps", l = 1), NULL,
      "at least 2 locations")
  )
  for (name in names(cases)) {
    case <- cases[[name]]
    expect_error(page_design(design_app_spec(case[[1]]), case[[2]], case[[3]]), case[[4]], fixed = TRUE,
                 class = "fieldhub_input_error", info = name)
  }
  expect_error(design_app_spec("sparse_allocation")$accept(list(fieldBook = NULL)), "do not fit the entries",
               class = "fieldhub_input_error")
})

test_that("the augmented row-column page needs a check direction and reads advanced options only when on", {
  values <- list(nrows = 10, ncols = 8, lines = 48, checks = 4, budget = 300, seed = 29)
  expect_problems(lapply(list(NULL, "", NA_character_, "diagonal", c("rows", "columns")), function(direction) {
    function() design_args_Augmented_RowCol(c(values, list(check_direction = direction)))
  }), "Choose the check direction", case = "builder directions")
  spec <- design_app_spec("Augmented_RowCol")
  raw <- utils::modifyList(design_control_defaults(spec), list(check_direction = "columns"))
  off <- read_design_controls(spec, utils::modifyList(raw, list(budget = "abc", time_limit = "-1")))
  raw$advanced <- TRUE
  blank <- read_design_controls(spec, raw)
  on <- read_design_controls(spec, utils::modifyList(raw, list(budget = "500", time_limit = "2.5", year = "2031")))
  expect_equal(list(off$budget, off$time_limit, blank$budget, blank$time_limit, on$objective,
                    c(on$budget, on$time_limit, on$year)),
               list(NULL, NULL, NULL, NULL, "V_tt", c(500, 2.5, 2031)))
  expect_problems(lapply(list(list(budget = "1.5"), list(budget = "0"), list(time_limit = "soon"),
                              list(time_limit = "0"), list(year = "20.5")), function(bad) {
    function() read_design_controls(spec, utils::modifyList(raw, bad))
  }), "", case = "advanced options")
})

test_that("spatial engines report a missing or invalid number of locations by class", {
  allocation <- do_optim(design = "sparse", lines = 135, l = 3, copies_per_entry = 2, add_checks = TRUE,
                         checks = 4, seed = 5)
  base <- list(lines = 135, copies_per_entry = 2, checks = 4, seed = 5, nrows = 10, ncols = 10,
               plot_start = c(1, 1001, 2001), location_names = c("A", "B", "C"), sparse_list = allocation)
  prep <- list(lines = 40, copies_per_entry = 3, seed = 7, nrows = 8, ncols = 8, plot_start = c(1, 1001),
               location_names = c("A", "B"), allow_fillers = TRUE)
  for (l in list(NULL, "3", 0, 2.5, c(2, 3))) {
    with_l <- function(values) if (is.null(l)) values else c(values, list(l = l))
    builds <- list(
      function() do.call(sparse_allocation, design_args_sparse_allocation(with_l(base))),
      function() do.call(do_optim, design_args_sparse_allocation_optim(with_l(base))),
      function() do.call(multi_location_prep, design_args_multi_loc_preps(with_l(prep))),
      function() do.call(do_optim, design_args_multi_loc_preps_optim(with_l(prep))))
    if (!is.null(l)) {
      diagonal <- list(nrows = 15, ncols = 20, lines = 270, checks = 4, seed = 1, l = l,
                       plot_start = c(1, 1001), location_names = c("A", "B"), blocks = c(100, 170))
      builds <- c(builds, list(
        function() do.call(diagonal_arrangement, design_args_Diagonal(diagonal)),
        function() do.call(diagonal_arrangement, design_args_diagonal_multiple(diagonal))))
    }
    expect_problems(builds, "", case = paste("l =", deparse(l)))
  }
})

# --- The page's controls ----------------------------------------------------

test_that("each page shows every control, the shared buttons and its labels", {
  for (package in app_dependencies()) skip_if_not_installed(package)
  for (module in all_modules) {
    spec <- design_app_spec(module)
    html <- as.character(mod_design_ui("x", spec))
    omitted <- app_sidebar_layout(spec)$omitted
    shown <- Filter(function(control) !control$id %in% omitted,
                    c(lapply(spec$controls, app_sidebar_control, module = module), spec$steps))
    upload <- app_upload_spec(spec$upload)
    ids <- c(vapply(shown, `[[`, "", "id"), "run", spec$workflow$ids[["simulate"]], upload$toggle, upload$file)
    # one select per location is drawn by the server, labelled "<label> <i>"
    labels <- unlist(lapply(shown, function(control) {
      if (!identical(control$type, "location_selects")) htmltools::htmlEscape(control$label)
    }))
    missing <- c(ids[!vapply(paste0('id="x-', ids, '"'), grepl, TRUE, html, fixed = TRUE)],
                 labels[!vapply(labels, grepl, TRUE, html, fixed = TRUE)],
                 setdiff(c(spec$title, "Generate design"), Filter(function(text) grepl(text, html, fixed = TRUE),
                                                                 c(spec$title, "Generate design"))))
    expect(length(missing) == 0L, sprintf("page %s does not show %s", module, missing[1]))
  }
})

test_that("every page has unique controls, the shared ones, the seed last, and locations only for its engine", {
  problems <- unlist(lapply(all_modules, function(module) {
    spec <- design_app_spec(module)
    ids <- vapply(spec$controls, `[[`, character(1), "id")
    checks <- c(
      "unique ids" = anyDuplicated(c(ids, vapply(spec$steps, `[[`, "", "id"))) == 0L,
      "shared controls" = all(c("planter", "plot_start", "location_names", "seed") %in% ids),
      "seed last" = identical(ids[[length(ids)]], "seed"),
      "locations as the engine" = identical("l" %in% ids, "l" %in% names(formals(spec$engine))),
      "location view on spatial pages" = identical("location_view" %in% ids, identical(spec$kind, "spatial")),
      "whole-number defaults" = all(vapply(spec$controls, function(control) {
        !identical(control$type, "number") || (is.numeric(control$value) && control$value >= control$min)
      }, logical(1))))
    if (!all(checks)) paste(module, names(checks)[!checks])
  }))
  expect(length(problems) == 0L, paste("page", problems[1]))
})

test_that("every minimum a page offers is the smallest value its engine accepts", {
  # FD's single replicate is for its CRD type
  at_minimum <- list(FD.reps = list(type = "1"), Latin_Rectangle.t = list(nrows = 2))
  for (module in classic_modules) {
    spec <- design_app_spec(module)
    computed <- any(vapply(spec$controls, function(control) !is.null(control$options), logical(1)))
    failures <- character()
    for (control in spec$controls) {
      if (!identical(control$type, "number") || is.null(control$min)) next
      raw <- utils::modifyList(page_defaults(module), c(list(), at_minimum[[paste(module, control$id, sep = ".")]]))
      if (!is.null(control$enabled_by)) raw[[control$enabled_by]] <- TRUE
      if (!inherits(problem_of(page_design(spec, replace(raw, control$id, list(control$min - 1)))),
                    "fieldhub_input_error")) failures <- c(failures, paste(control$id, "below its minimum builds"))
      # a treatment count at its minimum leaves no block size to choose
      if (computed && identical(control$id, "t")) next
      problem <- problem_of(page_design(spec, replace(raw, control$id, list(control$min))))
      if (!is.null(problem)) failures <- c(failures, paste(control$id, "at its minimum:", conditionMessage(problem)))
    }
    for (control in Filter(function(control) !is.null(control$max), spec$controls)) {
      if (!inherits(problem_of(page_design(spec, replace(page_defaults(module), control$id,
                                                         list(control$max + 1)))), "fieldhub_input_error")) {
        failures <- c(failures, paste(control$id, "above its maximum builds"))
      }
    }
    expect(length(failures) == 0L, sprintf("page %s: %s", module, failures[1]))
  }
})

test_that("pages name their controls their own way where their designs need it", {
  label_of <- function(module, id) {
    Filter(function(control) control$id == id, design_app_spec(module)$controls)[[1]]$label
  }
  expect_identical(c(label_of("LSD", "reps"), label_of("RCBD_augmented", "checks"), label_of("CRD", "reps"),
                     app_sidebar_control(ctl_seed(), "pREPS")$label),
                   c("Replicates (squares)", "Checks per block", "Replicates", "Random seed"))
})

test_that("each sidebar draws every control exactly once and an automatic seed", {
  for (spec in fieldhub_design_specs()) {
    layout <- app_sidebar_layout(spec)
    query <- htmltools::tagQuery(app_sidebar_ui(spec, shiny::NS("page"), toggle = "upload"))
    counts <- vapply(spec$controls, function(control) query$find(paste0("#page-", control$id))$length(), 1L)
    expected <- vapply(spec$controls, function(control) as.integer(!control$id %in% layout$omitted), 1L)
    seed <- query$find("#page-seed")$selectedTags()[[1]]
    wrong <- which(counts != expected)
    expect(length(wrong) == 0L && identical(seed$attribs$placeholder, "Automatic") && is.null(seed$attribs$value),
           sprintf("page %s: %s", spec$module, if (length(wrong)) {
             sprintf("control %s drawn %d times", spec$controls[[wrong[1]]]$id, counts[wrong[1]])
           } else "the seed is not blank with an automatic placeholder"))
  }
})

test_that("check counts drawn as selects still parse as the engine needs", {
  for (module in c("Diagonal", "diagonal_multiple", "sparse_allocation")) {
    spec <- design_app_spec(module)
    checks <- Filter(function(x) x$id == "checks", spec$controls)[[1]]
    restored <- app_sidebar_control(checks, module)
    raw <- utils::modifyList(design_control_defaults(spec), list(checks = "4"))
    expect_equal(list(restored$type, restored$choices, restored$parse("4", list()),
                      read_design_controls(spec, raw)$checks),
                 list("select", seq_len(if (module == "diagonal_multiple") 20 else 10), restored$parse(4, list()), 4),
                 info = module)
    expect_problems(list(function() restored$parse("garbage", list()), function() restored$parse("0", list())),
                    "", case = module)
  }
  expect_null(app_sidebar_control(ctl_seed(), "pREPS")$parse("", list()))
})

test_that("multi-location Yes/No controls pass logical flags to the engine", {
  spec <- design_app_spec("multi_loc_preps")
  flag <- Filter(function(x) x$id == "use_checks", spec$controls)[[1]]
  expect_identical(lapply(list(TRUE, "TRUE", FALSE, "FALSE"), flag$parse, list()), list(TRUE, TRUE, FALSE, FALSE))
  expect_error(flag$parse("garbage", list()), class = "fieldhub_input_error")
  raw <- design_control_defaults(spec)
  off <- read_design_controls(spec, utils::modifyList(raw, list(use_checks = "FALSE")))
  on <- read_design_controls(spec, utils::modifyList(raw, list(use_checks = "TRUE")))
  expect_equal(list(off$use_checks, off$checks, on$use_checks, on$checks, on$rep_checks),
               list(FALSE, NULL, TRUE, 3, c(8, 8, 8)))
})

# --- Reading controls: values and the messages a user sees -------------------

page <- function(...) list(controls = list(...))

test_that("controls read the values the engines take", {
  k_select <- function(engine) ctl_dependent_select("k", depends_on = "t", options = function(values, data) {
    block_size_choices(if (is.null(data)) values$t else nrow(data), engine)
  })
  flagged <- page(ctl_flag("use_checks"), ctl_checks(2, enabled_by = "use_checks"),
                  ctl_rep_checks(enabled_by = "use_checks"),
                  ctl_flag("spread_checks", enabled_by = "use_checks", disabled_value = TRUE))
  reps_page <- page(ctl_text_list("repGens", "75,150"),
                    ctl_text_list("repUnits", "2,1", parse = function(value, label, values) {
                      parse_control_rep_units(value, label, values$repGens)
                    }), ctl_plot_start())
  cases <- list(
    counts = list(page(ctl_count("t", 15), ctl_reps(3), ctl_locations()), list(t = 15L, reps = 3L, l = 2L),
                  list(t = 15L, reps = 3L, l = 2L)),
    "one plot start" = list(page(ctl_plot_start()), list(plot_start = "101"), list(plot_start = 101)),
    "plot starts" = list(page(ctl_plot_start()), list(plot_start = "101, 1001"), list(plot_start = c(101, 1001))),
    "factor levels" = list(page(ctl_text_list("setfactors", "2,2,3", parse = function(value, label, values) {
      parse_control_factor_counts(value, label)
    })), list(setfactors = "2,3"), list(setfactors = c(2, 3))),
    "location names" = list(page(ctl_location_names()), list(location_names = "A, B"),
                            list(location_names = c("A", " B"))),
    "trimmed location names" = list(page(ctl_location_names(trim = TRUE)), list(location_names = " X ,Y"),
                                    list(location_names = c("X", "Y"))),
    "experiment names" = list(page(ctl_expt_name()), list(expt_name = "A, B"), list(expt_name = c("A", " B"))),
    "trimmed experiment names" = list(page(ctl_expt_name(trim = TRUE)), list(expt_name = "A, B"),
                                      list(expt_name = c("A", "B"))),
    "whole experiment name" = list(page(ctl_expt_name(split = FALSE)), list(expt_name = "A,B"),
                                   list(expt_name = "A,B")),
    selects = list(page(ctl_planter(), ctl_select("type", c("In a RCBD" = "2", "In a CRD" = "1"),
                                                  convert = as.numeric)),
                   list(planter = "cartesian", type = "1"), list(planter = "cartesian", type = 1)),
    "dependent select" = list(page(ctl_count("t", 12, generated_only = TRUE), k_select("incomplete_blocks")),
                              list(t = 12L, k = "4"), list(t = 12L, k = 4)),
    flags = list(page(ctl_flag("continuous"), ctl_flag("use_checks")), list(continuous = FALSE, use_checks = TRUE),
                 list(continuous = FALSE, use_checks = TRUE)),
    "unrendered flags take their default" = list(page(ctl_flag("continuous"), ctl_flag("use_checks")),
                                                 list(continuous = NA), list(continuous = TRUE, use_checks = FALSE)),
    seed = list(page(ctl_seed()), list(seed = 17L), list(seed = 17)),
    "negative seed" = list(page(ctl_seed()), list(seed = -3), list(seed = -3)),
    "generated counts skipped on upload" = list(page(ctl_count("t", 15, generated_only = TRUE), ctl_reps(3)),
                                                list(t = NA, reps = 3L), list(reps = 3L), uploaded = TRUE),
    "flagged controls off" = list(flagged,
                                  list(use_checks = FALSE, checks = NA, rep_checks = "", spread_checks = FALSE),
                                  list(use_checks = FALSE, checks = NULL, rep_checks = NULL, spread_checks = TRUE)),
    "flagged controls on" = list(flagged,
                                 list(use_checks = TRUE, checks = 2L, rep_checks = "2,3", spread_checks = FALSE),
                                 list(use_checks = TRUE, checks = 2L, rep_checks = c(2, 3), spread_checks = FALSE)),
    "one replicate for every check" = list(flagged, list(use_checks = TRUE, checks = 3L, rep_checks = "2"),
                                           list(use_checks = TRUE, checks = 3L, rep_checks = c(2, 2, 2),
                                                spread_checks = TRUE)),
    "parsers read earlier values" = list(reps_page, list(repGens = "75,150", repUnits = "2,1", plot_start = "1"),
                                         list(repGens = c(75, 150), repUnits = c(2, 1), plot_start = 1))
  )
  for (name in names(cases)) {
    case <- cases[[name]]
    expect_equal(read_design_controls(case[[1]], case[[2]], uploaded = isTRUE(case$uploaded)), case[[3]],
                 info = name)
  }
  # every blank seed means an automatic seed
  seeds <- lapply(blanks, function(value) read_design_controls(page(ctl_seed()), list(seed = value)))
  expect_identical(unique(seeds), list(stats::setNames(list(NULL), "seed")))
})

test_that("controls name their label in the message of a bad value", {
  flagged <- page(ctl_flag("use_checks"), ctl_checks(2, enabled_by = "use_checks"),
                  ctl_rep_checks(enabled_by = "use_checks"))
  k_page <- page(ctl_count("t", 12, generated_only = TRUE),
                 ctl_dependent_select("k", depends_on = "t", options = function(values, data) {
                   block_size_choices(if (is.null(data)) values$t else nrow(data), "incomplete_blocks")
                 }))
  reps_page <- page(ctl_text_list("repGens", "75,150"),
                    ctl_text_list("repUnits", "2,1", parse = function(value, label, values) {
                      parse_control_rep_units(value, label, values$repGens)
                    }), ctl_plot_start())
  counts <- page(ctl_count("t", 15), ctl_reps(3), ctl_locations())
  factors <- page(ctl_text_list("setfactors", "2,2,3", parse = function(value, label, values) {
    parse_control_factor_counts(value, label)
  }))
  each <- function(spec, raws) lapply(raws, function(raw) function() read_design_controls(spec, raw))
  with_blanks <- function(spec, make) each(spec, lapply(blanks, make))
  cases <- list(
    "blank count" = list(with_blanks(counts, function(v) list(t = v, reps = 3L, l = 1L)),
                         "Treatments cannot be blank.", "Treatments"),
    "not a whole number" = list(each(counts, lapply(list(2.5, "12", TRUE, c(1, 2), Inf),
                                                    function(v) list(t = 4L, reps = v, l = 1L))),
                                "Replicates must be one whole number.", "Replicates"),
    "unrendered count" = list(each(counts, list(list(t = 4L, reps = 2L))), "Locations cannot be blank.", "Locations"),
    "blank plot start" = list(with_blanks(page(ctl_plot_start()), function(v) list(plot_start = v)),
                              "Starting plot number cannot be blank."),
    "unreadable plot start" = list(each(page(ctl_plot_start()), list(list(plot_start = "101,abc"))),
                                   "Starting plot number could not read \"abc\""),
    "one factor" = list(each(factors, list(list(setfactors = "4"))), "more than one factor needs to be specified"),
    "blank factors" = list(with_blanks(factors, function(v) list(setfactors = v)), "cannot be blank."),
    "blank location name" = list(with_blanks(page(ctl_location_names()), function(v) list(location_names = v)),
                                 "Location name cannot be blank."),
    "location name not text" = list(each(page(ctl_location_names()), list(list(location_names = 3))),
                                    "Location name must be one comma-separated text value."),
    "blank experiment name" = list(with_blanks(page(ctl_expt_name()), function(v) list(expt_name = v)),
                                   "Experiment name cannot be blank."),
    "blank select" = list(with_blanks(page(ctl_planter()), function(v) list(planter = v)),
                          "Plot order cannot be blank."),
    "not a choice" = list(each(page(ctl_planter()), list(list(planter = "spiral"))),
                          "Plot order must be one of: serpentine, cartesian."),
    "not a choice value" = list(each(page(ctl_select("type", c("In a RCBD" = "2", "In a CRD" = "1"),
                                                     convert = as.numeric)), list(list(type = 2))),
                                "Design type must be one of: 2, 1."),
    "blank dependent select" = list(with_blanks(k_page, function(v) list(t = 12L, k = v)),
                                    "Plots per block cannot be blank."),
    "nothing fits" = list(each(k_page, list(list(t = 13L, k = "No Options Available"))),
                          "No options for this combination of treatments!"),
    "dependent select not a number" = list(each(k_page, list(list(t = 12L, k = "four"))),
                                           "Plots per block must be one whole number."),
    "flag not logical" = list(each(page(ctl_flag("continuous")), list(list(continuous = "yes"))),
                              "Continuous plot numbers must be TRUE or FALSE."),
    "seed not a number" = list(each(page(ctl_seed()), list(list(seed = "abc"), list(seed = c(1, 2)))),
                               "Random seed must be one number, or blank for an automatic seed."),
    "generated count needed without upload" = list(each(page(ctl_count("t", 15, generated_only = TRUE)),
                                                        list(list(t = NA))), "cannot be blank."),
    "blank flagged count" = list(each(flagged, list(list(use_checks = TRUE, checks = NA, rep_checks = "2"))),
                                 "Checks cannot be blank.", "Checks"),
    "flagged count out of range" = list(each(flagged, list(list(use_checks = TRUE, checks = 0L, rep_checks = "2"))),
                                        "Checks must be a whole number from 1 to", "Checks"),
    "blank replicates per check" = list(with_blanks(flagged, function(v) list(use_checks = TRUE, checks = 2L,
                                                                              rep_checks = v)),
                                        "Replicates per check cannot be blank."),
    "replicates per check count" = list(each(flagged, list(list(use_checks = TRUE, checks = 2L, rep_checks = "1,2,3"))),
                                        "Replicates per check must have 1 value or 2 values", "Replicates per check"),
    "unreadable group" = list(each(reps_page, list(list(repGens = "75,x", repUnits = "2,1", plot_start = "1"))),
                              "Entries per replicate group could not read", "Entries per replicate group"),
    "replicates per group count" = list(each(reps_page, list(list(repGens = "75,150", repUnits = "2",
                                                                  plot_start = "1"))),
                                        "Replicates per group must have one value per group of entries (2); got 1.",
                                        "Replicates per group"),
    "plot start zero" = list(each(reps_page, list(list(repGens = "75", repUnits = "2", plot_start = "0"))),
                             "Starting plot number could not read", "Starting plot number")
  )
  for (name in names(cases)) {
    case <- cases[[name]]
    expect_problems(case[[1]], case[[2]], case = name, control = if (length(case) > 2L) case[[3]])
  }
})

test_that("dependent selects offer the block sizes that fit the entries, typed or uploaded", {
  k <- ctl_dependent_select("k", depends_on = "t", options = function(values, data) {
    block_size_choices(if (is.null(data)) values$t else nrow(data), "incomplete_blocks")
  })
  k_page <- page(ctl_count("t", 12, generated_only = TRUE), k)
  # checks are read on the upload path too; entries typed only on the other
  b <- ctl_dependent_select("b", depends_on = c("lines", "checks"), options = function(values, data) {
    list(choices = values, selected = if (is.null(data)) "typed" else nrow(data))
  })
  b_page <- page(ctl_count("lines", 50, generated_only = TRUE), ctl_checks(4), b)
  cases <- list(
    "typed entries" = list(design_control_choices(k_page, k, list(t = 12L)),
                           list(choices = c(2L, 3L, 4L, 6L), selected = 3L)),
    "uploaded entries" = list(design_control_choices(k_page, k, list(t = NA), data = data.frame(ENTRY = 1:15)),
                              list(choices = c(3L, 5L), selected = 3L)),
    "controls read with an upload" = list(design_control_choices(b_page, b, list(lines = NA, checks = 3L),
                                                                 data.frame(ENTRY = 1:9)),
                                          list(choices = list(checks = 3L), selected = 9L)),
    "controls read without an upload" = list(design_control_choices(b_page, b, list(lines = 50L, checks = 3L))$choices,
                                             list(lines = 50L, checks = 3L)),
    "block sizes" = list(block_size_choices(49, "square_lattice"), list(choices = 7L, selected = 7L)),
    "no block size" = list(block_size_choices(13, "row_column"),
                           list(choices = "No Options Available", selected = "No Options Available"))
  )
  for (name in names(cases)) expect_equal(cases[[name]][[1]], cases[[name]][[2]], info = name)
  expect_problems(list(function() design_control_choices(k_page, k, list(t = NA)),
                       function() block_size_choices(NA, "row_column")), "", case = "no entries")
})

test_that("a page shows its default values before any change", {
  control <- ctl_dependent_select("k", depends_on = "t", options = function(values, data) {
    block_size_choices(values$t, "alpha_lattice")
  })
  spec <- page(ctl_count("t", 36), control, ctl_planter(), ctl_plot_start(), ctl_flag("continuous"),
               ctl_preview("checks_note", text = function(raw, uploaded) NULL), ctl_seed())
  expect_equal(design_control_defaults(spec),
               list(t = 36, k = 6L, planter = "serpentine", plot_start = "101", continuous = TRUE, seed = NULL))
})

test_that("the RCBD page previews its block size only when it can", {
  raw <- list(use_checks = TRUE, t = 18L, reps = 3L, checks = 2L, rep_checks = "2")
  # 18 treatments and 2 checks twice: 22 plots per block, in 3 replicates
  expect_identical(rcbd_checks_note(raw, FALSE), "Block size: 22 plots. Total: 66 plots.")
  expect_match(rcbd_checks_note(raw, TRUE), "depends on the uploaded list", fixed = TRUE)
  silent <- c(list(replace(raw, "use_checks", list(FALSE)), replace(raw, "use_checks", list(NULL))),
              unlist(lapply(c("t", "reps", "checks", "rep_checks"), function(id) {
                lapply(list(NULL, NA, ""), function(value) replace(raw, id, list(value)))
              }), recursive = FALSE))
  expect_true(all(vapply(silent, function(r) is.null(rcbd_checks_note(r, FALSE)), logical(1))))
  expect_error(rcbd_checks_note(replace(raw, "rep_checks", "2,x"), FALSE), class = "fieldhub_input_error")
})

test_that("uploads keep, name and complete the design's columns, and need two factors for a factorial", {
  file <- data.frame(a = c("x", "y", NA), b = c(1, NA, 3), extra = 1:3)
  kept <- shape_design_upload(file, c("Hplot", "Vplot"), omit_na = FALSE)
  two <- data.frame(FACTOR = c("A", "A", "B"), LEVEL = c("a0", "a1", "b0"))
  expect_equal(list(shape_design_upload(file, "TREATMENT"), nrow(shape_design_upload(file, c("ENTRY", "NAME"))),
                    names(kept), nrow(kept), upload_level_counts(kept), check_factorial_upload(two),
                    design_upload_data(two)),
               list(stats::na.omit(data.frame(TREATMENT = c("x", "y", NA))), 1L, c("Hplot", "Vplot"), 3L, c(2, 2),
                    two, two))
  expect_problems(list(function() shape_design_upload(file, c("A", "B", "C", "D")),
                       function() shape_design_upload("file", "A")), "", case = "unusable file")
  expect_error(check_factorial_upload(two[1:2, ]), "More than one factor", class = "fieldhub_input_error")
  expect_error(design_upload_data(NULL), "Check the input file", class = "fieldhub_input_error")
})

# --- Steps of a spatial page: field sizes and percentages of checks ----------

test_that("field sizes and percentages of checks are read once they hold a current choice", {
  steps <- list(
    ctl_dimensions(function(values, data) list(choices = c("2 x 3", "3 x 2"), selected = "2 x 3"),
                   per_location = TRUE),
    ctl_flag("multi_dimension", stage = "run"),
    ctl_location_dimensions(function(values, data) NULL, enabled_by = "multi_dimension"))
  choices <- list(dimensions = design_step_choices(steps[[1]], list(l = 2)),
                  location_dimensions = list(choices = list("2 x 3", c("1 x 6", "2 x 3"))))
  one <- list(ctl_dimensions(function(values, data) list(choices = c("15 x 20", "20 x 15"))))
  percent <- list(ctl_checks_percent(function(values, data) NULL))
  percents <- list(checks_percent = list(choices = c(6.98, 9.6, 11.11), selected = 11.11))
  cases <- list(
    "one size" = list(read_design_steps(one, list(dimensions = "15 x 20"), list(l = 1),
                                        list(dimensions = list(choices = c("15 x 20", "20 x 15")))),
                      list(l = 1, nrows = 15, ncols = 20)),
    "one size for every location" = list(read_design_steps(steps, list(dimensions = "3 x 2", multi_dimension = FALSE),
                                                           list(l = 2), choices),
                                         list(l = 2, nrows = c(3, 3), ncols = c(2, 2), multi_dimension = FALSE)),
    "a size per location" = list(read_design_steps(steps, list(dimensions = "3 x 2", multi_dimension = TRUE,
                                                               location_dimensions = list("2 x 3", "1 x 6")),
                                                   list(l = 2), choices)[c("nrows", "ncols")],
                                 list(nrows = c(2, 1), ncols = c(3, 6))),
    # each Randomize! starts from the choice it selects, as the select sends it
    "selected percentage" = list(read_design_steps(percent, selected_step_values(percent, percents), list(), percents),
                                 list(checksPercent = 11.11)),
    "chosen percentage" = list(read_design_steps(percent, list(checks_percent = "9.6"), list(), percents),
                               list(checksPercent = 9.6)),
    # a select still holding a value of earlier choices is waited for
    "old size" = list(read_design_steps(steps, list(dimensions = "6 x 1"), list(l = 2), choices), NULL),
    # a select that has not sent a value yet is waited for too
    "no size yet" = list(read_design_steps(one, list(), list(l = 1),
                                           list(dimensions = list(choices = c("15 x 20", "20 x 15")))), NULL),
    "old size of a location" = list(read_design_steps(steps, list(dimensions = "3 x 2", multi_dimension = TRUE,
                                                                  location_dimensions = list("2 x 3", "9 x 9")),
                                                      list(l = 2), choices), NULL),
    "old percentage" = list(read_design_steps(percent, list(checks_percent = "9.7"), list(), percents), NULL)
  )
  for (name in names(cases)) expect_equal(cases[[name]][[1]], cases[[name]][[2]], info = name)
  bad_sizes <- c(blanks, list("15x20", "15 x", "a x b", "0 x 3", "2.5 x 3", 15, "No Options Available"))
  expect_problems(lapply(bad_sizes, function(value) {
    function() parse_control_dimensions(value, "Size:")
  }), "", case = "bad field size")
})

test_that("a step with nothing to choose from is explained instead of waited for", {
  sizes <- ctl_location_dimensions(function(values, data) NULL, enabled_by = "multi_dimension")
  steps <- list(ctl_flag("multi_dimension", stage = "run"), sizes)
  choices <- list(location_dimensions = list(choices = list("8 x 8", character(), character())))
  raw <- list(multi_dimension = TRUE, location_dimensions = list("8 x 8", NULL, NULL))
  expect_problems(list(function() read_design_steps(steps, raw, list(l = 3), choices)),
                  "Field dimensions, location 2: no field size fits this location", case = "location",
                  control = "Field dimensions, location 2")
  # the per-location sizes are not read while their flag is off
  expect_equal(read_design_steps(steps, list(multi_dimension = FALSE), list(l = 3), choices),
               list(l = 3, multi_dimension = FALSE))
  empty <- ctl_dimensions(function(values, data) NULL)
  expect_error(read_design_steps(list(empty), list(dimensions = ""), list(),
                                 list(dimensions = list(choices = character()))),
               "Field dimensions has no option that fits.", class = "fieldhub_input_error")
})

test_that("the diagonal page's field sizes are those the engine suggests for an unusable field", {
  err <- tryCatch(diagonal_arrangement(nrows = 4, ncols = 70, lines = 270, checks = 4, seed = 1),
                  fieldhub_dimension_error = function(e) e)
  suggested <- paste(err$options$rows, "x", err$options$cols)
  offered <- diagonal_dimension_choices(lines = 270, checks = 1:4)
  # the engine suggests fields 10% to 20% larger than the 270 lines
  area <- err$options$rows * err$options$cols
  expect(inherits(err, "fieldhub_dimension_error") && length(offered) > 0L && all(offered %in% suggested) &&
           all(area >= 297 & area <= 324),
         sprintf("offered %s, suggested %s", paste(offered, collapse = ", "), paste(suggested, collapse = ", ")))
})

test_that("sparse_allocation() picks the squarest field 11% to 20% larger than a location", {
  design <- sparse_allocation(lines = 150, l = 3, copies_per_entry = 2, checks = 4, seed = 2)
  n <- as.numeric(design$size_locations[1])
  # brute force over every field with at least two rows and columns
  fields <- expand.grid(rows = 2:n, cols = 2:n)
  fields <- fields[fields$rows * fields$cols >= floor(n * 1.11) & fields$rows * fields$cols <= ceiling(n * 1.20), ]
  squarest <- min(abs(fields$rows - fields$cols))
  got <- c(design$infoDesign$rows, design$infoDesign$columns)
  expect(any(fields$rows == got[1] & fields$cols == got[2]) && abs(got[1] - got[2]) == squarest,
         sprintf("%d entries per location got %s", n, paste(got, collapse = " x ")))
})

test_that("the augmented RCBD page offers block counts whose fields hold every block, without the RNG", {
  for (case in list(c(lines = 60, checks = 4), c(lines = 40, checks = 3), c(lines = 8, checks = 2))) {
    lines <- case[["lines"]]; checks <- case[["checks"]]
    set.seed(71)
    before <- .Random.seed
    options <- set_augmented_blocks(lines = lines, checks = checks, start = 3)
    dims <- do.call(rbind, strsplit(options$blocks_dims[, 2], " x ", fixed = TRUE))
    rows <- as.numeric(dims[, 1]); cols <- as.numeric(dims[, 2])
    # counts searched from 3 up to a third (more than 40 lines) or a half of
    # the lines; no one-row or one-column fields; each holds b equal blocks
    plots_per_block <- ceiling((lines + checks * options$b) / options$b)
    ok <- c(rng = identical(.Random.seed, before),
            rows = nrow(options$blocks_dims) == length(options$b),
            counts = all(as.numeric(options$blocks_dims[, 1]) == options$b),
            range = all(options$b >= 3 & options$b <= ceiling(lines / if (lines > 40) 3 else 2)),
            shape = all(rows > 1 & cols > 1),
            blocks = all(rows * cols == plots_per_block * options$b))
    expect(all(ok), sprintf("%d lines, %d checks: %s fails", lines, checks, names(ok)[!ok][1]))
  }
  # 8 lines and 2 checks in 3 blocks: 14 plots, 5 per block, a 3 x 5 field
  expect_identical(set_augmented_blocks(lines = 8, checks = 2, start = 3)$blocks_dims[1, ], c("3", "3 x 5"))
  options <- set_augmented_blocks(lines = 60, checks = 4, start = 3)$blocks_dims
  built <- vapply(unique(options[, 1]), function(b) {
    dims <- as.numeric(strsplit(options[options[, 1] == b, 2][1], " x ")[[1]])
    design <- RCBD_augmented(lines = 60, checks = 4, b = as.numeric(b), seed = 1, nrows = dims[1], ncols = dims[2])
    identical(c(design$infoDesign$rows, design$infoDesign$columns), dims)
  }, logical(1))
  expect(all(built), sprintf("RCBD_augmented() does not build the field offered for %s blocks",
                             names(built)[!built][1]))
})

test_that("the choices offered before randomizing leave the RNG alone", {
  allocation <- do_optim(design = "prep", lines = 40, l = 2, copies_per_entry = 3, add_checks = TRUE, checks = 2,
                         rep_checks = c(2, 2), seed = 7)
  design <- RCBD_augmented(lines = 50, checks = 3, b = 5, seed = 29)
  calls <- list(
    field_dimensions = function() field_dimensions(270),
    diagonal_dimension_choices = function() diagonal_dimension_choices(90, checks = 1:4),
    diagonal_check_options = function() {
      diagonal_check_options(n_rows = 15, n_cols = 20, checks = 1:4, Option_NCD = TRUE, kindExpt = "SUDC",
                             planter_mov1 = "serpentine", data = NULL, dim_data = 274, dim_data_1 = 270,
                             Block_Fillers = NULL)
    },
    optimized_dimension_choices = function() optimized_dimension_choices(120),
    prep_dimension_options = function() prep_dimension_options(64, allow_fillers = TRUE),
    set_augmented_blocks = function() set_augmented_blocks(60, 4, start = 3),
    allocation_entry_names = function() allocation_entry_names(allocation, 40),
    field_book_location_grids = function() field_book_location_grids(design$fieldBook, "ENTRY"),
    checked_layout_view = function() checked_layout_view(design, location = 1))
  untouched <- vapply(calls, function(call) {
    set.seed(2026)
    before <- .Random.seed
    !is.null(call()) && identical(.Random.seed, before)
  }, logical(1))
  expect(all(untouched), sprintf("%s changes the RNG or offers nothing", names(untouched)[!untouched][1]))
})

# --- The engines the allocation pages hand an upload to ---------------------

test_that("allocation engines put the uploaded entries in their locations", {
  data <- data.frame(ENTRY = c(901:904, 101:250), NAME = c(paste0("C", 1:4), paste0("L", 1:150)))
  sparse <- sparse_allocation(lines = 150, l = 3, copies_per_entry = 2, checks = 4, seed = 1, data = data)
  allocation <- do_optim(design = "sparse", lines = 150, l = 3, copies_per_entry = 2, add_checks = TRUE,
                         checks = 4, seed = 1)
  # generated entry e is the uploaded row holding it: the checks (entries
  # 151 to 154) are the first rows, line i is row 4 + i
  row_of <- function(entry) ifelse(entry > 150, entry - 150, entry + 4)
  placed <- vapply(1:3, function(loc) {
    book <- sparse$fieldBook[sparse$fieldBook$LOCATION == paste0("LOC", loc), ]
    setequal(setdiff(book$TREATMENT, "Filler"), data$NAME[row_of(allocation$list_locs[[loc]]$ENTRY)])
  }, logical(1))
  expect(all(placed), sprintf("location %d does not hold its uploaded entries", which(!placed)[1]))
  # a p-rep allocation keeps the replication of every uploaded entry
  data <- data.frame(ENTRY = c(501, 502, 1:40), NAME = c("CHK-A", "CHK-B", paste0("SB-", 1:40)))
  prep <- multi_location_prep(lines = 40, l = 2, copies_per_entry = 3, checks = 2, rep_checks = c(2, 2),
                              seed = 7, data = data)
  allocation <- do_optim(design = "prep", lines = 40, l = 2, copies_per_entry = 3, add_checks = TRUE, checks = 2,
                         rep_checks = c(2, 2), seed = 7)
  row_of <- function(entry) ifelse(entry > 40, entry - 40, entry + 2)
  kept <- vapply(1:2, function(loc) {
    book <- prep$fieldBook[prep$fieldBook$LOCATION == paste0("LOC", loc), ]
    generated <- allocation$list_locs[[loc]]
    expected <- stats::setNames(generated$REPS, data$NAME[row_of(generated$ENTRY)])
    counts <- table(book$TREATMENT)
    setequal(names(counts), names(expected)) && all(counts[names(expected)] == expected)
  }, logical(1))
  expect(all(kept), sprintf("location %d changes the replication of an uploaded entry", which(!kept)[1]))
})

test_that("allocation engines reject uploads that do not match the allocation", {
  data <- data.frame(ENTRY = c(901:904, 101:250), NAME = c(paste0("C", 1:4), paste0("L", 1:150)))
  duplicated_entry <- data; duplicated_entry$ENTRY[6] <- 101
  duplicated_name <- data; duplicated_name$NAME[6] <- "L1"
  sparse <- function(data) function() {
    sparse_allocation(lines = 150, l = 3, copies_per_entry = 2, checks = 4, seed = 1, data = data)
  }
  cases <- list(
    "a row missing" = list(sparse(data[-5, ]), "Input lines does not match number of lines in input data!"),
    "repeated entry" = list(sparse(duplicated_entry), "Please ensure all ENTRIES in data are distinct."),
    "repeated name" = list(sparse(duplicated_name), "Please ensure all NAMES in data are distinct."),
    "replicates of other checks" = list(function() {
      multi_location_prep(lines = 40, l = 2, copies_per_entry = 3, checks = 2, rep_checks = c(2, 2, 2), seed = 7,
                          data = data.frame(ENTRY = c(501, 502, 1:40), NAME = c("CHK-A", "CHK-B", paste0("SB-", 1:40))))
    }, "Length of rep_checks does not match with number of checks"))
  for (name in names(cases)) expect_problems(cases[[name]][1], cases[[name]][[2]], case = name)
})

# --- Plot numbers and field book columns of the engines the pages call -------

test_that("RCBD() needs whole-number plot starts and orders its field book with and without checks", {
  expect_problems(lapply(list("101", 101.5, Inf, NA_real_, matrix(101, 1, 1)), function(start) {
    function() RCBD(t = 4, reps = 2, plotNumber = start, seed = 1)
  }), "plotNumber", case = "plot starts")
  expect_identical(list(names(RCBD(t = 4, reps = 2, seed = 1)$fieldBook),
                        names(RCBD(t = 4, reps = 2, checks = 2, rep_checks = c(2, 2), seed = 1)$fieldBook)),
                   list(c("ID", "LOCATION", "PLOT", "REP", "TREATMENT"),
                        c("ID", "LOCATION", "PLOT", "REP", "ENTRY", "CHECKS", "TREATMENT")))
})

test_that("the Augmented Row-Column entry tables and simulated field book come from the field book", {
  design <- quiet_design(augmented_row_column(10, 8, 4, "columns", budget = 300, l = 2,
                                              locationNames = c("A", "B"), seed = 4))
  entries <- arc_location_entries(design, 2)
  checks <- arc_location_checks(design, 2)
  book <- app_trait_simulation(design, design$fieldBook,
                               list(mean = 6, residual_sd = .4, trait = "Yield", spatial = TRUE), 4)
  expect_identical(list(entries$ENTRY, entries$NAME[1:4], checks$ENTRY, checks$TIMES,
                        all(c("CHECK_DIRECTION", "Yield") %in% names(book$df)), nrow(book$df)),
                   list(1:52, paste0("CH", 1:4), 1:4, rep(8L, 4), TRUE, nrow(design$fieldBook)))
})

# --- Seeds ------------------------------------------------------------------

test_that("a blank app seed means an automatic seed and draws nothing", {
  skip_if_not_installed("shiny")
  set.seed(419)
  before <- .Random.seed
  blank <- list(NULL, numeric(), character(), NA, NA_real_, NA_integer_, "", "  ",
                shiny:::inputHandlers$get("shiny.number")(NULL))
  expect_true(all(vapply(blank, function(value) is.null(read_app_seed(value)), logical(1))))
  expect_identical(.Random.seed, before)
})

test_that("app seeds convert valid numbers and reject everything else by class", {
  values <- list(0, 0L, -17, 123L, 7.9, "123", " -17 ", "1e3", c(seed = 42))
  expect_equal(lapply(values, read_app_seed), lapply(values, as.numeric))
  expect_problems(lapply(list(TRUE, FALSE, NaN, Inf, -Inf, "NA", "NaN", "bad", "abc", "1,2", NA_character_,
                              list(12), matrix(12), c(1, 2), .Machine$integer.max + 1), function(value) {
    function() read_app_seed(value)
  }), "", case = "bad seeds")
})

test_that("automatic app seeds vary within the API range and never touch the global stream", {
  set.seed(3)
  before <- .Random.seed
  seeds <- replicate(5, app_design_seed(NA))
  expect_identical(.Random.seed, before)
  expect_true(length(unique(seeds)) > 1L && all(seeds >= 1 & seeds <= .Machine$integer.max & seeds %% 1 == 0))
  expect_equal(app_design_seed(17), 17)
})

test_that("automatic app seeds leave an unseeded process unseeded", {
  remove_seed <- function() if (exists(".Random.seed", globalenv())) rm(".Random.seed", envir = globalenv())
  saved <- if (exists(".Random.seed", globalenv())) get(".Random.seed", globalenv())
  on.exit(if (!is.null(saved)) assign(".Random.seed", saved, globalenv()) else remove_seed())
  remove_seed()
  app_design_seed(NA)
  expect_false(exists(".Random.seed", globalenv()))
})

test_that("automatic simulation seeds come from the accepted design", {
  design <- CRD(t = 4, reps = 2, seed = NULL)
  expect_equal(list(workflow_seed(NULL, design), workflow_seed(numeric(), design),
                    workflow_seed(19, stop("Do not evaluate the design"))),
               list(design$metadata$seed, design$metadata$seed, 19))
  expect_error(workflow_seed(NULL, list()), class = "fieldhub_internal_error")
  settings <- list(mean = 20, residual_sd = 4, trait = "Yield")
  expect_equal(app_trait_simulation(design, design$fieldBook, settings, workflow_seed(NULL, design)),
               app_trait_simulation(design, design$fieldBook, settings, design$metadata$seed))
})

test_that("a run resolves an automatic seed once for allocation and field construction", {
  set.seed(419)
  expected <- sample.int(.Machine$integer.max, 1L)
  after_one_draw <- .Random.seed
  set.seed(419)
  seed <- resolve_seed(read_app_seed(NULL))
  expect_identical(seed, expected)
  expect_identical(.Random.seed, after_one_draw)
  allocation <- do_optim(design = "sparse", lines = 120, l = 4, copies_per_entry = 3, add_checks = TRUE,
                         checks = 4, seed = seed)
  design <- sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4, sparse_list = allocation,
                              seed = seed, year = 2026)
  expect_identical(c(allocation$metadata$seed, design$metadata$seed), c(seed, seed))
  expect_equal(reproduce_design(design), design)
  expect_identical(.Random.seed, after_one_draw)
})

test_that("one seed widget shares labels and API bounds while keeping a supplied default", {
  skip_if_not_installed("shiny")
  html <- as.character(app_seed_input("trial-seed", value = 123))
  shown <- vapply(c("trial-seed", "Random seed", 'value="123"', 'min="-2147483647"', 'max="2147483647"'),
                  grepl, TRUE, html, fixed = TRUE)
  expect(all(shown), sprintf("the seed widget does not show %s", names(shown)[!shown][1]))
})

# --- Background design jobs and workers -------------------------------------

test_that("design jobs return full results and classed failures", {
  ok <- run_design_job("RCBD", list(t = 4, reps = 2, seed = 1))
  expect_equal(ok[c("ok", "value", "warnings")], list(ok = TRUE, value = RCBD(t = 4, reps = 2, seed = 1),
                                                      warnings = list()))
  # failing inputs and engines that are not FielDHub designs
  failed <- c(list(run_design_job("RCBD", list(t = 4, reps = 0, seed = 1))),
              lapply(list("run_app", "not_an_engine", "base::system", NA_character_, c("RCBD", "CRD")),
                     run_design_job, args = list()))
  expect_true(all(vapply(failed, function(job) !job$ok && inherits(job$condition, "fieldhub_input_error"),
                         logical(1))))
  expect_false(any(vapply(list(NULL, 1, list(1), stats::setNames(list(1, 2), c("t", "t"))),
                          function(args) run_design_job("RCBD", args)$ok, logical(1))))
})

test_that("design jobs keep warnings even when generation fails", {
  warned <- run_design_job("RCBD", list(t = 4, reps = 2, l = 2, plotNumber = 101, seed = 1))
  expect_true(warned$ok && any(vapply(warned$warnings, inherits, logical(1), "fieldhub_default_warning")))
  testthat::local_mocked_bindings(
    RCBD = function(...) {
      fieldhub_warn("A recorded warning", class = "fieldhub_design_warning")
      fieldhub_abort("A recorded error")
    }, .package = "FielDHub")
  bad <- run_design_job("RCBD", list(seed = 1))
  expect_true(!bad$ok && length(bad$warnings) == 1L && inherits(bad$condition, "fieldhub_input_error"))
})

test_that("jobs use the submitted RNG kind and restore process state", {
  local_rng_state()
  previous <- RNGkind()
  on.exit(do.call(RNGkind, as.list(previous)), add = TRUE, after = FALSE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  submitted_kind <- RNGkind()
  args <- list(t = 12, nrows = 3, reps = 2, method = "twostage", seed = 4)
  expected <- do.call(row_column, args)
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  set.seed(9)
  seed <- .Random.seed
  kind <- RNGkind()
  opts <- options()
  job <- run_design_job("row_column", args, rng_kind = submitted_kind)
  expect_equal(job$value, expected)
  expect_equal(list(.Random.seed, RNGkind(), options()), list(seed, kind, opts))
  expect_false(run_design_job("RCBD", list(t = 4, reps = 0), rng_kind = c("bad", "kind", "value"))$ok)
  expect_identical(list(.Random.seed, RNGkind()), list(seed, kind))
})

test_that("the app runs without workers by default and rejects a bad worker count", {
  skip_if_not_installed("shiny")
  expect_identical(formals(run_app)$workers, 0L)
  expect_problems(lapply(list(NULL, NA, -1, 1.5, "2", c(1, 2), Inf), function(workers) {
    function() run_app(workers = workers, launch.browser = FALSE)
  }), "", case = "worker counts")
})

test_that("worker pools belong to the app and close once", {
  started <- stopped <- list()
  callbacks <- list()
  runtime <- app_worker_lifecycle(2, available = function() TRUE, installed = function() TRUE,
    start_pool = function(workers, profile) started[[length(started) + 1L]] <<- list(workers, profile),
    stop_pool = function(profile) stopped[[length(stopped) + 1L]] <<- profile)
  expect_identical(runtime$backend, "sync")
  set.seed(34)
  before <- .Random.seed
  opts <- options()
  runtime$start(register_stop = function(callback) callbacks[[length(callbacks) + 1L]] <<- callback)
  # the pool runs on the app's own profile, not the default one
  expect_equal(list(runtime$backend, started, runtime$profile %in% c("default", "")),
               list("mirai", list(list(2, runtime$profile)), FALSE))
  expect_equal(list(.Random.seed, options()), list(before, opts))
  callbacks[[1L]]()
  callbacks[[1L]]()
  expect_identical(list(runtime$backend, stopped), list("sync", list(runtime$profile)))
})

test_that("unavailable workers fall back to synchronous runs with one notice", {
  for (reason in c("disabled", "missing", "development", "startup")) {
    notices <- character()
    runtime <- app_worker_lifecycle(if (reason == "disabled") 0L else 2L,
      available = function() reason != "missing", installed = function() reason != "development",
      start_pool = function(workers, profile) stop("cannot start"),
      stop_pool = function(profile) NULL,
      inform = function(message) notices <<- c(notices, message))
    runtime$start(register_stop = function(callback) NULL)
    runtime$start(register_stop = function(callback) NULL)
    expect_equal(list(runtime$backend, length(notices)), list("sync", if (reason == "disabled") 0L else 1L),
                 info = reason)
  }
})

test_that("task buttons announce the results they update and the app UI renders", {
  skip_if_not_installed("shiny")
  previous <- options(sass.cache = FALSE)
  on.exit(options(previous), add = TRUE)
  button <- as.character(app_task_button("x-run", "Run!", results_id = "x-results"))
  expect_match(button, 'aria-controls="x-results"', fixed = TRUE)
  # deferred bslib dependencies can reject the theme even when as.character()
  # of an individual control succeeds
  expect_no_error(htmltools::renderTags(app_ui(NULL)))
})

# --- The diagonal page: cached percentages of checks ------------------------

test_that("the diagonal page shows the design of the latest choices, cached or not", {
  for (package in app_dependencies()) skip_if_not_installed(package)
  spec <- design_app_spec("Diagonal")
  raw <- page_raw("Diagonal", lines = 60, seed = 5, dist_method = "manhattan")
  # what a direct API call builds for the field and percentage the page shows
  direct <- function(percent, seed = 5) {
    diagonal_arrangement(nrows = 7, ncols = 10, lines = 60, checks = 4, l = 1, plotNumber = 1, seed = seed,
                         exptName = "Expt1", locationNames = "FARGO", checksPercent = percent,
                         spread_checks = TRUE, dist_method = "manhattan")$fieldBook
  }
  seen <- new.env()
  shiny::testServer(mod_design_server, args = list(spec = spec), {
    settle <- function() for (i in 1:5) { session$flushReact(); later::run_now(0.02) }
    shown <- function() tryCatch(session$returned$design()$fieldBook, error = function(e) class(e)[1])
    do.call(session$setInputs, raw)
    session$setInputs(run = 1)
    session$setInputs(dimensions = "7 x 10")
    session$setInputs(randomize = 1)
    offered <- session$returned$design_choices()$checks_percent$choices
    settle()
    got <- list(first = shown())
    # two choices before the first finishes: only the latest is ever shown
    session$setInputs(checks_percent = as.character(offered[2]))
    session$setInputs(checks_percent = as.character(offered[1]))
    trace <- lapply(1:5, function(i) { later::run_now(0.02); session$flushReact(); shown() })
    got$latest <- unique(Filter(is.data.frame, trace))
    # every offered percentage builds the API's design with that percentage
    # and the page's optimization options, whether computed or cached
    for (percent in c(rev(offered), utils::tail(offered, 1L))) {
      session$setInputs(checks_percent = as.character(percent))
      settle()
      got[[paste("percent", percent, length(got))]] <- shown()
    }
    # a new seed is a new request: the design cached for the old one is not shown
    session$setInputs(seed = 6, run = 2)
    session$setInputs(randomize = 2)
    settle()
    got$new_seed <- shown()
    # an invalid run clears the design instead of showing the last one
    session$setInputs(lines = NA, run = 3)
    session$setInputs(randomize = 3)
    settle()
    got$cleared <- shown()
    want <- c(list(first = direct(utils::tail(offered, 1L)), latest = list(direct(offered[1]))),
              lapply(c(rev(offered), utils::tail(offered, 1L)), direct),
              list(new_seed = direct(utils::tail(offered, 1L), seed = 6), cleared = "shiny.silent.error"))
    seen$got <- got
    seen$want <- want
  })
  same <- mapply(function(a, b) isTRUE(all.equal(a, b)), seen$got, seen$want)
  expect(length(seen$got) == length(seen$want) && all(same),
         paste("the page does not show the expected design for:", paste(names(seen$got)[!same], collapse = ", ")))
})

test_that("with workers, the diagonal page shows a percentage prepared in the background as soon as it is chosen", {
  for (package in c(app_dependencies(), "mirai")) skip_if_not_installed(package)
  skip_on_cran()
  runtime <- app_worker_lifecycle(2L)
  stops <- list()
  runtime$start(register_stop = function(callback) stops[[length(stops) + 1L]] <<- callback)
  on.exit(for (callback in stops) callback(), add = TRUE)
  skip_if_not(identical(runtime$backend, "mirai"), "workers could not start")
  session <- shiny::MockShinySession$new()
  session$userData$fieldhub_task_runtime <- runtime
  raw <- page_raw("Diagonal", lines = 60, seed = 5, dist_method = "manhattan")
  seen <- new.env()
  shiny::testServer(mod_design_server, args = list(spec = design_app_spec("Diagonal")), session = session, {
    shown <- function() tryCatch(session$returned$design()$fieldBook, error = function(e) NULL)
    do.call(session$setInputs, raw)
    session$setInputs(run = 1, dimensions = "7 x 10")
    session$setInputs(randomize = 1)
    seen$offered <- session$returned$design_choices()$checks_percent$choices
    # the selected percentage first, then the others on the idle workers
    for (i in 1:100) { later::run_now(0.05); session$flushReact() }
    # each other percentage is shown in the same flush that chooses it, with no worker round trip
    seen$got <- lapply(utils::head(seen$offered, -1L), function(percent) {
      session$setInputs(checks_percent = as.character(percent))
      shown()
    })
  })
  want <- lapply(utils::head(seen$offered, -1L), function(percent) {
    diagonal_arrangement(nrows = 7, ncols = 10, lines = 60, checks = 4, l = 1, plotNumber = 1, seed = 5,
                         exptName = "Expt1", locationNames = "FARGO", checksPercent = percent,
                         spread_checks = TRUE, dist_method = "manhattan")$fieldBook
  })
  expect_equal(seen$got, want)
})
