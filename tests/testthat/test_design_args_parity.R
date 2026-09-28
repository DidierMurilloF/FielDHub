library(testthat)
library(FielDHub)

# Task 11a (M2 AD-01): every classic module builds its API call through a
# plain design_args_<Module>(values, data) function (R/app_design_args.R)
# instead of assembling arguments inline. These tests prove app/API parity:
# do.call(<engine>, design_args_<Module>(values, data)) must reproduce the
# exact fieldBook and metadata$parameters of an equivalent direct API call,
# for the same inputs and seed.

# shiny_shaped() and parity() live in helper-parity.R (shared with
# test_design_module.R).

#' Muffle only the onestage->twostage fallback warning row_column() raises
#' for some (t, nrows) combinations, so an unrelated warning still surfaces.
#' @noRd
suppress_design_warning <- function(expr) {
  withCallingHandlers(expr, fieldhub_design_warning = function(w) invokeRestart("muffleWarning"))
}

# --- Step 1: one primary parity test per design (l = 2 wherever the engine
# supports locations), plus an uploaded-data parity test for every design
# (all twelve classic engines accept `data`). ---

test_that("CRD app arguments reproduce the API design", {
  values <- design_values_CRD(treatment_count = 5, reps = 3, planter = "serpentine",
                              plot_start = 101, location_names = "FARGO", seed = 11, data = NULL)
  parity(design_args_CRD, CRD, values,
         direct = CRD(t = 5, reps = 3, plotNumber = 101, locationNames = "FARGO", seed = 11))
})

test_that("CRD app arguments reproduce an uploaded-data design", {
  # crd_inputs() always computes a bare treatment count from get_data_crd()
  # (nrow(data) on this path) and hands it to design_values_CRD() alongside
  # `data`; model that exactly, instead of a `values` that conveniently omits
  # `t`, so this test would have caught the regression where the module sent
  # `t = 4` through to metadata$parameters even though CRD(data = ...) never
  # records a `t`.
  data <- data.frame(TREATMENT = paste0("ND-", 1:4), REP = 4)
  values <- design_values_CRD(treatment_count = nrow(data), reps = NULL, planter = "serpentine",
                              plot_start = 2001, location_names = "Cali", seed = 21, data = data)
  expect_null(values$t)
  parity(design_args_CRD, CRD, values, data,
         direct = CRD(plotNumber = 2001, locationNames = "Cali", seed = 21, data = data))
})

test_that("design_values_CRD() nulls the treatment count whenever data is supplied", {
  with_data <- design_values_CRD(treatment_count = 4, reps = 2, planter = "serpentine",
                                 plot_start = 101, location_names = "A", seed = 1,
                                 data = data.frame(TREATMENT = "A", REP = 1))
  expect_null(with_data$t)
  without_data <- design_values_CRD(treatment_count = 4, reps = 2, planter = "serpentine",
                                    plot_start = 101, location_names = "A", seed = 1, data = NULL)
  expect_identical(without_data$t, 4)
})

test_that("RCBD app arguments reproduce the API design with checks and two locations", {
  values <- list(t = 6, reps = 3, l = 2, planter = "cartesian", plot_start = c(101, 1001),
                 location_names = c("A", "B"), continuous = FALSE, seed = 4,
                 checks = 2, rep_checks = c(2, 2), spread_checks = TRUE)
  parity(design_args_RCBD, RCBD, values,
         direct = RCBD(t = 6, reps = 3, l = 2, planter = "cartesian", plotNumber = c(101, 1001),
                       locationNames = c("A", "B"), continuous = FALSE, seed = 4,
                       checks = 2, rep_checks = c(2, 2), spread_checks = TRUE))
})

test_that("RCBD app arguments reproduce an uploaded-data design", {
  data <- data.frame(TREATMENT = paste0("ND-", 1:6))
  values <- list(reps = 3, l = 1, plot_start = 101, location_names = "IBAGUE",
                 continuous = FALSE, seed = 13)
  parity(design_args_RCBD, RCBD, values, data,
         direct = RCBD(reps = 3, l = 1, plotNumber = 101, continuous = FALSE,
                       locationNames = "IBAGUE", seed = 13, data = data))
})

test_that("LSD app arguments reproduce the API design", {
  values <- list(t = 5, reps = 2, plot_start = 101, planter = "cartesian",
                 location_names = "Loc1", seed = 7)
  parity(design_args_LSD, latin_square, values,
         direct = latin_square(t = 5, reps = 2, plotNumber = 101, planter = "cartesian",
                               locationNames = "Loc1", seed = 7))
})

test_that("LSD app arguments reproduce an uploaded-data design", {
  data <- data.frame(ROW = paste0("Period", 1:4), COLUMN = paste0("Cow", 1:4),
                     TREATMENT = paste0("Diet", 1:4))
  values <- list(reps = 1, plot_start = 101, planter = "serpentine",
                 location_names = "A", seed = 9)
  parity(design_args_LSD, latin_square, values, data,
         direct = latin_square(reps = 1, plotNumber = 101, planter = "serpentine",
                               locationNames = "A", seed = 9, data = data))
})

test_that("FD app arguments reproduce the API design across two locations", {
  values <- list(setfactors = c(2, 3), reps = 2, l = 2, type = 2, plot_start = c(101, 1001),
                 planter = "serpentine", location_names = c("A", "B"), seed = 5)
  parity(design_args_FD, full_factorial, values,
         direct = full_factorial(setfactors = c(2, 3), reps = 2, l = 2, type = 2,
                                 plotNumber = c(101, 1001), planter = "serpentine",
                                 locationNames = c("A", "B"), seed = 5))
})

test_that("FD app arguments reproduce an uploaded-data design", {
  data <- data.frame(FACTOR = rep(c("A", "B"), c(2, 2)), LEVEL = c("a0", "a1", "b0", "b1"))
  values <- list(reps = 2, l = 1, type = 1, plot_start = 101, planter = "serpentine",
                 location_names = "Loc", seed = 3)
  parity(design_args_FD, full_factorial, values, data,
         direct = full_factorial(reps = 2, l = 1, type = 1, plotNumber = 101,
                                 planter = "serpentine", locationNames = "Loc", seed = 3,
                                 data = data))
})

test_that("SPD app arguments reproduce the API design across two locations", {
  values <- design_values_SPD(wp_count = 4, sp_count = 2, reps = 3, l = 2, seed = 8,
                              planter = NULL, plot_start = c(101, 1001),
                              location_names = c("A", "B"), type = 2, data = NULL)
  parity(design_args_SPD, split_plot, values,
         direct = split_plot(wp = 4, sp = 2, reps = 3, l = 2, type = 2,
                             plotNumber = c(101, 1001), locationNames = c("A", "B"), seed = 8))
})

test_that("SPD app arguments reproduce an uploaded-data design", {
  # spd_inputs() always reads get_data_spd()$treatments[1]/[2] as wp_count/
  # sp_count; on the upload path that vector is the uploaded WHOLEPLOT/SUBPLOT
  # labels concatenated, so treatments[1] is a WP label and treatments[2] is
  # a SECOND WP label, not a SP label. Model that exactly (garbage in) and
  # confirm design_values_SPD() still nulls both out because `data` is
  # supplied, instead of a `values` that conveniently passes real counts.
  wp <- c("A", "B"); sp <- c("s1", "s2", "s3")
  data <- data.frame(WHOLEPLOT = c(wp, NA), SUBPLOT = sp)
  treatments <- c(wp, sp)
  values <- design_values_SPD(wp_count = treatments[1], sp_count = treatments[2], reps = 2,
                              l = 1, seed = 4, planter = NULL, plot_start = 101,
                              location_names = "Loc1", type = 2, data = data)
  expect_null(values$wp)
  expect_null(values$sp)
  parity(design_args_SPD, split_plot, values, data,
         direct = split_plot(reps = 2, l = 1, type = 2, plotNumber = 101,
                             locationNames = "Loc1", seed = 4, data = data))
})

test_that("design_values_SPD() nulls wp/sp whenever data is supplied", {
  with_data <- design_values_SPD(wp_count = "A", sp_count = "B", reps = 2, l = 1, seed = 1,
                                 planter = NULL, plot_start = 101, location_names = "Loc",
                                 type = 2, data = data.frame(WHOLEPLOT = "A", SUBPLOT = "B"))
  expect_null(with_data$wp)
  expect_null(with_data$sp)
  without_data <- design_values_SPD(wp_count = 3, sp_count = 2, reps = 2, l = 1, seed = 1,
                                    planter = NULL, plot_start = 101, location_names = "Loc",
                                    type = 2, data = NULL)
  expect_identical(without_data$wp, 3)
  expect_identical(without_data$sp, 2)
})

test_that("SSPD app arguments reproduce the API design across two locations", {
  values <- design_values_SSPD(wp_count = 2, sp_count = 2, ssp_count = 3, reps = 2, l = 2,
                               seed = 6, planter = NULL, plot_start = c(101, 1001),
                               location_names = c("A", "B"), type = 2, data = NULL)
  parity(design_args_SSPD, split_split_plot, values,
         direct = split_split_plot(wp = 2, sp = 2, ssp = 3, reps = 2, l = 2, type = 2,
                                   plotNumber = c(101, 1001), locationNames = c("A", "B"), seed = 6))
})

test_that("SSPD app arguments reproduce an uploaded-data design", {
  # sspd_inputs() always reads get_data_sspd()$treatments[1:3] as wp_count/
  # sp_count/ssp_count; on the upload path that vector is the uploaded
  # WHOLEPLOT/SUBPLOT/SUB_SUBPLOT labels concatenated, so these are WP labels,
  # not a wp/sp/ssp count. Model that exactly (garbage in) and confirm
  # design_values_SSPD() still nulls all three out because `data` is supplied.
  wp <- c("A", "B"); sp <- c("s1", "s2"); ssp <- c("x1", "x2", "x3")
  data <- data.frame(WHOLEPLOT = c(wp, NA), SUBPLOT = c(sp, NA), SUB_SUBPLOT = ssp)
  treatments <- c(wp, sp, ssp)
  values <- design_values_SSPD(wp_count = treatments[1], sp_count = treatments[2],
                               ssp_count = treatments[3], reps = 1, l = 1, seed = 2,
                               planter = NULL, plot_start = 101, location_names = "Loc",
                               type = 1, data = data)
  expect_null(values$wp)
  expect_null(values$sp)
  expect_null(values$ssp)
  parity(design_args_SSPD, split_split_plot, values, data,
         direct = split_split_plot(reps = 1, l = 1, type = 1, plotNumber = 101,
                                   locationNames = "Loc", seed = 2, data = data))
})

test_that("design_values_SSPD() nulls wp/sp/ssp whenever data is supplied", {
  with_data <- design_values_SSPD(wp_count = "A", sp_count = "B", ssp_count = "C", reps = 1,
                                  l = 1, seed = 1, planter = NULL, plot_start = 101,
                                  location_names = "Loc", type = 1,
                                  data = data.frame(WHOLEPLOT = "A", SUBPLOT = "B", SUB_SUBPLOT = "C"))
  expect_null(with_data$wp)
  expect_null(with_data$sp)
  expect_null(with_data$ssp)
  without_data <- design_values_SSPD(wp_count = 2, sp_count = 2, ssp_count = 2, reps = 1, l = 1,
                                     seed = 1, planter = NULL, plot_start = 101,
                                     location_names = "Loc", type = 1, data = NULL)
  expect_identical(without_data$wp, 2)
  expect_identical(without_data$sp, 2)
  expect_identical(without_data$ssp, 2)
})

test_that("STRIPD app arguments reproduce the API design across two locations", {
  values <- list(Hplots = 4, Vplots = 3, reps = 2, l = 2, planter = "cartesian",
                 plot_start = c(101, 1001), location_names = c("A", "B"), seed = 12,
                 randomizeH = TRUE, randomizeV = FALSE)
  parity(design_args_STRIPD, strip_plot, values,
         direct = strip_plot(Hplots = 4, Vplots = 3, reps = 2, l = 2, planter = "cartesian",
                             plotNumber = c(101, 1001), locationNames = c("A", "B"), seed = 12,
                             randomizeH = TRUE, randomizeV = FALSE))
})

test_that("STRIPD app arguments accept a single replicate and reproduce an uploaded-data design", {
  # strip_plot() accepts reps = 1 (unlike the module's former "at least 2
  # replicates" alert, dropped because the engine no longer needs it).
  Hplots <- c("H1", "H2"); Vplots <- c("V1", "V2", "V3")
  data <- data.frame(Hplot = c(Hplots, NA), Vplot = Vplots)
  values <- list(reps = 1, l = 1, planter = "serpentine", plot_start = 101,
                 location_names = "Loc", seed = 1, randomizeH = TRUE, randomizeV = FALSE)
  parity(design_args_STRIPD, strip_plot, values, data,
         direct = strip_plot(reps = 1, l = 1, planter = "serpentine", plotNumber = 101,
                             locationNames = "Loc", seed = 1, randomizeH = TRUE,
                             randomizeV = FALSE, data = data))
})

test_that("IBD app arguments reproduce the API design across two locations", {
  values <- list(t = 12, k = 4, reps = 2, l = 2, plot_start = c(101, 1001),
                 location_names = c("A", "B"), seed = 9)
  parity(design_args_IBD, incomplete_blocks, values,
         direct = incomplete_blocks(t = 12, k = 4, reps = 2, l = 2, plotNumber = c(101, 1001),
                                    locationNames = c("A", "B"), seed = 9))
})

test_that("IBD app arguments reproduce an uploaded-data design", {
  data <- data.frame(ENTRY = 1:12, TREATMENT = paste0("TX-", 1:12))
  values <- list(t = 12, k = 4, reps = 2, l = 1, plot_start = 101, location_names = "Loc", seed = 3)
  parity(design_args_IBD, incomplete_blocks, values, data,
         direct = incomplete_blocks(t = 12, k = 4, reps = 2, l = 1, plotNumber = 101,
                                    locationNames = "Loc", seed = 3, data = data))
})

test_that("RowCol app arguments reproduce the API design across two locations", {
  values <- list(t = 9, nrows = 3, reps = 2, l = 2, plot_start = c(101, 1001),
                 location_names = c("A", "B"), seed = 14)
  via_app <- suppress_design_warning(do.call(row_column, design_args_RowCol(shiny_shaped(values))))
  direct <- suppress_design_warning(row_column(t = 9, nrows = 3, reps = 2, l = 2,
                                               plotNumber = c(101, 1001), locationNames = c("A", "B"),
                                               seed = 14))
  expect_identical(via_app$fieldBook, direct$fieldBook)
  expect_identical(via_app$metadata$parameters, direct$metadata$parameters)
})

test_that("RowCol app arguments reproduce an uploaded-data design", {
  data <- data.frame(ENTRY = 1:9, TREATMENT = paste0("ND-", 1:9))
  values <- list(t = 9, nrows = 3, reps = 2, l = 1, plot_start = 101, location_names = "A", seed = 5)
  via_app <- suppress_design_warning(do.call(row_column, design_args_RowCol(shiny_shaped(values), data)))
  direct <- suppress_design_warning(row_column(t = 9, nrows = 3, reps = 2, l = 1, plotNumber = 101,
                                               locationNames = "A", seed = 5, data = data))
  expect_identical(via_app$fieldBook, direct$fieldBook)
  expect_identical(via_app$metadata$parameters, direct$metadata$parameters)
})

test_that("Alpha_Lattice app arguments reproduce the API design across two locations", {
  values <- list(t = 9, k = 3, reps = 2, l = 2, plot_start = c(101, 1001),
                 location_names = c("A", "B"), seed = 1)
  parity(design_args_Alpha_Lattice, alpha_lattice, values,
         direct = alpha_lattice(t = 9, k = 3, reps = 2, l = 2, plotNumber = c(101, 1001),
                                locationNames = c("A", "B"), seed = 1))
})

test_that("Alpha_Lattice app arguments reproduce an uploaded-data design", {
  data <- data.frame(ENTRY = 1:9, TREATMENT = paste0("ND-", 1:9))
  values <- list(t = 9, k = 3, reps = 2, l = 1, plot_start = 101, location_names = "Loc", seed = 1)
  parity(design_args_Alpha_Lattice, alpha_lattice, values, data,
         direct = alpha_lattice(t = 9, k = 3, reps = 2, l = 1, plotNumber = 101,
                                locationNames = "Loc", seed = 1, data = data))
})

test_that("Square_Lattice app arguments reproduce the API design across two locations", {
  values <- list(t = 9, k = 3, reps = 2, l = 2, plot_start = c(101, 1001),
                 location_names = c("A", "B"), seed = 1)
  parity(design_args_Square_Lattice, square_lattice, values,
         direct = square_lattice(t = 9, k = 3, reps = 2, l = 2, plotNumber = c(101, 1001),
                                 locationNames = c("A", "B"), seed = 1))
})

test_that("Square_Lattice app arguments reproduce an uploaded-data design", {
  data <- data.frame(ENTRY = 1:9, TREATMENT = paste0("ND-", 1:9))
  values <- list(t = 9, k = 3, reps = 2, l = 1, plot_start = 101, location_names = "Loc", seed = 1)
  parity(design_args_Square_Lattice, square_lattice, values, data,
         direct = square_lattice(t = 9, k = 3, reps = 2, l = 1, plotNumber = 101,
                                 locationNames = "Loc", seed = 1, data = data))
})

test_that("Rectangular_Lattice app arguments reproduce the API design across two locations", {
  values <- list(t = 6, k = 2, reps = 2, l = 2, plot_start = c(101, 1001),
                 location_names = c("A", "B"), seed = 1)
  parity(design_args_Rectangular_Lattice, rectangular_lattice, values,
         direct = rectangular_lattice(t = 6, k = 2, reps = 2, l = 2, plotNumber = c(101, 1001),
                                      locationNames = c("A", "B"), seed = 1))
})

test_that("Rectangular_Lattice app arguments reproduce an uploaded-data design", {
  data <- data.frame(ENTRY = 1:6, TREATMENT = paste0("ND-", 1:6))
  values <- list(t = 6, k = 2, reps = 2, l = 1, plot_start = 101, location_names = "Loc", seed = 1)
  parity(design_args_Rectangular_Lattice, rectangular_lattice, values, data,
         direct = rectangular_lattice(t = 6, k = 2, reps = 2, l = 1, plotNumber = 101,
                                      locationNames = "Loc", seed = 1, data = data))
})

# --- Builder edge cases: an optional value missing from `values` reproduces
# the API's own default (rather than overriding it with an explicit NULL). ---

test_that("design_args_CRD() only returns the fields CRD() takes", {
  built <- design_args_CRD(list(t = 4, reps = 2, plot_start = 101, seed = 1))
  expect_identical(names(built), c("t", "reps", "plotNumber", "locationNames", "seed", "data"))
  expect_null(built$locationNames)
  expect_null(built$data)
})

test_that("design_args_RCBD() falls back to l/planter/continuous/spread_checks defaults", {
  values <- list(t = 6, reps = 3, plot_start = 101, location_names = "FARGO", seed = 4)
  parity(design_args_RCBD, RCBD, values,
         direct = RCBD(t = 6, reps = 3, plotNumber = 101, locationNames = "FARGO", seed = 4))
  built <- design_args_RCBD(shiny_shaped(values))
  expect_identical(built$l, 1)
  expect_identical(built$planter, "serpentine")
  expect_false(built$continuous)
  expect_true(built$spread_checks)
  expect_null(built$checks)
  expect_null(built$rep_checks)
})

test_that("design_args_LSD() falls back to the planter default", {
  values <- list(t = 4, reps = 1, plot_start = 101, location_names = "Loc", seed = 1)
  parity(design_args_LSD, latin_square, values,
         direct = latin_square(t = 4, reps = 1, plotNumber = 101, locationNames = "Loc", seed = 1))
  expect_identical(design_args_LSD(shiny_shaped(values))$planter, "serpentine")
})

test_that("design_args_FD() falls back to l/type/planter defaults", {
  values <- list(setfactors = c(2, 2), reps = 2, plot_start = 101, location_names = "Loc", seed = 1)
  parity(design_args_FD, full_factorial, values,
         direct = full_factorial(setfactors = c(2, 2), reps = 2, plotNumber = 101,
                                 locationNames = "Loc", seed = 1))
  built <- design_args_FD(shiny_shaped(values))
  expect_identical(built$l, 1)
  expect_identical(built$type, 2)
  expect_identical(built$planter, "serpentine")
})

test_that("design_args_SPD() and design_args_SSPD() fall back to l/type defaults", {
  values <- list(wp = 3, sp = 2, reps = 2, plot_start = 101, location_names = "Loc", seed = 1)
  parity(design_args_SPD, split_plot, values,
         direct = split_plot(wp = 3, sp = 2, reps = 2, plotNumber = 101,
                             locationNames = "Loc", seed = 1))
  built <- design_args_SPD(shiny_shaped(values))
  expect_identical(built$l, 1)
  expect_identical(built$type, 2)

  values2 <- list(wp = 2, sp = 2, ssp = 2, reps = 1, plot_start = 101, location_names = "Loc", seed = 1)
  parity(design_args_SSPD, split_split_plot, values2,
         direct = split_split_plot(wp = 2, sp = 2, ssp = 2, reps = 1, plotNumber = 101,
                                   locationNames = "Loc", seed = 1))
  built2 <- design_args_SSPD(shiny_shaped(values2))
  expect_identical(built2$l, 1)
  expect_identical(built2$type, 2)
})

test_that("design_args_STRIPD() falls back to l/planter/randomizeH/randomizeV defaults", {
  values <- list(Hplots = 3, Vplots = 3, reps = 1, plot_start = 101, location_names = "Loc", seed = 1)
  parity(design_args_STRIPD, strip_plot, values,
         direct = strip_plot(Hplots = 3, Vplots = 3, reps = 1, plotNumber = 101,
                             locationNames = "Loc", seed = 1))
  built <- design_args_STRIPD(shiny_shaped(values))
  expect_identical(built$l, 1)
  expect_identical(built$planter, "serpentine")
  expect_true(built$randomizeH)
  expect_false(built$randomizeV)
})

test_that("design_args_IBD()/design_args_Alpha_Lattice()/design_args_Square_Lattice()/design_args_Rectangular_Lattice() fall back to the l default", {
  values <- list(t = 12, k = 4, reps = 2, plot_start = 101, location_names = "Loc", seed = 1)
  parity(design_args_IBD, incomplete_blocks, values,
         direct = incomplete_blocks(t = 12, k = 4, reps = 2, plotNumber = 101,
                                    locationNames = "Loc", seed = 1))
  expect_identical(design_args_IBD(shiny_shaped(values))$l, 1)

  values2 <- list(t = 9, k = 3, reps = 2, plot_start = 101, location_names = "Loc", seed = 1)
  parity(design_args_Alpha_Lattice, alpha_lattice, values2,
         direct = alpha_lattice(t = 9, k = 3, reps = 2, plotNumber = 101,
                                locationNames = "Loc", seed = 1))
  expect_identical(design_args_Alpha_Lattice(shiny_shaped(values2))$l, 1)
  parity(design_args_Square_Lattice, square_lattice, values2,
         direct = square_lattice(t = 9, k = 3, reps = 2, plotNumber = 101,
                                 locationNames = "Loc", seed = 1))
  expect_identical(design_args_Square_Lattice(shiny_shaped(values2))$l, 1)

  values3 <- list(t = 6, k = 2, reps = 2, plot_start = 101, location_names = "Loc", seed = 1)
  parity(design_args_Rectangular_Lattice, rectangular_lattice, values3,
         direct = rectangular_lattice(t = 6, k = 2, reps = 2, plotNumber = 101,
                                      locationNames = "Loc", seed = 1))
  expect_identical(design_args_Rectangular_Lattice(shiny_shaped(values3))$l, 1)
})

test_that("design_args_RowCol() falls back to the l default", {
  # Not run through row_column() itself here: the onestage optimizer's
  # default search budget makes every extra call noticeably slower, and the
  # l = 2 and uploaded-data tests above already exercise real do.call()
  # parity for this builder. This checks the pure translation instead: a
  # `values` list missing `l` still asks the engine for its own default (1),
  # not for the string "l" would otherwise partially match by `$` (`values$l`
  # would silently read `location_names`; the builder uses `values[["l"]]`
  # precisely to avoid that).
  values <- list(t = 8, nrows = 2, reps = 2, plot_start = 101, location_names = "A", seed = 1)
  built <- design_args_RowCol(shiny_shaped(values))
  expect_identical(built$l, 1)
  expect_identical(names(built), c("t", "nrows", "reps", "l", "plotNumber", "seed",
                                   "locationNames", "data"))
})

# --- Step 2 (ruling R2): structural checks on the classic module bodies,
# via the namespace so they also work under R CMD check (no R/ directory). ---

test_that("classic pages build their design only through design_args_<Module>() and do.call()", {
  classic_engines <- list(
    CRD = "CRD", RCBD = "RCBD", LSD = "latin_square", FD = "full_factorial",
    SPD = "split_plot", SSPD = "split_split_plot", STRIPD = "strip_plot",
    IBD = "incomplete_blocks", RowCol = "row_column",
    Alpha_Lattice = "alpha_lattice", Square_Lattice = "square_lattice",
    Rectangular_Lattice = "rectangular_lattice"
  )
  # Unexported helpers the generic page server (mod_design_server()) and the
  # page specs (design_app_spec()) may call, and why. Anything else found in
  # them must be a base/shiny/DT/plotly/shinyjs call, or is a bug.
  allowed_helpers <- c(
    "validate_design",     # shows any error where the output would be (R/app_conditions.R)
    "app_design_seed",     # resolves the optional app seed without touching the shared RNG stream
    "app_read_upload",     # reads/validates the page's upload, reporting a failure itself (R/app_upload.R)
    "app_upload_spec",     # the page's upload ids (the toggle it reads)
    "app_upload_dialog_observer", # opens the shared entries-format dialog when the upload toggle switches to "Yes"
    "app_classic_layout",  # shared layout-panel lifecycle for classic design pages
    "app_classic_workflow",# shared results/simulation/export lifecycle for classic design pages
    "read_design_controls",   # parses the page's controls (R/validate_design_controls.R)
    "design_control_choices", # choices of a select computed from other controls or the upload
    "design_upload_data",  # says why a run with a failed upload does nothing
    "design_values_CRD",   # assembles CRD's values from parsed inputs (nulls t when data is given)
    "design_values_SPD",   # assembles SPD's values from parsed inputs (nulls wp/sp when data is given)
    "design_values_SSPD",  # assembles SSPD's values from parsed inputs (nulls wp/sp/ssp when data is given)
    "upload_level_counts", # counts the strips of an uploaded strip-plot file
    "classic_workflow_spec", # registry of per-design IDs/labels the shared workflow helpers use
    "app_spatial_page"     # the steps and results of a spatial page (checked below)
  )
  # `default_entries` is deliberately absent from allowed_helpers: no classic
  # page still needs it to build a generated-path entry table (that would
  # defeat decision 2 -- pass bare counts and let the engine generate the
  # same "G-" labels itself). Its absence is a regression guard, enforced by
  # the setdiff() checks below: if a page's values called default_entries(nt)
  # again (as mod_IBD.R/mod_RowCol.R/etc. used to), that symbol would show up
  # in `called` but not in `allowed`, and the test would fail.
  forbidden <- c("sample", "set.seed", "runif", "get.levels", "blocksdesign")

  namespace <- asNamespace("FielDHub")
  all_objects <- mget(ls(namespace, all.names = TRUE), namespace, inherits = FALSE)
  package_functions <- names(Filter(is.function, all_objects))

  code <- body(mod_design_server)
  nm <- all.names(code)
  expect_identical(sum(nm == "do.call"), 1L)
  expect_true(grepl("do.call(spec$engine, spec$args(", paste(deparse(code), collapse = " "), fixed = TRUE))
  expect_identical(intersect(nm, forbidden), character(0))
  expect_identical(intersect(nm, c(unlist(classic_engines), paste0("design_args_", names(classic_engines)))),
                   character(0))
  expect_identical(setdiff(intersect(nm, package_functions), allowed_helpers), character(0))

  # Everything a spec runs while its page is used (values, data, the upload
  # shape and check, parsers, computed choices and previews): plain parsers
  # and choice helpers only
  spec_helpers <- c(
    "shape_design_upload",        # names and completes the uploaded columns
    "check_factorial_upload",     # an uploaded factorial list needs two factors
    "block_size_choices",         # block sizes offered for a number of entries
    "rcbd_checks_note",           # RCBD block-size preview
    "parse_control_number", "parse_control_whole_numbers", "parse_control_factor_counts",
    "parse_control_names", "parse_control_choice", "parse_control_option", "parse_control_flag",
    "parse_control_seed", "parse_control_rep_checks", "parse_control_checks"
  )
  for (module in names(classic_engines)) {
    spec <- design_app_spec(module)
    expect_identical(spec$engine, get(classic_engines[[module]], namespace), info = module)
    expect_identical(spec$args, get(paste0("design_args_", module), namespace), info = module)
    runtime <- spec_runtime_functions(spec)
    called <- runtime$named
    for (f in runtime$closures) {
      nm <- fieldhub_call_heads(body(f))
      expect_identical(intersect(nm, forbidden), character(0), info = module)
      called <- c(called, intersect(nm, package_functions))
    }
    expect_true(length(runtime$closures) > length(spec$controls), info = module)
    expect_identical(setdiff(called, c(allowed_helpers, spec_helpers)), character(0), info = module)
    expect_identical(intersect(called, c(unlist(classic_engines), paste0("design_args_", names(classic_engines)))),
                     character(0), info = module)
  }
})

# --- Step 7: direct unit tests for helpers classic modules use that had
# none: validate_plot_starts(), app_csv_archive(), rcbd_fieldbook_cols(). ---

test_that("validate_plot_starts() accepts finite whole-number vectors and rejects everything else", {
  expect_identical(validate_plot_starts(c(101, 1001)), c(101, 1001))
  expect_identical(validate_plot_starts(101L), 101L)
  expect_error(validate_plot_starts("101"), class = "fieldhub_input_error")
  expect_error(validate_plot_starts(101.5), class = "fieldhub_input_error")
  expect_error(validate_plot_starts(Inf), class = "fieldhub_input_error")
  expect_error(validate_plot_starts(NA_real_), class = "fieldhub_input_error")
  expect_error(validate_plot_starts(matrix(101, 1, 1)), class = "fieldhub_input_error")
})

test_that("rcbd_fieldbook_cols() orders the field book with and without checks", {
  expect_identical(rcbd_fieldbook_cols(FALSE), c("ID", "LOCATION", "PLOT", "REP", "TREATMENT"))
  expect_identical(rcbd_fieldbook_cols(TRUE),
                   c("ID", "LOCATION", "PLOT", "REP", "ENTRY", "CHECKS", "TREATMENT"))
})

test_that("app_csv_archive() wraps the already-tested archive writer in a zip download handler", {
  # app_csv_archive() (R/app_export.R) is a thin shiny::downloadHandler()
  # wrapper: handlers <- csv_archive_handlers(...); shiny::downloadHandler(
  # filename = handlers$filename, content = handlers$content, contentType =
  # "application/zip"). The actual archive-writing logic -- write a CSV plus
  # workflow.rds/reproduce.R/README.txt, zip them, and read them back -- is
  # csv_archive_handlers()/write_workflow_archive(), a plain function with
  # its own full tempfile-write/unzip/metadata-check coverage in
  # test_workflow_archives.R ("archive callbacks preserve CSV bytes..."), so
  # it is not repeated here. This test only confirms app_csv_archive() wires
  # that plain function up correctly, checked structurally (no Shiny
  # reactive/test-server context, and no reaching into
  # shiny::downloadHandler()'s private closure layout to call its callbacks,
  # per the "No Shiny tests" rule).
  skip_if_not_installed("shiny")
  code <- body(app_csv_archive)
  expect_identical(sum(all.names(code) == "csv_archive_handlers"), 1L)
  expect_identical(sum(all.names(code) == "downloadHandler"), 1L)
  expect_true(grepl('"application/zip"', paste(deparse(code), collapse = " "), fixed = TRUE))
})

# === Task 11b: the seven spatial modules =====================================
#
# The spatial modules build their design at "Randomize!" time, through the
# same design_args_<Module>(values, data) pattern (plus a
# design_args_<Module>_optim() builder for the do_optim() allocation the
# sparse and multi-location p-rep modules compute at "Run!"). Each `values`
# below is what the module sends: the parsed inputs of Run!, plus the field
# dimensions (and percentage of checks) chosen afterwards.

#' Collect the warnings of one class an expression signals, muffling them
#' @noRd
warnings_of_class <- function(expr, class) {
  found <- list()
  withCallingHandlers(expr, warning = function(w) {
    if (inherits(w, class)) {
      found[[length(found) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  })
  found
}

#' The percentages of checks the diagonal modules offer for a field, as the
#' module reads them from diagonal_check_options() before randomizing
#' @noRd
offered_check_percents <- function(nrows, ncols, checks, lines, kindExpt = "SUDC",
                                   planter = "serpentine", data = NULL,
                                   stacked = "By Row", blocks = NULL) {
  options <- diagonal_check_options(
    n_rows = nrows, n_cols = ncols, checks = checks, Option_NCD = TRUE,
    kindExpt = kindExpt, stacked = stacked, planter_mov1 = planter, data = data,
    dim_data = lines + length(checks), dim_data_1 = lines, Block_Fillers = blocks
  )
  as.numeric(options$dt[, 2])
}

test_that("Diagonal app arguments reproduce the API design across two locations", {
  percents <- offered_check_percents(15, 20, checks = 1:4, lines = 270)
  # The module preselects the last option (the API default)
  values <- list(nrows = 15, ncols = 20, lines = 270, checks = 4, planter = "serpentine",
                 l = 2, plot_start = c(101, 2001), seed = 24, expt_name = "20WRY1",
                 location_names = c("MINOT", "FARGO"), checksPercent = percents[length(percents)])
  parity(design_args_Diagonal, diagonal_arrangement, values,
         direct = diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4,
                                       l = 2, plotNumber = c(101, 2001), seed = 24,
                                       exptName = "20WRY1", locationNames = c("MINOT", "FARGO")))
})

test_that("Diagonal app arguments reproduce an uploaded-data design with a chosen percentage of checks", {
  data <- data.frame(ENTRY = 1:274, NAME = c(paste0("CHECK", 1:4), paste0("LINE", 5:274)))
  percents <- offered_check_percents(15, 20, checks = 1:4, lines = 270, planter = "cartesian")
  # On the upload path the module has no `lines` count: the entries are the file
  values <- list(nrows = 15, ncols = 20, lines = NULL, checks = 4, planter = "cartesian",
                 l = 1, plot_start = 1, seed = 7, expt_name = "Expt1",
                 location_names = "CASSELTON", checksPercent = percents[1])
  parity(design_args_Diagonal, diagonal_arrangement, values, data,
         direct = diagonal_arrangement(nrows = 15, ncols = 20, checks = 4, planter = "cartesian",
                                       plotNumber = 1, seed = 7, exptName = "Expt1",
                                       locationNames = "CASSELTON", data = data,
                                       checksPercent = percents[1]))
  # A generated-path count left over in `values` never reaches the engine
  built <- design_args_Diagonal(utils::modifyList(values, list(lines = 999)), data)
  expect_null(built$lines)
})

test_that("design_args_Diagonal() gives misfit plot starts and names the engine's own defaults (R9)", {
  values <- list(nrows = 15, ncols = 20, lines = 270, checks = 4, planter = "serpentine",
                 l = 2, plot_start = 5, seed = 3, expt_name = "E", location_names = "ONLY")
  built <- design_args_Diagonal(shiny_shaped(values))
  expect_identical(built$plotNumber, default_plot_starts(2, 1001))
  expect_null(built$locationNames)
  # ... silently, where passing the misfit values straight through would
  # raise the DEF-12 fieldhub_default_warning
  found <- warnings_of_class(via_app <- do.call(diagonal_arrangement, built),
                             "fieldhub_default_warning")
  expect_length(found, 0L)
  direct <- diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4, l = 2,
                                 plotNumber = c(1001, 2001), seed = 3, exptName = "E")
  expect_identical(via_app$fieldBook, direct$fieldBook)
  expect_identical(via_app$metadata$parameters, direct$metadata$parameters)
})

test_that("design_args_Diagonal() falls back to the l/planter/plot-start defaults", {
  values <- list(nrows = 15, ncols = 20, lines = 270, checks = 4, seed = 9)
  parity(design_args_Diagonal, diagonal_arrangement, values,
         direct = diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4, seed = 9))
})

test_that("diagonal_arrangement() suggests the field sizes the app offers", {
  # With an unusable field, diagonal_arrangement() lists the rectangular
  # sizes field_dimensions() gives for the lines, the same candidates
  # diagonal_dimension_choices() (the module's dimension list) starts from.
  err <- tryCatch(diagonal_arrangement(nrows = 4, ncols = 70, lines = 270, checks = 4, seed = 1),
                  fieldhub_dimension_error = function(e) e)
  expect_s3_class(err, "fieldhub_dimension_error")
  expect_identical(err$options, dimension_options(unlist(field_dimensions(270))))
  offered <- diagonal_dimension_choices(lines = 270, checks = 1:4)
  expect_true(all(offered %in% unlist(field_dimensions(270))))
  # The module no longer narrows the candidates with its own minimum_extra:
  # both functions, and the error's size range, read the same margins
  namespace <- asNamespace("FielDHub")
  expect_identical(eval(formals(diagonal_dimension_choices)$minimum_extra, namespace),
                   eval(formals(field_dimensions)$minimum_extra, namespace))
  expect_identical(.diagonal_size_margins, c(minimum = 0.10, maximum = 0.20))
  expect_identical(field_size_range(270), c(floor(270 * 1.10), ceiling(270 * 1.20)))
  sizes <- vapply(strsplit(unlist(field_dimensions(270)), " x "),
                  function(d) prod(as.numeric(d)), numeric(1))
  expect_true(all(sizes >= field_size_range(270)[1] & sizes <= field_size_range(270)[2]))
})

test_that("diagonal_multiple app arguments reproduce the API design with per-experiment plot starts in two locations", {
  blocks <- c(100, 120, 80)
  # The module's block layout (checks first, then one BLOCK per experiment),
  # which only feeds the percentage-of-checks options
  layout <- data.frame(ENTRY = 1:304, BLOCK = c(rep("ALL", 4), rep(1:3, times = blocks)))
  percents <- offered_check_percents(19, 18, checks = 1:4, lines = 300, kindExpt = "DBUDC",
                                     data = layout, blocks = 3)
  values <- list(nrows = 19, ncols = 18, lines = 300, checks = 4, planter = "serpentine",
                 l = 2, plot_start = c(1, 1001, 2001), stacked = "By Row", seed = 3,
                 blocks = blocks, expt_name = c("A", "B", "C"),
                 location_names = c("FARGO", "MINOT"), checksPercent = percents[1],
                 sameEntries = FALSE)
  # One start per experiment: the same starts in every location
  parity(design_args_diagonal_multiple, diagonal_arrangement, values,
         direct = diagonal_arrangement(nrows = 19, ncols = 18, lines = 300, checks = 4, l = 2,
                                       plotNumber = list(c(1, 1001, 2001), c(1, 1001, 2001)),
                                       kindExpt = "DBUDC", splitBy = "row", seed = 3,
                                       blocks = blocks, exptName = c("A", "B", "C"),
                                       locationNames = c("FARGO", "MINOT"),
                                       checksPercent = percents[1]))
})

test_that("diagonal_multiple app arguments reproduce an uploaded-data design split by column", {
  blocks <- c(100, 120, 80)
  data <- data.frame(ENTRY = 1:304, NAME = c(paste0("CH", 1:4), paste0("SB-", 5:304)))
  values <- list(nrows = 19, ncols = 18, lines = NULL, checks = 4, planter = "cartesian",
                 l = 1, plot_start = 101, stacked = "By Column", seed = 11,
                 blocks = blocks, expt_name = "ONE", location_names = "CASSELTON",
                 checksPercent = NULL, sameEntries = FALSE)
  parity(design_args_diagonal_multiple, diagonal_arrangement, values, data,
         direct = diagonal_arrangement(nrows = 19, ncols = 18, checks = 4, planter = "cartesian",
                                       plotNumber = 101, kindExpt = "DBUDC", splitBy = "column",
                                       seed = 11, blocks = blocks, exptName = "ONE",
                                       locationNames = "CASSELTON", data = data))
})

test_that("design_args_diagonal_multiple() shares one start across locations, or falls back to the engine default (R9)", {
  blocks <- c(100, 120, 80)
  values <- list(nrows = 19, ncols = 18, lines = 300, checks = 4, l = 2, plot_start = 5,
                 stacked = "By Row", seed = 3, blocks = blocks, sameEntries = FALSE)
  # One start: every location starts there
  expect_identical(design_args_diagonal_multiple(shiny_shaped(values))$plotNumber, c(5, 5))
  # A start count that is neither one nor one per experiment: the engine's
  # own per-location default, default_plot_starts(l, 1001), silently
  misfit <- utils::modifyList(values, list(plot_start = c(1, 2), location_names = "ONLY"))
  built <- design_args_diagonal_multiple(shiny_shaped(misfit))
  expect_identical(built$plotNumber, default_plot_starts(2, 1001))
  expect_null(built$locationNames)
  found <- warnings_of_class(via_app <- do.call(diagonal_arrangement, built),
                             "fieldhub_default_warning")
  expect_length(found, 0L)
  direct <- diagonal_arrangement(nrows = 19, ncols = 18, lines = 300, checks = 4, l = 2,
                                 plotNumber = c(1001, 2001), kindExpt = "DBUDC", seed = 3,
                                 blocks = blocks)
  expect_identical(via_app$fieldBook, direct$fieldBook)
  expect_identical(via_app$metadata$parameters, direct$metadata$parameters)
})

test_that("design_args_diagonal_multiple() repeats entries across experiments on request", {
  blocks <- rep(40, 5)
  values <- list(nrows = 12, ncols = 18, lines = 200, checks = 4, planter = "serpentine",
                 l = 1, plot_start = 1, stacked = "By Row", seed = 21, blocks = blocks,
                 sameEntries = TRUE)
  expect_identical(design_args_diagonal_multiple(shiny_shaped(values))$splitBy, "row")
  parity(design_args_diagonal_multiple, diagonal_arrangement, values,
         direct = diagonal_arrangement(nrows = 12, ncols = 18, lines = 200, checks = 4,
                                       plotNumber = 1, kindExpt = "DBUDC", seed = 21,
                                       blocks = blocks, sameEntries = TRUE))
})

test_that("sparse_allocation app arguments reproduce the API allocation and design across three locations", {
  # Run!: the allocation, through its own builder
  optim_values <- list(lines = 135, l = 3, copies_per_entry = 2, checks = 4, seed = 5)
  via_app_allocation <- do.call(do_optim, design_args_sparse_allocation_optim(shiny_shaped(optim_values)))
  direct_allocation <- do_optim(design = "sparse", lines = 135, l = 3, copies_per_entry = 2,
                                add_checks = TRUE, checks = 4, seed = 5)
  expect_identical(via_app_allocation, direct_allocation)
  # Randomize!: the design of every location from that allocation
  lines_within_loc <- as.numeric(via_app_allocation$size_locations[1])
  dims <- as.numeric(strsplit(diagonal_dimension_choices(lines_within_loc, checks = 136:139)[1],
                              " x ")[[1]])
  percents <- offered_check_percents(dims[1], dims[2], checks = 136:139, lines = lines_within_loc)
  values <- c(optim_values, list(nrows = dims[1], ncols = dims[2], planter = "cartesian",
                                 plot_start = c(1, 1001, 2001), expt_name = "Sparse1",
                                 location_names = c("A", "B", "C"),
                                 sparse_list = via_app_allocation, checksPercent = percents[1]))
  parity(design_args_sparse_allocation, sparse_allocation, values,
         direct = sparse_allocation(lines = 135, nrows = dims[1], ncols = dims[2], l = 3,
                                    planter = "cartesian", plotNumber = c(1, 1001, 2001),
                                    copies_per_entry = 2, checks = 4, exptName = "Sparse1",
                                    locationNames = c("A", "B", "C"),
                                    sparse_list = direct_allocation, seed = 5,
                                    checksPercent = percents[1]))
})

test_that("sparse_allocation app arguments reproduce an uploaded-data allocation and design", {
  data <- data.frame(ENTRY = 1:139, NAME = c(paste0("CHECK", 1:4), paste0("SB-", 5:139)))
  optim_values <- list(lines = 135, l = 3, copies_per_entry = 2, checks = 4, seed = 8)
  via_app_allocation <- do.call(do_optim, design_args_sparse_allocation_optim(shiny_shaped(optim_values), data))
  direct_allocation <- do_optim(design = "sparse", lines = 135, l = 3, copies_per_entry = 2,
                                add_checks = TRUE, checks = 4, seed = 8, data = data)
  expect_identical(via_app_allocation, direct_allocation)
  values <- c(optim_values, list(nrows = 10, ncols = 10, planter = "serpentine",
                                 plot_start = c(101, 201, 301), expt_name = "E",
                                 location_names = c("X", "Y", "Z"),
                                 sparse_list = via_app_allocation, checksPercent = NULL))
  parity(design_args_sparse_allocation, sparse_allocation, values, data,
         direct = sparse_allocation(lines = 135, nrows = 10, ncols = 10, l = 3,
                                    plotNumber = c(101, 201, 301), copies_per_entry = 2,
                                    checks = 4, exptName = "E", locationNames = c("X", "Y", "Z"),
                                    sparse_list = direct_allocation, seed = 8, data = data))
})

test_that("design_args_sparse_allocation() leaves misfit plot starts, names and experiment names to the engine (R9)", {
  allocation <- do_optim(design = "sparse", lines = 135, l = 3, copies_per_entry = 2,
                         add_checks = TRUE, checks = 4, seed = 5)
  values <- list(lines = 135, l = 3, copies_per_entry = 2, checks = 4, seed = 5,
                 nrows = 10, ncols = 10, planter = "serpentine", plot_start = 1001,
                 expt_name = character(), location_names = "ONLY", sparse_list = allocation)
  built <- design_args_sparse_allocation(shiny_shaped(values))
  # The engine's own base: sparse_allocation() starts the default at 1, the
  # app's inline formula used to start it at 1001
  expect_identical(built$plotNumber, default_plot_starts(3, 1))
  expect_false(any(c("locationNames", "exptName") %in% names(built)))
  found <- warnings_of_class(via_app <- do.call(sparse_allocation, built),
                             "fieldhub_default_warning")
  expect_length(found, 0L)
  direct <- sparse_allocation(lines = 135, nrows = 10, ncols = 10, l = 3, plotNumber = c(1, 1001, 2001),
                              copies_per_entry = 2, checks = 4, sparse_list = allocation, seed = 5)
  expect_identical(via_app$fieldBook, direct$fieldBook)
  expect_identical(via_app$metadata$parameters, direct$metadata$parameters)
  expect_identical(unique(via_app$fieldBook$LOCATION), paste0("LOC", 1:3))
  expect_true(all(via_app$fieldBook$EXPT %in% c("SparseExpt", "Filler")))
})

test_that("design_args_sparse_allocation() without an allocation or dimensions lets sparse_allocation() compute them", {
  values <- list(lines = 135, l = 3, copies_per_entry = 2, checks = 4, seed = 5,
                 plot_start = c(1, 1001, 2001))
  built <- design_args_sparse_allocation(shiny_shaped(values))
  expect_false(any(c("sparse_list", "nrows", "ncols") %in% names(built)))
  parity(design_args_sparse_allocation, sparse_allocation, values,
         direct = sparse_allocation(lines = 135, l = 3, plotNumber = c(1, 1001, 2001),
                                    copies_per_entry = 2, checks = 4, seed = 5))
})

test_that("sparse_allocation() keeps its automatic field size, now read from field_dimensions()", {
  # The automatic choice (no nrows/ncols) is unchanged: the squarest size
  # 11% to 20% larger than a location. Its candidates now come from
  # field_dimensions(n, minimum_extra = 0.11), identical to the former
  # inline search for every size.
  former_candidates <- function(n) {
    t <- floor(n + n * 0.11):ceiling(n + n * 0.20)
    unlist(lapply(t[!is_prime(t)], function(size) factor_subsets(size, diagonal = TRUE)$labels))
  }
  for (n in c(40, 46, 90, 100, 110, 287, 1000)) {
    expect_identical(unlist(field_dimensions(n, minimum_extra = 0.11)), former_candidates(n),
                     info = n)
  }
  # 100 entries per location: still 10 x 12 (the catalogue's 90 gives 10 x 10)
  design <- sparse_allocation(lines = 150, l = 3, copies_per_entry = 2, checks = 4, seed = 2)
  expect_identical(as.numeric(design$size_locations[1]), 100)
  expect_identical(c(design$infoDesign$rows, design$infoDesign$columns), c(10, 12))
})

test_that("the sparse module's explicit dimensions reproduce the API design", {
  # The module offers diagonal_dimension_choices() (field_dimensions()
  # sizes the checks fit, squarest first) and preselects the first; it
  # always passes the chosen nrows/ncols, so the app's design is the API
  # design with those dimensions, whatever sparse_allocation() would
  # choose on its own (10 x 12 here).
  optim_values <- list(lines = 150, l = 3, copies_per_entry = 2, checks = 4, seed = 2)
  allocation <- do.call(do_optim, design_args_sparse_allocation_optim(shiny_shaped(optim_values)))
  preselected <- diagonal_dimension_choices(as.numeric(allocation$size_locations[1]),
                                            checks = 151:154)[1]
  expect_identical(preselected, "10 x 11")
  values <- c(optim_values, list(nrows = 10, ncols = 11, plot_start = c(1, 1001, 2001),
                                 sparse_list = allocation))
  parity(design_args_sparse_allocation, sparse_allocation, values,
         direct = sparse_allocation(lines = 150, nrows = 10, ncols = 11, l = 3,
                                    plotNumber = c(1, 1001, 2001), copies_per_entry = 2,
                                    checks = 4, sparse_list = allocation, seed = 2))
})

test_that("Optim app arguments reproduce the API design from counts across two locations", {
  values <- list(nrows = 12, ncols = 10, lines = 100, checks = 4, rep_checks = c(5, 5, 5, 5),
                 planter = "serpentine", l = 2, plot_start = c(1, 1001), seed = 5,
                 expt_name = "Expt1", location_names = c("A", "B"))
  via_app <- do.call(optimized_arrangement, design_args_Optim(shiny_shaped(values)))
  direct <- optimized_arrangement(nrows = 12, ncols = 10, lines = 100, checks = 4,
                                  rep_checks = c(5, 5, 5, 5), l = 2, plotNumber = c(1, 1001),
                                  seed = 5, exptName = "Expt1", locationNames = c("A", "B"))
  expect_identical(via_app$fieldBook, direct$fieldBook)
  expect_identical(via_app$metadata$parameters, direct$metadata$parameters)
  # Passing counts instead of the module's former CH/G entry table changes
  # the recorded inputs, not the field book: the engine generates the same
  # CH1..CH4, G5..G104 list
  former <- data.frame(ENTRY = 1:104, NAME = c(paste0("CH", 1:4), paste0("G", 5:104)),
                       REPS = c(5, 5, 5, 5, rep(1, 100)))
  former_design <- optimized_arrangement(nrows = 12, ncols = 10, l = 2, plotNumber = c(1, 1001),
                                         seed = 5, exptName = "Expt1",
                                         locationNames = c("A", "B"), data = former)
  expect_identical(via_app$fieldBook, former_design$fieldBook)
  expect_null(via_app$metadata$parameters$data)
})

test_that("Optim app arguments reproduce an uploaded-data design", {
  data <- data.frame(ENTRY = 1:104, NAME = c(paste0("CHECK", 1:4), paste0("SB-", 5:104)),
                     REPS = c(4, 4, 6, 6, rep(1, 100)))
  values <- list(nrows = 12, ncols = 10, planter = "cartesian", l = 1, plot_start = 101,
                 seed = 17, expt_name = "Trial", location_names = "FARGO")
  parity(design_args_Optim, optimized_arrangement, values, data,
         direct = optimized_arrangement(nrows = 12, ncols = 10, planter = "cartesian",
                                        plotNumber = 101, seed = 17, exptName = "Trial",
                                        locationNames = "FARGO", data = data))
  # Generated-path counts left over in `values` never reach the engine
  built <- design_args_Optim(c(values, list(lines = 100, checks = 4, rep_checks = 1:4)), data)
  expect_null(built$lines)
  expect_null(built$checks)
  expect_null(built$rep_checks)
})

test_that("design_args_Optim() falls back to the l/planter/plot-start defaults", {
  values <- list(nrows = 12, ncols = 10, lines = 100, checks = 4, rep_checks = c(5, 5, 5, 5),
                 seed = 6)
  parity(design_args_Optim, optimized_arrangement, values,
         direct = optimized_arrangement(nrows = 12, ncols = 10, lines = 100, checks = 4,
                                        rep_checks = c(5, 5, 5, 5), seed = 6))
})

test_that("pREPS app arguments reproduce the API design from repGens/repUnits across two locations", {
  values <- list(nrows = 7, ncols = 15, repGens = c(75, 15), repUnits = c(1, 2),
                 planter = "serpentine", l = 2, plot_start = c(1, 1001), seed = 4095,
                 expt_name = "Expt1", location_names = c("A", "B"), allow_fillers = FALSE)
  via_app <- do.call(partially_replicated, design_args_pREPS(shiny_shaped(values)))
  direct <- partially_replicated(nrows = 7, ncols = 15, repGens = c(75, 15), repUnits = c(1, 2),
                                 l = 2, plotNumber = c(1, 1001), seed = 4095, exptName = "Expt1",
                                 locationNames = c("A", "B"))
  expect_identical(via_app$fieldBook, direct$fieldBook)
  expect_identical(via_app$metadata$parameters, direct$metadata$parameters)
  # The module used to build its own G1..G105 table and hard-code the
  # optimizer settings; both equal what the engine does by default, so the
  # field book is unchanged and only the recorded inputs differ
  expect_identical(formals(partially_replicated)$border_penalization, 0.5)
  expect_identical(formals(partially_replicated)$dist_method, "euclidean")
  former <- data.frame(ENTRY = 1:90, NAME = paste0("G", 1:90), REPS = rep(c(1, 2), times = c(75, 15)))
  former_design <- partially_replicated(nrows = rep(7, 2), ncols = rep(15, 2), l = 2, seed = 4095,
                                        plotNumber = c(1, 1001), exptName = "Expt1",
                                        locationNames = c("A", "B"), border_penalization = 0.5,
                                        dist_method = "euclidean", data = former)
  expect_identical(via_app$fieldBook, former_design$fieldBook)
  expect_null(via_app$metadata$parameters$data)
})

test_that("pREPS app arguments reproduce an uploaded-data design with filler plots", {
  data <- data.frame(ENTRY = 1:57, NAME = paste0("SB-", 1:57), REPS = rep(c(1, 2), times = c(50, 7)))
  values <- list(nrows = 9, ncols = 8, planter = "cartesian", l = 1, plot_start = 101,
                 seed = 33, expt_name = "PREP", location_names = "CASSELTON",
                 allow_fillers = TRUE)
  parity(design_args_pREPS, partially_replicated, values, data,
         direct = partially_replicated(nrows = 9, ncols = 8, planter = "cartesian",
                                       plotNumber = 101, seed = 33, exptName = "PREP",
                                       locationNames = "CASSELTON", data = data,
                                       allow_fillers = TRUE))
  built <- design_args_pREPS(c(values, list(repGens = 50, repUnits = 1)), data)
  expect_null(built$repGens)
  expect_null(built$repUnits)
})

test_that("design_args_pREPS() falls back to the l/planter/plot-start/allow_fillers defaults", {
  values <- list(nrows = 8, ncols = 8, repGens = c(50, 7), repUnits = c(1, 2), seed = 32)
  parity(design_args_pREPS, partially_replicated, values,
         direct = partially_replicated(nrows = 8, ncols = 8, repGens = c(50, 7),
                                       repUnits = c(1, 2), seed = 32))
})

test_that("multi_loc_preps app arguments reproduce the API allocation and design with checks", {
  optim_values <- list(lines = 40, l = 2, copies_per_entry = 3, checks = 2,
                       rep_checks = c(2, 2), seed = 7)
  via_app_allocation <- do.call(do_optim, design_args_multi_loc_preps_optim(shiny_shaped(optim_values)))
  direct_allocation <- do_optim(design = "prep", lines = 40, l = 2, copies_per_entry = 3,
                                add_checks = TRUE, checks = 2, rep_checks = c(2, 2), seed = 7)
  expect_identical(via_app_allocation, direct_allocation)
  # The module sends one field size per location
  values <- c(optim_values, list(nrows = c(8, 8), ncols = c(8, 8), planter = "cartesian",
                                 plot_start = c(1, 1001), expt_name = "MET",
                                 location_names = c("FARGO", "MINOT"),
                                 optim_list = via_app_allocation, allow_fillers = FALSE))
  parity(design_args_multi_loc_preps, multi_location_prep, values,
         direct = multi_location_prep(lines = 40, nrows = c(8, 8), ncols = c(8, 8), l = 2,
                                      planter = "cartesian", plotNumber = c(1, 1001),
                                      copies_per_entry = 3, checks = 2, rep_checks = c(2, 2),
                                      exptName = "MET", locationNames = c("FARGO", "MINOT"),
                                      optim_list = direct_allocation, seed = 7))
})

test_that("multi_loc_preps app arguments let multi_location_prep() merge an uploaded list", {
  data <- data.frame(ENTRY = 101:140, NAME = paste0("SB-", 101:140))
  optim_values <- list(lines = 40, l = 2, copies_per_entry = 3, seed = 12)
  via_app_allocation <- do.call(do_optim, design_args_multi_loc_preps_optim(shiny_shaped(optim_values), data))
  direct_allocation <- do_optim(design = "prep", lines = 40, l = 2, copies_per_entry = 3,
                                add_checks = FALSE, seed = 12, data = data)
  expect_identical(via_app_allocation, direct_allocation)
  # Without checks the 60 plots of each location need filler plots
  values <- c(optim_values, list(nrows = c(8, 8), ncols = c(8, 8), plot_start = c(1, 1001),
                                 expt_name = "MET", location_names = c("A", "B"),
                                 optim_list = via_app_allocation, allow_fillers = TRUE))
  via_app <- do.call(multi_location_prep, design_args_multi_loc_preps(shiny_shaped(values), data))
  direct <- multi_location_prep(lines = 40, nrows = c(8, 8), ncols = c(8, 8), l = 2,
                                plotNumber = c(1, 1001), copies_per_entry = 3, exptName = "MET",
                                locationNames = c("A", "B"), optim_list = direct_allocation,
                                seed = 12, data = data, allow_fillers = TRUE)
  expect_identical(via_app$fieldBook, direct$fieldBook)
  expect_identical(via_app$metadata$parameters, direct$metadata$parameters)
  # The engine merges the uploaded names, as the module's own
  # merge_user_data() call used to do before handing it the merged list
  expect_true(all(via_app$fieldBook$TREATMENT[via_app$fieldBook$ENTRY > 0] %in% data$NAME))
  merged <- merge_user_data(direct_allocation, data = data, lines = 40)
  former <- multi_location_prep(lines = 40, nrows = c(8, 8), ncols = c(8, 8), l = 2,
                                plotNumber = c(1, 1001), copies_per_entry = 3, exptName = "MET",
                                locationNames = c("A", "B"), optim_list = merged, seed = 12,
                                allow_fillers = TRUE)
  expect_identical(via_app$fieldBook, former$fieldBook)
})

test_that("design_args_multi_loc_preps() leaves misfit plot starts, names and experiment names to the engine (R9)", {
  allocation <- do_optim(design = "prep", lines = 40, l = 2, copies_per_entry = 3,
                         add_checks = TRUE, checks = 2, rep_checks = c(2, 2), seed = 7)
  values <- list(lines = 40, l = 2, copies_per_entry = 3, checks = 2, rep_checks = c(2, 2),
                 seed = 7, nrows = c(8, 8), ncols = c(8, 8), plot_start = 1001,
                 expt_name = character(), location_names = "ONLY", optim_list = allocation)
  built <- design_args_multi_loc_preps(shiny_shaped(values))
  # The engine's own base, where the module's inline formulas used 1
  # in one place and 1, 1001, ... in another
  expect_identical(built$plotNumber, default_plot_starts(2, 1))
  expect_false(any(c("locationNames", "exptName") %in% names(built)))
  found <- warnings_of_class(via_app <- do.call(multi_location_prep, built),
                             "fieldhub_default_warning")
  expect_length(found, 0L)
  direct <- multi_location_prep(lines = 40, nrows = c(8, 8), ncols = c(8, 8), l = 2,
                                plotNumber = c(1, 1001), copies_per_entry = 3, checks = 2,
                                rep_checks = c(2, 2), optim_list = allocation, seed = 7)
  expect_identical(via_app$fieldBook, direct$fieldBook)
  expect_identical(via_app$metadata$parameters, direct$metadata$parameters)
  expect_identical(unique(via_app$fieldBook$EXPT), "PrepExpt")
})

test_that("allocation_entry_names() reads the names do_optim() gave the entries, in entry order", {
  allocation <- do_optim(design = "prep", lines = 40, l = 2, copies_per_entry = 3,
                         add_checks = TRUE, checks = 2, rep_checks = c(2, 2), seed = 7)
  expect_identical(allocation_entry_names(allocation, 40), paste0("G-", 1:40))
  sparse <- do_optim(design = "sparse", lines = 135, l = 3, copies_per_entry = 2,
                     add_checks = TRUE, checks = 4, seed = 5)
  expect_identical(allocation_entry_names(sparse, 135), paste0("G-", 1:135))
  # The allocation table has one row per entry, in the same order
  expect_identical(rownames(sparse$allocation), as.character(1:135))
})

#' The first field size the RCBD_augmented module offers for `b` blocks,
#' as the module reads it from set_augmented_blocks()
#' @noRd
offered_augmented_dims <- function(lines, checks, b) {
  options <- set_augmented_blocks(lines = lines, checks = checks, start = 3)$blocks_dims
  as.numeric(strsplit(options[options[, 1] == b, 2][1], " x ")[[1]])
}

test_that("RCBD_augmented app arguments reproduce the API design from counts across two locations", {
  dims <- offered_augmented_dims(60, 4, b = 5)
  values <- list(lines = 60, checks = 4, b = 5, l = 2, planter = "serpentine",
                 plot_start = c(1, 1001), expt_name = "Expt1", seed = 3,
                 location_names = c("fargo", "minot"), repsExpt = 1, random = TRUE,
                 repsStack = NULL, nrows = dims[1], ncols = dims[2])
  via_app <- do.call(RCBD_augmented, design_args_RCBD_augmented(shiny_shaped(values)))
  direct <- RCBD_augmented(lines = 60, checks = 4, b = 5, l = 2, plotNumber = c(1, 1001),
                           exptName = "Expt1", seed = 3, locationNames = c("fargo", "minot"),
                           nrows = dims[1], ncols = dims[2])
  expect_identical(via_app$fieldBook, direct$fieldBook)
  expect_identical(via_app$metadata$parameters, direct$metadata$parameters)
  # Passing counts instead of the module's former CH/G entry table leaves
  # the field book unchanged: the engine generates the same list (and, on
  # this path, records that generated list itself, as a direct call does)
  former <- data.frame(ENTRY = 1:64, NAME = c(paste0("CH", 1:4), paste0("G", 5:64)))
  former_design <- RCBD_augmented(lines = 60, checks = 4, b = 5, l = 2, plotNumber = c(1, 1001),
                                  exptName = "Expt1", seed = 3,
                                  locationNames = c("fargo", "minot"), repsStack = NULL,
                                  data = former, nrows = dims[1], ncols = dims[2])
  expect_identical(via_app$fieldBook, former_design$fieldBook)
})

test_that("RCBD_augmented app arguments reproduce an uploaded-data design with two stacked experiments", {
  data <- data.frame(ENTRY = 1:43, NAME = c(paste0("CHECK", 1:3), paste0("SB-", 4:43)))
  dims <- offered_augmented_dims(40, 3, b = 4)
  # On the upload path the module counts the lines of the file
  values <- list(lines = nrow(data) - 3, checks = 3, b = 4, l = 1, planter = "cartesian",
                 plot_start = c(101, 201), expt_name = c("E1", "E2"), seed = 29,
                 location_names = "CASSELTON", repsExpt = 2, random = FALSE,
                 repsStack = "horizontal", nrows = dims[1], ncols = dims[2])
  parity(design_args_RCBD_augmented, RCBD_augmented, values, data,
         direct = RCBD_augmented(lines = 40, checks = 3, b = 4, planter = "cartesian",
                                 plotNumber = c(101, 201), repsStack = "horizontal",
                                 exptName = c("E1", "E2"), seed = 29,
                                 locationNames = "CASSELTON", repsExpt = 2, random = FALSE,
                                 data = data, nrows = dims[1], ncols = dims[2]))
})

test_that("design_args_RCBD_augmented() falls back to the l/planter/plot-start/repsExpt/random defaults", {
  values <- list(lines = 50, checks = 3, b = 5, seed = 29)
  parity(design_args_RCBD_augmented, RCBD_augmented, values,
         direct = RCBD_augmented(lines = 50, checks = 3, b = 5, seed = 29))
})

test_that("an integer from Shiny and a typed double record the same design through a builder", {
  # The engines record an argument as given: 270L and 270 build the same
  # field book but different metadata$parameters...
  typed <- diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4,
                                plotNumber = 101, seed = 24)
  from_integer <- diagonal_arrangement(nrows = 15, ncols = 20, lines = 270L, checks = 4,
                                       plotNumber = 101, seed = 24)
  expect_identical(from_integer$fieldBook, typed$fieldBook)
  expect_false(identical(from_integer$metadata$parameters, typed$metadata$parameters))
  # ... so the builders send doubles, whichever Shiny delivered
  values <- list(nrows = 15, ncols = 20, lines = 270, checks = 4, plot_start = 101, seed = 24)
  from_shiny <- shiny_shaped(values)
  expect_type(from_shiny$lines, "integer")
  via_integer <- do.call(diagonal_arrangement, design_args_Diagonal(from_shiny))
  via_double <- do.call(diagonal_arrangement, design_args_Diagonal(values))
  expect_identical(via_integer$metadata$parameters, via_double$metadata$parameters)
  expect_identical(via_integer$metadata$parameters, typed$metadata$parameters)
  # The other two reported paths: the augmented RCBD number of experiments
  # (numericInput, default 1L) and do_optim()'s lines (an nrow() count)
  arcbd <- list(lines = 40L, checks = 3L, b = 4L, repsExpt = 2L, repsStack = "vertical", seed = 5L)
  expect_identical(
    do.call(RCBD_augmented, design_args_RCBD_augmented(arcbd))$metadata$parameters,
    RCBD_augmented(lines = 40, checks = 3, b = 4, repsExpt = 2, repsStack = "vertical",
                   seed = 5)$metadata$parameters)
  sparse <- list(lines = 135L, l = 3L, copies_per_entry = 2L, checks = 4L, seed = 5L)
  expect_identical(
    do.call(do_optim, design_args_sparse_allocation_optim(sparse))$metadata$parameters,
    do_optim(design = "sparse", lines = 135, l = 3, copies_per_entry = 2, add_checks = TRUE,
             checks = 4, seed = 5)$metadata$parameters)
})

test_that("as_design_number() turns integer storage into double and leaves everything else", {
  expect_null(as_design_number(NULL))
  expect_identical(as_design_number(270L), 270)
  expect_identical(as_design_number(c(a = 1L, b = 2L)), c(a = 1, b = 2))
  expect_identical(as_design_number(matrix(1:4, 2)), matrix(c(1, 2, 3, 4), 2))
  for (unchanged in list(9.6, c(101, 1001), "4", c("A", "B"), TRUE, NA,
                         factor(c("x", "y")), list(1L), data.frame(ENTRY = 1:2))) {
    expect_identical(as_design_number(unchanged), unchanged)
  }
})

test_that("spatial builders leave a missing or invalid l to the engine's classed error", {
  allocation <- do_optim(design = "sparse", lines = 135, l = 3, copies_per_entry = 2,
                         add_checks = TRUE, checks = 4, seed = 5)
  base <- list(lines = 135, copies_per_entry = 2, checks = 4, seed = 5, nrows = 10, ncols = 10,
               plot_start = c(1, 1001, 2001), location_names = c("A", "B", "C"),
               sparse_list = allocation)
  prep <- list(lines = 40, copies_per_entry = 3, seed = 7, nrows = 8, ncols = 8,
               plot_start = c(1, 1001), location_names = c("A", "B"), allow_fillers = TRUE)
  for (l in list(NULL, "3", 0, 2.5, c(2, 3))) {
    with_l <- function(values) if (is.null(l)) values else c(values, list(l = l))
    expect_error(do.call(sparse_allocation, design_args_sparse_allocation(with_l(base))),
                 class = "fieldhub_input_error")
    expect_error(do.call(do_optim, design_args_sparse_allocation_optim(with_l(base))),
                 class = "fieldhub_input_error")
    expect_error(do.call(multi_location_prep, design_args_multi_loc_preps(with_l(prep))),
                 class = "fieldhub_input_error")
    expect_error(do.call(do_optim, design_args_multi_loc_preps_optim(with_l(prep))),
                 class = "fieldhub_input_error")
    if (!is.null(l)) {
      diagonal <- list(nrows = 15, ncols = 20, lines = 270, checks = 4, seed = 1, l = l,
                       plot_start = c(1, 1001), location_names = c("A", "B"),
                       blocks = c(100, 170))
      expect_error(do.call(diagonal_arrangement, design_args_Diagonal(diagonal)),
                   class = "fieldhub_input_error")
      expect_error(do.call(diagonal_arrangement, design_args_diagonal_multiple(diagonal)),
                   class = "fieldhub_input_error")
    }
  }
})

# --- Spatial structural check (ruling R2): namespace bodies, call heads ---

test_that("spatial modules build their designs only through design_args_<Module>() and do.call()", {
  # Every engine call each spatial module makes, keyed by the builder that
  # must supply its arguments.
  spatial_engines <- list(
    Diagonal = c(design_args_Diagonal = "diagonal_arrangement"),
    diagonal_multiple = c(design_args_diagonal_multiple = "diagonal_arrangement"),
    sparse_allocation = c(design_args_sparse_allocation_optim = "do_optim",
                          design_args_sparse_allocation = "sparse_allocation"),
    Optim = c(design_args_Optim = "optimized_arrangement"),
    pREPS = c(design_args_pREPS = "partially_replicated"),
    multi_loc_preps = c(design_args_multi_loc_preps_optim = "do_optim",
                        design_args_multi_loc_preps = "multi_location_prep"),
    RCBD_augmented = c(design_args_RCBD_augmented = "RCBD_augmented")
  )
  # Unexported helpers a spatial module server may call, and why. Anything
  # else it calls must be one of its engines/builders, or a base/shiny/DT/
  # plotly/shinyjs function.
  allowed_helpers <- c(
    "validate_design",            # shows any error where the output would be (R/app_conditions.R)
    "app_report_problem",         # reports a problem of an event with no output slot (dialog or notice)
    "app_attempt",                # evaluates event work, reporting its errors/warnings via app_report_problem()
    "parse_rep_groups",           # parses the p-rep "entries per group"/"reps per group" inputs
    "prep_no_dimensions_problem", # explains why a p-rep design has no field dimensions to offer
    "fieldhub_abort",             # writes a problem an output shows through validate_design()
    "app_plot_state",             # explains an empty layout panel (not randomized yet, or failed)
    "app_design_state",           # reads a design that has not run as NULL for app_plot_state()
    "app_design_seed",            # resolves the optional app seed without touching the shared RNG stream
    "read_app_seed",              # re-reads the resolved design seed for the simulation workflow; never draws
    "app_read_upload",            # reads/validates the module's upload, reporting a failure itself (R/app_upload.R)
    "app_upload_dialog_observer", # opens the shared entries-format dialog when the upload toggle switches to "Yes"
    "parse_whole_numbers",        # parses the comma-separated starting-plot-number input
    "app_spatial_workflow",       # shared results/simulation/export lifecycle for spatial modules
    "spatial_workflow_spec",      # registry of per-design IDs/labels the shared workflow uses
    "app_table_export_buttons",   # DT export buttons carrying the design's metadata
    "field_dimensions",           # candidate field sizes: rejects too few entries before any randomization
    "diagonal_dimension_choices", # feasible diagonal field dimensions offered before randomizing (seed-isolated)
    "diagonal_check_options",     # percentages of checks offered for a field before randomizing (seed-isolated)
    "optimized_dimension_choices",# field dimensions offered for an optimized arrangement (no RNG)
    "prep_dimension_options",     # field dimensions (and filler counts) offered for a p-rep design (no RNG)
    "set_augmented_blocks",       # block counts and field dimensions offered for an augmented RCBD (no RNG)
    "checked_layout_view",        # draws the augmented RCBD field layout of one location
    "field_book_location_grids",  # splits a field book into per-location EXPT grids for display
    "allocation_entry_names",     # reads the entry names of a do_optim() allocation for its table
    "validate_locations_input",   # validates a raw "# of Locations" input before using the count
    "location_view_choices",      # choices for a "view location" select input (Task 13)
    "plant_rep_choices",          # choices for the sparse allocation "plant reps" select input (Task 13)
    "parse_n_checks"              # validates a raw "# of Checks" input before using the count (Task 13)
  )
  forbidden <- c("sample", "sample.int", "set.seed", "runif", "available_percent",
                 "random_checks", "merge_user_data", "pREP", "get_random",
                 "get_random_stacked", "get_single_random", "swap_pairs",
                 "default_entries")

  namespace <- asNamespace("FielDHub")
  all_objects <- mget(ls(namespace, all.names = TRUE), namespace, inherits = FALSE)
  package_functions <- names(Filter(is.function, all_objects))
  modules <- app_functions()
  registry <- fieldhub_app_registry()
  generic <- vapply(registry, function(entry) !is.null(entry$spec), logical(1))
  generic <- vapply(registry[generic], `[[`, character(1), "workflow")

  # The generic spatial page reaches each engine only through its spec:
  # do.call(spec$engine, spec$args(...)), and do.call(spec$optim$engine,
  # spec$optim$args(...)) for the allocation of Run!
  code <- body(app_spatial_page)
  text <- gsub("[[:space:]]+", "", paste(deparse(code), collapse = ""))
  expect_identical(sum(all.names(code) == "do.call"), 2L)
  expect_true(grepl("do.call(spec$engine,spec$args(", text, fixed = TRUE))
  expect_true(grepl("do.call(spec$optim$engine,spec$optim$args(", text, fixed = TRUE))
  heads <- fieldhub_call_heads(code)
  expect_identical(intersect(heads, c(forbidden, unlist(spatial_engines),
                                      unlist(lapply(spatial_engines, names)))), character(0))
  page_helpers <- c(
    "validate_design", "app_design_state", "app_plot_state", "app_upload_spec",
    "read_design_controls",       # reads again the controls the steps follow (filler plots)
    "design_step_choices",        # choices of a step, from the values of the run
    "read_design_steps",          # reads the steps into the values of the builder
    "location_view_choices",      # the locations of the run
    "app_spatial_workflow", "app_spatial_table", "app_spatial_grid"
  )
  expect_identical(setdiff(intersect(heads, package_functions), page_helpers), character(0))
  # What the specs run: plain parsers, checks, choice helpers and views
  spec_helpers <- c(
    "shape_design_upload", "check_reps_upload", "optim_total_plots", "optim_field_choices",
    "field_grid_view", "spatial_highlight_colours", "entry_list_view", "location_view_choices",
    "parse_control_number", "parse_control_whole_numbers", "parse_control_names",
    "parse_control_choice", "parse_control_option", "parse_control_flag", "parse_control_seed",
    "parse_control_rep_checks", "parse_control_checks", "parse_control_dimensions"
  )

  for (module in names(spatial_engines)) {
    engines <- spatial_engines[[module]]
    if (module %in% generic) {
      spec <- design_app_spec(module)
      builder <- paste0("design_args_", module)
      expect_identical(spec$engine, get(engines[[builder]], namespace), info = module)
      expect_identical(spec$args, get(builder, namespace), info = module)
      optim <- setdiff(names(engines), builder)
      if (length(optim) == 1L) {
        expect_identical(spec$optim$engine, get(engines[[optim]], namespace), info = module)
        expect_identical(spec$optim$args, get(optim, namespace), info = module)
      } else {
        expect_null(spec$optim, info = module)
      }
      runtime <- spec_runtime_functions(spec)
      called <- runtime$named
      for (f in runtime$closures) called <- c(called, intersect(fieldhub_call_heads(body(f)), package_functions))
      expect_identical(intersect(called, c(forbidden, unname(engines), names(engines))), character(0),
                       info = module)
      expect_identical(setdiff(called, spec_helpers), character(0), info = module)
      next
    }
    server_name <- paste0("mod_", module, "_server")
    f <- modules[[server_name]]
    expect_false(is.null(f), info = server_name)
    code <- body(f)
    engines <- spatial_engines[[module]]
    heads <- fieldhub_call_heads(code)

    # Each engine is reached exactly once, as do.call(<engine>, <its builder>(...))
    pairs <- fieldhub_do_call_pairs(code)
    engine_pairs <- Filter(function(p) p[["fun"]] %in% package_functions, pairs)
    expect_setequal(vapply(engine_pairs, `[[`, character(1), "builder"), names(engines))
    expect_identical(length(engine_pairs), length(engines), info = module)
    for (p in engine_pairs) {
      expect_identical(unname(engines[p[["builder"]]]), p[["fun"]], info = module)
    }
    # ... and never called directly, nor referred to anywhere else
    for (engine in unique(engines)) {
      expect_false(engine %in% heads, info = paste(module, engine))
      expect_identical(sum(fieldhub_symbol_refs(code) == engine), sum(engines == engine),
                       info = paste(module, engine))
    }
    for (builder in names(engines)) {
      expect_identical(sum(heads == builder), 1L, info = paste(module, builder))
    }
    expect_identical(intersect(heads, forbidden), character(0), info = module)
    expect_false("blocksdesign" %in% all.names(code), info = module)

    called <- intersect(c(heads, vapply(pairs, `[[`, character(1), "fun")), package_functions)
    allowed <- c(allowed_helpers, unname(engines), names(engines))
    expect_identical(setdiff(called, allowed), character(0), info = module)
  }
})

# --- Direct tests for the helpers the spatial modules keep calling ---

test_that("set_augmented_blocks() lists block counts with fields that hold every block", {
  for (case in list(c(lines = 60, checks = 4), c(lines = 40, checks = 3), c(lines = 8, checks = 2))) {
    lines <- case[["lines"]]; checks <- case[["checks"]]
    set.seed(71)
    before <- .Random.seed
    options <- set_augmented_blocks(lines = lines, checks = checks, start = 3)
    expect_identical(.Random.seed, before)
    expect_identical(options, set_augmented_blocks(lines = lines, checks = checks, start = 3))
    expect_named(options, c("b", "option_dims", "blocks_dims"))
    # One row per option: the block count and the field it fits in
    expect_identical(nrow(options$blocks_dims), length(options$b))
    expect_identical(as.numeric(options$blocks_dims[, 1]), as.numeric(options$b))
    expect_identical(options$blocks_dims[, 2], unlist(options$option_dims))
    # Counts searched from `start` up to a third (more than 40 lines) or a
    # half of the lines
    divisor <- if (lines > 40) 3 else 2
    expect_true(all(options$b >= 3 & options$b <= ceiling(lines / divisor)))
    dims <- do.call(rbind, strsplit(options$blocks_dims[, 2], " x ", fixed = TRUE))
    rows <- as.numeric(dims[, 1]); cols <- as.numeric(dims[, 2])
    # No one-row or one-column fields, and each field holds b equal blocks
    expect_true(all(rows > 1 & cols > 1))
    plots_per_block <- ceiling((lines + checks * options$b) / options$b)
    expect_identical(rows * cols, plots_per_block * options$b)
  }
  # 8 lines and 2 checks in 3 blocks: 14 plots, 5 per block, a 3 x 5 field
  expect_identical(set_augmented_blocks(lines = 8, checks = 2, start = 3)$blocks_dims[1, ],
                   c("3", "3 x 5"))
})

test_that("RCBD_augmented() accepts the fields set_augmented_blocks() offers", {
  options <- set_augmented_blocks(lines = 60, checks = 4, start = 3)$blocks_dims
  for (b in unique(options[, 1])) {
    dims <- as.numeric(strsplit(options[options[, 1] == b, 2][1], " x ")[[1]])
    design <- RCBD_augmented(lines = 60, checks = 4, b = as.numeric(b), seed = 1,
                             nrows = dims[1], ncols = dims[2])
    expect_identical(c(design$infoDesign$rows, design$infoDesign$columns), dims, info = b)
  }
})

test_that("merge_user_data() puts the uploaded entries of a sparse allocation in its locations", {
  allocation <- do_optim(design = "sparse", lines = 20, l = 3, copies_per_entry = 2,
                         add_checks = TRUE, checks = 2, seed = 1)
  data <- data.frame(ENTRY = c(901, 902, 101:120), NAME = c("C1", "C2", paste0("L", 1:20)))
  merged <- merge_user_data(allocation, data, lines = 20, add_checks = TRUE, checks = 2)
  expect_identical(class(merged), class(allocation))
  expect_identical(merged$allocation, allocation$allocation)
  expect_identical(merged$size_locations, allocation$size_locations)
  expect_identical(names(merged$list_locs), paste0("LOC", 1:3))
  # Generated entry e is the uploaded row holding it: checks (entries 21,
  # 22) are the first rows, line i is row 2 + i
  row_of <- function(entry) ifelse(entry > 20, entry - 20, entry + 2)
  for (loc in 1:3) {
    generated <- allocation$list_locs[[loc]]$ENTRY
    expect_setequal(merged$list_locs[[loc]]$ENTRY, data$ENTRY[row_of(generated)])
    expect_setequal(merged$list_locs[[loc]]$NAME, data$NAME[row_of(generated)])
    expect_named(merged$list_locs[[loc]], c("ENTRY", "NAME"))
  }
})

test_that("merge_user_data() keeps the replication of a p-rep allocation", {
  allocation <- do_optim(design = "prep", lines = 40, l = 2, copies_per_entry = 3,
                         add_checks = TRUE, checks = 2, rep_checks = c(2, 2), seed = 7)
  data <- data.frame(ENTRY = c(501, 502, 1:40), NAME = c("CHK-A", "CHK-B", paste0("SB-", 1:40)))
  merged <- merge_user_data(allocation, data, lines = 40, add_checks = TRUE, checks = 2,
                            rep_checks = c(2, 2))
  row_of <- function(entry) ifelse(entry > 40, entry - 40, entry + 2)
  for (loc in 1:2) {
    generated <- allocation$list_locs[[loc]]
    merged_loc <- merged$list_locs[[loc]]
    expect_named(merged_loc, c("ENTRY", "NAME", "REPS"))
    expect_identical(merged_loc$REPS, sort(merged_loc$REPS, decreasing = TRUE))
    expected_reps <- setNames(generated$REPS, data$NAME[row_of(generated$ENTRY)])
    expect_identical(merged_loc$REPS, unname(expected_reps[merged_loc$NAME]))
    expect_identical(sum(merged_loc$REPS[-(1:2)]), as.numeric(allocation$size_locations[loc]))
  }
})

test_that("merge_user_data() rejects uploads that do not match the allocation", {
  allocation <- do_optim(design = "sparse", lines = 20, l = 3, copies_per_entry = 2,
                         add_checks = TRUE, checks = 2, seed = 1)
  data <- data.frame(ENTRY = c(901, 902, 101:120), NAME = c("C1", "C2", paste0("L", 1:20)))
  expect_error(merge_user_data(allocation, data[-3, ], lines = 20, add_checks = TRUE, checks = 2),
               class = "fieldhub_input_error")
  duplicated_entry <- data; duplicated_entry$ENTRY[4] <- 101
  expect_error(merge_user_data(allocation, duplicated_entry, lines = 20, add_checks = TRUE,
                               checks = 2), class = "fieldhub_input_error")
  duplicated_name <- data; duplicated_name$NAME[4] <- "L1"
  expect_error(merge_user_data(allocation, duplicated_name, lines = 20, add_checks = TRUE,
                               checks = 2), class = "fieldhub_input_error")
  expect_error(merge_user_data(allocation, data, lines = 20, add_checks = TRUE, checks = 2,
                               rep_checks = c(2, 2, 2)), class = "fieldhub_input_error")
  # No upload: nothing to merge (callers only call it with data)
  expect_null(merge_user_data(allocation, NULL, lines = 20))
})

test_that("the choice helpers spatial modules call before randomizing are Shiny-free and leave the RNG alone", {
  allocation <- do_optim(design = "prep", lines = 40, l = 2, copies_per_entry = 3,
                         add_checks = TRUE, checks = 2, rep_checks = c(2, 2), seed = 7)
  design <- RCBD_augmented(lines = 50, checks = 3, b = 5, seed = 29)
  calls <- list(
    field_dimensions = function() field_dimensions(270),
    diagonal_dimension_choices = function() diagonal_dimension_choices(90, checks = 1:4),
    diagonal_check_options = function() {
      diagonal_check_options(n_rows = 15, n_cols = 20, checks = 1:4, Option_NCD = TRUE,
                             kindExpt = "SUDC", planter_mov1 = "serpentine", data = NULL,
                             dim_data = 274, dim_data_1 = 270, Block_Fillers = NULL)
    },
    optimized_dimension_choices = function() optimized_dimension_choices(120),
    prep_dimension_options = function() prep_dimension_options(64, allow_fillers = TRUE),
    set_augmented_blocks = function() set_augmented_blocks(60, 4, start = 3),
    allocation_entry_names = function() allocation_entry_names(allocation, 40),
    field_book_location_grids = function() field_book_location_grids(design$fieldBook, "ENTRY"),
    checked_layout_view = function() checked_layout_view(design, location = 1)
  )
  ui_packages <- c("shiny", "DT", "shinyjs", "shinyalert", "bslib", "plotly")
  # Every FielDHub function a helper can reach, not only its own body:
  # follow each package-function symbol the body refers to (called, or
  # passed on as in lapply(x, f)), transitively.
  namespace <- asNamespace("FielDHub")
  package_functions <- names(Filter(is.function,
                                    mget(ls(namespace, all.names = TRUE), namespace)))
  reachable <- function(name) {
    seen <- character()
    queue <- name
    while (length(queue) > 0L) {
      current <- queue[1]
      queue <- queue[-1]
      if (current %in% seen) next
      seen <- c(seen, current)
      f <- get(current, envir = namespace)
      if (!is.function(f) || is.primitive(f)) next
      queue <- c(queue, setdiff(intersect(fieldhub_symbol_refs(body(f)), package_functions), seen))
    }
    seen
  }
  for (name in names(calls)) {
    for (callee in reachable(name)) {
      f <- get(callee, envir = namespace)
      if (is.primitive(f)) next
      expect_identical(intersect(all.names(body(f)), ui_packages), character(0),
                       info = paste(name, "->", callee))
      expect_false(grepl("^(app_|mod_)", callee), info = paste(name, "->", callee))
    }
    set.seed(2026)
    before <- .Random.seed
    result <- calls[[name]]()
    expect_false(is.null(result), info = name)
    expect_identical(.Random.seed, before, info = name)
  }
})
