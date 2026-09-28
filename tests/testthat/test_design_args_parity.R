library(testthat)
library(FielDHub)

# Task 11a (M2 AD-01): every classic module builds its API call through a
# plain design_args_<Module>(values, data) function (R/app_design_args.R)
# instead of assembling arguments inline. These tests prove app/API parity:
# do.call(<engine>, design_args_<Module>(values, data)) must reproduce the
# exact fieldBook and metadata$parameters of an equivalent direct API call,
# for the same inputs and seed.

parity <- function(builder, engine, values, data = NULL, direct) {
  via_app <- do.call(engine, builder(values, data))
  expect_identical(via_app$fieldBook, direct$fieldBook)
  expect_identical(via_app$metadata$parameters, direct$metadata$parameters)
}

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
  via_app <- suppress_design_warning(do.call(row_column, design_args_RowCol(values)))
  direct <- suppress_design_warning(row_column(t = 9, nrows = 3, reps = 2, l = 2,
                                               plotNumber = c(101, 1001), locationNames = c("A", "B"),
                                               seed = 14))
  expect_identical(via_app$fieldBook, direct$fieldBook)
  expect_identical(via_app$metadata$parameters, direct$metadata$parameters)
})

test_that("RowCol app arguments reproduce an uploaded-data design", {
  data <- data.frame(ENTRY = 1:9, TREATMENT = paste0("ND-", 1:9))
  values <- list(t = 9, nrows = 3, reps = 2, l = 1, plot_start = 101, location_names = "A", seed = 5)
  via_app <- suppress_design_warning(do.call(row_column, design_args_RowCol(values, data)))
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
  built <- design_args_RCBD(values)
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
  expect_identical(design_args_LSD(values)$planter, "serpentine")
})

test_that("design_args_FD() falls back to l/type/planter defaults", {
  values <- list(setfactors = c(2, 2), reps = 2, plot_start = 101, location_names = "Loc", seed = 1)
  parity(design_args_FD, full_factorial, values,
         direct = full_factorial(setfactors = c(2, 2), reps = 2, plotNumber = 101,
                                 locationNames = "Loc", seed = 1))
  built <- design_args_FD(values)
  expect_identical(built$l, 1)
  expect_identical(built$type, 2)
  expect_identical(built$planter, "serpentine")
})

test_that("design_args_SPD() and design_args_SSPD() fall back to l/type defaults", {
  values <- list(wp = 3, sp = 2, reps = 2, plot_start = 101, location_names = "Loc", seed = 1)
  parity(design_args_SPD, split_plot, values,
         direct = split_plot(wp = 3, sp = 2, reps = 2, plotNumber = 101,
                             locationNames = "Loc", seed = 1))
  built <- design_args_SPD(values)
  expect_identical(built$l, 1)
  expect_identical(built$type, 2)

  values2 <- list(wp = 2, sp = 2, ssp = 2, reps = 1, plot_start = 101, location_names = "Loc", seed = 1)
  parity(design_args_SSPD, split_split_plot, values2,
         direct = split_split_plot(wp = 2, sp = 2, ssp = 2, reps = 1, plotNumber = 101,
                                   locationNames = "Loc", seed = 1))
  built2 <- design_args_SSPD(values2)
  expect_identical(built2$l, 1)
  expect_identical(built2$type, 2)
})

test_that("design_args_STRIPD() falls back to l/planter/randomizeH/randomizeV defaults", {
  values <- list(Hplots = 3, Vplots = 3, reps = 1, plot_start = 101, location_names = "Loc", seed = 1)
  parity(design_args_STRIPD, strip_plot, values,
         direct = strip_plot(Hplots = 3, Vplots = 3, reps = 1, plotNumber = 101,
                             locationNames = "Loc", seed = 1))
  built <- design_args_STRIPD(values)
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
  expect_identical(design_args_IBD(values)$l, 1)

  values2 <- list(t = 9, k = 3, reps = 2, plot_start = 101, location_names = "Loc", seed = 1)
  parity(design_args_Alpha_Lattice, alpha_lattice, values2,
         direct = alpha_lattice(t = 9, k = 3, reps = 2, plotNumber = 101,
                                locationNames = "Loc", seed = 1))
  expect_identical(design_args_Alpha_Lattice(values2)$l, 1)
  parity(design_args_Square_Lattice, square_lattice, values2,
         direct = square_lattice(t = 9, k = 3, reps = 2, plotNumber = 101,
                                 locationNames = "Loc", seed = 1))
  expect_identical(design_args_Square_Lattice(values2)$l, 1)

  values3 <- list(t = 6, k = 2, reps = 2, plot_start = 101, location_names = "Loc", seed = 1)
  parity(design_args_Rectangular_Lattice, rectangular_lattice, values3,
         direct = rectangular_lattice(t = 6, k = 2, reps = 2, plotNumber = 101,
                                      locationNames = "Loc", seed = 1))
  expect_identical(design_args_Rectangular_Lattice(values3)$l, 1)
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
  built <- design_args_RowCol(values)
  expect_identical(built$l, 1)
  expect_identical(names(built), c("t", "nrows", "reps", "l", "plotNumber", "seed",
                                   "locationNames", "data"))
})

# --- Step 2 (ruling R2): structural checks on the classic module bodies,
# via the namespace so they also work under R CMD check (no R/ directory). ---

test_that("classic modules build their design only through design_args_<Module>() and do.call()", {
  classic_engines <- list(
    CRD = "CRD", RCBD = "RCBD", LSD = "latin_square", FD = "full_factorial",
    SPD = "split_plot", SSPD = "split_split_plot", STRIPD = "strip_plot",
    IBD = "incomplete_blocks", RowCol = "row_column",
    Alpha_Lattice = "alpha_lattice", Square_Lattice = "square_lattice",
    Rectangular_Lattice = "rectangular_lattice"
  )
  # Unexported helpers a classic module server may call, and why. Anything
  # else found in a module body must be the module's own engine/builder, a
  # base/shiny/DT/plotly/shinyjs/shinyalert call, or is a bug.
  allowed_helpers <- c(
    "validate_design",     # shows a fieldhub_error as a Shiny validation message
    "app_design_seed",     # resolves the optional app seed without touching the shared RNG stream
    "app_upload_error",    # shows the shared upload-error alert for a failed file parse
    "app_classic_layout",  # shared layout-panel lifecycle for classic design modules
    "app_classic_workflow",# shared results/simulation/export lifecycle for classic design modules
    "classic_workflow_spec", # registry of per-design IDs/labels the shared workflow helpers use
    "load_file",           # parses an uploaded CSV into a data frame
    "read_whole_numbers",  # parses a comma-separated starting-plot-number input
    "valid_block_sizes",   # lists the valid incomplete-block sizes for a treatment count
    "parse_n_checks",      # parses the RCBD "# of checks" input
    "parse_rep_checks",    # parses the RCBD "reps per check" input
    "rcbd_size_preview",   # previews the RCBD block size before Run is clicked
    "parse_whole_numbers", # parses the FD "entries per factor" input
    "design_values_CRD",   # assembles CRD's values from parsed inputs (nulls t when data is given)
    "design_values_SPD",   # assembles SPD's values from parsed inputs (nulls wp/sp when data is given)
    "design_values_SSPD"   # assembles SSPD's values from parsed inputs (nulls wp/sp/ssp when data is given)
  )
  # `default_entries` is deliberately absent from allowed_helpers: no classic
  # module still needs it to build a generated-path entry table (that would
  # defeat decision 2 -- pass bare counts and let the engine generate the
  # same "G-" labels itself). Its absence is a regression guard, enforced by
  # the setdiff() check below: if a module's generated path called
  # default_entries(nt) again (as mod_IBD.R/mod_RowCol.R/etc. used to), that
  # symbol would show up in `called` but not in `allowed`, and the test would
  # fail -- the same way it would for any other un-allow-listed helper.
  forbidden <- c("sample", "set.seed", "runif", "get.levels", "blocksdesign")

  namespace <- asNamespace("FielDHub")
  all_objects <- mget(ls(namespace, all.names = TRUE), namespace, inherits = FALSE)
  package_functions <- names(Filter(is.function, all_objects))

  modules <- app_functions()
  for (module in names(classic_engines)) {
    server_name <- paste0("mod_", module, "_server")
    f <- modules[[server_name]]
    expect_false(is.null(f), info = server_name)
    code <- body(f)
    nm <- all.names(code)

    expect_identical(sum(nm == "do.call"), 1L, info = module)
    expect_identical(sum(nm == classic_engines[[module]]), 1L, info = module)
    expect_identical(sum(nm == paste0("design_args_", module)), 1L, info = module)
    expect_identical(intersect(nm, forbidden), character(0), info = module)

    called <- intersect(nm, package_functions)
    allowed <- c(allowed_helpers, classic_engines[[module]], paste0("design_args_", module))
    expect_identical(setdiff(called, allowed), character(0), info = module)
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
  built <- design_args_Diagonal(values)
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
  # The module no longer narrows the candidates with its own minimum_extra
  expect_identical(formals(diagonal_dimension_choices)$minimum_extra,
                   formals(field_dimensions)$minimum_extra)
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
  expect_identical(design_args_diagonal_multiple(values)$plotNumber, c(5, 5))
  # A start count that is neither one nor one per experiment: the engine's
  # own per-location default, default_plot_starts(l, 1001), silently
  misfit <- utils::modifyList(values, list(plot_start = c(1, 2), location_names = "ONLY"))
  built <- design_args_diagonal_multiple(misfit)
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
  expect_identical(design_args_diagonal_multiple(values)$splitBy, "row")
  parity(design_args_diagonal_multiple, diagonal_arrangement, values,
         direct = diagonal_arrangement(nrows = 12, ncols = 18, lines = 200, checks = 4,
                                       plotNumber = 1, kindExpt = "DBUDC", seed = 21,
                                       blocks = blocks, sameEntries = TRUE))
})

test_that("sparse_allocation app arguments reproduce the API allocation and design across three locations", {
  # Run!: the allocation, through its own builder
  optim_values <- list(lines = 135, l = 3, copies_per_entry = 2, checks = 4, seed = 5)
  via_app_allocation <- do.call(do_optim, design_args_sparse_allocation_optim(optim_values))
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
  via_app_allocation <- do.call(do_optim, design_args_sparse_allocation_optim(optim_values, data))
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
  built <- design_args_sparse_allocation(values)
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
  built <- design_args_sparse_allocation(values)
  expect_false(any(c("sparse_list", "nrows", "ncols") %in% names(built)))
  parity(design_args_sparse_allocation, sparse_allocation, values,
         direct = sparse_allocation(lines = 135, l = 3, plotNumber = c(1, 1001, 2001),
                                    copies_per_entry = 2, checks = 4, seed = 5))
})

test_that("sparse_allocation() without dimensions builds the field the app preselects", {
  # The app offers diagonal_dimension_choices() (field_dimensions()
  # candidates the checks fit, squarest first) and preselects the first;
  # sparse_allocation() now defaults to that same field. With 100 entries
  # per location it used to take 10 x 12 from a narrower candidate range.
  design <- sparse_allocation(lines = 150, l = 3, copies_per_entry = 2, checks = 4, seed = 2)
  lines_within_loc <- as.numeric(design$size_locations[1])
  expect_identical(lines_within_loc, 100)
  preselected <- diagonal_dimension_choices(lines_within_loc, checks = 151:154)[1]
  expect_identical(preselected, "10 x 11")
  expect_identical(paste(design$infoDesign$rows, "x", design$infoDesign$columns), preselected)
})

test_that("diagonal_dimension_choices(first = TRUE) is the first choice, found without testing the rest", {
  for (lines in c(46, 90, 100, 287)) {
    expect_identical(diagonal_dimension_choices(lines, checks = 1:4, first = TRUE),
                     diagonal_dimension_choices(lines, checks = 1:4)[1], info = lines)
  }
  layout <- data.frame(ENTRY = 1:304, BLOCK = c(rep("ALL", 4), rep(1:3, times = c(100, 120, 80))))
  expect_identical(
    diagonal_dimension_choices(300, 1:4, kindExpt = "DBUDC", stacked = "By Column",
                               data = layout, first = TRUE),
    diagonal_dimension_choices(300, 1:4, kindExpt = "DBUDC", stacked = "By Column",
                               data = layout)[1])
  expect_identical(diagonal_dimension_choices(40, checks = 1:4, first = TRUE), character())
  expect_error(diagonal_dimension_choices(90, checks = 1:4, first = NA),
               class = "fieldhub_input_error")
})

test_that("Optim app arguments reproduce the API design from counts across two locations", {
  values <- list(nrows = 12, ncols = 10, lines = 100, checks = 4, rep_checks = c(5, 5, 5, 5),
                 planter = "serpentine", l = 2, plot_start = c(1, 1001), seed = 5,
                 expt_name = "Expt1", location_names = c("A", "B"))
  via_app <- do.call(optimized_arrangement, design_args_Optim(values))
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

# --- Spatial structural check (ruling R2): namespace bodies, call heads ---

test_that("spatial modules build their designs only through design_args_<Module>() and do.call()", {
  # Every engine call each spatial module makes, keyed by the builder that
  # must supply its arguments.
  spatial_engines <- list(
    Diagonal = c(design_args_Diagonal = "diagonal_arrangement"),
    diagonal_multiple = c(design_args_diagonal_multiple = "diagonal_arrangement"),
    sparse_allocation = c(design_args_sparse_allocation_optim = "do_optim",
                          design_args_sparse_allocation = "sparse_allocation"),
    Optim = c(design_args_Optim = "optimized_arrangement")
  )
  # Unexported helpers a spatial module server may call, and why. Anything
  # else it calls must be one of its engines/builders, or a base/shiny/DT/
  # plotly/shinyjs/shinyalert function.
  allowed_helpers <- c(
    "validate_design",            # shows a fieldhub_error as a Shiny validation message
    "app_design_seed",            # resolves the optional app seed without touching the shared RNG stream
    "read_app_seed",              # re-reads the resolved design seed for the simulation workflow; never draws
    "app_upload_error",           # shows the shared upload-error alert for a failed file parse
    "load_file",                  # parses an uploaded CSV into a data frame
    "read_whole_numbers",         # parses the comma-separated starting-plot-number input
    "app_spatial_workflow",       # shared results/simulation/export lifecycle for spatial modules
    "spatial_workflow_spec",      # registry of per-design IDs/labels the shared workflow uses
    "app_table_export_buttons",   # DT export buttons carrying the design's metadata
    "field_dimensions",           # candidate field sizes: rejects too few entries before any randomization
    "diagonal_dimension_choices", # feasible diagonal field dimensions offered before randomizing (seed-isolated)
    "diagonal_check_options",     # percentages of checks offered for a field before randomizing (seed-isolated)
    "optimized_dimension_choices",# field dimensions offered for an optimized arrangement (no RNG)
    "field_book_location_grids",  # splits a field book into per-location EXPT grids for display
    "allocation_entry_names"      # reads the entry names of a do_optim() allocation for its table
  )
  forbidden <- c("sample", "sample.int", "set.seed", "runif", "available_percent",
                 "random_checks", "merge_user_data", "pREP", "get_random",
                 "get_random_stacked", "get_single_random", "swap_pairs",
                 "default_entries")

  namespace <- asNamespace("FielDHub")
  all_objects <- mget(ls(namespace, all.names = TRUE), namespace, inherits = FALSE)
  package_functions <- names(Filter(is.function, all_objects))
  modules <- app_functions()
  for (module in names(spatial_engines)) {
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
