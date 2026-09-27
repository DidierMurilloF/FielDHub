library(FielDHub)

test_that("fieldhub_abort builds one classed condition from message parts", {
  err <- tryCatch(
    fieldhub_abort("Invalid ", "argument", ".", call. = FALSE),
    error = function(e) e
  )

  expect_s3_class(err, "fieldhub_input_error")
  expect_s3_class(err, "fieldhub_error")
  expect_identical(conditionMessage(err), "Invalid argument.")
  expect_null(conditionCall(err))
})

test_that("package functions signal errors through fieldhub_abort", {
  namespace <- asNamespace("FielDHub")
  objects <- mget(ls(namespace, all.names = TRUE), namespace, inherits = FALSE)
  functions <- Filter(
    function(x) is.function(x) && identical(environment(x), namespace),
    objects
  )
  functions <- functions[!grepl("^(app_|golem_|mod_)|^run_app$", names(functions))]
  direct_stop <- names(Filter(
    function(x) "stop" %in% all.names(body(x), functions = TRUE),
    functions
  ))

  expect_setequal(direct_stop, "fieldhub_abort")
})

test_that("invalid arguments raise a fieldhub_input_error", {
  expect_error(CRD(t = 5, reps = 3, plotNumber = 0), class = "fieldhub_input_error")
  expect_error(incomplete_blocks(t = 12, k = 12, reps = 2), class = "fieldhub_input_error")
  expect_error(square_lattice(t = 15, k = 4, reps = 2), "square number",
               class = "fieldhub_error")
  err <- tryCatch(RCBD(t = 4, reps = 1), error = function(e) e)
  expect_false(inherits(err, "shiny.silent.error"))
})

test_that("public design functions use the shared input-error contract", {
  bad_calls <- list(
    CRD = quote(CRD(reps = 2, data = "not a data frame")),
    RCBD = quote(RCBD(t = 4, reps = 2, planter = "diagonal")),
    latin_square = quote(latin_square(t = 3, planter = "diagonal")),
    full_factorial = quote(full_factorial(setfactors = c(2, 2), planter = "diagonal")),
    split_plot = quote(split_plot(wp = 2, sp = 2, reps = 2, type = 3)),
    split_split_plot = quote(split_split_plot(wp = 2, sp = 2, ssp = 2, reps = 2, type = 3)),
    strip_plot = quote(strip_plot(Hplots = 2, Vplots = 2, reps = 2,
                                  data = "not a data frame")),
    split_families = quote(split_families(data = data.frame(
      ENTRY = 1:4, NAME = letters[1:4], FAMILY = rep(1:2, 2)
    ))),
    incomplete_blocks = quote(incomplete_blocks(
      t = 12, k = 3, reps = 2, data = "not a data frame"
    )),
    row_column = quote(row_column(
      t = 12, nrows = 3, reps = 2, data = "not a data frame"
    )),
    square_lattice = quote(square_lattice(
      t = 16, k = 4, reps = 2, data = "not a data frame"
    )),
    rectangular_lattice = quote(rectangular_lattice(
      t = 12, k = 3, reps = 2, data = "not a data frame"
    )),
    alpha_lattice = quote(alpha_lattice(
      t = 12, k = 3, reps = 2, data = "not a data frame"
    )),
    RCBD_augmented = quote(RCBD_augmented(planter = "diagonal")),
    diagonal_arrangement = quote(diagonal_arrangement(planter = "diagonal")),
    optimized_arrangement = quote(optimized_arrangement(planter = "diagonal")),
    partially_replicated = quote(partially_replicated(planter = "diagonal")),
    do_optim = quote(do_optim(lines = 20, l = 2, design = "unknown"))
  )

  for (fun in names(bad_calls)) {
    expect_error(eval(bad_calls[[fun]]), class = "fieldhub_input_error", info = fun)
  }
})

# The error, and anything printed, from a call that should fail
failure_of <- function(expr) {
  err <- NULL
  printed <- utils::capture.output(err <- tryCatch(expr, error = function(e) e))
  list(error = err, printed = printed)
}

test_that("field dimensions that do not fit raise an error with the valid options", {
  # Regression test: these functions printed a banner and returned NULL, or,
  # for RCBD_augmented(), returned a data frame of options instead of a design
  calls <- list(
    diagonal_arrangement = quote(diagonal_arrangement(nrows = 7, ncols = 7, lines = 100, checks = 4)),
    optimized_arrangement = quote(optimized_arrangement(nrows = 7, ncols = 7, lines = 100,
                                                        amountChecks = 20, checks = 1:5)),
    partially_replicated = quote(partially_replicated(nrows = 7, ncols = 7, repGens = c(50, 7),
                                                      repUnits = c(1, 2))),
    RCBD_augmented = quote(RCBD_augmented(lines = 20, checks = 3, b = 4, nrows = 7, ncols = 7))
  )
  for (fn in names(calls)) {
    result <- failure_of(suppressWarnings(eval(calls[[fn]])))
    expect_s3_class(result$error, "fieldhub_dimension_error")
    expect_match(conditionMessage(result$error), fn, fixed = TRUE, info = fn)
    expect_s3_class(result$error$options, "data.frame")
    expect_gt(nrow(result$error$options), 0)
    expect_length(result$printed, 0)
  }
})

test_that("plot() explains that a layout option is not available", {
  rcbd <- RCBD(t = 6, reps = 3, seed = 1)
  expect_warning(
    expect_error(plot(rcbd, layout = 99), class = "fieldhub_error"),
    "Layout option 99 is not available"
  )
})

test_that("the app shows FielDHub errors as validation messages", {
  skip_if_not_installed("shiny")
  err <- tryCatch(validate_design(CRD(t = 5, reps = 3, plotNumber = 0)),
                  error = function(e) e)
  expect_s3_class(err, "shiny.silent.error")
  expect_match(conditionMessage(err), "plotNumber must be an integer greater than 0")
})
