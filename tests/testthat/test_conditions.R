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
  functions <- core_functions()
  direct_stop <- names(Filter(
    function(x) "stop" %in% all.names(body(x), functions = TRUE),
    functions
  ))

  expect_setequal(direct_stop, "fieldhub_abort")
})

test_that("core code signals warnings only through fieldhub_warn()", {
  # See helper-source.R: core_functions() inspects the namespace instead of
  # parsing R/ source files, so this also works under R CMD check, where no
  # R/ directory exists (constraints.md ruling R2).
  functions <- core_functions()
  direct_warning <- names(Filter(
    function(x) "warning" %in% all.names(body(x), functions = TRUE),
    functions
  ))

  expect_setequal(direct_warning, "fieldhub_warn")
})

test_that("wrong-length plot numbers and location names warn with the values used", {
  w <- expect_warning(RCBD(t = 4, reps = 2, l = 2, plotNumber = 101, seed = 1),
                      class = "fieldhub_default_warning")
  expect_identical(w$argument, "plotNumber")
  expect_identical(w$supplied, 101)
  expect_length(w$used, 2L)
  # plotNumber must have one value per location here, otherwise RCBD()'s own
  # default plotNumber = 101 (length 1) also mismatches l = 2 and warns first,
  # leaking a second, unmatched warning (RCBD's plotNumber check runs before
  # its locationNames check; test_default_arguments.R avoids this the same
  # way).
  w <- expect_warning(RCBD(t = 4, reps = 2, l = 2, plotNumber = c(1, 101),
                           locationNames = "A", seed = 1),
                      class = "fieldhub_default_warning")
  expect_identical(w$argument, "locationNames")
})

test_that("split_plot() and split_split_plot() warn with a classed default when plotNumber is NULL", {
  w <- expect_warning(
    split_plot(wp = 3, sp = 2, reps = 2, plotNumber = NULL, seed = 1),
    class = "fieldhub_default_warning"
  )
  expect_identical(w$argument, "plotNumber")
  expect_null(w$supplied)

  w <- expect_warning(
    split_split_plot(wp = 2, sp = 2, ssp = 2, reps = 2, plotNumber = NULL, seed = 1),
    class = "fieldhub_default_warning"
  )
  expect_identical(w$argument, "plotNumber")
  expect_null(w$supplied)
})

test_that("row_column() warns with a classed default for a wrong-length IBD plotNumber", {
  w <- expect_warning(
    row_column(t = 12, nrows = 3, reps = 2, l = 2, plotNumber = 101, seed = 1,
               method = "twostage", iterations = 50),
    class = "fieldhub_default_warning"
  )
  expect_identical(w$argument, "plotNumber")
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
                                                        rep_checks = 20, checks = 1:5)),
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

test_that("plot() of an unavailable view is a classed error with the options", {
  # Regression test: plot_layout() used to warn (fieldhub_layout_warning) and
  # return NULL, which plot.FielDHub() then turned into a generic error that
  # did not carry the valid options. Now the location/layout/stacking check
  # itself raises the classed error, with no warning along the way.
  rcbd <- RCBD(t = 6, reps = 3, seed = 1)
  e <- expect_error(plot(rcbd, l = 3), class = "fieldhub_input_error")
  expect_true(length(e$options) >= 1L)
  expect_warning(
    expect_error(plot(rcbd, layout = 99), "available layout option",
                 class = "fieldhub_input_error"),
    NA
  )
})

test_that("the app shows FielDHub errors as validation messages", {
  skip_if_not_installed("shiny")
  err <- tryCatch(validate_design(CRD(t = 5, reps = 3, plotNumber = 0)),
                  error = function(e) e)
  expect_s3_class(err, "shiny.silent.error")
  expect_match(conditionMessage(err), "plotNumber must be an integer greater than 0")
})

test_that("app/module functions do not call cat() or print() outside a rendered summary", {
  # See helper-source.R: app_functions() inspects the namespace instead of
  # parsing R/ source files, so this also works under R CMD check, where no
  # R/ directory exists (constraints.md ruling R2). fieldhub_calls_named()
  # is used instead of all.names(body(f)) because the latter also matches a
  # same-named argument read (app_table_export_buttons() has a `print`
  # formal), not just a call.
  functions <- app_functions()
  direct_cat_or_print <- names(Filter(
    function(x) fieldhub_calls_named(body(x), c("cat", "print")),
    functions
  ))
  # Each of these `*_server` functions renders its design summary with
  # `shiny::renderPrint({ cat(...); print(...) })` bound to a
  # `shiny::verbatimTextOutput()` in the module's own UI (e.g.
  # the generic design page's "summary"): renderPrint() captures
  # the cat()/print() output into that text output, it never reaches the R
  # console, so this is the legitimate exception ruling R2 allows.
  summary_render_servers <- c(
    "mod_Alpha_Lattice_server",
    "mod_design_server",
    "mod_IBD_server",
    "mod_Optim_server",
    "mod_pREPS_server",
    "mod_RCBD_augmented_server",
    "mod_Rectangular_Lattice_server",
    "mod_RowCol_server",
    "mod_Square_Lattice_server"
  )
  expect_setequal(direct_cat_or_print, summary_render_servers)
})

test_that("no *_server function body calls shinyjs::useShinyjs()", {
  # useShinyjs() only registers the JavaScript shinyjs needs when it is part
  # of a UI definition; calling it inside a server function is a no-op. Each
  # module keeps its (one) useShinyjs() call in its `*_ui` function instead.
  functions <- app_functions()
  server_functions <- functions[grepl("_server$", names(functions))]
  direct_shinyjs <- names(Filter(
    function(x) fieldhub_calls_named(body(x), "useShinyjs"),
    server_functions
  ))
  expect_setequal(direct_shinyjs, character(0))
})
