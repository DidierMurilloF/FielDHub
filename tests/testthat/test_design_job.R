test_that("design jobs return full results and classed failures", {
  x <- RCBD(t = 4, reps = 2, seed = 1)
  ok <- run_design_job("RCBD", list(t = 4, reps = 2, seed = 1))
  expect_true(ok$ok)
  expect_identical(ok$value, x)
  expect_length(ok$warnings, 0L)
  bad <- run_design_job("RCBD", list(t = 4, reps = 0, seed = 1))
  expect_false(bad$ok)
  expect_s3_class(bad$condition, "fieldhub_input_error")
  for (engine in list("run_app", "not_an_engine", "base::system", NA_character_, c("RCBD", "CRD"))) {
    bad <- run_design_job(engine, list())
    expect_false(bad$ok)
    expect_s3_class(bad$condition, "fieldhub_input_error")
  }
  for (args in list(NULL, 1, list(1), setNames(list(1, 2), c("t", "t")))) {
    expect_false(run_design_job("RCBD", args)$ok)
  }
})

test_that("design jobs keep warnings even when generation fails", {
  warned <- run_design_job("RCBD", list(t = 4, reps = 2, l = 2, plotNumber = 101, seed = 1))
  expect_true(warned$ok)
  expect_true(any(vapply(warned$warnings, inherits, logical(1), "fieldhub_default_warning")))
  testthat::local_mocked_bindings(
    RCBD = function(...) {
      fieldhub_warn("A recorded warning", class = "fieldhub_design_warning")
      fieldhub_abort("A recorded error")
    }, .package = "FielDHub")
  bad <- run_design_job("RCBD", list(seed = 1))
  expect_false(bad$ok)
  expect_length(bad$warnings, 1L)
  expect_s3_class(bad$condition, "fieldhub_input_error")
})

test_that("jobs use the submitted RNG kind and restore process state", {
  local_rng_state()
  previous <- RNGkind()
  on.exit(do.call(RNGkind, as.list(previous)), add = TRUE, after = FALSE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  args <- list(t = 12, nrows = 3, reps = 2, method = "twostage", seed = 4)
  expected <- do.call(row_column, args)
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  set.seed(9)
  seed <- .Random.seed
  kind <- RNGkind()
  opts <- options()
  job <- run_design_job("row_column", args, rng_kind = c("Mersenne-Twister", "Inversion", "Rejection"))
  expect_true(job$ok)
  expect_identical(job$value, expected)
  expect_identical(.Random.seed, seed)
  expect_identical(RNGkind(), kind)
  expect_identical(options(), opts)
  expect_false(run_design_job("RCBD", list(t = 4, reps = 0), rng_kind = c("bad", "kind", "value"))$ok)
  expect_identical(.Random.seed, seed)
  expect_identical(RNGkind(), kind)
})

test_that("backend selection and worker settings have safe defaults", {
  expect_identical(design_task_backend(TRUE, TRUE), "mirai")
  expect_identical(design_task_backend(TRUE, FALSE), "sync")
  expect_identical(design_task_backend(FALSE, TRUE), "sync")
  expect_identical(design_task_backend(FALSE, FALSE), "sync")
  expect_identical(formals(run_app)$workers, 0L)
  for (workers in list(NULL, NA, -1, 1.5, "2", c(1, 2), Inf)) {
    expect_error(design_worker_count(workers), class = "fieldhub_input_error")
  }
  expect_identical(design_worker_count(0L), 0L)
  expect_identical(design_worker_count(2), 2)
})

test_that("long-running specs share a task lifecycle and a progress message", {
  long <- c("IBD", "RowCol", "Alpha_Lattice", "Square_Lattice", "Rectangular_Lattice",
            "Optim", "RCBD_augmented", "pREPS", "Diagonal", "diagonal_multiple", "sparse_allocation", "multi_loc_preps")
  for (module in long) {
    spec <- design_app_spec(module)
    expect_true(spec$long_running, info = module)
    expect_true(is.character(spec$busy_message) && length(spec$busy_message) == 1L && nzchar(spec$busy_message),
                 info = module)
  }
  expect_false(design_app_spec("CRD")$long_running)
  expect_true("app_design_task" %in% all.names(body(mod_design_server)))
  expect_true("app_design_task" %in% all.names(body(app_spatial_page)))
  expect_identical(as.list(body(app_design_task))[[2L]], quote(force(args_reactive)))
  expect_false(any(c("shiny", "bslib", "promises", "mirai") %in% all.names(body(run_design_job))))
})

test_that("worker pools belong to the app and close once", {
  started <- stopped <- list()
  callbacks <- list()
  runtime <- app_worker_lifecycle(2, available = function() TRUE, installed = function() TRUE,
    start_pool = function(workers, profile) started[[length(started) + 1L]] <<- list(workers, profile),
    stop_pool = function(profile) stopped[[length(stopped) + 1L]] <<- profile)
  expect_identical(runtime$backend, "sync")
  expect_length(started, 0L)
  set.seed(34)
  before <- .Random.seed
  opts <- options()
  runtime$start(register_stop = function(callback) callbacks[[length(callbacks) + 1L]] <<- callback)
  expect_identical(runtime$backend, "mirai")
  expect_length(started, 1L)
  expect_identical(started[[1L]][[1L]], 2)
  expect_false(runtime$profile %in% c("default", ""))
  expect_identical(started[[1L]][[2L]], runtime$profile)
  expect_identical(.Random.seed, before)
  expect_identical(options(), opts)
  callbacks[[1L]]()
  callbacks[[1L]]()
  expect_identical(runtime$backend, "sync")
  expect_identical(stopped, list(runtime$profile))
})

test_that("task controls keep the existing theme and announce progress", {
  previous <- options(sass.cache = FALSE)
  on.exit(options(previous), add = TRUE)
  button <- as.character(app_task_button("x-run", "Run!", TRUE))
  expect_match(button, 'class="btn btn-default action-button"', fixed = TRUE)
  expect_match(button, 'aria-describedby="x-run_status"', fixed = TRUE)
  expect_false(grepl("bslib-task-button", button, fixed = TRUE))
  expect_false(grepl("aria-describedby", as.character(app_task_button("x-run", "Run!", FALSE)), fixed = TRUE))
  # Render dependencies too: deferred bslib dependencies can reject the theme
  # even when as.character() of an individual control succeeds.
  expect_no_error(htmltools::renderTags(app_ui(NULL)))
})

test_that("unavailable workers fall back once without touching another pool", {
  for (reason in c("disabled", "missing", "development", "startup")) {
    notices <- character()
    starts <- stops <- 0L
    runtime <- app_worker_lifecycle(if (reason == "disabled") 0L else 2L,
      available = function() reason != "missing", installed = function() reason != "development",
      start_pool = function(workers, profile) {starts <<- starts + 1L; stop("cannot start")},
      stop_pool = function(profile) stops <<- stops + 1L,
      inform = function(message) notices <<- c(notices, message))
    runtime$start(register_stop = function(callback) NULL)
    runtime$start(register_stop = function(callback) NULL)
    expect_identical(runtime$backend, "sync")
    expect_length(notices, if (reason == "disabled") 0L else 1L)
    expect_identical(starts, if (reason == "startup") 1L else 0L)
    expect_identical(stops, if (reason == "startup") 1L else 0L)
    runtime$stop()
    expect_identical(stops, if (reason == "startup") 1L else 0L)
  }
})
