#' A worker pool owned by one running app, never the default mirai pool
#' @noRd
app_worker_lifecycle <- function(workers, available = function() requireNamespace("mirai", quietly = TRUE),
                                 installed = function() {
                                   file.exists(file.path(getNamespaceInfo(asNamespace("FielDHub"), "path"),
                                                         "Meta", "package.rds"))
                                 },
                                 start_pool = function(workers, profile) mirai::daemons(workers, .compute = profile),
                                 stop_pool = function(profile) mirai::daemons(0, .compute = profile),
                                 inform = message) {
  design_worker_count(workers)
  runtime <- new.env(parent = emptyenv())
  runtime$backend <- "sync"
  runtime$profile <- basename(tempfile("fieldhub-workers-"))
  runtime$libraries <- .libPaths()
  runtime$started <- FALSE
  runtime$active <- FALSE
  runtime$stop <- function() {
    if (runtime$active) {
      runtime$active <- FALSE
      runtime$backend <- "sync"
      stop_pool(runtime$profile)
    }
    invisible(NULL)
  }
  runtime$start <- function(register_stop = function(callback) shiny::onStop(callback, session = NULL)) {
    if (runtime$started) return(invisible(NULL))
    runtime$started <- TRUE
    if (workers == 0) return(invisible(NULL))
    reason <- if (!available()) "Install the optional mirai package to enable workers." else if (!installed()) {
      "Workers require an installed FielDHub package; development source uses synchronous execution."
    }
    if (!is.null(reason)) {
      inform(paste("FielDHub:", reason))
      return(invisible(NULL))
    }
    # Register before starting anything, so a registration failure cannot leak workers.
    register_stop(runtime$stop)
    local_rng_state()
    previous_kind <- RNGkind()
    on.exit(do.call(RNGkind, as.list(previous_kind)), add = TRUE, after = FALSE)
    runtime$active <- TRUE
    tryCatch({
      start_pool(workers, runtime$profile)
      runtime$backend <- design_task_backend(TRUE, TRUE)
    }, error = function(condition) {
      tryCatch(runtime$stop(), error = function(e) NULL)
      inform(paste("FielDHub: workers could not start; using synchronous execution.",
                   conditionMessage(condition)))
    })
    invisible(NULL)
  }
  runtime
}

#' A Run or Randomize button with immediate feedback, even for small designs
#' @noRd
app_task_button <- function(id, label, ..., busy_message = "Preparing your results...", results_id = NULL) {
  shiny::actionButton(id, label,
    `aria-controls` = results_id,
    `data-fieldhub-task` = id, `data-fieldhub-message` = busy_message, ...)
}

#' Replace one tab's content with a centered loading state while its tasks run
#' @noRd
app_task_feedback <- function(tasks, ...) {
  shiny::div(class = "fieldhub-task-region", `data-fieldhub-tasks` = paste(tasks, collapse = " "),
    shiny::div(class = "fieldhub-task-content", ...),
    shiny::div(class = "fieldhub-task-feedback", hidden = "hidden",
      role = "status", `aria-live` = "polite", `aria-atomic` = "true",
      app_loading_indicator(NULL)))
}

#' Send every terminal state, even when consecutive requests have the same status
#' @noRd
app_report_task_feedback <- function(session, id, busy, message = "") {
  session$sendCustomMessage("fieldhub-task-feedback", list(
    id = session$ns(id), busy = isTRUE(busy), message = if (isTRUE(busy)) message else ""))
}

#' Run immutable argument snapshots outside the reactive graph
#'
#' Only the most recent request may publish a result. Invalid input clears
#' the previous result too, so a slow worker cannot revive an obsolete design.
#' @noRd
app_design_task <- function(id_prefix, engine, args_reactive, on_done = identity,
                             long_running = TRUE, busy_message = "Randomizing design...") {
  # Resolve configuration now: callers may reuse their local reactive variable
  # for the next stage before our observers first run.
  force(args_reactive)
  force(on_done)
  force(busy_message)
  session <- shiny::getDefaultReactiveDomain()
  runtime <- session$userData$fieldhub_task_runtime
  engine_name <- design_engine_name(engine)
  sequence <- 0L
  submitted <- shiny::reactiveVal(NULL)
  completed <- shiny::reactiveVal(NULL)
  request <- shiny::reactive({
    tryCatch(list(ok = TRUE, args = args_reactive()),
             error = function(condition) list(ok = FALSE, condition = condition, warnings = list()))
  })
  task <- shiny::ExtendedTask$new(function(args, request_id, rng_kind) {
    promise <- tryCatch(if (!is.null(runtime) && identical(runtime$backend, "mirai")) {
      library_paths <- runtime$libraries
      mirai::mirai({
        .libPaths(library_paths)
        # The worker is a fresh process; resolve the installed internal runner.
        job_runner <- get("run_design_job", envir = asNamespace("FielDHub"), inherits = FALSE)
        job_runner(engine, args, rng_kind)
      }, engine = engine_name, args = args, rng_kind = rng_kind,
      library_paths = library_paths, .compute = runtime$profile)
    } else {
      # Tab-local feedback covers both synchronous work and background jobs.
      value <- run_design_job(engine_name, args, rng_kind)
      promises::promise_resolve(value)
    }, error = function(condition) promises::promise_reject(condition))
    promises::then(promise,
      onFulfilled = function(value) list(id = request_id, job = value),
      onRejected = function(condition) {
        if (!inherits(condition, "condition")) condition <- simpleError(as.character(condition))
        list(id = request_id, job = list(ok = FALSE, condition = condition, warnings = list()))
      })
  })
  # bslib task buttons require Bootstrap 5; preserve this app's Bootstrap 3 theme.
  if (isTRUE(long_running)) shiny::observe({
    if (identical(task$status(), "running")) shinyjs::disable(id_prefix) else shinyjs::enable(id_prefix)
  })
  shiny::observeEvent(request(), {
    current <- request()
    sequence <<- sequence + 1L
    submitted(current)
    completed(NULL)
    if (!current$ok) {
      completed(current)
      app_report_task_feedback(session, id_prefix, FALSE)
    } else {
      app_report_task_feedback(session, id_prefix, TRUE, busy_message)
      task$invoke(current$args, sequence, RNGkind())
    }
  }, priority = 20)
  shiny::observeEvent(task$result(), {
    result <- task$result()
    if (result$id != sequence) return(invisible(NULL))
    job <- result$job
    if (isTRUE(job$ok)) {
      extra <- list()
      job <- tryCatch({
        value <- capture_fieldhub_warnings(on_done(job$value),
          on_error = function(condition, warnings) extra <<- warnings)
        list(ok = TRUE, value = value$value, warnings = c(job$warnings, value$warnings))
      }, error = function(condition) {
        list(ok = FALSE, condition = condition, warnings = c(job$warnings, extra))
      })
    }
    for (condition in job$warnings) app_report_problem(condition)
    if (!isTRUE(job$ok) && !inherits(job$condition, "shiny.silent.error")) {
      if (!inherits(job$condition, "fieldhub_error")) app_log_problem(job$condition)
      app_report_problem(job$condition)
    }
    completed(job)
    app_report_task_feedback(session, id_prefix, FALSE)
  })
  shiny::reactive({
    current <- request()
    shiny::req(identical(current, submitted()))
    result <- shiny::req(completed())
    if (!isTRUE(result$ok)) {
      if (inherits(result$condition, "shiny.silent.error")) stop(result$condition)
      shiny::validate(problem_message(result$condition))
    }
    result$value
  })
}
