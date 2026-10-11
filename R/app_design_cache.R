#' Submit one immutable design request to the configured backend
#' @noRd
app_design_job_promise <- function(engine, args, rng_kind, runtime) {
  tryCatch({
    if (!is.null(runtime) && identical(runtime$backend, "mirai")) {
      library_paths <- runtime$libraries
      mirai::mirai({
        .libPaths(library_paths)
        job_runner <- get("run_design_job", envir = asNamespace("FielDHub"), inherits = FALSE)
        job_runner(engine, args, rng_kind)
      }, engine = engine, args = args, rng_kind = rng_kind,
      library_paths = library_paths, .compute = runtime$profile)
    } else {
      promises::promise_resolve(run_design_job(engine, args, rng_kind))
    }
  }, error = function(condition) promises::promise_reject(condition))
}

#' Publish one selected job through the page's acceptance and condition handling
#' @noRd
app_accept_design_job <- function(job, on_done) {
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
  job
}

#' Cache selected designs and prepare alternatives only on background workers
#' @noRd
app_cached_design_task <- function(id_prefix, engine, args_reactive, prefetch_args,
                                    on_done, long_running, busy_message) {
  session <- shiny::getDefaultReactiveDomain()
  runtime <- session$userData$fieldhub_task_runtime
  engine_name <- design_engine_name(engine)
  queue <- new_design_option_queue()
  submitted <- shiny::reactiveVal(NULL)
  completed <- shiny::reactiveVal(NULL)
  published <- NULL
  closed <- FALSE
  session$onSessionEnded(function() {
    closed <<- TRUE
    queue$clear()
  })
  request <- shiny::reactive({
    tryCatch(list(ok = TRUE, args = args_reactive()),
      error = function(condition) list(ok = FALSE, condition = condition, warnings = list()))
  })
  task <- shiny::ExtendedTask$new(function(token) {
    key <- token$key
    promise <- app_design_job_promise(key$engine, key$args, key$rng_kind, runtime)
    promises::then(promise,
      onFulfilled = function(job) list(token = token, job = job),
      onRejected = function(condition) {
        if (!inherits(condition, "condition")) condition <- simpleError(as.character(condition))
        list(token = token, job = list(ok = FALSE, condition = condition, warnings = list()))
      })
  })
  dispatch <- function() {
    if (closed) return(invisible(NULL))
    # The synchronous fallback computes only requested options, avoiding work
    # that would block every session merely to fill a speculative cache.
    background <- !is.null(runtime) && identical(runtime$backend, "mirai")
    token <- queue$next_job(allow_prefetch = background)
    if (!is.null(token)) task$invoke(token)
    invisible(NULL)
  }
  publish <- function() {
    job <- queue$result()
    if (is.null(job) || identical(job, published)) return(invisible(NULL))
    published <<- job
    completed(app_accept_design_job(job, on_done))
    app_report_task_feedback(session, id_prefix, FALSE)
    invisible(NULL)
  }
  if (isTRUE(long_running)) shiny::observe({
    if (is.null(completed()) && !is.null(submitted())) {
      shinyjs::disable(id_prefix)
    } else shinyjs::enable(id_prefix)
  })
  shiny::observeEvent(request(), {
    current <- request()
    submitted(current)
    completed(NULL)
    published <<- NULL
    if (!current$ok) {
      queue$clear()
      completed(current)
      app_report_task_feedback(session, id_prefix, FALSE)
    } else {
      kind <- RNGkind()
      key <- function(args) list(engine = engine_name, args = args, rng_kind = kind)
      alternatives <- lapply(prefetch_args(), key)
      queue$select(key(current$args), alternatives)
      app_report_task_feedback(session, id_prefix, TRUE, busy_message)
      publish()
      dispatch()
    }
  }, priority = 20)
  shiny::observeEvent(task$result(), {
    if (closed) return(invisible(NULL))
    result <- task$result()
    queue$finish(result$token, result$job)
    publish()
    dispatch()
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
