# The app shell: dependencies and startup, the bundled resources, the Help,
# About and Welcome pages, and how problems reach the user (validation
# messages, dialogs and notices).

library(FielDHub)

# Strings of `texts` that `html` does not contain.
missing_texts <- function(html, texts) {
  texts[!vapply(texts, function(text) grepl(text, html, fixed = TRUE), logical(1))]
}

input_error <- function(message = "Starting Plot Number cannot be blank.") {
  tryCatch(fieldhub_abort(message), error = function(e) e)
}

# ---- dependencies and startup ----------------------------------------------

test_that("the standard app installs with FielDHub while workers and tooling are optional", {
  app <- c("shiny", "htmltools", "DT", "bslib", "promises", "shinyjs")
  packages <- function(field) {
    text <- utils::packageDescription("FielDHub", fields = field)
    trimws(sub("\\s*\\(.*$", "", strsplit(text, ",")[[1L]]))
  }
  expect_identical(app_dependencies(), app)
  expect_setequal(packages("Imports"),
    c("dplyr", "ggplot2", "rlang", "viridisLite", "desplot", app))
  expect_setequal(packages("Suggests"),
    c("mirai", "codetools", "testthat", "spelling", "knitr", "rmarkdown"))
  # Imports installs packages; qualified calls do not need whole-namespace imports.
  expect_length(intersect(names(getNamespaceImports("FielDHub")), app), 0L)
})

test_that("the app dependency guard reports only unavailable packages", {
  expect_null(app_check_dependencies(function(package) TRUE))
  error <- tryCatch(app_check_dependencies(function(package) !package %in% c("DT", "shiny")),
                    fieldhub_dependency_error = identity)
  expect_s3_class(error, "fieldhub_dependency_error")
  expect_identical(error$packages, c("shiny", "DT"))
  expect_match(conditionMessage(error), 'install.packages(c("shiny", "DT"))', fixed = TRUE)
})

test_that("app startup stops with the dependency error before building the app", {
  local_mocked_bindings(app_check_dependencies = function(...) {
    fieldhub_abort("missing", class = "fieldhub_dependency_error", data = list(packages = "DT"))
  })
  expect_error(run_app(launch.browser = FALSE), class = "fieldhub_dependency_error")
})

test_that("app functions do not rely on a whole Shiny namespace import", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("codetools")
  namespace <- asNamespace("FielDHub")
  exports <- getNamespaceExports("shiny")
  names <- c("app_ui", "app_add_external_resources", "run_app",
             ls(namespace, pattern = "^mod_.*_(ui|server)$"))
  used <- lapply(names, function(name) {
    globals <- codetools::findGlobals(get(name, namespace), merge = FALSE)
    intersect(c(globals$functions, globals$variables), exports)
  })
  bad <- lengths(used) > 0L
  expect(!any(bad), sprintf("%s uses unqualified shiny::%s", names[bad][1L],
                            unlist(used[bad])[1L]))
})

test_that("building the UI is not configured through process-global options", {
  # Hard-rule guard: no global options changed. Inspect the body only.
  uses_options <- function(code) {
    if (missing(code) || !is.call(code)) return(FALSE)
    if (identical(code[[1]], as.name("options"))) return(TRUE)
    any(vapply(as.list(code), uses_options, logical(1)))
  }
  expect_false(uses_options(body(app_ui)))
})

# ---- the app object and its resources --------------------------------------

test_that("the app UI builds and registers its bundled resources", {
  for (package in app_dependencies()) skip_if_not_installed(package)
  expect_s3_class(app_ui(), "shiny.tag.list")
  ui <- app_add_external_resources()
  expect_identical(unname(shiny::resourcePaths()[["www"]]),
    normalizePath(app_sys("app/www"), winslash = "/"))
  expect_true(file.exists(app_sys("app/www/favicon.ico")))
  expect_match(htmltools::renderTags(ui)$head, 'href="www/favicon.ico"', fixed = TRUE)
  dependency <- Filter(function(x) identical(x$name, "fieldhub-resources"),
                       htmltools::findDependencies(ui))[[1L]]
  # Served under the package version plus a fingerprint of the files, so a
  # browser drops a stale copy when a file changes
  version <- package_version(dependency$version)
  expect_true(startsWith(dependency$version, paste0(utils::packageVersion("FielDHub"), ".")) &&
                length(unlist(version)) > length(unlist(utils::packageVersion("FielDHub"))))
  expect_s3_class(run_app(launch.browser = FALSE), "shiny.appobj")
})

test_that("the app keeps its Flatly Bootstrap 3 theme", {
  skip_if_not_installed("bslib")
  # The page markup is written for Bootstrap 3
  expect_identical(bslib::theme_version(fieldhub_theme()), "3")
})

test_that("shipped pages are fragments and local assets request no remote resources", {
  # Hard-rule guard: no network. Outbound hyperlinks and XML namespace
  # identifiers do not fetch anything.
  root <- system.file("app/www", package = "FielDHub")
  files <- list.files(root, pattern = "[.](html|css|js)$", full.names = TRUE)
  namespaces <- c(
    "http://schemas.openxmlformats.org/spreadsheetml/2006/main",
    "http://schemas.openxmlformats.org/officeDocument/2006/relationships",
    "http://schemas.openxmlformats.org/package/2006/relationships",
    "http://schemas.openxmlformats.org/package/2006/content-types",
    "http://www.w3.org/XML/1998/namespace")
  remote <- vapply(files, function(file) {
    text <- paste(readLines(file, warn = FALSE), collapse = "\n")
    text <- gsub('<a[^>]*href="https?://[^"]*"[^>]*>', "", text, perl = TRUE)
    for (namespace in namespaces) text <- gsub(namespace, "", text, fixed = TRUE)
    grepl("https?://", text)
  }, logical(1))
  expect(!any(remote), sprintf("%s requests a remote resource", basename(files)[remote][1L]))
  # The Welcome and About pages are pieces of the app page, not documents
  pages <- c(home = as.character(app_home_ui()), about = as.character(app_about_ui()))
  whole <- grepl("<(html|head|body)([ >])|<!DOCTYPE", pages, ignore.case = TRUE)
  expect(!any(whole), sprintf("the %s page is a whole HTML document", names(pages)[whole][1L]))
})

test_that("shipped page images stay small", {
  www <- system.file("app/www", package = "FielDHub")
  images <- c("welcome-banner.jpg", "help-banner.jpg", "about-banner.jpg", "map-diagonal.png",
              "heatmap-yield.svg", paste0("thumb-", c("unreplicated", "prep", "lattice", "other"), ".svg"))
  sizes <- file.size(file.path(www, images))
  expect_true(!anyNA(sizes) && all(sizes < 300 * 1024),
              info = paste(images[is.na(sizes) | sizes >= 300 * 1024], collapse = ", "))
})

# ---- Help, About and Welcome -------------------------------------------------

test_that("every design in Help is an exported function with a reference page and a guide", {
  topics <- fieldhub_help_topics()
  exported <- topics$engine %in% getNamespaceExports("FielDHub")
  expect(all(exported), sprintf("%s is not exported", topics$engine[!exported][1L]))
  # pkgdown names a reference page after its Rd file
  pages <- paste0(sub("[.]html$", "", basename(topics$url)), ".Rd")
  documented <- pages %in% names(tools::Rd_db("FielDHub"))
  expect(all(documented), sprintf("%s has no reference page %s", topics$engine[!documented][1L],
                                  pages[!documented][1L]))
  expect_identical(topics$help, paste0('help("', topics$engine, '", package = "FielDHub")'))
  vignettes <- test_path("..", "..", "vignettes")
  skip_if_not(dir.exists(vignettes), "package sources not available")
  articles <- grepl("/articles/", topics$guide)
  guides <- paste0(sub("[.]html$", "", basename(topics$guide[articles])), ".Rmd")
  found <- file.exists(file.path(vignettes, guides))
  expect(all(found), sprintf("guide %s has no vignette", guides[!found][1L]))
})

test_that("About shows the package facts, the team and the current year", {
  html <- as.character(app_about_ui())
  description <- utils::packageDescription("FielDHub")
  persons <- eval(parse(text = description[["Authors@R"]]))
  shown <- Filter(function(p) !"cph" %in% p$role, persons)
  names <- vapply(shown, function(p) paste(c(p$given, p$family), collapse = " "), "")
  names <- setdiff(names, "Jean-Marc Montpetit")
  facts <- c(description$Version, sub(" [+] file LICENSE$", "", description$License),
             description$BugReports, '<code>citation("FielDHub")</code>', "joss.03122",
             fieldhub_unit()$name, format(Sys.Date(), "%Y"), names)
  expect_identical(missing_texts(html, facts), character())
  expect_false(grepl("Jean-Marc Montpetit|jeanmarc[.]montpetit@videotron[.]ca|[+] file LICENSE", html))
})

test_that("the Welcome page links every design to its navbar tab", {
  registry <- fieldhub_app_registry()
  html <- as.character(app_home_ui(registry))
  targets <- gsub('^data-fieldhub-tab="|"$', "",
                  regmatches(html, gregexpr('data-fieldhub-tab="[^"]*"', html))[[1]])
  labels <- vapply(registry, `[[`, "", "label")
  # every design once, plus Help and the photo credits on About
  expect_identical(sort(targets), sort(c(htmltools::htmlEscape(labels), "Help", "About Us")))
  images <- unique(gsub('^src="|"$', "", regmatches(html, gregexpr('src="www/[^"]+"', html))[[1]]))
  found <- file.exists(vapply(images, function(image) system.file("app", image, package = "FielDHub"), ""))
  expect(length(images) > 0L && all(found), sprintf("image %s is not shipped", images[!found][1L]))
})

# ---- uploads and empty choices -----------------------------------------------

test_that("uploads read with a BOM in the C locale and explain missing columns", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("ENTRY", "1", "2"), path)
  error <- tryCatch(read_design_upload(path, ",", "alpha"), error = identity)
  expect_s3_class(error, "fieldhub_input_error")
  expect_match(conditionMessage(error), "ENTRY and NAME", fixed = TRUE)
  # Regression: a UTF-8 BOM stuck to the first column name
  previous <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", previous), add = TRUE)
  invisible(Sys.setlocale("LC_CTYPE", "C"))
  writeBin(c(as.raw(c(239, 187, 191)), charToRaw("ENTRY,NAME\n1,G1\n2,G2\n")), path)
  expect_identical(names(read_design_upload(path, ",", "alpha")$data), c("ENTRY", "NAME"))
})

test_that("empty dimensions do not suggest an already-enabled filler option", {
  step <- list(label = "Dimensions for location", type = "location_selects")
  error <- tryCatch(check_step_choices(step, list(character()), allow_fillers = TRUE), error = identity)
  expect_s3_class(error, "fieldhub_input_error")
  expect_match(conditionMessage(error), "Change the entries or checks", fixed = TRUE)
  expect_false(grepl("Allow filler plots", conditionMessage(error), fixed = TRUE))
})

# ---- loading feedback --------------------------------------------------------

test_that("task buttons, task regions and output loaders are announced to assistive technology", {
  skip_if_not_installed("shiny")
  button <- as.character(app_task_button("prep-run", "Run!", results_id = "prep-results"))
  expect_identical(missing_texts(button, c('data-fieldhub-task="prep-run"',
                                           'aria-controls="prep-results"')), character())
  panel <- as.character(app_task_feedback(c("prep-run", "prep-randomize"), shiny::div(id = "plot")))
  expect_identical(missing_texts(panel, c('data-fieldhub-tasks="prep-run prep-randomize"',
                                          'role="status"', 'aria-live="polite"',
                                          'aria-atomic="true"', 'id="plot"')), character())
  output <- shiny::imageOutput("page-layout", width = NULL, height = NULL)
  wrapper <- app_output_feedback(output)
  html <- as.character(wrapper)
  expect_identical(wrapper$attribs[["aria-busy"]], "false")
  expect_identical(missing_texts(html, c('role="status"', 'aria-live="polite"', 'id="page-layout"',
                                         "Loading")), character())
})

test_that("task feedback messages are namespaced and explicitly clear terminal states", {
  messages <- list()
  session <- list(ns = function(id) paste0("prep-", id),
    sendCustomMessage = function(type, message) messages[[length(messages) + 1L]] <<- list(type, message))
  app_report_task_feedback(session, "run", TRUE, "Optimizing allocation...")
  app_report_task_feedback(session, "run", FALSE)
  expect_identical(messages, list(
    list("fieldhub-task-feedback", list(id = "prep-run", busy = TRUE, message = "Optimizing allocation...")),
    list("fieldhub-task-feedback", list(id = "prep-run", busy = FALSE, message = ""))))
})

# ---- problem wording ---------------------------------------------------------

test_that("problem_message() shows FielDHub conditions verbatim and flags other errors", {
  warning <- tryCatch(fieldhub_warn("Using plotNumber 1001.", class = "fieldhub_default_warning"),
                      warning = function(w) w)
  cases <- list(
    list(input_error(), "Starting Plot Number cannot be blank.", "error"),
    list(simpleError("missing value where TRUE/FALSE needed"),
         "Unexpected problem: missing value where TRUE/FALSE needed", "error"),
    list(warning, "Using plotNumber 1001.", "warning"),
    list(simpleWarning("NAs introduced by coercion"),
         "Unexpected warning: NAs introduced by coercion", "warning"),
    list("Blocks should have the same size.", "Blocks should have the same size.", "error"))
  got <- t(vapply(cases, function(case) c(problem_message(case[[1]]), problem_severity(case[[1]])),
                  character(2)))
  expect_identical(got, do.call(rbind, lapply(cases, function(case) c(case[[2]], case[[3]]))))
  expect_error(problem_message(42), class = "fieldhub_input_error")
})

test_that("validate_design() turns any error into a validation message and lets req() pass", {
  skip_if_not_installed("shiny")
  # An unexpected error is also logged to the R console, never lost
  expect_message(
    unexpected <- tryCatch(validate_design(stop(simpleError("missing value where TRUE/FALSE needed"))),
                           error = function(e) e),
    "FielDHub: unexpected problem: missing value where TRUE/FALSE needed", fixed = TRUE
  )
  expect_s3_class(unexpected, c("validation", "shiny.silent.error"))
  expect_identical(conditionMessage(unexpected),
                   "Unexpected problem: missing value where TRUE/FALSE needed")
  # A FielDHub condition is the user's input problem: not logged
  expect_silent(
    classed <- tryCatch(validate_design(fieldhub_abort("Starting Plot Number cannot be blank.")),
                        error = function(e) e)
  )
  expect_s3_class(classed, "shiny.silent.error")
  expect_identical(conditionMessage(classed), "Starting Plot Number cannot be blank.")
  silent <- tryCatch(validate_design(shiny::req(FALSE)), error = function(e) e)
  expect_s3_class(silent, "shiny.silent.error")
  expect_identical(conditionMessage(silent), "")
  expect_identical(validate_design(1 + 1), 2)
})

test_that("validate_design() and app_report_problem() word the same condition the same way", {
  skip_if_not_installed("shiny")
  conditions <- list(input = input_error(), unexpected = simpleError("subscript out of bounds"))
  words <- vapply(conditions, function(condition) {
    shown <- suppressMessages(tryCatch(validate_design(stop(condition)), error = conditionMessage))
    reported <- NULL
    app_report_problem(condition, notify = function(message, ...) reported <<- message)
    c(shown, reported)
  }, character(2))
  expect_identical(words[1, ], words[2, ])
})

test_that("app_report_problem() passes the message, severity and title to its presenter", {
  calls <- list()
  notify <- function(...) calls[[length(calls) + 1L]] <<- list(...)
  out <- app_report_problem(input_error("Too few entries."), notify = notify)
  expect_identical(out, "Too few entries.")
  app_report_problem("By unchecking this option only the checks are randomized.",
                     severity = "warning", notify = notify)
  app_report_problem("No field was found.", title = "No field dimensions available",
                     notify = notify)
  expect_identical(calls, list(
    list("Too few entries.", severity = "error", title = NULL),
    list("By unchecking this option only the checks are randomized.", severity = "warning", title = NULL),
    list("No field was found.", severity = "error", title = "No field dimensions available")))
})

# ---- warnings and failures of app work ---------------------------------------

test_that("app_capture_conditions() returns the value and the FielDHub warnings", {
  out <- app_capture_conditions({
    fieldhub_warn("plotNumber has the wrong length; using 1001.",
                  class = "fieldhub_default_warning")
    fieldhub_warn("Only one location.", class = "fieldhub_design_warning")
    "design"
  })
  expect_identical(out$value, "design")
  expect_identical(lapply(out$warnings, function(w) class(w)[1L]),
                   list("fieldhub_default_warning", "fieldhub_design_warning"))
  # A real engine warning is captured instead of reaching the console
  expect_warning(
    out <- app_capture_conditions(RCBD(t = 4, reps = 2, l = 2, plotNumber = 101, seed = 1)),
    NA
  )
  expect_s3_class(out$value, "FielDHub")
  expect_s3_class(out$warnings[[1]], "fieldhub_default_warning")
  # Other warnings are not FielDHub's to explain: they keep R's own handling
  expect_warning(out <- app_capture_conditions({warning("plain"); 1}), "plain")
  expect_identical(out[c("value", "warnings")], list(value = 1, warnings = list()))
})

test_that("validate_design() shows FielDHub warnings as notices, also before a failure", {
  skip_if_not_installed("shiny")
  noticed <- list()
  notice <- function(problem) noticed[[length(noticed) + 1L]] <<- problem_message(problem)
  value <- validate_design({
    fieldhub_warn("Using 1001.", class = "fieldhub_default_warning")
    3
  }, notice = notice)
  expect_identical(value, 3)
  err <- tryCatch(validate_design({
    fieldhub_warn("Using 1002.", class = "fieldhub_default_warning")
    fieldhub_abort("Too few entries.")
  }, notice = notice), error = function(e) e)
  expect_identical(conditionMessage(err), "Too few entries.")
  expect_identical(noticed, list("Using 1001.", "Using 1002."))
})

test_that("app_attempt() reports errors and warnings of work with no output", {
  reported <- list()
  report <- function(problem) reported[[length(reported) + 1L]] <<- problem_message(problem)
  expect_message(expect_null(app_attempt(stop("boom"), report = report)),
                 "FielDHub: unexpected problem", fixed = TRUE)
  value <- app_attempt({
    fieldhub_warn("Few plots.", class = "fieldhub_design_warning")
    "ok"
  }, report = report)
  expect_identical(value, "ok")
  # the warnings raised before a failure are reported with the error
  expect_null(app_attempt({
    fieldhub_warn("Using 1001.", class = "fieldhub_default_warning")
    fieldhub_abort("Too few entries.")
  }, report = report))
  expect_identical(reported, list("Unexpected problem: boom", "Few plots.", "Using 1001.",
                                  "Too few entries."))
  skip_if_not_installed("shiny")
  silent <- tryCatch(app_attempt(shiny::req(FALSE), report = report), error = function(e) e)
  expect_s3_class(silent, "shiny.silent.error")
  expect_length(reported, 4L)
})

test_that("app_attempt(fail = \"validate\") keeps the failure as the design's state", {
  skip_if_not_installed("shiny")
  reported <- list()
  report <- function(problem) reported[[length(reported) + 1L]] <<- problem_message(problem)
  failed <- tryCatch(app_attempt(fieldhub_abort("Too few entries."), report = report,
                                 fail = "validate"),
                     error = function(e) e)
  expect_s3_class(failed, "shiny.silent.error")
  expect_identical(conditionMessage(failed), "Too few entries.")
  expect_identical(reported, list("Too few entries."))
  # A design reactive that failed this way is explained in its panels
  state <- app_design_state(function() app_attempt(fieldhub_abort("Too few entries."),
                                                   report = report, fail = "validate"))
  expect_identical(c(plot_state_message(state, NULL, "layout"), plot_state_message(state, NULL, "heatmap")),
                   c("Too few entries.", "Too few entries."))
})

test_that("validate_design(report = TRUE) also reports unexpected errors, for observers", {
  skip_if_not_installed("shiny")
  noticed <- list()
  notice <- function(problem) noticed[[length(noticed) + 1L]] <<- problem_message(problem)
  expect_message(
    err <- tryCatch(validate_design(stop("subscript out of bounds"), report = TRUE,
                                    notice = notice),
                    error = function(e) e),
    "unexpected problem", fixed = TRUE
  )
  expect_s3_class(err, "shiny.silent.error")
  # FielDHub input problems in an observer keep the selector quiet, and
  # without report an unexpected error is logged but not reported
  tryCatch(validate_design(fieldhub_abort("Checks cannot be blank."), report = TRUE, notice = notice),
           error = function(e) e)
  suppressMessages(tryCatch(validate_design(stop("boom"), notice = notice), error = identity))
  expect_identical(noticed, list("Unexpected problem: subscript out of bounds"))
})

# ---- dialogs and notices -----------------------------------------------------

test_that("problems use escaped native modals, notifications and one notice gate per session", {
  modals <- notices <- list()
  local_mocked_bindings(
    showModal = function(ui, session) modals[[length(modals) + 1L]] <<- list(ui = ui, session = session),
    showNotification = function(ui, ..., session) notices[[length(notices) + 1L]] <<- list(ui = ui, session = session),
    .package = "shiny"
  )
  session <- list(userData = new.env(parent = emptyenv()))
  other <- list(userData = new.env(parent = emptyenv()))
  app_present_problem("<script>bad()</script>", "error", session = session)
  app_present_problem("Read the instructions.", "info", title = "Details", session = session)
  expect_identical(lapply(modals, `[[`, "session"), list(session, session))
  expect_match(as.character(modals[[1L]]$ui), "&lt;script&gt;bad()&lt;/script&gt;", fixed = TRUE)
  expect_match(as.character(modals[[1L]]$ui), 'data-dismiss="modal"', fixed = TRUE)
  expect_match(as.character(modals[[2L]]$ui), "Details", fixed = TRUE)
  app_present_problem("Few plots.", "warning", session = session)
  expect_identical(lapply(notices, `[[`, "session"), list(session))
  # The same unexpected problem opens one dialog in each session
  app_present_problem("Unexpected problem: test", session = session)
  app_present_problem("Unexpected problem: test", session = session)
  app_present_problem("Unexpected problem: test", session = other)
  expect_identical(lapply(modals, `[[`, "session"), list(session, session, session, other))
  expect_message(app_present_problem("No session.", "info", session = NULL), "Information: No session.")
  expect_warning(app_present_problem("No session.", "warning", session = NULL), "Warning: No session.")
})

# ---- empty states --------------------------------------------------------------

test_that("the p-rep modules share the no-dimensions explanation", {
  without <- prep_no_dimensions_problem(FALSE, 10)
  with <- prep_no_dimensions_problem(TRUE, 7)
  expect_identical(c(without$severity, without$title, with$severity),
                   c("info", "Filler plots required", "error"))
  expect_match(without$message, "no more than 10 filler plots", fixed = TRUE)
  expect_match(with$message, "7 or fewer filler plots", fixed = TRUE)
})

test_that("plot_state_message() explains each empty plot state", {
  design <- structure(list(fieldBook = data.frame(PLOT = 1)), class = "FielDHub")
  cases <- list(
    list(NULL, NULL, "layout", "Generate the design to see the field layout."),
    list(NULL, NULL, "heatmap", "Generate the design to see the heatmap."),
    list(design, NULL, "layout", NA_character_),
    list(design, NULL, "heatmap", "Simulate data to see the heatmap."),
    list(design, list(response_name = "YIELD"), "heatmap", NA_character_),
    list(input_error("Too few entries."), NULL, "layout", "Too few entries."),
    list(simpleError("boom"), NULL, "heatmap", "Unexpected problem: boom"))
  got <- vapply(cases, function(case) plot_state_message(case[[1]], case[[2]], case[[3]]) %||% NA_character_, "")
  expect_identical(got, vapply(cases, `[[`, "", 4L))
  expect_error(plot_state_message(design, NULL, "table"), class = "fieldhub_input_error")
})

test_that("app_plot_state() shows the explanation as a validation message", {
  skip_if_not_installed("shiny")
  err <- tryCatch(app_plot_state(NULL, NULL, "layout"), error = function(e) e)
  expect_s3_class(err, "shiny.silent.error")
  expect_identical(conditionMessage(err), "Generate the design to see the field layout.")
  design <- structure(list(fieldBook = data.frame(PLOT = 1)), class = "FielDHub")
  expect_null(app_plot_state(design, NULL, "layout"))
})

test_that("app_design_state() reads a design that has not run as NULL, a failed one as its condition", {
  skip_if_not_installed("shiny")
  expect_null(app_design_state(function() shiny::req(FALSE)))
  expect_identical(app_design_state(function() "design"), "design")
  failed <- app_design_state(function() shiny::validate("Too few entries."))
  expect_s3_class(failed, "validation")
  # A validation message is already written for the user: shown verbatim
  expect_identical(plot_state_message(failed, NULL, "layout"), "Too few entries.")
})
