#' Result tabs of a spatial design page
#'
#' @description The main panel of a spatial page
#' (\code{mod_design_ui()} with a \code{kind = "spatial"} spec): "Get
#' Random" (the steps offered after Run!, Randomize!, what the run found and
#' the page's \code{setup} view), "Data Input" (the entry tables), the
#' spec's \code{panels} (the randomize steps above the first), "Field Book"
#' and "Heatmap", with the ids of the page's spatial workflow
#' (\code{spatial_workflow_spec()}).
#'
#' @param ns The page's namespace function.
#' @param spec The page spec (\code{design_app_spec()}).
#' @noRd
app_spatial_tabs <- function(ns, spec) {
  ids <- spec$workflow$ids
  step_ui <- function(step) {
    shinyjs::hidden(shiny::div(id = ns(paste0(step$id, "_step")), app_control_ui(step, ns)))
  }
  steps <- function(stage) lapply(Filter(function(step) identical(step$stage, stage), spec$steps), step_ui)
  setup <- if (identical(spec$setup$type, "summary")) {
    shiny::div(class = "fieldhub-design-summary",
      fieldhub_spinner(shiny::verbatimTextOutput(ns("setup"), placeholder = FALSE), type = 4))
  } else {
    fieldhub_spinner(DT::DTOutput(ns("setup"), width = NULL, height = NULL), type = 4)
  }
  entries <- lapply(seq_along(spec$entries), function(i) {
    DT::DTOutput(ns(paste0("entries_", i)), width = NULL, height = NULL)
  })
  if (length(entries) == 2L) {
    entries <- shiny::fluidRow(shiny::column(6, entries[[1L]]), shiny::column(6, entries[[2L]]))
  }
  panels <- lapply(seq_along(spec$panels), function(i) {
    panel <- spec$panels[[i]]
    view <- if (identical(panel$type, "plot")) {
      shiny::plotOutput(ns(panel$id), width = NULL, height = NULL)
    } else {
      DT::DTOutput(ns(panel$id), width = NULL, height = NULL)
    }
    shiny::tabPanel(panel$title, shiny::br(), if (i == 1L) steps("randomize"), view)
  })
  tabs <- c(
    list(
      id = ns(ids[["tabset"]]),
      shiny::tabPanel("Get Random", value = "setup",
        shinyjs::useShinyjs(),
        shiny::br(),
        steps("run"),
        shinyjs::hidden(shiny::actionButton(ns("randomize"), "Randomize!")),
        shiny::br(), shiny::br(),
        shiny::uiOutput(ns("status")),
        setup),
      shiny::tabPanel("Data Input", entries)
    ),
    panels,
    list(
      shiny::tabPanel("Field Book",
        fieldhub_spinner(DT::DTOutput(ns(ids[["table"]]), width = NULL, height = NULL), type = 5)),
      shiny::tabPanel("Heatmap", shiny::div(class = "fieldhub-spatial-heatmap",
        fieldhub_spinner(plotly::plotlyOutput(ns(ids[["heatmap"]]), width = NULL, height = NULL),
                         type = 5)))
    )
  )
  do.call(shiny::tabsetPanel, tabs)
}

#' Run the steps and results of a spatial design page
#'
#' @description Called by \code{mod_design_server()} for a
#' \code{kind = "spatial"} spec, with the page's Run! (the parsed controls,
#' the upload and the seed). Where the spec has an \code{optim} step, Run!
#' also computes the allocation through its argument builder. The run's
#' \code{"run"} steps (field sizes, from plain choice functions) are offered
#' next; each Randomize! reads them, offers the \code{"randomize"} steps (the
#' percentage of checks) and builds the design through the spec's argument
#' builder. Results, simulation, exports and reproduction come from the
#' shared spatial workflow (\code{app_spatial_workflow()}).
#'
#' @param input,output,session The page's module arguments.
#' @param spec The page spec.
#' @param run The page's Run! reactive: \code{list(values = , data = )}.
#' @param raw_controls Function returning the raw values of the given
#'   control ids.
#' @return The page's design, run choices and randomize choices
#'   (reactives), invisibly.
#' @noRd
app_spatial_page <- function(input, output, session, spec, run, raw_controls) {
  ids <- spec$workflow$ids
  ns <- session$ns
  stage <- function(controls, name) Filter(function(control) identical(control$stage, name), controls)
  offered <- function(steps) {
    steps <- Filter(function(step) !is.null(step$options), steps)
    stats::setNames(steps, vapply(steps, `[[`, character(1), "id"))
  }
  run_steps <- stage(spec$steps, "run")
  design_steps <- stage(spec$steps, "randomize")
  views <- stage(spec$controls, "run")
  live_ids <- unique(unlist(lapply(run_steps, `[[`, "depends_on")))
  in_progress <- function(message, expr) {
    if (isTRUE(spec$long_running)) shiny::withProgress(message = message, expr) else expr
  }
  # A reactive's value, or a silent stop while it has not run or failed
  # (the status panel explains a failure)
  settled <- function(reactive) {
    state <- app_design_state(reactive)
    shiny::req(!is.null(state), !inherits(state, "condition"))
    state
  }

  # Run!: the inputs of the run, with the design's allocation where it has one
  prepared <- shiny::reactive({
    inputs <- run()
    if (!is.null(spec$optim)) {
      inputs$values[[spec$optim$into]] <- in_progress("Optimization in progress ...", validate_design(
        do.call(spec$optim$engine, spec$optim$args(inputs$values, inputs$data))
      ))
    }
    inputs
  }) |>
    shiny::bindEvent(input$run)
  # The run's values, with the controls the steps follow read as they change
  staged <- shiny::reactive({
    inputs <- prepared()
    if (length(live_ids) > 0L) {
      live <- validate_design(read_design_controls(spec, raw_controls(live_ids), only = live_ids))
      inputs$values[names(live)] <- live
    }
    inputs
  })
  run_choices <- shiny::reactive({
    inputs <- staged()
    in_progress("Getting field dimensions ...", lapply(offered(run_steps), function(step) {
      validate_design(design_step_choices(step, inputs$values, inputs$data))
    }))
  })

  randomized <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(input$run, {
    randomized(FALSE)
    for (step in spec$steps) shinyjs::hide(paste0(step$id, "_step"))
    shinyjs::hide("randomize")
    shiny::updateTabsetPanel(session, ids[["tabset"]], selected = "setup")
  })
  if (!is.null(spec$upload)) {
    toggle <- app_upload_spec(spec$upload)$toggle
    shiny::observeEvent(input[[toggle]], {
      shiny::updateTabsetPanel(session, ids[["tabset"]], selected = "setup")
    })
  }
  shiny::observeEvent(prepared(), {
    for (control in views) {
      options <- validate_design(design_step_choices(control, prepared()$values, prepared()$data),
                                 report = TRUE)
      shiny::updateSelectInput(session, control$id, choices = options$choices,
                               selected = options$selected)
    }
  })
  shiny::observeEvent(run_choices(), {
    choices <- run_choices()
    for (step in offered(run_steps)) {
      if (identical(step$type, "dependent_select")) {
        shiny::updateSelectInput(session, step$id, choices = choices[[step$id]]$choices,
                                 selected = choices[[step$id]]$selected)
      }
    }
    for (step in run_steps) shinyjs::show(paste0(step$id, "_step"))
    shinyjs::show("randomize")
  })
  for (step in Filter(function(step) identical(step$type, "location_selects"), run_steps)) local({
    step <- step
    output[[step$id]] <- shiny::renderUI({
      options <- run_choices()[[step$id]]
      shiny::tagList(lapply(seq_along(options$choices), function(i) {
        shiny::selectInput(ns(paste0(step$id, "_", i)), paste(step$label, i),
                           choices = options$choices[[i]], selected = options$selected[[i]])
      }))
    })
  })
  # A new field size hides the results of the last one
  for (step in run_steps) local({
    id <- step$id
    shiny::observeEvent(input[[id]], randomized(FALSE), ignoreInit = TRUE)
  })
  step_values <- function(steps, choices) {
    raw <- list()
    for (step in steps) {
      raw[step$id] <- list(if (identical(step$type, "location_selects")) {
        lapply(seq_along(choices[[step$id]]$choices), function(i) input[[paste0(step$id, "_", i)]])
      } else {
        input[[step$id]]
      })
    }
    raw
  }

  # Randomize!: the steps of the run, then those of the field
  shiny::observeEvent(input$randomize, randomized(TRUE))
  randomized_inputs <- shiny::reactive({
    inputs <- staged()
    choices <- run_choices()
    # A select that does not hold one of its new choices yet is waited for
    inputs$values <- shiny::req(validate_design(
      read_design_steps(run_steps, step_values(run_steps, choices), inputs$values, choices)
    ))
    inputs
  }) |>
    shiny::bindEvent(input$randomize)
  design_choices <- shiny::reactive({
    inputs <- randomized_inputs()
    lapply(offered(design_steps), function(step) {
      validate_design(design_step_choices(step, inputs$values, inputs$data))
    })
  })
  shiny::observeEvent(design_choices(), {
    choices <- design_choices()
    for (step in offered(design_steps)) {
      shiny::updateSelectInput(session, step$id, choices = choices[[step$id]]$choices,
                               selected = choices[[step$id]]$selected)
    }
    for (step in design_steps) shinyjs::show(paste0(step$id, "_step"))
  })
  design_inputs <- shiny::reactive({
    inputs <- randomized_inputs()
    if (length(design_steps) > 0L) {
      choices <- design_choices()
      inputs$values <- shiny::req(validate_design(
        read_design_steps(design_steps, step_values(design_steps, choices), inputs$values, choices)
      ))
    }
    inputs
  })
  # The design comes from the engine through the spec's argument builder,
  # as a direct API call with the same values builds it
  design <- shiny::reactive({
    inputs <- design_inputs()
    in_progress(spec$randomizing, validate_design(
      spec$accept(do.call(spec$engine, spec$args(inputs$values, inputs$data)))
    ))
  })
  shiny::observeEvent(randomized(), {
    if (randomized()) shinyjs::show(ids[["download"]]) else shinyjs::hide(ids[["download"]])
  })
  location <- shiny::reactive({
    selected <- suppressWarnings(as.numeric(input$location_view))
    shiny::req(length(selected) == 1L, is.finite(selected), selected >= 1,
               selected <= length(location_view_choices(design_inputs()$values$l)))
    selected
  })

  # What the run found: its problems, and the page's setup view
  output$status <- shiny::renderUI({
    prepared()
    run_choices()
    NULL
  })
  if (identical(spec$setup$type, "summary")) {
    output$setup <- shiny::renderPrint({
      if (!randomized()) return(invisible(NULL))
      shiny::req(design())
      cat("Randomization was successful!", "\n", "\n")
      print(design(), n = 6)
    })
  } else {
    output$setup <- DT::renderDT({
      if (identical(spec$setup$stage, "randomize")) {
        if (!randomized()) return(NULL)
        inputs <- randomized_inputs()
        choices <- design_choices()
      } else {
        inputs <- settled(prepared)
        choices <- settled(run_choices)
      }
      app_spatial_table(validate_design(spec$setup$view(inputs$values, choices)),
                        caption = spec$setup$caption, design = inputs$values[[spec$optim$into]],
                        export = spec$setup$export)
    })
  }
  for (i in seq_along(spec$entries)) local({
    entries <- spec$entries[[i]]
    output[[paste0("entries_", i)]] <- DT::renderDT({
      if (!randomized()) return(NULL)
      table <- validate_design(entries(design(), location(), design_inputs()$values))
      app_spatial_table(table$data, caption = table$caption, height = table$height,
                        filter = table$filter)
    })
  })
  for (i in seq_along(spec$panels)) local({
    panel <- spec$panels[[i]]
    first <- i == 1L
    draw <- function() {
      # Explain a design that has not been randomized (or failed)
      if (first) app_plot_state(if (randomized()) app_design_state(design), NULL, "layout")
      if (!randomized()) return(NULL)
      validate_design(panel$view(design(), location(), design_inputs()$values))
    }
    output[[panel$id]] <- if (identical(panel$type, "plot")) {
      shiny::renderPlot(draw(), height = 620, res = 100)
    } else {
      DT::renderDT({
        view <- draw()
        if (is.null(view)) return(NULL)
        app_spatial_grid(view, design(), panel$export, location())
      })
    }
  })

  app_spatial_workflow(input, output, session,
    design = function() design(),
    seed = function() run()$values$seed,
    dimensions = function(field_book) spec$field_size(design(), design_inputs()$values),
    selected = function() location(),
    filename = function() {
      names <- paste(run()$values$location_names, collapse = ",")
      paste0(names, "_", spec$file_tag, Sys.Date(), ".csv")
    },
    visible = function() randomized(),
    simulation_ready = function() {
      shiny::req(design()$fieldBook)
      isTRUE(randomized())
    },
    book_ready = function() shiny::req(design()$fieldBook),
    spec = spec$workflow
  )
  invisible(list(design = design, run_choices = run_choices, design_choices = design_choices))
}

#' A table of a spatial page (entry list, allocation, check options)
#'
#' @param data The data frame.
#' @param caption Optional caption.
#' @param height Scroll height.
#' @param filter \code{"top"} to filter by column, \code{"none"}.
#' @param design Optional result whose metadata the export buttons carry.
#' @param export Optional name of the exported table (adds export buttons).
#' @noRd
app_spatial_table <- function(data, caption = NULL, height = "600px", filter = "none",
                              design = NULL, export = NULL) {
  options <- list(pageLength = nrow(data), autoWidth = FALSE, scrollX = TRUE, scrollY = height,
                  columnDefs = list(list(className = "dt-center", targets = "_all")))
  if (!is.null(export)) {
    options$dom <- "Bfrtip"
    options$buttons <- app_table_export_buttons(design, export, print = TRUE)
  }
  DT::datatable(data, caption = caption, filter = filter, rownames = !is.null(export),
                extensions = if (!is.null(export)) "Buttons" else list(), options = options)
}

#' A field grid of a spatial page, its highlighted values coloured
#'
#' @param view A \code{field_grid_view()}.
#' @param design The design (its metadata goes with the exports).
#' @param export Name of the exported table.
#' @param location The location shown.
#' @noRd
app_spatial_grid <- function(view, design, export, location) {
  data <- view$data
  table <- DT::datatable(data, extensions = "Buttons", options = list(
    dom = "Blfrtip", autoWidth = FALSE, scrollX = TRUE, fixedColumns = TRUE,
    pageLength = nrow(data), scrollY = "600px", class = "compact cell-border stripe",
    rownames = FALSE, server = FALSE,
    filter = list(position = "top", clear = FALSE, plain = TRUE),
    buttons = app_table_export_buttons(design, export, location),
    lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
  ))
  if (length(view$highlight) == 0L) return(table)
  DT::formatStyle(table, colnames(data),
                  backgroundColor = DT::styleEqual(view$highlight, view$colours))
}
