#' One design page, rendered from its spec
#'
#' @description The sidebar is the page's upload controls
#' (\code{app_upload_ui()}), its controls (\code{app_control_ui()}) and the
#' Run!/Simulate!/Save buttons. The main panel of a classic page shows the
#' field layout and field book (and a design summary where the spec asks
#' for one), with the output ids of the page's workflow
#' (\code{classic_workflow_spec()}); a spatial page shows its steps and
#' result tabs (\code{app_spatial_tabs()}).
#'
#' @param id The page's module id.
#' @param spec The page spec (\code{design_app_spec()}).
#' @noRd
mod_design_ui <- function(id, spec) {
  ns <- shiny::NS(id)
  ids <- spec$workflow$ids
  toggle <- if (!is.null(spec$upload)) app_upload_spec(spec$upload)$toggle
  spatial <- identical(spec$kind, "spatial")
  feedback <- function(...) app_task_feedback(ns("run"), ...)
  # A spatial page offers its experiment once it is randomized
  save <- if (spatial) {
    shinyjs::hidden(shiny::downloadButton(ns(ids[["download"]]), "Save experiment (ZIP)",
                                          class = "btn-block"))
  } else {
    shiny::downloadButton(ns(ids[["field_book_download"]]), "Save experiment (ZIP)",
                          class = "btn-block")
  }
  tabs <- if (!spatial) list(
    if (isTRUE(spec$summary)) shiny::tabPanel(
      "Summary Design",
      feedback(
        shiny::br(),
        shiny::div(class = "fieldhub-design-summary",
          fieldhub_spinner(shiny::verbatimTextOutput(ns("summary"), placeholder = FALSE), type = 4)))
    ),
    shiny::tabPanel(
      "Field Layout",
      feedback(
        shinyjs::useShinyjs(),
        shinyjs::hidden(shiny::downloadButton(
          ns(ids[["layout_download"]]), label = "CSV + metadata (ZIP)",
          icon = shiny::icon("download"), class = "fieldhub-csv-download"
        )),
        shiny::div(class = "fieldhub-design-plot",
          fieldhub_spinner(plotly::plotlyOutput(ns(ids[["plot"]]), width = NULL, height = NULL),
                           type = 5)),
        shiny::br(),
        shiny::column(12, shiny::uiOutput(ns(spec$layout$output))))
    ),
    shiny::tabPanel(
      "Field Book",
      feedback(fieldhub_spinner(DT::DTOutput(ns(ids[["table"]]), width = NULL, height = NULL), type = 5))
    )
  )
  shiny::tagList(
    shiny::h4(spec$title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        if (!is.null(spec$upload)) app_upload_ui(ns, spec$upload),
        lapply(spec$controls, app_control_ui, ns = ns, toggle = toggle),
        shiny::fluidRow(
          shiny::column(6, app_task_button(
            ns("run"), "Run!", icon = shiny::icon("circle-nodes", verify_fa = FALSE),
            class = "btn-block", results_id = ns("results"),
            busy_message = if (!is.null(spec$optim)) "Optimizing allocation..." else
              if (spatial) "Preparing field dimensions..." else spec$busy_message
          )),
          shiny::column(6, shiny::actionButton(
            ns(ids[["simulate"]]), "Simulate!",
            icon = shiny::icon("greater-than-equal", verify_fa = FALSE), class = "btn-block"
          ))
        ),
        shiny::br(),
        save
      ),
      shiny::mainPanel(
        width = 8,
        shiny::div(id = ns("results"), shiny::fluidRow(if (spatial) {
          app_spatial_tabs(ns, spec)
        } else {
          do.call(shiny::tabsetPanel, Filter(Negate(is.null), tabs))
        }))
      )
    )
  )
}

#' Run one design page from its spec
#'
#' @description Run! reads the controls (\code{read_design_controls()}) and
#' the upload (\code{app_read_upload()}), builds the builder's \code{values}
#' with the spec and resolves the seed (\code{app_design_seed()}). A classic
#' page then calls the engine only through the spec's argument builder, so
#' the page and a direct API call build the same design, and its results,
#' simulation, exports and reproduction come from the shared workflow
#' (\code{app_classic_layout()}, \code{app_classic_workflow()}). A spatial
#' page goes on to its steps (\code{app_spatial_page()}).
#'
#' @inheritParams mod_design_ui
#' @noRd
mod_design_server <- function(id, spec) {
  shiny::moduleServer(id, function(input, output, session) {
    workflow <- spec$workflow
    controls <- Filter(function(control) !is.null(control$parse), spec$controls)
    raw_controls <- function(ids = vapply(controls, `[[`, character(1), "id")) {
      stats::setNames(lapply(ids, function(control_id) input[[control_id]]), ids)
    }
    uploaded <- function() {
      !is.null(spec$upload) && identical(input[[app_upload_spec(spec$upload)$toggle]], "Yes")
    }
    # The shaped upload, or NULL when reading it failed (already reported)
    upload <- shiny::reactive({
      # A page may let the entries of an upload repeat (a flag control)
      repeats <- !is.null(spec$upload_repeats) && isTRUE(input[[spec$upload_repeats]])
      file <- app_read_upload(input, spec$upload, check = !repeats)
      if (is.null(file)) return(NULL)
      validate_design(spec$upload_shape(file$data))
    })
    if (!is.null(spec$upload)) app_upload_dialog_observer(input, spec$upload)

    # Selects whose choices follow other controls or the uploaded entries
    live <- Filter(function(control) !is.null(control$options) && identical(control$stage, "live"),
                   controls)
    for (control in live) local({
      control <- control
      dependency <- shiny::reactive({
        list(raw = raw_controls(control$depends_on),
             data = if (uploaded()) shiny::req(upload()))
      })
      shiny::observeEvent(dependency(), {
        options <- validate_design(
          design_control_choices(spec, control, dependency()$raw, dependency()$data), report = TRUE
        )
        shiny::updateSelectInput(session, control$id, label = control$label,
                                 choices = options$choices, selected = options$selected)
      })
    })
    # Notes computed from the other controls
    for (control in Filter(function(control) !is.null(control$preview), spec$controls)) local({
      control <- control
      output[[control$id]] <- shiny::renderUI({
        note <- validate_design(control$preview(raw_controls(), uploaded()))
        if (!is.null(note)) shiny::helpText(note)
      })
    })

    run <- shiny::reactive({
      data <- if (uploaded()) validate_design(design_upload_data(upload()))
      parsed <- validate_design(read_design_controls(spec, raw_controls(), uploaded = !is.null(data)))
      data <- spec$data(data, parsed)
      values <- validate_design(spec$values(parsed, data))
      values$seed <- validate_design(app_design_seed(values$seed))
      list(values = values, data = data)
    }) |>
      shiny::bindEvent(input$run)

    if (identical(spec$kind, "spatial")) {
      return(app_spatial_page(input, output, session, spec, run = run, raw_controls = raw_controls))
    }

    arguments <- shiny::reactive({
      inputs <- run()
      validate_design(spec$args(inputs$values, inputs$data))
    })
    design <- app_design_task("run", spec$engine, arguments,
      long_running = spec$long_running, busy_message = spec$busy_message)
    shiny::observeEvent(design(), shinyjs::show(id = workflow$ids[["layout_download"]]))

    if (isTRUE(spec$summary)) {
      output$summary <- shiny::renderPrint({
        shiny::req(design())
        cat("Randomization was successful!", "\n", "\n")
        print(design(), n = 6)
      })
    }

    layout <- app_classic_layout(input, output, session,
      design = function() design(),
      planter = function() run()$values$planter,
      spec = workflow
    )
    location <- workflow$layout$ids["location"]
    app_classic_workflow(input, output, session,
      design = function() design(),
      layout = function() layout(),
      seed = function() run()$values$seed,
      selected = function() if (is.na(location)) 1L else as.numeric(input[[location]]),
      spec = workflow,
      simulation_ready = function() shiny::req(design()$fieldBook)
    )
  })
}
