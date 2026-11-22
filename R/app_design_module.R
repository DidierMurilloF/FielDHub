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
  # Prototype: the modernized look and wording, on the lattice pages only
  modern <- app_design_modern(spec)
  outline <- if (modern) "btn-block fh-btn-outline" else "btn-block"
  # A spatial page offers its experiment once it is randomized
  save <- if (spatial) {
    shinyjs::hidden(shiny::downloadButton(ns(ids[["download"]]), "Download field book (CSV)",
                                          class = outline))
  } else {
    shiny::downloadButton(ns(ids[["field_book_download"]]), "Download field book (CSV)",
                          class = outline)
  }
  tabs <- if (!spatial) list(
    if (isTRUE(spec$summary)) shiny::tabPanel(
      if (modern) "Summary" else "Summary Design",
      feedback(
        shiny::br(),
        shiny::div(class = "fieldhub-design-summary",
          app_output_feedback(shiny::verbatimTextOutput(ns("summary"), placeholder = FALSE))))
    ),
    shiny::tabPanel(
      "Field Layout",
      feedback(
        shinyjs::useShinyjs(),
        # Prototype: the view controls sit on the plot, not in a panel below it
        app_plot_ui(ns, list(id = ids[["plot"]], title = "Field layout or heatmap"),
                    csv_id = ids[["layout_download"]],
                    controls = if (modern) shiny::uiOutput(ns(spec$layout$output))),
        if (!modern) shiny::br(),
        if (!modern) shiny::column(12, shiny::uiOutput(ns(spec$layout$output))))
    ),
    shiny::tabPanel(
      "Field Book",
      feedback(app_output_feedback(DT::DTOutput(ns(ids[["table"]]), width = NULL, height = NULL)))
    )
  )
  page <- shiny::tagList(
    if (modern) app_design_header(spec) else shiny::h4(spec$title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        app_sidebar_ui(spec, ns = ns, toggle = toggle),
        shiny::fluidRow(
          shiny::column(6, app_task_button(
            ns("run"), if (modern) "Generate design" else "Run!",
            icon = shiny::icon("circle-nodes", verify_fa = FALSE),
            class = if (modern) "btn-block fh-btn-primary" else "btn-block", results_id = ns("results"),
            busy_message = if (!is.null(spec$optim)) "Optimizing allocation..." else
              if (spatial) "Preparing field dimensions..." else spec$busy_message
          )),
          shiny::column(6, shiny::actionButton(
            ns(ids[["simulate"]]), if (modern) "Simulate data" else "Simulate!",
            icon = shiny::icon("greater-than-equal", verify_fa = FALSE), class = outline
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
  if (modern) shiny::div(class = "fh-design", page) else page
}

#' Prototype: the pages that have the modernized look
#' @noRd
app_design_modern <- function(spec) {
  spec$module %in% c("Square_Lattice", "Rectangular_Lattice", "Alpha_Lattice")
}

#' Prototype: a slim page header for a design page (title, purpose, guide)
#' @noRd
app_design_header <- function(spec) {
  purpose <- c(
    Square_Lattice = "Resolvable incomplete blocks for a square number of treatments, with blocks as large as the square root of that number.",
    Rectangular_Lattice = "Resolvable incomplete blocks for k(k + 1) treatments in blocks of k plots.",
    Alpha_Lattice = "Resolvable incomplete blocks for any number of treatments that splits into equal blocks."
  )
  modules <- vapply(fieldhub_app_registry(), `[[`, character(1), "spec")
  guide <- fieldhub_help_topics()$guide[match(spec$module, modules)]
  shiny::tags$header(class = "fh-design-head",
    shiny::div(class = "fh-design-head-copy",
      shiny::h2(spec$title),
      shiny::p(purpose[[spec$module]])),
    app_external_link("Design guide", guide, class = "fh-design-guide"))
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
    if (app_design_modern(spec)) workflow$layout$toolbar <- TRUE
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
        shiny::updateSelectInput(session, control$id,
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
