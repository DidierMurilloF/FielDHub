#' RCBD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_RCBD_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Randomized Complete Block Designs"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        app_upload_ui(ns, "rcbd"),
        shiny::conditionalPanel(
          condition = "input.owndatarcbd != 'Yes'",
          ns = ns,
          shiny::numericInput(ns("t"),
                       label = "Input # of Treatments:",
                       value = 18,
                       min = 2)
        ),

        shiny::numericInput(inputId = ns("b"),
                     label = "Input # of Full Reps:", 
                     value = 3, min = 2),
        shiny::checkboxInput(inputId = ns("use_checks_rcbd"),
                      label = "Add repeated checks?",
                      value = FALSE),
        shiny::conditionalPanel(
          condition = "input.use_checks_rcbd == true",
          ns = ns,
          shiny::fluidRow(
            shiny::column(6, style = list("padding-right: 28px;"),
                   shiny::numericInput(inputId = ns("n_checks_rcbd"),
                                label = "Input # of Checks:",
                                value = 2, min = 1)),
            shiny::column(6, style = list("padding-left: 5px;"),
                   shiny::textInput(inputId = ns("rep_checks_rcbd"),
                             label = "Reps per Check:",
                             value = "2"))
          ),
          shiny::checkboxInput(inputId = ns("spread_checks_rcbd"),
                        label = "Spread checks within each block",
                        value = TRUE),
          shiny::uiOutput(ns("block_size_rcbd"))
        ),
        
        shiny::numericInput(inputId = ns("l.rcbd"),
                     label = "Input # of Locations:", 
                     value = 1, 
                     min = 1),
        shiny::selectInput(inputId = ns("planter_mov_rcbd"),
                    label = "Plot Order Layout:",
                    choices = c("serpentine", "cartesian"), 
                    multiple = FALSE,
                    selected = "serpentine"),
        shiny::fluidRow(
          shiny::column(6, style=list("padding-right: 28px;"),
                 shiny::textInput(inputId = ns("plot_start.rcbd"),
                           "Starting Plot Number(s):", 
                           value = 101)
          ),
          shiny::column(6,style=list("padding-left: 5px;"),
                 shiny::checkboxInput(inputId = ns("continuous.plot"),
                               label = "Continuous Plot ", 
                               value = TRUE),
          )
        ),
        
        shiny::textInput(inputId = ns("Location.rcbd"),
                  "Input Location:",
                  value = "FARGO"),
        
        app_seed_input(ns("seed.rcbd"), value = 123),
        
        shiny::fluidRow(
          shiny::column(6,
                 shiny::actionButton(
                   inputId = ns("RUN.rcbd"), 
                   label = "Run!", 
                   icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                   width = '100%'),
          ),
          shiny::column(6,
                 shiny::actionButton(
                   ns("Simulate.rcbd"), 
                   label = "Simulate!", 
                   icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                   width = '100%'),
          )
          
        ), 
        shiny::br(),
        shiny::downloadButton(ns("downloadData.rcbd"),
                       "Save experiment (ZIP)",
                       style = "width:100%")
                   
      ),

      shiny::mainPanel(
        width = 8,
        shiny::fluidRow(
          shiny::tabsetPanel(
            shiny::tabPanel("Field Layout",
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(
                       shiny::downloadButton(
                         ns("downloadCsv.rcbd"), 
                         label = "CSV + metadata (ZIP)",
                         icon = shiny::icon("download"),
                         width = 'auto',
                         style="color: #337ab7; background-color: #fff; border-color: #2e6da4")
                      ),
                     fieldhub_spinner(
                       plotly::plotlyOutput(
                         ns("layouts"), 
                         width = "97%", 
                         height = "550px"),
                       type = 5
                     ),
                     shiny::br(),
                     shiny::column(12,shiny::uiOutput(ns("well_panel_layout_RCBD")))
            ),
            shiny::tabPanel("Field Book",
                     fieldhub_spinner(
                       DT::DTOutput(ns("RCBD_fieldbook")), 
                       type = 5)
            )
          )
        )
      )
    )
  )
}
#' RCBD Server Functions
#'
#' @noRd 
mod_RCBD_server <- function(id) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    ns <- session$ns

    get_data_rcbd <- shiny::reactive({
      if (input$owndatarcbd == "Yes") {
        data_ingested <- app_read_upload(input, "rcbd")
        if (is.null(data_ingested)) return(NULL)
        data_up <- data_ingested$data
        data_up <- as.data.frame(data_up[,1])
        data_rcbd <- na.omit(data_up)
        colnames(data_rcbd) <- "TREATMENT"
        nt <- nrow(data_rcbd)
        return(list(data_rcbd = data_rcbd, treatments = nt))
      } else {
        shiny::req(input$t)
        nt <- as.numeric(input$t)
        # No entry list is built here: RCBD() generates its own "CH1".."CHk"
        # and "T1".."Tn" labels from bare counts (design_args_RCBD()/
        # RCBD(t =, checks = )); checks itself is parsed in rcbd_inputs().
        return(list(data_rcbd = NULL, treatments = nt))
      }
    }) |>
      shiny::bindEvent(input$RUN.rcbd)
    
    rcbd_inputs <- shiny::reactive({
      
      shiny::req(get_data_rcbd())
      
      shiny::req(input$b)
      shiny::req(input$plot_start.rcbd)
      shiny::req(input$Location.rcbd)
      shiny::req(input$l.rcbd)
      shiny::req(input$planter_mov_rcbd)
      
      reps <- as.numeric(input$b)
      treatments <- as.numeric(get_data_rcbd()$treatments)
      planter <- input$planter_mov_rcbd
      plot_start <- validate_design(parse_whole_numbers(
        input$plot_start.rcbd, "Starting Plot Number"
      ))
      location_names <-  as.vector(unlist(strsplit(input$Location.rcbd, ",")))
      seed <- validate_design(app_design_seed(input$seed.rcbd))
      l <- as.numeric(input$l.rcbd)
      continuous <- input$continuous.plot

      use_checks <- isTRUE(input$use_checks_rcbd)
      checks <- NULL
      rep_checks <- NULL
      spread_checks <- TRUE
      if (use_checks) {
        if (is.null(input$n_checks_rcbd) || is.null(input$rep_checks_rcbd)) {
          shiny::req(FALSE)  # UI not rendered yet; nothing to validate
        }
        checks <- app_attempt(parse_n_checks(input$n_checks_rcbd))
        shiny::req(checks)
        rep_checks <- app_attempt(parse_rep_checks(input$rep_checks_rcbd, checks))
        shiny::req(rep_checks)
        spread_checks <- isTRUE(input$spread_checks_rcbd)
      }

      return(list(
        reps = reps,
        t = treatments,
        planter = planter,
        plot_start = plot_start,
        l = l,
        location_names = location_names,
        continuous = continuous,
        seed = seed,
        checks = checks,
        rep_checks = rep_checks,
        spread_checks = spread_checks)
        )
    }) |>
      shiny::bindEvent(input$RUN.rcbd)

    app_upload_dialog_observer(input, "rcbd")

    RCBD_reactive <- shiny::reactive({
      
      shiny::req(get_data_rcbd())
      shiny::req(rcbd_inputs())
      
      shinyjs::show(id = "downloadCsv.rcbd")

      validate_design(do.call(
        RCBD, design_args_RCBD(rcbd_inputs(), get_data_rcbd()$data_rcbd)
      ))

    })  |>
      shiny::bindEvent(input$RUN.rcbd)

    output$block_size_rcbd <- shiny::renderUI({
      shiny::req(input$use_checks_rcbd)
      # On the upload path the pool comes from the file and the checks are carved
      # out of it, so `input$t` says nothing about the block size. Only predict it
      # for the manually generated entry list.
      if (!identical(input$owndatarcbd, "No")) {
        return(shiny::helpText(
          "Block size depends on the uploaded list: its first rows are taken as the checks."
        ))
      }
      shiny::req(input$t, input$b)
      if (is.null(input$n_checks_rcbd) || is.null(input$rep_checks_rcbd)) {
        return(NULL)  # UI not rendered yet
      }
      shiny::helpText(validate_design(
        rcbd_size_preview(input$t, input$b, input$n_checks_rcbd, input$rep_checks_rcbd)
      ))
    })

    
    reactive_layoutRCBD <- app_classic_layout(input, output, session,
      design = function() RCBD_reactive(),
      planter = function() rcbd_inputs()$planter,
      spec = classic_workflow_spec("RCBD")
    )

    app_classic_workflow(input, output, session,
      design = function() RCBD_reactive(),
      layout = function() reactive_layoutRCBD(),
      seed = function() rcbd_inputs()$seed,
      selected = function() return(as.numeric(input$locLayout_rcbd)),
      spec = classic_workflow_spec("RCBD"),
      simulation_ready = function() {
        shiny::req(RCBD_reactive()$fieldBook)
      }
    )

  })
}
    
## To be copied in the UI
# mod_RCBD_ui("RCBD_ui_1")
    
## To be copied in the server
# mod_RCBD_server("RCBD_ui_1")
