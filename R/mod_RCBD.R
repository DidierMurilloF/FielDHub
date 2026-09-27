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
        shiny::radioButtons(ns("owndatarcbd"),
                     label = "Import entries' list?", 
                     choices = c("Yes", "No"), 
                     selected = "No",
                     inline = TRUE, 
                     width = NULL, 
                     choiceNames = NULL, 
                     choiceValues = NULL),
        shiny::conditionalPanel(
          condition = "input.owndatarcbd != 'Yes'", 
          ns = ns,
          shiny::numericInput(ns("t"),
                       label = "Input # of Treatments:",
                       value = 18, 
                       min = 2)
        ),
        shiny::conditionalPanel(
          condition = "input.owndatarcbd == 'Yes'", 
          ns = ns,
          shiny::fluidRow(
            shiny::column(8, style=list("padding-right: 28px;"),
                   shiny::fileInput(inputId = ns("file.RCBD"),
                             label = "Upload a CSV File:", 
                             multiple = FALSE)),
            shiny::column(4, style=list("padding-left: 5px;"),
                   shiny::radioButtons(inputId = ns("sep.rcbd"), "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","))
          )        
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
        
        shiny::numericInput(inputId = ns("seed.rcbd"),
                     label = "Random Seed:",
                     value = 123, 
                     min = 1),
        
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
    
    shinyjs::useShinyjs()

    get_data_rcbd <- shiny::reactive({
      if (input$owndatarcbd == "Yes") {
        shiny::req(input$file.RCBD)
        inFile <- input$file.RCBD
        data_ingested <- load_file(name = inFile$name, 
                                   path = inFile[["datapath"]],
                                   sep = input$sep.rcbd, 
                                   check = TRUE, 
                                   design = "rcbd")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_up <- as.data.frame(data_up[,1])
          data_rcbd <- na.omit(data_up)
          colnames(data_rcbd) <- "TREATMENT"
          nt <- nrow(data_rcbd)
          return(list(data_rcbd = data_rcbd, treatments = nt))
        } else {
          app_upload_error(data_ingested,
                           missing_columns = "Data input needs at least one column: TREATMENT")
          return(NULL)
        }
      } else {
        shiny::req(input$t)
        nt <- as.numeric(input$t)
        if (isTRUE(input$use_checks_rcbd)) {
          if (is.null(input$n_checks_rcbd)) return(NULL)  # UI not rendered yet
          n_ck_parsed <- parse_n_checks(input$n_checks_rcbd)
          if (!n_ck_parsed$ok) {
            shinyalert::shinyalert("Error!!", n_ck_parsed$message, type = "error")
            return(NULL)
          }
          n_ck <- n_ck_parsed$value
          # Checks lead the pool, matching mod_RCBD_augmented.R:299-301.
          labels <- c(paste0("CH", seq_len(n_ck)), paste0("G-", seq_len(nt)))
        } else {
          labels <- paste0("G-", seq_len(nt))
        }
        data_rcbd <- data.frame(TREATMENT = labels)
        return(list(data_rcbd = data_rcbd, treatments = nt))
      }
    }) |>
      shiny::bindEvent(input$RUN.rcbd)
    
    rcbd_inputs <- shiny::reactive({
      
      shiny::req(get_data_rcbd())
      
      shiny::req(input$b)
      shiny::req(input$seed.rcbd)
      shiny::req(input$plot_start.rcbd)
      shiny::req(input$Location.rcbd)
      shiny::req(input$l.rcbd)
      shiny::req(input$planter_mov_rcbd)
      
      r <- as.numeric(input$b)
      treatments <- as.numeric(get_data_rcbd()$treatments)
      planter <- input$planter_mov_rcbd
      plot_start <- validate_design(read_whole_numbers(
        input$plot_start.rcbd, "Starting Plot Number"
      ))
      site_names <-  as.vector(unlist(strsplit(input$Location.rcbd, ",")))
      seed <- as.numeric(input$seed.rcbd)
      sites <- as.numeric(input$l.rcbd)
      continuous <- input$continuous.plot

      use_checks <- isTRUE(input$use_checks_rcbd)
      n_checks <- NULL
      rep_checks <- NULL
      spread_checks <- TRUE
      if (use_checks) {
        if (is.null(input$n_checks_rcbd) || is.null(input$rep_checks_rcbd)) {
          shiny::req(FALSE)  # UI not rendered yet; nothing to validate
        }
        n_ck_parsed <- parse_n_checks(input$n_checks_rcbd)
        if (!n_ck_parsed$ok) {
          shinyalert::shinyalert("Error!!", n_ck_parsed$message, type = "error")
          shiny::req(FALSE)
        }
        n_checks <- n_ck_parsed$value
        rep_parsed <- parse_rep_checks(input$rep_checks_rcbd, n_checks)
        if (!rep_parsed$ok) {
          shinyalert::shinyalert("Error!!", rep_parsed$message, type = "error")
          shiny::req(FALSE)
        }
        rep_checks <- rep_parsed$value
        spread_checks <- isTRUE(input$spread_checks_rcbd)
      }

      return(list(
        r = r,
        t = treatments,
        planter = planter,
        plot_start = plot_start,
        sites = sites,
        site_names = site_names,
        continuous = continuous,
        seed = seed,
        use_checks = use_checks,
        n_checks = n_checks,
        rep_checks = rep_checks,
        spread_checks = spread_checks)
        )
    }) |>
      shiny::bindEvent(input$RUN.rcbd)

    entryListFormat_RCBD <- data.frame(
      TREATMENT = c(paste("TRT_", LETTERS[1:9], sep = ""))
      )
    entriesInfoModal_RCBD <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(entryListFormat_RCBD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        shiny::h4("Note that only the TREATMENT column is required. When repeated checks are enabled, the first rows of the file are taken as the checks."),
        easyClose = FALSE
      )
    }
    
    toListen <- shiny::reactive({
      list(input$owndatarcbd)
    })
    
    shiny::observeEvent(toListen(), {
      if (input$owndatarcbd == "Yes") {
        shiny::showModal(
          entriesInfoModal_RCBD()
        )
      }
    })

    RCBD_reactive <- shiny::reactive({
      
      shiny::req(get_data_rcbd())
      shiny::req(rcbd_inputs())
      
      shinyjs::show(id = "downloadCsv.rcbd")
      
      result <- tryCatch(
        RCBD(
          t = rcbd_inputs()$t,
          reps = rcbd_inputs()$r,
          l = rcbd_inputs()$sites,
          plotNumber = rcbd_inputs()$plot_start,
          continuous = rcbd_inputs()$continuous,
          planter = rcbd_inputs()$planter,
          seed = rcbd_inputs()$seed,
          locationNames = rcbd_inputs()$site_names,
          checks = if (rcbd_inputs()$use_checks) rcbd_inputs()$n_checks else NULL,
          rep_checks = if (rcbd_inputs()$use_checks) rcbd_inputs()$rep_checks else NULL,
          spread_checks = rcbd_inputs()$spread_checks,
          data = get_data_rcbd()$data_rcbd
        ),
        error = function(e) {
          shinyalert::shinyalert("Error!!", conditionMessage(e), type = "error")
          NULL
        }
      )
      shiny::req(result)

      result

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
      description <- tryCatch(
        rcbd_size_preview(input$t, input$b, input$n_checks_rcbd, input$rep_checks_rcbd),
        fieldhub_error = conditionMessage
      )
      shiny::helpText(description)
    })

    output$well_panel_layout_RCBD <- shiny::renderUI({
      shiny::req(RCBD_reactive()$fieldBook)
      obj_rcbd <- RCBD_reactive()
      layoutOptions_rcbd <- validate_design(layout_choices(x = obj_rcbd, stacked = "vertical"))
      df <- RCBD_reactive()$fieldBook
      stacked_rcbd <- c("Vertical Stack Panel" = "vertical", 
                          "Horizontal Stack Panel" = "horizontal")
      sites <- length(levels(as.factor(df$LOCATION)))
      shiny::wellPanel(
        shiny::column(3,
               shiny::radioButtons(ns("typlotRCBD"), "Type of Plot:",
                            c("Entries/Treatments" = 1,
                              "Plots" = 2,
                              "Heatmap" = 3))
        ),
        shiny::fluidRow(
          shiny::column(3,
                 shiny::selectInput(inputId = ns("stackedRCBD"),
                             label = "Reps layout:", 
                             choices = stacked_rcbd),
          ),
          shiny::column(2,
                 shiny::selectInput(inputId = ns("layoutO_rcbd"),
                             label = "Layout option:", 
                             choices = layoutOptions_rcbd, 
                             selected = 1)
          ),
          shiny::column(2,
                 shiny::selectInput(inputId = ns("locLayout_rcbd"),
                             label = "Location:", 
                             choices = 1:sites)
          )
        )
      )
    })
    
    reactive_layoutRCBD <- app_layout_selection(input, session,
      design = function() RCBD_reactive(),
      planter = function() rcbd_inputs()$planter,
      ids = c(layout = "layoutO_rcbd", stacked = "stackedRCBD", location = "locLayout_rcbd")
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
