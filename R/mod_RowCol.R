#' RowCol UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_RowCol_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Row-Column Design"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 4,

                   shiny::radioButtons(inputId = ns("owndataRCD"),
                                label = "Import entries' list?",
                                choices = c("Yes", "No"), 
                                selected = "No",
                                inline = TRUE, 
                                width = NULL, 
                                choiceNames = NULL, 
                                choiceValues = NULL),
                   
                   shiny::conditionalPanel(
                     condition = "input.owndataRCD == 'Yes'", 
                     ns = ns,
                     shiny::fluidRow(
                       shiny::column(8, style=list("padding-right: 28px;"),
                              shiny::fileInput(ns("file.RCD"),
                                        label = "Upload a csv File:", 
                                        multiple = FALSE)),
                       shiny::column(4,style=list("padding-left: 5px;"),
                              shiny::radioButtons(ns("sep.rcd"), "Separator",
                                           choices = c(Comma = ",",
                                                       Semicolon = ";",
                                                       Tab = "\t"),
                                           selected = ","
                              )
                       )
                    )
                   ),
                   shiny::conditionalPanel(
                     condition = "input.owndataRCD != 'Yes'",
                     ns = ns,
                     shiny::numericInput(ns("t.rcd"),
                                  label = "Input # of Treatments:",
                                  value = 42,
                                  min = 2),
                   ),
                   shiny::fluidRow(
                     shiny::column(6, style=list("padding-right: 28px;"),
                            shiny::selectInput(inputId = ns("k.rcd"),
                                        label = "Input # of Rows:",
                                        choices = ""),
                     ),
                     shiny::column(6,style=list("padding-left: 5px;"),
                            shiny::numericInput(ns("r.rcd"),
                                         label = "Input # of Full Reps:",
                                         value = 2, 
                                         min = 2)
                     )
                   ),
                   shiny::numericInput(inputId = ns("l.rcd"),
                                label = "Input # of Locations:", 
                                value = 1, min = 1),
                   shiny::selectInput(inputId = ns("planter_mov_rcd"),
                               label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"),
                               multiple = FALSE,
                               selected = "serpentine"),
                   shiny::fluidRow(
                     shiny::column(6, style=list("padding-right: 28px;"),
                            shiny::textInput(ns("plot_start.rcd"),
                                      "Starting Plot Number:", 
                                      value = 101)
                     ),
                     shiny::column(6, style=list("padding-left: 5px;"),
                            shiny::textInput(ns("Location.rcd"),
                                      "Input Location:", 
                                      value = "FARGO")
                     )
                   ),
                   app_seed_input(ns("seed.rcd"), value = 2437),
                   shiny::fluidRow(
                     shiny::column(6,
                            shiny::actionButton(
                              inputId = ns("RUN.rcd"), 
                              label = "Run!", 
                              icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                              width = '100%'),
                     ),
                     shiny::column(6,
                            shiny::actionButton(
                              ns("Simulate.RowCol"), 
                              label = "Simulate!", 
                              icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                              width = '100%'),
                     )
                   ), 
                   shiny::br(),
                   shiny::downloadButton(ns("downloadData.rowcolD"),
                                  "Save experiment (ZIP)",
                                  style = "width:100%")
      ),
      shiny::mainPanel(
        width = 8,
        shiny::fluidRow(
          shiny::tabsetPanel(
            shiny::tabPanel(
              "Summary Design",
              shiny::br(),
              fieldhub_spinner(
                shiny::verbatimTextOutput(outputId = ns("summary_row_column"),
                                   placeholder = FALSE), 
                type = 4
              ),
              style = "padding-right: 40px;"
            ),
            shiny::tabPanel("Field Layout",
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(shiny::downloadButton(ns("downloadCsv.rcd"),
                                                    label = "CSV + metadata (ZIP)",
                                                    icon = shiny::icon("download"),
                                                    width = 'auto',
                                                    style="color: #337ab7; background-color: #fff; border-color: #2e6da4")),
                     fieldhub_spinner(
                       plotly::plotlyOutput(ns("layouts"), 
                                            width = "97%", 
                                            height = "550px"),
                       type = 5),
                     shiny::br(),
                     shiny::column(12,shiny::uiOutput(ns("well_panel_layout_ROWCOL")))
            ),
            shiny::tabPanel("Field Book",
                     fieldhub_spinner(DT::DTOutput(ns("rowcolD")),
                                                  type = 5)
            )
          )
        )
      )
    )
  )
}

#' RowCol Server Functions
#'
#' @noRd 
mod_RowCol_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    entryListFormat_RCD <- data.frame(ENTRY = 1:9, 
                                       NAME = c(paste("Genotype", 
                                                      LETTERS[1:9], 
                                                      sep = "")))
    entriesInfoModal_RCD <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(entryListFormat_RCD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        shiny::h4("Entry numbers can be any set of consecutive positive numbers."),
        easyClose = FALSE
      )
    }
    
    toListen <- shiny::reactive({
      list(input$owndataRCD)
    })
    
    shiny::observeEvent(toListen(), {
      if (input$owndataRCD == "Yes") {
        shiny::showModal(
          entriesInfoModal_RCD()
        )
      }
    })
    
    init_data_rcd <- shiny::reactive({
      
      if (input$owndataRCD == "Yes") {
        shiny::req(input$file.RCD)
        inFile <- input$file.RCD
        data_ingested <- load_file(name = inFile$name, 
                                   path = inFile[["datapath"]],
                                   sep = input$sep.rcd, 
                                   check = TRUE, 
                                   design = "rcd")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_up <- as.data.frame(data_up[,1:2])
          data_rcd <- na.omit(data_up)
          colnames(data_rcd) <- c("ENTRY", "NAME")
          treatments = nrow(data_rcd)
          return(list(data_rcd = data_rcd, treatments = treatments))
        } else {
          app_upload_error(data_ingested,
                           missing_columns = "Data input needs at least two columns: ENTRY and NAME")
          return(NULL)
        }
      } else {
        shiny::req(input$t.rcd)
        nt <- as.numeric(input$t.rcd)
        # No entry list is built here: row_column() generates its own
        # "G-1".."G-n" labels from a bare treatment count
        # (design_args_RowCol()/row_column(t = )).
        return(list(data_rcd = NULL, treatments = nt))
      }
    })
    
    list_to_observe <- shiny::reactive({
      shiny::req(init_data_rcd())
      list(
        entry_list = input$owndataRCD,
        entries = init_data_rcd()$treatments
      )
    })
    
    shiny::observeEvent(list_to_observe(), {
      shiny::req(init_data_rcd())
      options <- valid_block_sizes(
        as.numeric(init_data_rcd()$treatments),
        "row_column"
      )
      k <- if (length(options) == 0L) "No Options Available" else options
      
      if (length(options) > 2L) {
        selected <- options[ceiling(length(options) / 2)]
      } else selected <- k[1]
      
      shiny::updateSelectInput(session = session,
                        inputId = 'k.rcd', 
                        label = "Input # of Rows:",
                        choices = k, 
                        selected = selected)
      
    })

    get_data_rcd <- shiny::reactive({
      if (is.null(init_data_rcd())) {
        app_report_problem("Check input file and try again!")
        return(NULL)
      } else return(init_data_rcd())
    }) |>
      shiny::bindEvent(input$RUN.rcd)

    rcd_inputs <- shiny::reactive({
      shiny::req(get_data_rcd())
      shiny::req(input$k.rcd)
      shiny::req(input$r.rcd)
      shiny::req(input$plot_start.rcd)
      shiny::req(input$Location.rcd)
      shiny::req(input$l.rcd)
      if (input$k.rcd == "No Options Available") {
        app_report_problem("No options for this combination of treatments!")
        return(NULL)
      } 
      l <- as.numeric(input$l.rcd)
      reps <- as.numeric(input$r.rcd)
      nrows <- as.numeric(input$k.rcd)
      treatments <- as.numeric(get_data_rcd()$treatments)
      planter <- input$planter_mov_rcd
      plot_start <- validate_design(parse_whole_numbers(
        input$plot_start.rcd, "Starting Plot Number"
      ))
      location_names <-  as.vector(unlist(strsplit(input$Location.rcd, ",")))
      seed <- validate_design(app_design_seed(input$seed.rcd))
      return(list(reps = reps,
                  nrows = nrows,
                  t = treatments,
                  plot_start = plot_start,
                  planter = planter,
                  l = l,
                  location_names = location_names,
                  seed = seed))
    }) |>
      shiny::bindEvent(input$RUN.rcd)

    RowCol_reactive <- shiny::reactive({

      shiny::req(rcd_inputs())
      shiny::req(get_data_rcd())

      shinyjs::show(id = "downloadCsv.rcd")

      # row_column() itself rejects an under-replicated design (a classed
      # fieldhub_input_error surfaced below through validate_design()); no
      # duplicate reps < 2 check is needed here.
      validate_design(do.call(
        row_column, design_args_RowCol(rcd_inputs(), get_data_rcd()$data_rcd)
      ))

    }) |>
      shiny::bindEvent(input$RUN.rcd)
    
    output$summary_row_column <- shiny::renderPrint({
      shiny::req(RowCol_reactive())
      cat("Randomization was successful!", "\n", "\n")
      print(RowCol_reactive(), n = 6)
    })
    
    upDateSites <- shiny::reactive({
      shiny::req(input$l.rcd)
      locs <- as.numeric(input$l.rcd)
      sites <- 1:locs
      return(list(sites = sites))
    }) |>
      shiny::bindEvent(input$RUN.rcd)
    
    
    reactive_layoutROWCOL <- app_classic_layout(input, output, session,
      design = function() RowCol_reactive(),
      planter = function() rcd_inputs()$planter,
      spec = classic_workflow_spec("RowCol"),
      locations = function() as.numeric(upDateSites()$sites)
    )
    
    app_classic_workflow(input, output, session,
      design = function() RowCol_reactive(),
      layout = function() reactive_layoutROWCOL(),
      seed = function() rcd_inputs()$seed,
      selected = function() return(as.numeric(input$locLayout_rcd)),
      spec = classic_workflow_spec("RowCol"),
      simulation_ready = function() {
        shiny::req(RowCol_reactive()$fieldBook)
      }
    )

  })
}
