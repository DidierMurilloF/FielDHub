#' CRD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom utils write.csv
mod_CRD_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Completely Randomized Design"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 4,
                   shiny::radioButtons(
                        inputId = ns("owndatacrd"), 
                        label = "Import entries' list?",
                        choices = c("Yes", "No"), 
                        selected = "No",
                        inline = TRUE, 
                        width = NULL,
                        choiceNames = NULL, 
                        choiceValues = NULL
                    ),
                   shiny::conditionalPanel(
                     "input.owndatacrd != 'Yes'",
                     ns = ns,
                     shiny::numericInput(ns("t.crd"),
                       label = "Input # of Treatments:",
                       value = 15, 
                       min = 2),
                    ),
                   shiny::conditionalPanel(
                     "input.owndatacrd == 'Yes'", 
                     ns = ns,
                     shiny::fluidRow(
                      shiny::column(7, style=list("padding-right: 28px;"),
                             shiny::fileInput(ns("file.CRD"),
                                       label = "Upload a CSV File:",
                                       multiple = FALSE)),
                      shiny::column(5,style=list("padding-left: 5px;"),
                             shiny::radioButtons(ns("sep.crd"),
                                          "Separator",
                                          choices = c(Comma = ",",
                                                      Semicolon = ";",
                                                      Tab = "\t"),
                                          selected = ","))
                    )
                   ),
                    shiny::numericInput(ns("reps.crd"),
                      label = "Input # of Full Reps:",
                      value = 4, 
                      min = 1),
                   shiny::selectInput(inputId = ns("planter_mov_crd"),
                               label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"),
                               multiple = FALSE,
                               selected = "serpentine"),
                   shiny::fluidRow(
                     shiny::column(6, style=list("padding-right: 28px;"),
                            shiny::textInput(ns("plot_start.crd"),
                                      "Starting Plot Number:", 
                                      value = 101)
                     ),
                     shiny::column(6,style=list("padding-left: 5px;"),
                            shiny::textInput(ns("Location.crd"),
                                      "Input Location:", 
                                      value = "FARGO")
                     )
                   ),
                   
                   app_seed_input(ns("seed.crd"), value = 123),
                   
                   shiny::fluidRow(
                     shiny::column(6,
                            shiny::actionButton(
                              inputId = ns("RUN.crd"), 
                              "Run!",
                              icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                              width = '100%'),
                     ),
                     shiny::column(6,
                            shiny::actionButton(
                              inputId = ns("Simulate.crd"),
                              "Simulate!", 
                              icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                              width = '100%')
                     )
                     
                   ), 
                   shiny::br(),
                   shiny::downloadButton(ns("downloadData.crd"),
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
                         ns("downloadCsv.crd"), 
                         label = "CSV + metadata (ZIP)",
                         icon = shiny::icon("download"),
                         width = 'auto',
                         style="color: #337ab7; background-color: #fff; border-color: #2e6da4")
                      ),
                      fieldhub_spinner(
                       plotly::plotlyOutput(ns("layout_random"), 
                                            width = "97%", 
                                            height = "560px"),
                       type = 5
                     ),
                     shiny::br(),
                     shiny::column(12,shiny::uiOutput(ns("well_panel_layout_CRD")))
            ),
            shiny::tabPanel("Field Book",
                     fieldhub_spinner(
                       DT::DTOutput(ns("CRD_fieldbook")), 
                       type = 5
                    )
            )
          )
        )
      )
    )
  )
}
#' CRD Server Function
#'
#' @noRd 
mod_CRD_server <- function(id) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    shinyjs::useShinyjs()
    
    get_data_crd <- shiny::reactive({
      
      if (input$owndatacrd == "Yes") {
        shiny::req(input$file.CRD)
        shiny::req(input$sep.crd)
        inFile <- input$file.CRD
        data_ingested <- load_file(name = inFile$name,
                                   path = inFile[["datapath"]],
                                   sep = input$sep.crd,
                                   check = TRUE, 
                                   design = "crd")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_up <- as.data.frame(data_up[,1])
          data_crd <- na.omit(data_up)
          data_crd$REP <- rep(input$reps.crd, times = nrow(data_crd))
          colnames(data_crd) <- c("TREATMENT", "REP")
          treatments = nrow(data_crd)
          return(list(data_crd = data_crd, treatments = treatments))
        } else {
          app_upload_error(data_ingested,
                           missing_columns = "Data input needs at least two columns: TREATMENT and REP.")
          return(NULL)
        }
      } else {
        shiny::req(input$t.crd)
        nt <- as.numeric(input$t.crd)
        reps <- as.numeric(input$reps.crd)
        data_crd <- data.frame(
          list(
            TREATMENT = paste0("T-", 1:nt),
            REP = rep(reps, times = nt)
            )
          )
        colnames(data_crd) <- c("TREATMENT", "REP")
        return(list(data_crd = data_crd, treatments = nt))
      }
    }) |>
      shiny::bindEvent(input$RUN.crd)
    
    crd_inputs <- shiny::reactive({
      shiny::req(get_data_crd())
      shiny::req(input$planter_mov_crd)
      shiny::req(input$reps.crd)
      shiny::req(input$plot_start.crd)
      shiny::req(input$Location.crd)
      
      treatments <- as.numeric(get_data_crd()$treatments)
      reps <- as.numeric(input$reps.crd)
      planter <- input$planter_mov_crd
      plot_start <- validate_design(read_whole_numbers(
        input$plot_start.crd, "Starting Plot Number"
      ))[1]
      site_names <-  as.vector(unlist(strsplit(input$Location.crd, ",")))
      seed <- validate_design(resolve_seed(read_app_seed(input$seed.crd)))
      return(list(t = treatments, 
        r = reps, 
        planter = planter,
        plot_start = plot_start, 
        site_names = site_names,
        seed = seed))
    }) |>
      shiny::bindEvent(input$RUN.crd)

    CRD_reactive <- shiny::reactive({
      
      shiny::req(get_data_crd())
      shiny::req(crd_inputs())
      
      shinyjs::show(id = "downloadCsv.crd")
      
      my.design <- validate_design(CRD(
        reps = crd_inputs()$r, 
        plotNumber = crd_inputs()$plot_start, 
        seed = crd_inputs()$seed,
        locationNames = crd_inputs()$site_names,
        data = get_data_crd()$data_crd
      ))
      
    }) |> 
      shiny::bindEvent(input$RUN.crd)
    
    
    reactive_layoutCRD <- app_classic_layout(input, output, session,
      design = function() CRD_reactive(),
      planter = function() crd_inputs()$planter,
      spec = classic_workflow_spec("CRD")
    )
    
    entryListFormat_CRD <- data.frame(TREATMENT = c(paste("TRT_", LETTERS[1:9], sep = "")))
    entriesInfoModal_CRD <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(entryListFormat_CRD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        shiny::h4("Note that only the TREATMENT column is required."),
        easyClose = FALSE
      )
    }
    
    toListen <- shiny::reactive({
      list(input$owndatacrd)
    })
    
    shiny::observeEvent(toListen(), {
      if (input$owndatacrd == "Yes") {
        shiny::showModal(
          entriesInfoModal_CRD()
        )
      }
    })
    
    app_classic_workflow(input, output, session,
      design = function() CRD_reactive(),
      layout = function() reactive_layoutCRD(),
      seed = function() crd_inputs()$seed,
      selected = function() 1L,
      spec = classic_workflow_spec("CRD"),
      simulation_ready = function() {
        shiny::req(CRD_reactive()$fieldBook)
      }
    )

    output$tabsetCRD <- shiny::renderUI({
      shiny::req(input$typlotCRD)
      shiny::tabsetPanel(
        if (input$typlotCRD != 3) {
          shiny::tabPanel("Completely Randomized Field Layout",
                   fieldhub_spinner(
                     shiny::plotOutput(ns("layout.crd"),
                                width = "100%",
                                height = "650px"),
                    type = 5))
        } else {
          shiny::tabPanel("Completely Randomized Field Layout",
                   fieldhub_spinner(
                     plotly::plotlyOutput(ns("heatmapCRD"), 
                                          width = "100%", 
                                          height = "650px"),
                     type = 5))
        },
        shiny::tabPanel("Completely Randomized Field Book",
                 fieldhub_spinner(
                   DT::DTOutput(ns("CRD.output")), 
                   type = 5))
      )
      
    })

  })
}
