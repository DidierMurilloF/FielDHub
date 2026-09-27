#' LSD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_LSD_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Latin Square Design"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        shiny::radioButtons(inputId = ns("owndataLSD"),
                     label = "Import entries' list?", 
                     choices = c("Yes", "No"), 
                     selected = "No",
                     inline = TRUE, 
                     width = NULL, 
                     choiceNames = NULL, 
                     choiceValues = NULL), 
        
        shiny::conditionalPanel(
          condition = "input.owndataLSD == 'Yes'", 
          ns = ns,
          shiny::fluidRow(
            shiny::column(8, style=list("padding-right: 28px;"),
                   shiny::fileInput(ns("file.LSD"),
                             label = "Upload a CSV File:", 
                             multiple = FALSE)),
            
            shiny::column(4,style=list("padding-left: 5px;"),
                   shiny::radioButtons(ns("sep.lsd"), "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","))
          )             
        ),
        
        shiny::conditionalPanel(
          condition = "input.owndataLSD != 'Yes'", ns = ns,
                         
          shiny::numericInput(ns("n.lsd"),
                       label = "Input # of Treatments:",
                       value = 5, 
                       min = 2),             
        ),
        
        shiny::numericInput(ns("reps.lsd"),
                     label = "Input # of Full Reps (Squares):",
                     value = 1, 
                     min = 1),
        shiny::selectInput(inputId = ns("planter.lsd"),
                    label = "Plot Order Layout:",
                    choices = c("serpentine", "cartesian"), 
                    multiple = FALSE,
                    selected = "serpentine"),
        shiny::fluidRow(
          shiny::column(6, style=list("padding-right: 28px;"),
                 shiny::textInput(ns("plot_start.lsd"),
                           "Starting Plot Number:", 
                           value = 101)
          ),
          shiny::column(6,style=list("padding-left: 5px;"),
                 shiny::textInput(ns("Location.lsd"),
                           "Input Location:", 
                           value = "FARGO")
          )
        ),
        app_seed_input(ns("seed.lsd"), value = 123),
        
        shiny::fluidRow(
          shiny::column(6,
                 shiny::actionButton(
                   inputId = ns("RUN.lsd"), 
                   label = "Run!", 
                   icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                   width = '100%'),
          ),
          shiny::column(6,
                 shiny::actionButton(
                   ns("Simulate.lsd"), 
                   label = "Simulate!", 
                   icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                   width = '100%'),
          )
          
        ), 
        shiny::br(),
        shiny::downloadButton(ns("downloadData.lsd"),
                       "Save experiment (ZIP)",
                       style = "width:100%")
                   
      ),
      shiny::mainPanel(width = 8,
          shiny::fluidRow(
            shiny::tabsetPanel(
              shiny::tabPanel("Field Layout",
                       shinyjs::useShinyjs(),
                       shinyjs::hidden(
                         shiny::downloadButton(
                           ns("downloadCsv.lsd"), 
                           label = "CSV + metadata (ZIP)",
                           icon = shiny::icon("download"),
                           width = 'auto',
                           style="color: #337ab7; background-color: #fff; border-color: #2e6da4")
                        ),
                       plotly::plotlyOutput(ns("layout_lsd"),
                                            width = "97%",
                                            height = "550px"),
                       shiny::br(),
                       shiny::column(12, shiny::uiOutput(ns("well_panel_layout_LSD")))
              ),
              shiny::tabPanel("Field Book",
                       fieldhub_spinner(
                         DT::DTOutput(ns("LSD_fieldbook")), 
                         type = 5
                      )
              )
            )
          )
      )
    ) 
  )
}

#' LSD Server Functions
#'
#' @noRd 
mod_LSD_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    shinyjs::useShinyjs()
    
    entryListFormat_LSD <- data.frame(
      list(ROW = paste("Period", 1:5, sep = ""),
           COLUMN = paste("Cow", 1:5, sep = ""),
           TREATMENT = paste("Diet", 1:5, sep = ""))
    )
    entriesInfoModal_LSD <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message",
                            style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(entryListFormat_LSD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        easyClose = FALSE
      )
    }
    
    toListen <- shiny::reactive({
      list(input$owndataLSD)
    })
    
    shiny::observeEvent(toListen(), {
      if (input$owndataLSD == "Yes") {
        shiny::showModal(
          entriesInfoModal_LSD()
        )
      }
    })

    get_data_lsd <- shiny::reactive({
      if (input$owndataLSD == "Yes") {
        shiny::req(input$file.LSD)
        shiny::req(input$sep.lsd)
        inFile <- input$file.LSD
        data_ingested <- load_file(name = inFile$name, 
                                path = inFile[["datapath"]],
                                sep = input$sep.lsd,
                                check = TRUE, 
                                design = "lsd")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_up <- as.data.frame(data_up[,1:3])
          data_lsd <- na.omit(data_up)
          colnames(data_lsd) <- c("ROW", "COLUMN", "TREATMENT")
          return(list(data_lsd = data_lsd))
        } else {
          app_upload_error(data_ingested,
                           missing_columns = "Data input needs at least one column: ROW, COLUMN, and  TREATMENT")
          return(NULL)
        }
      }
    })

    lsd_inputs <- shiny::reactive({
      
      shiny::req(input$plot_start.lsd)
      shiny::req(input$Location.lsd)
      shiny::req(input$reps.lsd)
      
      if (input$owndataLSD == "Yes") {
        shiny::req(get_data_lsd())
        n.lsd <- NULL
        reps.lsd <- as.numeric(input$reps.lsd)
        data.lsd <- get_data_lsd()$data_lsd
        n <- as.numeric(nrow(data.lsd))
        n.lsd <- n
        if (n > 10) {
          shinyalert::shinyalert(
            "Error!!", 
            "Only up to 10 treatments are allowed.", 
            type = "error")
          return(NULL)
        }
      } else {
        shiny::req(input$n.lsd)
        n <- as.numeric(input$n.lsd)
        if (n > 10) {
          shinyalert::shinyalert(
            "Error!!", 
            "Only up to 10 treatments are allowed.", 
            type = "error")
          return(NULL)
        }
        n.lsd <- n
        reps.lsd <- as.numeric(input$reps.lsd)
        data.lsd <- NULL
      }
      
      plot_number <- validate_design(read_whole_numbers(
        input$plot_start.lsd, "Starting Plot Number"
      ))
      loc.lsd <-  as.vector(unlist(strsplit(input$Location.lsd, ",")))
      seed.number.lsd <- validate_design(app_design_seed(input$seed.lsd))
      planting_lsd <- input$planter.lsd

      return(
        list(
        t = n.lsd, 
        reps = reps.lsd, 
        plot_number = plot_number[1],
        planter = planting_lsd,
        location_names = loc.lsd[1], 
        data = data.lsd,
        seed = seed.number.lsd)
      )
    }) |>
      shiny::bindEvent(input$RUN.lsd)
    
    latinsquare_reactive <- shiny::reactive({
      
      shiny::req(lsd_inputs())
      
      shinyjs::show(id = "downloadCsv.lsd")
      
      validate_design(latin_square(
        t = lsd_inputs()$t, 
        reps = lsd_inputs()$reps, 
        plotNumber = lsd_inputs()$plot_number,
        planter = lsd_inputs()$planter,
        seed = lsd_inputs()$seed, 
        locationNames = lsd_inputs()$location_names, 
        data = lsd_inputs()$data
      ))
      
    }) |> 
      shiny::bindEvent(input$RUN.lsd)

    
    reactive_layoutLSD <- app_classic_layout(input, output, session,
      design = function() latinsquare_reactive(),
      planter = function() lsd_inputs()$planter,
      spec = classic_workflow_spec("LSD")
    )
    
    app_classic_workflow(input, output, session,
      design = function() latinsquare_reactive(),
      layout = function() reactive_layoutLSD(),
      seed = function() lsd_inputs()$seed,
      selected = function() 1L,
      spec = classic_workflow_spec("LSD"),
      simulation_ready = function() {
        shiny::req(latinsquare_reactive()$fieldBook)
      }
    )

  })
}

## To be copied in the UI
# mod_LSD_ui("LSD_ui_1")

## To be copied in the server
# mod_LSD_server("LSD_ui_1")
