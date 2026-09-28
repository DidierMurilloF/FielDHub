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
        app_upload_ui(ns, "lsd"),

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

    app_upload_dialog_observer(input, "lsd")

    get_data_lsd <- shiny::reactive({
      if (input$owndataLSD == "Yes") {
        data_ingested <- app_read_upload(input, "lsd")
        if (is.null(data_ingested)) return(NULL)
        data_up <- data_ingested$data
        data_up <- as.data.frame(data_up[,1:3])
        data_lsd <- na.omit(data_up)
        colnames(data_lsd) <- c("ROW", "COLUMN", "TREATMENT")
        return(list(data_lsd = data_lsd))
      }
    })

    lsd_inputs <- shiny::reactive({
      
      shiny::req(input$plot_start.lsd)
      shiny::req(input$Location.lsd)
      shiny::req(input$reps.lsd)
      
      # latin_square() itself rejects more than 10 treatments (a classed
      # fieldhub_input_error surfaced below through validate_design()); no
      # duplicate check is needed here on either path.
      if (input$owndataLSD == "Yes") {
        shiny::req(get_data_lsd())
        reps.lsd <- as.numeric(input$reps.lsd)
        data.lsd <- get_data_lsd()$data_lsd
        n.lsd <- NULL
      } else {
        shiny::req(input$n.lsd)
        n.lsd <- as.numeric(input$n.lsd)
        reps.lsd <- as.numeric(input$reps.lsd)
        data.lsd <- NULL
      }

      plot_start <- validate_design(parse_whole_numbers(
        input$plot_start.lsd, "Starting Plot Number"
      ))
      loc.lsd <-  as.vector(unlist(strsplit(input$Location.lsd, ",")))
      seed.number.lsd <- validate_design(app_design_seed(input$seed.lsd))
      planting_lsd <- input$planter.lsd

      return(
        list(
        t = n.lsd,
        reps = reps.lsd,
        plot_start = plot_start[1],
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

      validate_design(do.call(
        latin_square, design_args_LSD(lsd_inputs(), lsd_inputs()$data)
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
