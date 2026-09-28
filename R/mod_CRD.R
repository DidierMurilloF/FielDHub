#' CRD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_CRD_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Completely Randomized Design"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 4,
                   app_upload_ui(ns, "crd"),
                   shiny::conditionalPanel(
                     "input.owndatacrd != 'Yes'",
                     ns = ns,
                     shiny::numericInput(ns("t.crd"),
                       label = "Input # of Treatments:",
                       value = 15,
                       min = 2),
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

    get_data_crd <- shiny::reactive({
      
      if (input$owndatacrd == "Yes") {
        data_ingested <- app_read_upload(input, "crd")
        if (is.null(data_ingested)) return(NULL)
        data_up <- data_ingested$data
        data_up <- as.data.frame(data_up[,1])
        data_crd <- na.omit(data_up)
        data_crd$REP <- rep(input$reps.crd, times = nrow(data_crd))
        colnames(data_crd) <- c("TREATMENT", "REP")
        treatments = nrow(data_crd)
        return(list(data_crd = data_crd, treatments = treatments))
      } else {
        shiny::req(input$t.crd)
        nt <- as.numeric(input$t.crd)
        # No entry list is built here: CRD() generates its own "T1".."Tn"
        # labels from a bare treatment count (design_args_CRD()/CRD(t = )).
        return(list(data_crd = NULL, treatments = nt))
      }
    }) |>
      shiny::bindEvent(input$RUN.crd)
    
    crd_inputs <- shiny::reactive({
      shiny::req(get_data_crd())
      shiny::req(input$planter_mov_crd)
      shiny::req(input$reps.crd)
      shiny::req(input$plot_start.crd)
      shiny::req(input$Location.crd)
      
      plot_start <- validate_design(parse_whole_numbers(
        input$plot_start.crd, "Starting Plot Number"
      ))[1]
      location_names <-  as.vector(unlist(strsplit(input$Location.crd, ",")))
      seed <- validate_design(app_design_seed(input$seed.crd))
      design_values_CRD(
        treatment_count = as.numeric(get_data_crd()$treatments),
        reps = as.numeric(input$reps.crd),
        planter = input$planter_mov_crd,
        plot_start = plot_start,
        location_names = location_names,
        seed = seed,
        data = get_data_crd()$data_crd
      )
    }) |>
      shiny::bindEvent(input$RUN.crd)

    CRD_reactive <- shiny::reactive({

      shiny::req(get_data_crd())
      shiny::req(crd_inputs())

      shinyjs::show(id = "downloadCsv.crd")

      my.design <- validate_design(do.call(
        CRD, design_args_CRD(crd_inputs(), get_data_crd()$data_crd)
      ))

    }) |>
      shiny::bindEvent(input$RUN.crd)
    
    
    reactive_layoutCRD <- app_classic_layout(input, output, session,
      design = function() CRD_reactive(),
      planter = function() crd_inputs()$planter,
      spec = classic_workflow_spec("CRD")
    )
    
    app_upload_dialog_observer(input, "crd")

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

  })
}
