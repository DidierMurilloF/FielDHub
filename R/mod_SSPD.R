#' SSPD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_SSPD_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Split-Split-Plot Design"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        app_upload_ui(ns, "sspd"),

        shiny::selectInput(inputId = ns("kindSSPD"),
                    label = "Select SSPD Type:",
                    choices = c("Split-Split Plot in a RCBD" = "SSPD_RCBD",
                                "Split-Split Plot in a CRD" = "SSPD_CRD"),
                    multiple = FALSE),

        shiny::conditionalPanel(
          condition = "input.owndataSSPD != 'Yes'", 
          ns = ns,
          shiny::numericInput(ns("mp.sspd"),
                       label = "Whole-plots:",
                       value = 2, 
                       min = 2),
          shiny::numericInput(ns("sp.sspd"),
                       label = "Sub-plots Within Whole-plots:",
                       value = 2, 
                       min = 2),
          shiny::numericInput(ns("ssp.sspd"),
                       label = "Sub-Sub-plots within Sub-plots:",
                       value = 5, 
                       min = 2)
          ),
        
        shiny::fluidRow(
          shiny::column(6, style=list("padding-right: 28px;"),
            shiny::numericInput(ns("reps.sspd"),
                         label = "Input # of Full Reps:",
                         value = 3,
                         min = 1)
          ),
          shiny::column(6, style=list("padding-left: 5px;"),
            shiny::numericInput(ns("l.sspd"),
                         label = "Input # of Locations:",
                         value = 1, 
                         min = 1)
          )
        ), 
        
        # The RCBD-type layouts number whole plots in a fixed order, so the
        # plot order only applies to the CRD type
        shiny::conditionalPanel("input.kindSSPD == 'SSPD_CRD'", ns = ns,
          shiny::selectInput(inputId = ns("planter_mov_sspd"),
                      label = "Plot Order Layout:",
                      choices = c("serpentine", "cartesian"), 
                      multiple = FALSE,
                      selected = "serpentine")
        ),
        
        shiny::fluidRow(
          shiny::column(6,style=list("padding-right: 28px;"),
                 shiny::textInput(ns("plot_start.sspd"),
                           "Starting Plot Number:", 
                           value = 101)
          ),
          shiny::column(6,style=list("padding-left: 5px;"),
                 shiny::textInput(ns("Location.sspd"), "
                           Input Location:", 
                           value = "FARGO")
          )
        ),
        
        app_seed_input(ns("seed.sspd"), value = 123),
        
        shiny::fluidRow(
          shiny::column(6,
                 shiny::actionButton(
                   inputId = ns("RUN.sspd"), 
                   "Run!", 
                   icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                   width = '100%'),
          ),
          shiny::column(6,
                 shiny::actionButton(
                   ns("Simulate.sspd"), 
                   "Simulate!", 
                   icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                   width = '100%'),
          )
          
        ), 
        shiny::br(),
        shiny::downloadButton(
          ns("downloadData.sspd"), 
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
                         ns("downloadCsv.sspd"), 
                         label = "CSV + metadata (ZIP)",
                         icon = shiny::icon("download"),
                         width = 'auto',
                         style="color: #337ab7; background-color: #fff; border-color: #2e6da4")),
                     fieldhub_spinner(
                       plotly::plotlyOutput(ns("layouts"), 
                                            width = "97%", 
                                            height = "580px"),
                       type = 5
                     ),
                     shiny::br(),
                     shiny::column(12,
                            shiny::uiOutput(ns("well_panel_layout_SSPD"))
                            )
            ),
            shiny::tabPanel("Field Book",
                     fieldhub_spinner(
                       DT::DTOutput(ns("SSPD.output")), 
                       type = 5)
            )
          )
        )
      )
    )
  )
}
    
#' SSPD Server Functions
#'
#' @noRd 
mod_SSPD_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    app_upload_dialog_observer(input, "sspd")

    get_data_sspd <- shiny::reactive({
      if (input$owndataSSPD == "Yes") {
        data_ingested <- app_read_upload(input, "sspd")
        if (is.null(data_ingested)) return(NULL)
        data_up <- data_ingested$data
        data_sspd <- as.data.frame(data_up[, 1:3])
        colnames(data_sspd) <- c("WHOLEPLOT", "SUBPLOT", "SUB_SUBPLOT")
        wp <- as.vector(na.omit(data_sspd[,1]))
        sp <- as.vector(na.omit(data_sspd[,2]))
        ssp <- as.vector(na.omit(data_sspd[,3]))
        treatments <- c(wp, sp, ssp)
        return(list(data_sspd = data_sspd, treatments = treatments))
      } else {
        shiny::req(input$mp.sspd, input$sp.sspd, input$ssp.sspd)
        wp <- as.numeric(input$mp.sspd)
        sp <- as.numeric(input$sp.sspd)
        ssp <- as.numeric(input$ssp.sspd)
        treatments <- c(wp, sp, ssp)
        return(list(data_sspd = NULL, treatments = treatments))
      }
    }) |> 
      shiny::bindEvent(input$RUN.sspd)
    
    sspd_inputs <- shiny::reactive({
      
      shiny::req(get_data_sspd())
      
      shiny::req(input$plot_start.sspd)
      shiny::req(input$Location.sspd)
      shiny::req(input$l.sspd)
      shiny::req(input$reps.sspd)
      
      l <- as.numeric(input$l.sspd)
      seed <- validate_design(app_design_seed(input$seed.sspd))
      plot_start <- validate_design(parse_whole_numbers(
        input$plot_start.sspd, "Starting Plot Number"
      ))
      location_names <-  as.vector(unlist(strsplit(input$Location.sspd, ",")))
      reps <- as.numeric(input$reps.sspd)
      planter <- input$planter_mov_sspd
      type <- if (input$kindSSPD == "SSPD_RCBD") 2 else 1

      design_values_SSPD(
        wp_count = get_data_sspd()$treatments[1],
        sp_count = get_data_sspd()$treatments[2],
        ssp_count = get_data_sspd()$treatments[3],
        reps = reps,
        l = l,
        seed = seed,
        planter = planter,
        plot_start = plot_start,
        location_names = location_names,
        type = type,
        data = get_data_sspd()$data_sspd
      )
    }) |>
      shiny::bindEvent(input$RUN.sspd)

    sspd_reactive <- shiny::reactive({

      shiny::req(sspd_inputs())

      shinyjs::show(id = "downloadCsv.sspd")

      validate_design(do.call(
        split_split_plot, design_args_SSPD(sspd_inputs(), get_data_sspd()$data_sspd)
      ))

    }) |>
      shiny::bindEvent(input$RUN.sspd)
  
    
    reactive_layoutSSPD <- app_classic_layout(input, output, session,
      design = function() sspd_reactive(),
      planter = function() sspd_inputs()$planter,
      spec = classic_workflow_spec("SSPD")
    )

    app_classic_workflow(input, output, session,
      design = function() sspd_reactive(),
      layout = function() reactive_layoutSSPD(),
      seed = function() sspd_inputs()$seed,
      selected = function() return(as.numeric(input$locLayout_sspd)),
      spec = classic_workflow_spec("SSPD"),
      simulation_ready = function() {
        shiny::req(sspd_reactive()$fieldBook)
      }
    )

  })
}
