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
        shiny::radioButtons(inputId = ns("owndataSSPD"),
                     label = "Do you have your own data?", 
                     choices = c("Yes", "No"), 
                     selected = "No",
                     inline = TRUE, 
                     width = NULL, 
                     choiceNames = NULL, 
                     choiceValues = NULL),
        
        shiny::selectInput(inputId = ns("kindSSPD"),
                    label = "Select SSPD Type:",
                    choices = c("Split-Split Plot in a RCBD" = "SSPD_RCBD", 
                                "Split-Split Plot in a CRD" = "SSPD_CRD"),
                    multiple = FALSE),
        
        shiny::conditionalPanel(
          condition = "input.owndataSSPD == 'Yes'", 
          ns = ns,
          shiny::fluidRow(
            shiny::column(8, style=list("padding-right: 28px;"),
                   shiny::fileInput(ns("file.SSPD"),
                             label = "Upload a csv File:", 
                             multiple = FALSE)),
            
            shiny::column(4,style=list("padding-left: 5px;"),
                   shiny::radioButtons(ns("sep.sspd"), "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","))
          )          
        ),
        
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
                         min = 2)
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
    shinyjs::useShinyjs()
    
   wp <- paste("IRR_", c("NO", "Yes"), sep = "") 
   sp <- c("NFung", paste("Fung", 1:4, sep = "")) 
   ssp <- paste("Beans", 1:10, sep = "") 
   entryListFormat_SSPD <- data.frame(list(WHOLPLOT = c(wp, rep("", 8)), 
                                            SUBPLOT = c(sp, rep("", 5)),
                                            SUB_SUBPLOT = ssp))            
  
    entriesInfoModal_SSPD <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(entryListFormat_SSPD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        easyClose = FALSE
      )
    }
    
    toListen <- shiny::reactive({
      list(input$owndataSSPD)
    })
    
    shiny::observeEvent(toListen(), {
      if (input$owndataSSPD == "Yes") {
        shiny::showModal(
          entriesInfoModal_SSPD()
        )
      }
    })
    
    get_data_sspd <- shiny::reactive({
      if (input$owndataSSPD == "Yes") {
        shiny::req(input$file.SSPD)
        inFile <- input$file.SSPD
        data_ingested <- load_file(name = inFile$name, 
                                   path = inFile[["datapath"]],
                                   sep = input$sep.sspd, 
                                   check = TRUE, 
                                   design = "sspd")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_sspd <- as.data.frame(data_up[, 1:3])
          colnames(data_sspd) <- c("WHOLEPLOT", "SUBPLOT", "SUB_SUBPLOT")
          wp <- as.vector(na.omit(data_sspd[,1]))
          sp <- as.vector(na.omit(data_sspd[,2]))
          ssp <- as.vector(na.omit(data_sspd[,3]))
          treatments <- c(wp, sp, ssp)
          return(list(data_sspd = data_sspd, treatments = treatments))
        } else {
          app_upload_error(data_ingested,
                           missing_columns = "Data input needs at least two column: WHOLEPLOT, SUBPLOT, and SUB_SUBPLOT")
          return(NULL)
        }
      } else {
        shiny::req(input$mp.sspd, input$sp.sspd, input$ssp.sspd)
        wp <- as.numeric(input$mp.sspd)
        sp <- as.numeric(input$sp.sspd)
        ssp <- as.numeric(input$ssp.sspd)
        treatments <- c(wp, sp, ssp)
        data_spd <- NULL
        return(list(data_spd = data_spd, treatments = treatments))
      }
    }) |> 
      shiny::bindEvent(input$RUN.sspd)
    
    sspd_inputs <- shiny::reactive({
      
      shiny::req(get_data_sspd())
      
      shiny::req(input$plot_start.sspd)
      shiny::req(input$Location.sspd)
      shiny::req(input$l.sspd)
      shiny::req(input$reps.sspd)
      
      sites <- as.numeric(input$l.sspd)
      seed <- validate_design(app_design_seed(input$seed.sspd))
      plot_start <- validate_design(read_whole_numbers(
        input$plot_start.sspd, "Starting Plot Number"
      ))
      site_names <-  as.vector(unlist(strsplit(input$Location.sspd, ",")))
      reps <- as.numeric(input$reps.sspd)
      planter <- input$planter_mov_sspd
      data_sspd <- get_data_sspd()$data_sspd
      
      if (input$kindSSPD == "SSPD_RCBD") {
        type_design <- 2
      } else type_design <- 1
      
      return(
        list(
          wp = get_data_sspd()$treatments[1], 
          sp = get_data_sspd()$treatments[2], 
          ssp = get_data_sspd()$treatments[3], 
          r = reps, 
          sites = sites,
          seed = seed,
          planter = planter,
          plot_start = plot_start,
          site_names = site_names, 
          type_design = type_design,
          data = data_sspd
        )
      )
    }) |>
      shiny::bindEvent(input$RUN.sspd)

    sspd_reactive <- shiny::reactive({
      
      shiny::req(sspd_inputs())
      
      shinyjs::show(id = "downloadCsv.sspd")
      
      validate_design(split_split_plot(
        wp = sspd_inputs()$wp, 
        sp = sspd_inputs()$sp, 
        ssp = sspd_inputs()$ssp, 
        reps = sspd_inputs()$r, 
        l = sspd_inputs()$sites, 
        plotNumber = sspd_inputs()$plot_start, 
        seed = sspd_inputs()$seed, 
        type = sspd_inputs()$type_design, 
        locationNames = sspd_inputs()$site_names, 
        data = sspd_inputs()$data
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
