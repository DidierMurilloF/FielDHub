#' SPD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_SPD_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Split-Plot Design"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        shiny::radioButtons(inputId = ns("owndataSPD"),
                     label = "Import entries' list?", 
                     choices = c("Yes", "No"), 
                     selected = "No",
                     inline = TRUE, 
                     width = NULL, 
                     choiceNames = NULL, 
                     choiceValues = NULL),
        shiny::selectInput(inputId = ns("kindSPD"),
                    label = "Select SPD Type:",
                    choices = c("Split-Plot in a RCBD" = "SPD_RCBD", 
                                "Split-Plot in a CRD" = "SPD_CRD"),
                    multiple = FALSE),
        
        shiny::conditionalPanel("input.owndataSPD == 'Yes'", ns = ns,
                         shiny::fluidRow(
                           shiny::column(8, style=list("padding-right: 28px;"),
                                  shiny::fileInput(ns("file.SPD"),
                                            label = "Upload a csv File:", 
                                            multiple = FALSE)),
                           shiny::column(4,style=list("padding-left: 5px;"),
                                  shiny::radioButtons(ns("sep.spd"), "Separator",
                                               choices = c(Comma = ",",
                                                           Semicolon = ";",
                                                           Tab = "\t"),
                                               selected = ","))
                         )
        ),
        shiny::conditionalPanel("input.owndataSPD != 'Yes'", ns = ns,
                          shiny::numericInput(ns("mp.spd"),
                                       label = "Whole-plots:",
                                       value = 4, min = 2),
                          shiny::numericInput(ns("sp.spd"),
                                       label = "Sub-plots Within Whole-plots:",
                                       value = 3, min = 2)
        ),
        
        shiny::fluidRow(
          shiny::column(6, style=list("padding-right: 28px;"),
                 shiny::numericInput(ns("reps.spd"), label = "Input # of Full Reps:",
                              value = 3, min = 2), 
          ),
          shiny::column(6,style=list("padding-left: 5px;"),
                 shiny::numericInput(ns("l.spd"), label = "Input # of Locations:",
                              value = 1, min = 1)
          )
        ),
        # The RCBD-type layouts number whole plots in a fixed order, so the
        # plot order only applies to the CRD type
        shiny::conditionalPanel("input.kindSPD == 'SPD_CRD'", ns = ns,
          shiny::selectInput(inputId = ns("planter_mov_spd"),
            label = "Plot Order Layout:",
            choices = c("serpentine", "cartesian"), 
            multiple = FALSE,
            selected = "serpentine")
        ),
        shiny::fluidRow(
          shiny::column(6, style=list("padding-right: 28px;"),
                 shiny::textInput(ns("plot_start.spd"), "Starting Plot Number:",
                           value = 101)
          ),
          shiny::column(6,style=list("padding-left: 5px;"),
                 shiny::textInput(ns("Location.spd"), "Input the Location:",
                           value = "FARGO")
          )
        ),
        app_seed_input(ns("seed.spd"), value = 118),
        
        shiny::fluidRow(
          shiny::column(6,
                 shiny::actionButton(
                   inputId = ns("RUN.spd"), 
                   "Run!", 
                   icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                   width = '100%'),
          ),
          shiny::column(6,
                 shiny::actionButton(
                   ns("Simulate.spd"), 
                   "Simulate!", 
                   icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                   width = '100%'),
          )
          
        ), 
        shiny::br(),
        shiny::downloadButton(ns("downloadData.spd"), "Save experiment (ZIP)",
                      style = "width:100%")
      ),
      
      shiny::mainPanel(
        width = 8,
        shiny::fluidRow(
          shiny::tabsetPanel(
            shiny::tabPanel("Field Layout",
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(shiny::downloadButton(ns("downloadCsv.spd"),
                                                    label = "CSV + metadata (ZIP)",
                                                    icon = shiny::icon("download"),
                                                    width = 'auto',
                                                    style="color: #337ab7; background-color: #fff; border-color: #2e6da4")),
                     fieldhub_spinner(
                       plotly::plotlyOutput(ns("layouts"), 
                                            width = "97%", 
                                            height = "560px"),
                       type = 5
                     ),
                     shiny::br(),
                     shiny::column(12,shiny::uiOutput(ns("well_panel_layout_SPD")))
            ),
            shiny::tabPanel("Field Book",
                     fieldhub_spinner(
                       DT::DTOutput(ns("SPD.output")), 
                       type = 5
                       )
            )
          )
        )
      )
    )    
  )
}
    
#' SPD Server Functions
#'
#' @noRd 
mod_SPD_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    shinyjs::useShinyjs()
    
    wp <- c("NFung", paste("Fung", 1:4, sep = "")) 
    sp <- paste("Beans", 1:10, sep = "")            
    entryListFormat_SPD <- data.frame(list(WHOLEPLOT = c(wp, rep("", 5)), 
                                           SUBPLOT = sp))
    
    entriesInfoModal_SPD<- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(entryListFormat_SPD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        #h4("Note that reps might be unbalanced."),
        easyClose = FALSE
      )
    }
    
    toListen <- shiny::reactive({
      list(input$owndataSPD)
    })
    
    shiny::observeEvent(toListen(), {
      if (input$owndataSPD == "Yes") {
        shiny::showModal(
          entriesInfoModal_SPD()
        )
      }
    })
    
    get_data_spd <- shiny::reactive({
      if (input$owndataSPD == "Yes") {
        shiny::req(input$file.SPD)
        inFile <- input$file.SPD
        data_ingested <- load_file(name = inFile$name, 
                                   path = inFile[["datapath"]],
                                   sep = input$sep.spd, 
                                   check = TRUE, 
                                   design = "spd")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_spd <- as.data.frame(data_up[,1:2])
          colnames(data_spd) <- c("WHOLEPLOT", "SUBPLOT")
          wp <- as.vector(na.omit(data_spd[,1]))
          sp <- as.vector(na.omit(data_spd[,2]))
          treatments <- c(wp, sp)
          return(list(data_spd = data_spd, treatments = treatments))
        } else {
          app_upload_error(data_ingested,
                           missing_columns = "Data input needs at least two column: WHOLEPLOT and SUBPLOT")
          return(NULL)
        }
      } else {
        shiny::req(input$mp.spd, input$sp.spd)
        wp <- as.numeric(input$mp.spd)
        sp <- as.numeric(input$sp.spd)
        treatments <- c(wp, sp)
        data_spd <- NULL
        return(list(data_spd = data_spd, treatments = treatments))
      }
    }) |> 
      shiny::bindEvent(input$RUN.spd)
    
    spd_inputs <- shiny::reactive({
      
      shiny::req(get_data_spd())
      
      shiny::req(input$plot_start.spd)
      shiny::req(input$Location.spd)
      shiny::req(input$l.spd)
      shiny::req(input$reps.spd)
      
      sites <- as.numeric(input$l.spd)
      seed <- validate_design(app_design_seed(input$seed.spd))
      plot_start <- validate_design(read_whole_numbers(
        input$plot_start.spd, "Starting Plot Number"
      ))
      site_names <-  as.vector(unlist(strsplit(input$Location.spd, ",")))
      reps <- as.numeric(input$reps.spd)
      planter <- input$planter_mov_spd
      data_spd <- get_data_spd()$data_spd
      
      if (input$kindSPD == "SPD_RCBD") {
        type_design <- 2
      } else type_design <- 1
      
      return(
        list(
          wp = get_data_spd()$treatments[1], 
          sp = get_data_spd()$treatments[2], 
          r = reps, 
          sites = sites,
          seed = seed,
          planter = planter,
          plot_start = plot_start,
          site_names = site_names, 
          type_design = type_design,
          data = data_spd
        )
      )
    }) |>
      shiny::bindEvent(input$RUN.spd)

    spd_reactive <- shiny::reactive({
      
      shiny::req(spd_inputs())
      
      shinyjs::show(id = "downloadCsv.spd")
      
      validate_design(split_plot(
        wp = spd_inputs()$wp, 
        sp = spd_inputs()$sp, 
        reps = spd_inputs()$r, 
        l = spd_inputs()$sites,
        plotNumber = spd_inputs()$plot_start,
        seed = spd_inputs()$seed,
        type = spd_inputs()$type_design, 
        locationNames = spd_inputs()$site_names, 
        data = spd_inputs()$data
      ))
    }) |> 
      shiny::bindEvent(input$RUN.spd)

    upDateSites <- shiny::eventReactive(input$RUN.spd, {
      shiny::req(input$l.spd)
      locs <- as.numeric(input$l.spd)
      sites <- 1:locs
      return(list(sites = sites))
    })

    
    reactive_layoutSPD <- app_classic_layout(input, output, session,
      design = function() spd_reactive(),
      planter = function() spd_inputs()$planter,
      spec = classic_workflow_spec("SPD"),
      locations = function() as.numeric(upDateSites()$sites)
    )

    app_classic_workflow(input, output, session,
      design = function() spd_reactive(),
      layout = function() reactive_layoutSPD(),
      seed = function() spd_inputs()$seed,
      selected = function() return(as.numeric(input$locLayout_spd)),
      spec = classic_workflow_spec("SPD"),
      simulation_ready = function() {
        shiny::req(spd_reactive()$fieldBook)
      }
    )

  })
}
