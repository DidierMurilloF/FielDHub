#' STRIPD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_STRIPD_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Strip-Plot Design"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        shiny::radioButtons(inputId = ns("owndataSTRIP"),
                     label = "Import entries' list?", 
                     choices = c("Yes", "No"), 
                     selected = "No",
                     inline = TRUE, 
                     width = NULL, 
                     choiceNames = NULL, 
                     choiceValues = NULL),
        shiny::conditionalPanel(
          condition = "input.owndataSTRIP == 'Yes'", 
          ns = ns,
          shiny::fluidRow(
            shiny::column(8, style=list("padding-right: 28px;"),
                   shiny::fileInput(ns("file.STRIP"),
                             label = "Upload a csv File:",
                             multiple = FALSE)),
            
            shiny::column(4,style=list("padding-left: 5px;"),
                   shiny::radioButtons(ns("sep.strip"), "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","))
          )
        ),
        
        shiny::conditionalPanel(
          condition = "input.owndataSTRIP != 'Yes'", 
          ns = ns,
          shiny::fluidRow(
            shiny::column(6, style=list("padding-right: 28px;"),
                   shiny::numericInput(ns("HStrip.strip"),
                                label = "Input # of Horizontal Strips:",
                                value = 5, 
                                min = 2)
            ),
            shiny::column(6, style=list("padding-left: 5px;"),
                   shiny::numericInput(ns("VStrip.strip"),
                                label = "Input # of Vertical Strips:",
                                value = 5, 
                                min = 2)
            )
          )           
        ),
        shiny::numericInput(ns("blocks.strip"),
                     label = "Input # of Full Reps:", 
                     value = 3, 
                     min = 2),
        shiny::numericInput(ns("l.strip"),
                     label = "Input # of Locations:",
                     value = 1, 
                     min = 1), 
        shiny::selectInput(inputId = ns("planter.strip"),
                    label = "Plot Order Layout:",
                    choices = c("serpentine", "cartesian"), 
                    multiple = FALSE,
                    selected = "serpentine"),
        shiny::fluidRow(
          shiny::column(6, style=list("padding-right: 28px;"),
                 shiny::textInput(ns("plot_start.strip"),
                           "Starting Plot Number:", 
                           value = 101)
          ),
          shiny::column(6, style=list("padding-left: 5px;"),
                 shiny::textInput(ns("Location.strip"),
                           "Input Location:", 
                           value = "FARGO")
          )
        ),
        
        # ---- Added UI for randomizeH and randomizeV ----
        shiny::checkboxInput(
          ns("randomizeH.strip"),
          label = "Randomize Horizontal Strips (Across reps)",
          value = TRUE
        ),
        shiny::checkboxInput(
          ns("randomizeV.strip"),
          label = "Randomize Vertical Strips (Across reps)",
          value = TRUE
        ),
        # -----------------------------------------------
        
        app_seed_input(ns("myseed.strip"), value = 123),
        
        shiny::fluidRow(
          shiny::column(6,
                 shiny::actionButton(
                   inputId = ns("RUN.strip"), 
                   "Run!", 
                   icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                   width = '100%'
                   ),
          ),
          shiny::column(6,
                 shiny::actionButton(
                   ns("Simulate.strip"), 
                   "Simulate!", 
                   icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                   width = '100%'
                   ),
          )
          
        ), 
        shiny::br(),
        shiny::downloadButton(ns("downloadData.strip"),
                       "Save experiment (ZIP)",
                       style = "width:100%")
      ),
      
      shiny::mainPanel(
        width = 8,
        shiny::fluidRow(
          shiny::tabsetPanel(
            shiny::tabPanel(title = "Field Layout",
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(shiny::downloadButton(ns("downloadCsv.strip"),
                                                    label = "CSV + metadata (ZIP)",
                                                    icon = shiny::icon("download"),
                                                    width = 'auto',
                                                    style="color: #337ab7; background-color: #fff; border-color: #2e6da4")),
                     fieldhub_spinner(
                       plotly::plotlyOutput(ns("layout.strip"), 
                                            width = "97%", 
                                            height = "560px"),
                       type = 5
                     ),
                     shiny::br(),
                     shiny::column(12,
                            shiny::uiOutput(ns("well_panel_layout_STRIP"))
                            )
            ),
            shiny::tabPanel(title = "Field Book",
                     fieldhub_spinner(
                       DT::DTOutput(ns("STRIP.output")), 
                       type = 5
                       )
            )
          )
        )
      )
    ) 
  )
}
    
#' STRIPD Server Functions
#'
#' @noRd 
mod_STRIPD_server <- function(id) {
  shiny::moduleServer( id, function(input, output, session) {
    ns <- session$ns
    shinyjs::useShinyjs()

    Hplots <- LETTERS[1:5]
    Vplots <- LETTERS[1:5]
    entryListFormat_STRIP <- data.frame(
      list(HPLOTS = Hplots, VPLOTS = Vplots)
      )           
    entriesInfoModal_STRIP <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(entryListFormat_STRIP,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        easyClose = FALSE
      )
    }
    
    toListen <- shiny::reactive({
      list(input$owndataSTRIP)
    })
    
    shiny::observeEvent(toListen(), {
      if (input$owndataSTRIP == "Yes") {
        shiny::showModal(
          entriesInfoModal_STRIP()
        )
      }
    })
    
    get_data_strip <- shiny::reactive({
      if (input$owndataSTRIP == "Yes") {
        shiny::req(input$file.STRIP)
        inFile <- input$file.STRIP
        data_ingested <- load_file(name = inFile$name, 
                                   path = inFile[["datapath"]],
                                   sep = input$sep.strip, 
                                   check = TRUE,
                                   design = "strip")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_strip <- as.data.frame(data_up[,1:2])
          colnames(data_strip) <- c("Hplot", "Vplot")
          Hstrip <- length(as.vector(na.omit(data_strip[,1])))
          Vstrip <- length(as.vector(na.omit(data_strip[,2])))
          treatments <- c(Hstrip, Vstrip)
          return(list(data_strip = data_strip, treatments = treatments))
        } else {
          app_upload_error(data_ingested,
                           missing_columns = "Data input needs at least two column: Hplot and Vplot")
          return(NULL)
        }
      } else {
        shiny::req(input$HStrip.strip, input$VStrip.strip)
        shiny::req(input$blocks.strip)
        Hplots <- as.numeric(input$HStrip.strip)
        Vplots <- as.numeric(input$VStrip.strip)
        treatments = c(Hplots, Vplots)
        return(list(data_strip = NULL, treatments = treatments))
      }
    }) |> 
      shiny::bindEvent(input$RUN.strip)

    strip_inputs <- shiny::reactive({
      shiny::req(input$blocks.strip)
      if (input$blocks.strip < 2) {
        shinyalert::shinyalert(
          "Error!!", 
          "Strip-Plot Design needs at least 2 replicates.", 
          type = "error")
        return(NULL)
      }
      shiny::req(get_data_strip())
      
      shiny::req(input$plot_start.strip)
      shiny::req(input$Location.strip)
      shiny::req(input$l.strip)
      shiny::req(input$blocks.strip)
      shiny::req(input$planter.strip)
      
      l.strip <- as.numeric(input$l.strip)
      seed.strip <- validate_design(resolve_seed(read_app_seed(input$myseed.strip)))
      plot_start.strip <- validate_design(read_whole_numbers(
        input$plot_start.strip, "Starting Plot Number"
      ))
      loc.strip <-  as.vector(unlist(strsplit(input$Location.strip, ",")))
      reps.strip <- as.numeric(input$blocks.strip)
      planter <- input$planter.strip
      data_strip <- get_data_strip()$data_strip
      
      return(
        list(
          Hplots = get_data_strip()$treatments[1], 
          Vplots = get_data_strip()$treatments[2], 
          b = reps.strip, 
          l = l.strip,
          seed = seed.strip,
          planter = planter,
          plot_number = plot_start.strip,
          site_names = loc.strip, 
          data = data_strip
        )
      )
    }) |>
      shiny::bindEvent(input$RUN.strip)
    
    strip_reactive <- shiny::reactive({
      shiny::req(strip_inputs())
      
      shinyjs::show(id = "downloadCsv.strip")
    
      validate_design(strip_plot(
        Hplots = strip_inputs()$Hplots,
        Vplots = strip_inputs()$Vplots,
        reps = strip_inputs()$b,
        l = strip_inputs()$l,
        planter = strip_inputs()$planter,
        plotNumber = strip_inputs()$plot_number,
        locationNames = strip_inputs()$site_names,
        seed = strip_inputs()$seed,
        randomizeH = input$randomizeH.strip,
        randomizeV = input$randomizeV.strip,
        data = strip_inputs()$data
      ))
      
    }) |> 
      shiny::bindEvent(input$RUN.strip)

    upDateSites <- shiny::reactive({
      shiny::req(input$l.strip)
      locs <- as.numeric(input$l.strip)
      sites <- 1:locs
      return(list(sites = sites))
    }) |> 
      shiny::bindEvent(input$RUN.strip)
    
    
    reactive_layoutSTRIP <- app_classic_layout(input, output, session,
      design = function() strip_reactive(),
      planter = function() strip_inputs()$planter,
      spec = classic_workflow_spec("STRIPD"),
      locations = function() as.numeric(upDateSites()$sites)
    )
    
    app_classic_workflow(input, output, session,
      design = function() strip_reactive(),
      layout = function() reactive_layoutSTRIP(),
      seed = function() strip_inputs()$seed,
      selected = function() return(as.numeric(input$locLayout_strip)),
      spec = classic_workflow_spec("STRIPD"),
      simulation_ready = function() {
        shiny::req(strip_reactive()$fieldBook)
      }
    )

  })
}
    
## To be copied in the UI
# mod_STRIPD_ui("STRIPD_ui_1")
    
## To be copied in the server
# mod_STRIPD_server("STRIPD_ui_1")
