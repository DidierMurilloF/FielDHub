#FD first try

#' FD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
mod_FD_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Full Factorial Designs"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 4,
                   shiny::radioButtons(inputId = ns("owndata"),
                                label = "Import entries' list?", 
                                choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, 
                                choiceNames = NULL, choiceValues = NULL),
                   shiny::selectInput(inputId = ns("kindFD"),
                               label = "Select a Factorial Design Type:",
                               choices = c("Factorial in a RCBD" = "FD_RCBD", 
                                           "Factorial in a CRD" = "FD_CRD"),
                               multiple = FALSE),
                   
                   shiny::conditionalPanel("input.owndata != 'Yes'", ns = ns,
                                    shiny::textInput(inputId = ns("setfactors"),
                                              label = "Input # of Entries for Each Factor: (Separated by Comma)",
                                              value = "2,2,3")     
                   ),
                   shiny::conditionalPanel("input.owndata == 'Yes'", ns = ns,
                                    shiny::fluidRow(
                                      shiny::column(8, style=list("padding-right: 28px;"),
                                             shiny::fileInput(ns("file.FD"),
                                                       label = "Upload a CSV File:", 
                                                       multiple = FALSE)),
                                      shiny::column(4,style=list("padding-left: 5px;"),
                                             shiny::radioButtons(ns("sep.fd"), "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t"),
                                                          selected = ","))
                                    )
                   ),
                   
                   shiny::fluidRow(
                     shiny::column(6, style=list("padding-right: 28px;"),
                            shiny::numericInput(inputId = ns("reps.fd"), label = "Input # of Full Reps:",
                                         value = 3, min = 2)
                     ),
                     shiny::column(6,style=list("padding-left: 5px;"),
                            shiny::numericInput(ns("l.fd"), label = "Input # of Locations:",
                                         value = 1, min = 1)
                     )
                   ),
                   shiny::fluidRow(
                     shiny::column(6, style=list("padding-right: 28px;"),
                            shiny::textInput(ns("plot_start.fd"), "Starting Plot Number:", value = 101)
                     ),
                     shiny::column(6,style=list("padding-left: 5px;"),
                            shiny::textInput(ns("Location.fd"), "Input Location:", value = "FARGO")
                     )
                   ),
                   shiny::selectInput(inputId = ns("planter_mov_fd"), label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"), multiple = FALSE,
                               selected = "serpentine"),
                   
                   shiny::numericInput(inputId = ns("seed.fd"), label = "Random Seed:",
                                value = 123, min = 1),
                   shiny::fluidRow(
                     shiny::column(6,
                            shiny::actionButton(
                              inputId = ns("RUN.fd"), "Run!", 
                              icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                              width = '100%'),
                     ),
                     shiny::column(6,
                            shiny::actionButton(
                              ns("Simulate.fd"), "Simulate!", 
                              icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                              width = '100%'),
                     )
                     
                   ), 
                   shiny::br(),
                   shiny::downloadButton(ns("downloadData.fd"), "Save experiment (ZIP)",
                                  style = "width:100%")
      ),
      
      shiny::mainPanel(
        width = 8,
        shiny::fluidRow(
          shiny::tabsetPanel(
            shiny::tabPanel("Field Layout",
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(shiny::downloadButton(ns("downloadCsv.fd"),
                                                    label = "CSV + metadata (ZIP)",
                                                    icon = shiny::icon("download"),
                                                    width = 'auto',
                                                    style="color: #337ab7; background-color: #fff; border-color: #2e6da4")),
                     fieldhub_spinner(
                       plotly::plotlyOutput(ns("layouts"), width = "97%", 
                                            height = "550px"), type = 5
                     ),
                     shiny::br(),
                     shiny::column(12, shiny::uiOutput(ns("well_panel_layout_FD")))
            ),
            shiny::tabPanel("Field Book",
                     fieldhub_spinner(DT::DTOutput(ns("FD.Output")), type = 5)
            )
          )
        )
      )
    ) 
  )
}
#' FD Server Functions
#'
#' @noRd 
mod_FD_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    shinyjs::useShinyjs()
    
    FACTORS <- rep(c("A", "B", "C"), c(2,3,2))
    LEVELS <- c("a0", "a1", "b0", "b1", "b2", "c0", "c1")
    entryListFormat_FD <- data.frame(list(FACTOR = FACTORS, LEVEL = LEVELS))
    
    entriesInfoModal_FD <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(entryListFormat_FD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        easyClose = FALSE
      )
    }
    
    toListen <- shiny::reactive({
      list(input$owndata)
    })
    
    shiny::observeEvent(toListen(), {
      if (input$owndata == "Yes") {
        shiny::showModal(
          entriesInfoModal_FD()
        )
      }
    })
    
    get_data_factorial <- shiny::reactive({
      
      if (input$owndata == "Yes") {
        shiny::req(input$file.FD)
        shiny::req(input$sep.fd)
        inFile <- input$file.FD
        
        data_ingested <- load_file(name = inFile$name,
          path = inFile[["datapath"]],
          sep = input$sep.fd,
          check = TRUE, 
          design = "factorial")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_up <- as.data.frame(data_up[,1:2])
          data_factorial <- na.omit(data_up)
          colnames(data_factorial) <- c("FACTOR", "LEVEL")
          set_factors <- factor(data_factorial$FACTOR, as.character(unique(data_factorial$FACTOR)))
          set_factors.fd <- levels(set_factors)
          nt <- length(set_factors.fd)
          if (nt < 2) {
            shinyalert::shinyalert(
              "Error!!", 
              "More than one factor needs to be specified.", 
              type = "error")
            return(NULL)
          }
          return(list(data_fd = data_factorial, treatments = set_factors.fd))
        } else {
          app_upload_error(data_ingested,
                           missing_columns = "Data input needs at least two column: FACTOR and LEVEL")
          return(NULL)
        }
      } else {
        shiny::req(input$setfactors)
        reps <- as.numeric(input$reps.fd)
        setfactors.fd <- parse_whole_numbers(input$setfactors, "# of Entries for Each Factor")
        if (!setfactors.fd$ok) {
          shinyalert::shinyalert("Error!!", setfactors.fd$message, type = "error")
          return(NULL)
        }
        setfactors.fd <- setfactors.fd$value
        nt <- length(setfactors.fd)
        if (nt < 2) {
          shinyalert::shinyalert(
            "Error!!", 
            "More than one factor needs to be specified.", 
            type = "error")
          return(NULL)
        }
        TRT <- rep(LETTERS[1:nt], each = reps)
        newlevels <- get.levels(k = setfactors.fd)
        data_fd <- data.frame(
          list(
            factors = rep(levels(as.factor(TRT)), times = setfactors.fd),
            levels = unlist(newlevels)
          )
        )
        colnames(data_fd) <- c("factors", "levels")
        return(list(data_fd = data_fd, treatments = setfactors.fd))
      }
    }) |> 
      shiny::bindEvent(input$RUN.fd)

    fd_inputs <- shiny::reactive({
      shiny::req(get_data_factorial())
      shiny::req(input$plot_start.fd)
      shiny::req(input$Location.fd)
      shiny::req(input$l.fd)
      shiny::req(input$seed.fd)
      shiny::req(input$kindFD)
      shiny::req(input$planter_mov_fd)
      
      setfactors.fd <- get_data_factorial()$treatments
      plot_start <- validate_design(read_whole_numbers(
        input$plot_start.fd, "Starting Plot Number"
      ))
      planter <- input$planter_mov_fd
      site_names <-  as.vector(unlist(strsplit(input$Location.fd, ",")))
      seed <- as.numeric(input$seed.fd)
      reps <- as.numeric(input$reps.fd)
      sites <- as.numeric(input$l.fd)
      type_design <- input$kindFD
      
      return(
        list(
        set_factors = setfactors.fd,
        r = reps,
        planter = planter,
        plot_start = plot_start,
        sites = sites,
        site_names = site_names,
        type_design = type_design,
        seed = seed))
    }) |>
      shiny::bindEvent(input$RUN.fd)

    fd_reactive <- shiny::reactive({
      
      shiny::req(get_data_factorial())
      shiny::req(fd_inputs())
      
      shinyjs::show(id = "downloadCsv.fd")
      
      if (fd_inputs()$type_design == "FD_CRD") {
        type_design <- 1
      } else type_design <- 2
      
      validate_design(full_factorial(
        reps = fd_inputs()$r, 
        l = fd_inputs()$sites, 
        type = type_design, 
        planter = fd_inputs()$planter,
        plotNumber = fd_inputs()$plot_start, 
        seed = fd_inputs()$seed, 
        locationNames = fd_inputs()$site_names,
        data = get_data_factorial()$data_fd
      )) 
      
    }) |> 
      shiny::bindEvent(input$RUN.fd)

    upDateSites <- shiny::reactive({
      shiny::req(input$l.fd)
      locs <- as.numeric(input$l.fd)
      sites <- 1:locs
      return(list(sites = sites))
    })  |> 
      shiny::bindEvent(input$RUN.fd)
    
    output$well_panel_layout_FD <- shiny::renderUI({
      shiny::req(fd_reactive()$fieldBook)
      obj_fd <- fd_reactive()
      layoutOptions_fd <- validate_design(layout_choices(x = obj_fd, stacked = "vertical"))
      stacked_fd <- c("Vertical Stack Panel" = "vertical", 
                        "Horizontal Stack Panel" = "horizontal")
      sites <- as.numeric(input$l.fd)
      shiny::wellPanel(
        shiny::column(2,
               shiny::radioButtons(ns("typlotfd"), "Type of Plot:",
                            c("Entries/Treatments" = 1,
                              "Plots" = 2,
                              "Heatmap" = 3), selected = 1)
        ),
        shiny::fluidRow(
          shiny::column(3,
                 shiny::selectInput(inputId = ns("stackedFD"),
                             label = "Reps layout:", 
                             choices = stacked_fd),
          ),
          shiny::column(3, #align="center",
                 shiny::selectInput(inputId = ns("layoutO_fd"),
                             label = "Layout option:", 
                             choices = layoutOptions_fd)
          ),
          shiny::column(3, #align="center",
                 shiny::selectInput(inputId = ns("locLayout_fd"), label = "Location:",
                             choices = as.numeric(upDateSites()$sites), 
                             selected = 1)
          )
        )
      )
    })
    
    reactive_layoutFD <- app_layout_selection(input, session,
      design = function() fd_reactive(),
      planter = function() fd_inputs()$planter,
      ids = c(layout = "layoutO_fd", stacked = "stackedFD", location = "locLayout_fd")
    )

    app_classic_workflow(input, output, session,
      design = function() fd_reactive(),
      layout = function() reactive_layoutFD(),
      seed = function() fd_inputs()$seed,
      selected = function() return(as.numeric(input$locLayout_fd)),
      spec = classic_workflow_spec("FD"),
      simulation_ready = function() {
        shiny::req(fd_reactive()$fieldBook)
      }
    )

  })
}
