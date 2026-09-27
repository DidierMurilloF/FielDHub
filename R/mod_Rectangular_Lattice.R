#' Rectangular_Lattice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_Rectangular_Lattice_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Rectangular Lattice Design"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 4,
                   shiny::radioButtons(ns("owndata_rectangular"), label = "Import entries' list?", choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                   
                   shiny::conditionalPanel("input.owndata_rectangular != 'Yes'", ns = ns,
                                    shiny::numericInput(ns("t.rectangular"), label = "Input # of Treatments:",
                                                 value = 30, min = 2)
                   ),
                   shiny::conditionalPanel("input.owndata_rectangular == 'Yes'", ns = ns,
                                    shiny::fluidRow(
                                      shiny::column(8, style=list("padding-right: 28px;"),
                                             shiny::fileInput(inputId = ns("file.rectangular"), label = "Upload a CSV File:", multiple = FALSE)),
                                      shiny::column(4, style=list("padding-left: 5px;"),
                                             shiny::radioButtons(inputId = ns("sep.rectangular"), "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t"),
                                                          selected = ","))
                                    )        
                   ),
                   
                   shiny::numericInput(inputId = ns("r.rectangular"), label = "Input # of Full Reps:", value = 3, min = 2),
                   shiny::selectInput(inputId = ns("k.rectangular"), label = "Input # of Plots per IBlock:", choices = ""),
                   shiny::numericInput(inputId = ns("l.rectangular"), label = "Input # of Locations:", value = 1, min = 1),
                   
                   shiny::selectInput(inputId = ns("planter_mov_rect"), label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"), multiple = FALSE,
                               selected = "serpentine"),

                   shiny::fluidRow(
                     shiny::column(6, style=list("padding-right: 28px;"),
                            shiny::textInput(inputId = ns("plot_start.rectangular"), "Starting Plot Number:", value = 101)
                     ),
                     shiny::column(6,style=list("padding-left: 5px;"),
                            shiny::textInput(inputId = ns("Location.rectangular"), "Input Location:", value = "FARGO")
                     )
                   ), 
                   app_seed_input(ns("myseed.rectangular"), value = 007),
                   shiny::fluidRow(
                     shiny::column(6,
                            shiny::actionButton(
                              inputId = ns("RUN.rectangular"), 
                              "Run!", 
                              icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                              width = '100%'),
                     ),
                     shiny::column(6,
                            shiny::actionButton(
                              inputId = ns("Simulate.rectangular"), 
                              "Simulate!",
                              icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                              width = '100%'),
                     )
                     
                   ), 
                   shiny::br(),
                   shiny::downloadButton(ns("downloadData.rectangular"), "Save experiment (ZIP)", style = "width:100%")
      ),
      
      shiny::mainPanel(
        width = 8,
        shiny::fluidRow(
          shiny::tabsetPanel(
            shiny::tabPanel(
              "Summary Design",
              shiny::br(),
              fieldhub_spinner(
                shiny::verbatimTextOutput(outputId = ns("summary_rectangular_lattice"),
                                   placeholder = FALSE), 
                type = 4
              ),
              style = "padding-right: 40px;"
            ),
            shiny::tabPanel("Field Layout",
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(shiny::downloadButton(ns("downloadCsv.rectangular"),
                                                    label = "CSV + metadata (ZIP)",
                                                    icon = shiny::icon("download"),
                                                    width = 'auto',
                                                    style="color: #337ab7; background-color: #fff; border-color: #2e6da4")),
                     fieldhub_spinner(
                       plotly::plotlyOutput(ns("random_layout"), 
                                            width = "97%", 
                                            height = "550px"),
                       type = 5
                     ),
                     shiny::br(),
                     shiny::column(12,shiny::uiOutput(ns("well_panel_layout_rt")))
            ),
            shiny::tabPanel("Field Book",
                     fieldhub_spinner(DT::DTOutput(ns("rectangular_fieldbook")), type = 5)
            )
          )
        )
      )
    )
  )
}
    
#' Rectangular_Lattice Server Functions
#'
#' @noRd 
mod_Rectangular_Lattice_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shinyjs::useShinyjs()
    
    init_data_rectangular <- shiny::reactive({
      
      if (input$owndata_rectangular == "Yes") {
      shiny::req(input$file.rectangular)
      inFile <- input$file.rectangular
      data_ingested <- load_file(name = inFile$name,
                                 path = inFile[["datapath"]],
                                 sep = input$sep.rectangular,
                                 check = TRUE, 
                                 design = "rect")
      
      if (names(data_ingested) == "dataUp") {
        data_up <- data_ingested$dataUp
        if (ncol(data_up) < 2) {
          shinyalert::shinyalert(
            "Error!!", 
            "Data input needs at least two columns: ENTRY and NAME.", 
            type = "error")
          return(NULL)
        } 
        data_up <- as.data.frame(data_up[,1:2])
        data_rectangular <- na.omit(data_up)
        colnames(data_rectangular) <- c("ENTRY", "NAME")
        treatments = nrow(data_rectangular)
        return(list(data_rectangular = data_rectangular, treatments = treatments))
      } else {
        app_upload_error(data_ingested,
                         missing_columns = "Data input needs at least two columns: ENTRY and NAME")
        return(NULL)
      }
    } else {
      shiny::req(input$t.rectangular)
      nt <- as.numeric(input$t.rectangular)
      df <- default_entries(nt)
      data_rectangular <- df
      treatments = nrow(data_rectangular)
      return(list(data_rectangular = data_rectangular, treatments = treatments))
      }
    })
    
    list_to_observe <- shiny::reactive({
      shiny::req(init_data_rectangular())
      list(
        entry_list = input$owndata_rectangular,
        entries = init_data_rectangular()$treatments
      )
    })
    
    shiny::observeEvent(list_to_observe(), {
      shiny::req(init_data_rectangular())
      options <- valid_block_sizes(
        as.numeric(init_data_rectangular()$treatments),
        "rectangular_lattice"
      )
      k <- if (length(options) == 0L) "No Options Available" else options
      
      shiny::updateSelectInput(session = session,
                        inputId = 'k.rectangular', 
                        label = "Input # of Plots per IBlock:",
                        choices = k, 
                        selected = k[1])
    })

    get_data_rectangular <- shiny::reactive({
      if (is.null(init_data_rectangular())) {
        shinyalert::shinyalert(
          "Error!!", 
          "Check input file and try again!", 
          type = "error")
        return(NULL)
      } else return(init_data_rectangular())
    }) |>
      shiny::bindEvent(input$RUN.rectangular)

    rectangular_inputs <- shiny::reactive({
      shiny::req(init_data_rectangular())
      shiny::req(input$k.rectangular)
      shiny::req(input$planter_mov_rect)
      shiny::req(input$plot_start.rectangular)
      shiny::req(input$Location.rectangular)
      shiny::req(input$l.rectangular)
      shiny::req(input$r.rectangular)
      if (input$k.rectangular == "No Options Available") {
        shinyalert::shinyalert(
          "Error!!",
          "No options for this combination of treatments!",
          type = "error")
        return(NULL)
      }
      
      treatments <- get_data_rectangular()$treatments
      r.rectangular <- as.numeric(input$r.rectangular)
      k.rectangular <- as.numeric(input$k.rectangular)
      planter <- input$planter_mov_rect
      plot_start <- validate_design(read_whole_numbers(
        input$plot_start.rectangular, "Starting Plot Number"
      ))
      site_names <- as.vector(unlist(strsplit(input$Location.rectangular, ",")))
      seed <- validate_design(app_design_seed(input$myseed.rectangular))
      sites <- as.numeric(input$l.rectangular)
      return(list(r = r.rectangular,
                  k = k.rectangular,
                  t = treatments,
                  planter = planter,
                  plot_start = plot_start,
                  sites = sites,
                  site_names = site_names,
                  seed = seed))
    }) |>
      shiny::bindEvent(input$RUN.rectangular)

    entryListFormat_RECT <- data.frame(ENTRY = 1:9, 
                                       NAME = c(paste("Genotype", LETTERS[1:9], sep = "")))
    entriesInfoModal_RECT <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(entryListFormat_RECT,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        shiny::h4("Entry numbers can be any set of consecutive positive numbers."),
        easyClose = FALSE
      )
    }
    
    toListen <- shiny::reactive({
      list(input$owndata_rectangular)
    })
    
    shiny::observeEvent(toListen(), {
      if (input$owndata_rectangular == "Yes") {
        shiny::showModal(
          entriesInfoModal_RECT()
        )
      }
    })

    RECTANGULAR_reactive <- shiny::reactive({
      
      shiny::req(get_data_rectangular())
      shiny::req(rectangular_inputs())
      
      shinyjs::show(id = "downloadCsv.rectangular", anim = FALSE)
      
      if (rectangular_inputs()$r < 2) {
        shinyalert::shinyalert(
          "Error!!", 
          "Alpha Lattice Design needs at least 2 replicates.", 
          type = "error")
        return(NULL)
      }
      
      data <- get_data_rectangular()$data_rectangular

      validate_design(rectangular_lattice(
        t = rectangular_inputs()$t, 
        k = rectangular_inputs()$k, 
        reps = rectangular_inputs()$r,
        l = rectangular_inputs()$sites, 
        plotNumber = rectangular_inputs()$plot_start,
        seed = rectangular_inputs()$seed, 
        locationNames = rectangular_inputs()$site_names, 
        data = data
      )) 
    }) |>
      shiny::bindEvent(input$RUN.rectangular)
    
    output$summary_rectangular_lattice <- shiny::renderPrint({
      shiny::req(RECTANGULAR_reactive())
      cat("Randomization was successful!", "\n", "\n")
      print(RECTANGULAR_reactive(), n = 6)
    })

    upDateSites_RT <- shiny::reactive({
      shiny::req(rectangular_inputs())
      locs <- rectangular_inputs()$sites
      sites <- 1:locs
      return(list(sites = sites))
    })
    
    
    reactive_layoutRect <- app_classic_layout(input, output, session,
      design = function() RECTANGULAR_reactive(),
      planter = function() rectangular_inputs()$planter,
      spec = classic_workflow_spec("Rectangular_Lattice"),
      locations = function() as.numeric(upDateSites_RT()$sites)
    )

    app_classic_workflow(input, output, session,
      design = function() RECTANGULAR_reactive(),
      layout = function() reactive_layoutRect(),
      seed = function() rectangular_inputs()$seed,
      selected = function() return(as.numeric(input$locLayout_rt)),
      spec = classic_workflow_spec("Rectangular_Lattice"),
      simulation_ready = function() {
        shiny::req(input$k.rectangular)
        shiny::req(input$r.rectangular)
        shiny::req(reactive_layoutRect()$fieldBookXY)
      }
    )

  })
}
