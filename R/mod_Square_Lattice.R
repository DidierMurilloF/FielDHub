#' Square_Lattice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_Square_Lattice_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Square Lattice Design"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 4,
                   shiny::radioButtons(
                     ns("owndata_square"), 
                     label = "Import entries' list?",
                     choices = c("Yes", "No"), 
                     selected = "No",
                     inline = TRUE,
                     width = NULL, 
                     choiceNames = NULL, 
                     choiceValues = NULL),
                   
                   shiny::conditionalPanel("input.owndata_square != 'Yes'", ns = ns,
                                    shiny::numericInput(ns("t.square"), label = "Input # of Treatments:",
                                                 value = 49, min = 2)
                   ),
                   shiny::conditionalPanel("input.owndata_square == 'Yes'", ns = ns,
                                    shiny::fluidRow(
                                      shiny::column(8, style=list("padding-right: 28px;"),
                                             shiny::fileInput(inputId = ns("file.square"), label = "Upload a CSV File:", multiple = FALSE)),
                                      shiny::column(4, style=list("padding-left: 5px;"),
                                             shiny::radioButtons(inputId = ns("sep.square"), "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t"),
                                                          selected = ","))
                                    )        
                   ),
                   shiny::numericInput(inputId = ns("r.square"), label = "Input # of Full Reps:", value = 3, min = 2),
                   shiny::selectInput(inputId = ns("k.square"), label = "Input # of Plots per IBlock:", choices = ""),
                   shiny::numericInput(inputId = ns("l.square"), label = "Input # of Locations:", value = 1, min = 1),
                   
                   shiny::selectInput(inputId = ns("planter_mov_square"), label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"), multiple = FALSE,
                               selected = "serpentine"),
                   
                   shiny::fluidRow(
                     shiny::column(6, style=list("padding-right: 28px;"),
                            shiny::textInput(inputId = ns("plot_start.square"), "Starting Plot Number:", value = 101)
                     ),
                     shiny::column(6,style=list("padding-left: 5px;"),
                            shiny::textInput(inputId = ns("Location.square"), "Input Location:", value = "FARGO")
                     )
                   ),
                   app_seed_input(ns("myseed.square"), value = 5),
                    
                   shiny::fluidRow(
                     shiny::column(6,# style=list("padding-right: 28px;"),
                            shiny::actionButton(
                              inputId = ns("RUN.square"), 
                              "Run!", 
                              icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                              width = '100%'),
                     ),
                     shiny::column(6,#style=list("padding-left: 5px;"),
                            
                            shiny::actionButton(
                              inputId = ns("Simulate.square"), 
                              "Simulate!", 
                              icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                              width = '100%'),
                     )
                     
                   ), 
                   shiny::br(),
                   shiny::downloadButton(ns("downloadData.square"), "Save experiment (ZIP)", style = "width:100%")
      ),
      
      shiny::mainPanel(
        width = 8,
        shiny::fluidRow(
          shiny::tabsetPanel(
            shiny::tabPanel(
              "Summary Design",
              shiny::br(),
              shiny::div(
                fieldhub_spinner(
                  shiny::verbatimTextOutput(outputId = ns("summary_square_lattice"),
                                     placeholder = FALSE), 
                  type = 4
                ),
                style = "padding-right: 40px;"
              )
            ),
            shiny::tabPanel("Field Layout",
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(shiny::downloadButton(ns("downloadCsv.square"),
                                                    label = "CSV + metadata (ZIP)",
                                                    icon = shiny::icon("download"),
                                                    width = 'auto',
                                                    style="color: #337ab7; background-color: #fff; border-color: #2e6da4")),
                     fieldhub_spinner(
                       plotly::plotlyOutput(ns("random_layout"), width = "97%", height = "550px"),type = 5
                     ),
                     shiny::br(),
                     shiny::column(12, shiny::uiOutput(ns("well_panel_layout_sq")))
            ),
            shiny::tabPanel("Field Book",
                     fieldhub_spinner(DT::DTOutput(ns("square_fieldbook")), type = 5)
            )
          )
        )
      )
    )
  )
}

#' Square_Lattice Server Functions
#'
#' @noRd 
mod_Square_Lattice_server <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    init_data_square <- shiny::reactive({
      
      if (input$owndata_square == "Yes") {
        shiny::req(input$file.square)
        inFile <- input$file.square
        data_ingested <- load_file(name = inFile$name, 
                                   path = inFile[["datapath"]],
                                   sep = input$sep.square, check = TRUE, design = "square")
        
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
          data_square <- na.omit(data_up)
          colnames(data_square) <- c("ENTRY", "NAME")
          treatments = nrow(data_square)
          return(list(data_square = data_square, treatments = treatments))
        } else {
          app_upload_error(data_ingested,
                           missing_columns = "Data input needs at least two columns: ENTRY and NAME")
          return(NULL)
        }
      } else {
        shiny::req(input$t.square)
        nt <- as.numeric(input$t.square)
        df <- default_entries(nt)
        data_square <- df
        treatments = nrow(data_square)
        return(list(data_square = data_square, treatments = treatments))
      }     
    })
    
    list_to_observe <- shiny::reactive({
      shiny::req(init_data_square())
      list(
        entry_list = input$owndata_square,
        entries = init_data_square()$treatments
      )
    })
    
    shiny::observeEvent(list_to_observe(), {
      
      options <- valid_block_sizes(
        as.numeric(init_data_square()$treatments),
        "square_lattice"
      )
      k <- if (length(options) == 0L) "No Options Available" else options

      shiny::updateSelectInput(session = session,
                        inputId = 'k.square', 
                        label = "Input # of Plots per IBlock:",
                        choices = k,
                        selected = k[1])
    })
    
    # getData.square
    get_data_square <- shiny::reactive({
      if (is.null(init_data_square())) {
        shinyalert::shinyalert(
          "Error!!", 
          "Check input file and try again!", 
          type = "error")
        return(NULL)
      } else return(init_data_square())
    }) |>
      shiny::bindEvent(input$RUN.square)

    square_inputs <- shiny::reactive({
      shiny::req(get_data_square())
      shiny::req(input$k.square)
      shiny::req(input$owndata_square)
      shiny::req(input$planter_mov_square)
      shiny::req(input$plot_start.square)
      shiny::req(input$Location.square)
      shiny::req(input$l.square)
      shiny::req(input$r.square)
      r.square <- as.numeric(input$r.square)
      k.square <- as.numeric(input$k.square)
      if (input$k.square == "No Options Available") {
        shinyalert::shinyalert(
          "Error!!", 
          "No options for this combination of treatments!", 
          type = "error")
        return(NULL)
      } 
      plot_start <- validate_design(read_whole_numbers(
        input$plot_start.square, "Starting Plot Number"
      ))
      planter <- input$planter_mov_square
      site_names <- as.vector(unlist(strsplit(input$Location.square, ",")))
      seed <- validate_design(app_design_seed(input$myseed.square))
      sites <- as.numeric(input$l.square)
      treatments <- get_data_square()$treatments
      
      return(list(r = r.square, 
                  k = k.square, 
                  t = treatments, 
                  planter = planter,
                  plot_start = plot_start, 
                  sites = sites,
                  site_names = site_names,
                  seed = seed))
    }) |>
      shiny::bindEvent(input$RUN.square)

    entryListFormat_SQUARE <- data.frame(ENTRY = 1:9, 
                                         NAME = c(paste("Genotype", LETTERS[1:9], sep = "")))
    entriesInfoModal_SQUARE <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(entryListFormat_SQUARE,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        shiny::h4("Entry numbers can be any set of consecutive positive numbers."),
        easyClose = FALSE
      )
    }
    
    toListen <- shiny::reactive({
      list(input$owndata_square)
    })
    
    shiny::observeEvent(toListen(), {
      if (input$owndata_square == "Yes") {
        shiny::showModal(
          entriesInfoModal_SQUARE()
        )
      }
    })

    SQUARE_reactive <- shiny::eventReactive(input$RUN.square,{
      
      shiny::req(get_data_square())
      shiny::req(square_inputs())
      
      shinyjs::show(id = "downloadCsv.square", anim = FALSE)
      
      data_square <- get_data_square()$data_square
      
      if (square_inputs()$r < 2) {
        shinyalert::shinyalert(
          "Error!!", 
          "Square Lattice Design needs at least 2 replicates.", 
          type = "error")
        return(NULL)
      }
      
      validate_design(square_lattice(
        t = square_inputs()$t, 
        k = square_inputs()$k, 
        reps = square_inputs()$r,
        l = square_inputs()$sites, 
        plotNumber = square_inputs()$plot_start, 
        seed = square_inputs()$seed, 
        locationNames = square_inputs()$site_names, 
        data = data_square
      )) 
      
    })
    
    output$summary_square_lattice <- shiny::renderPrint({
      shiny::req(SQUARE_reactive())
        cat("Randomization was successful!", "\n", "\n")
        print(SQUARE_reactive(), n = 6)
    })
    
    upDateSites_SQ <- shiny::reactive({
      shiny::req(square_inputs())
      locs <- square_inputs()$sites
      sites <- 1:locs
      return(list(sites = sites))
    })
    
    
    reactive_layoutSquare <- app_classic_layout(input, output, session,
      design = function() SQUARE_reactive(),
      planter = function() square_inputs()$planter,
      spec = classic_workflow_spec("Square_Lattice"),
      locations = function() as.numeric(upDateSites_SQ()$sites)
    )
    
    output$layout.output_sq <- shiny::renderPlot({
      shiny::req(reactive_layoutSquare())
      shiny::req(SQUARE_reactive())
      shiny::req(input$typlotSQ)
      if (input$typlotSQ == 1) {
        reactive_layoutSquare()$out_layout
      } else if (input$typlotSQ == 2) {
        reactive_layoutSquare()$out_layoutPlots
      }
    })
    
    app_classic_workflow(input, output, session,
      design = function() SQUARE_reactive(),
      layout = function() reactive_layoutSquare(),
      seed = function() square_inputs()$seed,
      selected = function() return(as.numeric(input$locLayout_sq)),
      spec = classic_workflow_spec("Square_Lattice"),
      simulation_ready = function() {
        shiny::req(input$k.square)
        shiny::req(input$r.square)
        shiny::req(reactive_layoutSquare()$fieldBookXY)
      }
    )

  })
}
