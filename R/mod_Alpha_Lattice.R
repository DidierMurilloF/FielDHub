#' Alpha_Lattice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom utils write.csv
mod_Alpha_Lattice_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::h4("Alpha Lattice Design"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 4,
                   shiny::radioButtons(ns("owndata_alpha"), label = "Import entries' list?", choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                   
                   shiny::conditionalPanel("input.owndata_alpha != 'Yes'", ns = ns,
                                    shiny::numericInput(ns("t.alpha"), label = "Input # of Treatments:",
                                                 value = 36, min = 2)
                                    
                   ),
                   shiny::conditionalPanel("input.owndata_alpha == 'Yes'", ns = ns,
                                    shiny::fluidRow(
                                      shiny::column(8, style=list("padding-right: 28px;"),
                                             shiny::fileInput(inputId = ns("file.alpha"), label = "Upload a CSV File:", multiple = FALSE)),
                                      shiny::column(4, style=list("padding-left: 5px;"),
                                             shiny::radioButtons(inputId = ns("sep.alpha"), "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t"),
                                                          selected = ","))
                                    )        
                   ),
                   shiny::numericInput(inputId = ns("r.alpha"), label = "Input # of Full Reps:", value = 3, min = 2),
                   shiny::selectInput(inputId = ns("k.alpha"), label = "Input # of Plots per IBlock:", choices = ""),
                   shiny::numericInput(inputId = ns("l.alpha"), label = "Input # of Locations:", value = 1, min = 1),
                   
                   shiny::selectInput(inputId = ns("planter_mov_alpha"), label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"), multiple = FALSE,
                               selected = "serpentine"),
                   
                   shiny::fluidRow(
                     shiny::column(6, style=list("padding-right: 28px;"),
                            shiny::textInput(inputId = ns("plot_start.alpha"), "Starting Plot Number:", value = 101)
                     ),
                     shiny::column(6,style=list("padding-left: 5px;"),
                            shiny::textInput(inputId = ns("Location.alpha"), "Input Location:", value = "FARGO")
                     )
                   ),  
                   app_seed_input(ns("myseed.alpha"), value = 16),
                   shiny::fluidRow(
                     shiny::column(6,
                            shiny::actionButton(
                              inputId = ns("RUN.alpha"), 
                              "Run!", 
                              icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                              width = '100%'),
                     ),
                     shiny::column(6,
                            shiny::actionButton(
                              inputId = ns("Simulate.alpha"), 
                              "Simulate!", 
                              icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                              width = '100%'),
                     )
                     
                   ), 
                   shiny::br(),
                   shiny::downloadButton(ns("downloadData.alpha"), "Save experiment (ZIP)", style = "width:100%")
      ),
      
      shiny::mainPanel(
        width = 8,
        shiny::fluidRow(
          shiny::tabsetPanel(
            shiny::tabPanel(
              "Summary Design",
              shiny::br(),
              fieldhub_spinner(
                shiny::verbatimTextOutput(outputId = ns("summary_alpha_lattice"),
                                   placeholder = FALSE), 
                type = 4
              ),
              style = "padding-right: 40px;"
            ),
            shiny::tabPanel("Field Layout",
                     
                     # hidden .csv download button
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(shiny::downloadButton(ns("downloadCsv.alpha"),
                                    label = "CSV + metadata (ZIP)",
                                    icon = shiny::icon("download"),
                                    width = 'auto',
                                    style="color: #337ab7; background-color: #fff; border-color: #2e6da4")),
                     
                     fieldhub_spinner(
                       plotly::plotlyOutput(ns("random_layout"), width = "97%", height = "550px"),type = 5
                     ),
                     shiny::br(),
                     shiny::column(12, shiny::uiOutput(ns("well_panel_layout")))
            ),
            shiny::tabPanel("Field Book",
                     fieldhub_spinner(DT::DTOutput(ns("ALPHA_fieldbook")), type = 5)
            )
          )
        )
      )
    )
  )
}

#' Alpha_Lattice Server Functions
#'
#' @noRd 
mod_Alpha_Lattice_server <- function(id){
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # for showing .csv button on run
    shinyjs::useShinyjs()
    
    init_data_alpha <- shiny::reactive({
      if (input$owndata_alpha == "Yes") {
        shiny::req(input$file.alpha)
        inFile <- input$file.alpha
        data_ingested <- load_file(name = inFile$name,
                                   path = inFile[["datapath"]],
                                   sep = input$sep.alpha,
                                   check = TRUE, design = "alpha")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_up <- as.data.frame(data_up[,1:2])
          data_alpha <- na.omit(data_up)
          colnames(data_alpha) <- c("ENTRY", "NAME")
          treatments = nrow(data_alpha)
          return(list(data_alpha = data_alpha, treatments = treatments))
        } else {
          app_upload_error(data_ingested,
                           missing_columns = "Data input needs at least two columns: ENTRY and NAME")
          return(NULL)
        }
      } else {
        shiny::req(input$t.alpha)
        nt <- as.numeric(input$t.alpha)
        df <- default_entries(nt)
        data_alpha <- df
        treatments = nrow(data_alpha)
        return(list(data_alpha = data_alpha, treatments = treatments))
      }
    })

    list_to_observe <- shiny::reactive({
      shiny::req(init_data_alpha())
      list(
        entry_list = input$owndata_alpha,
        entries = init_data_alpha()$treatments
      )
    })
    
    shiny::observeEvent(list_to_observe(), {
      shiny::req(init_data_alpha())
      options <- valid_block_sizes(
        as.numeric(init_data_alpha()$treatments),
        "alpha_lattice"
      )
      k <- if (length(options) == 0L) "No Options Available" else options
      
      if (length(options) > 2L) {
        selected <- options[ceiling(length(options) / 2)]
      } else selected <- k[1]
      
      shiny::updateSelectInput(session = session, inputId = 'k.alpha',
                        label = "Input # of Plots per IBlock:",
                        choices = k, selected = selected)
    })
    
    get_data_alpha <- shiny::reactive({
      if (is.null(init_data_alpha())) {
        shinyalert::shinyalert(
          "Error!!", 
          "Check input file and try again!", 
          type = "error")
        return(NULL)
      } else return(init_data_alpha())
    }) |>
      shiny::bindEvent(input$RUN.alpha)

    alpha_inputs <- shiny::reactive({
      shiny::req(get_data_alpha())
      shiny::req(input$planter_mov_alpha)
      shiny::req(input$k.alpha)
      shiny::req(input$r.alpha)
      shiny::req(input$plot_start.alpha)
      shiny::req(input$Location.alpha)
      shiny::req(input$l.alpha)
      if (input$k.alpha == "No Options Available") {
        shinyalert::shinyalert(
          "Error!!", 
          "No options for this combination of treatments!", 
          type = "error")
        return(NULL)
      } 
      sites <- as.numeric(input$l.alpha)
      r.alpha <- as.numeric(input$r.alpha)
      k.alpha <- as.numeric(input$k.alpha)
      treatments <- as.numeric(get_data_alpha()$treatments)
      planter <- input$planter_mov_alpha
      plot_start <- validate_design(read_whole_numbers(
        input$plot_start.alpha, "Starting Plot Number"
      ))
      site_names <-  as.vector(unlist(strsplit(input$Location.alpha, ",")))
      seed <- validate_design(app_design_seed(input$myseed.alpha))
      return(list(r = r.alpha, 
                  k = k.alpha, 
                  t = treatments, 
                  planter = planter,
                  plot_start = plot_start, 
                  sites = sites,
                  site_names = site_names,
                  seed = seed))
    }) |>
      shiny::bindEvent(input$RUN.alpha)

    entryListFormatreatments <- data.frame(ENTRY = 1:9, 
                                        NAME = c(paste("Genotype", LETTERS[1:9], sep = "")))
    entriesInfoModal_ALPHA <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(entryListFormatreatments,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        shiny::h4("Entry numbers can be any set of consecutive positive numbers."),
        easyClose = FALSE
      )
    }
    
    toListen <- shiny::reactive({
      list(input$owndata_alpha)
    })
    
    shiny::observeEvent(toListen(), {
      if (input$owndata_alpha == "Yes") {
        shiny::showModal(
          entriesInfoModal_ALPHA()
        )
      }
    })

    ALPHA_reactive <- shiny::eventReactive(input$RUN.alpha, {
      shiny::req(get_data_alpha())
      shiny::req(alpha_inputs())

      # show .csv download button when run
      shinyjs::show(id = "downloadCsv.alpha")
    
      data_alpha <- get_data_alpha()$data_alpha
      
      if (alpha_inputs()$r < 2) {
        shinyalert::shinyalert(
          "Error!!", 
          "Alpha Lattice Design needs at least 2 replicates.", 
          type = "error")
        return(NULL)
      }
      
      validate_design(alpha_lattice(
        t = alpha_inputs()$t, 
        k = alpha_inputs()$k, 
        reps = alpha_inputs()$r,
        l = alpha_inputs()$sites, 
        plotNumber = alpha_inputs()$plot_start, 
        seed = alpha_inputs()$seed,
        locationNames = alpha_inputs()$site_names, 
        data = data_alpha
      ))
    })
    
    output$summary_alpha_lattice <- shiny::renderPrint({
      shiny::req(ALPHA_reactive())
      cat("Randomization was successful!", "\n", "\n")
      print(ALPHA_reactive(), n = 6)
    })
    
    upDateSites <- shiny::reactive({
      shiny::req(alpha_inputs())
      locs <- alpha_inputs()$sites
      sites <- 1:locs
      return(list(sites = sites))
    })

    
    reactive_layoutAlpha <- app_classic_layout(input, output, session,
      design = function() ALPHA_reactive(),
      planter = function() alpha_inputs()$planter,
      spec = classic_workflow_spec("Alpha_Lattice"),
      locations = function() as.numeric(upDateSites()$sites)
    )

    app_classic_workflow(input, output, session,
      design = function() ALPHA_reactive(),
      layout = function() reactive_layoutAlpha(),
      seed = function() alpha_inputs()$seed,
      selected = function() return(as.numeric(input$locLayout)),
      spec = classic_workflow_spec("Alpha_Lattice"),
      simulation_ready = function() {
        shiny::req(input$k.alpha)
        shiny::req(input$r.alpha)
        shiny::req(reactive_layoutAlpha()$fieldBookXY)
      }
    )

  })
}
