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
                   shiny::numericInput(inputId = ns("myseed.rectangular"), label = "Random Seed:",
                                value = 007, min = 1),
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
      shiny::req(input$myseed.rectangular)
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
      seed <- as.numeric(input$myseed.rectangular)
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
    
    output$well_panel_layout_rt <- shiny::renderUI({
      shiny::req(RECTANGULAR_reactive()$fieldBook)
      df <- RECTANGULAR_reactive()$fieldBook
      locs_rt <- length(levels(as.factor(df$LOCATION)))
      repsRect <- length(levels(as.factor(df$REP)))
      if ((repsRect >= 4 & repsRect %% 2 == 0) | (repsRect >= 4 & sqrt(repsRect) %% 1 == 0)) {
        stacked <- c("Vertical Stack Panel" = "vertical", "Horizontal Stack Panel" = "horizontal",  
                       "Grid Panel" = "grid_panel")
      } else {
        stacked <- c("Vertical Stack Panel" = "vertical", "Horizontal Stack Panel" = "horizontal")
      }
      obj_rt <- RECTANGULAR_reactive()
      layoutOptions_rt <- validate_design(layout_choices(x = obj_rt, stacked = "vertical"))
      shiny::wellPanel(
        shiny::column(3,
               shiny::radioButtons(ns("typlotRT"), "Type of Plot:",
                            c("Entries/Treatments" = 1,
                              "Plots" = 2,
                              "Heatmap" = 3))
        ),
        shiny::fluidRow(
          shiny::column(3,
                 shiny::selectInput(inputId = ns("stackedRT"), label = "Reps layout:",
                             choices = stacked)
          ),
          shiny::column(2,
                 shiny::selectInput(inputId = ns("layoutO_rt"), label = "Layout option:", choices = layoutOptions_rt, selected = 1)
          ),
          shiny::column(2,
                 shiny::selectInput(inputId = ns("locLayout_rt"), label = "Location:", choices = as.numeric(upDateSites_RT()$sites))
          )
        )
      )
    })
    
    shiny::observeEvent(input$stackedRT, {
      shiny::req(input$stackedRT)
      shiny::req(input$l.rectangular)
      obj_rt <- RECTANGULAR_reactive()
      NewlayoutOptions <- validate_design(layout_choices(x = obj_rt, stacked = input$stackedRT))
      shiny::updateSelectInput(session = session, inputId = 'layoutO_rt',
                        label = "Layout option:",
                        choices = NewlayoutOptions,
                        selected = 1
      )
    })
    
    reset_selection <- shiny::reactiveValues(reset = 0)
    
    shiny::observeEvent(input$stackedRT, {
      reset_selection$reset <- 1
    })
    
    shiny::observeEvent( input$layoutO_rt, {
      reset_selection$reset <- 0
    })
    
    reactive_layoutRect <- shiny::reactive({
      shiny::req(input$stackedRT)
      shiny::req(input$layoutO_rt)
      shiny::req(input$locLayout_rt)
      shiny::req(rectangular_inputs()$planter)
      shiny::req(RECTANGULAR_reactive())
      obj_rt <- RECTANGULAR_reactive()
      
      if (reset_selection$reset == 1) {
        opt_rt <- 1
      } else opt_rt <- as.numeric(input$layoutO_rt)
      
      locSelected <- as.numeric(input$locLayout_rt)
      try(plot_layout(x = obj_rt, layout = opt_rt,
                      planter = rectangular_inputs()$planter,
                      l = locSelected, 
                      stacked = input$stackedRT), 
          silent = TRUE)
    })
    
    
    valsRECT <- shiny::reactiveValues(maxV.rectangular= NULL, minV.rectangular= NULL, trail.rectangular= NULL)
    
    simuModal.rectangular<- function(failed = FALSE) {
      shiny::modalDialog(
        shiny::selectInput(inputId = ns("trailsRECT"), label = "Select One:", choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        shiny::conditionalPanel("input.trailsRECT == 'Other'", ns = ns,
                         shiny::textInput(inputId = ns("OtherRECT"), label = "Input Trial Name:", value = NULL)
        ),
        shiny::fluidRow(
          shiny::column(6,
                 shiny::numericInput(inputId = ns("min.rectangular"), "Input the min value", value = NULL)
          ),
          shiny::column(6,
                 shiny::numericInput(inputId = ns("max.rectangular"), "Input the max value", value = NULL)
                 
          )
          
        ),
        
        if (failed)
          shiny::div(shiny::tags$b("Invalid input of data max and min", style = "color: red;")),
        
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(inputId = ns("ok.rectangular"), "GO")
        )
        
      )
      
    }
    
    shiny::observeEvent(input$Simulate.rectangular, {
      shiny::req(input$k.rectangular)
      shiny::req(input$r.rectangular)
      shiny::req(reactive_layoutRect()$fieldBookXY)
      shiny::showModal(
        simuModal.rectangular()
      )
    })
    
    shiny::observeEvent(input$ok.rectangular, {
      shiny::req(input$max.rectangular, input$min.rectangular)
      if (input$max.rectangular> input$min.rectangular&& input$min.rectangular!= input$max.rectangular) {
        valsRECT$maxV.rectangular<- input$max.rectangular
        valsRECT$minV.rectangular<- input$min.rectangular
        if(input$trailsRECT == "Other") {
          shiny::req(input$OtherRECT)
          if(!is.null(input$OtherRECT)) {
            valsRECT$trail.rectangular <- as.character(input$OtherRECT)
          }else shiny::showModal(simuModal.rectangular(failed = TRUE))
        }else {
          valsRECT$trail.rectangular <- as.character(input$trailsRECT)
        }
        shiny::removeModal()
      }else {
        shiny::showModal(
          simuModal.rectangular(failed = TRUE)
        )
      }
    })
    
    
    simuDataRECT <- shiny::reactive({
      shiny::req(reactive_layoutRect()$allSitesFieldbook)
      if(!is.null(valsRECT$maxV.rectangular) && !is.null(valsRECT$minV.rectangular) && !is.null(valsRECT$trail.rectangular)) {
        max <- as.numeric(valsRECT$maxV.rectangular)
        min <- as.numeric(valsRECT$minV.rectangular)
        df.rectangular <- reactive_layoutRect()$allSitesFieldbook
        simulation <- validate_design(simulate_classic_field_book(
          field_book = df.rectangular, min_value = min, max_value = max,
          response_name = valsRECT$trail.rectangular, seed = rectangular_inputs()$seed,
          order_by_id = FALSE
        ))
        df.rectangular <- simulation$field_book
        a <- ncol(df.rectangular)
      }else {
        simulation <- NULL
        df.rectangular <- reactive_layoutRect()$allSitesFieldbook
        a <- ncol(df.rectangular)
      }
      return(list(df = df.rectangular, a = a, simulation = simulation))
    })
    
    heatmapInfoModal_Rect <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    locNum <- shiny::reactive(
      return(as.numeric(input$locLayout_rt))
    )
    
    heatmap_obj <- shiny::reactive({
      shiny::req(simuDataRECT()$df)
      book <- simuDataRECT()$df
      response <- as.character(valsRECT$trail.rectangular)
      if (length(response) == 1L && response %in% names(book)) {
        validate_design(app_field_heatmap(
          book, response_name = response, selected = locNum(),
          label_column = "ENTRY", label_title = "Entry",
          include_site = TRUE, include_checks = FALSE
        ))
      } else {
        shiny::showModal(heatmapInfoModal_Rect())
        NULL
      }
    })
    
    output$random_layout <- plotly::renderPlotly({
      shiny::req(reactive_layoutRect())
      shiny::req(RECTANGULAR_reactive())
      shiny::req(input$typlotRT)
      if (input$typlotRT == 1) {
        reactive_layoutRect()$out_layout
      } else if (input$typlotRT == 2) {
        reactive_layoutRect()$out_layoutPlots
      } else {
        shiny::req(heatmap_obj())
        heatmap_obj()
      }
    })
    
    output$rectangular_fieldbook <- DT::renderDataTable({
      shiny::req(simuDataRECT())
      df <- simuDataRECT()$df
      validate_design(app_field_book_table(
        df, factor_columns = c("LOCATION", "PLOT", "ROW", "COLUMN", "REP", "IBLOCK", "UNIT", "ENTRY"),
        height = 500
      ))
    })
    
    output$downloadData.rectangular <- app_csv_archive(
      filename = function() {
        loc <- paste("Rectangular_Lattice_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      data = function() as.data.frame(simuDataRECT()$df),
      design = RECTANGULAR_reactive,
      field_book = function() simuDataRECT()$df,
      simulation = function() simuDataRECT()$simulation,
      layout = function() reactive_layoutRect()$layout_metadata,
      kind = "field_book"
    )
    
    csv_data <- shiny::reactive({
      shiny::req(simuDataRECT()$df)
      df <- simuDataRECT()$df
      shiny::req(input$typlotRT)
      if (input$typlotRT == 2) {
        export_layout(df, locNum(), TRUE)
      } else {
        export_layout(df, locNum())
      }
    })
    
    
    # Downloadable csv of selected dataset ----
    output$downloadCsv.rectangular <- app_csv_archive(
      filename = function() {
        loc <- paste("Rectangular_Lattice_Layout", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      data = function() as.data.frame(csv_data()$file),
      design = RECTANGULAR_reactive,
      field_book = function() simuDataRECT()$df,
      simulation = function() simuDataRECT()$simulation,
      layout = function() reactive_layoutRect()$layout_metadata,
      kind = "layout"
    )
    
    app_reproduction_outputs(output, RECTANGULAR_reactive)
  })
}
