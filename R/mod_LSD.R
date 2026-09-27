#' LSD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_LSD_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Latin Square Design"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        shiny::radioButtons(inputId = ns("owndataLSD"),
                     label = "Import entries' list?", 
                     choices = c("Yes", "No"), 
                     selected = "No",
                     inline = TRUE, 
                     width = NULL, 
                     choiceNames = NULL, 
                     choiceValues = NULL), 
        
        shiny::conditionalPanel(
          condition = "input.owndataLSD == 'Yes'", 
          ns = ns,
          shiny::fluidRow(
            shiny::column(8, style=list("padding-right: 28px;"),
                   shiny::fileInput(ns("file.LSD"),
                             label = "Upload a CSV File:", 
                             multiple = FALSE)),
            
            shiny::column(4,style=list("padding-left: 5px;"),
                   shiny::radioButtons(ns("sep.lsd"), "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","))
          )             
        ),
        
        shiny::conditionalPanel(
          condition = "input.owndataLSD != 'Yes'", ns = ns,
                         
          shiny::numericInput(ns("n.lsd"),
                       label = "Input # of Treatments:",
                       value = 5, 
                       min = 2),             
        ),
        
        shiny::numericInput(ns("reps.lsd"),
                     label = "Input # of Full Reps (Squares):",
                     value = 1, 
                     min = 1),
        shiny::selectInput(inputId = ns("planter.lsd"),
                    label = "Plot Order Layout:",
                    choices = c("serpentine", "cartesian"), 
                    multiple = FALSE,
                    selected = "serpentine"),
        shiny::fluidRow(
          shiny::column(6, style=list("padding-right: 28px;"),
                 shiny::textInput(ns("plot_start.lsd"),
                           "Starting Plot Number:", 
                           value = 101)
          ),
          shiny::column(6,style=list("padding-left: 5px;"),
                 shiny::textInput(ns("Location.lsd"),
                           "Input Location:", 
                           value = "FARGO")
          )
        ),
        shiny::numericInput(ns("seed.lsd"),
                     label = "Random Seed:", 
                     value = 123, 
                     min = 1),
        
        shiny::fluidRow(
          shiny::column(6,
                 shiny::actionButton(
                   inputId = ns("RUN.lsd"), 
                   label = "Run!", 
                   icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                   width = '100%'),
          ),
          shiny::column(6,
                 shiny::actionButton(
                   ns("Simulate.lsd"), 
                   label = "Simulate!", 
                   icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                   width = '100%'),
          )
          
        ), 
        shiny::br(),
        shiny::downloadButton(ns("downloadData.lsd"),
                       "Save experiment (ZIP)",
                       style = "width:100%")
                   
      ),
      shiny::mainPanel(width = 8,
          shiny::fluidRow(
            shiny::tabsetPanel(
              shiny::tabPanel("Field Layout",
                       shinyjs::useShinyjs(),
                       shinyjs::hidden(
                         shiny::downloadButton(
                           ns("downloadCsv.lsd"), 
                           label = "CSV + metadata (ZIP)",
                           icon = shiny::icon("download"),
                           width = 'auto',
                           style="color: #337ab7; background-color: #fff; border-color: #2e6da4")
                        ),
                       plotly::plotlyOutput(ns("layout_lsd"),
                                            width = "97%",
                                            height = "550px"),
                       shiny::br(),
                       shiny::column(12, shiny::uiOutput(ns("well_panel_layout_LSD")))
              ),
              shiny::tabPanel("Field Book",
                       fieldhub_spinner(
                         DT::DTOutput(ns("LSD_fieldbook")), 
                         type = 5
                      )
              )
            )
          )
      )
    ) 
  )
}

#' LSD Server Functions
#'
#' @noRd 
mod_LSD_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    shinyjs::useShinyjs()
    
    entryListFormat_LSD <- data.frame(
      list(ROW = paste("Period", 1:5, sep = ""),
           COLUMN = paste("Cow", 1:5, sep = ""),
           TREATMENT = paste("Diet", 1:5, sep = ""))
    )
    entriesInfoModal_LSD <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message",
                            style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(entryListFormat_LSD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        easyClose = FALSE
      )
    }
    
    toListen <- shiny::reactive({
      list(input$owndataLSD)
    })
    
    shiny::observeEvent(toListen(), {
      if (input$owndataLSD == "Yes") {
        shiny::showModal(
          entriesInfoModal_LSD()
        )
      }
    })
    
    
    get_data_lsd <- shiny::reactive({
      if (input$owndataLSD == "Yes") {
        shiny::req(input$file.LSD)
        shiny::req(input$sep.lsd)
        inFile <- input$file.LSD
        data_ingested <- load_file(name = inFile$name, 
                                path = inFile[["datapath"]],
                                sep = input$sep.lsd,
                                check = TRUE, 
                                design = "lsd")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_up <- as.data.frame(data_up[,1:3])
          data_lsd <- na.omit(data_up)
          colnames(data_lsd) <- c("ROW", "COLUMN", "TREATMENT")
          return(list(data_lsd = data_lsd))
        } else {
          app_upload_error(data_ingested,
                           missing_columns = "Data input needs at least one column: ROW, COLUMN, and  TREATMENT")
          return(NULL)
        }
      }
    })
    
    
    lsd_inputs <- shiny::reactive({
      
      shiny::req(input$plot_start.lsd)
      shiny::req(input$Location.lsd)
      shiny::req(input$reps.lsd)
      shiny::req(input$seed.lsd)
      
      if (input$owndataLSD == "Yes") {
        shiny::req(get_data_lsd())
        n.lsd <- NULL
        reps.lsd <- as.numeric(input$reps.lsd)
        data.lsd <- get_data_lsd()$data_lsd
        n <- as.numeric(nrow(data.lsd))
        n.lsd <- n
        if (n > 10) {
          shinyalert::shinyalert(
            "Error!!", 
            "Only up to 10 treatments are allowed.", 
            type = "error")
          return(NULL)
        }
      } else {
        shiny::req(input$n.lsd)
        n <- as.numeric(input$n.lsd)
        if (n > 10) {
          shinyalert::shinyalert(
            "Error!!", 
            "Only up to 10 treatments are allowed.", 
            type = "error")
          return(NULL)
        }
        n.lsd <- n
        reps.lsd <- as.numeric(input$reps.lsd)
        data.lsd <- NULL
      }
      
      plot_number <- validate_design(read_whole_numbers(
        input$plot_start.lsd, "Starting Plot Number"
      ))
      loc.lsd <-  as.vector(unlist(strsplit(input$Location.lsd, ",")))
      seed.number.lsd <- as.numeric(input$seed.lsd)
      planting_lsd <- input$planter.lsd

      return(
        list(
        t = n.lsd, 
        reps = reps.lsd, 
        plot_number = plot_number[1],
        planter = planting_lsd,
        location_names = loc.lsd[1], 
        data = data.lsd,
        seed = seed.number.lsd)
      )
    }) |>
      shiny::bindEvent(input$RUN.lsd)
    
    latinsquare_reactive <- shiny::reactive({
      
      shiny::req(lsd_inputs())
      
      shinyjs::show(id = "downloadCsv.lsd")
      
      validate_design(latin_square(
        t = lsd_inputs()$t, 
        reps = lsd_inputs()$reps, 
        plotNumber = lsd_inputs()$plot_number,
        planter = lsd_inputs()$planter,
        seed = lsd_inputs()$seed, 
        locationNames = lsd_inputs()$location_names, 
        data = lsd_inputs()$data
      ))
      
    }) |> 
      shiny::bindEvent(input$RUN.lsd)
    
    
    output$well_panel_layout_LSD <- shiny::renderUI({
      shiny::req(latinsquare_reactive()$fieldBook)
      shiny::req(latinsquare_reactive())
      obj_lsd <- latinsquare_reactive()
      allBooks_lsd <- plot_layout(x = obj_lsd, 
                                 layout = 1,
                                 stacked = "vertical")$newBooks
      nBooks_lsd <- length(allBooks_lsd)
      layoutOptions_lsd <- 1:nBooks_lsd
      df <- latinsquare_reactive()$fieldBook
      stacked_lsd <- c("Vertical Stack Panel" = "vertical", 
                          "Horizontal Stack Panel" = "horizontal")
      nBooks_lsd <- length(allBooks_lsd)
      shiny::wellPanel(
        shiny::column(3,
               shiny::radioButtons(ns("typlotLSD"), "Type of Plot:",
                            c("Entries/Treatments" = 1,
                              "Plots" = 2,
                              "Heatmap" = 3))
        ),
        shiny::fluidRow(
          shiny::column(3,
                 shiny::selectInput(inputId = ns("stackedLSD"),
                             label = "Reps layout:",
                             choices = stacked_lsd),
          ),
          shiny::column(3,
                 shiny::selectInput(inputId = ns("layoutO_lsd"),
                             label = "Layout option:", 
                             choices = layoutOptions_lsd)
          )
        )
      )
    })
    
    shiny::observeEvent(input$stackedLSD, {
      shiny::req(input$stackedLSD)
      shiny::req(lsd_inputs())
      obj_lsd <- latinsquare_reactive()
      allBooks <- try(plot_layout(x = obj_lsd, 
                                  layout = 1, 
                                  planter = lsd_inputs()$planter,
                                  stacked = input$stackedLSD)$newBooks, 
                      silent = TRUE)
      nBooks <- length(allBooks)
      NewlayoutOptions <- 1:nBooks
      shiny::updateSelectInput(session = session, inputId = 'layoutO_lsd',
                        label = "Layout option:",
                        choices = NewlayoutOptions,
                        selected = 1
      )
    })
    
    
    reset_selection <- shiny::reactiveValues(reset = 0)
    
    shiny::observeEvent(input$stackedLSD, {
      reset_selection$reset <- 1
    })
    
    shiny::observeEvent(input$layoutO_lsd, {
      reset_selection$reset <- 0
    })
    
    reactive_layoutLSD <- shiny::reactive({
      shiny::req(input$layoutO_lsd)
      shiny::req(latinsquare_reactive())
      shiny::req(lsd_inputs()$planter)
      obj_lsd <- latinsquare_reactive()
      if (reset_selection$reset == 1) {
        opt_lsd <- 1
      } else opt_lsd <- as.numeric(input$layoutO_lsd)
      planting_lsd <- lsd_inputs()$planter
      try(plot_layout(x = obj_lsd,
                      layout = opt_lsd,
                      stacked = input$stackedLSD,
                      planter = planting_lsd,
                      l = 1),
          silent = TRUE)
    })
    
    valsLSD <- shiny::reactiveValues(maxV.lsd = NULL,
                              minV.lsd = NULL, 
                              trail.lsd = NULL)
    
    simuModal.lsd <- function(failed = FALSE) {
      shiny::modalDialog(
        shiny::selectInput(inputId = ns("trailsLSD"),
                    label = "Select One:", 
                    choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        shiny::conditionalPanel(
          condition = "input.trailsLSD == 'Other'", ns = ns,
          shiny::textInput(inputId = ns("OtherLSD"),
                    label = "Input Trial Name:", 
                    value = NULL)
        ),
        shiny::fluidRow(
          shiny::column(6,
                 shiny::numericInput(ns("min.lsd"),
                              "Input the min value", 
                              value = NULL)
          ),
          shiny::column(6,
                 shiny::numericInput(ns("max.lsd"),
                              "Input the max value", 
                              value = NULL)
          )
        ),
        
        if (failed)
          shiny::div(shiny::tags$b("Invalid input of data max and min",
                     style = "color: red;")),
        
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("ok.lsd"), "GO")
        )
      )
    }
    shiny::observeEvent(input$Simulate.lsd, {
      shiny::req(latinsquare_reactive()$fieldBook)
      shiny::showModal(
        simuModal.lsd()
      )
    })
    
    shiny::observeEvent(input$ok.lsd, {
      shiny::req(input$max.lsd, input$min.lsd)
      if (input$max.lsd > input$min.lsd && input$min.lsd != input$max.lsd) {
        valsLSD$maxV.lsd <- input$max.lsd
        valsLSD$minV.lsd <- input$min.lsd
        if(input$trailsLSD == "Other") {
          shiny::req(input$OtherLSD)
          if(!is.null(input$OtherLSD)) {
            valsLSD$trail.lsd <- input$OtherLSD
          }else shiny::showModal(simuModal.lsd(failed = TRUE))
        }else {
          valsLSD$trail.lsd <- as.character(input$trailsLSD)
        }
        shiny::removeModal()
      }else {
        shiny::showModal(
          simuModal.lsd(failed = TRUE)
        )
      }
    })
    
    
    simuDataLSD <- shiny::reactive({
      shiny::req(latinsquare_reactive()$fieldBook)
      if(!is.null(valsLSD$maxV.lsd) && !is.null(valsLSD$minV.lsd) && 
         !is.null(valsLSD$trail.lsd)) {
        max <- as.numeric(valsLSD$maxV.lsd)
        min <- as.numeric(valsLSD$minV.lsd)
        df.lsd <- reactive_layoutLSD()$allSitesFieldbook
        simulation <- validate_design(simulate_classic_field_book(
          field_book = df.lsd, min_value = min, max_value = max,
          response_name = valsLSD$trail.lsd, seed = lsd_inputs()$seed,
          order_by_id = TRUE
        ))
        df.lsd <- simulation$field_book
      }else {
        simulation <- NULL
        df.lsd <- reactive_layoutLSD()$allSitesFieldbook
      }
      return(list(df = df.lsd, simulation = simulation))
    })
    
    heatmapInfoModal_LSD <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message",
                            style = "color: red;")),
        shiny::h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    heatmap_obj <- shiny::reactive({
      shiny::req(simuDataLSD()$df)
      book <- simuDataLSD()$df
      response <- as.character(valsLSD$trail.lsd)
      if (length(response) == 1L && response %in% names(book)) {
        validate_design(app_field_heatmap(
          book, response_name = response, selected = 1L,
          label_column = "TREATMENT", label_title = "Treatment",
          include_site = TRUE, include_checks = FALSE
        ))
      } else {
        shiny::showModal(heatmapInfoModal_LSD())
        NULL
      }
    })
    
    output$layout_lsd <- plotly::renderPlotly({
      shiny::req(reactive_layoutLSD())
      shiny::req(latinsquare_reactive())
      shiny::req(input$typlotLSD)
      if (input$typlotLSD == 1) {
        reactive_layoutLSD()$out_layout
      } else if (input$typlotLSD == 2) {
        reactive_layoutLSD()$out_layoutPlots
      } else {
        shiny::req(heatmap_obj())
        heatmap_obj()
      }
    })
    
    output$LSD_fieldbook <- DT::renderDataTable({
      
      df <- simuDataLSD()$df
      validate_design(app_field_book_table(
        df, factor_columns = c("LOCATION", "PLOT", "ROW", "COLUMN", "SQUARE", "TREATMENT"),
        height = 500
      ))
    })
    
    output$downloadData.lsd <- app_csv_archive(
      filename = function() {
        loc <- paste("Latin_Square_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      data = function() as.data.frame(simuDataLSD()$df),
      design = latinsquare_reactive,
      field_book = function() simuDataLSD()$df,
      simulation = function() simuDataLSD()$simulation,
      layout = function() reactive_layoutLSD()$layout_metadata,
      kind = "field_book"
    )
    
    csv_data <- shiny::reactive({
      shiny::req(simuDataLSD()$df)
      df <- simuDataLSD()$df
      shiny::req(input$typlotLSD)
      if (input$typlotLSD == 2) {
        export_layout(df, 1, TRUE)
      } else {
        export_layout(df, 1)
      }
    })
    
    
    # Downloadable csv of selected dataset ----
    output$downloadCsv.lsd <- app_csv_archive(
      filename = function() {
        loc <- paste("Latin_Square_Layout", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      data = function() as.data.frame(csv_data()$file),
      design = latinsquare_reactive,
      field_book = function() simuDataLSD()$df,
      simulation = function() simuDataLSD()$simulation,
      layout = function() reactive_layoutLSD()$layout_metadata,
      kind = "layout"
    )
    
    app_reproduction_outputs(output, latinsquare_reactive)
  })
}

## To be copied in the UI
# mod_LSD_ui("LSD_ui_1")

## To be copied in the server
# mod_LSD_server("LSD_ui_1")
