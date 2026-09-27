#' CRD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom utils write.csv
mod_CRD_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Completely Randomized Design"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 4,
                   shiny::radioButtons(
                        inputId = ns("owndatacrd"), 
                        label = "Import entries' list?",
                        choices = c("Yes", "No"), 
                        selected = "No",
                        inline = TRUE, 
                        width = NULL,
                        choiceNames = NULL, 
                        choiceValues = NULL
                    ),
                   shiny::conditionalPanel(
                     "input.owndatacrd != 'Yes'",
                     ns = ns,
                     shiny::numericInput(ns("t.crd"),
                       label = "Input # of Treatments:",
                       value = 15, 
                       min = 2),
                    ),
                   shiny::conditionalPanel(
                     "input.owndatacrd == 'Yes'", 
                     ns = ns,
                     shiny::fluidRow(
                      shiny::column(7, style=list("padding-right: 28px;"),
                             shiny::fileInput(ns("file.CRD"),
                                       label = "Upload a CSV File:",
                                       multiple = FALSE)),
                      shiny::column(5,style=list("padding-left: 5px;"),
                             shiny::radioButtons(ns("sep.crd"),
                                          "Separator",
                                          choices = c(Comma = ",",
                                                      Semicolon = ";",
                                                      Tab = "\t"),
                                          selected = ","))
                    )
                   ),
                    shiny::numericInput(ns("reps.crd"),
                      label = "Input # of Full Reps:",
                      value = 4, 
                      min = 1),
                   shiny::selectInput(inputId = ns("planter_mov_crd"),
                               label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"),
                               multiple = FALSE,
                               selected = "serpentine"),
                   shiny::fluidRow(
                     shiny::column(6, style=list("padding-right: 28px;"),
                            shiny::textInput(ns("plot_start.crd"),
                                      "Starting Plot Number:", 
                                      value = 101)
                     ),
                     shiny::column(6,style=list("padding-left: 5px;"),
                            shiny::textInput(ns("Location.crd"),
                                      "Input Location:", 
                                      value = "FARGO")
                     )
                   ),
                   
                   shiny::numericInput(inputId = ns("seed.crd"),
                                label = "Random Seed:",
                                value = 123,
                                min = 1),
                   
                   shiny::fluidRow(
                     shiny::column(6,
                            shiny::actionButton(
                              inputId = ns("RUN.crd"), 
                              "Run!",
                              icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                              width = '100%'),
                     ),
                     shiny::column(6,
                            shiny::actionButton(
                              inputId = ns("Simulate.crd"),
                              "Simulate!", 
                              icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                              width = '100%')
                     )
                     
                   ), 
                   shiny::br(),
                   shiny::downloadButton(ns("downloadData.crd"),
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
                         ns("downloadCsv.crd"), 
                         label = "CSV + metadata (ZIP)",
                         icon = shiny::icon("download"),
                         width = 'auto',
                         style="color: #337ab7; background-color: #fff; border-color: #2e6da4")
                      ),
                      fieldhub_spinner(
                       plotly::plotlyOutput(ns("layout_random"), 
                                            width = "97%", 
                                            height = "560px"),
                       type = 5
                     ),
                     shiny::br(),
                     shiny::column(12,shiny::uiOutput(ns("well_panel_layout_CRD")))
            ),
            shiny::tabPanel("Field Book",
                     fieldhub_spinner(
                       DT::DTOutput(ns("CRD_fieldbook")), 
                       type = 5
                    )
            )
          )
        )
      )
    )
  )
}
#' CRD Server Function
#'
#' @noRd 
mod_CRD_server <- function(id) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    shinyjs::useShinyjs()
    
    get_data_crd <- shiny::reactive({
      
      if (input$owndatacrd == "Yes") {
        shiny::req(input$file.CRD)
        shiny::req(input$sep.crd)
        inFile <- input$file.CRD
        data_ingested <- load_file(name = inFile$name,
                                   path = inFile[["datapath"]],
                                   sep = input$sep.crd,
                                   check = TRUE, 
                                   design = "crd")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_up <- as.data.frame(data_up[,1])
          data_crd <- na.omit(data_up)
          data_crd$REP <- rep(input$reps.crd, times = nrow(data_crd))
          colnames(data_crd) <- c("TREATMENT", "REP")
          treatments = nrow(data_crd)
          return(list(data_crd = data_crd, treatments = treatments))
        } else {
          app_upload_error(data_ingested,
                           missing_columns = "Data input needs at least two columns: TREATMENT and REP.")
          return(NULL)
        }
      } else {
        shiny::req(input$t.crd)
        nt <- as.numeric(input$t.crd)
        reps <- as.numeric(input$reps.crd)
        data_crd <- data.frame(
          list(
            TREATMENT = paste0("T-", 1:nt),
            REP = rep(reps, times = nt)
            )
          )
        colnames(data_crd) <- c("TREATMENT", "REP")
        return(list(data_crd = data_crd, treatments = nt))
      }
    }) |>
      shiny::bindEvent(input$RUN.crd)
    
    crd_inputs <- shiny::reactive({
      shiny::req(get_data_crd())
      shiny::req(input$planter_mov_crd)
      shiny::req(input$reps.crd)
      shiny::req(input$plot_start.crd)
      shiny::req(input$Location.crd)
      shiny::req(input$seed.crd)
      
      treatments <- as.numeric(get_data_crd()$treatments)
      reps <- as.numeric(input$reps.crd)
      planter <- input$planter_mov_crd
      plot_start <- validate_design(read_whole_numbers(
        input$plot_start.crd, "Starting Plot Number"
      ))[1]
      site_names <-  as.vector(unlist(strsplit(input$Location.crd, ",")))
      seed <- as.numeric(input$seed.crd)
      return(list(t = treatments, 
        r = reps, 
        planter = planter,
        plot_start = plot_start, 
        site_names = site_names,
        seed = seed))
    }) |>
      shiny::bindEvent(input$RUN.crd)
    
    
    CRD_reactive <- shiny::reactive({
      
      shiny::req(get_data_crd())
      shiny::req(crd_inputs())
      
      shinyjs::show(id = "downloadCsv.crd")
      
      my.design <- validate_design(CRD(
        reps = crd_inputs()$r, 
        plotNumber = crd_inputs()$plot_start, 
        seed = crd_inputs()$seed,
        locationNames = crd_inputs()$site_names,
        data = get_data_crd()$data_crd
      ))
      
    }) |> 
      shiny::bindEvent(input$RUN.crd)
    
    output$well_panel_layout_CRD <- shiny::renderUI({
      shiny::req(CRD_reactive())
      obj_crd <- CRD_reactive()
      planting_crd <- crd_inputs()$planter
      allBooks_crd <- plot_layout(x = obj_crd, 
                                  layout = 1, 
                                  planter = planting_crd)$newBooks
      nBooks_crd <- length(allBooks_crd)
      layoutOptions_crd <- 1:nBooks_crd
      shiny::wellPanel(
        shiny::fluidRow(
          shiny::column(3,
                 shiny::radioButtons(ns("typlotCRD"), "Type of Plot:",
                              c("Entries/Treatments" = 1,
                                "Plots" = 2,
                                "Heatmap" = 3))
          ),
          shiny::column(3,
                 shiny::selectInput(inputId = ns("layoutO_crd"),
                             label = "Layout option:", 
                             choices = layoutOptions_crd)
          )
        )
      )
    })
    
    reactive_layoutCRD <- shiny::reactive({
      shiny::req(input$layoutO_crd)
      shiny::req(CRD_reactive())
      obj_crd <- CRD_reactive()
      opt_crd <- as.numeric(input$layoutO_crd)
      planting_crd <- crd_inputs()$planter
      plot_layout(x = obj_crd, layout = opt_crd, planter = planting_crd)
    })
    
    entryListFormat_CRD <- data.frame(TREATMENT = c(paste("TRT_", LETTERS[1:9], sep = "")))
    entriesInfoModal_CRD <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(entryListFormat_CRD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        shiny::h4("Note that only the TREATMENT column is required."),
        easyClose = FALSE
      )
    }
    
    toListen <- shiny::reactive({
      list(input$owndatacrd)
    })
    
    shiny::observeEvent(toListen(), {
      if (input$owndatacrd == "Yes") {
        shiny::showModal(
          entriesInfoModal_CRD()
        )
      }
    })
    
    vals <- shiny::reactiveValues(maxV.CRD = NULL, minV.CRD = NULL, trail.CRD = NULL)
    
    simuModal.crd <- function(failed = FALSE) {
      shiny::modalDialog(
        shiny::selectInput(inputId = ns("trailsCRD"), label = "Select One:", choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        shiny::conditionalPanel("input.trailsCRD == 'Other'", ns = ns,
                         shiny::textInput(inputId = ns("OtherCRD"), label = "Input Trial Name:", value = NULL)
        ),
        shiny::fluidRow(
          shiny::column(6,
                 shiny::numericInput(ns("min.crd"), "Input the min value", value = NULL)
          ),
          shiny::column(6,
                 shiny::numericInput(ns("max.crd"), "Input the max value", value = NULL)
                 
          )
        ),
        
        if (failed)
          shiny::div(shiny::tags$b("Invalid input of data max and min", style = "color: red;")),
        
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("ok.crd"), "GO")
        )
        
      )
    }
    
    # Show modal when button is clicked.
    shiny::observeEvent(input$Simulate.crd, {
      shiny::req(CRD_reactive()$fieldBook)
      shiny::showModal(
        simuModal.crd()
      )
    })
    
    # When OK button is pressed, attempt to load the data set. If successful,
    # remove the modal. If not show another modal, but this time with a failure
    # message.
    shiny::observeEvent(input$ok.crd, {
      shiny::req(input$max.crd, input$min.crd)
      if (input$max.crd > input$min.crd && input$min.crd != input$max.crd) {
        vals$maxV.CRD <- input$max.crd
        vals$minV.CRD <- input$min.crd
        if(input$trailsCRD == "Other") {
          shiny::req(input$OtherCRD)
          vals$trail.CRD <- as.character(input$OtherCRD)
        }else {
          vals$trail.CRD <- as.character(input$trailsCRD)
        }
        shiny::removeModal()
      }else {
        shiny::showModal(
          simuModal.crd(failed = TRUE)
        )
      }
    })
    
    simuDataCRD <- shiny::reactive({
      shiny::req(CRD_reactive()$fieldBook)
      if(!is.null(vals$maxV.CRD) && !is.null(vals$minV.CRD) && !is.null(vals$trail.CRD)) {
        max <- as.numeric(vals$maxV.CRD)
        min <- as.numeric(vals$minV.CRD)
        df.crd <- reactive_layoutCRD()$fieldBookXY
        simulation <- validate_design(simulate_classic_field_book(
          field_book = df.crd, min_value = min, max_value = max,
          response_name = vals$trail.CRD, seed = crd_inputs()$seed,
          order_by_id = TRUE
        ))
        df.crd <- simulation$field_book
      }else {
        simulation <- NULL
        df.crd <- reactive_layoutCRD()$fieldBookXY
      }
      return(list(df = df.crd, simulation = simulation))
    })
    
    heatmapInfoModal_CRD <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    output$tabsetCRD <- shiny::renderUI({
      shiny::req(input$typlotCRD)
      shiny::tabsetPanel(
        if (input$typlotCRD != 3) {
          shiny::tabPanel("Completely Randomized Field Layout",
                   fieldhub_spinner(
                     shiny::plotOutput(ns("layout.crd"),
                                width = "100%",
                                height = "650px"),
                    type = 5))
        } else {
          shiny::tabPanel("Completely Randomized Field Layout",
                   fieldhub_spinner(
                     plotly::plotlyOutput(ns("heatmapCRD"), 
                                          width = "100%", 
                                          height = "650px"),
                     type = 5))
        },
        shiny::tabPanel("Completely Randomized Field Book",
                 fieldhub_spinner(
                   DT::DTOutput(ns("CRD.output")), 
                   type = 5))
      )
      
    })
    
    heatmap_obj <- shiny::reactive({
      shiny::req(simuDataCRD()$df)
      book <- simuDataCRD()$df
      response <- as.character(vals$trail.CRD)
      if (length(response) == 1L && response %in% names(book)) {
        validate_design(app_field_heatmap(
          book, response_name = response, selected = 1L,
          label_column = "TREATMENT", label_title = "Entry",
          include_site = FALSE, include_checks = FALSE
        ))
      } else {
        shiny::showModal(heatmapInfoModal_CRD())
        NULL
      }
    })

    output$layout_random <- plotly::renderPlotly({
      shiny::req(CRD_reactive())
      shiny::req(input$typlotCRD)
      if (input$typlotCRD == 1) {
        reactive_layoutCRD()$out_layout
      } else if (input$typlotCRD == 2) {
        reactive_layoutCRD()$out_layoutPlots
      } else {
        shiny::req(heatmap_obj())
        heatmap_obj()
      }
    })
    
    output$CRD_fieldbook <- DT::renderDT({
      df <- simuDataCRD()$df
      df$LOCATION <- as.factor(df$LOCATION)
      df$PLOT <- as.factor(df$PLOT)
      df$ROW <- as.factor(df$ROW)
      df$COLUMN <- as.factor(df$COLUMN)
      df$REP <- as.factor(df$REP)
      df$TREATMENT <- as.factor(df$TREATMENT)
      table_options <- list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px")
      
      DT::datatable(df,
                    filter = 'top',
                    rownames = FALSE, 
                    options = utils::modifyList(table_options, list(
                      columnDefs = list(list(className = 'dt-center', targets = "_all")))))
    })
    
    output$downloadData.crd <- app_csv_archive(
      filename = function() {
        loc <- paste("CRD_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      data = function() as.data.frame(simuDataCRD()$df),
      design = CRD_reactive,
      field_book = function() simuDataCRD()$df,
      simulation = function() simuDataCRD()$simulation,
      layout = function() reactive_layoutCRD()$layout_metadata,
      kind = "field_book"
    )
    
    csv_data <- shiny::reactive({
      shiny::req(simuDataCRD()$df)
      df <- simuDataCRD()$df
      shiny::req(input$typlotCRD)
      if (input$typlotCRD == 2) {
        export_layout(df, 1, TRUE)
      } else {
        export_layout(df, 1)
      }
    })
    
    
    # Downloadable csv of selected dataset ----
    output$downloadCsv.crd <- app_csv_archive(
      filename = function() {
        loc <- paste("Completely_Randomized_Layout", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      data = function() as.data.frame(csv_data()$file),
      design = CRD_reactive,
      field_book = function() simuDataCRD()$df,
      simulation = function() simuDataCRD()$simulation,
      layout = function() reactive_layoutCRD()$layout_metadata,
      kind = "layout"
    )
    
    app_reproduction_outputs(output, CRD_reactive)
  })
}
