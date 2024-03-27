#' CRD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom utils write.csv
mod_CRD_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Completely Randomized Design"),
    sidebarLayout(
      sidebarPanel(width = 4,
                   radioButtons(
                        inputId = ns("owndatacrd"), 
                        label = "Import entries' list?",
                        choices = c("Yes", "No"), 
                        selected = "No",
                        inline = TRUE, 
                        width = NULL,
                        choiceNames = NULL, 
                        choiceValues = NULL
                    ),
                   conditionalPanel(
                     "input.owndatacrd != 'Yes'",
                     ns = ns,
                     numericInput(ns("t.crd"), 
                       label = "Input # of Treatments:",
                       value = 15, 
                       min = 2),
                    ),
                   conditionalPanel(
                     "input.owndatacrd == 'Yes'", 
                     ns = ns,
                     fluidRow(
                      column(7, style=list("padding-right: 28px;"),
                             fileInput(ns("file.CRD"), 
                                       label = "Upload a CSV File:",
                                       multiple = FALSE)),
                      column(5,style=list("padding-left: 5px;"),
                             radioButtons(ns("sep.crd"), 
                                          "Separator",
                                          choices = c(Comma = ",",
                                                      Semicolon = ";",
                                                      Tab = "\t"),
                                          selected = ","))
                    )
                   ),
                    numericInput(ns("reps.crd"), 
                      label = "Input # of Full Reps:",
                      value = 4, 
                      min = 1),
                   selectInput(inputId = ns("planter_mov_crd"), 
                               label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"),
                               multiple = FALSE,
                               selected = "serpentine"),
                   fluidRow(
                     column(6, style=list("padding-right: 28px;"),
                            textInput(ns("plot_start.crd"), 
                                      "Starting Plot Number:", 
                                      value = 101)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            textInput(ns("Location.crd"), 
                                      "Input Location:", 
                                      value = "FARGO")
                     )
                   ),
                   
                   numericInput(inputId = ns("seed.crd"), 
                                label = "Random Seed:",
                                value = 123,
                                min = 1),
                   
                   fluidRow(
                     column(6,
                            actionButton(
                              inputId = ns("RUN.crd"), 
                              "Run!",
                              icon = icon("circle-nodes", verify_fa = FALSE),
                              width = '100%'),
                     ),
                     column(6,
                            actionButton(
                              inputId = ns("Simulate.crd"),
                              "Simulate!", 
                              icon = icon("greater-than-equal", verify_fa = FALSE),
                              width = '100%')
                     )
                     
                   ), 
                   br(),
                   downloadButton(ns("downloadData.crd"), 
                                  "Save My Experiment",
                                  style = "width:100%")
      ),
      mainPanel(
        width = 8,
        fluidRow(
          tabsetPanel(
            tabPanel("Field Layout",
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(
                       downloadButton(
                         ns("downloadCsv.crd"), 
                         label =  "Excel",
                         icon = icon("file-csv"), 
                         width = '10%',
                         style="color: #337ab7; background-color: #fff; border-color: #2e6da4")
                      ),
                      shinycssloaders::withSpinner(
                       plotly::plotlyOutput(ns("layout_random"), 
                                            width = "97%", 
                                            height = "560px"),
                       type = 5
                     ),
                     br(),
                     column(12,uiOutput(ns("well_panel_layout_CRD")))
            ),
            tabPanel("Field Book", 
                     shinycssloaders::withSpinner(
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
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    shinyjs::useShinyjs()
    
    get_data_crd <- reactive({
      
      if (input$owndatacrd == "Yes") {
        req(input$file.CRD)
        req(input$sep.crd)
        inFile <- input$file.CRD
        data_ingested <- load_file(name = inFile$name,
                                   path = inFile$datapat,
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
        } else if (names(data_ingested) == "bad_format") {
          shinyalert::shinyalert(
            "Error!!", 
            "Invalid file; Please upload a .csv file.", 
            type = "error")
          return(NULL)
        } else if (names(data_ingested) == "duplicated_vals") {
          shinyalert::shinyalert(
            "Error!!", 
            "Check input file for duplicate values.", 
            type = "error")
          return(NULL)
        } else if (names(data_ingested) == "missing_cols") {
          shinyalert::shinyalert(
            "Error!!", 
            "Data input needs at least two columns: TREATMENT and REP.", 
            type = "error")
          return(NULL)
        }
      } else {
        req(input$t.crd)
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
      bindEvent(input$RUN.crd)
    
    crd_inputs <- reactive({
      req(get_data_crd())
      req(input$planter_mov_crd)
      req(input$reps.crd)
      req(input$plot_start.crd)
      req(input$Location.crd)
      req(input$seed.crd)
      
      treatments <- as.numeric(get_data_crd()$treatments)
      reps <- as.numeric(input$reps.crd)
      planter <- input$planter_mov_crd
      plot_startreatments <- as.vector(unlist(strsplit(input$plot_start.crd, ",")))
      plot_start <- as.numeric(plot_startreatments)[1]
      site_names <-  as.vector(unlist(strsplit(input$Location.crd, ",")))
      seed <- as.numeric(input$seed.crd)
      return(list(t = treatments, 
        r = reps, 
        planter = planter,
        plot_start = plot_start, 
        site_names = site_names,
        seed = seed))
    }) |>
      bindEvent(input$RUN.crd)
    
    
    CRD_reactive <- reactive({
      
      req(get_data_crd())
      req(crd_inputs())
      
      shinyjs::show(id = "downloadCsv.crd")
      
      my.design <- CRD(
        reps = crd_inputs()$r, 
        plotNumber = crd_inputs()$plot_start, 
        seed = crd_inputs()$seed,
        locationName = crd_inputs()$site_names, 
        data = get_data_crd()$data_crd
      )
      
    }) |> 
      bindEvent(input$RUN.crd)
    
    output$well_panel_layout_CRD <- renderUI({
      req(CRD_reactive())
      obj_crd <- CRD_reactive()
      planting_crd <- crd_inputs()$planter
      allBooks_crd <- plot_layout(x = obj_crd, 
                                  layout = 1, 
                                  planter = planting_crd)$newBooks
      nBooks_crd <- length(allBooks_crd)
      layoutOptions_crd <- 1:nBooks_crd
      wellPanel(
        fluidRow(
          column(3,
                 radioButtons(ns("typlotCRD"), "Type of Plot:",
                              c("Entries/Treatments" = 1,
                                "Plots" = 2,
                                "Heatmap" = 3))
          ),
          column(3, 
                 selectInput(inputId = ns("layoutO_crd"), 
                             label = "Layout option:", 
                             choices = layoutOptions_crd)
          )
        )
      )
    })
    
    reactive_layoutCRD <- reactive({
      req(input$layoutO_crd)
      req(CRD_reactive())
      obj_crd <- CRD_reactive()
      opt_crd <- as.numeric(input$layoutO_crd)
      planting_crd <- crd_inputs()$planter
      plot_layout(x = obj_crd, layout = opt_crd, planter = planting_crd)
    })
    
    entryListFormat_CRD <- data.frame(TREATMENT = c(paste("TRT_", LETTERS[1:9], sep = "")))
    entriesInfoModal_CRD <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        renderTable(entryListFormat_CRD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        h4("Note that only the TREATMENT column is required."),
        easyClose = FALSE
      )
    }
    
    toListen <- reactive({
      list(input$owndatacrd)
    })
    
    observeEvent(toListen(), {
      if (input$owndatacrd == "Yes") {
        showModal(
          entriesInfoModal_CRD()
        )
      }
    })
    
    vals <- reactiveValues(maxV.CRD = NULL, minV.CRD = NULL, trail.CRD = NULL)
    
    simuModal.crd <- function(failed = FALSE) {
      modalDialog(
        selectInput(inputId = ns("trailsCRD"), label = "Select One:", choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        conditionalPanel("input.trailsCRD == 'Other'", ns = ns,
                         textInput(inputId = ns("OtherCRD"), label = "Input Trial Name:", value = NULL)
        ),
        fluidRow(
          column(6, 
                 numericInput(ns("min.crd"), "Input the min value", value = NULL)
          ),
          column(6, 
                 numericInput(ns("max.crd"), "Input the max value", value = NULL)
                 
          )
        ),
        
        if (failed)
          div(tags$b("Invalid input of data max and min", style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ok.crd"), "GO")
        )
        
      )
    }
    
    # Show modal when button is clicked.
    observeEvent(input$Simulate.crd, {
      req(CRD_reactive()$fieldBook)
      showModal(
        simuModal.crd()
      )
    })
    
    # When OK button is pressed, attempt to load the data set. If successful,
    # remove the modal. If not show another modal, but this time with a failure
    # message.
    observeEvent(input$ok.crd, {
      req(input$max.crd, input$min.crd)
      if (input$max.crd > input$min.crd && input$min.crd != input$max.crd) {
        vals$maxV.CRD <- input$max.crd
        vals$minV.CRD <- input$min.crd
        if(input$trailsCRD == "Other") {
          req(input$OtherCRD)
          vals$trail.CRD <- as.character(input$OtherCRD)
        }else {
          vals$trail.CRD <- as.character(input$trailsCRD)
        }
        removeModal()
      }else {
        showModal(
          simuModal.crd(failed = TRUE)
        )
      }
    })
    
    simuDataCRD <- reactive({
      req(CRD_reactive()$fieldBook)
      if(!is.null(vals$maxV.CRD) && !is.null(vals$minV.CRD) && !is.null(vals$trail.CRD)) {
        max <- as.numeric(vals$maxV.CRD)
        min <- as.numeric(vals$minV.CRD)
        df.crd <- reactive_layoutCRD()$fieldBookXY
        cnamesdf.crd <- colnames(df.crd)
        df.crd <- norm_trunc(
          a = min, 
          b = max, 
          data = df.crd, 
          seed = crd_inputs()$seed
        )
        colnames(df.crd) <- c(cnamesdf.crd[1:(ncol(df.crd) - 1)], vals$trail.CRD)
        df.crd <- df.crd[order(df.crd$ID),]
      }else {
        df.crd <- reactive_layoutCRD()$fieldBookXY
      }
      return(list(df = df.crd))
    })
    
    heatmapInfoModal_CRD <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    output$tabsetCRD <- renderUI({
      req(input$typlotCRD)
      tabsetPanel(
        if (input$typlotCRD != 3) {
          tabPanel("Completely Randomized Field Layout", 
                   shinycssloaders::withSpinner(
                     plotOutput(ns("layout.crd"), 
                                width = "100%",
                                height = "650px"),
                    type = 5))
        } else {
          tabPanel("Completely Randomized Field Layout", 
                   shinycssloaders::withSpinner(
                     plotly::plotlyOutput(ns("heatmapCRD"), 
                                          width = "100%", 
                                          height = "650px"),
                     type = 5))
        },
        tabPanel("Completely Randomized Field Book", 
                 shinycssloaders::withSpinner(
                   DT::DTOutput(ns("CRD.output")), 
                   type = 5))
      )
      
    })
    
    heatmap_obj <- reactive({
      req(simuDataCRD()$df)
      if (ncol(simuDataCRD()$df) == 8) {
        trail <- as.character(vals$trail.CRD)
        label_trail <- paste(trail, ": ")
        heatmapTitle <- paste("Heatmap for ", trail)
        df <- simuDataCRD()$df
        new_df <- df |>
          dplyr::mutate(text = paste0("Row: ", df$ROW, "\n", "Col: ", df$COLUMN, "\n", "Entry: ", 
                                      df$TREATMENT, "\n", label_trail, round(df[,8],2)))
        w <- as.character(vals$trail.CRD)
        new_df$ROW <- as.factor(new_df$ROW) # Set up ROWS as factors
        new_df$COLUMN <- as.factor(new_df$COLUMN) # Set up COLUMNS as factors
        p1 <- ggplot2::ggplot(new_df, ggplot2::aes(x = new_df[,5], y = new_df[,4], fill = new_df[,8], text = text)) +
          ggplot2::geom_tile() +
          ggplot2::xlab("COLUMN") +
          ggplot2::ylab("ROW") +
          ggplot2::labs(fill = w) +
          viridis::scale_fill_viridis(discrete = FALSE) +
          ggplot2::ggtitle(heatmapTitle) +
          ggplot2::theme_minimal() + # I added this option 
          ggplot2::theme(plot.title = ggplot2::element_text(family="Calibri", face="bold", size=13, hjust=0.5))
        p2 <- plotly::ggplotly(p1, tooltip="text", height = 560)
        return(p2)
      } else {
        showModal(
          heatmapInfoModal_CRD()
        )
        return(NULL)
      }
    })

    output$layout_random <- plotly::renderPlotly({
      req(CRD_reactive())
      req(input$typlotCRD)
      if (input$typlotCRD == 1) {
        reactive_layoutCRD()$out_layout
      } else if (input$typlotCRD == 2) {
        reactive_layoutCRD()$out_layoutPlots
      } else {
        req(heatmap_obj())
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
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      
      DT::datatable(df,
                    filter = 'top',
                    rownames = FALSE, 
                    options = list(
                      columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    })
    
    output$downloadData.crd <- downloadHandler(
      filename = function() {
        loc <- paste("CRD_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(simuDataCRD()$df)
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    csv_data <- reactive({
      req(simuDataCRD()$df)
      df <- simuDataCRD()$df
      req(input$typlotCRD)
      if (input$typlotCRD == 2) {
        export_layout(df, 1, TRUE)
      } else {
        export_layout(df, 1)
      }
    })
    
    
    # Downloadable csv of selected dataset ----
    output$downloadCsv.crd <- downloadHandler(
      filename = function() {
        loc <- paste("Completely_Randomized_Layout", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(csv_data()$file)
        write.csv(df, file, row.names = FALSE)
      }
    )
    
  })
}
