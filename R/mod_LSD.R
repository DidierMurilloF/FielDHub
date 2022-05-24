#' LSD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_LSD_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("Latin Square Design"),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        radioButtons(inputId = ns("owndataLSD"), 
                     label = "Import entries' list?", 
                     choices = c("Yes", "No"), 
                     selected = "No",
                     inline = TRUE, 
                     width = NULL, 
                     choiceNames = NULL, 
                     choiceValues = NULL), 
        
        conditionalPanel(
          condition = "input.owndataLSD == 'Yes'", 
          ns = ns,
          fluidRow(
            column(8, style=list("padding-right: 28px;"),
                   fileInput(ns("file.LSD"), 
                             label = "Upload a CSV File:", 
                             multiple = FALSE)),
            
            column(4,style=list("padding-left: 5px;"),
                   radioButtons(ns("sep.lsd"), "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","))
          )             
        ),
        
        conditionalPanel(
          condition = "input.owndataLSD != 'Yes'", ns = ns,
                         
          numericInput(ns("n.lsd"), 
                       label = "Input # of Treatments:",
                       value = 5, 
                       min = 2),             
        ),
        
        numericInput(ns("reps.lsd"), 
                     label = "Input # of Full Reps (Squares):",
                     value = 1, 
                     min = 1),
        selectInput(inputId = ns("planter.lsd"), label = "Plot Order Layout:",
                    choices = c("serpentine", "cartesian"), multiple = FALSE,
                    selected = "serpentine"),
        fluidRow(
          column(6, style=list("padding-right: 28px;"),
                 textInput(ns("plot_start.lsd"), 
                           "Starting Plot Number:", 
                           value = 101)
          ),
          column(6,style=list("padding-left: 5px;"),
                 textInput(ns("Location.lsd"), 
                           "Input Location:", 
                           value = "FARGO")
          )
        ),
        numericInput(ns("seed.lsd"), 
                     label = "Seed Number:", 
                     value = 123, 
                     min = 1),
        
        fluidRow(
          column(6,
                 actionButton(inputId = ns("RUN.lsd"), 
                              "Run!", 
                              icon = icon("cocktail"), 
                              width = '100%'),
          ),
          column(6,
                 actionButton(ns("Simulate.lsd"), 
                              "Simulate!", 
                              icon = icon("cocktail"), 
                              width = '100%')
          )
          
        ), 
        br(),
        downloadButton(ns("downloadData.lsd"), 
                       "Save Experiment!", 
                       style = "width:100%")
                   
      ),
      mainPanel(width = 8,
          fluidRow(
            tabsetPanel(
              tabPanel("Field Layout",
                       shinyjs::useShinyjs(),
                       shinyjs::hidden(downloadButton(ns("downloadCsv.lsd"), 
                                                      label =  "Excel",
                                                      icon = icon("file-csv"), 
                                                      width = '10%',
                                                      style="color: #337ab7; background-color: #fff; border-color: #2e6da4")),
                       plotly::plotlyOutput(ns("layout_lsd"),
                                            width = "98%",
                                            height = "650px"),
                       column(12, uiOutput(ns("well_panel_layout_LSD")))
              ),
              tabPanel("Field Book", 
                       shinycssloaders::withSpinner(
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
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    shinyjs::useShinyjs()
    
    entryListFormat_LSD <- data.frame(
      list(ROW = paste("Period", 1:5, sep = ""),
           COLUMN = paste("Cow", 1:5, sep = ""),
           TREATMENT = paste("Diet", 1:5, sep = ""))
    )
    entriesInfoModal_LSD <- function() {
      modalDialog(
        title = div(tags$h3("Important message",
                            style = "color: red;")),
        h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        renderTable(entryListFormat_LSD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        easyClose = FALSE
      )
    }
    
    toListen <- reactive({
      list(input$owndataLSD)
    })
    
    observeEvent(toListen(), {
      if (input$owndataLSD == "Yes") {
        showModal(
          shinyjqui::jqui_draggable(
            entriesInfoModal_LSD()
          )
        )
      }
    })
    
    
    getData.lsd <- reactive({
      req(input$file.LSD)
      req(input$sep.lsd)
      inFile <- input$file.LSD
      dataUp.lsd <- load_file(name = inFile$name, 
                              path = inFile$datapat, 
                              sep = input$sep.lsd, check = TRUE, design = "lsd")
      
      if (is.logical(dataUp.lsd)) {
        if (dataUp.lsd) {
          shinyalert::shinyalert(
            "Error!!", 
            "Check input file for duplicate values.", 
            type = "error")
          #dataUp.lsd <- NULL
          return(NULL)
        } else {
          shinyalert::shinyalert(
            "Error!!", 
            "Invalid file; Please upload a .csv file.", 
            type = "error")
          #dataUp.lsd <- NULL
          return(NULL)
        }
      }
      
      return(list(dataUp.lsd = dataUp.lsd))
    })
    
    latinsquare_reactive <- eventReactive(input$RUN.lsd, {
      req(input$plot_start.lsd)
      req(input$Location.lsd)
      req(input$reps.lsd)
      req(input$seed.lsd)
      
      shinyjs::show(id = "downloadCsv.lsd")
      
      plot_start.lsd <- as.vector(unlist(strsplit(input$plot_start.lsd, ",")))
      plot_start.lsd <- as.numeric(plot_start.lsd)
      loc.lsd <-  as.vector(unlist(strsplit(input$Location.lsd, ",")))
      seed.number.lsd <- as.numeric(input$seed.lsd)
      
      if (input$owndataLSD == "Yes") {
        n.lsd <- NULL
        reps.lsd <- as.numeric(input$reps.lsd)
        data.lsd <- getData.lsd()$dataUp.lsd
        n <- as.numeric(nrow(data.lsd))
        if (n > 10) validate("Only up to 10 treatments are allowed.")
      }else {
        req(input$n.lsd)
        n <- as.numeric(input$n.lsd)
        if (n > 10) validate("Only up to 10 treatments are allowed.")
        n.lsd <- n
        reps.lsd <- as.numeric(input$reps.lsd)
        data.lsd <- NULL
      }
      LSD.design <- latin_square(t = n.lsd, 
                                 reps = reps.lsd, 
                                 plotNumber = plot_start.lsd[1],
                                 planter = "cartesian",
                                 seed = seed.number.lsd, 
                                 locationNames = loc.lsd[1], 
                                 data = data.lsd)
    })
    
    
    output$well_panel_layout_LSD <- renderUI({
      req(latinsquare_reactive()$fieldBook)
      req(latinsquare_reactive())
      obj_lsd <- latinsquare_reactive()
      allBooks_lsd <- plot_layout(x = obj_lsd, 
                                 optionLayout = 1,
                                 orderReps = "vertical_stack_panel")$newBooks
      nBooks_lsd <- length(allBooks_lsd)
      layoutOptions_lsd <- 1:nBooks_lsd
      df <- latinsquare_reactive()$fieldBook
      orderReps_lsd <- c("Vertical Stack Panel" = "vertical_stack_panel", 
                          "Horizontal Stack Panel" = "horizontal_stack_panel")
      nBooks_lsd <- length(allBooks_lsd)
      wellPanel(
        column(3,
               radioButtons(ns("typlotLSD"), "Type of Plot:",
                            c("Entries/Treatments" = 1,
                              "Plots" = 2,
                              "Heatmap" = 3))
        ),
        fluidRow(
          column(3,
                 selectInput(inputId = ns("orderRepsLSD"),
                             label = "Reps layout:",
                             choices = orderReps_lsd),
          ),
          column(3, 
                 selectInput(inputId = ns("layoutO_lsd"), 
                             label = "Layout option:", 
                             choices = layoutOptions_lsd)
          )
        )
      )
    })
    
    observeEvent(input$orderRepsLSD, {
      req(input$orderRepsLSD)
      obj_lsd <- latinsquare_reactive()
      allBooks <- try(plot_layout(x = obj_lsd, 
                                  optionLayout = 1, 
                                  orderReps = input$orderRepsLSD)$newBooks, 
                      silent = TRUE)
      nBooks <- length(allBooks)
      NewlayoutOptions <- 1:nBooks
      updateSelectInput(session = session, inputId = 'layoutO_lsd',
                        label = "Layout option:",
                        choices = NewlayoutOptions,
                        selected = 1
      )
    })
    
    reactive_layoutLSD <- reactive({
      req(input$layoutO_lsd)
      req(latinsquare_reactive())
      req(input$planter.lsd)
      obj_lsd <- latinsquare_reactive()

      opt_lsd <- as.numeric(input$layoutO_lsd)
      planting_lsd <- input$planter.lsd
      try(plot_layout(x = obj_lsd,
                      optionLayout = opt_lsd,
                      orderReps = input$orderRepsLSD,
                      planter = planting_lsd,
                      l = 1),
          silent = TRUE)
    })
    
    valsLSD <- reactiveValues(maxV.lsd = NULL, 
                              minV.lsd = NULL, 
                              trail.lsd = NULL)
    
    simuModal.lsd <- function(failed = FALSE) {
      modalDialog(
        selectInput(inputId = ns("trailsLSD"), 
                    label = "Select One:", 
                    choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        conditionalPanel(
          condition = "input.trailsLSD == 'Other'", ns = ns,
          textInput(inputId = ns("OtherLSD"), 
                    label = "Input Trial Name:", 
                    value = NULL)
        ),
        fluidRow(
          column(6, 
                 numericInput(ns("min.lsd"), 
                              "Input the min value", 
                              value = NULL)
          ),
          column(6, 
                 numericInput(ns("max.lsd"), 
                              "Input the max value", 
                              value = NULL)
          )
        ),
        
        if (failed)
          div(tags$b("Invalid input of data max and min", 
                     style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ok.lsd"), "GO")
        )
      )
    }
    observeEvent(input$Simulate.lsd, {
      req(latinsquare_reactive()$fieldBook)
      showModal(
        shinyjqui::jqui_draggable(
          simuModal.lsd()
        )
      )
    })
    
    observeEvent(input$ok.lsd, {
      req(input$max.lsd, input$min.lsd)
      if (input$max.lsd > input$min.lsd && input$min.lsd != input$max.lsd) {
        valsLSD$maxV.lsd <- input$max.lsd
        valsLSD$minV.lsd <- input$min.lsd
        if(input$trailsLSD == "Other") {
          req(input$OtherLSD)
          if(!is.null(input$OtherLSD)) {
            valsLSD$trail.lsd <- input$OtherLSD
          }else showModal(simuModal.lsd(failed = TRUE))
        }else {
          valsLSD$trail.lsd <- as.character(input$trailsLSD)
        }
        removeModal()
      }else {
        showModal(
          shinyjqui::jqui_draggable(
            simuModal.lsd(failed = TRUE)
          )
        )
      }
    })
    
    
    simuDataLSD <- reactive({
      req(latinsquare_reactive()$fieldBook)
      if(!is.null(valsLSD$maxV.lsd) && !is.null(valsLSD$minV.lsd) && 
         !is.null(valsLSD$trail.lsd)) {
        max <- as.numeric(valsLSD$maxV.lsd)
        min <- as.numeric(valsLSD$minV.lsd)
        df.lsd <- reactive_layoutLSD()$allSitesFieldbook
        cnamesdf.lsd <- colnames(df.lsd)
        df.lsd <- norm_trunc(a = min, b = max, data = df.lsd)
        colnames(df.lsd) <- c(cnamesdf.lsd[1:(ncol(df.lsd) - 1)], 
                              valsLSD$trail.lsd)
        df.lsd <- df.lsd[order(df.lsd$ID),]
      }else {
        df.lsd <- reactive_layoutLSD()$allSitesFieldbook
      }
      return(list(df = df.lsd))
    })
    
    heatmapInfoModal_LSD <- function() {
      modalDialog(
        title = div(tags$h3("Important message", 
                            style = "color: red;")),
        h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    heatmap_obj <- reactive({
      req(simuDataLSD()$df)
      if (ncol(simuDataLSD()$df) == 10) {
        locs <- factor(simuDataLSD()$df$LOCATION, levels = unique(simuDataLSD()$df$LOCATION))
        locLevels <- levels(locs)
        df = subset(simuDataLSD()$df, LOCATION == locLevels[1])
        loc <- levels(factor(df$LOCATION))
        trail <- as.character(valsLSD$trail.lsd)
        label_trail <- paste(trail, ": ")
        heatmapTitle <- paste("Heatmap for ", trail)
        new_df <- df %>%
          dplyr::mutate(text = paste0("Site: ", loc, "\n", 
                                      "Row: ", df$ROW, "\n", 
                                      "Col: ", df$COLUMN, "\n", 
                                      "Entry: ", df$ENTRY, "\n", 
                                      label_trail, 
                                      round(df[,10],2)))
        w <- as.character(valsLSD$trail.lsd)
        new_df$ROW <- as.factor(new_df$ROW) # Set up ROWS as factors
        new_df$COLUMN <- as.factor(new_df$COLUMN) # Set up COLUMNS as factors
        p1 <- ggplot2::ggplot(
          new_df, 
          ggplot2::aes(x = new_df[,5], 
                       y = new_df[,4], 
                       fill = new_df[,10],
                       text = text)) +
          ggplot2::geom_tile() +
          ggplot2::xlab("COLUMN") +
          ggplot2::ylab("ROW") +
          ggplot2::labs(fill = w) +
          viridis::scale_fill_viridis(discrete = FALSE) +
          ggplot2::ggtitle(heatmapTitle) +
          ggplot2::theme_minimal() + 
          ggplot2::theme(
            plot.title = ggplot2::element_text(
              family="Calibri", 
              face="bold", 
              size=13, 
              hjust=0.5)
            )
        
        p2 <- plotly::ggplotly(p1, tooltip="text", width = 1350, height = 640)
        # p2 
        return(p2)
      } else {
        showModal(
          shinyjqui::jqui_draggable(
            heatmapInfoModal_LSD()
          )
        )
        return(NULL)
      }
    })
    
    output$layout_lsd <- plotly::renderPlotly({
      # print(simuDataLSD()$df)
      req(reactive_layoutLSD())
      req(latinsquare_reactive())
      req(input$typlotLSD)
      if (input$typlotLSD == 1) {
        reactive_layoutLSD()$out_layout
      } else if (input$typlotLSD == 2) {
        reactive_layoutLSD()$out_layoutPlots
      } else {
        req(heatmap_obj())
        heatmap_obj()
      }
    })
    
    output$LSD_fieldbook <- DT::renderDataTable({
      
      df <- simuDataLSD()$df
      # "ID LOCATION PLOT ROW COLUMN SQUARE ROW_SQ COLUMN_SQ TREATMENT"
      df$LOCATION <- as.factor(df$LOCATION)
      df$PLOT <- as.factor(df$PLOT)
      df$ROW <- as.factor(df$ROW)
      df$COLUMN <- as.factor(df$COLUMN)
      df$SQUARE <- as.factor(df$SQUARE)
      df$TREATMENT <- as.factor(df$TREATMENT)
      
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      
      DT::datatable(df, 
                    filter = "top",
                    rownames = FALSE, 
                    options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
      
    })
    
    output$downloadData.lsd <- downloadHandler(
      filename = function() {
        loc <- paste("Latin_Square_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(simuDataLSD()$df)
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    csv_data <- reactive({
      req(simuDataLSD()$df)
      df <- simuDataLSD()$df
      export_layout(df, 1)
    })
    
    
    # Downloadable csv of selected dataset ----
    output$downloadCsv.lsd <- downloadHandler(
      filename = function() {
        loc <- paste("Latin_Square_Layout", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(csv_data()$file)
        write.csv(df, file, row.names = FALSE)
      }
    )
    
  })
}

## To be copied in the UI
# mod_LSD_ui("LSD_ui_1")

## To be copied in the server
# mod_LSD_server("LSD_ui_1")