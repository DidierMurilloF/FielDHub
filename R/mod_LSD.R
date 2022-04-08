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
      sidebarPanel(width = 4,
                   radioButtons(inputId = ns("owndataLSD"), label = "Import entries' list?", choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL), 
                   
                   conditionalPanel("input.owndataLSD == 'Yes'", ns = ns,
                                    fluidRow(
                                      column(8, style=list("padding-right: 28px;"),
                                             fileInput(ns("file.LSD"), label = "Upload a CSV File:", multiple = FALSE)),
                                      
                                      column(4,style=list("padding-left: 5px;"),
                                             radioButtons(ns("sep.lsd"), "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t"),
                                                          selected = ","))
                                    )             
                   ),
                   
                   conditionalPanel("input.owndataLSD != 'Yes'", ns = ns,
                                    
                                    numericInput(ns("n.lsd"), label = "Input # of Treatments:",
                                                 value = 4, min = 2),             
                   ),
                   
                   numericInput(ns("reps.lsd"), label = "Input # of Full Reps (Squares):",
                                value = 1, min = 1),
                   # selectInput(inputId = ns("planter.lsd"), label = "Plot Order Layout:",
                   #             choices = c("serpentine", "cartesian"), multiple = FALSE,
                   #             selected = "serpentine"),
                   fluidRow(
                     column(6, style=list("padding-right: 28px;"),
                            textInput(ns("plot_start.lsd"), "Starting Plot Number:", value = 101)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            textInput(ns("Location.lsd"), "Input Location:", value = "FARGO")
                     )
                   ),
                   numericInput(ns("seed.lsd"), label = "Seed Number:", value = 123, min = 1),
                   
                   fluidRow(
                     column(6,
                            actionButton(inputId = ns("RUN.lsd"), "Run!", icon = icon("cocktail"), width = '100%'),
                     ),
                     column(6,
                            actionButton(ns("Simulate.lsd"), "Simulate!", icon = icon("cocktail"), width = '100%')
                     )
                     
                   ), 
                   br(),
                   downloadButton(ns("downloadData.lsd"), "Save Experiment!", style = "width:100%")
                   
                   # fluidRow(
                   #   column(6,
                   #          downloadButton(ns("downloadData.lsd"), "Save Experiment!", style = "width:100%")
                   #   ),
                   #   column(6,
                   #          actionButton(ns("Simulate.lsd"), "Simulate!", icon = icon("cocktail"), width = '100%')
                   #   )
                   # )
      ),
      #plotly::plotlyOutput(ns("layouts_lsd"), width = "100%", height = "650px")
      # plotOutput(ns("layout_lsd"), width = "100%", height = "650px")
      mainPanel(width = 8,
          fluidRow(
            tabsetPanel(
              tabPanel("Field Layout",
                       shinycssloaders::withSpinner(
                         plotOutput(ns("layout_lsd"), width = "100%", height = "650px"),type = 5
                       ),
                       column(12, uiOutput(ns("well_panel_layout_LSD")))
              ),
              tabPanel("Field Book", 
                       shinycssloaders::withSpinner(DT::DTOutput(ns("LSD_fieldbook")), type = 5)
              )
            )
          )
                # fixedRow(
                #   column(12, align="center", uiOutput(ns("tabsetLSD"))),
                #   column(12, uiOutput(ns("well_panel_layout_LSD")))
                # )
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
    
    entryListFormat_LSD <- data.frame(list(ROW = paste("Period", 1:5, sep = ""),
                                           COLUMN = paste("Cow", 1:5, sep = ""),
                                           TREATMENT = paste("Diet", 1:5, sep = "")))
    
    entriesInfoModal_LSD <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
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
      dataUp.lsd <- load_file(name = inFile$name, path = inFile$datapat, sep = input$sep.lsd)
      return(list(dataUp.lsd = dataUp.lsd))
    })
    
    latinsquare_reactive <- eventReactive(input$RUN.lsd, {
    # latinsquare_reactive <- reactive({
      
      req(input$plot_start.lsd)
      req(input$Location.lsd)
      req(input$reps.lsd)
      req(input$seed.lsd)
      
      plot_start.lsd <- as.vector(unlist(strsplit(input$plot_start.lsd, ",")))
      plot_start.lsd <- as.numeric(plot_start.lsd)
      loc.lsd <-  as.vector(unlist(strsplit(input$Location.lsd, ",")))
      seed.number.lsd <- as.numeric(input$seed.lsd)
      
      #if (input$kindLSD == "LSD.REP") {
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
      planting_lsd <- input$planter.lsd
      allBooks_lsd<- plot_layout(x = obj_lsd, optionLayout = 1)$newBooks
      nBooks_lsd <- length(allBooks_lsd)
      layoutOptions_lsd <- 1:nBooks_lsd
      loc <-  as.vector(unlist(strsplit(input$Location.lsd, ",")))
      wellPanel(
        fluidRow(
          column(2,
                 radioButtons(ns("typlotlsd"), "Type of Plot:",
                              c("Entries/Treatments" = 1,
                                "Plots" = 2,
                                "Heatmap" = 3), selected = 1)
          ),
          column(3, #align="center",
                 selectInput(inputId = ns("layoutO_lsd"), label = "Layout option:", choices = layoutOptions_lsd)
          ),
          column(3, #align="center",
                 selectInput(inputId = ns("locLayout_lsd"), label = "Location:", choices = loc)
          )
        )
      )
    })
    
    reactive_layoutLSD <- reactive({
      req(input$layoutO_lsd)
      req(latinsquare_reactive())
      obj_lsd <- latinsquare_reactive()
      # print(obj_lsd)
      opt_lsd <- as.numeric(input$layoutO_lsd)
      planting_lsd <- input$planter.lsd
      try(plot_layout(x = obj_lsd, optionLayout = opt_lsd, planter = "cartesian"), silent = TRUE)
    })
    
    # output$layout_lsd <- renderPlot({
    #   req(latinsquare_reactive())
    #   req(input$typlotlsd)
    #   if (input$typlotlsd == 1) {
    #     reactive_layoutLSD()$out_layout
    #   } else reactive_layoutLSD()$out_layoutPlots
    # })
    
    
    
    valsLSD <- reactiveValues(maxV.lsd = NULL, minV.lsd = NULL, trail.lsd = NULL)
    
    simuModal.lsd <- function(failed = FALSE) {
      modalDialog(
        selectInput(inputId = ns("trailsLSD"), label = "Select One:", choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        conditionalPanel("input.trailsLSD == 'Other'", ns = ns,
                         textInput(inputId = ns("OtherLSD"), label = "Input Trial Name:", value = NULL)
        ),
        fluidRow(
          column(6, 
                 numericInput(ns("min.lsd"), "Input the min value", value = NULL)
          ),
          column(6, 
                 numericInput(ns("max.lsd"), "Input the max value", value = NULL)
                 
          )
          
        ),
        
        if (failed)
          div(tags$b("Invalid input of data max and min", style = "color: red;")),
        
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
      
      if(!is.null(valsLSD$maxV.lsd) && !is.null(valsLSD$minV.lsd) && !is.null(valsLSD$trail.lsd)) {
        max <- as.numeric(valsLSD$maxV.lsd)
        min <- as.numeric(valsLSD$minV.lsd)
        #df.lsd <- latinsquare_reactive()$fieldBook
        df.lsd <- reactive_layoutLSD()$fieldBookXY
        cnamesdf.lsd <- colnames(df.lsd)
        df.lsd <- norm_trunc(a = min, b = max, data = df.lsd)
        colnames(df.lsd) <- c(cnamesdf.lsd[1:(ncol(df.lsd) - 1)], valsLSD$trail.lsd)
        df.lsd <- df.lsd[order(df.lsd$ID),]
      }else {
        #df.lsd <- latinsquare_reactive()$fieldBook
        df.lsd <- reactive_layoutLSD()$fieldBookXY
      }
      return(list(df = df.lsd))
    })
    
    heatmapInfoModal_LSD <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    # output$tabsetLSD <- renderUI({
    #   req(input$typlotlsd)
    #   tabsetPanel(
    #     if (input$typlotlsd != 3) {
    #       tabPanel("Latin Square Field Layout", shinycssloaders::withSpinner(plotOutput(ns("layout.output"), width = "100%", height = "650px"),
    #                                                                          type = 5))
    #     } else {
    #       tabPanel("Latin Square Field Layout", shinycssloaders::withSpinner(plotly::plotlyOutput(ns("heatmapLSD"), width = "100%", height = "650px"),
    #                                                                          type = 5))
    #     },
    #     tabPanel("Latin Square Field Book", shinycssloaders::withSpinner(DT::DTOutput(ns("LSD.output")), type = 5))
    #   )
    #   
    # })
    
    
    
    heatmap_obj <- reactive({
      req(simuDataLSD()$df)
      # print(simuDataLSD()$df)
      if (ncol(simuDataLSD()$df) == 10) {
        locs <- factor(simuDataLSD()$df$LOCATION, levels = unique(simuDataLSD()$df$LOCATION))
        locLevels <- levels(locs)
        df = subset(simuDataLSD()$df, LOCATION == locLevels[1])
        loc <- levels(factor(df$LOCATION))
        trail <- as.character(valsLSD$trail.lsd)
        label_trail <- paste(trail, ": ")
        heatmapTitle <- paste("Heatmap for ", trail)
        new_df <- df %>%
          dplyr::mutate(text = paste0("Site: ", loc, "\n", "Row: ", df$ROW, "\n", "Col: ", df$COLUMN, "\n", "Entry: ", 
                                      df$ENTRY, "\n", label_trail, round(df[,10],2)))
        w <- as.character(valsLSD$trail.lsd)
        new_df$ROW <- as.factor(new_df$ROW) # Set up ROWS as factors
        new_df$COLUMN <- as.factor(new_df$COLUMN) # Set up COLUMNS as factors
        p1 <- ggplot2::ggplot(new_df, ggplot2::aes(x = new_df[,5], y = new_df[,4], fill = new_df[,10], text = text)) +
          ggplot2::geom_tile() +
          ggplot2::xlab("COLUMN") +
          ggplot2::ylab("ROW") +
          ggplot2::labs(fill = w) +
          viridis::scale_fill_viridis(discrete = FALSE) +
          ggplot2::ggtitle(heatmapTitle) +
          ggplot2::theme_minimal() + # I added this option 
          ggplot2::theme(plot.title = ggplot2::element_text(family="Calibri", face="bold", size=13, hjust=0.5))
        
        p2 <- plotly::ggplotly(p1, tooltip="text", width = 1150, height = 640)
        p2 
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
    
    # output$heatmapLSD <- plotly::renderPlotly({
    #   req(heatmap_obj())
    #   heatmap_obj()
    # })
    
    
    # output$layout_lsd <- plotly::renderPlotly({
    #   print(simuDataLSD()$df)
    #   req(reactive_layoutLSD())
    #   req(latinsquare_reactive())
    #   req(input$typlotlsd)
    #   if (input$typlotlsd == 1) {
    #     reactive_layoutLSD()$out_layout
    #   } else if (input$typlotlsd == 2) {
    #     reactive_layoutLSD()$out_layoutPlots
    #   } else {
    #     req(heatmap_obj())
    #     heatmap_obj()
    #   }
    # })
    
    output$layout_lsd <- renderPlot({
      print(simuDataLSD()$df)
      req(reactive_layoutLSD())
      req(latinsquare_reactive())
      req(input$typlotlsd)
      if (input$typlotlsd == 1) {
        reactive_layoutLSD()$out_layout
      } else if (input$typlotlsd == 2) {
        reactive_layoutLSD()$out_layoutPlots
      } else {
        req(heatmap_obj())
        heatmap_obj()
      }
    })
        
    
    
    
    output$LSD_fieldbook <- DT::renderDataTable({
      
      df <- simuDataLSD()$df
      
      df$LOCATION <- as.factor(df$LOCATION)
      df$PLOT <- as.factor(df$PLOT)
      df$ROW <- as.factor(df$ROW)
      df$COLUMN <- as.factor(df$COLUMN)
      df$REP <- as.factor(df$REP)
      df$TREATMENT <- as.factor(df$TREATMENT)
      a <- as.numeric(simuDataLSD()$a)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      
      DT::datatable(df, rownames = FALSE, options = list(
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
    
  })
}

## To be copied in the UI
# mod_LSD_ui("LSD_ui_1")

## To be copied in the server
# mod_LSD_server("LSD_ui_1")