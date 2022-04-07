#' RowCol UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_RowCol_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("Row-Column Design"),
    sidebarLayout(
      sidebarPanel(width = 4,

                   radioButtons(inputId = ns("owndataRCD"), label = "Import entries' list?", choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                   
                   conditionalPanel("input.owndataRCD == 'Yes'", ns = ns,
                                    fluidRow(
                                      column(8, style=list("padding-right: 28px;"),
                                             fileInput(ns("file.RCD"), label = "Upload a csv File:", multiple = FALSE)),
                                      column(4,style=list("padding-left: 5px;"),
                                             radioButtons(ns("sep.rcd"), "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t"),
                                                          selected = ","
                                             )
                                      )
                                    )
                   ),
                   
                   numericInput(ns("t.rcd"), label = "Input # of Treatments:",
                                value = 36, min = 2),
                   
                   fluidRow(
                     column(6, style=list("padding-right: 28px;"),
                            selectInput(inputId = ns("k.rcd"), label = "Input # of Rows:", choices = ""),
                            # numericInput(ns("k.rcd"), label = "Input # of Rows:",
                            #              value = NULL, min = 2)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            numericInput(ns("r.rcd"), label = "Input # of Full Reps:",
                                         value = 3, min = 2)
                     )
                   ),
                   
                   numericInput(inputId = ns("l.rcd"), label = "Input # of Locations:", value = 1, min = 1),
                   
                   fluidRow(

                     column(6, style=list("padding-right: 28px;"),
                            textInput(ns("plot_start.rcd"), "Starting Plot Number:", value = 101)
                     ),
                     column(6, style=list("padding-left: 5px;"),
                            textInput(ns("Location.rcd"), "Input Location:", value = "FARGO")
                     )
                   ),
                   
                   selectInput(inputId = ns("planter_mov_rcd"), label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"), multiple = FALSE,
                               selected = "serpentine"),
                   
                   numericInput(ns("seed.rcd"), label = "Seed Number:", value = 1),
                   
                   fluidRow(
                     column(6,
                            actionButton(inputId = ns("RUN.rcd"), "Run!", icon = icon("cocktail"), width = '100%'),
                     ),
                     column(6,
                            actionButton(ns("Simulate.RowCol"), "Simulate!", icon = icon("cocktail"), width = '100%')
                     )
                     
                   ), 
                   br(),
                   downloadButton(ns("downloadData.rowcolD"), "Save Experiment!", style = "width:100%")
      ),

      mainPanel(
        width = 8,
        fluidRow(
          tabsetPanel(
            tabPanel("Field Layout",
                     shinycssloaders::withSpinner(
                       plotly::plotlyOutput(ns("layouts"), width = "100%", height = "650px"),type = 5
                     ),
                     column(12,uiOutput(ns("well_panel_layout_ROWCOL")))
            ),
            tabPanel("Field Book", 
                     shinycssloaders::withSpinner(DT::DTOutput(ns("rowcolD")), type = 5)
            )
          )
        )
      )
    )
  )
}

#' RowCol Server Functions
#'
#' @noRd 
mod_RowCol_server <- function(id){
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    entryListFormat_RCD <- data.frame(ENTRY = 1:9, 
                                       NAME = c(paste("Genotype", LETTERS[1:9], sep = "")))
    entriesInfoModal_RCD <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        renderTable(entryListFormat_RCD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        h4("Entry numbers can be any set of consecutive positive numbers."),
        easyClose = FALSE
      )
    }
    
    toListen <- reactive({
      list(input$owndataRCD)
    })
    
    observeEvent(toListen(), {
      if (input$owndataRCD == "Yes") {
        showModal(
          shinyjqui::jqui_draggable(
            entriesInfoModal_RCD()
          )
        )
      }
    })
    
    getData.rcd <- reactive({
      req(input$file.RCD)
      inFile <- input$file.RCD
      dataUp.rcd <- load_file(name = inFile$name, path = inFile$datapat, sep = input$sep.rcd)
      return(list(dataUp.rcd = dataUp.rcd))
    })
    
    Get_tROWCOL <- reactive({
      if(is.null(input$file.RCD)) {
        req(input$t.rcd)
        t.ROWCOL <- input$t.rcd
      }else {
        req(input$file.RCD)
        t.ROWCOL <- nrow(getData.rcd()$dataUp.rcd)
      }
      return(list(t.ROWCOL = t.ROWCOL))
    })
    
    
    
    observeEvent(Get_tROWCOL()$t.ROWCOL, {
      req(Get_tROWCOL()$t.ROWCOL)
      
      t <- as.numeric(Get_tROWCOL()$t.ROWCOL)
      if (numbers::isPrime(t)) {
        w <- 1
        k <- "No Options Available"
      }else {
        k <- numbers::divisors(t)
        k <- k[2:(length(k) - 1)]
        w <- 2
      }
      
      updateSelectInput(session = session, inputId = 'k.rcd', label = "Input # of Rows:",
                        choices = k, selected = k[1])
      
    })
    
    
    
    
    RowCol_reactive <- eventReactive(input$RUN.rcd, {
      
      req(input$t.rcd)
      req(input$k.rcd)
      req(input$r.rcd)
      req(input$seed.rcd)
      req(input$plot_start.rcd)
      req(input$Location.rcd)
      
      t.rcd <- as.numeric(input$t.rcd)
      k.rcd <- as.numeric(input$k.rcd)
      r.rcd <- as.numeric(input$r.rcd)
      
      
      plot_start.rcd <- as.vector(unlist(strsplit(input$plot_start.rcd, ",")))
      plot_start.rcd <- as.numeric(plot_start.rcd)
      loc.rcd <-  as.vector(unlist(strsplit(input$Location.rcd, ",")))
      
      if (input$owndataRCD == "Yes") {
        t.rcd <- as.numeric(Get_tROWCOL()$t.ROWCOL)
        data.rcd <- getData.rcd()$dataUp.rcd
      }else {
        req(input$t.rcd)
        t.rcd <- as.numeric(input$t.rcd)
        data.rcd <- NULL
      }
      seed.rcd <- as.numeric(input$seed.rcd)
      l.rcd <- as.numeric(input$l.rcd)

      row_column(t = t.rcd, nrows = k.rcd, r = r.rcd, l = l.rcd,
                 plotNumber = plot_start.rcd, 
                 locationNames = loc.rcd,
                 seed = seed.rcd, 
                 data = data.rcd)
      
    })
    
    upDateSites <- eventReactive(input$RUN.rcd, {
      req(input$l.rcd)
      locs <- as.numeric(input$l.rcd)
      sites <- 1:locs
      return(list(sites = sites))
    })
    
    output$well_panel_layout_ROWCOL <- renderUI({
      req(RowCol_reactive()$fieldBook)
      obj_rcd <- RowCol_reactive()
      planting_rcd <- input$planter_mov_rcd
      allBooks_rcd<- plot_layout(x = obj_rcd, optionLayout = 1)$newBooks
      nBooks_rcd <- length(allBooks_rcd)
      layoutOptions_rcd <- 1:nBooks_rcd
      orderReps <- c("Vertical Stack Panel" = "vertical_stack_panel", "Horizontal Stack Panel" = "horizontal_stack_panel")
      #loc <-  as.vector(unlist(strsplit(input$Location.rcd, ",")))
      sites <- as.numeric(input$l.rcd)
      wellPanel(
        column(2,
               radioButtons(ns("typlotrcd"), "Type of Plot:",
                            c("Entries/Treatments" = 1,
                              "Plots" = 2,
                              "Heatmap" = 3))
        ),
        fluidRow(
 
          column(3,
                 selectInput(inputId = ns("orderRepsRowCol"), label = "Reps layout:", 
                             choices = orderReps)
          ),
          column(3, #align="center",
                 selectInput(inputId = ns("layoutO_rcd"), label = "Layout option:", choices = layoutOptions_rcd)
          ),
          column(3, #align="center",
                 selectInput(inputId = ns("locLayout_rcd"), label = "Location:", choices = as.numeric(upDateSites()$sites))
          )
        )
      )
    })
    
    
    observeEvent(input$orderRepsRowCol, {
      req(input$orderRepsRowCol)
      req(input$l.rcd)
      obj <- RowCol_reactive()
      allBooks <- plot_layout(x = obj, optionLayout = 1, orderReps = input$orderRepsRowCol)$newBooks
      nBooks <- length(allBooks)
      NewlayoutOptions <- 1:nBooks
      updateSelectInput(session = session, inputId = 'layoutO_rcd',
                        label = "Layout option:",
                        choices = NewlayoutOptions,
                        selected = 1
      )
    })
    
    reactive_layoutROWCOL <- reactive({
      req(input$layoutO_rcd)
      req(RowCol_reactive())
      obj_rcd <- RowCol_reactive()
      opt_rcd <- as.numeric(input$layoutO_rcd)
      planting_rcd <- input$planter_mov_rcd
      locSelected <- as.numeric(input$locLayout_rcd)
      try(plot_layout(x = obj_rcd, optionLayout = opt_rcd, planter = planting_rcd, l = locSelected, 
                      orderReps = input$orderRepsRowCol), silent = TRUE)
    })
    
    # output$layout_rowcol <- renderPlot({
    #   req(RowCol_reactive())
    #   req(input$typlotrcd)
    #   if (input$typlotrcd == 1) {
    #     reactive_layoutROWCOL()$out_layout
    #   } else reactive_layoutROWCOL()$out_layoutPlots
    # })
    
    
    
    valsRowColD <- reactiveValues(maxV.RowCol = NULL, minV.RowCol = NULL, trail.RowCol = NULL)
    
    simuModal.RowCol <- function(failed = FALSE) {
      modalDialog(
        selectInput(inputId = ns("trailsRowCol"), label = "Select One:", choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        conditionalPanel("input.trailsRowCol == 'Other'", ns = ns,
                         textInput(inputId = ns("OtherRowCol"), label = "Input Trial Name:", value = NULL)
        ),
        fluidRow(
          column(6, 
                 numericInput(ns("min.RowCol"), "Input the min value", value = NULL)
          ),
          column(6, 
                 numericInput(ns("max.RowCol"), "Input the max value", value = NULL)
                 
          )
          
        ),
        
        if (failed)
          div(tags$b("Invalid input of data max and min", style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ok.RowCol"), "GO")
        )
        
      )
      
    }
    
    observeEvent(input$Simulate.RowCol, {
      req(RowCol_reactive()$fieldBook)
      showModal(
        shinyjqui::jqui_draggable(
          simuModal.RowCol()
        )
      )
    })
    
    observeEvent(input$ok.RowCol, {
      req(input$max.RowCol, input$min.RowCol)
      if (input$max.RowCol > input$min.RowCol && input$min.RowCol != input$max.RowCol) {
        valsRowColD$maxV.RowCol <- input$max.RowCol
        valsRowColD$minV.RowCol <- input$min.RowCol
        if(input$trailsRowCol == "Other") {
          req(input$OtherRowCol)
          if(!is.null(input$OtherRowCol)) {
            valsRowColD$trail.RowCol <- input$OtherRowCol
          }else showModal(simuModal.RowCol(failed = TRUE))
        }else {
          valsRowColD$trail.RowCol <- as.character(input$trailsRowCol)
        }
        removeModal()
      }else {
        showModal(
          shinyjqui::jqui_draggable(
            simuModal.RowCol(failed = TRUE)
          )
        )
      }
    })
    
    simuData_RowCol <- reactive({
      set.seed(input$seed.rcd)
      req(RowCol_reactive()$fieldBook)
      if(!is.null(valsRowColD$maxV.RowCol) && !is.null(valsRowColD$minV.RowCol) && !is.null(valsRowColD$trail.RowCol)) {
        max <- as.numeric(valsRowColD$maxV.RowCol)
        min <- as.numeric(valsRowColD$minV.RowCol)
        #df.RowCol <- RowCol_reactive()$fieldBook
        df.RowCol <- reactive_layoutROWCOL()$allSitesFieldbook
        cnamesdf.RowCol <- colnames(df.RowCol)
        df.RowCol <- norm_trunc(a = min, b = max, data = df.RowCol)
        colnames(df.RowCol) <- c(cnamesdf.RowCol[1:(ncol(df.RowCol) - 1)], valsRowColD$trail.RowCol)
        a <- ncol(df.RowCol)
      }else {
        #df.RowCol <- RowCol_reactive()$fieldBook  
        df.RowCol <- reactive_layoutROWCOL()$allSitesFieldbook
        a <- ncol(df.RowCol)
      }
      return(list(df = df.RowCol, a = a))
    })
    
    heatmapInfoModal_RCD <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    # output$tabsetRCD <- renderUI({
    #   req(input$typlotrcd)
    #   tabsetPanel(
    #     if (input$typlotrcd != 3) {
    #       tabPanel("Split Plot Field Layout", shinycssloaders::withSpinner(plotOutput(ns("layout.rcd"), width = "100%", height = "650px"),
    #                                                                        type = 5))
    #     } else {
    #       tabPanel("Split Plot Field Layout", shinycssloaders::withSpinner(plotly::plotlyOutput(ns("heatmapRCD"), width = "100%", height = "650px"),
    #                                                                        type = 5))
    #     },
    #     tabPanel("Split Plot Field Book", shinycssloaders::withSpinner(DT::DTOutput(ns("rowcolD")), type = 5))
    #   )
    #   
    # })
    
    locNum <- reactive(
      return(as.numeric(input$locLayout_rcd))
    )
    
    heatmap_obj <- reactive({
      req(simuData_RowCol()$df)
      if (ncol(simuData_RowCol()$df) == 10) {
        locs <- factor(simuData_RowCol()$df$LOCATION, levels = unique(simuData_RowCol()$df$LOCATION))
        locLevels <- levels(locs)
        df = subset(simuData_RowCol()$df, LOCATION == locLevels[locNum()])
        loc <- levels(factor(df$LOCATION))
        trail <- as.character(valsRowColD$trail.RowCol)
        label_trail <- paste(trail, ": ")
        heatmapTitle <- paste("Heatmap for ", trail)
        new_df <- df %>%
          dplyr::mutate(text = paste0("Site: ", loc, "\n", "Row: ", df$ROW, "\n", "Col: ", df$COLUMN, "\n", "Entry: ", 
                                      df$ENTRY, "\n", label_trail, round(df[,10],2)))
        w <- as.character(valsRowColD$trail.RowCol)
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
        return(p2)
      } else {
        showModal(
          shinyjqui::jqui_draggable(
            heatmapInfoModal_RCD()
          )
        )
        return(NULL)
      }
    })
    
    
    output$layouts <- plotly::renderPlotly({
      #reactive_layoutSPD()$out_layout
      req(RowCol_reactive())
      req(input$typlotrcd)
      if (input$typlotrcd == 1) {
        reactive_layoutROWCOL()$out_layout
      } else if (input$typlotrcd == 2) {
        reactive_layoutROWCOL()$out_layoutPlots
      } else {
        req(heatmap_obj())
        heatmap_obj()
      }
    })
    
    output$rowcolD <- DT::renderDataTable({
      df <- simuData_RowCol()$df
      df$LOCATION <- as.factor(df$LOCATION)
      df$PLOT <- as.factor(df$PLOT)
      df$ROW <- as.factor(df$ROW)
      df$COLUMN <- as.factor(df$COLUMN)
      df$REP <- as.factor(df$REP)
      df$ROW_REP <- as.factor(df$ROW_REP)
      df$COLUMN_REP <- as.factor(df$COLUMN_REP)
      df$ENTRY <- as.factor(df$ENTRY)
      a <- as.numeric(simuData_RowCol()$a)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      DT::datatable(df, filter = 'top', rownames = FALSE, options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
      
    })

    output$downloadData.rowcolD <- downloadHandler(
      filename = function() {
        loc <- paste("Row-Column_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(simuData_RowCol()$df)
        write.csv(df, file, row.names = FALSE)
      }
    )
  })
}
    
## To be copied in the UI
# mod_RowCol_ui("RowCol_ui_1")
    
## To be copied in the server
# mod_RowCol_server("RowCol_ui_1")
