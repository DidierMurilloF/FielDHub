#' Square_Lattice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Square_Lattice_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("Square Lattice Design"),
    sidebarLayout(
      sidebarPanel(width = 4,
                   radioButtons(ns("owndata_square"), label = "Import entries' list?", choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                   
                   conditionalPanel("input.owndata_square != 'Yes'", ns = ns,
                                    numericInput(ns("t.square"), label = "Input # of Treatments:",
                                                 value = 49, min = 2)
                   ),
                   conditionalPanel("input.owndata_square == 'Yes'", ns = ns,
                                    fluidRow(
                                      column(8, style=list("padding-right: 28px;"),
                                             fileInput(inputId = ns("file.square"), label = "Upload a CSV File:", multiple = FALSE)),
                                      column(4, style=list("padding-left: 5px;"),
                                             radioButtons(inputId = ns("sep.square"), "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t"),
                                                          selected = ","))
                                    )        
                   ),
                   numericInput(inputId = ns("r.square"), label = "Input # of Full Reps:", value = 3, min = 2),
                   selectInput(inputId = ns("k.square"), label = "Input # of Plots per IBlock:", choices = ""),
                   numericInput(inputId = ns("l.square"), label = "Input # of Locations:", value = 1, min = 1),
                   
                   selectInput(inputId = ns("planter_mov_square"), label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"), multiple = FALSE,
                               selected = "serpentine"),
                   
                   fluidRow(
                     column(6, style=list("padding-right: 28px;"),
                            textInput(inputId = ns("plot_start.square"), "Starting Plot Number:", value = 101)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            textInput(inputId = ns("Location.square"), "Input Location:", value = "FARGO")
                     )
                   ),
                   numericInput(inputId = ns("myseed.square"), label = "Seed Number:",
                                                       value = 5, min = 1),
                    
                   fluidRow(
                     column(6,# style=list("padding-right: 28px;"),
                            actionButton(inputId = ns("RUN.square"), "Run!", icon = icon("cocktail"), width = '100%'),
                     ),
                     column(6,#style=list("padding-left: 5px;"),
                            
                            actionButton(inputId = ns("Simulate.square"), "Simulate!", icon = icon("cocktail"), width = '100%')
                     )
                     
                   ), 
                   br(),
                   downloadButton(ns("downloadData.square"), "Save My Experiment", style = "width:100%")
      ),
      
      mainPanel(
        width = 8,
        fluidRow(
          tabsetPanel(
            tabPanel("Field Layout",
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(downloadButton(ns("downloadCsv.square"), 
                                                    label =  "Excel",
                                                    icon = icon("file-csv"), 
                                                    width = '10%',
                                                    style="color: #337ab7; background-color: #fff; border-color: #2e6da4")),
                     shinycssloaders::withSpinner(
                       plotly::plotlyOutput(ns("random_layout"), width = "98%", height = "650px"),type = 5
                     ),
                     column(12, uiOutput(ns("well_panel_layout_sq")))
            ),
            tabPanel("Field Book", 
                     shinycssloaders::withSpinner(DT::DTOutput(ns("square_fieldbook")), type = 5)
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
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    shinyjs::useShinyjs()
    
    getData.square <- reactive({
      req(input$file.square)
      inFile <- input$file.square
      dataUp.square <- load_file(name = inFile$name, path = inFile$datapat, sep = input$sep.square)
      return(list(dataUp.square = dataUp.square))
    })
    
    get_tSQUARE <- reactive({
      if(input$owndata_square != "Yes") {
        req(input$t.square)
        t_square <- input$t.square
      }else {
        req(input$file.square)
        t_square <- nrow(getData.square()$dataUp.square)
      }
      return(list(t_square = t_square))
    })
    
    observeEvent(get_tSQUARE()$t_square, {
      req(get_tSQUARE()$t_square)
      
      t <- as.numeric(get_tSQUARE()$t_square)
      if (sqrt(t) %% 1 != 0) {
        w <- 1
        k <- "No Options Available"
      }else {
        k <- sqrt(t)
        w <- 2
      }

      updateSelectInput(session = session, inputId = 'k.square', label = "Input # of Plots per IBlock:",
                        choices = k, selected = k[1])
    })
    
    
    entryListFormat_SQUARE <- data.frame(ENTRY = 1:9, 
                                         NAME = c(paste("Genotype", LETTERS[1:9], sep = "")))
    entriesInfoModal_SQUARE <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        renderTable(entryListFormat_SQUARE,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        h4("Entry numbers can be any set of consecutive positive numbers."),
        easyClose = FALSE
      )
    }
    
    toListen <- reactive({
      list(input$owndata_square)
    })
    
    observeEvent(toListen(), {
      if (input$owndata_square == "Yes") {
        showModal(
          shinyjqui::jqui_draggable(
            entriesInfoModal_SQUARE()
          )
        )
      }
    })
    
    
    SQUARE_reactive <- eventReactive(input$RUN.square,{
      
      req(input$k.square)
      req(input$owndata_square)
      req(input$myseed.square)
      req(input$plot_start.square)
      req(input$Location.square)
      req(input$l.square)
      req(input$r.square)
      r.square <- as.numeric(input$r.square)
      k.square <- as.numeric(input$k.square)
      plot_start.square <- as.vector(unlist(strsplit(input$plot_start.square, ",")))
      plot_start.square <- as.numeric(plot_start.square)
      loc <- as.vector(unlist(strsplit(input$Location.square, ",")))
      seed.rcbd <- as.numeric(input$myseed.square)
      
      shinyjs::show(id = "downloadCsv.square", anim = FALSE)
      
      if (input$owndata_square == "Yes") {
        t.square <- as.numeric(get_tSQUARE()$t_square)
        data.square <- getData.square()$dataUp.square
      }else {
        req(input$t.square)
        t.square <- as.numeric(input$t.square)
        data.square <- NULL
      }
      seed.square <- as.numeric(input$myseed.square)
      l.square <- as.numeric(input$l.square)
      
      square_lattice(t = t.square, k = k.square, r = r.square, l = l.square, plotNumber = plot_start.square, 
                     seed = seed.square, locationNames = loc, data = data.square) 
      
    })
    
    upDateSites_SQ <- eventReactive(input$RUN.square, {
      req(input$l.square)
      locs <- as.numeric(input$l.square)
      sites <- 1:locs
      return(list(sites = sites))
    })
    
    output$well_panel_layout_sq <- renderUI({
      req(SQUARE_reactive()$fieldBook)
      df <- SQUARE_reactive()$fieldBook
      locs_sq <- length(levels(as.factor(df$LOCATION)))
      repsSquare <- length(levels(as.factor(df$REP)))
      if ((repsSquare >= 4 & repsSquare %% 2 == 0) | (repsSquare >= 4 & sqrt(repsSquare) %% 1 == 0)) {
        orderReps <- c("Vertical Stack Panel" = "vertical_stack_panel", "Horizontal Stack Panel" = "horizontal_stack_panel",  
                       "Grid Panel" = "grid_panel")
      } else {
        orderReps <- c("Vertical Stack Panel" = "vertical_stack_panel", "Horizontal Stack Panel" = "horizontal_stack_panel")
      }
      obj_sq <- SQUARE_reactive()
      allBooks_sq <- plot_layout(x = obj_sq, optionLayout = 1)$newBooks
      nBooks_sq <- length(allBooks_sq)
      layoutOptions_sq <- 1:nBooks_sq
      wellPanel(
        column(3,
               radioButtons(ns("typlotSQ"), "Type of Plot:",
                            c("Entries/Treatments" = 1,
                              "Plots" = 2,
                              "Heatmap" = 3))
        ),
        fluidRow(
          column(3,
                 selectInput(inputId = ns("orderReps_sq"), label = "Reps layout:", 
                             choices = orderReps)
          ),
          column(2, #align="center",
                 selectInput(inputId = ns("layoutO_sq"), label = "Layout option:", choices = layoutOptions_sq, selected = 1)
          ),
          column(2, #align="center",
                 selectInput(inputId = ns("locLayout_sq"), label = "Location:", choices = as.numeric(upDateSites_SQ()$sites))
          )
        )
      )
    })
    
    observeEvent(input$orderReps_sq, {
      req(input$orderReps_sq)
      req(input$l.square)
      obj_sq <- SQUARE_reactive()
      allBooks <- plot_layout(x = obj_sq, optionLayout = 1, orderReps = input$orderReps_sq)$newBooks
      nBooks <- length(allBooks)
      NewlayoutOptions <- 1:nBooks
      updateSelectInput(session = session, inputId = 'layoutO_sq',
                        label = "Layout option:",
                        choices = NewlayoutOptions,
                        selected = 1
      )
    })
    
    reactive_layoutSquare <- reactive({
      req(input$layoutO_sq)
      req(SQUARE_reactive())
      obj_sq <- SQUARE_reactive()
      opt_sq <- as.numeric(input$layoutO_sq)
      locSelected <- as.numeric(input$locLayout_sq)
      try(plot_layout(x = obj_sq, optionLayout = opt_sq, planter = input$planter_mov_square, 
                      l = locSelected, 
                      orderReps = input$orderReps_sq), silent = TRUE)
    })
    
    output$layout.output_sq <- renderPlot({
      req(reactive_layoutSquare())
      req(SQUARE_reactive())
      req(input$typlotSQ)
      if (input$typlotSQ == 1) {
        reactive_layoutSquare()$out_layout
      } else if (input$typlotSQ == 2) {
        reactive_layoutSquare()$out_layoutPlots
      }
      
    })
    
    valsSQUARE <- reactiveValues(maxV.square = NULL, minV.square = NULL, trail.square = NULL)
    
    simuModal.square <- function(failed = FALSE) {
      
      modalDialog(
        h4("Generate a random response variable:"),
        selectInput(inputId = ns("trailsSQUARE"), label = "Select One:", choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        conditionalPanel("input.trailsSQUARE == 'Other'", ns = ns,
                         textInput(inputId = ns("OtherSQUARE"), label = "Input Trail Name:", value = NULL)
        ),
        fluidRow(
          column(6,
                 numericInput(inputId = ns("min.square"), "Input the min value", value = NULL)
          ),
          column(6,
                 numericInput(inputId = ns("max.square"), "Input the max value", value = NULL)
                 
          )
          
        ),
        
        if (failed)
          div(tags$b("Invalid input of data max and min", style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(inputId = ns("ok.square"), "GO")
        )
        
      )
      
    }
    
    observeEvent(input$Simulate.square, {
      req(input$k.square)
      req(input$r.square)
      req(reactive_layoutSquare()$fieldBookXY)
      showModal(
        shinyjqui::jqui_draggable(
          simuModal.square()
        )
      )
    })
    
    observeEvent(input$ok.square, {
      req(input$max.square, input$min.square)
      if (input$max.square > input$min.square && input$min.square != input$max.square) {
        valsSQUARE$maxV.square <- input$max.square
        valsSQUARE$minV.square <- input$min.square
        if(input$trailsSQUARE == "Other") {
          req(input$OtherSQUARE)
          if(!is.null(input$OtherSQUARE)) {
            valsSQUARE$trail.square <- as.character(input$OtherSQUARE)
          }else showModal(simuModal.square(failed = TRUE))
        }else {
          valsSQUARE$trail.square <- as.character(input$trailsSQUARE)
        }
        removeModal()
      }else {
        showModal(
          shinyjqui::jqui_draggable(
            simuModal.square(failed = TRUE)
          )
        )
      }
    })
    
    
    simuDataSQUARE <- reactive({
      set.seed(input$myseed.square)
      #req(SQUARE_reactive()$fieldBook)
      req(reactive_layoutSquare()$allSitesFieldbook)
      if(!is.null(valsSQUARE$maxV.square) && !is.null(valsSQUARE$minV.square) && !is.null(valsSQUARE$trail.square)) {
        max <- as.numeric(valsSQUARE$maxV.square)
        min <- as.numeric(valsSQUARE$minV.square)
        #df.square <- SQUARE_reactive()$fieldBook
        df.square <- reactive_layoutSquare()$allSitesFieldbook
        cnamesdf.square <- colnames(df.square)
        df.square <- norm_trunc(a = min, b = max, data = df.square)
        colnames(df.square) <- c(cnamesdf.square[1:(ncol(df.square) - 1)], valsSQUARE$trail.square)
        a <- ncol(df.square)
      }else {
        #df.square <-  SQUARE_reactive()$fieldBook
        df.square <- reactive_layoutSquare()$allSitesFieldbook
        a <- ncol(df.square)
      }
      return(list(df = df.square, a = a))
    })
    
    heatmapInfoModal_Square <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    locNum <- reactive(
      return(as.numeric(input$locLayout_sq))
    )
    
    heatmap_obj <- reactive({
      req(simuDataSQUARE()$df)
      if (ncol(simuDataSQUARE()$df) == 11) {
        locs <- factor(simuDataSQUARE()$df$LOCATION, levels = unique(simuDataSQUARE()$df$LOCATION))
        locLevels <- levels(locs)
        df = subset(simuDataSQUARE()$df, LOCATION == locLevels[locNum()])
        loc <- levels(factor(df$LOCATION))
        trail <- as.character(valsSQUARE$trail.square)
        label_trail <- paste(trail, ": ")
        heatmapTitle <- paste("Heatmap for ", trail)
        new_df <- df %>%
          dplyr::mutate(text = paste0("Site: ", loc, "\n", 
                                      "Row: ", df$ROW, "\n", 
                                      "Col: ", df$COLUMN, "\n", 
                                      "Entry: ", df$ENTRY, "\n", 
                                      label_trail, round(df[,11],2)))
        w <- as.character(valsSQUARE$trail.square)
        new_df$ROW <- as.factor(new_df$ROW) # Set up ROWS as factors
        new_df$COLUMN <- as.factor(new_df$COLUMN) # Set up COLUMNS as factors
        p1 <- ggplot2::ggplot(new_df, ggplot2::aes(x = new_df[,5], 
                                                   y = new_df[,4],
                                                   fill = new_df[,11], 
                                                   text = text)) +
          ggplot2::geom_tile() +
          ggplot2::xlab("COLUMN") +
          ggplot2::ylab("ROW") +
          ggplot2::labs(fill = w) +
          viridis::scale_fill_viridis(discrete = FALSE) +
          ggplot2::ggtitle(heatmapTitle) +
          ggplot2::theme_minimal() + 
          ggplot2::theme(plot.title = ggplot2::element_text(family="Calibri", face="bold", size=13, hjust=0.5))
        
        p2 <- plotly::ggplotly(p1, tooltip="text", width = 1250, height = 640)
        return(p2)
      } else {
        showModal(
          shinyjqui::jqui_draggable(
            heatmapInfoModal_Square()
          )
        )
        return(NULL)
      }
    })
    
    output$random_layout <- plotly::renderPlotly({
      req(SQUARE_reactive())
      req(reactive_layoutSquare())
      req(input$typlotSQ)
      if (input$typlotSQ == 1) {
        reactive_layoutSquare()$out_layout
      } else if (input$typlotSQ == 2) {
        reactive_layoutSquare()$out_layoutPlots
      } else {
        req(heatmap_obj())
        heatmap_obj()
      }
    })
    
    output$square_fieldbook <- DT::renderDataTable({
      req(input$k.square)
      k.square <- input$k.square
      if (k.square == "No Options Available") {
        validate("A Square Lattice requires the number of treatments to be a square number.")
      }
      df <- simuDataSQUARE()$df
      df$LOCATION <- as.factor(df$LOCATION)
      df$PLOT <- as.factor(df$PLOT)
      df$ROW <- as.factor(df$ROW)
      df$COLUMN <- as.factor(df$COLUMN)
      df$REP <- as.factor(df$REP)
      df$IBLOCK <- as.factor(df$IBLOCK)
      df$UNIT <- as.factor(df$UNIT)
      df$ENTRY <- as.factor(df$ENTRY)
      df$TREATMENT <- as.factor(df$TREATMENT)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      
      DT::datatable(df,
                    filter = 'top',
                    rownames = FALSE, 
                    options = list(
                      columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    })
    
    output$downloadData.square <- downloadHandler(
      filename = function() {
        loc <- paste("Square_Lattice_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        req(simuDataSQUARE()$df)
        df <- as.data.frame(simuDataSQUARE()$df)
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    csv_data <- reactive({
      req(simuDataSQUARE()$df)
      df <- simuDataSQUARE()$df
      export_layout(df, locNum())
    })
    
    
    # Downloadable csv of selected dataset ----
    output$downloadCsv.square <- downloadHandler(
      filename = function() {
        loc <- paste("Square_Lattice_Layout", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(csv_data()$file)
        write.csv(df, file, row.names = FALSE)
      }
    )
    
  })
}
