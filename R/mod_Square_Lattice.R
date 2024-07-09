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
                   radioButtons(
                     ns("owndata_square"), 
                     label = "Import entries' list?",
                     choices = c("Yes", "No"), 
                     selected = "No",
                     inline = TRUE,
                     width = NULL, 
                     choiceNames = NULL, 
                     choiceValues = NULL),
                   
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
                   numericInput(inputId = ns("myseed.square"), label = "Random Seed:",
                                                       value = 5, min = 1),
                    
                   fluidRow(
                     column(6,# style=list("padding-right: 28px;"),
                            actionButton(
                              inputId = ns("RUN.square"), 
                              "Run!", 
                              icon = icon("circle-nodes", verify_fa = FALSE),
                              width = '100%'),
                     ),
                     column(6,#style=list("padding-left: 5px;"),
                            
                            actionButton(
                              inputId = ns("Simulate.square"), 
                              "Simulate!", 
                              icon = icon("greater-than-equal", verify_fa = FALSE),
                              width = '100%'),
                     )
                     
                   ), 
                   br(),
                   downloadButton(ns("downloadData.square"), "Save My Experiment", style = "width:100%")
      ),
      
      mainPanel(
        width = 8,
        fluidRow(
          tabsetPanel(
            tabPanel(
              "Summary Design",
              br(),
              div(
                shinycssloaders::withSpinner(
                  verbatimTextOutput(outputId = ns("summary_square_lattice"), 
                                     placeholder = FALSE), 
                  type = 4
                ),
                style = "padding-right: 40px;"
              )
            ),
            tabPanel("Field Layout",
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(downloadButton(ns("downloadCsv.square"), 
                                                    label =  "Excel",
                                                    icon = icon("file-csv"), 
                                                    width = '10%',
                                                    style="color: #337ab7; background-color: #fff; border-color: #2e6da4")),
                     shinycssloaders::withSpinner(
                       plotly::plotlyOutput(ns("random_layout"), width = "97%", height = "550px"),type = 5
                     ),
                     br(),
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
    
    init_data_square <- reactive({
      
      if (input$owndata_square == "Yes") {
        req(input$file.square)
        inFile <- input$file.square
        data_ingested <- load_file(name = inFile$name, 
                                   path = inFile$datapat, 
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
            "Data input needs at least two columns: ENTRY and NAME",
            type = "error")
          return(NULL)
        }
      } else {
        req(input$t.square)
        nt <- as.numeric(input$t.square)
        df <- data.frame(list(ENTRY = 1:nt, NAME = paste0("G-", 1:nt)))
        colnames(df) <- c("ENTRY", "NAME")
        data_square <- df
        treatments = nrow(data_square)
        return(list(data_square = data_square, treatments = treatments))
      }     
    })
    
    list_to_observe <- reactive({
      req(init_data_square())
      list(
        entry_list = input$owndata_square,
        entries = init_data_square()$treatments
      )
    })
    
    observeEvent(list_to_observe(), {
      
      t <- as.numeric(init_data_square()$treatments)
      if (sqrt(t) %% 1 != 0) {
        w <- 1
        k <- "No Options Available"
      }else {
        k <- sqrt(t)
        w <- 2
      }

      updateSelectInput(session = session, 
                        inputId = 'k.square', 
                        label = "Input # of Plots per IBlock:",
                        choices = k,
                        selected = k[1])
    })
    
    # getData.square
    get_data_square <- reactive({
      if (is.null(init_data_square())) {
        shinyalert::shinyalert(
          "Error!!", 
          "Check input file and try again!", 
          type = "error")
        return(NULL)
      } else return(init_data_square())
    }) |>
      bindEvent(input$RUN.square)
    
    square_inputs <- reactive({
      req(get_data_square())
      req(input$k.square)
      req(input$owndata_square)
      req(input$myseed.square)
      req(input$planter_mov_square)
      req(input$plot_start.square)
      req(input$Location.square)
      req(input$l.square)
      req(input$r.square)
      r.square <- as.numeric(input$r.square)
      k.square <- as.numeric(input$k.square)
      if (input$k.square == "No Options Available") {
        shinyalert::shinyalert(
          "Error!!", 
          "No options for this combination of treatments!", 
          type = "error")
        return(NULL)
      } 
      plot_start.square <- as.vector(unlist(strsplit(input$plot_start.square, ",")))
      plot_start <- as.numeric(plot_start.square)
      planter <- input$planter_mov_square
      site_names <- as.vector(unlist(strsplit(input$Location.square, ",")))
      seed <- as.numeric(input$myseed.square)
      sites <- as.numeric(input$l.square)
      treatments <- get_data_square()$treatments
      
      return(list(r = r.square, 
                  k = k.square, 
                  t = treatments, 
                  planter = planter,
                  plot_start = plot_start, 
                  sites = sites,
                  sites_names = site_names,
                  seed = seed))
    }) |>
      bindEvent(input$RUN.square)
    
    
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
          entriesInfoModal_SQUARE()
        )
      }
    })
    
    
    SQUARE_reactive <- eventReactive(input$RUN.square,{
      
      req(get_data_square())
      req(square_inputs())
      
      shinyjs::show(id = "downloadCsv.square", anim = FALSE)
      
      data_square <- get_data_square()$data_square
      
      if (square_inputs()$r < 2) {
        shinyalert::shinyalert(
          "Error!!", 
          "Square Lattice Design needs at least 2 replicates.", 
          type = "error")
        return(NULL)
      }
      
      square_lattice(
        t = square_inputs()$t, 
        k = square_inputs()$k, 
        r = square_inputs()$r, 
        l = square_inputs()$sites, 
        plotNumber = square_inputs()$plot_start, 
        seed = square_inputs()$seed, 
        locationNames = square_inputs()$site_names, 
        data = data_square
      ) 
      
    })
    
    output$summary_square_lattice <- renderPrint({
      req(SQUARE_reactive())
        cat("Randomization was successful!", "\n", "\n")
        print(SQUARE_reactive(), n = 6)
    })
    
    upDateSites_SQ <- reactive({
      req(square_inputs())
      locs <- square_inputs()$sites
      sites <- 1:locs
      return(list(sites = sites))
    })
    
    output$well_panel_layout_sq <- renderUI({
      req(SQUARE_reactive()$fieldBook)
      df <- SQUARE_reactive()$fieldBook
      locs_sq <- length(levels(as.factor(df$LOCATION)))
      repsSquare <- length(levels(as.factor(df$REP)))
      if ((repsSquare >= 4 & repsSquare %% 2 == 0) | (repsSquare >= 4 & sqrt(repsSquare) %% 1 == 0)) {
        stacked <- c("Vertical Stack Panel" = "vertical", "Horizontal Stack Panel" = "horizontal",  
                       "Grid Panel" = "grid_panel")
      } else {
        stacked <- c("Vertical Stack Panel" = "vertical", "Horizontal Stack Panel" = "horizontal")
      }
      obj_sq <- SQUARE_reactive()
      allBooks_sq <- plot_layout(x = obj_sq, layout = 1)$newBooks
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
                 selectInput(inputId = ns("stacked_sq"), label = "Reps layout:", 
                             choices = stacked)
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
    
    observeEvent(input$stacked_sq, {
      req(input$stacked_sq)
      req(input$l.square)
      obj_sq <- SQUARE_reactive()
      allBooks <- plot_layout(x = obj_sq, layout = 1, stacked = input$stacked_sq)$newBooks
      nBooks <- length(allBooks)
      NewlayoutOptions <- 1:nBooks
      updateSelectInput(session = session, inputId = 'layoutO_sq',
                        label = "Layout option:",
                        choices = NewlayoutOptions,
                        selected = 1
      )
    })
    
    reset_selection <- reactiveValues(reset = 0)
    
    observeEvent(input$stacked_sq, {
      reset_selection$reset <- 1
    })
    
    observeEvent(input$layoutO_sq, {
      reset_selection$reset <- 0
    })
    
    reactive_layoutSquare <- reactive({
      req(square_inputs()$planter)
      req(input$layoutO_sq)
      req(SQUARE_reactive())
      obj_sq <- SQUARE_reactive()
      
      if (reset_selection$reset == 1) {
        opt_sq <- 1
      } else opt_sq <- as.numeric(input$layoutO_sq)
      
      locSelected <- as.numeric(input$locLayout_sq)
      try(plot_layout(x = obj_sq, 
                      layout = opt_sq, 
                      planter = square_inputs()$planter, 
                      l = locSelected, 
                      stacked = input$stacked_sq), silent = TRUE)
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
        simuModal.square()
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
          simuModal.square(failed = TRUE)
        )
      }
    })
    
    
    simuDataSQUARE <- reactive({
      set.seed(input$myseed.square)
      req(reactive_layoutSquare()$allSitesFieldbook)
      if(!is.null(valsSQUARE$maxV.square) && !is.null(valsSQUARE$minV.square) && !is.null(valsSQUARE$trail.square)) {
        max <- as.numeric(valsSQUARE$maxV.square)
        min <- as.numeric(valsSQUARE$minV.square)
        df.square <- reactive_layoutSquare()$allSitesFieldbook
        cnamesdf.square <- colnames(df.square)
        df.square <- norm_trunc(a = min, b = max, data = df.square)
        colnames(df.square) <- c(cnamesdf.square[1:(ncol(df.square) - 1)], valsSQUARE$trail.square)
        a <- ncol(df.square)
      }else {
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
        new_df <- df |>
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
        
        p2 <- plotly::ggplotly(p1, tooltip="text", height = 560)
        return(p2)
      } else {
        showModal(
          heatmapInfoModal_Square()
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
      req(simuDataSQUARE()$df)
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
      req(input$typlotSQ)
      if (input$typlotSQ == 2) {
        export_layout(df, locNum(), TRUE)
      } else {
        export_layout(df, locNum())
      }
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
