#' Rectangular_Lattice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Rectangular_Lattice_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("Rectangular Lattice Design"),
    sidebarLayout(
      sidebarPanel(width = 4,
                   radioButtons(ns("owndata_rectangular"), label = "Import entries' list?", choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                   
                   conditionalPanel("input.owndata_rectangular != 'Yes'", ns = ns,
                                    numericInput(ns("t.rectangular"), label = "Input # of Treatments:",
                                                 value = 30, min = 2)
                   ),
                   conditionalPanel("input.owndata_rectangular == 'Yes'", ns = ns,
                                    fluidRow(
                                      column(8, style=list("padding-right: 28px;"),
                                             fileInput(inputId = ns("file.rectangular"), label = "Upload a CSV File:", multiple = FALSE)),
                                      column(4, style=list("padding-left: 5px;"),
                                             radioButtons(inputId = ns("sep.rectangular"), "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t"),
                                                          selected = ","))
                                    )        
                   ),
                   
                   numericInput(inputId = ns("r.rectangular"), label = "Input # of Full Reps:", value = 3, min = 2),
                   selectInput(inputId = ns("k.rectangular"), label = "Input # of Plots per IBlock:", choices = ""),
                   numericInput(inputId = ns("l.rectangular"), label = "Input # of Locations:", value = 1, min = 1),
                   
                   selectInput(inputId = ns("planter_mov_rect"), label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"), multiple = FALSE,
                               selected = "serpentine"),

                   fluidRow(
                     column(6, style=list("padding-right: 28px;"),
                            textInput(inputId = ns("plot_start.rectangular"), "Starting Plot Number:", value = 101)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            textInput(inputId = ns("Location.rectangular"), "Input Location:", value = "FARGO")
                     )
                   ), 
                   numericInput(inputId = ns("myseed.rectangular"), label = "Seed Number:",
                                value = 007, min = 1),
                   fluidRow(
                     column(6,
                            actionButton(inputId = ns("RUN.rectangular"), "Run!", icon = icon("cocktail"), width = '100%'),
                     ),
                     column(6,
                            actionButton(inputId = ns("Simulate.rectangular"), "Simulate!", icon = icon("cocktail"), width = '100%')
                     )
                     
                   ), 
                   br(),
                   downloadButton(ns("downloadData.rectangular"), "Save My Experiment", style = "width:100%")
      ),
      
      mainPanel(
        width = 8,
        fluidRow(
          tabsetPanel(
            tabPanel("Field Layout",
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(downloadButton(ns("downloadCsv.rectangular"), 
                                                    label =  "Excel",
                                                    icon = icon("file-csv"), 
                                                    width = '10%',
                                                    style="color: #337ab7; background-color: #fff; border-color: #2e6da4")),
                     shinycssloaders::withSpinner(
                       plotly::plotlyOutput(ns("random_layout"), width = "98%", height = "650px"),type = 5
                     ),
                     column(12,uiOutput(ns("well_panel_layout_rt")))
            ),
            tabPanel("Field Book", 
                     shinycssloaders::withSpinner(DT::DTOutput(ns("rectangular_fieldbook")), type = 5)
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
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shinyjs::useShinyjs()
    
    getData.rectangular <- reactive({
      req(input$file.rectangular)
      inFile <- input$file.rectangular
      dataUp.rectangular<- load_file(name = inFile$name, path = inFile$datapat, sep = input$sep.rectangular)
      return(list(dataUp.rectangular= dataUp.rectangular))
    })
    
    get_tRECT <- reactive({
      if(input$owndata_rectangular != "Yes") {
        req(input$t.rectangular)
        t.rectangular <- input$t.rectangular
      }else {
        req(input$file.rectangular)
        t.rectangular <- nrow(getData.rectangular()$dataUp.rectangular)
      }
      return(list(t.rectangular = t.rectangular))
    })
    
    observeEvent(get_tRECT()$t.rectangular, {
      req(get_tRECT()$t.rectangular)
      
      t <- as.numeric(get_tRECT()$t.rectangular)
      D <- numbers::divisors(t)
      D <- D[2:(length(D)-1)]
      pk <- numeric()
      z <- 1
      for (i in D) {
        s <- t / i
        if (i == s - 1) {
          pk[z] <- i
          z <- z + 1
        }else z <- z
      }
      if (length(pk) == 0) {
        k <- "No Options Available"
      }else {
        k <- pk
      }
      
      updateSelectInput(session = session, inputId = 'k.rectangular', label = "Input # of Plots per IBlock:",
                        choices = k, selected = k[1])
    })
    
    
    entryListFormat_RECT <- data.frame(ENTRY = 1:9, 
                                       NAME = c(paste("Genotype", LETTERS[1:9], sep = "")))
    entriesInfoModal_RECT <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        renderTable(entryListFormat_RECT,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        h4("Entry numbers can be any set of consecutive positive numbers."),
        easyClose = FALSE
      )
    }
    
    toListen <- reactive({
      list(input$owndata_rectangular)
    })
    
    observeEvent(toListen(), {
      if (input$owndata_rectangular == "Yes") {
        showModal(
          shinyjqui::jqui_draggable(
            entriesInfoModal_RECT()
          )
        )
      }
    })
    
    
    RECTANGULAR_reactive <- eventReactive(input$RUN.rectangular,{
      
      req(input$k.rectangular)
      req(input$owndata_rectangular)
      req(input$myseed.rectangular)
      req(input$plot_start.rectangular)
      req(input$Location.rectangular)
      req(input$l.rectangular)
      req(input$r.rectangular)
      r.rectangular<- as.numeric(input$r.rectangular)
      k.rectangular<- as.numeric(input$k.rectangular)
      plot_start.rectangular<- as.vector(unlist(strsplit(input$plot_start.rectangular, ",")))
      plot_start.rectangular<- as.numeric(plot_start.rectangular)
      loc <- as.vector(unlist(strsplit(input$Location.rectangular, ",")))
      seed.rcbd <- as.numeric(input$myseed.rectangular)
      
      shinyjs::show(id = "downloadCsv.rectangular", anim = FALSE)
      
      if (input$owndata_rectangular == "Yes") {
        t.rectangular <- as.numeric(get_tRECT()$t.rectangular)
        data.rectangular <- getData.rectangular()$dataUp.rectangular
      }else {
        req(input$t.rectangular)
        t.rectangular <- as.numeric(input$t.rectangular)
        data.rectangular <- NULL
      }
      seed.rectangular <- as.numeric(input$myseed.rectangular)
      l.rectangular <- as.numeric(input$l.rectangular)
      
      rectangular_lattice(t = t.rectangular, k = k.rectangular, r = r.rectangular, 
                          l = l.rectangular, 
                          plotNumber = plot_start.rectangular,
                          seed = seed.rectangular, 
                          locationNames = loc, 
                          data = data.rectangular) 
    })
    
    upDateSites_RT <- eventReactive(input$RUN.rectangular, {
      req(input$l.rectangular)
      locs <- as.numeric(input$l.rectangular)
      sites <- 1:locs
      return(list(sites = sites))
    })
    
    output$well_panel_layout_rt <- renderUI({
      req(RECTANGULAR_reactive()$fieldBook)
      df <- RECTANGULAR_reactive()$fieldBook
      locs_rt <- length(levels(as.factor(df$LOCATION)))
      repsRect <- length(levels(as.factor(df$REP)))
      if ((repsRect >= 4 & repsRect %% 2 == 0) | (repsRect >= 4 & sqrt(repsRect) %% 1 == 0)) {
        orderReps <- c("Vertical Stack Panel" = "vertical_stack_panel", "Horizontal Stack Panel" = "horizontal_stack_panel",  
                       "Grid Panel" = "grid_panel")
      } else {
        orderReps <- c("Vertical Stack Panel" = "vertical_stack_panel", "Horizontal Stack Panel" = "horizontal_stack_panel")
      }
      obj_rt <- RECTANGULAR_reactive()
      allBooks_rt <- plot_layout(x = obj_rt, optionLayout = 1, orderReps = "vertical_stack_panel")$newBooks
      nBooks_rt <- length(allBooks_rt)
      layoutOptions_rt <- 1:nBooks_rt
      wellPanel(
        column(3,
               radioButtons(ns("typlotRT"), "Type of Plot:",
                            c("Entries/Treatments" = 1,
                              "Plots" = 2,
                              "Heatmap" = 3))
        ),
        fluidRow(
          column(3,
                 selectInput(inputId = ns("orderRepsRT"), label = "Reps layout:", 
                             choices = orderReps)
          ),
          column(2,
                 selectInput(inputId = ns("layoutO_rt"), label = "Layout option:", choices = layoutOptions_rt, selected = 1)
          ),
          column(2, 
                 selectInput(inputId = ns("locLayout_rt"), label = "Location:", choices = as.numeric(upDateSites_RT()$sites))
          )
        )
      )
    })
    
    observeEvent(input$orderRepsRT, {
      req(input$orderRepsRT)
      req(input$l.rectangular)
      obj_rt <- RECTANGULAR_reactive()
      allBooks <- plot_layout(x = obj_rt, optionLayout = 1, orderReps = input$orderRepsRT)$newBooks
      nBooks <- length(allBooks)
      NewlayoutOptions <- 1:nBooks
      updateSelectInput(session = session, inputId = 'layoutO_rt',
                        label = "Layout option:",
                        choices = NewlayoutOptions,
                        selected = 1
      )
    })
    
    reactive_layoutRect <- reactive({
      req(input$orderRepsRT)
      req(input$layoutO_rt)
      req(input$locLayout_rt)
      req(input$planter_mov_rect)
      req(RECTANGULAR_reactive())
      obj_rt <- RECTANGULAR_reactive()
      opt_rt <- as.numeric(input$layoutO_rt)
      locSelected <- as.numeric(input$locLayout_rt)
      try(plot_layout(x = obj_rt, optionLayout = opt_rt, planter = input$planter_mov_rect, l = locSelected, 
                      orderReps = input$orderRepsRT), silent = TRUE)
    })
    
    
    valsRECT <- reactiveValues(maxV.rectangular= NULL, minV.rectangular= NULL, trail.rectangular= NULL)
    
    simuModal.rectangular<- function(failed = FALSE) {
      modalDialog(
        selectInput(inputId = ns("trailsRECT"), label = "Select One:", choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        conditionalPanel("input.trailsRECT == 'Other'", ns = ns,
                         textInput(inputId = ns("OtherRECT"), label = "Input Trial Name:", value = NULL)
        ),
        fluidRow(
          column(6,
                 numericInput(inputId = ns("min.rectangular"), "Input the min value", value = NULL)
          ),
          column(6,
                 numericInput(inputId = ns("max.rectangular"), "Input the max value", value = NULL)
                 
          )
          
        ),
        
        if (failed)
          div(tags$b("Invalid input of data max and min", style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(inputId = ns("ok.rectangular"), "GO")
        )
        
      )
      
    }
    
    observeEvent(input$Simulate.rectangular, {
      req(input$k.rectangular)
      req(input$r.rectangular)
      req(reactive_layoutRect()$fieldBookXY)
      showModal(
        shinyjqui::jqui_draggable(
          simuModal.rectangular()
        )
      )
    })
    
    observeEvent(input$ok.rectangular, {
      req(input$max.rectangular, input$min.rectangular)
      if (input$max.rectangular> input$min.rectangular&& input$min.rectangular!= input$max.rectangular) {
        valsRECT$maxV.rectangular<- input$max.rectangular
        valsRECT$minV.rectangular<- input$min.rectangular
        if(input$trailsRECT == "Other") {
          req(input$OtherRECT)
          if(!is.null(input$OtherRECT)) {
            valsRECT$trail.rectangular <- as.character(input$OtherRECT)
          }else showModal(simuModal.rectangular(failed = TRUE))
        }else {
          valsRECT$trail.rectangular <- as.character(input$trailsRECT)
        }
        removeModal()
      }else {
        showModal(
          shinyjqui::jqui_draggable(
            simuModal.rectangular(failed = TRUE)
          )
        )
      }
    })
    
    
    simuDataRECT <- reactive({
      set.seed(input$myseed.rectangular)
      req(reactive_layoutRect()$allSitesFieldbook)
      if(!is.null(valsRECT$maxV.rectangular) && !is.null(valsRECT$minV.rectangular) && !is.null(valsRECT$trail.rectangular)) {
        max <- as.numeric(valsRECT$maxV.rectangular)
        min <- as.numeric(valsRECT$minV.rectangular)
        df.rectangular <- reactive_layoutRect()$allSitesFieldbook
        cnamesdf.rectangular<- colnames(df.rectangular)
        df.rectangular<- norm_trunc(a = min, b = max, data = df.rectangular)
        colnames(df.rectangular) <- c(cnamesdf.rectangular[1:(ncol(df.rectangular) - 1)], valsRECT$trail.rectangular)
        a <- ncol(df.rectangular)
      }else {
        df.rectangular <- reactive_layoutRect()$allSitesFieldbook
        a <- ncol(df.rectangular)
      }
      return(list(df = df.rectangular, a = a))
    })
    
    heatmapInfoModal_Rect <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    locNum <- reactive(
      return(as.numeric(input$locLayout_rt))
    )
    
    heatmap_obj <- reactive({
      req(simuDataRECT()$df)
      if (ncol(simuDataRECT()$df) == 11) {
        locs <- factor(simuDataRECT()$df$LOCATION, levels = unique(simuDataRECT()$df$LOCATION))
        locLevels <- levels(locs)
        df = subset(simuDataRECT()$df, LOCATION == locLevels[locNum()])
        loc <- levels(factor(df$LOCATION))
        trail <- as.character(valsRECT$trail.rectangular)
        label_trail <- paste(trail, ": ")
        heatmapTitle <- paste("Heatmap for ", trail)
        new_df <- df %>%
          dplyr::mutate(text = paste0("Site: ", loc, "\n", 
                                      "Row: ", df$ROW, "\n",
                                      "Col: ", df$COLUMN, "\n", 
                                      "Entry: ", df$ENTRY, "\n", 
                                      label_trail, round(df[,11],2)))
        w <- as.character(valsRECT$trail.rectangular)
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
          ggplot2::theme_minimal() + # I added this option 
          ggplot2::theme(plot.title = ggplot2::element_text(family="Calibri", face="bold", size=13, hjust=0.5))
        
        p2 <- plotly::ggplotly(p1, tooltip="text", width = 1250, height = 640)
        return(p2)
      } else {
        showModal(
          shinyjqui::jqui_draggable(
            heatmapInfoModal_Rect()
          )
        )
        return(NULL)
      }
    })
    
    output$random_layout <- plotly::renderPlotly({
      req(reactive_layoutRect())
      req(RECTANGULAR_reactive())
      req(input$typlotRT)
      if (input$typlotRT == 1) {
        reactive_layoutRect()$out_layout
      } else if (input$typlotRT == 2) {
        reactive_layoutRect()$out_layoutPlots
      } else {
        req(heatmap_obj())
        heatmap_obj()
      }
    })
    
    output$rectangular_fieldbook <- DT::renderDataTable({
      req(input$k.rectangular)
      k.rect <- input$k.rectangular
      if (k.rect == "No Options Available") {
        validate("A Rectangular Lattice requires t = s*(s-1), where s is the number of iBlock per replicate.")
      }
      df <- simuDataRECT()$df
      df$LOCATION <- as.factor(df$LOCATION)
      df$PLOT <- as.factor(df$PLOT)
      df$ROW <- as.factor(df$ROW)
      df$COLUMN <- as.factor(df$COLUMN)
      df$REP <- as.factor(df$REP)
      df$IBLOCK <- as.factor(df$IBLOCK)
      df$UNIT <- as.factor(df$UNIT)
      df$ENTRY <- as.factor(df$ENTRY)
      a <- as.numeric(simuDataRECT()$a)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      
      DT::datatable(df,
                    filter = 'top',
                    rownames = FALSE, 
                    options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
      
    })
    
    output$downloadData.rectangular <- downloadHandler(
      filename = function() {
        loc <- paste("Rectangular_Lattice_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(simuDataRECT()$df)
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    csv_data <- reactive({
      req(simuDataRECT()$df)
      df <- simuDataRECT()$df
      export_layout(df, locNum())
    })
    
    
    # Downloadable csv of selected dataset ----
    output$downloadCsv.rectangular <- downloadHandler(
      filename = function() {
        loc <- paste("Rectangular_Lattice_Layout", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(csv_data()$file)
        write.csv(df, file, row.names = FALSE)
      }
    )
    
  })
}