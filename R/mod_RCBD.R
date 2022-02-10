#' RCBD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_RCBD_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Randomized Complete Block Designs"),
    sidebarLayout(
      sidebarPanel(width = 4,
                   radioButtons(ns("owndatarcbd"), label = "Import entries' list?", choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                   conditionalPanel("input.owndatarcbd != 'Yes'", ns = ns,
                                    numericInput(ns("t"), label = "Input # of Treatments:",
                                                 value = 18, min = 2)
                   ),
                   conditionalPanel("input.owndatarcbd == 'Yes'", ns = ns,
                                    fluidRow(
                                      column(8, style=list("padding-right: 28px;"),
                                             fileInput(inputId = ns("file.RCBD"), label = "Upload a CSV File:", multiple = FALSE)),
                                      column(4, style=list("padding-left: 5px;"),
                                             radioButtons(inputId = ns("sep.rcbd"), "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t"),
                                                          selected = ","))
                                    )        
                   ),
                   
                   numericInput(inputId = ns("b"), label = "Input # of Full Reps:", value = 3, min = 2),
                   
                   numericInput(inputId = ns("l.rcbd"), label = "Input # of Locations:", value = 1, min = 1),
                   selectInput(inputId = ns("planter_mov_rcbd"), label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"), multiple = FALSE,
                               selected = "serpentine"),
                   fluidRow(
                     column(6, style=list("padding-right: 28px;"),
                            textInput(inputId = ns("plot_start.rcbd"), "Starting Plot Number(s):", value = 101)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            textInput(inputId = ns("Location.rcbd"), "Input Location:", value = "FARGO")
                     )
                   ),
                   
                   numericInput(inputId = ns("myseed.rcbd"), label = "Seed Number:",
                                value = 123, min = 1),
                   fluidRow(
                     column(6,
                            downloadButton(ns("downloadData.rcbd"), "Save Experiment!", style = "width:100%")
                     ),
                     column(6,
                            actionButton(ns("Simulate.rcbd"), "Simulate!", icon = icon("cocktail"), width = '100%')
                     )
                     
                   )
      ),

      mainPanel(
        width = 8,
        fixedRow(
          column(12, align="center", uiOutput(ns("tabsetRCBD"))),
        ),
        column(12,uiOutput(ns("well_panel_layout_RCBD")))
      )
    )
  )
}
#' RCBD Server Functions
#'
#' @noRd 
mod_RCBD_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns

    getData.rcbd <- reactive({
      req(input$file.RCBD)
      inFile <- input$file.RCBD
      dataUp.rcbd <- load_file(name = inFile$name, path = inFile$datapat, sep = input$sep.rcbd)
      return(list(dataUp.rcbd = dataUp.rcbd))
    })
    
    
    entryListFormat_RCBD <- data.frame(TREATMENT = c(paste("TRT_", LETTERS[1:9], sep = "")))
    entriesInfoModal_RCBD <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        renderTable(entryListFormat_RCBD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        h4("Note that only the TREATMENT column is requared."),
        easyClose = FALSE
      )
    }
    
    toListen <- reactive({
      list(input$owndatarcbd)
    })
    
    observeEvent(toListen(), {
      if (input$owndatarcbd == "Yes") {
        showModal(
          shinyjqui::jqui_draggable(
            entriesInfoModal_RCBD()
          )
        )
      }
    })
    
    RCBD_reactive <- reactive({
      
      req(input$b)
      req(input$myseed.rcbd)
      req(input$plot_start.rcbd)
      req(input$Location.rcbd)
      req(input$l.rcbd)
      req(input$planter_mov_rcbd)
      planter <- as.character(input$planter_mov_rcbd)
      b <- as.numeric(input$b)
      plot_start.rcbd <- as.vector(unlist(strsplit(input$plot_start.rcbd, ",")))
      plot_start.rcbd <- as.numeric(plot_start.rcbd)
      loc <-  as.vector(unlist(strsplit(input$Location.rcbd, ",")))
      seed.rcbd <- as.numeric(input$myseed.rcbd)

      if (input$owndatarcbd == "Yes") {
        t <- NULL 
        data.rcbd <- getData.rcbd()$dataUp.rcbd
      }else {
        req(input$t)
        t <- as.numeric(input$t)
        data.rcbd <- NULL
      }
      
      l.rcbd <- as.numeric(input$l.rcbd)
      
      myRCBD <- RCBD(t = t, reps = b, l = l.rcbd, plotNumber = plot_start.rcbd, continuous = FALSE,
                     planter = planter, seed = seed.rcbd, locationNames = loc, 
                     data = data.rcbd)
      
    })
    
    output$well_panel_layout_RCBD <- renderUI({
      req(RCBD_reactive()$fieldBook)
      req(input$l.rcbd)
      obj_rcbd <- RCBD_reactive()
      planting_rcbd <- input$planter_mov_rcbd
      allBooks_rcbd <- plot_layout(x = obj_rcbd, optionLayout = 1, orderReps = "vertical_stack_panel")$newBooks
      nBooks_rcbd <- length(allBooks_rcbd)
      layoutOptions_rcbd <- 1:nBooks_rcbd
      orderReps_rcbd <- c("Vertical Stack Panel" = "vertical_stack_panel", "Horizontal Stack Panel" = "horizontal_stack_panel")
      #loc <-  as.vector(unlist(strsplit(input$Location.rcbd, ",")))
      sites <- as.numeric(input$l.rcbd)
      wellPanel(
        column(3,
               radioButtons(ns("typlotRCBD"), "Type of Plot:",
                            c("Entries/Treatments" = 1,
                              "Plots" = 2,
                              "Heatmap" = 3))
        ),
        fluidRow(
          column(3,
                 selectInput(inputId = ns("orderRepsRCBD"), label = "Reps layout:", 
                             choices = orderReps_rcbd),
          ),
          column(2, #align="center",
                 selectInput(inputId = ns("layoutO_rcbd"), label = "Layout option:", choices = layoutOptions_rcbd, selected = 1)
          ),
          column(2, #align="center",
                 selectInput(inputId = ns("locLayout_rcbd"), label = "Location:", choices = 1:sites)
          )
        )
      )
    })
    
    observeEvent(input$orderRepsRCBD, {
      req(input$orderRepsRCBD)
      req(input$l.rcbd)
      obj_rcbd <- RCBD_reactive()
      allBooks <- try(plot_layout(x = obj_rcbd, optionLayout = 1, orderReps = input$orderRepsRCBD)$newBooks, silent = TRUE)
      nBooks <- length(allBooks)
      NewlayoutOptions <- 1:nBooks
      updateSelectInput(session = session, inputId = 'layoutO_rcbd',
                        label = "Layout option:",
                        choices = NewlayoutOptions,
                        selected = 1
      )
    })
    
    reactive_layoutRCBD <- reactive({
      req(input$orderRepsRCBD)
      req(input$layoutO_rcbd)
      req(input$planter_mov_rcbd)
      req(input$locLayout_rcbd)
      req(RCBD_reactive())
      obj_rcbd <- RCBD_reactive()
      opt_rcbd <- as.numeric(input$layoutO_rcbd)
      planting_rcbd <- input$planter_mov_rcbd
      locSelected <- as.numeric(input$locLayout_rcbd)
      try(plot_layout(x = obj_rcbd, optionLayout = opt_rcbd, orderReps = input$orderRepsRCBD,
                      planter = planting_rcbd, l = locSelected), silent = TRUE)
    })

    
    valsRCBD <- reactiveValues(maxV.rcbd = NULL, minV.rcbd = NULL, trail.rcbd = NULL)
    
    simuModal.rcbd <- function(failed = FALSE) {
      modalDialog(
        selectInput(inputId = ns("trailsRCBD"), label = "Select One:", choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        conditionalPanel("input.trailsRCBD == 'Other'", ns = ns,
                         textInput(inputId = ns("OtherRCBD"), label = "Input Trial Name:", value = NULL)
        ),
        fluidRow(
          column(6, 
                 numericInput(inputId = ns("min.rcbd"), "Input the min value", value = NULL)
          ),
          column(6, 
                 numericInput(inputId = ns("max.rcbd"), "Input the max value", value = NULL)
                 
          )
          
        ),
        
        if (failed)
          div(tags$b("Invalid input of data max and min", style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(inputId = ns("ok.rcbd"), "GO")
        )
        
      )
      
    }
    
    observeEvent(input$Simulate.rcbd, {
      req(RCBD_reactive()$fieldBook)
      showModal(
        shinyjqui::jqui_draggable(
          simuModal.rcbd()
        )
      )
    })
    
    observeEvent(input$ok.rcbd, {
      req(input$max.rcbd, input$min.rcbd)
      if (input$max.rcbd > input$min.rcbd && input$min.rcbd != input$max.rcbd) {
        valsRCBD$maxV.rcbd <- input$max.rcbd
        valsRCBD$minV.rcbd <- input$min.rcbd
        if(input$trailsRCBD == "Other") {
          req(input$OtherRCBD)
          if(!is.null(input$OtherRCBD)) {
            valsRCBD$trail.rcbd <- as.character(input$OtherRCBD)
          }else showModal(simuModal.rcbd(failed = TRUE))
        }else {
          valsRCBD$trail.rcbd <- as.character(input$trailsRCBD)
        }
        removeModal()
      }else {
        showModal(
          shinyjqui::jqui_draggable(
            simuModal.rcbd(failed = TRUE)
          )
        )
      }
    })
    
    
    simuDataRCBD <- reactive({
      req(RCBD_reactive()$fieldBook)
      if(!is.null(valsRCBD$maxV.rcbd) && !is.null(valsRCBD$minV.rcbd) && !is.null(valsRCBD$trail.rcbd)) {
        max <- as.numeric(valsRCBD$maxV.rcbd)
        min <- as.numeric(valsRCBD$minV.rcbd)
        df.rcbd <- RCBD_reactive()$fieldBook
        df.rcbd <- reactive_layoutRCBD()$allSitesFielbook
        cnamesdf.rcbd <- colnames(df.rcbd)
        df.rcbd <- norm_trunc(a = min, b = max, data = df.rcbd)
        colnames(df.rcbd) <- c(cnamesdf.rcbd[1:(ncol(df.rcbd) - 1)], valsRCBD$trail.rcbd)
        df.rcbd <- df.rcbd[order(df.rcbd$ID),]
      }else {
        #df.rcbd <-  RCBD_reactive()$fieldBook
        df.rcbd <- reactive_layoutRCBD()$allSitesFielbook
      }
      return(list(df = df.rcbd))
    })
    
    heatmapInfoModal_RCBD <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    output$tabsetRCBD <- renderUI({
      req(input$typlotRCBD)
      tabsetPanel(
        if (input$typlotRCBD != 3) {
          tabPanel("Randomized Complete Block Layout", shinycssloaders::withSpinner(plotOutput(ns("layout.output_RCBD"), width = "100%", height = "650px"),
                                                                                    type = 5))
        } else {
          tabPanel("Randomized Complete Block Layout", shinycssloaders::withSpinner(plotly::plotlyOutput(ns("heatmapRCBD"), width = "100%", height = "650px"),
                                                                                    type = 5))
        },
        tabPanel("Randomized Complete Block Book", shinycssloaders::withSpinner(DT::DTOutput(ns("RCBD.output")), type = 5))
      )
      
    })
    
    locNum <- reactive(
      return(as.numeric(input$locLayout_rcbd))
    )
    
    heatmap_obj <- reactive({
      req(simuDataRCBD()$df)
      if (ncol(simuDataRCBD()$df) == 8) {
        locs <- factor(simuDataRCBD()$df$LOCATION, levels = unique(simuDataRCBD()$df$LOCATION))
        locLevels <- levels(locs)
        df = subset(simuDataRCBD()$df, LOCATION == locLevels[locNum()])
        loc <- levels(factor(df$LOCATION))
        trail <- as.character(valsRCBD$trail.rcbd)
        label_trail <- paste(trail, ": ")
        heatmapTitle <- paste("Heatmap for ", trail)
        new_df <- df %>%
          dplyr::mutate(text = paste0("Site: ", loc, "\n", "Row: ", df$ROW, "\n", "Col: ", df$COLUMN, "\n", "Entry: ", 
                                      df$ENTRY, "\n", label_trail, round(df[,8],2)))
        w <- as.character(valsRCBD$trail.rcbd)
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
        
        p2 <- plotly::ggplotly(p1, tooltip="text", width = 1150, height = 640)
        return(p2)
      } else {
        showModal(
          shinyjqui::jqui_draggable(
            heatmapInfoModal_RCBD()
          )
        )
        return(NULL)
      }
    })
    
    output$heatmapRCBD <- plotly::renderPlotly({
      req(heatmap_obj())
      heatmap_obj()
    })
    
    output$layout.output_RCBD <- renderPlot({
      req(reactive_layoutRCBD())
      req(RCBD_reactive())
      req(input$typlotRCBD)
      if (input$typlotRCBD == 1) {
        reactive_layoutRCBD()$out_layout
      } else if (input$typlotRCBD == 2) {
        reactive_layoutRCBD()$out_layoutPlots
      } 
    })
    
    output$RCBD.output <- DT::renderDataTable({
      df <- simuDataRCBD()$df
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      
      DT::datatable(df, rownames = FALSE, options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
      
    })

    output$downloadData.rcbd <- downloadHandler(
      filename = function() {
        loc <- paste("RCBD_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(simuDataRCBD()$df)
        write.csv(df, file, row.names = FALSE)
      }
    )
 
  })
}
    
## To be copied in the UI
# mod_RCBD_ui("RCBD_ui_1")
    
## To be copied in the server
# mod_RCBD_server("RCBD_ui_1")
