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
                                                 value = NULL, min = 2)
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
                   numericInput(inputId = ns("r.square"), label = "Input # of Full Reps:", value = NULL, min = 2),
                   selectInput(inputId = ns("k.square"), label = "Input # of Plots per IBlock:", choices = ""),
                   numericInput(inputId = ns("l.square"), label = "Input # of Locations:", value = NULL, min = 1),
                   
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
                   # fluidRow(
                   #   column(6, style=list("padding-right: 28px;"),
                   #          numericInput(inputId = ns("myseed.square"), label = "Seed Number:",
                   #                       value = 5, min = 1)),
                   #   column(6, style=list("padding-right: 28px;"),
                   #          actionButton(inputId = ns("RUN.square"), "Run!", icon = icon("cocktail"))
                   #   )
                   #  ),
                    
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
        tabsetPanel(
          tabPanel("Square Lattice Field Book", shinycssloaders::withSpinner(DT::DTOutput(ns("SQUARE.output")), type = 5))
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
    
    getData.square <- reactive({
      req(input$file.square)
      inFile <- input$file.square
      dataUp.square <- load_file(name = inFile$name, path = inFile$datapat, sep = input$sep.square)
      return(list(dataUp.square = dataUp.square))
    })
    
    get_tSQUARE <- reactive({
      if(is.null(input$file.square)) {
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
        h4("Users can use any set of entry numbers."),
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
      req(SQUARE_reactive()$fieldBook)
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
      req(SQUARE_reactive()$fieldBook)
      if(!is.null(valsSQUARE$maxV.square) && !is.null(valsSQUARE$minV.square) && !is.null(valsSQUARE$trail.square)) {
        max <- as.numeric(valsSQUARE$maxV.square)
        min <- as.numeric(valsSQUARE$minV.square)
        df.square <- SQUARE_reactive()$fieldBook
        cnamesdf.square <- colnames(df.square)
        df.square <- norm_trunc(a = min, b = max, data = df.square)
        colnames(df.square) <- c(cnamesdf.square[1:(ncol(df.square) - 1)], valsSQUARE$trail.square)
        a <- ncol(df.square)
      }else {
        df.square <-  SQUARE_reactive()$fieldBook
        a <- ncol(df.square)
      }
      return(list(df = df.square, a = a))
    })
    
    
    output$SQUARE.output <- DT::renderDataTable({
      #input$RUN.square
      req(input$k.square)
      k.square <- input$k.square
      if (k.square == "No Options Available") {
        validate("A Square Lattice requires the number of treatments to be a square number.")
      }
      df <- simuDataSQUARE()$df
      a <- as.numeric(simuDataSQUARE()$a)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      
      DT::datatable(df, rownames = FALSE, options = list(
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
    #Update_k = Update_k
    #return(list(SQUARE.output = SQUARE.output))
    
  })
}
    
## To be copied in the UI
# mod_Square_Lattice_ui("Square_Lattice_ui_1")
    
## To be copied in the server
# mod_Square_Lattice_server("Square_Lattice_ui_1")
