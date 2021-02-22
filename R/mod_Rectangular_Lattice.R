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
                                                 value = NULL, min = 2)
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
                   
                   numericInput(inputId = ns("r.rectangular"), label = "Input # of Full Reps:", value = NULL, min = 2),
                   selectInput(inputId = ns("k.rectangular"), label = "Input # of Plots per IBlock:", choices = ""),
                   numericInput(inputId = ns("l.rectangular"), label = "Input # of Locations:", value = NULL, min = 1),
                   # 
                   # fluidRow(
                   #   column(6,style=list("padding-right: 28px;"),
                   #          numericInput(inputId = ns("r.rectangular"), label = "Input # of Blocks:", value = NULL, min = 2)
                   #   ),
                   #   column(6, style=list("padding-left: 5px;"),
                   #          numericInput(inputId = ns("l.rectangular"), label = "Input # of Locations:", value = 1, min = 1)
                   #   )
                   # ),
                   
                   
                   # selectInput(inputId = ns("planter_mov__rectangular"), label = "Plot Order Layout:",
                   #             choices = c("serpentine", "cartesian"), multiple = FALSE,
                   #             selected = "serpentine"),
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
        tabsetPanel(
          tabPanel("Rectangular Lattice Field Book", shinycssloaders::withSpinner(DT::DTOutput(ns("RECTANGULAR.output")), type = 5))
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
    
    getData.rectangular <- reactive({
      req(input$file.rectangular)
      inFile <- input$file.rectangular
      dataUp.rectangular<- load_file(name = inFile$name, path = inFile$datapat, sep = input$sep.rectangular)
      return(list(dataUp.rectangular= dataUp.rectangular))
    })
    
    observeEvent(input$t.rectangular, {
      req(input$t.rectangular)
      t <- input$t.rectangular
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
      
      if (input$owndata_rectangular == "Yes") {
        t.rectangular <- NULL 
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
    
    
    valsRECT <- reactiveValues(maxV.rectangular= NULL, minV.rectangular= NULL, trail.rectangular= NULL)
    
    simuModal.rectangular<- function(failed = FALSE) {
      modalDialog(
        selectInput(inputId = ns("trailsRECT"), label = "Select One:", choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        conditionalPanel("input.trailsRECT == 'Other'", ns = ns,
                         textInput(inputId = ns("OtherRECT"), label = "Input Trail Name:", value = NULL)
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
      req(RECTANGULAR_reactive()$fieldBook)
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
            valsRECT$trail.rectangular<- as.character(input$OtherRECT)
          }else showModal(simuModal.rectangular(failed = TRUE))
        }else {
          valsRECT$trail.rectangular<- as.character(input$trailsRECT)
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
      req(RECTANGULAR_reactive()$fieldBook)
      if(!is.null(valsRECT$maxV.rectangular) && !is.null(valsRECT$minV.rectangular) && !is.null(valsRECT$trail.rectangular)) {
        max <- as.numeric(valsRECT$maxV.rectangular)
        min <- as.numeric(valsRECT$minV.rectangular)
        df.rectangular<- RECTANGULAR_reactive()$fieldBook
        cnamesdf.rectangular<- colnames(df.rectangular)
        df.rectangular<- norm_trunc(a = min, b = max, data = df.rectangular)
        colnames(df.rectangular) <- c(cnamesdf.rectangular[1:(ncol(df.rectangular) - 1)], valsRECT$trail.rectangular)
        a <- ncol(df.rectangular)
      }else {
        df.rectangular<-  RECTANGULAR_reactive()$fieldBook
        a <- ncol(df.rectangular)
      }
      return(list(df = df.rectangular, a = a))
    })
    
    
    output$RECTANGULAR.output <- DT::renderDataTable({
      req(input$k.rectangular)
      k.rect <- input$k.rectangular
      if (k.rect == "No Options Available") {
        validate("A Rectangular Lattice requires t = s*(s-1), where s is the number of iBlock per replicate.")
      }
      df <- simuDataRECT()$df
      a <- as.numeric(simuDataRECT()$a)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      
      DT::datatable(df, rownames = FALSE, options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
      
    })
    
    
    # Downloadable csv of selected dataset ----
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
    #Update_k = Update_k
    #return(list(RECTANGULAR.output = RECTANGULAR.output))
    
  })
}

    
## To be copied in the UI
# mod_Rectangular_Lattice_ui("Rectangular_Lattice_ui_1")
    
## To be copied in the server
# mod_Rectangular_Lattice_server("Rectangular_Lattice_ui_1")
