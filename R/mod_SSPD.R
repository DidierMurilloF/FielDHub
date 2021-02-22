#' SSPD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_SSPD_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      
      sidebarPanel(width = 4,
                   radioButtons(inputId = ns("owndataSSPD"), label = "Do you have your own data?", choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                   
                   selectInput(inputId = ns("kindSSPD"), label = "Select SSPD Type:",
                               choices = c("Split-Split Plot in a RCBD" = "SSPD_RCBD", "Split-Split Plot in a CRD" = "SSPD_CRD"),
                               multiple = FALSE),
                   
                   conditionalPanel("input.owndataSSPD == 'Yes'", ns = ns,
                                    fluidRow(
                                      column(8, style=list("padding-right: 28px;"),
                                             fileInput(ns("file.SSPD"), label = "Upload a csv File:", multiple = FALSE)),
                                      
                                      column(4,style=list("padding-left: 5px;"),
                                             radioButtons(ns("sep.sspd"), "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t"),
                                                          selected = ","))
                                    )          
                   ),
                   
                   conditionalPanel("input.owndataSPD != 'Yes'", ns = ns,
                                    numericInput(ns("mp.sspd"), label = "Whole-plots:",
                                                 value = NULL, min = 2),
                                    numericInput(ns("sp.sspd"), label = "Sub-plots Within Whole-plots:",
                                                 value = NULL, min = 2),
                                    numericInput(ns("ssp.sspd"), label = "Sub-Sub-plots within Sub-plots:",
                                                 value = NULL, min = 2) 
                   ),

                   fluidRow(
                     column(6, style=list("padding-right: 28px;"),
                       numericInput(ns("reps.sspd"), label = "Input # of Full Reps:",
                                    value = 2, min = 2)
                     ),
                     column(6, style=list("padding-left: 5px;"),
                       numericInput(ns("l.sspd"), label = "Input # of Locations:",
                                    value = 1, min = 1)
                     )
                   ), 
                   
                   fluidRow(
                     column(6,style=list("padding-right: 28px;"),
                            textInput(ns("plot_start.sspd"), "Starting Plot Number:", value = 101)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            textInput(ns("Location.sspd"), "Input Location:", value = "FARGO")
                     )
                   ),
                   
                   numericInput(inputId = ns("myseed.sspd"), label = "Seed Number:", value = 123, min = 1),
                   
                   fluidRow(
                     column(6,
                            downloadButton(ns("downloadData.sspd"), "Save Experiment!", style = "width:100%")
                     ),
                     column(6,
                            actionButton(ns("Simulate.sspd"), "Simulate!", icon = icon("cocktail"), width = '100%')
                     )
                   )
      ),
      
      mainPanel(
        width = 8,
        tabsetPanel(
          tabPanel("Field Book", DT::DTOutput(ns("SSPD.output")))
        )     
      )
    )
  )
}
    
#' SSPD Server Functions
#'
#' @noRd 
mod_SSPD_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    getData.sspd <- reactive({
      req(input$file.SSPD)
      inFile <- input$file.SSPD
      dataUp.sspd <- load_file(name = inFile$name, path = inFile$datapat, sep = input$sep.sspd)
      return(list(dataUp.sspd = dataUp.sspd))
    })
    
    sspd_reactive <- reactive({
      
      req(input$plot_start.sspd)
      req(input$Location.sspd)
      req(input$myseed.sspd)
      req(input$l.sspd)
      
      l.sspd <- as.numeric(input$l.sspd)
      seed.sspd <- as.numeric(input$myseed.sspd)
      plot_start.sspd <- as.vector(unlist(strsplit(input$plot_start.sspd, ",")))
      plot_start.sspd <- as.numeric(plot_start.sspd)
      loc.sspd <-  as.vector(unlist(strsplit(input$Location.sspd, ",")))
      req(input$reps.sspd)
      reps.sspd <- as.numeric(input$reps.sspd)
      
      if (input$kindSSPD == "SSPD_RCBD") {
        
        if (input$owndataSSPD == "Yes") {
          wp.sspd <- NULL
          sp.sspd <- NULL
          ssp.sspd <- NULL
          data.sspd <- getData.sspd()$dataUp.sspd
        }else {
          req(input$mp.sspd, input$sp.sspd)
          req(input$ssp.sspd)
          wp.sspd <- as.numeric(input$mp.sspd)
          sp.sspd <- as.numeric(input$sp.sspd)
          ssp.sspd <- as.numeric(input$ssp.sspd)
          data.sspd <- NULL
        }
        type <- 2
      }else {
        if (input$owndataSSPD == "Yes") {
          wp.sspd <- NULL
          sp.sspd <- NULL
          ssp.sspd <- NULL
          data.sspd <- getData.sspd()$dataUp.sspdd
        }else {
          req(input$mp.sspd, input$sp.sspd)
          req(input$ssp.sspd)
          wp.sspd <- as.numeric(input$mp.sspd)
          sp.sspd <- as.numeric(input$sp.sspd)
          ssp.sspd <- as.numeric(input$ssp.sspd)
          data.sspd <- NULL
        }
        type <- 1
      }
      
      SSPD <- split_split_plot(wp = wp.sspd, sp = sp.sspd, ssp = ssp.sspd, reps = reps.sspd, l = l.sspd, 
                               plotNumber = plot_start.sspd, seed = seed.sspd, type = type, 
                               locationNames = loc.sspd, data = data.sspd)
      
      
    })
    
    
    valsspd <- reactiveValues(maxV.sspd = NULL, minV.sspd = NULL, trail.sspd = NULL)
    
    simuModal.sspd <- function(failed = FALSE) {
      modalDialog(
        selectInput(inputId = ns("trailsRowCol"), label = "Select One:", choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        conditionalPanel("input.trailsRowCol == 'Other'", ns = ns,
                         textInput(inputId = ns("Otherspd"), label = "Input Trail Name:", value = NULL)
        ),
        fluidRow(
          column(6, 
                 numericInput(ns("min.sspd"), "Input the min value", value = NULL)
          ),
          column(6, 
                 numericInput(ns("max.sspd"), "Input the max value", value = NULL)
                 
          )
          
        ),
        
        if (failed)
          div(tags$b("Invalid input of data max and min", style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ok.sspd"), "GO")
        )
        
      )
      
    }
    
    observeEvent(input$Simulate.sspd, {
      req(sspd_reactive()$fieldBook)
      showModal(
        shinyjqui::jqui_draggable(
          simuModal.sspd()
        )
      )
    })
    
    observeEvent(input$ok.sspd, {
      req(input$max.sspd, input$min.sspd)
      if (input$max.sspd > input$min.sspd && input$min.sspd != input$max.sspd) {
        valsspd$maxV.sspd <- input$max.sspd
        valsspd$minV.sspd <- input$min.sspd
        if(input$trailsRowCol == "Other") {
          req(input$Otherspd)
          if(!is.null(input$Otherspd)) {
            valsspd$trail.sspd <- input$Otherspd
          }else showModal(simuModal.sspd(failed = TRUE))
        }else {
          valsspd$trail.sspd <- as.character(input$trailsRowCol)
        }
        removeModal()
      }else {
        showModal(
          shinyjqui::jqui_draggable(
            simuModal.sspd(failed = TRUE)
          )
        )
      }
    })
    
    simuData_sspd <- reactive({
      req(sspd_reactive()$fieldBook)
      
      if(!is.null(valsspd$maxV.sspd) && !is.null(valsspd$minV.sspd) && !is.null(valsspd$trail.sspd)) {
        max <- as.numeric(valsspd$maxV.sspd)
        min <- as.numeric(valsspd$minV.sspd)
        df.sspd <- sspd_reactive()$fieldBook
        cnamesdf.sspd <- colnames(df.sspd)
        df.sspd <- norm_trunc(a = min, b = max, data = df.sspd)
        colnames(df.sspd) <- c(cnamesdf.sspd[1:(ncol(df.sspd) - 1)], valsspd$trail.sspd)
        a <- ncol(df.sspd)
      }else {
        df.sspd <- sspd_reactive()$fieldBook  
        a <- ncol(df.sspd)
      }
      return(list(df = df.sspd, a = a))
    })
    
    
    output$SSPD.output  <- DT::renderDataTable({
      
      df <- simuData_sspd()$df
      a <- as.numeric(simuData_sspd()$a)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      
      DT::datatable(df, rownames = FALSE, options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
      
    })
    
    output$downloadData.sspd <- downloadHandler(
      filename = function() {
        loc <- paste("Split-Split-Plot_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(simuData_sspd()$df)
        write.csv(df, file, row.names = FALSE)
      }
    )
    #return(list(SSPD.output = SSPD.output))
  })
}
    
## To be copied in the UI
# mod_SSPD_ui("SSPD_ui_1")
    
## To be copied in the server
# mod_SSPD_server("SSPD_ui_1")
