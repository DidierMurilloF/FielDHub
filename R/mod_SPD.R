#' SPD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_SPD_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("Split-Plot Design"),
    sidebarLayout(
      sidebarPanel(width = 4,
                   radioButtons(inputId = ns("owndataSPD"), label = "Import entries' list?", choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                   selectInput(inputId = ns("kindSPD"), label = "Select SPD Type:",
                               choices = c("Split-Plot in a CRD" = "SPD_CRD", "Split-Plot in a RCBD" = "SPD_RCBD"),
                               multiple = FALSE),
                   
                   conditionalPanel("input.owndataSPD == 'Yes'", ns = ns,
                                    fluidRow(
                                      column(8, style=list("padding-right: 28px;"),
                                             fileInput(ns("file.SPD"), label = "Upload a csv File:", multiple = FALSE)),
                                      column(4,style=list("padding-left: 5px;"),
                                             radioButtons(ns("sep.spd"), "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t"),
                                                          selected = ","))
                                    )
                   ),
                   conditionalPanel("input.owndataSPD != 'Yes'", ns = ns,
                                     numericInput(ns("mp.spd"), label = "Whole-plots:",
                                                  value = NULL, min = 2),
                                     numericInput(ns("sp.spd"), label = "Sub-plots Within Whole-plots:",
                                                  value = NULL, min = 2)
                   ),
                   
                   fluidRow(
                     column(6, style=list("padding-right: 28px;"),
                            numericInput(ns("reps.spd"), label = "Input # of Full Reps:",
                                         value = 2, min = 2), 
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            numericInput(ns("l.spd"), label = "Input # of Locations:",
                                         value = 1, min = 1)
                     )
                   ),
                   fluidRow(
                     column(6, style=list("padding-right: 28px;"),
                            textInput(ns("plot_start.spd"), "Starting Plot Number:", value = 101)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            textInput(ns("Location.spd"), "Input the Location:", value = "FARGO")
                     )
                   ),
                   
                   numericInput(inputId = ns("myseed.spd"), label = "Seed Number:", value = 123, min = 1),
                   
                   fluidRow(
                     column(6,
                            downloadButton(ns("downloadData.spd"), "Save Experiment!", style = "width:100%")
                     ),
                     column(6,
                            actionButton(ns("Simulate.spd"), "Simulate!", icon = icon("cocktail"), width = '100%')
                     )
                   )
      ),
      
      mainPanel(
        width = 8,
        tabsetPanel(
          tabPanel("Field Book", DT::DTOutput(ns("SPD.output")))
        )   
      )
    )    
  )
}
    
#' SPD Server Functions
#'
#' @noRd 
mod_SPD_server <- function(id){
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    getData.spd <- reactive({
      req(input$file.SPD)
      inFile <- input$file.SPD
      dataUp.spd <- load_file(name = inFile$name, path = inFile$datapat, sep = input$sep.spd)
      return(list(dataUp.spd = dataUp.spd))
    })
    
    spd_reactive <- reactive({
      req(input$plot_start.spd)
      req(input$Location.spd)
      req(input$myseed.spd)
      req(input$l.spd)
      
      l.spd <- as.numeric(input$l.spd)
      seed.spd <- as.numeric(input$myseed.spd)
      plot_start.spd <- as.vector(unlist(strsplit(input$plot_start.spd, ",")))
      plot_start.spd <- as.numeric(plot_start.spd)
      loc.spd <-  as.vector(unlist(strsplit(input$Location.spd, ",")))
      req(input$reps.spd)
      reps.spd <- as.numeric(input$reps.spd)
      
      if (input$kindSPD == "SPD_RCBD") {
        if (input$owndataSPD == "Yes") {
          wp <- NULL
          sp <- NULL
          data.spd <- getData.spd()$dataUp.spd
        }else {
          req(input$mp.spd, input$sp.spd)
          wp <- as.numeric(input$mp.spd)
          sp <- as.numeric(input$sp.spd)
          data.spd <- NULL
        }
        type <- 2
        
      }else {
        if (input$owndataSPD == "Yes") {
          wp <- NULL
          sp <- NULL
          data.spd <- getData.spd()$dataUp.spd
        }else {
          
          req(input$mp.spd, input$sp.spd)
          wp <- as.numeric(input$mp.spd)
          sp <- as.numeric(input$sp.spd)
          data.spd <- NULL
        }
        type <- 1
      }
      
      SPD <- split_plot(wp = wp, sp = sp, reps = reps.spd, l = l.spd, plotNumber = plot_start.spd, seed = seed.spd,
                        type = type, locationNames = loc.spd, data = data.spd)
      
      
    })
    
    
    valspd <- reactiveValues(maxV.spd = NULL, minV.spd = NULL, trail.spd = NULL)
    
    simuModal.spd <- function(failed = FALSE) {
      modalDialog(
        selectInput(inputId = ns("trailsspd"), label = "Select One:", choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        conditionalPanel("input.trailsspd == 'Other'", ns = ns,
                         textInput(inputId = ns("Otherspd"), label = "Input Trial Name:", value = NULL)
        ),
        fluidRow(
          column(6, 
                 numericInput(ns("min.spd"), "Input the min value", value = NULL)
          ),
          column(6, 
                 numericInput(ns("max.spd"), "Input the max value", value = NULL)
                 
          )
          
        ),
        
        if (failed)
          div(tags$b("Invalid input of data max and min", style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ok.spd"), "GO")
        )
        
      )
      
    }
    
    observeEvent(input$Simulate.spd, {
      req(spd_reactive()$fieldBook)
      showModal(
        shinyjqui::jqui_draggable(
          simuModal.spd()
        )
      )
    })
    
    observeEvent(input$ok.spd, {
      req(input$max.spd, input$min.spd)
      if (input$max.spd > input$min.spd && input$min.spd != input$max.spd) {
        valspd$maxV.spd <- input$max.spd
        valspd$minV.spd <- input$min.spd
        if(input$trailsspd == "Other") {
          req(input$Otherspd)
          if(!is.null(input$Otherspd)) {
            valspd$trail.spd <- input$Otherspd
          }else showModal(simuModal.spd(failed = TRUE))
        }else {
          valspd$trail.spd <- as.character(input$trailsspd)
        }
        removeModal()
      }else {
        showModal(
          shinyjqui::jqui_draggable(
            simuModal.spd(failed = TRUE)
          )
        )
      }
    })
    
    simuData_spd <- reactive({
      req(spd_reactive()$fieldBook)
      
      if(!is.null(valspd$maxV.spd) && !is.null(valspd$minV.spd) && !is.null(valspd$trail.spd)) {
        max <- as.numeric(valspd$maxV.spd)
        min <- as.numeric(valspd$minV.spd)
        df.spd <- spd_reactive()$fieldBook
        cnamesdf.spd <- colnames(df.spd)
        df.spd <- norm_trunc(a = min, b = max, data = df.spd)
        colnames(df.spd) <- c(cnamesdf.spd[1:(ncol(df.spd) - 1)], valspd$trail.spd)
        df.spd <- df.spd[order(df.spd$ID),]
      }else {
        df.spd <- spd_reactive()$fieldBook  
      }
      return(list(df = df.spd))
    })
    
    
    output$SPD.output <- DT::renderDataTable({
      
      df <- simuData_spd()$df
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      
      DT::datatable(df, rownames = FALSE, options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
      
    })
    
    output$downloadData.spd <- downloadHandler(
      filename = function() {
        loc <- paste("Split-Plot_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(simuData_spd()$df)
        write.csv(df, file, row.names = FALSE)
      }
    )
  })
}
    
## To be copied in the UI
# mod_SPD_ui("SPD_ui_1")
    
## To be copied in the server
# mod_SPD_server("SPD_ui_1")
