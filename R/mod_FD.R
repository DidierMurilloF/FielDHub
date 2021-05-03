#' FD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_FD_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("Full Factorial Designs"),
    sidebarLayout(
      sidebarPanel(width = 4,
                   radioButtons(inputId = ns("owndata"), label = "Import entries' list?", choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                   selectInput(inputId = ns("kindFD"), label = "Select a Factorial Design Type:",
                               choices = c("Factorial in a CRD" = "FD_CRD", "Factorial in a RCBD" = "FD_RCBD"),
                               multiple = FALSE),
                   
                   conditionalPanel("input.owndata != 'Yes'", ns = ns,
                                    textInput(inputId = ns("setfactors"), label = "Input # of Entries for Each Factor: (Separated by Comma)",
                                              value = NULL)     
                   ),
                   conditionalPanel("input.owndata == 'Yes'", ns = ns,
                                    fluidRow(
                                      column(8, style=list("padding-right: 28px;"),
                                             fileInput(ns("file.FD"), label = "Upload a CSV File:", multiple = FALSE)),
                                      column(4,style=list("padding-left: 5px;"),
                                             radioButtons(ns("sep.fd"), "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t"),
                                                          selected = ","))
                                    )
                   ),
               
                   fluidRow(
                     column(6, style=list("padding-right: 28px;"),
                       numericInput(inputId = ns("reps.fd"), label = "Input # of Full Reps:",
                                    value = 2, min = 2)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                       numericInput(ns("l.fd"), label = "Input # of Locations:",
                                    value = 1, min = 1)
                     )
                   ),
                   fluidRow(
                     column(6, style=list("padding-right: 28px;"),
                            textInput(ns("plot_start.fd"), "Starting Plot Number:", value = 101)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            textInput(ns("Location.fd"), "Input Location:", value = "FARGO")
                     )
                   ),
                   numericInput(inputId = ns("myseed.reps"), label = "Seed Number:",
                                value = 123, min = 1),
                   fluidRow(
                     column(6,
                            downloadButton(ns("downloadData.fd"), "Save Experiment!", style = "width:100%")
                     ),
                     column(6,
                            actionButton(ns("Simulate.fd"), "Simulate!", icon = icon("cocktail"), width = '100%')
                     )
                     
                   )
      ),
      
      mainPanel(width = 8,
        tabsetPanel(
          tabPanel("Field Book", DT::DTOutput(ns("FD.Output")))
        )
      )
    ) 
  )
}
    
#' FD Server Functions
#'
#' @noRd 
mod_FD_server <- function(id) {
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    FACTORS <- rep(c("A", "B", "C"), c(2,3,2))
    LEVELS <- c("a0", "a1", "b0", "b1", "b2", "c0", "c1")
    entryListFormat_FD <- data.frame(list(FACTOR = FACTORS, LEVEL = LEVELS))
    
    entriesInfoModal_FD <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        renderTable(entryListFormat_FD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        #h4("Note that reps might be unbalanced."),
        easyClose = FALSE
      )
    }
    
    toListen <- reactive({
      list(input$owndata)
    })
    
    observeEvent(toListen(), {
      if (input$owndata == "Yes") {
        showModal(
          shinyjqui::jqui_draggable(
            entriesInfoModal_FD()
          )
        )
      }
    })
    
    getData.fd <- reactive({
      req(input$file.FD)
      req(input$sep.fd)
      inFile <- input$file.FD
      dataUp.fd <- load_file(name = inFile$name, path = inFile$datapat, sep = input$sep.fd)
      return(list(dataUp.fd = dataUp.fd))
    })
    
    fd_reactive <- reactive({
      
      req(input$plot_start.fd)
      req(input$Location.fd)
      req(input$l.fd)
      req(input$myseed.reps)
      l.fd <- as.numeric(input$l.fd)
      plot_start.fd <- as.vector(unlist(strsplit(input$plot_start.fd, ",")))
      plot_start.fd <- as.numeric(plot_start.fd)
      loc <-  as.vector(unlist(strsplit(input$Location.fd, ",")))
      seed.fd <- as.numeric(input$myseed.reps)
      if (input$kindFD == "FD_RCBD") {
        if (input$owndata == "Yes") {
          setfactors.fd <- NULL
          data.fd <- getData.fd()$dataUp.fd
        }else {
          req(input$setfactors)
          setfactors.fd <- as.numeric(as.vector(unlist(strsplit(input$setfactors, ","))))
          if (length(setfactors.fd) < 2) validate("We need more than one factor.")
          data.fd <- NULL
        }
        type <- 2
        req(input$reps.fd)
        reps.fd <- as.numeric(input$reps.fd)
        
      }else {
        if (input$owndata == "Yes") {
          setfactors.fd <- NULL
          data.fd <- getData.fd()$dataUp.fd
        }else {
          req(input$setfactors)
          setfactors.fd <- as.numeric(as.vector(unlist(strsplit(input$setfactors, ","))))
          if (length(setfactors.fd) < 2) validate("We need more than one factor.")
          data.fd <- NULL
        }
        type <- 1
        req(input$reps.fd)
        reps.fd <- as.numeric(input$reps.fd)
        
      }
      
      myfd <- full_factorial(setfactors = setfactors.fd, reps = reps.fd, l = l.fd, type = type,
                             plotNumber = plot_start.fd, seed = seed.fd, locationNames = loc,
                             data = data.fd) 
      
    })
    
    valsfd <- reactiveValues(maxV.fd = NULL, minV.fd = NULL, trail.fd = NULL)
    
    simuModal.fd <- function(failed = FALSE) {
      modalDialog(
        selectInput(inputId = ns("trailsfd"), label = "Select One:", choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        conditionalPanel("input.trailsfd == 'Other'", ns = ns,
                         textInput(inputId = ns("Otherfd"), label = "Input Trail Name:", value = NULL)
        ),
        fluidRow(
          column(6, 
                 numericInput(inputId = ns("min.fd"), "Input the min value", value = NULL)
          ),
          column(6, 
                 numericInput(inputId = ns("max.fd"), "Input the max value", value = NULL)
                 
          )
          
        ),
        
        if (failed)
          div(tags$b("Invalid input of data max and min", style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ok.fd"), "GO")
        )
        
      )
      
    }
    
    observeEvent(input$Simulate.fd, {
      req(fd_reactive()$fieldBook)
      showModal(
        shinyjqui::jqui_draggable(
          simuModal.fd()
        )
      )
    })
    
    observeEvent(input$ok.fd, {
      req(input$max.fd, input$min.fd)
      if (input$max.fd > input$min.fd && input$min.fd != input$max.fd) {
        valsfd$maxV.fd <- input$max.fd
        valsfd$minV.fd <- input$min.fd
        if(input$trailsfd == "Other") {
          req(input$Otherfd)
          if(!is.null(input$Otherfd)) {
            valsfd$trail.fd <- input$Otherfd
          }else showModal(simuModal.fd(failed = TRUE))
        }else {
          valsfd$trail.fd <- as.character(input$trailsfd)
        }
        removeModal()
      }else {
        showModal(
          shinyjqui::jqui_draggable(
            simuModal.fd(failed = TRUE)
          )
        )
      }
    })
    
    simuData_fd <- reactive({
      req(fd_reactive()$fieldBook)
      if(!is.null(valsfd$maxV.fd) && !is.null(valsfd$minV.fd) && !is.null(valsfd$trail.fd)) {
        max <- as.numeric(valsfd$maxV.fd)
        min <- as.numeric(valsfd$minV.fd)
        df.fd <- fd_reactive()$fieldBook
        cnamesdf.fd <- colnames(df.fd)
        df.fd <- norm_trunc(a = min, b = max, data = df.fd)
        colnames(df.fd) <- c(cnamesdf.fd[1:(ncol(df.fd) - 1)], valsfd$trail.fd)
        a <- ncol(df.fd)
      }else {
        df.fd <- fd_reactive()$fieldBook  
        a <- ncol(df.fd)
      }
      return(list(df = df.fd, a = a))
    })
    
    
    output$FD.Output <- DT::renderDataTable({
      df <- simuData_fd()$df
      a <- as.numeric(simuData_fd()$a)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      
      DT::datatable(df, rownames = FALSE, options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
      
    })
    
    output$downloadData.fd <- downloadHandler(
      filename = function() {
        loc <- paste("Full_Factorial_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(simuData_fd()$df)
        write.csv(df, file, row.names = FALSE)
      }
    )
  })
}
    
## To be copied in the UI
# mod_FD_ui("FD_ui_1")
    
## To be copied in the server
# mod_FD_server("FD_ui_1")
