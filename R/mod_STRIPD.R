#' STRIPD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_STRIPD_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("Strip-Plot Design"),
    sidebarLayout(
      sidebarPanel(width = 4,
                   radioButtons(inputId = ns("owndataSTRIP"), label = "Import entries' list?", choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                   conditionalPanel("input.owndataSTRIP == 'Yes'", ns = ns,
                                    fluidRow(
                                      column(8, style=list("padding-right: 28px;"),
                                             fileInput(ns("file.STRIP"), label = "Upload a csv File:", multiple = FALSE)),
                                      
                                      column(4,style=list("padding-left: 5px;"),
                                             radioButtons(ns("sep.strip"), "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t"),
                                                          selected = ","))
                                    )
                   ),
                   
                   conditionalPanel("input.owndataSTRIP != 'Yes'", ns = ns,
                                    fluidRow(
                                      column(6, style=list("padding-right: 28px;"),
                                             numericInput(ns("HStrip.strip"), label = "Input # of Horizontal Stripes:",
                                                          value = NULL, min = 2)
                                      ),
                                      column(6, style=list("padding-left: 5px;"),
                                             numericInput(ns("VStrip.strip"), label = "Input # of Vertical Stripes:",
                                                          value = NULL, min = 2)
                                      )
                                    )           
                   ),
                   numericInput(ns("blocks.strip"), label = "Input # of Full Reps:", value = 2, min = 2),
                   numericInput(ns("l.strip"), label = "Input # of Locations:",
                                value = 1, min = 1), 
                   selectInput(inputId = ns("planter.strip"), label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"), multiple = FALSE,
                               selected = "serpentine"),
                   fluidRow(
                     column(6, style=list("padding-right: 28px;"),
                            textInput(ns("plot_start.strip"), "Starting Plot Number:", value = 101)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            textInput(ns("Location.strip"), "Input Location:", value = "FARGO")
                     )
                   ),
                   
                   numericInput(inputId = ns("myseed.strip"), label = "Seed Number:", value = 123, min = 1),
                   
                   fluidRow(
                     column(6,
                            downloadButton(ns("downloadData.strip"), "Save Experiment!", style = "width:100%")
                     ),
                     column(6,
                            actionButton(ns("Simulate.strip"), "Simulate!", icon = icon("cocktail"), width = '100%')
                     )
                   )
      ),
      
      mainPanel(
        width = 8,
        tabsetPanel(
          tabPanel("Field Book", DT::DTOutput(ns("STRIP.output")))
        )
      )
    ) 
  )
}
    
#' STRIPD Server Functions
#'
#' @noRd 
mod_STRIPD_server <- function(id){
  moduleServer( id, function(input, output, session) {
    
    ns <- session$ns
    
    getData.strip <- reactive({
      req(input$file.STRIP)
      inFile <- input$file.STRIP
      dataUp.strip <- load_file(name = inFile$name, path = inFile$datapat, sep = input$sep.strip)
      return(list(dataUp.strip = dataUp.strip))
    })
    
    strip_reactive <- reactive({
      
      req(input$plot_start.strip)
      req(input$Location.strip)
      req(input$myseed.strip)
      req(input$l.strip)
      
      l.strip <- as.numeric(input$l.strip)
      seed.strip <- as.numeric(input$myseed.strip)
      plot_start.strip <- as.vector(unlist(strsplit(input$plot_start.strip, ",")))
      plot_start.strip <- as.numeric(plot_start.strip)
      loc.strip <-  as.vector(unlist(strsplit(input$Location.strip, ",")))
      
      
      req(input$blocks.strip)
      reps.strip <- as.numeric(input$blocks.strip)
      
      if (input$owndataSTRIP == "Yes") {
        Hplots.strip <- NULL
        Vplots.strip <- NULL
        data.strip <- getData.strip()$dataUp.strip
      }else {
        req(input$HStrip.strip, input$VStrip.strip)
        Hplots.strip <- as.numeric(input$HStrip.strip)
        Vplots.strip <- as.numeric(input$VStrip.strip)
        data.strip <- NULL
      }
      
      STRIP <- strip_plot(Hplots = Hplots.strip, 
                          Vplots = Vplots.strip, 
                          b = reps.strip, 
                          l = l.strip,
                          seed = seed.strip,
                          planter = input$planter.strip,
                          plotNumber = plot_start.strip,
                          locationNames = loc.strip, 
                          data = data.strip)
    })
    
    
    valsStrip <- reactiveValues(maxV.strip = NULL, minV.strip = NULL, trail.strip = NULL)
    
    simuModal.strip <- function(failed = FALSE) {
      modalDialog(
        selectInput(inputId = ns("trailsStrip"), label = "Select One:", choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        conditionalPanel("input.trailsStrip == 'Other'", ns = ns,
                         textInput(inputId = ns("OtherStrip"), label = "Input Trail Name:", value = NULL)
        ),
        fluidRow(
          column(6, 
                 numericInput(ns("min.strip"), "Input the min value", value = NULL)
          ),
          column(6, 
                 numericInput(ns("max.strip"), "Input the max value", value = NULL)  
          )
        ),
        
        if (failed)
          div(tags$b("Invalid input of data max and min", style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ok.strip"), "GO")
        )
        
      )
      
    }
    
    observeEvent(input$Simulate.strip, {
      req(strip_reactive()$fieldBook)
      showModal(
        shinyjqui::jqui_draggable(
          simuModal.strip()
        )
      )
    })
    
    observeEvent(input$ok.strip, {
      req(input$max.strip, input$min.strip)
      if (input$max.strip > input$min.strip && input$min.strip != input$max.strip) {
        valsStrip$maxV.strip <- input$max.strip
        valsStrip$minV.strip <- input$min.strip
        if(input$trailsStrip == "Other") {
          req(input$OtherStrip)
          if(!is.null(input$OtherStrip)) {
            valsStrip$trail.strip <- input$OtherStrip
          }else showModal(simuModal.strip(failed = TRUE))
        }else {
          valsStrip$trail.strip <- as.character(input$trailsStrip)
        }
        removeModal()
      }else {
        showModal(
          shinyjqui::jqui_draggable(
            simuModal.strip(failed = TRUE)
          )
        )
      }
    })
    
    
    simuData_strip <- reactive({
      req(strip_reactive()$fieldBook)
      
      if(!is.null(valsStrip$maxV.strip) && !is.null(valsStrip$minV.strip) && !is.null(valsStrip$trail.strip)) {
        max <- as.numeric(valsStrip$maxV.strip)
        min <- as.numeric(valsStrip$minV.strip)
        df.strip <- strip_reactive()$fieldBook
        cnamesdf.strip <- colnames(df.strip)
        df.strip <- norm_trunc(a = min, b = max, data = df.strip)
        colnames(df.strip) <- c(cnamesdf.strip[1:(ncol(df.strip) - 1)], valsStrip$trail.strip)
        a <- ncol(df.strip)
      }else {
        df.strip <- strip_reactive()$fieldBook 
        a <- ncol(df.strip)
      }
      return(list(df = df.strip, a = a))
    })
    
    output$STRIP.output <- DT::renderDataTable({
      
      df <- simuData_strip()$df
      a <- as.numeric(simuData_strip()$a)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      
      DT::datatable(df, rownames = FALSE, options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
      
    })
    
    output$downloadData.strip <- downloadHandler(
      filename = function() {
        loc <- paste("Strip-Plot_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(simuData_strip()$df)
        write.csv(df, file, row.names = FALSE)
      }
    )
    
  })
}
    
## To be copied in the UI
# mod_STRIPD_ui("STRIPD_ui_1")
    
## To be copied in the server
# mod_STRIPD_server("STRIPD_ui_1")
