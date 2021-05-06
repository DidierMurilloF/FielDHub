#' Alpha_Lattice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom utils write.csv
mod_Alpha_Lattice_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("Alpha Lattice Design"),
    sidebarLayout(
      sidebarPanel(width = 4,
                   radioButtons(ns("owndata_alpha"), label = "Import entries' list?", choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                   
                   conditionalPanel("input.owndata_alpha != 'Yes'", ns = ns,
                                    numericInput(ns("t.alpha"), label = "Input # of Treatments:",
                                                 value = NULL, min = 2)
                                    
                   ),
                   conditionalPanel("input.owndata_alpha == 'Yes'", ns = ns,
                                    fluidRow(
                                      column(8, style=list("padding-right: 28px;"),
                                             fileInput(inputId = ns("file.alpha"), label = "Upload a CSV File:", multiple = FALSE)),
                                      column(4, style=list("padding-left: 5px;"),
                                             radioButtons(inputId = ns("sep.alpha"), "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t"),
                                                          selected = ","))
                                    )        
                   ),
                   numericInput(inputId = ns("r.alpha"), label = "Input # of Full Reps:", value = NULL, min = 2),
                   selectInput(inputId = ns("k.alpha"), label = "Input # of Plots per IBlock:", choices = ""),
                   numericInput(inputId = ns("l.alpha"), label = "Input # of Locations:", value = NULL, min = 1),
                   
                   fluidRow(
                     column(6, style=list("padding-right: 28px;"),
                            textInput(inputId = ns("plot_start.alpha"), "Starting Plot Number:", value = 101)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            textInput(inputId = ns("Location.alpha"), "Input Location:", value = "FARGO")
                     )
                   ),  
                   numericInput(inputId = ns("myseed.alpha"), label = "Seed Number:",
                                value = 16, min = 1),
                   fluidRow(
                     column(6,
                            actionButton(inputId = ns("RUN.alpha"), "Run!", icon = icon("cocktail"), width = '100%'),
                     ),
                     column(6,
                            actionButton(inputId = ns("Simulate.alpha"), "Simulate!", icon = icon("cocktail"), width = '100%')
                     )
                     
                   ), 
                   br(),
                   downloadButton(ns("downloadData.alpha"), "Save My Experiment", style = "width:100%")
      ),
      
      mainPanel(
        width = 8,
        tabsetPanel(
          tabPanel("Alpha Lattice Field Book", shinycssloaders::withSpinner(DT::DTOutput(ns("ALPHA.output")), type = 5))
        )
      )
    )
  )
}
    
#' Alpha_Lattice Server Functions
#'
#' @noRd 
mod_Alpha_Lattice_server <- function(id){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    getData.alpha <- reactive({
      req(input$file.alpha)
      inFile <- input$file.alpha
      dataUp.alpha <- load_file(name = inFile$name, path = inFile$datapat, sep = input$sep.alpha)
      return(list(dataUp.alpha = dataUp.alpha))
    })
    
    get_tALPHA <- reactive({
      if(is.null(input$file.alpha)) {
        req(input$t.alpha)
        t_alpha <- input$t.alpha
      }else {
        req(input$file.alpha)
        t_alpha <- nrow(getData.alpha()$dataUp.alpha)
      }
      return(list(t_alpha = t_alpha))
    })
    
    observeEvent(get_tALPHA()$t_alpha, {
      req(get_tALPHA()$t_alpha)
      
      t <- as.numeric(get_tALPHA()$t_alpha)
      if (numbers::isPrime(t)) {
        w <- 1
        k <- "No Options Available"
      }else {
        k <- numbers::divisors(t)
        k <- k[2:(length(k) - 1)]
        w <- 2
      }

      updateSelectInput(session = session, inputId = 'k.alpha', label = "Input # of Plots per IBlock:",
                        choices = k, selected = k[1])

    })
    
    
    entryListFormat_ALPHA <- data.frame(ENTRY = 1:9, 
                                        NAME = c(paste("Genotype", LETTERS[1:9], sep = "")))
    entriesInfoModal_ALPHA <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        renderTable(entryListFormat_ALPHA,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        h4("Entry numbers can be any set of consecutive positive numbers."),
        easyClose = FALSE
      )
    }
    
    toListen <- reactive({
      list(input$owndata_alpha)
    })
    
    observeEvent(toListen(), {
      if (input$owndata_alpha == "Yes") {
        showModal(
          shinyjqui::jqui_draggable(
            entriesInfoModal_ALPHA()
          )
        )
      }
    })
    

    ALPHA_reactive <- eventReactive(input$RUN.alpha, {
      
      req(input$k.alpha)
      req(input$myseed.alpha)
      req(input$plot_start.alpha)
      req(input$Location.alpha)
      req(input$l.alpha)
      req(input$r.alpha)
      r.alpha <- as.numeric(input$r.alpha)
      k.alpha <- as.numeric(input$k.alpha)

      plot_start.alpha <- as.vector(unlist(strsplit(input$plot_start.alpha, ",")))
      plot_start.alpha <- as.numeric(plot_start.alpha)
      loc <-  as.vector(unlist(strsplit(input$Location.alpha, ",")))
      
      if (input$owndata_alpha == "Yes") {
        t.alpha <- as.numeric(get_tALPHA()$t_alpha)
        data.alpha <- as.data.frame(getData.alpha()$dataUp.alpha)
        if (ncol(data.alpha) < 2) shiny::validate("Data input needs at least two columns with: ENTRY and NAME.")
        data_alpha <- as.data.frame(data.alpha[,c(1,2)])
      }else {
        req(input$t.alpha)
        t.alpha <- as.numeric(input$t.alpha)
        data_alpha <- NULL
      }
      seed.alpha <- as.numeric(input$myseed.alpha)
      l.alpha <- as.numeric(input$l.alpha)
      if (r.alpha < 2) validate("Alpha Design needs at least 2 replicates.")
      
      
      if(k.alpha == "No Options Available") shiny::validate("No Options Available.")
      s <- t.alpha / k.alpha
      if (s %% 1 != 0) validate("No Options Available.")

      alpha_lattice(t = t.alpha, k = k.alpha, r = r.alpha, l = l.alpha, 
                    plotNumber = plot_start.alpha, 
                    seed = seed.alpha,
                    locationNames = loc, 
                    data = data_alpha) 
      
    })
    
    
    valsALPHA <- reactiveValues(maxV.alpha = NULL, minV.alpha = NULL, trail.alpha = NULL)

      simuModal.alpha <- function(failed = FALSE) {
        modalDialog(
          selectInput(inputId = ns("trailsALPHA"), label = "Select One:", choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
          conditionalPanel("input.trailsALPHA == 'Other'", ns = ns,
                           textInput(inputId = ns("OtherALPHA"), label = "Input the Trial Name:", value = NULL)
          ),
          fluidRow(
            column(6,
                   numericInput(inputId = ns("min.alpha"), "Input the min value", value = NULL)
            ),
            column(6,
                   numericInput(inputId = ns("max.alpha"), "Input the max value", value = NULL)
     
            )
     
          ),
     
          if (failed)
            div(tags$b("Invalid input of data max and min", style = "color: red;")),
     
          footer = tagList(
            modalButton("Cancel"),
            actionButton(inputId = ns("ok.alpha"), "GO")
          )
     
        )
     
      }
     
      observeEvent(input$Simulate.alpha, {
        req(input$k.alpha)
        req(input$r.alpha)
        req(ALPHA_reactive()$fieldBook)
        showModal(
          shinyjqui::jqui_draggable(
            simuModal.alpha()
          )
        )
      })
     
      observeEvent(input$ok.alpha, {
        req(input$max.alpha, input$min.alpha)
        if (input$max.alpha > input$min.alpha && input$min.alpha != input$max.alpha) {
          valsALPHA$maxV.alpha <- input$max.alpha
          valsALPHA$minV.alpha <- input$min.alpha
          if(input$trailsALPHA == "Other") {
            req(input$OtherALPHA)
            if(!is.null(input$OtherALPHA)) {
              valsALPHA$trail.alpha <- as.character(input$OtherALPHA)
            }else showModal(simuModal.alpha(failed = TRUE))
          }else {
            valsALPHA$trail.alpha <- as.character(input$trailsALPHA)
          }
          removeModal()
        }else {
          showModal(
            shinyjqui::jqui_draggable(
              simuModal.alpha(failed = TRUE)
            )
          )
        }
      })
     
     
      simuDataALPHA <- reactive({
        req(ALPHA_reactive()$fieldBook)
        if(!is.null(valsALPHA$maxV.alpha) && !is.null(valsALPHA$minV.alpha) && !is.null(valsALPHA$trail.alpha)) {
          max <- as.numeric(valsALPHA$maxV.alpha)
          min <- as.numeric(valsALPHA$minV.alpha)
          df.alpha <- ALPHA_reactive()$fieldBook
          cnamesdf.alpha <- colnames(df.alpha)
          df.alpha <- norm_trunc(a = min, b = max, data = df.alpha)
          colnames(df.alpha) <- c(cnamesdf.alpha[1:(ncol(df.alpha) - 1)], valsALPHA$trail.alpha)
          a <- ncol(df.alpha)
        }else {
          df.alpha <-  ALPHA_reactive()$fieldBook
          a <- ncol(df.alpha)
        }
        return(list(df = df.alpha, a = a))
      })
     
     
      output$ALPHA.output <- DT::renderDataTable({
        req(input$k.alpha)
        k.alpha <- input$k.alpha
        if (k.alpha == "No Options Available") {
          validate("No options for these amout of treatments ):")
        }
        req(simuDataALPHA()$df)
        df <- simuDataALPHA()$df
        a <- as.numeric(simuDataALPHA()$a)
        options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                  scrollX = TRUE, scrollY = "500px"))
     
        DT::datatable(df, rownames = FALSE, options = list(
          columnDefs = list(list(className = 'dt-center', targets = "_all"))))
     
      })
    
    
    # Downloadable csv of selected dataset ----
    output$downloadData.alpha <- downloadHandler(
      filename = function() {
        loc <- paste("Alpha_Lattice_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(simuDataALPHA()$df)
        write.csv(df, file, row.names = FALSE)
      }
    )
    #Update_k = Update_k
    #return(list(ALPHA.output = ALPHA.output))
    
  })
}
    
## To be copied in the UI
# mod_Alpha_Lattice_ui("Alpha_Lattice_ui_1")
    
## To be copied in the server
# mod_Alpha_Lattice_server("Alpha_Lattice_ui_1")
