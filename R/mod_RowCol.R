#' RowCol UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_RowCol_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("Row-Column Design"),
    sidebarLayout(
      sidebarPanel(width = 4,

                   radioButtons(inputId = ns("owndataRCD"), label = "Import entries' list?", choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                   
                   conditionalPanel("input.owndataRCD == 'Yes'", ns = ns,
                                    fluidRow(
                                      column(8, style=list("padding-right: 28px;"),
                                             fileInput(ns("file.RCD"), label = "Upload a csv File:", multiple = FALSE)),
                                      column(4,style=list("padding-left: 5px;"),
                                             radioButtons(ns("sep.rcd"), "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t"),
                                                          selected = ","
                                             )
                                      )
                                    )
                   ),
                   
                   numericInput(ns("t.rcd"), label = "Input # of Treatments:",
                                value = NULL, min = 1),
                   
                   fluidRow(
                     column(6, style=list("padding-right: 28px;"),
                            numericInput(ns("k.rcd"), label = "Input # of Rows:",
                                         value = NULL, min = 2)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            numericInput(ns("r.rcd"), label = "Input # of Full Reps:",
                                         value = 2, min = 2)
                     )
                   ),
                   
                   numericInput(inputId = ns("l.rcd"), label = "Input # of Locations:", value = 1, min = 1),
                   
                   fluidRow(

                     column(6, style=list("padding-right: 28px;"),
                            textInput(ns("plot_start.rcd"), "Starting Plot Number:", value = 101)
                     ),
                     column(6, style=list("padding-left: 5px;"),
                            textInput(ns("Location.rcd"), "Input Location:", value = "FARGO")
                     )
                   ),
                   
                   selectInput(inputId = ns("planter_mov_rcd"), label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"), multiple = FALSE,
                               selected = "serpentine"),
                   
                   numericInput(ns("seed.rcd"), label = "Seed Number:", value = 1),
                   
                   fluidRow(
                     column(6,
                            downloadButton(ns("downloadData.rowcolD"), "Save Experiment!", style = "width:100%")
                     ),
                     column(6,
                            actionButton(ns("Simulate.RowCol"), "Simulate!", icon = icon("cocktail"), width = '100%')
                     )
                   )
      ),

      mainPanel(
        width = 8,
        fluidRow(
          column(12, align="center",
                 tabsetPanel(
                   tabPanel("Field Layout", shinycssloaders::withSpinner(plotOutput(ns("layout_rowcol"), width = "100%", height = "630px"),
                                                                         type = 5)),
                   tabPanel("Field Book", DT::DTOutput(ns("rowcolD")))
                 )
          ),
          column(12,uiOutput(ns("well_panel_layout_ROWCOL")))
        )
      )
    )
  )
}

#' RowCol Server Functions
#'
#' @noRd 
mod_RowCol_server <- function(id){
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    entryListFormat_RCD <- data.frame(ENTRY = 1:9, 
                                       NAME = c(paste("Genotype", LETTERS[1:9], sep = "")))
    entriesInfoModal_RCD <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        renderTable(entryListFormat_RCD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        h4("Entry numbers can be any set of consecutive positive numbers."),
        easyClose = FALSE
      )
    }
    
    toListen <- reactive({
      list(input$owndataRCD)
    })
    
    observeEvent(toListen(), {
      if (input$owndataRCD == "Yes") {
        showModal(
          shinyjqui::jqui_draggable(
            entriesInfoModal_RCD()
          )
        )
      }
    })
    
    getData.rcd <- reactive({
      req(input$file.RCD)
      inFile <- input$file.RCD
      dataUp.rcd <- load_file(name = inFile$name, path = inFile$datapat, sep = input$sep.rcd)
      return(list(dataUp.rcd = dataUp.rcd))
    })
    
    Get_tROWCOL <- reactive({
      if(is.null(input$file.RCD)) {
        req(input$t.rcd)
        t.ROWCOL <- input$t.rcd
      }else {
        req(input$file.RCD)
        t.ROWCOL <- nrow(getData.rcd()$dataUp.rcd)
      }
      return(list(t.ROWCOL = t.ROWCOL))
    })
    
    RowCol_reactive <- reactive({
      
      req(input$t.rcd)
      req(input$k.rcd)
      req(input$r.rcd)
      
      req(input$plot_start.rcd)
      req(input$Location.rcd)
      
      t.rcd <- as.numeric(input$t.rcd)
      k.rcd <- as.numeric(input$k.rcd)
      r.rcd <- as.numeric(input$r.rcd)
      
      
      plot_start.rcd <- as.vector(unlist(strsplit(input$plot_start.rcd, ",")))
      plot_start.rcd <- as.numeric(plot_start.rcd)
      loc.rcd <-  as.vector(unlist(strsplit(input$Location.rcd, ",")))
      
      if (input$owndataRCD == "Yes") {
        t.rcd <- as.numeric(Get_tROWCOL()$t.ROWCOL)
        data.rcd <- getData.rcd()$dataUp.rcd
      }else {
        req(input$t.rcd)
        t.rcd <- as.numeric(input$t.rcd)
        data.rcd <- NULL
      }
      seed.rcd <- as.numeric(input$seed.rcd)
      l.rcd <- as.numeric(input$l.rcd)

      row_column(t = t.rcd, nrows = k.rcd, r = r.rcd, l = l.rcd,
                 plotNumber = plot_start.rcd, 
                 locationNames = loc.rcd,
                 seed = seed.rcd, 
                 data = data.rcd)
      
    })
    
    
    output$well_panel_layout_ROWCOL <- renderUI({
      req(RowCol_reactive()$fieldBook)
      obj_rcd <- RowCol_reactive()
      planting_rcd <- input$planter_mov_rcd
      allBooks_rcd<- plot_layout(x = obj_rcd, optionLayout = 1)$newBooks
      nBooks_rcd <- length(allBooks_rcd)
      layoutOptions_rcd <- 1:nBooks_rcd
      loc <-  as.vector(unlist(strsplit(input$Location.rcd, ",")))
      wellPanel(
        fluidRow(
          column(2,
                 radioButtons(ns("typlotrcd"), "Type of Plot:",
                              c("Entries/Treatments" = 1,
                                "Plots" = 2))
          ),
          column(3, #align="center",
                 selectInput(inputId = ns("layoutO_rcd"), label = "Layout option:", choices = layoutOptions_rcd)
          ),
          column(3, #align="center",
                 selectInput(inputId = ns("locLayout_rcd"), label = "Location:", choices = loc)
          )
        )
      )
    })
    
    reactive_layoutROWCOL <- reactive({
      req(input$layoutO_rcd)
      req(RowCol_reactive())
      obj_rcd <- RowCol_reactive()
      opt_rcd <- as.numeric(input$layoutO_rcd)
      planting_rcd <- input$planter_mov_rcd
      plot_layout(x = obj_rcd, optionLayout = opt_rcd, planter = planting_rcd)
    })
    
    output$layout_rowcol <- renderPlot({
      req(RowCol_reactive())
      req(input$typlotrcd)
      if (input$typlotrcd == 1) {
        reactive_layoutROWCOL()$out_layout
      } else reactive_layoutROWCOL()$out_layoutPlots
    })
    
    
    
    valsRowColD <- reactiveValues(maxV.RowCol = NULL, minV.RowCol = NULL, trail.RowCol = NULL)
    
    simuModal.RowCol <- function(failed = FALSE) {
      modalDialog(
        selectInput(inputId = ns("trailsRowCol"), label = "Select One:", choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        conditionalPanel("input.trailsRowCol == 'Other'", ns = ns,
                         textInput(inputId = ns("OtherRowCol"), label = "Input Trial Name:", value = NULL)
        ),
        fluidRow(
          column(6, 
                 numericInput(ns("min.RowCol"), "Input the min value", value = NULL)
          ),
          column(6, 
                 numericInput(ns("max.RowCol"), "Input the max value", value = NULL)
                 
          )
          
        ),
        
        if (failed)
          div(tags$b("Invalid input of data max and min", style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ok.RowCol"), "GO")
        )
        
      )
      
    }
    
    observeEvent(input$Simulate.RowCol, {
      req(RowCol_reactive()$fieldBook)
      showModal(
        shinyjqui::jqui_draggable(
          simuModal.RowCol()
        )
      )
    })
    
    observeEvent(input$ok.RowCol, {
      req(input$max.RowCol, input$min.RowCol)
      if (input$max.RowCol > input$min.RowCol && input$min.RowCol != input$max.RowCol) {
        valsRowColD$maxV.RowCol <- input$max.RowCol
        valsRowColD$minV.RowCol <- input$min.RowCol
        if(input$trailsRowCol == "Other") {
          req(input$OtherRowCol)
          if(!is.null(input$OtherRowCol)) {
            valsRowColD$trail.RowCol <- input$OtherRowCol
          }else showModal(simuModal.RowCol(failed = TRUE))
        }else {
          valsRowColD$trail.RowCol <- as.character(input$trailsRowCol)
        }
        removeModal()
      }else {
        showModal(
          shinyjqui::jqui_draggable(
            simuModal.RowCol(failed = TRUE)
          )
        )
      }
    })
    
    simuData_RowCol <- reactive({
      req(RowCol_reactive()$fieldBook)
      if(!is.null(valsRowColD$maxV.RowCol) && !is.null(valsRowColD$minV.RowCol) && !is.null(valsRowColD$trail.RowCol)) {
        max <- as.numeric(valsRowColD$maxV.RowCol)
        min <- as.numeric(valsRowColD$minV.RowCol)
        #df.RowCol <- RowCol_reactive()$fieldBook
        df.RowCol <- reactive_layoutROWCOL()$fieldBookXY
        cnamesdf.RowCol <- colnames(df.RowCol)
        df.RowCol <- norm_trunc(a = min, b = max, data = df.RowCol)
        colnames(df.RowCol) <- c(cnamesdf.RowCol[1:(ncol(df.RowCol) - 1)], valsRowColD$trail.RowCol)
        a <- ncol(df.RowCol)
      }else {
        #df.RowCol <- RowCol_reactive()$fieldBook  
        df.RowCol <- reactive_layoutROWCOL()$fieldBookXY
        a <- ncol(df.RowCol)
      }
      return(list(df = df.RowCol, a = a))
    })
    
    output$rowcolD <- DT::renderDataTable({
      df <- simuData_RowCol()$df
      a <- as.numeric(simuData_RowCol()$a)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      DT::datatable(df, rownames = FALSE, options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
      
    })

    output$downloadData.rowcolD <- downloadHandler(
      filename = function() {
        loc <- paste("Row-Column_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(simuData_RowCol()$df)
        write.csv(df, file, row.names = FALSE)
      }
    )
  })
}
    
## To be copied in the UI
# mod_RowCol_ui("RowCol_ui_1")
    
## To be copied in the server
# mod_RowCol_server("RowCol_ui_1")
