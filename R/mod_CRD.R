#' CRD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom utils write.csv
mod_CRD_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Completely Randomized Design"),
    sidebarLayout(
      sidebarPanel(width = 4,
                   radioButtons(inputId = ns("owndatacrd"), label = "Import entries' list?", choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                   conditionalPanel("input.owndatacrd != 'Yes'", ns = ns,
                                    fluidRow(
                                      column(6, style=list("padding-right: 28px;"),
                                             numericInput(ns("t.crd"), label = "Input # of Treatments:",
                                                          value = NULL, min = 2)
                                      ),
                                      column(6, style=list("padding-left: 5px;"),
                                             numericInput(ns("reps.crd"), label = "Input # of Full Reps:",
                                                          value = NULL, min = 1)
                                      )
                                    )
                   ),
                   conditionalPanel("input.owndatacrd == 'Yes'", ns = ns,
                                    fluidRow(
                                      column(7, style=list("padding-right: 28px;"),
                                             fileInput(ns("file.CRD"), label = "Upload a CSV File:", multiple = FALSE)),
                                      column(5,style=list("padding-left: 5px;"),
                                             radioButtons(ns("sep.crd"), "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t"),
                                                          selected = ","))
                                    )
                   ),
                   
                   fluidRow(
                     column(6, style=list("padding-right: 28px;"),
                            textInput(ns("plot_start.crd"), "Starting Plot Number:", value = 101)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            textInput(ns("Location.crd"), "Input Location:", value = "FARGO")
                     )
                   ),
                   
                   selectInput(inputId = ns("planter_mov_crd"), label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"), multiple = FALSE,
                               selected = "serpentine"),
                   
                   numericInput(inputId = ns("myseed.crd"), label = "Seed Number:",
                                value = 123, min = 1),
                   fluidRow(
                     column(6,
                            downloadButton(ns("downloadData.crd"), "Save Experiment!", style = "width:100%")
                     ),
                     column(6,
                            actionButton(ns("Simulate.crd"), "Simulate!", icon = icon("cocktail"), width = '100%')
                     )
                   )
      ),
      mainPanel(width = 8,
        fluidRow(
          column(12, align="center",
                 tabsetPanel(
                   tabPanel("Completely Randomized Field Layout", shinycssloaders::withSpinner(plotOutput(ns("layout.crd"), width = "100%", height = "630px"),
                                                                                               type = 5)),
                   tabPanel("Completely Randomized Field Book", DT::DTOutput(ns("CRD.output")))
                 )
          ),
          column(12,uiOutput(ns("well_panel_layout_CRD")))
        )
      )
    )
  )
}

# fluidRow(
#   column(12, align="center",
#          tabsetPanel(
#            tabPanel("Completely Randomized Field Layout", shinycssloaders::withSpinner(plotOutput(ns("layout.crd"), width = "100%", height = "630px"),
#                                                                                        type = 5)),
#            tabPanel("Completely Randomized Field Book", DT::DTOutput(ns("CRD.output")))
#          )
#   ),
#   column(12,uiOutput(ns("well_panel_layout")))
# )
# "Completely Randomized Field Book", DT::DTOutput(ns("CRD.output"))
#' CRD Server Function
#'
#' @noRd 
mod_CRD_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    getData.crd <- reactive({
      req(input$file.CRD)
      req(input$sep.crd)
      inFile <- input$file.CRD
      dataUp.crd <- load_file(name = inFile$name, path = inFile$datapat, sep = input$sep.crd)
      return(list(dataUp.crd = dataUp.crd))
    })
    
    
    CRD_reactive <- reactive({
      req(input$plot_start.crd)
      req(input$Location.crd)
      req(input$myseed.crd)
      
      myseed.crd <- as.numeric(input$myseed.crd)
      
      if (input$owndatacrd == "Yes") {
        t <- NULL; reps <- NULL
        data.crd <- getData.crd()$dataUp.crd
      }else {
        req(input$t.crd, input$reps.crd)
        t <- as.numeric(input$t.crd);reps <- as.numeric(input$reps.crd)
        data.crd <- NULL
      }
      
      plot_start.crd <- as.vector(unlist(strsplit(input$plot_start.crd, ",")))
      plot_start.crd <- as.numeric(plot_start.crd)
      loc <-  as.vector(unlist(strsplit(input$Location.crd, ",")))
      
      my.design <- CRD(t = t, reps = reps, plotNumber = plot_start.crd, seed = myseed.crd,
                       locationName = loc, data = data.crd)
      
    })
    
    output$well_panel_layout_CRD <- renderUI({
      req(CRD_reactive()$fieldBook)
      #req(input$planter_mov_crd)
      obj_crd <- CRD_reactive()
      planting_crd <- input$planter_mov_crd
      allBooks_crd <- plot_layout(x = obj_crd, optionLayout = 1, planter = planting_crd)$newBooks
      nBooks_crd <- length(allBooks_crd)
      layoutOptions_crd <- 1:nBooks_crd
      wellPanel(
        fluidRow(
          column(2,
                 radioButtons(ns("typlotCRD"), "Type of Plot:",
                              c("Entries/Treatments" = 1,
                                "Plots" = 2))
          ),
          column(3, #align="center",
                 selectInput(inputId = ns("layoutO_crd"), label = "Layout option:", choices = layoutOptions_crd)
          )
          # column(3, #align="center",
          #        selectInput(inputId = ns("locLayout_crd"), label = "Location:", choices = 1)
          # )
          # column(2, #align="center",
          #        downloadButton(outputId = "downCRDLayout", label = "Download the layout")
          # ),
        )
      )
    })
    
    reactive_layoutCRD <- reactive({
      req(input$layoutO_crd)
      req(CRD_reactive())
      obj_crd <- CRD_reactive()
      opt_crd <- as.numeric(input$layoutO_crd)
      planting_crd <- input$planter_mov_crd
      plot_layout(x = obj_crd, optionLayout = opt_crd, planter = planting_crd)
    })
    
    output$layout.crd <- renderPlot({
      #reactive_layoutCRD()$out_layout
      req(CRD_reactive())
      req(input$typlotCRD)
      if (input$typlotCRD == 1) {
        reactive_layoutCRD()$out_layout
      } else reactive_layoutCRD()$out_layoutPlots
    })
    
    entryListFormat_CRD <- data.frame(TREATMENT = c(paste("TRT_", LETTERS[1:9], sep = "")), 
                                      REP = as.factor(rep(5, 9)))
    entriesInfoModal_CRD <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        renderTable(entryListFormat_CRD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        h4("Note that reps might be unbalanced."),
        easyClose = FALSE
      )
    }
    
    toListen <- reactive({
      list(input$owndatacrd)
    })
    
    observeEvent(toListen(), {
      if (input$owndatacrd == "Yes") {
        showModal(
          shinyjqui::jqui_draggable(
            entriesInfoModal_CRD()
          )
        )
      }
    })
    
    vals <- reactiveValues(maxV.CRD = NULL, minV.CRD = NULL, trail.CRD = NULL)
    
    simuModal.crd <- function(failed = FALSE) {
      modalDialog(
        selectInput(inputId = ns("trailsCRD"), label = "Select One:", choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        conditionalPanel("input.trailsCRD == 'Other'", ns = ns,
                         textInput(inputId = ns("OtherCRD"), label = "Input Trial Name:", value = NULL)
        ),
        fluidRow(
          column(6, 
                 numericInput(ns("min.crd"), "Input the min value", value = NULL)
          ),
          column(6, 
                 numericInput(ns("max.crd"), "Input the max value", value = NULL)
                 
          )
        ),
        
        if (failed)
          div(tags$b("Invalid input of data max and min", style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ok.crd"), "GO")
        )
        
      )
    }
    
    # Show modal when button is clicked.
    observeEvent(input$Simulate.crd, {
      req(CRD_reactive()$fieldBook)
      showModal(
        shinyjqui::jqui_draggable(
          simuModal.crd()
        )
      )
    })
    
    # When OK button is pressed, attempt to load the data set. If successful,
    # remove the modal. If not show another modal, but this time with a failure
    # message.
    observeEvent(input$ok.crd, {
      req(input$max.crd, input$min.crd)
      if (input$max.crd > input$min.crd && input$min.crd != input$max.crd) {
        vals$maxV.CRD <- input$max.crd
        vals$minV.CRD <- input$min.crd
        if(input$trailsCRD == "Other") {
          req(input$OtherCRD)
          vals$trail.CRD <- as.character(input$OtherCRD)
        }else {
          vals$trail.CRD <- as.character(input$trailsCRD)
        }
        removeModal()
      }else {
        showModal(
          shinyjqui::jqui_draggable(
            simuModal.crd(failed = TRUE)
          )
        )
      }
    })
    
    simuDataCRD <- reactive({
      req(CRD_reactive()$fieldBook)
      if(!is.null(vals$maxV.CRD) && !is.null(vals$minV.CRD) && !is.null(vals$trail.CRD)) {
        max <- as.numeric(vals$maxV.CRD)
        min <- as.numeric(vals$minV.CRD)
        #df.crd <- CRD_reactive()$fieldBook
        df.crd <- reactive_layoutCRD()$fieldBookXY
        cnamesdf.crd <- colnames(df.crd)
        df.crd <- norm_trunc(a = min, b = max, data = df.crd)
        colnames(df.crd) <- c(cnamesdf.crd[1:(ncol(df.crd) - 1)], vals$trail.CRD)
        df.crd <- df.crd[order(df.crd$ID),]
      }else {
        #df.crd <-  CRD_reactive()$fieldBook
        df.crd <- reactive_layoutCRD()$fieldBookXY
      }
      
      return(list(df = df.crd))
    })
    
    output$CRD.output <- DT::renderDT({
      df <- simuDataCRD()$df
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      DT::datatable(df, rownames = FALSE, options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    })
    
    output$downloadData.crd <- downloadHandler(
      filename = function() {
        loc <- paste("CRD_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(simuDataCRD()$df)
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    # # downloadHandler contains 2 arguments as functions, namely filename, content
    # output$downCRDLayout <- downloadHandler(
    #   filename =  function() {
    #     paste("CRD_Layout", "png", sep=".")
    #   },
    #   # content is a function with argument file. content writes the plot to the device
    #   content = function(file) {
    #     grDevices::png(file)
    #     
    #     if (input$typlotCRD == 1) {
    #       reactive_layoutCRD()$out_layout
    #     } else reactive_layoutCRD()$out_layoutPlots
    #     plot(x=x(), y=y(), main = "iris dataset plot", xlab = xl(), ylab = yl()) # draw the plot
    #     dev.off()  # turn the device off
    #     # if(input$var3 == "png")
    #     #   png(file) # open the png device
    #     # else
    #     #   pdf(file) # open the pdf device
    #     # plot(x=x(), y=y(), main = "iris dataset plot", xlab = xl(), ylab = yl()) # draw the plot
    #     # dev.off()  # turn the device off
    #     
    #   } 
    # )
    
  })
}
    
## To be copied in the UI
# mod_CRD_ui("CRD_ui_1")
    
## To be copied in the server
# mod_CRD_server("CRD_ui_1")
