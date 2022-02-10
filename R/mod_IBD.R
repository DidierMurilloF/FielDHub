#' IBD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_IBD_ui <- function(id) {
  ns <- NS(id)
  # Options for Spinner
  #options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)
  tagList(
    h4("Incomplete Blocks Design"),
    sidebarLayout(
      sidebarPanel(width = 4,
                   radioButtons(ns("owndataibd"), label = "Import entries' list?", choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                   
                   conditionalPanel("input.owndataibd != 'Yes'", ns = ns,
                                    numericInput(ns("t.ibd"), label = "Input # of Treatments:",
                                                 value = NULL, min = 2)
                   ),
                   conditionalPanel("input.owndataibd == 'Yes'", ns = ns,
                                    fluidRow(
                                      column(8, style=list("padding-right: 28px;"),
                                             fileInput(inputId = ns("file.IBD"), label = "Upload a CSV File:", multiple = FALSE)),
                                      column(4, style=list("padding-left: 5px;"),
                                             radioButtons(inputId = ns("sep.ibd"), "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t"),
                                                          selected = ","))
                                    )        
                   ),
                   
                   numericInput(inputId = ns("r.ibd"), label = "Input # of Full Reps:", value = NULL, min = 2),
                   
                   selectInput(inputId = ns("k.ibd"), label = "Input # of Plots per IBlock:", choices = ""),
                   
                   numericInput(inputId = ns("l.ibd"), label = "Input # of Locations:", value = NULL, min = 1),
                   
                   
                   fluidRow(
                     column(6, style=list("padding-right: 28px;"),
                            textInput(inputId = ns("plot_start.ibd"), "Starting Plot Number:", value = 101)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            textInput(inputId = ns("Location.ibd"), "Input Location:", value = "FARGO")
                     )
                   ), 
                   
                   selectInput(inputId = ns("planter_mov_ibd"), label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"), multiple = FALSE,
                               selected = "serpentine"),
                   
                   numericInput(inputId = ns("myseed.ibd"), label = "Seed Number:",
                                value = 4),
                   
                   fluidRow(
                     column(6,
                            downloadButton(ns("downloadData.ibd"), "Save Experiment!", style = "width:100%")
                     ),
                     column(6,
                            actionButton(ns("Simulate.ibd"), "Simulate!", icon = icon("cocktail"), width = '100%')
                     )
                   )
      ),
      
      mainPanel(
        width = 8,
        fixedRow(
          column(12, align="center", uiOutput(ns("tabsetIBD"))),
          column(12, uiOutput(ns("well_panel_layout_IBD")))
        )
      )
    )
  )
}

#' IBD Server Functions
#'
#' @noRd 
mod_IBD_server <- function(id){
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    treatments <- paste("TX-", 1:9, sep = "")
    entryListFormat_IBD <- data.frame(ENTRY = 1:9, 
                                      NAME = treatments)
    entriesInfoModal_IBD <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        renderTable(entryListFormat_IBD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        h4("Entry numbers can be any set of consecutive positive numbers."),
        easyClose = FALSE
      )
    }
    
    toListen <- reactive({
      list(input$owndataibd)
    })
    
    observeEvent(toListen(), {
      if (input$owndataibd == "Yes") {
        showModal(
          shinyjqui::jqui_draggable(
            entriesInfoModal_IBD()
          )
        )
      }
    })
    
    
    
    getData.ibd <- reactive({
      req(input$file.IBD)
      inFile <- input$file.IBD
      dataUp.ibd <- load_file(name = inFile$name, path = inFile$datapat, sep = input$sep.ibd)
      return(list(dataUp.ibd = dataUp.ibd))
    })
    
    get_tIBD <- reactive({
      if(is.null(input$file.IBD)) {
        req(input$t.ibd)
        t_ibd <- input$t.ibd
      }else {
        req(input$file.IBD)
        t_ibd <- nrow(getData.ibd()$dataUp.ibd)
      }
      return(list(t_ibd = t_ibd))
    })
    
    observeEvent(get_tIBD()$t_ibd, {
      
      req(get_tIBD()$t_ibd)
      t <- as.numeric(get_tIBD()$t_ibd)
      if (numbers::isPrime(t)) {
        w <- 1
        k <- "No Options Available"
      }else {
        k <- numbers::divisors(t)
        k <- k[2:(length(k) - 1)]
        w <- 2
      }
      
      updateSelectInput(session = session, inputId = 'k.ibd', label = "Input # of Plots per IBlock:",
                        choices = k, selected = k[1])
      
    })
    
    #shinybusy::show_modal_spinner() 
    IBD_reactive <- reactive({
      
      req(input$r.ibd)
      req(input$k.ibd)
      req(input$myseed.ibd)
      req(input$plot_start.ibd)
      req(input$Location.ibd)
      req(input$l.ibd)
      r.ibd <- as.numeric(input$r.ibd)
      k.ibd <- as.numeric(input$k.ibd)
      plot_start.ibd <- as.vector(unlist(strsplit(input$plot_start.ibd, ",")))
      plot_start.ibd <- as.numeric(plot_start.ibd)
      loc <-  as.vector(unlist(strsplit(input$Location.ibd, ",")))
      seed.rcbd <- as.numeric(input$myseed.ibd)
      
      if (input$owndataibd == "Yes") {
        req(get_tIBD()$t_ibd)
        t.ibd <- as.numeric(get_tIBD()$t_ibd)
        data.ibd <- getData.ibd()$dataUp.ibd
      }else {
        req(input$t.ibd)
        t.ibd <- as.numeric(input$t.ibd)
        data.ibd <- NULL
      }
      seed.ibd <- as.numeric(input$myseed.ibd)
      l.ibd <- as.numeric(input$l.ibd)
      
      if (r.ibd < 2) validate("Incomplete Blocks Design needs at least 2 replicates.")
      
      incomplete_blocks(t = t.ibd, k = k.ibd, r = r.ibd, l = l.ibd, plotNumber = plot_start.ibd,
                        seed = seed.ibd,
                        locationNames = loc,
                        data = data.ibd) 
      
    })
    
    output$well_panel_layout_IBD <- renderUI({
      req(IBD_reactive()$fieldBook)
      obj_ibd <- IBD_reactive()
      planting_ibd <- input$planter_mov_ibd
      allBooks_ibd<- plot_layout(x = obj_ibd, optionLayout = 1)$newBooks
      nBooks_ibd <- length(allBooks_ibd)
      layoutOptions_ibd <- 1:nBooks_ibd
      #loc <-  as.vector(unlist(strsplit(input$Location.ibd, ",")))
      site <- as.numeric(input$l.ibd)
      wellPanel(
        fluidRow(
          column(2,
                 radioButtons(ns("typlotibd"), "Type of Plot:",
                              c("Entries/Treatments" = 1,
                                "Plots" = 2,
                                "Heatmap" = 3), selected = 1)
          ),
          column(3, #align="center",
                 selectInput(inputId = ns("layoutO_ibd"), label = "Layout option:", choices = layoutOptions_ibd)
          ),
          column(3, #align="center",
                 selectInput(inputId = ns("locLayout_ibd"), label = "Location:", choices = 1:site)
          )
        )
      )
    })
    
    reactive_layoutIBD <- reactive({
      req(input$layoutO_ibd)
      req(IBD_reactive())
      obj_ibd <- IBD_reactive()
      opt_ibd <- as.numeric(input$layoutO_ibd)
      planting_ibd <- input$planter_mov_ibd
      plot_layout(x = obj_ibd, optionLayout = opt_ibd, planter = planting_ibd)
    })
    


    
    valsIBD <- reactiveValues(maxV.ibd = NULL, minV.ibd = NULL, trail.ibd = NULL)
    
    simuModal.ibd <- function(failed = FALSE) {
      modalDialog(
        selectInput(inputId = ns("trailsIBD"), label = "Select One:", choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        conditionalPanel("input.trailsIBD == 'Other'", ns = ns,
                         textInput(inputId = ns("OtherIBD"), label = "Input Trial Name:", value = NULL)
        ),
        fluidRow(
          column(6, 
                 numericInput(inputId = ns("min.ibd"), "Input the min value", value = NULL)
          ),
          column(6, 
                 numericInput(inputId = ns("max.ibd"), "Input the max value", value = NULL)
                 
          )
          
        ),
        
        if (failed)
          div(tags$b("Invalid input of data max and min", style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(inputId = ns("ok.ibd"), "GO")
        )
        
      )
      
    }
    
    observeEvent(input$Simulate.ibd, {
      req(IBD_reactive()$fieldBook)
      showModal(
        shinyjqui::jqui_draggable(
          simuModal.ibd()
        )
      )
    })
    
    observeEvent(input$ok.ibd, {
      req(input$max.ibd, input$min.ibd)
      if (input$max.ibd > input$min.ibd && input$min.ibd != input$max.ibd) {
        valsIBD$maxV.ibd <- input$max.ibd
        valsIBD$minV.ibd <- input$min.ibd
        if(input$trailsIBD == "Other") {
          req(input$OtherIBD)
          if(!is.null(input$OtherIBD)) {
            valsIBD$trail.ibd <- as.character(input$OtherIBD)
          }else showModal(simuModal.ibd(failed = TRUE))
        }else {
          valsIBD$trail.ibd <- as.character(input$trailsIBD)
        }
        removeModal()
      }else {
        showModal(
          shinyjqui::jqui_draggable(
            simuModal.ibd(failed = TRUE)
          )
        )
      }
    })
    
    
    simuDataIBD <- reactive({
      req(IBD_reactive()$fieldBook)
      if(!is.null(valsIBD$maxV.ibd) && !is.null(valsIBD$minV.ibd) && !is.null(valsIBD$trail.ibd)) {
        max <- as.numeric(valsIBD$maxV.ibd)
        min <- as.numeric(valsIBD$minV.ibd)
        #df.ibd <- IBD_reactive()$fieldBook
        #df.ibd <- reactive_layoutIBD()$fieldBookXY
        df.ibd <- reactive_layoutIBD()$allSitesFieldbook
        cnamesdf.ibd <- colnames(df.ibd)
        df.ibd <- norm_trunc(a = min, b = max, data = df.ibd)
        colnames(df.ibd) <- c(cnamesdf.ibd[1:(ncol(df.ibd) - 1)], valsIBD$trail.ibd)
        a <- ncol(df.ibd)
      }else {
        #df.ibd <-  IBD_reactive()$fieldBook
        #df.ibd <- reactive_layoutIBD()$fieldBookXY
        df.ibd <- reactive_layoutIBD()$allSitesFieldbook
        a <- ncol(df.ibd)
      }
      return(list(df = df.ibd, a = a))
    })
    
    heatmapInfoModal_IBD <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    output$tabsetIBD <- renderUI({
      req(input$typlotibd)
      tabsetPanel(
        if (input$typlotibd != 3) {
          tabPanel("Incomplete Block Field Layout", shinycssloaders::withSpinner(plotOutput(ns("layout.output"), width = "100%", height = "650px"),
                                                                              type = 5))
        } else {
          tabPanel("Incomplete Block Field Layout", shinycssloaders::withSpinner(plotly::plotlyOutput(ns("heatmapIBD"), width = "100%", height = "650px"),
                                                                              type = 5))
        },
        tabPanel("Incomplete Block Field Book", shinycssloaders::withSpinner(DT::DTOutput(ns("IBD.output")), type = 5))
      )
      
    })
    
    locNum <- reactive(
      return(as.numeric(input$locLayout_ibd))
    )
    
    heatmap_obj <- reactive({
      req(simuDataIBD()$df)
      if (ncol(simuDataIBD()$df) == 10) {
        locs <- factor(simuDataIBD()$df$LOCATION, levels = unique(simuDataIBD()$df$LOCATION))
        locLevels <- levels(locs)
        df = subset(simuDataIBD()$df, LOCATION == locLevels[locNum()])
        loc <- levels(factor(df$LOCATION))
        trail <- as.character(valsIBD$trail.ibd)
        label_trail <- paste(trail, ": ")
        heatmapTitle <- paste("Heatmap for ", trail)
        new_df <- df %>%
          dplyr::mutate(text = paste0("Site: ", loc, "\n", "Row: ", df$ROW, "\n", "Col: ", df$COLUMN, "\n", "Entry: ", 
                                      df$ENTRY, "\n", label_trail, round(df[,10],2)))
        w <- as.character(valsIBD$trail.ibd)
        new_df$ROW <- as.factor(new_df$ROW) # Set up ROWS as factors
        new_df$COLUMN <- as.factor(new_df$COLUMN) # Set up COLUMNS as factors
        p1 <- ggplot2::ggplot(new_df, ggplot2::aes(x = new_df[,5], y = new_df[,4], fill = new_df[,10], text = text)) +
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
            heatmapInfoModal_IBD()
          )
        )
        return(NULL)
      }
    })
    
    output$heatmapIBD <- plotly::renderPlotly({
      req(heatmap_obj())
      heatmap_obj()
    })
    
    
    output$layout.output <- renderPlot({
      req(reactive_layoutIBD())
      req(IBD_reactive())
      req(input$typlotibd)
      if (input$typlotibd == 1) {
        reactive_layoutIBD()$out_layout
      } else if (input$typlotibd == 2) {
        reactive_layoutIBD()$out_layoutPlots
      }
    })
    
    output$IBD.output <- DT::renderDataTable({
      req(input$k.ibd)
      k.ibd <- input$k.ibd
      if (k.ibd == "No Options Available") {
        validate("No options for these amout of treatments ):")
      }
      req(simuDataIBD()$df)
      df <- simuDataIBD()$df
      a <- as.numeric(simuDataIBD()$a)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      
      DT::datatable(df, rownames = FALSE, options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ))
    })
    
    
    output$downloadData.ibd <- downloadHandler(
      filename = function() {
        loc <- paste("IBD_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(simuDataIBD()$df)
        write.csv(df, file, row.names = FALSE)
      }
    )
    
  })
}

## To be copied in the UI
# mod_IBD_ui("IBD_ui_1")

## To be copied in the server
# mod_IBD_server("IBD_ui_1")