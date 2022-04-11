#' SPD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_SPD_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Split-Plot Design"),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        radioButtons(inputId = ns("owndataSPD"), 
                     label = "Import entries' list?", 
                     choices = c("Yes", "No"), 
                     selected = "No",
                     inline = TRUE, 
                     width = NULL, 
                     choiceNames = NULL, 
                     choiceValues = NULL),
        selectInput(inputId = ns("kindSPD"), 
                    label = "Select SPD Type:",
                    choices = c("Split-Plot in a RCBD" = "SPD_RCBD", 
                                "Split-Plot in a CRD" = "SPD_CRD"),
                    multiple = FALSE),
        
        conditionalPanel("input.owndataSPD == 'Yes'", ns = ns,
                         fluidRow(
                           column(8, style=list("padding-right: 28px;"),
                                  fileInput(ns("file.SPD"), 
                                            label = "Upload a csv File:", 
                                            multiple = FALSE)),
                           column(4,style=list("padding-left: 5px;"),
                                  radioButtons(ns("sep.spd"), "Separator",
                                               choices = c(Comma = ",",
                                                           Semicolon = ";",
                                                           Tab = "\t"),
                                               selected = ","))
                         )
        ),
        conditionalPanel("input.owndataSPD != 'Yes'", ns = ns,
                          numericInput(ns("mp.spd"), 
                                       label = "Whole-plots:",
                                       value = 4, min = 2),
                          numericInput(ns("sp.spd"), 
                                       label = "Sub-plots Within Whole-plots:",
                                       value = 3, min = 2)
        ),
        
        fluidRow(
          column(6, style=list("padding-right: 28px;"),
                 numericInput(ns("reps.spd"), label = "Input # of Full Reps:",
                              value = 3, min = 2), 
          ),
          column(6,style=list("padding-left: 5px;"),
                 numericInput(ns("l.spd"), label = "Input # of Locations:",
                              value = 1, min = 1)
          )
        ),
        fluidRow(
          column(6, style=list("padding-right: 28px;"),
                 textInput(ns("plot_start.spd"), "Starting Plot Number:", 
                           value = 101)
          ),
          column(6,style=list("padding-left: 5px;"),
                 textInput(ns("Location.spd"), "Input the Location:", 
                           value = "FARGO")
          )
        ),
        selectInput(inputId = ns("planter_mov_spd"), 
                    label = "Plot Order Layout:",
                    choices = c("serpentine", "cartesian"), 
                    multiple = FALSE,
                    selected = "serpentine"),
        
        numericInput(inputId = ns("myseed.spd"), label = "Seed Number:", 
                     value = 118, min = 1),
        
        fluidRow(
          column(6,
                 actionButton(inputId = ns("RUN.spd"), "Run!", 
                              icon = icon("cocktail"), width = '100%'),
          ),
          column(6,
                 actionButton(ns("Simulate.spd"), "Simulate!", 
                              icon = icon("cocktail"), width = '100%')
          )
          
        ), 
        br(),
        downloadButton(ns("downloadData.spd"), "Save Experiment!", 
                      style = "width:100%")
      ),
      
      mainPanel(
        width = 8,
        fluidRow(
          tabsetPanel(
            tabPanel("Field Layout",
                     shinycssloaders::withSpinner(
                       plotly::plotlyOutput(ns("layouts"), 
                                            width = "97%", 
                                            height = "650px"),
                       type = 5
                     ),
                     column(12,uiOutput(ns("well_panel_layout_SPD")))
            ),
            tabPanel("Field Book", 
                     shinycssloaders::withSpinner(
                       DT::DTOutput(ns("SPD.output")), 
                       type = 5
                       )
            )
          )
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
    
    wp <- c("NFung", paste("Fung", 1:4, sep = "")) 
    sp <- paste("Beans", 1:10, sep = "")            
    entryListFormat_SPD <- data.frame(list(WHOLPLOT = c(wp, rep("", 5)), 
                                           SUBPLOT = sp))
    
    entriesInfoModal_SPD<- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        renderTable(entryListFormat_SPD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        #h4("Note that reps might be unbalanced."),
        easyClose = FALSE
      )
    }
    
    toListen <- reactive({
      list(input$owndataSPD)
    })
    
    observeEvent(toListen(), {
      if (input$owndataSPD == "Yes") {
        showModal(
          shinyjqui::jqui_draggable(
            entriesInfoModal_SPD()
          )
        )
      }
    })
    
    getData.spd <- reactive({
      req(input$file.SPD)
      inFile <- input$file.SPD
      dataUp.spd <- load_file(name = inFile$name, path = inFile$datapat, sep = input$sep.spd)
      return(list(dataUp.spd = dataUp.spd))
    })
    
    spd_reactive <- eventReactive(input$RUN.spd, {
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
    
    upDateSites <- eventReactive(input$RUN.spd, {
      req(input$l.spd)
      locs <- as.numeric(input$l.spd)
      sites <- 1:locs
      return(list(sites = sites))
    })
    
    output$well_panel_layout_SPD <- renderUI({
      req(spd_reactive()$fieldBook)
      obj_spd <- spd_reactive()
      # planting_spd <- input$planter_mov_spd
      allBooks_spd<- plot_layout(x = obj_spd, optionLayout = 1, orderReps = "vertical_stack_panel")$newBooks
      nBooks_spd <- length(allBooks_spd)
      layoutOptions_spd <- 1:nBooks_spd
      sites <- as.numeric(input$l.spd)
      #loc <-  as.vector(unlist(strsplit(input$Location.spd, ",")))
      orderReps_spd <- c("Vertical Stack Panel" = "vertical_stack_panel", "Horizontal Stack Panel" = "horizontal_stack_panel")
      wellPanel(
        column(2,
               radioButtons(ns("typlotspd"), "Type of Plot:",
                            c("Entries/Treatments" = 1,
                              "Plots" = 2,
                              "Heatmap" = 3))
        ),
        fluidRow(
          column(3,
                 selectInput(inputId = ns("orderRepsSPD"), label = "Reps layout:", 
                             choices = orderReps_spd),
          ),
          column(2, #align="center",
                 selectInput(inputId = ns("layoutO_spd"), label = "Layout option:", choices = layoutOptions_spd)
          ),
          column(2, #align="center",
                 selectInput(inputId = ns("locLayout_spd"), label = "Location:", choices = as.numeric(upDateSites()$sites))
          )
        )
      )
    })
    
    observeEvent(input$orderRepsSPD, {
      req(input$orderRepsSPD)
      req(input$l.spd)
      obj_spd <- spd_reactive()
      allBooks <- plot_layout(x = obj_spd, optionLayout = 1, orderReps = input$orderRepsSPD)$newBooks
      nBooks <- length(allBooks)
      NewlayoutOptions <- 1:nBooks
      updateSelectInput(session = session, inputId = 'layoutO_spd',
                        label = "Layout option:",
                        choices = NewlayoutOptions,
                        selected = 1
      )
    })
    
    reactive_layoutSPD <- reactive({
      req(input$layoutO_spd)
      req(input$orderRepsSPD)
      req(spd_reactive())
      obj_spd <- spd_reactive()
      opt_spd <- as.numeric(input$layoutO_spd)
      planting_spd <- input$planter_mov_spd
      locSelected_spd <- as.numeric(input$locLayout_spd)
      try(plot_layout(x = obj_spd, optionLayout = opt_spd, planter = planting_spd, orderReps = input$orderRepsSPD,
                      l = locSelected_spd), silent = TRUE)
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
      set.seed(input$myseed.spd)
      req(spd_reactive()$fieldBook)
      
      if(!is.null(valspd$maxV.spd) && !is.null(valspd$minV.spd) && !is.null(valspd$trail.spd)) {
        max <- as.numeric(valspd$maxV.spd)
        min <- as.numeric(valspd$minV.spd)
        df.spd <- reactive_layoutSPD()$allSitesFieldbook
        cnamesdf.spd <- colnames(df.spd)
        df.spd <- norm_trunc(a = min, b = max, data = df.spd)
        colnames(df.spd) <- c(cnamesdf.spd[1:(ncol(df.spd) - 1)], valspd$trail.spd)
        df.spd <- df.spd[order(df.spd$ID),]
      }else {
        df.spd <- reactive_layoutSPD()$allSitesFieldbook
      }
      return(list(df = df.spd))
    })
    
    heatmapInfoModal_SPD <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    locNum <- reactive(
      return(as.numeric(input$locLayout_spd))
    )
    
    heatmap_obj <- reactive({
      req(simuData_spd()$df)
      if (ncol(simuData_spd()$df) == 10) {
        locs <- factor(simuData_spd()$df$LOCATION, levels = unique(simuData_spd()$df$LOCATION))
        locLevels <- levels(locs)
        df = subset(simuData_spd()$df, LOCATION == locLevels[locNum()])
        loc <- levels(factor(df$LOCATION))
        trail <- as.character(valspd$trail.spd)
        label_trail <- paste(trail, ": ")
        heatmapTitle <- paste("Heatmap for ", trail)
        new_df <- df %>%
          dplyr::mutate(text = paste0("Site: ", loc, "\n", "Row: ", df$ROW, "\n", "Col: ", df$COLUMN, "\n", "Entry: ", 
                                      df$ENTRY, "\n", label_trail, round(df[,10],2)))
        w <- as.character(valspd$trail.spd)
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
            heatmapInfoModal_SPD()
          )
        )
        return(NULL)
      }
    })
    
    output$layouts <- plotly::renderPlotly({
      req(reactive_layoutSPD())
      req(spd_reactive())
      req(input$typlotspd)
      if (input$typlotspd == 1) {
        reactive_layoutSPD()$out_layout
      } else if (input$typlotspd == 2) {
        reactive_layoutSPD()$out_layoutPlots
      } else {
        req(heatmap_obj())
        heatmap_obj()
      }
    })
    
    
    output$SPD.output <- DT::renderDataTable({
      
      df <- simuData_spd()$df
      df$LOCATION <- as.factor(df$LOCATION)
      df$PLOT <- as.factor(df$PLOT)
      df$ROW <- as.factor(df$ROW)
      df$COLUMN <- as.factor(df$COLUMN)
      df$REP <- as.factor(df$REP)
      df$WHOLE_PLOT <- as.factor(df$WHOLE_PLOT)
      df$SUB_PLOT <- as.factor(df$SUB_PLOT)
      df$TRT_COMB <- as.factor(df$TRT_COMB)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      
      DT::datatable(df, filter = 'top', rownames = FALSE, options = list(
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
