#FD first try

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
                   radioButtons(inputId = ns("owndata"), 
                                label = "Import entries' list?", 
                                choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, 
                                choiceNames = NULL, choiceValues = NULL),
                   selectInput(inputId = ns("kindFD"), 
                               label = "Select a Factorial Design Type:",
                               choices = c("Factorial in a RCBD" = "FD_RCBD", 
                                           "Factorial in a CRD" = "FD_CRD"),
                               multiple = FALSE),
                   
                   conditionalPanel("input.owndata != 'Yes'", ns = ns,
                                    textInput(inputId = ns("setfactors"), 
                                              label = "Input # of Entries for Each Factor: (Separated by Comma)",
                                              value = "2,2,3")     
                   ),
                   conditionalPanel("input.owndata == 'Yes'", ns = ns,
                                    fluidRow(
                                      column(8, style=list("padding-right: 28px;"),
                                             fileInput(ns("file.FD"), 
                                                       label = "Upload a CSV File:", 
                                                       multiple = FALSE)),
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
                                         value = 3, min = 2)
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
                   selectInput(inputId = ns("planter_mov_fd"), label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"), multiple = FALSE,
                               selected = "serpentine"),
                   
                   numericInput(inputId = ns("myseed.reps"), label = "Seed Number:",
                                value = 123, min = 1),
                   fluidRow(
                     column(6,
                            actionButton(inputId = ns("RUN.fd"), "Run!", 
                                         icon = icon("cocktail"), width = '100%'),
                     ),
                     column(6,
                            actionButton(ns("Simulate.fd"), "Simulate!", 
                                         icon = icon("cocktail"), width = '100%')
                     )
                     
                   ), 
                   br(),
                   downloadButton(ns("downloadData.fd"), "Save Experiment!", 
                                  style = "width:100%")
      ),
      
      mainPanel(
        width = 8,
        fluidRow(
          tabsetPanel(
            tabPanel("Field Layout",
                     shinycssloaders::withSpinner(
                       plotly::plotlyOutput(ns("layouts"), width = "98%", 
                                            height = "650px"),type = 5
                     ),
                     column(12, uiOutput(ns("well_panel_layout_FD")))
            ),
            tabPanel("Field Book", 
                     shinycssloaders::withSpinner(DT::DTOutput(ns("FD.Output")), type = 5)
            )
          )
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
    
    fd_reactive <- eventReactive(input$RUN.fd, {
      
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
      
      myfd <- full_factorial(setfactors = setfactors.fd, reps = reps.fd, 
                             l = l.fd, type = type, plotNumber = plot_start.fd, 
                             seed = seed.fd, locationNames = loc,
                             data = data.fd) 
      
    })
    
    upDateSites <- eventReactive(input$RUN.fd, {
      req(input$l.fd)
      locs <- as.numeric(input$l.fd)
      sites <- 1:locs
      return(list(sites = sites))
    })
    
    output$well_panel_layout_FD <- renderUI({
      req(fd_reactive()$fieldBook)
      obj_fd <- fd_reactive()
      allBooks_fd <- plot_layout(x = obj_fd, optionLayout = 1, 
                                 orderReps = "vertical_stack_panel")$newBooks
      nBooks_fd <- length(allBooks_fd)
      layoutOptions_fd <- 1:nBooks_fd
      orderReps_fd <- c("Vertical Stack Panel" = "vertical_stack_panel", 
                        "Horizontal Stack Panel" = "horizontal_stack_panel")
      sites <- as.numeric(input$l.fd)
      wellPanel(
        column(2,
               radioButtons(ns("typlotfd"), "Type of Plot:",
                            c("Entries/Treatments" = 1,
                              "Plots" = 2,
                              "Heatmap" = 3), selected = 1)
        ),
        fluidRow(
          column(3,
                 selectInput(inputId = ns("orderRepsFD"), 
                             label = "Reps layout:", 
                             choices = orderReps_fd),
          ),
          column(3, #align="center",
                 selectInput(inputId = ns("layoutO_fd"), 
                             label = "Layout option:", 
                             choices = layoutOptions_fd)
          ),
          column(3, #align="center",
                 selectInput(inputId = ns("locLayout_fd"), label = "Location:", 
                             choices = as.numeric(upDateSites()$sites), 
                             selected = 1)
          )
        )
      )
    })
    
    observeEvent(input$orderRepsFD, {
      req(input$orderRepsFD)
      req(input$l.fd)
      obj_fd <- fd_reactive()
      allBooks <- plot_layout(x = obj_fd, optionLayout = 1, 
                              orderReps = input$orderRepsFD)$newBooks
      nBooks <- length(allBooks)
      NewlayoutOptions <- 1:nBooks
      updateSelectInput(session = session, inputId = 'layoutO_fd',
                        label = "Layout option:",
                        choices = NewlayoutOptions,
                        selected = 1
      )
    })
    
    reactive_layoutFD <- reactive({
      req(input$layoutO_fd)
      req(fd_reactive())
      obj_fd <- fd_reactive()
      opt_fd <- as.numeric(input$layoutO_fd)
      planting_fd <- input$planter_mov_fd
      locSelected <- as.numeric(input$locLayout_fd)
      try(plot_layout(x = obj_fd, optionLayout = opt_fd, 
                      orderReps = input$orderRepsFD,
                      planter = planting_fd , 
                      l = locSelected), silent = TRUE)
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
      set.seed(input$myseed.reps)
      req(fd_reactive()$fieldBook)
      if(!is.null(valsfd$maxV.fd) && !is.null(valsfd$minV.fd) && !is.null(valsfd$trail.fd)) {
        max <- as.numeric(valsfd$maxV.fd)
        min <- as.numeric(valsfd$minV.fd)
        df.fd <- reactive_layoutFD()$allSitesFieldbook
        cnamesdf.fd <- colnames(df.fd)
        df.fd <- norm_trunc(a = min, b = max, data = df.fd)
        colnames(df.fd) <- c(cnamesdf.fd[1:(ncol(df.fd) - 1)], valsfd$trail.fd)
        a <- ncol(df.fd)
      }else {
        df.fd <- reactive_layoutFD()$allSitesFieldbook
        a <- ncol(df.fd)
      }
      return(list(df = df.fd, a = a))
    })
    
    heatmapInfoModal_fd <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    kindNum <- reactive({
      req(input$setfactors)
      setfactors.fd <- fd_reactive()$infoDesign$levels_each_factor
      lengthfactors <- length(setfactors.fd)
      end_columns <- lengthfactors + 7
      return(end_columns)
    }
    )
    
    locNum <- reactive(
      return(as.numeric(input$locLayout_fd))
    )
    
    heatmap_obj <- reactive({
      req(simuData_fd()$df)
      if (ncol(simuData_fd()$df) == (kindNum() + 1)) {
        locs <- factor(simuData_fd()$df$LOCATION, levels = unique(simuData_fd()$df$LOCATION))
        locLevels <- levels(locs)
        df = subset(simuData_fd()$df, LOCATION == locLevels[locNum()])
        loc <- levels(factor(df$LOCATION))
        trail <- as.character(valsfd$trail.fd)
        label_trail <- paste(trail, ": ")
        heatmapTitle <- paste("Heatmap for ", trail)
        new_df <- df %>%
          dplyr::mutate(text = paste0("Site: ", loc, "\n", 
                                      "Row: ", df$ROW, "\n", 
                                      "Col: ", df$COLUMN, "\n", 
                                      "Entry: ", df$ENTRY, "\n", 
                                      label_trail, round(df[,(kindNum() + 1)],2)))
        w <- as.character(valsfd$trail.fd)
        new_df$ROW <- as.factor(new_df$ROW) # Set up ROWS as factors
        new_df$COLUMN <- as.factor(new_df$COLUMN) # Set up COLUMNS as factors
        p1 <- ggplot2::ggplot(new_df, 
                              ggplot2::aes(
                                x = new_df[,5], 
                                y = new_df[,4], 
                                fill = new_df[,(kindNum() + 1)], 
                                text = text)) +
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
            heatmapInfoModal_fd()
          )
        )
        return(NULL)
      }
    })
    
    output$layouts <- plotly::renderPlotly({
      req(reactive_layoutFD())
      req(fd_reactive())
      req(input$typlotfd)
      if (input$typlotfd == 1) {
        reactive_layoutFD()$out_layout
      } else if (input$typlotfd == 2) {
        reactive_layoutFD()$out_layoutPlots
      } else {
        req(heatmap_obj())
        heatmap_obj()
      }
    })
    
    output$FD.Output <- DT::renderDataTable({
      df <- simuData_fd()$df
      df$LOCATION <- as.factor(df$LOCATION)
      df$PLOT <- as.factor(df$PLOT)
      df$ROW <- as.factor(df$ROW)
      df$COLUMN <- as.factor(df$COLUMN)
      df$REP <- as.factor(df$REP)
      df$FACTOR_A <- as.factor(df$FACTOR_A)
      df$FACTOR_B <- as.factor(df$FACTOR_B)
      if ("FACTOR_C" %in% colnames(df)) df$FACTOR_C <- as.factor(df$FACTOR_C)
      if ("FACTOR_D" %in% colnames(df)) df$FACTOR_D <- as.factor(df$FACTOR_D)
      df$TRT_COMB <- as.factor(df$TRT_COMB)
      a <- as.numeric(simuData_fd()$a)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      
      DT::datatable(df, filter = 'top', rownames = FALSE, options = list(
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
