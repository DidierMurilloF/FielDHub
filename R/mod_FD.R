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
                   
                   numericInput(inputId = ns("seed.fd"), label = "Random Seed:",
                                value = 123, min = 1),
                   fluidRow(
                     column(6,
                            actionButton(
                              inputId = ns("RUN.fd"), "Run!", 
                              icon = icon("circle-nodes", verify_fa = FALSE),
                              width = '100%'),
                     ),
                     column(6,
                            actionButton(
                              ns("Simulate.fd"), "Simulate!", 
                              icon = icon("greater-than-equal", verify_fa = FALSE),
                              width = '100%'),
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
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(downloadButton(ns("downloadCsv.fd"), 
                                                    label =  "Excel",
                                                    icon = icon("file-csv"), 
                                                    width = '10%',
                                                    style="color: #337ab7; background-color: #fff; border-color: #2e6da4")),
                     shinycssloaders::withSpinner(
                       plotly::plotlyOutput(ns("layouts"), width = "97%", 
                                            height = "550px"), type = 5
                     ),
                     br(),
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
    
    shinyjs::useShinyjs()
    
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
          entriesInfoModal_FD()
        )
      }
    })
    
    get_data_factorial <- reactive({
      
      if (input$owndata == "Yes") {
        req(input$file.FD)
        req(input$sep.fd)
        inFile <- input$file.FD
        
        data_ingested <- load_file(name = inFile$name,
          path = inFile$datapat,
          sep = input$sep.fd,
          check = TRUE, 
          design = "factorial")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_up <- as.data.frame(data_up[,1:2])
          data_factorial <- na.omit(data_up)
          colnames(data_factorial) <- c("FACTOR", "LEVEL")
          set_factors <- factor(data_factorial$FACTOR, as.character(unique(data_factorial$FACTOR)))
          set_factors.fd <- levels(set_factors)
          nt <- length(set_factors.fd)
          if (nt < 2) {
            shinyalert::shinyalert(
              "Error!!", 
              "More than one factor needs to be specified.", 
              type = "error")
            return(NULL)
          }
          return(list(data_fd = data_factorial, treatments = set_factors.fd))
        } else if (names(data_ingested) == "bad_format") {
          shinyalert::shinyalert(
            "Error!!", 
            "Invalid file; Please upload a .csv file.", 
            type = "error")
          return(NULL)
        } else if (names(data_ingested) == "duplicated_vals") {
          shinyalert::shinyalert(
            "Error!!", 
            "Check input file for duplicate values.", 
            type = "error")
          return(NULL)
        } else if (names(data_ingested) == "missing_cols") {
          shinyalert::shinyalert(
            "Error!!", 
            "Data input needs at least two column: FACTOR and LEVEL", 
            type = "error")
          return(NULL)
        }
      } else {
        req(input$setfactors)
        reps <- as.numeric(input$reps.fd)
        setfactors.fd <- as.numeric(as.vector(unlist(strsplit(input$setfactors, ","))))
        nt <- length(setfactors.fd)
        if (nt < 2) {
          shinyalert::shinyalert(
            "Error!!", 
            "More than one factor needs to be specified.", 
            type = "error")
          return(NULL)
        }
        TRT <- rep(LETTERS[1:nt], each = reps)
        newlevels <- get.levels(k = setfactors.fd)
        data_fd <- data.frame(
          list(
            factors = rep(levels(as.factor(TRT)), times = setfactors.fd),
            levels = unlist(newlevels)
          )
        )
        colnames(data_fd) <- c("factors", "levels")
        return(list(data_fd = data_fd, treatments = setfactors.fd))
      }
    }) |> 
      bindEvent(input$RUN.fd)
    
    fd_inputs <- reactive({
      req(get_data_factorial())
      req(input$plot_start.fd)
      req(input$Location.fd)
      req(input$l.fd)
      req(input$seed.fd)
      req(input$kindFD)
      req(input$planter_mov_fd)
      
      setfactors.fd <- get_data_factorial()$treatments
      plot_start.fd <- as.vector(unlist(strsplit(input$plot_start.fd, ",")))
      plot_start <- as.numeric(plot_start.fd)
      planter <- input$planter_mov_fd
      site_names <-  as.vector(unlist(strsplit(input$Location.fd, ",")))
      seed <- as.numeric(input$seed.fd)
      reps <- as.numeric(input$reps.fd)
      sites <- as.numeric(input$l.fd)
      type_design <- input$kindFD
      
      return(
        list(
        set_factors = setfactors.fd,
        r = reps,
        planter = planter,
        plot_start = plot_start,
        sites = sites,
        site_names = site_names,
        type_design = type_design,
        seed = seed))
    }) |>
      bindEvent(input$RUN.fd)
    
    
    fd_reactive <- reactive({
      
      req(get_data_factorial())
      
      shinyjs::show(id = "downloadCsv.fd")
      
      if (fd_inputs()$type_design == "FD_CRD") {
        type_design <- 1
      } else type_design <- 2
      
      full_factorial(
        reps = fd_inputs()$r, 
        l = fd_inputs()$sites, 
        type = type_design, 
        planter = fd_inputs()$planter,
        plotNumber = fd_inputs()$plot_start, 
        seed = fd_inputs()$seed, 
        locationNames = fd_inputs()$site_names,
        data = get_data_factorial()$data_fd
      ) 
      
    }) |> 
      bindEvent(input$RUN.fd)
    
    
    upDateSites <- reactive({
      req(input$l.fd)
      locs <- as.numeric(input$l.fd)
      sites <- 1:locs
      return(list(sites = sites))
    })  |> 
      bindEvent(input$RUN.fd)
    
    output$well_panel_layout_FD <- renderUI({
      req(fd_reactive()$fieldBook)
      obj_fd <- fd_reactive()
      allBooks_fd <- plot_layout(x = obj_fd, layout = 1, 
                                 stacked = "vertical")$newBooks
      nBooks_fd <- length(allBooks_fd)
      layoutOptions_fd <- 1:nBooks_fd
      stacked_fd <- c("Vertical Stack Panel" = "vertical", 
                        "Horizontal Stack Panel" = "horizontal")
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
                 selectInput(inputId = ns("stackedFD"), 
                             label = "Reps layout:", 
                             choices = stacked_fd),
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
    
    observeEvent(input$stackedFD, {
      req(input$stackedFD)
      req(input$l.fd)
      obj_fd <- fd_reactive()
      allBooks <- plot_layout(x = obj_fd, layout = 1, 
                              stacked = input$stackedFD)$newBooks
      nBooks <- length(allBooks)
      NewlayoutOptions <- 1:nBooks
      updateSelectInput(session = session, inputId = 'layoutO_fd',
                        label = "Layout option:",
                        choices = NewlayoutOptions,
                        selected = 1
      )
    })
    
    reset_selection <- reactiveValues(reset = 0)

    observeEvent(input$stackedFD, {
      reset_selection$reset <- 1
    })

    observeEvent(input$layoutO_fd, {
      reset_selection$reset <- 0
    })
    
    reactive_layoutFD <- reactive({
      req(input$layoutO_fd)
      req(fd_reactive())
      obj_fd <- fd_reactive()
      planting_fd <- fd_inputs()$planter
      
      if (reset_selection$reset == 1) {
        opt_fd <- 1
      } else opt_fd <- as.numeric(input$layoutO_fd)
      
      locSelected <- as.numeric(input$locLayout_fd)
      try(plot_layout(x = obj_fd, layout = opt_fd, 
                      stacked = input$stackedFD,
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
        simuModal.fd()
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
          simuModal.fd(failed = TRUE)
        )
      }
    })
    
    simuData_fd <- reactive({
      req(fd_inputs()$seed)
      req(fd_reactive()$fieldBook)
      if(!is.null(valsfd$maxV.fd) && !is.null(valsfd$minV.fd) && !is.null(valsfd$trail.fd)) {
        max <- as.numeric(valsfd$maxV.fd)
        min <- as.numeric(valsfd$minV.fd)
        df.fd <- reactive_layoutFD()$allSitesFieldbook
        cnamesdf.fd <- colnames(df.fd)
        df.fd <- norm_trunc(
          a = min, 
          b = max, 
          data = df.fd, 
          seed = fd_inputs()$seed
        )
        colnames(df.fd) <- c(cnamesdf.fd[1:(ncol(df.fd) - 1)], valsfd$trail.fd)
        a <- ncol(df.fd)
      } else {
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
        new_df <- df |>
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
          ggplot2::theme(plot.title = ggplot2::element_text(
            family="Calibri", face="bold", size=13, hjust=0.5)
            )
        
        p2 <- plotly::ggplotly(p1, tooltip="text", height = 560)
        return(p2)
      } else {
        showModal(
          heatmapInfoModal_fd()
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
      colnames_set <- colnames(df)
      
      len_colnames_set <- length(colnames_set)
      if (colnames_set[len_colnames_set] == "TRT_COMB") {
        df[, 7:len_colnames_set] <- lapply(df[, 7:len_colnames_set], as.factor)
      } else {
        df[, 7:(len_colnames_set - 1)] <- lapply(df[, 7:(len_colnames_set - 1)], as.factor)
      }
      
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
    
    csv_data <- reactive({
      req(simuData_fd()$df)
      df <- simuData_fd()$df
      req(input$typlotfd)
      if (input$typlotfd == 2) {
        export_layout(df, locNum(), TRUE)
      } else {
        export_layout(df, locNum())
      }
    })
    
    
    # Downloadable csv of selected dataset ----
    output$downloadCsv.fd <- downloadHandler(
      filename = function() {
        loc <- paste("Factorial_Layout", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(csv_data()$file)
        write.csv(df, file, row.names = FALSE)
      }
    )
    
  })
}
