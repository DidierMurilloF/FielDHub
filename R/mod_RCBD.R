#' RCBD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_RCBD_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Randomized Complete Block Designs"),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        radioButtons(ns("owndatarcbd"), 
                     label = "Import entries' list?", 
                     choices = c("Yes", "No"), 
                     selected = "No",
                     inline = TRUE, 
                     width = NULL, 
                     choiceNames = NULL, 
                     choiceValues = NULL),
        conditionalPanel(
          condition = "input.owndatarcbd != 'Yes'", 
          ns = ns,
          numericInput(ns("t"), 
                       label = "Input # of Treatments:",
                       value = 18, 
                       min = 2)
        ),
        conditionalPanel(
          condition = "input.owndatarcbd == 'Yes'", 
          ns = ns,
          fluidRow(
            column(8, style=list("padding-right: 28px;"),
                   fileInput(inputId = ns("file.RCBD"), 
                             label = "Upload a CSV File:", 
                             multiple = FALSE)),
            column(4, style=list("padding-left: 5px;"),
                   radioButtons(inputId = ns("sep.rcbd"), "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","))
          )        
        ),
        
        numericInput(inputId = ns("b"), 
                     label = "Input # of Full Reps:", 
                     value = 3, min = 2),
        
        numericInput(inputId = ns("l.rcbd"), 
                     label = "Input # of Locations:", 
                     value = 1, 
                     min = 1),
        selectInput(inputId = ns("planter_mov_rcbd"), 
                    label = "Plot Order Layout:",
                    choices = c("serpentine", "cartesian"), 
                    multiple = FALSE,
                    selected = "serpentine"),
        fluidRow(
          column(6, style=list("padding-right: 28px;"),
                 textInput(inputId = ns("plot_start.rcbd"), 
                           "Starting Plot Number(s):", 
                           value = 101)
          ),
          column(6,style=list("padding-left: 5px;"),
                 checkboxInput(inputId = ns("continuous.plot"), 
                               label = "Continuous Plot ", 
                               value = TRUE),
          )
        ),
        
        textInput(inputId = ns("Location.rcbd"), 
                  "Input Location:",
                  value = "FARGO"),
        
        numericInput(inputId = ns("seed.rcbd"), 
                     label = "Random Seed:",
                     value = 123, 
                     min = 1),
        
        fluidRow(
          column(6,
                 actionButton(
                   inputId = ns("RUN.rcbd"), 
                   label = "Run!", 
                   icon = icon("circle-nodes", verify_fa = FALSE),
                   width = '100%'),
          ),
          column(6,
                 actionButton(
                   ns("Simulate.rcbd"), 
                   label = "Simulate!", 
                   icon = icon("greater-than-equal", verify_fa = FALSE),
                   width = '100%'),
          )
          
        ), 
        br(),
        downloadButton(ns("downloadData.rcbd"), 
                       "Save Experiment!", 
                       style = "width:100%")
                   
      ),

      mainPanel(
        width = 8,
        fluidRow(
          tabsetPanel(
            tabPanel("Field Layout",
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(
                       downloadButton(
                         ns("downloadCsv.rcbd"), 
                         label =  "Excel",
                         icon = icon("file-csv"), 
                         width = '10%',
                         style="color: #337ab7; background-color: #fff; border-color: #2e6da4")
                      ),
                     shinycssloaders::withSpinner(
                       plotly::plotlyOutput(
                         ns("layouts"), 
                         width = "97%", 
                         height = "550px"),
                       type = 5
                     ),
                     br(),
                     column(12,uiOutput(ns("well_panel_layout_RCBD")))
            ),
            tabPanel("Field Book", 
                     shinycssloaders::withSpinner(
                       DT::DTOutput(ns("RCBD_fieldbook")), 
                       type = 5)
            )
          )
        )
      )
    )
  )
}
#' RCBD Server Functions
#'
#' @noRd 
mod_RCBD_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    shinyjs::useShinyjs()

    get_data_rcbd <- reactive({
      if (input$owndatarcbd == "Yes") {
        req(input$file.RCBD)
        inFile <- input$file.RCBD
        data_ingested <- load_file(name = inFile$name, 
                                   path = inFile$datapat, 
                                   sep = input$sep.rcbd, 
                                   check = TRUE, 
                                   design = "rcbd")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_up <- as.data.frame(data_up[,1])
          data_rcbd <- na.omit(data_up)
          colnames(data_rcbd) <- "TREATMENT"
          nt <- nrow(data_rcbd)
          return(list(data_rcbd = data_rcbd, treatments = nt))
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
            "Data input needs at least one column: TREATMENT", 
            type = "error")
          return(NULL)
        }
      } else {
        req(input$t)
        nt <- as.numeric(input$t)
        df <- data.frame(list(TREATMENT = paste0("G-", 1:nt)))
        colnames(df) <- "TREATMENT"
        data_rcbd <- df
        return(list(data_rcbd = data_rcbd, treatments = nt))
      }
    }) %>%
      bindEvent(input$RUN.rcbd)
    
    rcbd_inputs <- reactive({
      
      req(get_data_rcbd())
      
      req(input$b)
      req(input$seed.rcbd)
      req(input$plot_start.rcbd)
      req(input$Location.rcbd)
      req(input$l.rcbd)
      req(input$planter_mov_rcbd)
      
      r <- as.numeric(input$b)
      treatments <- as.numeric(get_data_rcbd()$treatments)
      planter <- input$planter_mov_rcbd
      plot_start <- as.vector(unlist(strsplit(input$plot_start.rcbd, ",")))
      plot_start <- as.numeric(plot_start)
      site_names <-  as.vector(unlist(strsplit(input$Location.rcbd, ",")))
      seed <- as.numeric(input$seed.rcbd)
      sites <- as.numeric(input$l.rcbd)
      continuous <- input$continuous.plot

      return(list(
        r = r, 
        t = treatments, 
        planter = planter,
        plot_start = plot_start, 
        sites = sites,
        site_names = site_names,
        continuous = continuous,
        seed = seed)
        )
    }) %>%
      bindEvent(input$RUN.rcbd)
    
    
    entryListFormat_RCBD <- data.frame(
      TREATMENT = c(paste("TRT_", LETTERS[1:9], sep = ""))
      )
    entriesInfoModal_RCBD <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        renderTable(entryListFormat_RCBD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        h4("Note that only the TREATMENT column is required."),
        easyClose = FALSE
      )
    }
    
    toListen <- reactive({
      list(input$owndatarcbd)
    })
    
    observeEvent(toListen(), {
      if (input$owndatarcbd == "Yes") {
        showModal(
          shinyjqui::jqui_draggable(
            entriesInfoModal_RCBD()
          )
        )
      }
    })

        
    RCBD_reactive <- reactive({
      
      req(get_data_rcbd())
      req(rcbd_inputs())
      
      shinyjs::show(id = "downloadCsv.rcbd")
      
      RCBD(
        t = rcbd_inputs()$t, 
        reps = rcbd_inputs()$r, 
        l = rcbd_inputs()$sites, 
        plotNumber = rcbd_inputs()$plot_start, 
        continuous = rcbd_inputs()$continuous,
        planter = rcbd_inputs()$planter, 
        seed = rcbd_inputs()$seed, 
        locationNames = rcbd_inputs()$site_names, 
        data = get_data_rcbd()$data_rcbd
      )

    })  %>%
      bindEvent(input$RUN.rcbd)
    
    output$well_panel_layout_RCBD <- renderUI({
      req(RCBD_reactive()$fieldBook)
      obj_rcbd <- RCBD_reactive()
      allBooks_rcbd <- plot_layout(x = obj_rcbd, 
                                   layout = 1, 
                                   stacked = "vertical")$newBooks
      nBooks_rcbd <- length(allBooks_rcbd)
      layoutOptions_rcbd <- 1:nBooks_rcbd
      df <- RCBD_reactive()$fieldBook
      stacked_rcbd <- c("Vertical Stack Panel" = "vertical", 
                          "Horizontal Stack Panel" = "horizontal")
      sites <- length(levels(as.factor(df$LOCATION)))
      wellPanel(
        column(3,
               radioButtons(ns("typlotRCBD"), "Type of Plot:",
                            c("Entries/Treatments" = 1,
                              "Plots" = 2,
                              "Heatmap" = 3))
        ),
        fluidRow(
          column(3,
                 selectInput(inputId = ns("stackedRCBD"), 
                             label = "Reps layout:", 
                             choices = stacked_rcbd),
          ),
          column(2, 
                 selectInput(inputId = ns("layoutO_rcbd"), 
                             label = "Layout option:", 
                             choices = layoutOptions_rcbd, 
                             selected = 1)
          ),
          column(2, 
                 selectInput(inputId = ns("locLayout_rcbd"), 
                             label = "Location:", 
                             choices = 1:sites)
          )
        )
      )
    })
    
    observeEvent(input$stackedRCBD, {
      req(input$stackedRCBD)
      req(input$l.rcbd)
      obj_rcbd <- RCBD_reactive()
      allBooks <- try(plot_layout(x = obj_rcbd, 
                                  layout = 1, 
                                  stacked = input$stackedRCBD)$newBooks, 
                      silent = TRUE)
      nBooks <- length(allBooks)
      NewlayoutOptions <- 1:nBooks
      updateSelectInput(session = session, inputId = 'layoutO_rcbd',
                        label = "Layout option:",
                        choices = NewlayoutOptions,
                        selected = 1
      )
    })
    
    
    reset_selection <- reactiveValues(reset = 0)
    
    observeEvent(input$stackedRCBD, {
      reset_selection$reset <- 1
    })
    
    observeEvent(input$layoutO_rcbd, {
      reset_selection$reset <- 0
    })
    
    reactive_layoutRCBD <- reactive({
      req(input$stackedRCBD)
      req(input$layoutO_rcbd)
      req(input$locLayout_rcbd)
      req(RCBD_reactive())
      obj_rcbd <- RCBD_reactive()
      planting_rcbd <- rcbd_inputs()$planter
      
      if (reset_selection$reset == 1) {
        opt_rcbd <- 1
      } else opt_rcbd <- as.numeric(input$layoutO_rcbd)
      
      locSelected <- as.numeric(input$locLayout_rcbd)
      try(plot_layout(x = obj_rcbd, 
                      layout = opt_rcbd, 
                      stacked = input$stackedRCBD,
                      planter = planting_rcbd, 
                      l = locSelected), 
          silent = TRUE)
    })

    
    valsRCBD <- reactiveValues(maxV.rcbd = NULL, 
                               minV.rcbd = NULL, 
                               trail.rcbd = NULL)
    
    simuModal.rcbd <- function(failed = FALSE) {
      modalDialog(
        selectInput(inputId = ns("trailsRCBD"), 
                    label = "Select One:", 
                    choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        conditionalPanel(
          condition = "input.trailsRCBD == 'Other'", 
          ns = ns,
          textInput(inputId = ns("OtherRCBD"), 
                    label = "Input Trial Name:", 
                    value = NULL)
        ),
        fluidRow(
          column(6, 
                 numericInput(inputId = ns("min.rcbd"), 
                              "Input the min value", 
                              value = NULL)
          ),
          column(6, 
                 numericInput(inputId = ns("max.rcbd"), 
                              "Input the max value", 
                              value = NULL)
                 
          )
          
        ),
        
        if (failed)
          div(tags$b("Invalid input of data max and min", 
                     style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(inputId = ns("ok.rcbd"), "GO")
        )
        
      )
      
    }
    
    observeEvent(input$Simulate.rcbd, {
      req(RCBD_reactive()$fieldBook)
      showModal(
        shinyjqui::jqui_draggable(
          simuModal.rcbd()
        )
      )
    })
    
    observeEvent(input$ok.rcbd, {
      req(input$max.rcbd, input$min.rcbd)
      if (input$max.rcbd > input$min.rcbd && input$min.rcbd != input$max.rcbd) {
        valsRCBD$maxV.rcbd <- input$max.rcbd
        valsRCBD$minV.rcbd <- input$min.rcbd
        if(input$trailsRCBD == "Other") {
          req(input$OtherRCBD)
          if(!is.null(input$OtherRCBD)) {
            valsRCBD$trail.rcbd <- as.character(input$OtherRCBD)
          }else showModal(simuModal.rcbd(failed = TRUE))
        }else {
          valsRCBD$trail.rcbd <- as.character(input$trailsRCBD)
        }
        removeModal()
      }else {
        showModal(
          shinyjqui::jqui_draggable(
            simuModal.rcbd(failed = TRUE)
          )
        )
      }
    })
    
    
    simuDataRCBD <- reactive({
      req(RCBD_reactive()$fieldBook)
      if(!is.null(valsRCBD$maxV.rcbd) && !is.null(valsRCBD$minV.rcbd) && 
         !is.null(valsRCBD$trail.rcbd)) {
        max <- as.numeric(valsRCBD$maxV.rcbd)
        min <- as.numeric(valsRCBD$minV.rcbd)
        df.rcbd <- reactive_layoutRCBD()$allSitesFieldbook
        cnamesdf.rcbd <- colnames(df.rcbd)
        df.rcbd <- norm_trunc(
          a = min, 
          b = max, 
          data = df.rcbd, 
          seed = rcbd_inputs()$seed
        )
        colnames(df.rcbd) <- c(cnamesdf.rcbd[1:(ncol(df.rcbd) - 1)], 
                               valsRCBD$trail.rcbd)
        df.rcbd <- df.rcbd[order(df.rcbd$ID),]
      }else {
        df.rcbd <- reactive_layoutRCBD()$allSitesFieldbook
      }
      return(list(df = df.rcbd))
    })
    
    heatmapInfoModal_RCBD <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    locNum <- reactive(
      return(as.numeric(input$locLayout_rcbd))
    )
    
    heatmap_obj <- reactive({
      req(simuDataRCBD()$df)
      if (ncol(simuDataRCBD()$df) == 8) {
        locs <- factor(simuDataRCBD()$df$LOCATION, 
                       levels = unique(simuDataRCBD()$df$LOCATION))
        locLevels <- levels(locs)
        df = subset(simuDataRCBD()$df, LOCATION == locLevels[locNum()])
        loc <- levels(factor(df$LOCATION))
        trail <- as.character(valsRCBD$trail.rcbd)
        label_trail <- paste(trail, ": ")
        heatmapTitle <- paste("Heatmap for ", trail)
        new_df <- df %>%
          dplyr::mutate(text = paste0("Site: ", loc, "\n", 
                                      "Row: ", df$ROW, "\n", 
                                      "Col: ", df$COLUMN, "\n", 
                                      "Entry: ", df$ENTRY, "\n", 
                                      label_trail, round(df[,8],2)))
        w <- as.character(valsRCBD$trail.rcbd)
        new_df$ROW <- as.factor(new_df$ROW) # Set up ROWS as factors
        new_df$COLUMN <- as.factor(new_df$COLUMN) # Set up COLUMNS as factors
        p1 <- ggplot2::ggplot(
          new_df, ggplot2::aes(
            x = new_df[,5], 
            y = new_df[,4], 
            fill = new_df[,8], 
            text = text)) +
          ggplot2::geom_tile() +
          ggplot2::xlab("COLUMN") +
          ggplot2::ylab("ROW") +
          ggplot2::labs(fill = w) +
          viridis::scale_fill_viridis(discrete = FALSE) +
          ggplot2::ggtitle(heatmapTitle) +
          ggplot2::theme_minimal() + # I added this option 
          ggplot2::theme(plot.title = ggplot2::element_text(
            family="Calibri", 
            face="bold", 
            size=13, 
            hjust=0.5))
        p2 <- plotly::ggplotly(p1, 
                               tooltip="text", 
                               height = 560)
        return(p2)
      } else {
        showModal(
          shinyjqui::jqui_draggable(
            heatmapInfoModal_RCBD()
          )
        )
        return(NULL)
      }
    })

    output$layouts <- plotly::renderPlotly({
      req(reactive_layoutRCBD())
      req(RCBD_reactive())
      req(input$typlotRCBD)
      if (input$typlotRCBD == 1) {
        reactive_layoutRCBD()$out_layout
      } else if (input$typlotRCBD == 2) {
        reactive_layoutRCBD()$out_layoutPlots
      } else {
        req(heatmap_obj())
        heatmap_obj()
      }
    })
    
    output$RCBD_fieldbook <- DT::renderDataTable({
      df <- simuDataRCBD()$df
      df$LOCATION <- as.factor(df$LOCATION)
      df$PLOT <- as.factor(df$PLOT)
      df$ROW <- as.factor(df$ROW)
      df$COLUMN <- as.factor(df$COLUMN)
      df$REP <- as.factor(df$REP)
      df$TREATMENT <- as.factor(df$TREATMENT)
      options(DT.options = list(pageLength = nrow(df), 
                                autoWidth = FALSE,
                                scrollX = TRUE, 
                                scrollY = "500px"))
      
      DT::datatable(df,
                    filter = 'top',
                    rownames = FALSE, options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all")))
        )
      
    })

    output$downloadData.rcbd <- downloadHandler(
      filename = function() {
        loc <- paste("RCBD_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(simuDataRCBD()$df)
        write.csv(df, file, row.names = FALSE)
      }
    )
    csv_data <- reactive({
      req(simuDataRCBD()$df)
      df <- simuDataRCBD()$df
      req(input$typlotRCBD)
      if (input$typlotRCBD == 2) {
        export_layout(df, locNum(), TRUE)
      } else {
        export_layout(df, locNum())
      }
    })
    
    
    # Downloadable csv of selected dataset ----
    output$downloadCsv.rcbd <- downloadHandler(
      filename = function() {
        loc <- paste("Randomized_Complete_Block_Layout", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(csv_data()$file)
        write.csv(df, file, row.names = FALSE)
      }
    )
 
  })
}
    
## To be copied in the UI
# mod_RCBD_ui("RCBD_ui_1")
    
## To be copied in the server
# mod_RCBD_server("RCBD_ui_1")
