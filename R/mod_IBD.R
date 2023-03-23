#' IBD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#'
#' @importFrom shiny NS tagList 
mod_IBD_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Incomplete Blocks Design"),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        radioButtons(ns("owndataibd"), 
                     label = "Import entries' list?", 
                     choices = c("Yes", "No"), 
                     selected = "No",
                     inline = TRUE, 
                     width = NULL, 
                     choiceNames = NULL, 
                     choiceValues = NULL),
        
        conditionalPanel(
          condition = "input.owndataibd != 'Yes'", 
          ns = ns,
          numericInput(ns("t.ibd"), 
                       label = "Input # of Treatments:",
                       value = 15, 
                       min = 2)
        ),
        conditionalPanel(
          condition = "input.owndataibd == 'Yes'", 
          ns = ns,
          fluidRow(
            column(8, style=list("padding-right: 28px;"),
                   fileInput(inputId = ns("file.IBD"), 
                             label = "Upload a CSV File:", 
                             multiple = FALSE)),
            column(4, style=list("padding-left: 5px;"),
                   radioButtons(inputId = ns("sep.ibd"), "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","))
          )        
        ),
        
        numericInput(inputId = ns("r.ibd"), 
                     label = "Input # of Full Reps:", 
                     value = 4, 
                     min = 2),
        
        selectInput(inputId = ns("k.ibd"), 
                    label = "Input # of Plots per IBlock:", 
                    choices = ""),
        
        numericInput(inputId = ns("l.ibd"), 
                     label = "Input # of Locations:",
                     value = 1, 
                     min = 1),
        selectInput(inputId = ns("planter_mov_ibd"), 
          label = "Plot Order Layout:",
          choices = c("serpentine", "cartesian"), 
          multiple = FALSE,
          selected = "serpentine"),
        fluidRow(
          column(6, style=list("padding-right: 28px;"),
                 textInput(inputId = ns("plot_start.ibd"), 
                           "Starting Plot Number:", 
                           value = 101)
          ),
          column(6,style=list("padding-left: 5px;"),
                 textInput(inputId = ns("Location.ibd"), 
                           "Input Location:", 
                           value = "FARGO")
          )
        ), 
        numericInput(inputId = ns("seed.ibd"), 
                     label = "Random Seed:",
                     value = 4),
        fluidRow(
          column(6,
                 actionButton(
                   inputId = ns("RUN.ibd"), 
                   label = "Run!", 
                   icon = icon("circle-nodes", verify_fa = FALSE),
                   width = '100%'),
          ),
          column(6,
                 actionButton(
                   ns("Simulate.ibd"), 
                   label = "Simulate!", 
                   icon = icon("greater-than-equal", verify_fa = FALSE),
                   width = '100%'),
          )
          
        ), 
        br(),
        downloadButton(ns("downloadData.ibd"), 
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
                         ns("downloadCsv.ibd"), 
                         label =  "Excel",
                         icon = icon("file-csv"), 
                         width = '10%',
                         style="color: #337ab7; background-color: #fff; border-color: #2e6da4")
                      ),
                     shinycssloaders::withSpinner(
                       plotly::plotlyOutput(ns("layouts"), 
                                            width = "97%", 
                                            height = "550px"),
                       type = 5
                     ),
                     br(),
                     column(12,
                            uiOutput(ns("well_panel_layout_IBD"))
                            )
            ),
            tabPanel("Field Book", 
                     shinycssloaders::withSpinner(
                       DT::DTOutput(ns("IBD.output")), 
                       type = 5
                       )
            )
          )
        )
      )
    )
  )
}

#' IBD Server Functions
#'
#' @noRd 
mod_IBD_server <- function(id) {
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    shinyjs::useShinyjs()
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
    
    init_data_ibd <- reactive({
      
      if(input$owndataibd == "Yes") {
        req(input$file.IBD)
        inFile <- input$file.IBD
        data_ingested <- load_file(name = inFile$name, 
                                   path = inFile$datapat, 
                                   sep = input$sep.ibd, 
                                   check = TRUE, 
                                   design = "ibd")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_up <- as.data.frame(data_up[,1:2])
          data_ibd <- na.omit(data_up)
          colnames(data_ibd) <- c("ENTRY", "NAME")
          treatments = nrow(data_ibd)
          return(list(data_ibd = data_ibd, treatments = treatments))
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
            "Data input needs at least two columns: ENTRY and NAME",
            type = "error")
          return(NULL)
        }
      } else {
        req(input$t.ibd)
        nt <- as.numeric(input$t.ibd)
        df <- data.frame(list(ENTRY = 1:nt, NAME = paste0("G-", 1:nt)))
        colnames(df) <- c("ENTRY", "NAME")
        data_ibd <- df
        treatments = nrow(data_ibd)
        return(list(data_ibd = data_ibd, treatments = treatments))
      }
    })
    
    list_to_observe <- reactive({
      req(init_data_ibd())
      list(
        entry_list = input$owndataibd,
        entries = init_data_ibd()$treatments
      )
    })
    
    observeEvent(list_to_observe(), {
      req(init_data_ibd())
      t <- as.numeric(req(init_data_ibd())$treatments)
      if (numbers::isPrime(t)) {
        w <- 1
        k <- "No Options Available"
      }else {
        k <- numbers::divisors(t)
        k <- k[2:(length(k) - 1)]
        w <- 2
      }
      
      if (length(k) > 2) {
        selected <- k[ceiling(length(k)/2)]
      } else selected <- k[1]
      
      updateSelectInput(session = session, 
                        inputId = 'k.ibd', 
                        label = "Input # of Plots per IBlock:",
                        choices = k, selected = selected)
      
    })
    
    get_data_ibd <- reactive({
      if (is.null(init_data_ibd())) {
        shinyalert::shinyalert(
          "Error!!", 
          "Check input file and try again!", 
          type = "error")
        return(NULL)
      } else return(init_data_ibd())
    }) %>%
      bindEvent(input$RUN.ibd)
    
    ibd_inputs <- reactive({
      
      req(get_data_ibd())
      
      req(input$r.ibd)
      req(input$k.ibd)
      req(input$seed.ibd)
      req(input$plot_start.ibd)
      req(input$Location.ibd)
      req(input$l.ibd)
      req(input$planter_mov_ibd)
      
      r.ibd <- as.numeric(input$r.ibd)
      k.ibd <- as.numeric(input$k.ibd)
      treatments <- as.numeric(get_data_ibd()$treatments)
      planter <- input$planter_mov_ibd
      plot_start.ibd <- as.vector(unlist(strsplit(input$plot_start.ibd, ",")))
      plot_start <- as.numeric(plot_start.ibd)
      site_names <-  as.vector(unlist(strsplit(input$Location.ibd, ",")))
      seed <- as.numeric(input$seed.ibd)
      sites <- as.numeric(input$l.ibd)
      if (input$k.ibd == "No Options Available") {
        shinyalert::shinyalert(
          "Error!!", 
          "No options for this combination of treatments!", 
          type = "error")
        return(NULL)
      } 
      return(list(
        r = r.ibd, 
        k = k.ibd, 
        t = treatments, 
        planter = planter,
        plot_start = plot_start, 
        sites = sites,
        site_names = site_names,
        seed = seed))
    }) %>%
      bindEvent(input$RUN.ibd)
    
    
    IBD_reactive <- reactive({
      req(get_data_ibd())
      
      shinyjs::show(id = "downloadCsv.ibd")
      
      data_ibd <- get_data_ibd()$data_ibd
      
      if (ibd_inputs()$r < 2) {
        shinyalert::shinyalert(
          "Error!!", 
          "Incomplete Blocks Design needs at least 2 replicates.", 
          type = "error")
        return(NULL)
      }
      
      incomplete_blocks(
        t = ibd_inputs()$t, 
        k = ibd_inputs()$k, 
        r = ibd_inputs()$r, 
        l = ibd_inputs()$sites, 
        plotNumber = ibd_inputs()$plot_start, 
        seed = ibd_inputs()$seed,
        locationNames = ibd_inputs()$site_names, 
        data = data_ibd
      ) 
      
    }) %>%
      bindEvent(input$RUN.ibd)
    
    upDateSites <- eventReactive(input$RUN.ibd, {
      req(input$l.ibd)
      locs <- as.numeric(input$l.ibd)
      sites <- 1:locs
      return(list(sites = sites))
    })
    
    output$well_panel_layout_IBD <- renderUI({
      req(IBD_reactive()$fieldBook)
      obj_ibd <- IBD_reactive()
      allBooks_ibd<- plot_layout(x = obj_ibd, layout = 1)$newBooks
      nBooks_ibd <- length(allBooks_ibd)
      layoutOptions_ibd <- 1:nBooks_ibd
      stacked <- c("Vertical Stack Panel" = "vertical", 
                     "Horizontal Stack Panel" = "horizontal")
      wellPanel(
        column(2,
               radioButtons(ns("typlotibd"), "Type of Plot:",
                            c("Entries/Treatments" = 1,
                              "Plots" = 2,
                              "Heatmap" = 3), selected = 1)
        ),
        fluidRow(
          column(3,
                 selectInput(inputId = ns("stackedibd"), 
                             label = "Reps layout:", 
                             choices = stacked)
          ),
          column(3, #align="center",
                 selectInput(inputId = ns("layoutO_ibd"), 
                             label = "Layout option:", 
                             choices = layoutOptions_ibd)
          ),
          column(3, #align="center",
                 selectInput(inputId = ns("locLayout_ibd"), 
                             label = "Location:", 
                             choices = as.numeric(upDateSites()$sites))
          )
        )
      )
    })
    
    observeEvent(input$stackedibd, {
      req(input$stackedibd)
      obj <- IBD_reactive()
      allBooks <- plot_layout(x = obj, 
                              layout = 1, 
                              stacked = input$stackedibd)$newBooks
      nBooks <- length(allBooks)
      NewlayoutOptions <- 1:nBooks
      updateSelectInput(session = session, inputId = 'layoutO_ibd',
                        label = "Layout option:",
                        choices = NewlayoutOptions,
                        selected = 1
      )
    })
    
    reset_selection <- reactiveValues(reset = 0)
    
    observeEvent(input$stackedibd, {
      reset_selection$reset <- 1
    })
    
    observeEvent(input$layoutO_ibd, {
      reset_selection$reset <- 0
    })
    
    reactive_layoutIBD <- reactive({
      req(input$layoutO_ibd)
      req(IBD_reactive())
      obj_ibd <- IBD_reactive()
      planting_ibd <- ibd_inputs()$planter
      
      if (reset_selection$reset == 1) {
        opt_ibd <- 1
      } else opt_ibd <- as.numeric(input$layoutO_ibd)
      
      locSelected <- as.numeric(input$locLayout_ibd)
      try(plot_layout(x = obj_ibd, 
                      layout =  opt_ibd, 
                      planter = planting_ibd, 
                      l = locSelected, 
                      stacked = input$stackedibd), 
          silent = TRUE)
    })
    
    
    valsIBD <- reactiveValues(maxV.ibd = NULL, 
                              minV.ibd = NULL, 
                              trail.ibd = NULL)
    
    simuModal.ibd <- function(failed = FALSE) {
      modalDialog(
        selectInput(inputId = ns("trailsIBD"), 
                    label = "Select One:", 
                    choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        conditionalPanel(
          condition = "input.trailsIBD == 'Other'", 
          ns = ns,
          textInput(inputId = ns("OtherIBD"), 
                    label = "Input Trial Name:", 
                    value = NULL)
        ),
        fluidRow(
          column(6, 
                 numericInput(inputId = ns("min.ibd"),
                              "Input the min value", 
                              value = NULL)
          ),
          column(6, 
                 numericInput(inputId = ns("max.ibd"), 
                              "Input the max value", 
                              value = NULL)
                 
          )
          
        ),
        
        if (failed)
          div(tags$b("Invalid input of data max and min", 
                     style = "color: red;")),
        
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
      if(!is.null(valsIBD$maxV.ibd) && !is.null(valsIBD$minV.ibd) && 
         !is.null(valsIBD$trail.ibd)) {
        max <- as.numeric(valsIBD$maxV.ibd)
        min <- as.numeric(valsIBD$minV.ibd)
        df.ibd <- reactive_layoutIBD()$allSitesFieldbook
        cnamesdf.ibd <- colnames(df.ibd)
        df.ibd <- norm_trunc(a = min, b = max, data = df.ibd)
        colnames(df.ibd) <- c(cnamesdf.ibd[1:(ncol(df.ibd) - 1)], 
                              valsIBD$trail.ibd)
        a <- ncol(df.ibd)
      }else {
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
    
    locNum <- reactive(
      return(as.numeric(input$locLayout_ibd))
    )
    
    heatmap_obj <- reactive({
      req(simuDataIBD()$df)
      if (ncol(simuDataIBD()$df) == 11) {
        locs <- factor(simuDataIBD()$df$LOCATION, 
                       levels = unique(simuDataIBD()$df$LOCATION))
        locLevels <- levels(locs)
        df = subset(simuDataIBD()$df, LOCATION == locLevels[locNum()])
        loc <- levels(factor(df$LOCATION))
        trail <- as.character(valsIBD$trail.ibd)
        label_trail <- paste(trail, ": ")
        heatmapTitle <- paste("Heatmap for ", trail)
        new_df <- df %>%
          dplyr::mutate(text = paste0("Site: ",
                                      loc, "\n", "Row: ", 
                                      df$ROW, "\n", "Col: ", 
                                      df$COLUMN, "\n", "Entry: ", 
                                      df$ENTRY, "\n", 
                                      label_trail, 
                                      round(df[,11],2)))
        w <- as.character(valsIBD$trail.ibd)
        new_df$ROW <- as.factor(new_df$ROW) # Set up ROWS as factors
        new_df$COLUMN <- as.factor(new_df$COLUMN) # Set up COLUMNS as factors
        p1 <- ggplot2::ggplot(
          new_df, 
          ggplot2::aes(x = new_df[,5], 
                       y = new_df[,4], 
                       fill = new_df[,11], 
                       text = text)) +
          ggplot2::geom_tile() +
          ggplot2::xlab("COLUMN") +
          ggplot2::ylab("ROW") +
          ggplot2::labs(fill = w) +
          viridis::scale_fill_viridis(discrete = FALSE) +
          ggplot2::ggtitle(heatmapTitle) +
          ggplot2::theme_minimal() + # I added this option 
          ggplot2::theme(
            plot.title = ggplot2::element_text(
              family="Calibri", 
              face="bold", 
              size=13, 
              hjust=0.5))
        p2 <- plotly::ggplotly(p1, tooltip="text", height = 560)
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
    
    output$layouts <- plotly::renderPlotly({
      req(reactive_layoutIBD())
      req(IBD_reactive())
      req(input$typlotibd)
      if (input$typlotibd == 1) {
        reactive_layoutIBD()$out_layout
      } else if (input$typlotibd == 2) {
        reactive_layoutIBD()$out_layoutPlots
      } else {
        req(heatmap_obj())
        heatmap_obj()
      }
    })
    
    output$IBD.output <- DT::renderDataTable({
      
      req(simuDataIBD()$df)
      df <- simuDataIBD()$df
      df$LOCATION <- as.factor(df$LOCATION)
      df$PLOT <- as.factor(df$PLOT)
      df$ROW <- as.factor(df$ROW)
      df$COLUMN <- as.factor(df$COLUMN)
      df$REP <- as.factor(df$REP)
      df$IBLOCK <- as.factor(df$IBLOCK)
      df$UNIT <- as.factor(df$UNIT)
      df$ENTRY <- as.factor(df$ENTRY)
      df$TREATMENT <- as.factor(df$TREATMENT)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      DT::datatable(df,
                    filter = 'top',
                    rownames = FALSE, 
                    options = list(
                      columnDefs = list(list(className = 'dt-center', 
                                             targets = "_all"))))
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
    csv_data <- reactive({
      req(simuDataIBD()$df)
      df <- simuDataIBD()$df
      req(input$typlotibd)
      if (input$typlotibd == 2) {
        export_layout(df, locNum(), TRUE)
      } else {
        export_layout(df, locNum())
      }
    })
    
    
    # Downloadable csv of selected dataset ----
    output$downloadCsv.ibd <- downloadHandler(
      filename = function() {
        loc <- paste("Incomplete_Block_Layout", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(csv_data()$file)
        write.csv(df, file, row.names = FALSE)
      }
    )
  })
}
