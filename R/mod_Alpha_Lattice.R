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
mod_Alpha_Lattice_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Alpha Lattice Design"),
    sidebarLayout(
      sidebarPanel(width = 4,
                   radioButtons(ns("owndata_alpha"), label = "Import entries' list?", choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                   
                   conditionalPanel("input.owndata_alpha != 'Yes'", ns = ns,
                                    numericInput(ns("t.alpha"), label = "Input # of Treatments:",
                                                 value = 36, min = 2)
                                    
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
                   numericInput(inputId = ns("r.alpha"), label = "Input # of Full Reps:", value = 3, min = 2),
                   selectInput(inputId = ns("k.alpha"), label = "Input # of Plots per IBlock:", choices = ""),
                   numericInput(inputId = ns("l.alpha"), label = "Input # of Locations:", value = 1, min = 1),
                   
                   selectInput(inputId = ns("planter_mov_alpha"), label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"), multiple = FALSE,
                               selected = "serpentine"),
                   
                   fluidRow(
                     column(6, style=list("padding-right: 28px;"),
                            textInput(inputId = ns("plot_start.alpha"), "Starting Plot Number:", value = 101)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            textInput(inputId = ns("Location.alpha"), "Input Location:", value = "FARGO")
                     )
                   ),  
                   numericInput(inputId = ns("myseed.alpha"), label = "Random Seed:",
                                value = 16, min = 1),
                   fluidRow(
                     column(6,
                            actionButton(
                              inputId = ns("RUN.alpha"), 
                              "Run!", 
                              icon = icon("circle-nodes", verify_fa = FALSE),
                              width = '100%'),
                     ),
                     column(6,
                            actionButton(
                              inputId = ns("Simulate.alpha"), 
                              "Simulate!", 
                              icon = icon("greater-than-equal", verify_fa = FALSE),
                              width = '100%'),
                     )
                     
                   ), 
                   br(),
                   downloadButton(ns("downloadData.alpha"), "Save My Experiment", style = "width:100%")
      ),
      
      mainPanel(
        width = 8,
        fluidRow(
          tabsetPanel(
            tabPanel(
              "Summary Design",
              br(),
              shinycssloaders::withSpinner(
                verbatimTextOutput(outputId = ns("summary_alpha_lattice"), 
                                   placeholder = FALSE), 
                type = 4
              ),
              style = "padding-right: 40px;"
            ),
            tabPanel("Field Layout",
                     
                     # hidden .csv download button
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(downloadButton(ns("downloadCsv.alpha"), 
                                    label =  "Excel",
                                    icon = icon("file-csv"), 
                                    width = '10%',
                                    style="color: #337ab7; background-color: #fff; border-color: #2e6da4")),
                     
                     shinycssloaders::withSpinner(
                       plotly::plotlyOutput(ns("random_layout"), width = "97%", height = "550px"),type = 5
                     ),
                     br(),
                     column(12, uiOutput(ns("well_panel_layout")))
            ),
            tabPanel("Field Book", 
                     shinycssloaders::withSpinner(DT::DTOutput(ns("ALPHA_fieldbook")), type = 5)
            )
          )
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
    
    # for showing .csv button on run
    shinyjs::useShinyjs()
    
    init_data_alpha <- reactive({
      if (input$owndata_alpha == "Yes") {
        req(input$file.alpha)
        inFile <- input$file.alpha
        data_ingested <- load_file(name = inFile$name,
                                   path = inFile$datapat,
                                   sep = input$sep.alpha,
                                   check = TRUE, design = "alpha")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_up <- as.data.frame(data_up[,1:2])
          data_alpha <- na.omit(data_up)
          colnames(data_alpha) <- c("ENTRY", "NAME")
          treatments = nrow(data_alpha)
          return(list(data_alpha = data_alpha, treatments = treatments))
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
        req(input$t.alpha)
        nt <- as.numeric(input$t.alpha)
        df <- data.frame(list(ENTRY = 1:nt, NAME = paste0("G-", 1:nt)))
        colnames(df) <- c("ENTRY", "NAME")
        data_alpha <- df
        treatments = nrow(data_alpha)
        return(list(data_alpha = data_alpha, treatments = treatments))
      }
    })
    
    
    list_to_observe <- reactive({
      req(init_data_alpha())
      list(
        entry_list = input$owndata_alpha,
        entries = init_data_alpha()$treatments
      )
    })
    
    observeEvent(list_to_observe(), {
      req(init_data_alpha())
      t <- as.numeric(init_data_alpha()$treatments)
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
      
      updateSelectInput(session = session, inputId = 'k.alpha', 
                        label = "Input # of Plots per IBlock:",
                        choices = k, selected = selected)
    })
    
    get_data_alpha <- reactive({
      if (is.null(init_data_alpha())) {
        shinyalert::shinyalert(
          "Error!!", 
          "Check input file and try again!", 
          type = "error")
        return(NULL)
      } else return(init_data_alpha())
    }) |>
      bindEvent(input$RUN.alpha)
    
    alpha_inputs <- reactive({
      req(get_data_alpha())
      req(input$planter_mov_alpha)
      req(input$k.alpha)
      req(input$r.alpha)
      req(input$plot_start.alpha)
      req(input$Location.alpha)
      req(input$myseed.alpha)
      req(input$l.alpha)
      if (input$k.alpha == "No Options Available") {
        shinyalert::shinyalert(
          "Error!!", 
          "No options for this combination of treatments!", 
          type = "error")
        return(NULL)
      } 
      sites <- as.numeric(input$l.alpha)
      r.alpha <- as.numeric(input$r.alpha)
      k.alpha <- as.numeric(input$k.alpha)
      treatments <- as.numeric(get_data_alpha()$treatments)
      planter <- input$planter_mov_alpha
      plot_startreatments <- as.vector(unlist(strsplit(input$plot_start.alpha, ",")))
      plot_start <- as.numeric(plot_startreatments)
      site_names <-  as.vector(unlist(strsplit(input$Location.alpha, ",")))
      seed <- as.numeric(input$myseed.alpha)
      return(list(r = r.alpha, 
                  k = k.alpha, 
                  t = treatments, 
                  planter = planter,
                  plot_start = plot_start, 
                  sites = sites,
                  site_names = site_names,
                  seed = seed))
    }) |>
      bindEvent(input$RUN.alpha)


    entryListFormatreatments <- data.frame(ENTRY = 1:9, 
                                        NAME = c(paste("Genotype", LETTERS[1:9], sep = "")))
    entriesInfoModal_ALPHA <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        renderTable(entryListFormatreatments,
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
          entriesInfoModal_ALPHA()
        )
      }
    })
    

    ALPHA_reactive <- eventReactive(input$RUN.alpha, {
      req(get_data_alpha())
      req(alpha_inputs())

      # show .csv download button when run
      shinyjs::show(id = "downloadCsv.alpha")
    
      data_alpha <- get_data_alpha()$data_alpha
      
      if (alpha_inputs()$r < 2) {
        shinyalert::shinyalert(
          "Error!!", 
          "Alpha Lattice Design needs at least 2 replicates.", 
          type = "error")
        return(NULL)
      }
      
      alpha_lattice(
        t = alpha_inputs()$t, 
        k = alpha_inputs()$k, 
        r = alpha_inputs()$r, 
        l = alpha_inputs()$sites, 
        plotNumber = alpha_inputs()$plot_start, 
        seed = alpha_inputs()$seed,
        locationNames = alpha_inputs()$site_names, 
        data = data_alpha
      )
    })
    
    output$summary_alpha_lattice <- renderPrint({
      req(ALPHA_reactive())
      cat("Randomization was successful!", "\n", "\n")
      print(ALPHA_reactive(), n = 6)
    })
    
    upDateSites <- reactive({
      req(alpha_inputs())
      locs <- alpha_inputs()$sites
      sites <- 1:locs
      return(list(sites = sites))
    })
    
    
    output$well_panel_layout <- renderUI({
      req(ALPHA_reactive()$fieldBook)
      df <- ALPHA_reactive()$fieldBook
      locs <- length(levels(as.factor(df$LOCATION)))
      repsAlpha <- length(levels(as.factor(df$REP)))
      if ((repsAlpha >= 4 & repsAlpha %% 2 == 0) | (repsAlpha >= 4 & sqrt(repsAlpha) %% 1 == 0)) {
        stacked <- c("Vertical Stack Panel" = "vertical", "Horizontal Stack Panel" = "horizontal",  
                       "Grid Panel" = "grid_panel")
      } else {
        stacked <- c("Vertical Stack Panel" = "vertical", "Horizontal Stack Panel" = "horizontal")
      }
      obj <- ALPHA_reactive()
      allBooks <- plot_layout(x = obj, layout = 1, stacked = "vertical")$newBooks
      nBooks <- length(allBooks)
      layoutOptions <- 1:nBooks
      wellPanel(
        fluidPage(
          column(3,
                 radioButtons(ns("typlotALPHA"), "Type of Plot:",
                              c("Entries/Treatments" = 1,
                                "Plots" = 2,
                                "Heatmap" = 3), selected = 1)
          ),
          column(3,
                 selectInput(inputId = ns("stackedAlpha"),
                             label = "Reps layout:", 
                             choices = stacked)
          ),
          column(2, 
                 selectInput(inputId = ns("layoutO"), 
                             label = "Layout option:",
                             choices = layoutOptions, 
                             selected = 1)
          ),
          column(2, 
                 selectInput(inputId = ns("locLayout"),
                             label = 'Location:', 
                             choices = as.numeric(upDateSites()$sites))
          )
        )
      )
    })
    
    observeEvent(input$stackedAlpha, {
      req(input$stackedAlpha)
      obj <- ALPHA_reactive()
      allBooks <- plot_layout(x = obj, 
                              layout = 1, 
                              stacked = input$stackedAlpha)$newBooks
      nBooks <- length(allBooks)
      NewlayoutOptions <- 1:nBooks
      updateSelectInput(session = session, inputId = 'layoutO',
                        label = "Layout option:",
                        choices = NewlayoutOptions,
                        selected = 1
      )
    })
    
    reset_selection <- reactiveValues(reset = 0)
    
    observeEvent(input$stackedAlpha, {
      reset_selection$reset <- 1
    })
    
    observeEvent(input$layoutO, {
      reset_selection$reset <- 0
    })
    
    reactive_layoutAlpha <- reactive({
      req(input$stackedAlpha)
      req(alpha_inputs()$planter)
      req(input$layoutO)
      req(ALPHA_reactive())
      obj <- ALPHA_reactive()
      
      if (reset_selection$reset == 1) {
        opt <- 1
      } else opt <- as.numeric(input$layoutO)
      
      locSelected <- as.numeric(input$locLayout)
      try(plot_layout(x = obj, 
                      layout = opt, 
                      planter = alpha_inputs()$planter, 
                      l = locSelected, 
                      stacked = input$stackedAlpha), 
          silent = TRUE)
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
      req(reactive_layoutAlpha()$fieldBookXY)
      showModal(
        simuModal.alpha()
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
          simuModal.alpha(failed = TRUE)
        )
      }
    })
    
    simuDataALPHA <- reactive({
      set.seed(input$myseed.alpha)
      req(reactive_layoutAlpha())
      if(!is.null(valsALPHA$maxV.alpha) && !is.null(valsALPHA$minV.alpha) && !is.null(valsALPHA$trail.alpha)) {
        max <- as.numeric(valsALPHA$maxV.alpha)
        min <- as.numeric(valsALPHA$minV.alpha)
        df.alpha <- reactive_layoutAlpha()$allSitesFieldbook
        cnamesdf.alpha <- colnames(df.alpha)
        df.alpha <- norm_trunc(a = min, b = max, data = df.alpha)
        colnames(df.alpha) <- c(cnamesdf.alpha[1:(ncol(df.alpha) - 1)], valsALPHA$trail.alpha)
        a <- ncol(df.alpha)
      }else {
        df.alpha <- reactive_layoutAlpha()$allSitesFieldbook
        a <- ncol(df.alpha)
      }
      return(list(df = df.alpha, a = a))
    })
    
    heatmapInfoModal_ALPHA <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    locNum <- reactive(
      return(as.numeric(input$locLayout))
    )
    
    heatmap_obj <- reactive({
      req(simuDataALPHA()$df)
      if (ncol(simuDataALPHA()$df) == 11) {
        locs <- factor(simuDataALPHA()$df$LOCATION, levels = unique(simuDataALPHA()$df$LOCATION))
        locLevels <- levels(locs)
        df = subset(simuDataALPHA()$df, LOCATION == locLevels[locNum()])
        loc <- levels(factor(df$LOCATION))
        trail <- as.character(valsALPHA$trail.alpha)
        label_trail <- paste(trail, ": ")
        heatmapTitle <- paste("Heatmap for ", trail)
        new_df <- df |>
          dplyr::mutate(text = paste0("Site: ", loc, "\n", 
                                      "Row: ", df$ROW, "\n", 
                                      "Col: ", df$COLUMN, "\n", 
                                      "Entry: ", df$ENTRY, "\n", 
                                      label_trail, round(df[,11],2)))
        w <- as.character(valsALPHA$trail.alpha)
        new_df$ROW <- as.factor(new_df$ROW) # Set up ROWS as factors
        new_df$COLUMN <- as.factor(new_df$COLUMN) # Set up COLUMNS as factors
        p1 <- ggplot2::ggplot(new_df, ggplot2::aes(x = new_df[,5], 
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
          ggplot2::theme(plot.title = ggplot2::element_text(family="Calibri", 
                                                            face="bold",
                                                            size=13, 
                                                            hjust=0.5))
        
        p2 <- plotly::ggplotly(p1, tooltip="text", height = 560)
        return(p2)
      } else {
        showModal(
          heatmapInfoModal_ALPHA()
        )
        return(NULL)
        }
    })
    
    output$random_layout <- plotly::renderPlotly({
      req(reactive_layoutAlpha())
      req(ALPHA_reactive())
      req(input$typlotALPHA)
      if (input$typlotALPHA == 1) {
        reactive_layoutAlpha()$out_layout
      } else if (input$typlotALPHA == 2) {
        reactive_layoutAlpha()$out_layoutPlots
      } else {
        req(heatmap_obj())
        heatmap_obj()
      }
    })
    
    output$ALPHA_fieldbook <- DT::renderDataTable({
      req(simuDataALPHA()$df)
      df <- simuDataALPHA()$df
      df$LOCATION <- as.factor(df$LOCATION)
      df$PLOT <- as.factor(df$PLOT)
      df$ROW <- as.factor(df$ROW)
      df$COLUMN <- as.factor(df$COLUMN)
      df$REP <- as.factor(df$REP)
      df$IBLOCK <- as.factor(df$IBLOCK)
      df$UNIT <- as.factor(df$UNIT)
      df$ENTRY <- as.factor(df$ENTRY)
      a <- as.numeric(simuDataALPHA()$a)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      
      DT::datatable(df,
                    filter = 'top',
                    rownames = FALSE, 
                    options = list(
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
    
    csv_data <- reactive({
      req(simuDataALPHA()$df)
      df <- simuDataALPHA()$df
      req(input$typlotALPHA)
      if (input$typlotALPHA == 2) {
        export_layout(df, locNum(), TRUE)
      } else {
        export_layout(df, locNum())
      }
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadCsv.alpha <- downloadHandler(
      filename = function() {
        loc <- paste("Alpha_Lattice_Layout", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(csv_data()$file)
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    
  })
}
