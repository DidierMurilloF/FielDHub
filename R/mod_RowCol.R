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

                   radioButtons(inputId = ns("owndataRCD"), 
                                label = "Import entries' list?",
                                choices = c("Yes", "No"), 
                                selected = "No",
                                inline = TRUE, 
                                width = NULL, 
                                choiceNames = NULL, 
                                choiceValues = NULL),
                   
                   conditionalPanel(
                     condition = "input.owndataRCD == 'Yes'", 
                     ns = ns,
                     fluidRow(
                       column(8, style=list("padding-right: 28px;"),
                              fileInput(ns("file.RCD"), 
                                        label = "Upload a csv File:", 
                                        multiple = FALSE)),
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
                   conditionalPanel(
                     condition = "input.owndataRCD != 'Yes'",
                     ns = ns,
                     numericInput(ns("t.rcd"), 
                                  label = "Input # of Treatments:",
                                  value = 36,
                                  min = 2),
                   ),
                   fluidRow(
                     column(6, style=list("padding-right: 28px;"),
                            selectInput(inputId = ns("k.rcd"), 
                                        label = "Input # of Rows:",
                                        choices = ""),
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            numericInput(ns("r.rcd"), 
                                         label = "Input # of Full Reps:",
                                         value = 3, 
                                         min = 2)
                     )
                   ),
                   numericInput(inputId = ns("l.rcd"), 
                                label = "Input # of Locations:", 
                                value = 1, min = 1),
                   selectInput(inputId = ns("planter_mov_rcd"),
                               label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"),
                               multiple = FALSE,
                               selected = "serpentine"),
                   fluidRow(
                     column(6, style=list("padding-right: 28px;"),
                            textInput(ns("plot_start.rcd"), 
                                      "Starting Plot Number:", 
                                      value = 101)
                     ),
                     column(6, style=list("padding-left: 5px;"),
                            textInput(ns("Location.rcd"), 
                                      "Input Location:", 
                                      value = "FARGO")
                     )
                   ),
                   numericInput(ns("seed.rcd"), 
                                label = "Random Seed:", 
                                value = 1),
                   fluidRow(
                     column(6,
                            actionButton(
                              inputId = ns("RUN.rcd"), 
                              label = "Run!", 
                              icon = icon("circle-nodes", verify_fa = FALSE),
                              width = '100%'),
                     ),
                     column(6,
                            actionButton(
                              ns("Simulate.RowCol"), 
                              label = "Simulate!", 
                              icon = icon("greater-than-equal", verify_fa = FALSE),
                              width = '100%'),
                     )
                   ), 
                   br(),
                   downloadButton(ns("downloadData.rowcolD"), 
                                  "Save Experiment!",
                                  style = "width:100%")
      ),
      mainPanel(
        width = 8,
        fluidRow(
          tabsetPanel(
            tabPanel("Field Layout",
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(downloadButton(ns("downloadCsv.rcd"), 
                                                    label =  "Excel",
                                                    icon = icon("file-csv"), 
                                                    width = '10%',
                                                    style="color: #337ab7; background-color: #fff; border-color: #2e6da4")),
                     shinycssloaders::withSpinner(
                       plotly::plotlyOutput(ns("layouts"), 
                                            width = "97%", 
                                            height = "550px"),
                       type = 5),
                     br(),
                     column(12,uiOutput(ns("well_panel_layout_ROWCOL")))
            ),
            tabPanel("Field Book", 
                     shinycssloaders::withSpinner(DT::DTOutput(ns("rowcolD")), 
                                                  type = 5)
            )
          )
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
    shinyjs::useShinyjs()
    
    entryListFormat_RCD <- data.frame(ENTRY = 1:9, 
                                       NAME = c(paste("Genotype", 
                                                      LETTERS[1:9], 
                                                      sep = "")))
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
          entriesInfoModal_RCD()
        )
      }
    })
    
    init_data_rcd <- reactive({
      
      if (input$owndataRCD == "Yes") {
        req(input$file.RCD)
        inFile <- input$file.RCD
        data_ingested <- load_file(name = inFile$name, 
                                   path = inFile$datapat, 
                                   sep = input$sep.rcd, 
                                   check = TRUE, 
                                   design = "rcd")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_up <- as.data.frame(data_up[,1:2])
          data_rcd <- na.omit(data_up)
          colnames(data_rcd) <- c("ENTRY", "NAME")
          treatments = nrow(data_rcd)
          return(list(data_rcd = data_rcd, treatments = treatments))
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
        req(input$t.rcd)
        nt <- as.numeric(input$t.rcd)
        df <- data.frame(list(ENTRY = 1:nt, NAME = paste0("G-", 1:nt)))
        colnames(df) <- c("ENTRY", "NAME")
        data_rcd <- df
        treatments = nrow(data_rcd)
        return(list(data_rcd = data_rcd, treatments = treatments))
      }
    })
    
    list_to_observe <- reactive({
      req(init_data_rcd())
      list(
        entry_list = input$owndataRCD,
        entries = init_data_rcd()$treatments
      )
    })
    
    observeEvent(list_to_observe(), {
      req(init_data_rcd())
      t <- as.numeric(init_data_rcd()$treatments)
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
                        inputId = 'k.rcd', 
                        label = "Input # of Rows:",
                        choices = k, 
                        selected = selected)
      
    })
    
    
    get_data_rcd <- reactive({
      if (is.null(init_data_rcd())) {
        shinyalert::shinyalert(
          "Error!!", 
          "Check input file and try again!", 
          type = "error")
        return(NULL)
      } else return(init_data_rcd())
    }) |>
      bindEvent(input$RUN.rcd)
    
    
    rcd_inputs <- reactive({
      req(get_data_rcd())
      req(input$k.rcd)
      req(input$r.rcd)
      req(input$plot_start.rcd)
      req(input$Location.rcd)
      req(input$seed.rcd)
      req(input$l.rcd)
      if (input$k.rcd == "No Options Available") {
        shinyalert::shinyalert(
          "Error!!", 
          "No options for this combination of treatments!", 
          type = "error")
        return(NULL)
      } 
      sites <- as.numeric(input$l.rcd)
      r.rcd <- as.numeric(input$r.rcd)
      k.rcd <- as.numeric(input$k.rcd)
      treatments <- as.numeric(get_data_rcd()$treatments)
      planter <- input$planter_mov_rcd
      plot_start.rcd <- as.vector(unlist(strsplit(input$plot_start.rcd, ",")))
      plot_start <- as.numeric(plot_start.rcd)
      site_names <-  as.vector(unlist(strsplit(input$Location.rcd, ",")))
      seed <- as.numeric(input$seed.rcd)
      return(list(r = r.rcd, 
                  k = k.rcd, 
                  t = treatments, 
                  plot_start = plot_start,
                  planter = planter,
                  sites = sites,
                  site_names = site_names,
                  seed = seed))
    }) |>
      bindEvent(input$RUN.rcd)
    
    RowCol_reactive <- reactive({
      
      req(rcd_inputs())
      req(get_data_rcd())
      
      shinyjs::show(id = "downloadCsv.rcd")
      
      data_rcd <- get_data_rcd()$data_rcd
      
      if (rcd_inputs()$r < 2) {
        shinyalert::shinyalert(
          "Error!!", 
          "Resolvable Row Columns Design needs at least 2 replicates.", 
          type = "error")
        return(NULL)
      }

      row_column(
        t = rcd_inputs()$t, 
        nrows = rcd_inputs()$k, 
        r = rcd_inputs()$r, 
        l = rcd_inputs()$sites, 
        plotNumber = rcd_inputs()$plot_start, 
        seed = rcd_inputs()$seed,
        locationNames = rcd_inputs()$site_names, 
        data = data_rcd
      )
      
    }) |>
      bindEvent(input$RUN.rcd)
    
    upDateSites <- reactive({
      req(input$l.rcd)
      locs <- as.numeric(input$l.rcd)
      sites <- 1:locs
      return(list(sites = sites))
    }) |>
      bindEvent(input$RUN.rcd)
    
    output$well_panel_layout_ROWCOL <- renderUI({
      req(RowCol_reactive()$fieldBook)
      obj_rcd <- RowCol_reactive()
      allBooks_rcd<- plot_layout(x = obj_rcd, layout = 1)$newBooks
      nBooks_rcd <- length(allBooks_rcd)
      layoutOptions_rcd <- 1:nBooks_rcd
      stacked <- c("Vertical Stack Panel" = "vertical", 
                     "Horizontal Stack Panel" = "horizontal")
      wellPanel(
        column(2,
               radioButtons(ns("typlotrcd"), "Type of Plot:",
                            c("Entries/Treatments" = 1,
                              "Plots" = 2,
                              "Heatmap" = 3))
        ),
        fluidRow(
 
          column(3,
                 selectInput(inputId = ns("stackedRowCol"), 
                             label = "Reps layout:", 
                             choices = stacked)
          ),
          column(3, #align="center",
                 selectInput(inputId = ns("layoutO_rcd"), 
                             label = "Layout option:", 
                             choices = layoutOptions_rcd)
          ),
          column(3, #align="center",
                 selectInput(inputId = ns("locLayout_rcd"), 
                             label = "Location:", 
                             choices = as.numeric(upDateSites()$sites))
          )
        )
      )
    })
    
    
    observeEvent(input$stackedRowCol, {
      req(input$stackedRowCol)
      req(input$l.rcd)
      obj <- RowCol_reactive()
      allBooks <- plot_layout(x = obj, layout = 1, 
                              stacked = input$stackedRowCol)$newBooks
      nBooks <- length(allBooks)
      NewlayoutOptions <- 1:nBooks
      updateSelectInput(session = session, inputId = 'layoutO_rcd',
                        label = "Layout option:",
                        choices = NewlayoutOptions,
                        selected = 1
      )
    })
    
    
    reset_selection <- reactiveValues(reset = 0)
    
    observeEvent(input$stackedRowCol, {
      reset_selection$reset <- 1
    })
    
    observeEvent(input$layoutO_rcd, {
      reset_selection$reset <- 0
    })
    
    reactive_layoutROWCOL <- reactive({
      req(input$layoutO_rcd)
      req(RowCol_reactive())
      obj_rcd <- RowCol_reactive()
      
      planting_rcd <- rcd_inputs()$planter
      
      if (reset_selection$reset == 1) {
        opt_rcd <- 1
      } else opt_rcd <- as.numeric(input$layoutO_rcd)
      
      locSelected <- as.numeric(input$locLayout_rcd)
      try(plot_layout(x = obj_rcd, 
                      layout = opt_rcd,
                      planter = planting_rcd, 
                      l = locSelected, 
                      stacked = input$stackedRowCol), 
          silent = TRUE)
    }) 
    
    valsRowColD <- reactiveValues(maxV.RowCol = NULL, 
                                  minV.RowCol = NULL, 
                                  trail.RowCol = NULL)
    
    simuModal.RowCol <- function(failed = FALSE) {
      modalDialog(
        selectInput(inputId = ns("trailsRowCol"), 
                    label = "Select One:", 
                    choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        conditionalPanel(
          condition = "input.trailsRowCol == 'Other'", ns = ns,
          textInput(inputId = ns("OtherRowCol"), 
                    label = "Input Trial Name:", 
                    value = NULL)
        ),
        fluidRow(
          column(6, 
                 numericInput(ns("min.RowCol"), 
                              "Input the min value", 
                              value = NULL)
          ),
          column(6, 
                 numericInput(ns("max.RowCol"), 
                              "Input the max value",
                              value = NULL)
          )
          
        ),
        
        if (failed)
          div(tags$b("Invalid input of data max and min", 
                     style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ok.RowCol"), "GO")
        )
        
      )
      
    }
    
    observeEvent(input$Simulate.RowCol, {
      req(RowCol_reactive()$fieldBook)
      showModal(
        simuModal.RowCol()
      )
    })
    
    observeEvent(input$ok.RowCol, {
      req(input$max.RowCol, input$min.RowCol)
      if (input$max.RowCol > input$min.RowCol && 
          input$min.RowCol != input$max.RowCol) {
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
          simuModal.RowCol(failed = TRUE)
        )
      }
    })
    
    simuData_RowCol <- reactive({
      set.seed(input$seed.rcd)
      req(RowCol_reactive()$fieldBook)
      if(!is.null(valsRowColD$maxV.RowCol) && 
         !is.null(valsRowColD$minV.RowCol) && 
         !is.null(valsRowColD$trail.RowCol)) {
        max <- as.numeric(valsRowColD$maxV.RowCol)
        min <- as.numeric(valsRowColD$minV.RowCol)
        df.RowCol <- reactive_layoutROWCOL()$allSitesFieldbook
        cnamesdf.RowCol <- colnames(df.RowCol)
        df.RowCol <- norm_trunc(
          a = min, 
          b = max, 
          data = df.RowCol, 
          seed = rcd_inputs()$seed
        )
        colnames(df.RowCol) <- c(cnamesdf.RowCol[1:(ncol(df.RowCol) - 1)], 
                                 valsRowColD$trail.RowCol)
        a <- ncol(df.RowCol)
      }else {
        df.RowCol <- reactive_layoutROWCOL()$allSitesFieldbook
        a <- ncol(df.RowCol)
      }
      return(list(df = df.RowCol, a = a))
    })
    
    heatmapInfoModal_RCD <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    locNum <- reactive(
      return(as.numeric(input$locLayout_rcd))
    )
    
    heatmap_obj <- reactive({
      req(simuData_RowCol()$df)
      if (ncol(simuData_RowCol()$df) == 9) {
        locs <- factor(simuData_RowCol()$df$LOCATION, 
                       levels = unique(simuData_RowCol()$df$LOCATION))
        locLevels <- levels(locs)
        df = subset(simuData_RowCol()$df, LOCATION == locLevels[locNum()])
        loc <- levels(factor(df$LOCATION))
        trail <- as.character(valsRowColD$trail.RowCol)
        label_trail <- paste(trail, ": ")
        heatmapTitle <- paste("Heatmap for ", trail)
        new_df <- df |>
          dplyr::mutate(text = paste0("Site: ", loc, "\n", 
                                      "Row: ", df$ROW, "\n", 
                                      "Col: ", df$COLUMN, "\n", 
                                      "Entry: ", df$ENTRY, "\n", 
                                      label_trail, round(df[,9],2)))
        w <- as.character(valsRowColD$trail.RowCol)
        new_df$ROW <- as.factor(new_df$ROW) # Set up ROWS as factors
        new_df$COLUMN <- as.factor(new_df$COLUMN) # Set up COLUMNS as factors
        p1 <- ggplot2::ggplot(new_df, ggplot2::aes(x = new_df[,5], 
                                                   y = new_df[,4], 
                                                   fill = new_df[,9], 
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
          heatmapInfoModal_RCD()
        )
        return(NULL)
      }
    })
    
    output$layouts <- plotly::renderPlotly({
      req(RowCol_reactive())
      req(input$typlotrcd)
      if (input$typlotrcd == 1) {
        reactive_layoutROWCOL()$out_layout
      } else if (input$typlotrcd == 2) {
        reactive_layoutROWCOL()$out_layoutPlots
      } else {
        req(heatmap_obj())
        heatmap_obj()
      }
    })
    
    output$rowcolD <- DT::renderDataTable({
      df <- simuData_RowCol()$df
      df$LOCATION <- as.factor(df$LOCATION)
      df$PLOT <- as.factor(df$PLOT)
      df$ROW <- as.factor(df$ROW)
      df$COLUMN <- as.factor(df$COLUMN)
      df$REP <- as.factor(df$REP)
      df$ENTRY <- as.factor(df$ENTRY)
      a <- as.numeric(simuData_RowCol()$a)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "490px"))
      DT::datatable(df, 
                    filter = 'top', 
                    rownames = FALSE, 
                    options = list(
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
    
    csv_data <- reactive({
      req(simuData_RowCol()$df)
      df <- simuData_RowCol()$df
      req(input$typlotrcd)
      if (input$typlotrcd == 2) {
        export_layout(df, locNum(), TRUE)
      } else {
        export_layout(df, locNum())
      }
    })
    
    
    # Downloadable csv of selected dataset ----
    output$downloadCsv.rcd <- downloadHandler(
      filename = function() {
        loc <- paste("Resolvable_Row-Column_Layout", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(csv_data()$file)
        write.csv(df, file, row.names = FALSE)
      }
    )
  })
}
