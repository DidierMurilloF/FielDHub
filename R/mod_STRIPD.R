#' STRIPD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_STRIPD_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("Strip-Plot Design"),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        radioButtons(inputId = ns("owndataSTRIP"), 
                     label = "Import entries' list?", 
                     choices = c("Yes", "No"), 
                     selected = "No",
                     inline = TRUE, 
                     width = NULL, 
                     choiceNames = NULL, 
                     choiceValues = NULL),
        conditionalPanel(
          condition = "input.owndataSTRIP == 'Yes'", 
          ns = ns,
          fluidRow(
            column(8, style=list("padding-right: 28px;"),
                   fileInput(ns("file.STRIP"), 
                             label = "Upload a csv File:",
                             multiple = FALSE)),
            
            column(4,style=list("padding-left: 5px;"),
                   radioButtons(ns("sep.strip"), "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","))
          )
        ),
        
        conditionalPanel(
          condition = "input.owndataSTRIP != 'Yes'", 
          ns = ns,
          fluidRow(
            column(6, style=list("padding-right: 28px;"),
                   numericInput(ns("HStrip.strip"), 
                                label = "Input # of Horizontal Strips:",
                                value = 5, 
                                min = 2)
            ),
            column(6, style=list("padding-left: 5px;"),
                   numericInput(ns("VStrip.strip"), 
                                label = "Input # of Vertical Strips:",
                                value = 5, 
                                min = 2)
            )
          )           
        ),
        numericInput(ns("blocks.strip"), 
                     label = "Input # of Full Reps:", 
                     value = 3, 
                     min = 2),
        numericInput(ns("l.strip"), 
                     label = "Input # of Locations:",
                     value = 1, 
                     min = 1), 
        selectInput(inputId = ns("planter.strip"), 
                    label = "Plot Order Layout:",
                    choices = c("serpentine", "cartesian"), 
                    multiple = FALSE,
                    selected = "serpentine"),
        fluidRow(
          column(6, style=list("padding-right: 28px;"),
                 textInput(ns("plot_start.strip"),
                           "Starting Plot Number:", 
                           value = 101)
          ),
          column(6, style=list("padding-left: 5px;"),
                 textInput(ns("Location.strip"), 
                           "Input Location:", 
                           value = "FARGO")
          )
        ),
        
        numericInput(inputId = ns("myseed.strip"), 
                     label = "Random Seed:", 
                     value = 123, 
                     min = 1),
        
        fluidRow(
          column(6,
                 actionButton(
                   inputId = ns("RUN.strip"), 
                   "Run!", 
                   icon = icon("circle-nodes", verify_fa = FALSE),
                   width = '100%'
                   ),
          ),
          column(6,
                 actionButton(
                   ns("Simulate.strip"), 
                   "Simulate!", 
                   icon = icon("greater-than-equal", verify_fa = FALSE),
                   width = '100%'
                   ),
          )
          
        ), 
        br(),
        downloadButton(ns("downloadData.strip"), 
                       "Save Experiment!", 
                       style = "width:100%")
      ),
      
      mainPanel(
        width = 8,
        fluidRow(
          tabsetPanel(
            tabPanel(title = "Field Layout",
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(downloadButton(ns("downloadCsv.strip"), 
                                                    label =  "Excel",
                                                    icon = icon("file-csv"), 
                                                    width = '10%',
                                                    style="color: #337ab7; background-color: #fff; border-color: #2e6da4")),
                     shinycssloaders::withSpinner(
                       plotly::plotlyOutput(ns("layout.strip"), 
                                            width = "97%", 
                                            height = "560px"),
                       type = 5
                     ),
                     br(),
                     column(12,
                            uiOutput(ns("well_panel_layout_STRIP"))
                            )
            ),
            tabPanel(title = "Field Book", 
                     shinycssloaders::withSpinner(
                       DT::DTOutput(ns("STRIP.output")), 
                       type = 5
                       )
            )
          )
        )
      )
    ) 
  )
}
    
#' STRIPD Server Functions
#'
#' @noRd 
mod_STRIPD_server <- function(id) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
    shinyjs::useShinyjs()
    Hplots <- LETTERS[1:5]
    Vplots <- LETTERS[1:5]
    entryListFormat_STRIP <- data.frame(
      list(HPLOTS = Hplots, VPLOTS = Vplots)
      )           
    entriesInfoModal_STRIP <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        renderTable(entryListFormat_STRIP,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        easyClose = FALSE
      )
    }
    
    toListen <- reactive({
      list(input$owndataSTRIP)
    })
    
    observeEvent(toListen(), {
      if (input$owndataSTRIP == "Yes") {
        showModal(
          shinyjqui::jqui_draggable(
            entriesInfoModal_STRIP()
          )
        )
      }
    })
    
    get_data_strip <- reactive({
      if (input$owndataSTRIP == "Yes") {
        req(input$file.STRIP)
        inFile <- input$file.STRIP
        data_ingested <- load_file(name = inFile$name, 
                                   path = inFile$datapat, 
                                   sep = input$sep.strip, 
                                   check = TRUE,
                                   design = "strip")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_strip <- as.data.frame(data_up[,1:2])
          colnames(data_strip) <- c("Hplot", "Vplot")
          Hstrip <- length(as.vector(na.omit(data_strip[,1])))
          Vstrip <- length(as.vector(na.omit(data_strip[,2])))
          treatments <- c(Hstrip, Vstrip)
          return(list(data_strip = data_strip, treatments = treatments))
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
            "Data input needs at least two column: Hplot and Vplot", 
            type = "error")
          return(NULL)
        }
      } else {
        req(input$HStrip.strip, input$VStrip.strip)
        req(input$blocks.strip)
        Hplots <- as.numeric(input$HStrip.strip)
        Vplots <- as.numeric(input$VStrip.strip)
        treatments = c(Hplots, Vplots)
        return(list(data_strip = NULL, treatments = treatments))
      }
    }) %>% 
      bindEvent(input$RUN.strip)
    
    
    strip_inputs <- reactive({
      
      req(get_data_strip())
      
      req(input$plot_start.strip)
      req(input$Location.strip)
      req(input$myseed.strip)
      req(input$l.strip)
      req(input$blocks.strip)
      req(input$planter.strip)
      
      l.strip <- as.numeric(input$l.strip)
      seed.strip <- as.numeric(input$myseed.strip)
      plot_start.strip <- as.vector(unlist(strsplit(input$plot_start.strip, ",")))
      plot_start.strip <- as.numeric(plot_start.strip)
      loc.strip <-  as.vector(unlist(strsplit(input$Location.strip, ",")))
      reps.strip <- as.numeric(input$blocks.strip)
      planter <- input$planter.strip
      data_strip <- get_data_strip()$data_strip
      
      return(
        list(
          Hplots = get_data_strip()$treatments[1], 
          Vplots = get_data_strip()$treatments[2], 
          b = reps.strip, 
          l = l.strip,
          seed = seed.strip,
          planter = planter,
          plot_number = plot_start.strip,
          site_names = loc.strip, 
          data = data_strip
        )
      )
    }) %>%
      bindEvent(input$RUN.strip)
    
    strip_reactive <- reactive({
      
      req(strip_inputs())
      
      shinyjs::show(id = "downloadCsv.strip")
    
      strip_plot(
        Hplots = strip_inputs()$Hplots,
        Vplots = strip_inputs()$Vplots,
        b = strip_inputs()$b, 
        l = strip_inputs()$l,
        planter = strip_inputs()$planter,
        plotNumber = strip_inputs()$plot_number,
        locationNames = strip_inputs()$site_names,
        seed = strip_inputs()$seed,
        data = strip_inputs()$data
      )
      
    }) %>% 
      bindEvent(input$RUN.strip)
    
    
    upDateSites <- reactive({
      req(input$l.strip)
      locs <- as.numeric(input$l.strip)
      sites <- 1:locs
      return(list(sites = sites))
    }) %>% 
      bindEvent(input$RUN.strip)
    
    output$well_panel_layout_STRIP <- renderUI({
      req(strip_reactive()$fieldBook)
      obj_strip <- strip_reactive()
      allBooks_strip<- plot_layout(x = obj_strip, 
                                   layout = 1, 
                                   stacked = "vertical")$newBooks
      nBooks_strip <- length(allBooks_strip)
      layoutOptions_strip <- 1:nBooks_strip
      stacked_strips <- c("Vertical Stack Panel" = "vertical", 
                            "Horizontal Stack Panel" = "horizontal")
      sites <- as.numeric(input$l.strip)
      wellPanel(
        fluidRow(
          column(2,
                 radioButtons(ns("typlotstrip"), "Type of Plot:",
                              c("Entries/Treatments" = 1,
                                "Plots" = 2,
                                "Heatmap" = 3))
          ),
          column(3,
                 selectInput(inputId = ns("stackedSTRIP"), 
                             label = "Reps layout:", 
                             choices = stacked_strips),
          ),
          column(3,
                 selectInput(inputId = ns("layoutO_strip"), 
                             label = "Layout option:", 
                             choices = layoutOptions_strip)
          ),
          column(3,
                 selectInput(inputId = ns("locLayout_strip"), 
                             label = "Location:", 
                             choices = as.numeric(upDateSites()$sites))
          )
        )
      )
    })
    
    
    observeEvent(input$stackedSTRIP, {
      req(input$stackedSTRIP)
      req(input$l.strip)
      obj_strips <- strip_reactive()
      allBooks <- try(plot_layout(x = obj_strips, 
                                  layout = 1,
                                  planter = strip_inputs()$planter,
                                  stacked = input$stackedSTRIP)$newBooks, 
                      silent = TRUE)
      nBooks <- length(allBooks)
      NewlayoutOptions <- 1:nBooks
      updateSelectInput(session = session, 
                        inputId = 'layoutO_rcbd',
                        label = "Layout option:",
                        choices = NewlayoutOptions,
                        selected = 1
      )
    })
    
    
    reset_selection <- reactiveValues(reset = 0)
    
    observeEvent(input$stackedSTRIP, {
      reset_selection$reset <- 1
    })
    
    observeEvent(input$layoutO_strip, {
      reset_selection$reset <- 0
    })
    
    reactive_layoutSTRIP <- reactive({
      req(input$layoutO_strip)
      req(strip_reactive())
      obj_strip <- strip_reactive()
      planting_strip <- strip_inputs()$planter
      
      if (reset_selection$reset == 1) {
        opt_strip <- 1
      } else opt_strip <- as.numeric(input$layoutO_strip)
      
      locSelected <- as.numeric(input$locLayout_strip)
      try(plot_layout(x = obj_strip, 
                      layout = opt_strip, 
                      planter = planting_strip, 
                      stacked = input$stackedSTRIP,
                      l = locSelected), silent = TRUE)
    })
    
    valsStrip <- reactiveValues(maxV.strip = NULL, 
                                minV.strip = NULL, 
                                trail.strip = NULL)
    
    simuModal.strip <- function(failed = FALSE) {
      modalDialog(
        selectInput(inputId = ns("trailsStrip"), 
                    label = "Select One:", 
                    choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        conditionalPanel(
          condition = "input.trailsStrip == 'Other'", 
          ns = ns,
          textInput(inputId = ns("OtherStrip"), 
                    label = "Input Trial Name:", 
                    value = NULL)
        ),
        fluidRow(
          column(6, 
                 numericInput(ns("min.strip"), 
                              "Input the min value", 
                              value = NULL)
          ),
          column(6, 
                 numericInput(ns("max.strip"), 
                              "Input the max value", 
                              value = NULL)  
          )
        ),
        
        if (failed)
          div(tags$b("Invalid input of data max and min", 
                     style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ok.strip"), "GO")
        )
        
      )
      
    }
    
    observeEvent(input$Simulate.strip, {
      req(strip_reactive()$fieldBook)
      showModal(
        shinyjqui::jqui_draggable(
          simuModal.strip()
        )
      )
    })
    
    observeEvent(input$ok.strip, {
      req(input$max.strip, input$min.strip)
      if (input$max.strip > input$min.strip && input$min.strip != input$max.strip) {
        valsStrip$maxV.strip <- input$max.strip
        valsStrip$minV.strip <- input$min.strip
        if(input$trailsStrip == "Other") {
          req(input$OtherStrip)
          if(!is.null(input$OtherStrip)) {
            valsStrip$trail.strip <- input$OtherStrip
          }else showModal(simuModal.strip(failed = TRUE))
        }else {
          valsStrip$trail.strip <- as.character(input$trailsStrip)
        }
        removeModal()
      }else {
        showModal(
          shinyjqui::jqui_draggable(
            simuModal.strip(failed = TRUE)
          )
        )
      }
    })
    
    
    simuData_strip <- reactive({
      req(strip_reactive()$fieldBook)
      set.seed(input$seed.strip)
      if(!is.null(valsStrip$maxV.strip) && 
         !is.null(valsStrip$minV.strip) && 
         !is.null(valsStrip$trail.strip)) {
        max <- as.numeric(valsStrip$maxV.strip)
        min <- as.numeric(valsStrip$minV.strip)
        df.strip <- reactive_layoutSTRIP()$allSitesFieldbook
        cnamesdf.strip <- colnames(df.strip)
        df.strip <- norm_trunc(
          a = min, 
          b = max, 
          data = df.strip, 
          seed = strip_inputs()$seed
         )
        colnames(df.strip) <- c(cnamesdf.strip[1:(ncol(df.strip) - 1)], valsStrip$trail.strip)
        a <- ncol(df.strip)
      }else {
        df.strip <- reactive_layoutSTRIP()$allSitesFieldbook
        a <- ncol(df.strip)
      }
      return(list(df = df.strip, a = a))
    })
    
    heatmapInfoModal_STRIP <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    locNum <- reactive(
      return(as.numeric(input$locLayout_strip))
    )
    
    heatmap_obj <- reactive({
      req(simuData_strip()$df)
      if (ncol(simuData_strip()$df) == 10) {
        locs <- factor(simuData_strip()$df$LOCATION, levels = unique(simuData_strip()$df$LOCATION))
        locLevels <- levels(locs)
        df = subset(simuData_strip()$df, LOCATION == locLevels[1])
        loc <- levels(factor(df$LOCATION))
        trail <- as.character(valsStrip$trail.strip)
        label_trail <- paste(trail, ": ")
        heatmapTitle <- paste("Heatmap for ", trail)
        new_df <- df %>%
          dplyr::mutate(text = paste0("Site: ", loc, "\n", "Row: ", df$ROW, "\n", "Col: ", df$COLUMN, "\n", "Entry: ", 
                                      df$ENTRY, "\n", label_trail, round(df[,10],2)))
        w <- as.character(valsStrip$trail.strip)
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
        
        p2 <- plotly::ggplotly(p1, tooltip="text", height = 560)
        return(p2)
      } else {
        showModal(
          shinyjqui::jqui_draggable(
            heatmapInfoModal_STRIP()
          )
        )
        return(NULL)
      }
    })
    
    output$layout.strip <- plotly::renderPlotly({
      req(strip_reactive())
      req(input$typlotstrip)
      if (input$typlotstrip == 1) {
        reactive_layoutSTRIP()$out_layout
      } else if (input$typlotstrip == 2) {
        reactive_layoutSTRIP()$out_layoutPlots
      } else {
        req(heatmap_obj())
        heatmap_obj()
      }
    })
    
    output$STRIP.output <- DT::renderDataTable({
      
      df <- simuData_strip()$df
      
      df$LOCATION <- as.factor(df$LOCATION)
      df$PLOT <- as.factor(df$PLOT)
      df$ROW <- as.factor(df$ROW)
      df$COLUMN <- as.factor(df$COLUMN)
      df$REP <- as.factor(df$REP)
      df$HSTRIP <- as.factor(df$HSTRIP)
      df$VSTRIP <- as.factor(df$VSTRIP)
      df$TRT_COMB <- as.factor(df$TRT_COMB)
      
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      
      DT::datatable(df, 
                    filter = "top",
                    rownames = FALSE, 
                    options = list(
                      columnDefs = list(
                        list(className = 'dt-center', targets = "_all"))))
      
    })
    
    output$downloadData.strip <- downloadHandler(
      filename = function() {
        loc <- paste("Strip-Plot_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(simuData_strip()$df)
        write.csv(df, file, row.names = FALSE)
      }
    )
    csv_data <- reactive({
      req(simuData_strip()$df)
      df <- simuData_strip()$df
      req(input$typlotstrip)
      if (input$typlotstrip == 2) {
        export_layout(df, locNum(), TRUE)
      } else {
        export_layout(df, locNum())
      }
    })
    
    
    # Downloadable csv of selected dataset ----
    output$downloadCsv.strip <- downloadHandler(
      filename = function() {
        loc <- paste("Strip_Plot_Layout", sep = "")
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
# mod_STRIPD_ui("STRIPD_ui_1")
    
## To be copied in the server
# mod_STRIPD_server("STRIPD_ui_1")
