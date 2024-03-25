#' SSPD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_SSPD_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("Split-Split-Plot Design"),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        radioButtons(inputId = ns("owndataSSPD"), 
                     label = "Do you have your own data?", 
                     choices = c("Yes", "No"), 
                     selected = "No",
                     inline = TRUE, 
                     width = NULL, 
                     choiceNames = NULL, 
                     choiceValues = NULL),
        
        selectInput(inputId = ns("kindSSPD"), 
                    label = "Select SSPD Type:",
                    choices = c("Split-Split Plot in a RCBD" = "SSPD_RCBD", 
                                "Split-Split Plot in a CRD" = "SSPD_CRD"),
                    multiple = FALSE),
        
        conditionalPanel(
          condition = "input.owndataSSPD == 'Yes'", 
          ns = ns,
          fluidRow(
            column(8, style=list("padding-right: 28px;"),
                   fileInput(ns("file.SSPD"), 
                             label = "Upload a csv File:", 
                             multiple = FALSE)),
            
            column(4,style=list("padding-left: 5px;"),
                   radioButtons(ns("sep.sspd"), "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","))
          )          
        ),
        
        conditionalPanel(
          condition = "input.owndataSSPD != 'Yes'", 
          ns = ns,
          numericInput(ns("mp.sspd"), 
                       label = "Whole-plots:",
                       value = 2, 
                       min = 2),
          numericInput(ns("sp.sspd"), 
                       label = "Sub-plots Within Whole-plots:",
                       value = 2, 
                       min = 2),
          numericInput(ns("ssp.sspd"), 
                       label = "Sub-Sub-plots within Sub-plots:",
                       value = 5, 
                       min = 2)
          ),
        
        fluidRow(
          column(6, style=list("padding-right: 28px;"),
            numericInput(ns("reps.sspd"), 
                         label = "Input # of Full Reps:",
                         value = 3, 
                         min = 2)
          ),
          column(6, style=list("padding-left: 5px;"),
            numericInput(ns("l.sspd"), 
                         label = "Input # of Locations:",
                         value = 1, 
                         min = 1)
          )
        ), 
        
        selectInput(inputId = ns("planter_mov_sspd"), 
                    label = "Plot Order Layout:",
                    choices = c("serpentine", "cartesian"), 
                    multiple = FALSE,
                    selected = "serpentine"),
        
        fluidRow(
          column(6,style=list("padding-right: 28px;"),
                 textInput(ns("plot_start.sspd"), 
                           "Starting Plot Number:", 
                           value = 101)
          ),
          column(6,style=list("padding-left: 5px;"),
                 textInput(ns("Location.sspd"), "
                           Input Location:", 
                           value = "FARGO")
          )
        ),
        
        numericInput(inputId = ns("seed.sspd"), 
                     label = "Random Seed:", 
                     value = 123, 
                     min = 1),
        
        fluidRow(
          column(6,
                 actionButton(
                   inputId = ns("RUN.sspd"), 
                   "Run!", 
                   icon = icon("circle-nodes", verify_fa = FALSE),
                   width = '100%'),
          ),
          column(6,
                 actionButton(
                   ns("Simulate.sspd"), 
                   "Simulate!", 
                   icon = icon("greater-than-equal", verify_fa = FALSE),
                   width = '100%'),
          )
          
        ), 
        br(),
        downloadButton(
          ns("downloadData.sspd"), 
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
                         ns("downloadCsv.sspd"), 
                         label =  "Excel",
                         icon = icon("file-csv"), 
                         width = '10%',
                         style="color: #337ab7; background-color: #fff; border-color: #2e6da4")),
                     shinycssloaders::withSpinner(
                       plotly::plotlyOutput(ns("layouts"), 
                                            width = "97%", 
                                            height = "580px"),
                       type = 5
                     ),
                     br(),
                     column(12,
                            uiOutput(ns("well_panel_layout_SSPD"))
                            )
            ),
            tabPanel("Field Book", 
                     shinycssloaders::withSpinner(
                       DT::DTOutput(ns("SSPD.output")), 
                       type = 5)
            )
          )
        )
      )
    )
  )
}
    
#' SSPD Server Functions
#'
#' @noRd 
mod_SSPD_server <- function(id){
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    shinyjs::useShinyjs()
    
   wp <- paste("IRR_", c("NO", "Yes"), sep = "") 
   sp <- c("NFung", paste("Fung", 1:4, sep = "")) 
   ssp <- paste("Beans", 1:10, sep = "") 
   entryListFormat_SSPD <- data.frame(list(WHOLPLOT = c(wp, rep("", 8)), 
                                            SUBPLOT = c(sp, rep("", 5)),
                                            SUB_SUBPLOT = ssp))            
  
    entriesInfoModal_SSPD <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        renderTable(entryListFormat_SSPD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        easyClose = FALSE
      )
    }
    
    toListen <- reactive({
      list(input$owndataSSPD)
    })
    
    observeEvent(toListen(), {
      if (input$owndataSSPD == "Yes") {
        showModal(
          entriesInfoModal_SSPD()
        )
      }
    })
    
    get_data_sspd <- reactive({
      if (input$owndataSSPD == "Yes") {
        req(input$file.SSPD)
        inFile <- input$file.SSPD
        data_ingested <- load_file(name = inFile$name, 
                                   path = inFile$datapat, 
                                   sep = input$sep.sspd, 
                                   check = TRUE, 
                                   design = "sspd")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_sspd <- as.data.frame(data_up[, 1:3])
          colnames(data_sspd) <- c("WHOLEPLOT", "SUBPLOT", "SUB_SUBPLOT")
          wp <- as.vector(na.omit(data_sspd[,1]))
          sp <- as.vector(na.omit(data_sspd[,2]))
          ssp <- as.vector(na.omit(data_sspd[,3]))
          treatments <- c(wp, sp, ssp)
          return(list(data_sspd = data_sspd, treatments = treatments))
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
            "Data input needs at least two column: WHOLEPLOT, SUBPLOT, and SUB_SUBPLOT", 
            type = "error")
          return(NULL)
        }
      } else {
        req(input$mp.sspd, input$sp.sspd, input$ssp.sspd)
        wp <- as.numeric(input$mp.sspd)
        sp <- as.numeric(input$sp.sspd)
        ssp <- as.numeric(input$ssp.sspd)
        treatments <- c(wp, sp, ssp)
        data_spd <- NULL
        return(list(data_spd = data_spd, treatments = treatments))
      }
    }) |> 
      bindEvent(input$RUN.sspd)
    
    sspd_inputs <- reactive({
      
      req(get_data_sspd())
      
      req(input$plot_start.sspd)
      req(input$Location.sspd)
      req(input$seed.sspd)
      req(input$l.sspd)
      req(input$reps.sspd)
      
      sites <- as.numeric(input$l.sspd)
      seed <- as.numeric(input$seed.sspd)
      plot_start.sspd <- as.vector(unlist(strsplit(input$plot_start.sspd, ",")))
      plot_start <- as.numeric(plot_start.sspd)
      site_names <-  as.vector(unlist(strsplit(input$Location.sspd, ",")))
      reps <- as.numeric(input$reps.sspd)
      planter <- input$planter_mov_sspd
      data_sspd <- get_data_sspd()$data_sspd
      
      if (input$kindSSPD == "SSPD_RCBD") {
        type_design <- 2
      } else type_design <- 1
      
      return(
        list(
          wp = get_data_sspd()$treatments[1], 
          sp = get_data_sspd()$treatments[2], 
          ssp = get_data_sspd()$treatments[3], 
          r = reps, 
          sites = sites,
          seed = seed,
          planter = planter,
          plot_start = plot_start,
          site_names = site_names, 
          type_design = type_design,
          data = data_sspd
        )
      )
    }) |>
      bindEvent(input$RUN.sspd)
    
    
    sspd_reactive <- reactive({
      
      req(sspd_inputs())
      
      shinyjs::show(id = "downloadCsv.sspd")
      
      split_split_plot(
        wp = sspd_inputs()$wp, 
        sp = sspd_inputs()$sp, 
        ssp = sspd_inputs()$ssp, 
        reps = sspd_inputs()$r, 
        l = sspd_inputs()$sites, 
        plotNumber = sspd_inputs()$plot_start, 
        seed = sspd_inputs()$seed, 
        type = sspd_inputs()$type_design, 
        locationNames = sspd_inputs()$site_names, 
        data = sspd_inputs()$data
      )
      
    }) |> 
      bindEvent(input$RUN.sspd)
  
    output$well_panel_layout_SSPD <- renderUI({
      req(sspd_reactive()$fieldBook)
      obj_sspd <- sspd_reactive()
      allBooks_sspd<- plot_layout(x = obj_sspd, layout = 1)$newBooks
      nBooks_sspd <- length(allBooks_sspd)
      layoutOptions_sspd <- 1:nBooks_sspd
      df <- sspd_reactive()$fieldBook
      stacked_sspd <- c("Vertical Stack Panel" = "vertical", 
                          "Horizontal Stack Panel" = "horizontal")
      sites <- 1:length(levels(as.factor(df$LOCATION)))
      wellPanel(
        column(2,
               radioButtons(ns("typlotsspd"), "Type of Plot:",
                            c("Entries/Treatments" = 1,
                              "Plots" = 2,
                              "Heatmap" = 3), selected = 1)
        ),
        fluidRow(
          column(3,
                 selectInput(inputId = ns("stackedSSPD"), 
                             label = "Reps layout:", 
                             choices = stacked_sspd),
          ),
          column(3, 
                 selectInput(inputId = ns("layoutO_sspd"), 
                             label = "Layout option:", 
                             choices = layoutOptions_sspd)
          ),
          column(3, 
                 selectInput(inputId = ns("locLayout_sspd"), 
                             label = "Location:", 
                             choices = sites) 
          )
        )
      )
    })
    
    observeEvent(input$stackedSSPD, {
      req(input$stackedSPD)
      obj_sspd <- sspd_reactive()
      allBooks <- try(plot_layout(x = obj_sspd, 
                                  layout = 1, 
                                  stacked = input$stackedSPD)$newBooks, 
                      silent = TRUE)
      nBooks <- length(allBooks)
      NewlayoutOptions <- 1:nBooks
      updateSelectInput(session = session, inputId = 'layoutO_sspd',
                        label = "Layout option:",
                        choices = NewlayoutOptions,
                        selected = 1
      )
    })
    
    reset_selection <- reactiveValues(reset = 0)
    
    observeEvent(input$stackedSSPD, {
      reset_selection$reset <- 1
    })
    
    observeEvent(input$layoutO_sspd, {
      reset_selection$reset <- 0
    })
    
    reactive_layoutSSPD <- reactive({
      req(input$layoutO_sspd)
      req(sspd_reactive())
      obj_sspd <- sspd_reactive()
      planting_sspd <- sspd_inputs()$planter
      
      if (reset_selection$reset == 1) {
        opt_sspd <- 1
      } else opt_sspd <- as.numeric(input$layoutO_sspd)
      
      locSelected <- as.numeric(input$locLayout_sspd)
      try(plot_layout(x = obj_sspd, 
                      layout = opt_sspd, 
                      stacked = input$stackedSSPD,
                      planter = planting_sspd, 
                      l = locSelected), 
          silent = TRUE)
    })
    
    
    valsspd <- reactiveValues(maxV.sspd = NULL, minV.sspd = NULL, Trial.sspd = NULL)
    
    simuModal.sspd <- function(failed = FALSE) {
      modalDialog(
        selectInput(inputId = ns("TrialsRowCol"), 
                    label = "Select One:", 
                    choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        conditionalPanel("input.TrialsRowCol == 'Other'", 
                         ns = ns,
                         textInput(inputId = ns("Otherspd"),
                                   label = "Input Trial Name:",
                                   value = NULL)
        ),
        fluidRow(
          column(6, 
                 numericInput(ns("min.sspd"), "Input the min value", value = NULL)
          ),
          column(6, 
                 numericInput(ns("max.sspd"), "Input the max value", value = NULL)
                 
          )
          
        ),
        
        if (failed)
          div(tags$b("Invalid input of data max and min", style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("ok.sspd"), "GO")
        )
        
      )
      
    }
    
    observeEvent(input$Simulate.sspd, {
      req(sspd_reactive()$fieldBook)
      showModal(
        simuModal.sspd()
      )
    })
    
    observeEvent(input$ok.sspd, {
      req(input$max.sspd, input$min.sspd)
      if (input$max.sspd > input$min.sspd && input$min.sspd != input$max.sspd) {
        valsspd$maxV.sspd <- input$max.sspd
        valsspd$minV.sspd <- input$min.sspd
        if(input$TrialsRowCol == "Other") {
          req(input$Otherspd)
          if(!is.null(input$Otherspd)) {
            valsspd$Trial.sspd <- input$Otherspd
          }else showModal(simuModal.sspd(failed = TRUE))
        }else {
          valsspd$Trial.sspd <- as.character(input$TrialsRowCol)
        }
        removeModal()
      }else {
        showModal(
          simuModal.sspd(failed = TRUE)
        )
      }
    })
    
    simuData_sspd <- reactive({
      req(sspd_reactive()$fieldBook)
      
      if(!is.null(valsspd$maxV.sspd) && !is.null(valsspd$minV.sspd) && 
         !is.null(valsspd$Trial.sspd)) {
        max <- as.numeric(valsspd$maxV.sspd)
        min <- as.numeric(valsspd$minV.sspd)
        df.sspd <- reactive_layoutSSPD()$allSitesFieldbook
        cnamesdf.sspd <- colnames(df.sspd)
        df.sspd <- norm_trunc(
          a = min, 
          b = max, 
          data = df.sspd, 
          seed = sspd_inputs()$seed
        )
        colnames(df.sspd) <- c(cnamesdf.sspd[1:(ncol(df.sspd) - 1)], valsspd$Trial.sspd)
        df.sspd <- df.sspd[order(df.sspd$ID),]
      }else {
        df.sspd <- reactive_layoutSSPD()$allSitesFieldbook
      }
      return(list(df = df.sspd))
    })
    
    
    heatmapInfoModal_SSPD <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    locNum <- reactive(
      return(as.numeric(input$locLayout_sspd))
    )
    
    heatmap_obj <- reactive({
     req(simuData_sspd()$df)
      if (ncol(simuData_sspd()$df) == 11) {
        locs <- factor(simuData_sspd()$df$LOCATION, 
                       levels = unique(simuData_sspd()$df$LOCATION))
        locLevels <- levels(locs)
        df = subset(simuData_sspd()$df, LOCATION == locLevels[locNum()])
        loc <- levels(factor(df$LOCATION))
        trail <- as.character(valsspd$Trial.sspd)
        label_trail <- paste(trail, ": ")
        heatmapTitle <- paste("Heatmap for ", trail)
        new_df <- df |>
          dplyr::mutate(text = paste0("Site: ", loc, "\n", 
                                      "Row: ", df$ROW, "\n", 
                                      "Col: ", df$COLUMN, "\n", 
                                      "TRT_COMB: ", df$TRT_COMB,"\n", 
                                      label_trail, round(df[,11],2)))
        w <- as.character(valsspd$Trial.sspd)
        new_df$ROW <- as.factor(new_df$ROW) # Set up ROWS as factors
        new_df$COLUMN <- as.factor(new_df$COLUMN) # Set up COLUMNS as factors
        p1 <- ggplot2::ggplot(new_df, 
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
          ggplot2::theme(plot.title = ggplot2::element_text(family="Calibri", 
                                                            face="bold", 
                                                            size=13, 
                                                            hjust=0.5))
        
        p2 <- plotly::ggplotly(p1, tooltip="text", 
                               height = 580)
        return(p2)
      } else {
        showModal(
          heatmapInfoModal_SSPD()
        )
        return(NULL)
      }
    })
    
    output$layouts <- plotly::renderPlotly({
      req(reactive_layoutSSPD())
      req(sspd_reactive())
      req(input$typlotsspd)
      if (input$typlotsspd == 1) {
        reactive_layoutSSPD()$out_layout
      } else if (input$typlotsspd == 2) {
        reactive_layoutSSPD()$out_layoutPlots
      } else {
        req(heatmap_obj())
        heatmap_obj()
      }
    })
    
    output$SSPD.output  <- DT::renderDataTable({
      
      df <- simuData_sspd()$df
      df$LOCATION <- as.factor(df$LOCATION)
      df$PLOT <- as.factor(df$PLOT)
      df$ROW <- as.factor(df$ROW)
      df$COLUMN <- as.factor(df$COLUMN)
      df$REP <- as.factor(df$REP)
      df$WHOLE_PLOT <- as.factor(df$WHOLE_PLOT)
      df$SUB_PLOT <- as.factor(df$SUB_PLOT)
      df$SUB_SUB_PLOT <- as.factor(df$SUB_SUB_PLOT)
      df$TRT_COMB <- as.factor(df$TRT_COMB)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      
      DT::datatable(df, 
                    filter = 'top',
                    rownames = FALSE, 
                    options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
      
    })
    
    output$downloadData.sspd <- downloadHandler(
      filename = function() {
        loc <- paste("Split-Split-Plot_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(simuData_sspd()$df)
        write.csv(df, file, row.names = FALSE)
      }
    )
    csv_data <- reactive({
      req(simuData_sspd()$df)
      df <- simuData_sspd()$df
      req(input$typlotsspd)
      if (input$typlotsspd == 2) {
        export_layout(df, locNum(), TRUE)
      } else {
        export_layout(df, locNum())
      }
    })
    
    
    # Downloadable csv of selected dataset ----
    output$downloadCsv.sspd <- downloadHandler(
      filename = function() {
        loc <- paste("Split_Split_Plot_Layout", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(csv_data()$file)
        write.csv(df, file, row.names = FALSE)
      }
    )
  })
}
