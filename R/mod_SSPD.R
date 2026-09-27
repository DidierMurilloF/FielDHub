#' SSPD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_SSPD_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Split-Split-Plot Design"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        shiny::radioButtons(inputId = ns("owndataSSPD"),
                     label = "Do you have your own data?", 
                     choices = c("Yes", "No"), 
                     selected = "No",
                     inline = TRUE, 
                     width = NULL, 
                     choiceNames = NULL, 
                     choiceValues = NULL),
        
        shiny::selectInput(inputId = ns("kindSSPD"),
                    label = "Select SSPD Type:",
                    choices = c("Split-Split Plot in a RCBD" = "SSPD_RCBD", 
                                "Split-Split Plot in a CRD" = "SSPD_CRD"),
                    multiple = FALSE),
        
        shiny::conditionalPanel(
          condition = "input.owndataSSPD == 'Yes'", 
          ns = ns,
          shiny::fluidRow(
            shiny::column(8, style=list("padding-right: 28px;"),
                   shiny::fileInput(ns("file.SSPD"),
                             label = "Upload a csv File:", 
                             multiple = FALSE)),
            
            shiny::column(4,style=list("padding-left: 5px;"),
                   shiny::radioButtons(ns("sep.sspd"), "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","))
          )          
        ),
        
        shiny::conditionalPanel(
          condition = "input.owndataSSPD != 'Yes'", 
          ns = ns,
          shiny::numericInput(ns("mp.sspd"),
                       label = "Whole-plots:",
                       value = 2, 
                       min = 2),
          shiny::numericInput(ns("sp.sspd"),
                       label = "Sub-plots Within Whole-plots:",
                       value = 2, 
                       min = 2),
          shiny::numericInput(ns("ssp.sspd"),
                       label = "Sub-Sub-plots within Sub-plots:",
                       value = 5, 
                       min = 2)
          ),
        
        shiny::fluidRow(
          shiny::column(6, style=list("padding-right: 28px;"),
            shiny::numericInput(ns("reps.sspd"),
                         label = "Input # of Full Reps:",
                         value = 3, 
                         min = 2)
          ),
          shiny::column(6, style=list("padding-left: 5px;"),
            shiny::numericInput(ns("l.sspd"),
                         label = "Input # of Locations:",
                         value = 1, 
                         min = 1)
          )
        ), 
        
        # The RCBD-type layouts number whole plots in a fixed order, so the
        # plot order only applies to the CRD type
        shiny::conditionalPanel("input.kindSSPD == 'SSPD_CRD'", ns = ns,
          shiny::selectInput(inputId = ns("planter_mov_sspd"),
                      label = "Plot Order Layout:",
                      choices = c("serpentine", "cartesian"), 
                      multiple = FALSE,
                      selected = "serpentine")
        ),
        
        shiny::fluidRow(
          shiny::column(6,style=list("padding-right: 28px;"),
                 shiny::textInput(ns("plot_start.sspd"),
                           "Starting Plot Number:", 
                           value = 101)
          ),
          shiny::column(6,style=list("padding-left: 5px;"),
                 shiny::textInput(ns("Location.sspd"), "
                           Input Location:", 
                           value = "FARGO")
          )
        ),
        
        shiny::numericInput(inputId = ns("seed.sspd"),
                     label = "Random Seed:", 
                     value = 123, 
                     min = 1),
        
        shiny::fluidRow(
          shiny::column(6,
                 shiny::actionButton(
                   inputId = ns("RUN.sspd"), 
                   "Run!", 
                   icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                   width = '100%'),
          ),
          shiny::column(6,
                 shiny::actionButton(
                   ns("Simulate.sspd"), 
                   "Simulate!", 
                   icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                   width = '100%'),
          )
          
        ), 
        shiny::br(),
        shiny::downloadButton(
          ns("downloadData.sspd"), 
          "Save experiment (ZIP)",
          style = "width:100%")
      ),
      
      shiny::mainPanel(
        width = 8,
        shiny::fluidRow(
          shiny::tabsetPanel(
            shiny::tabPanel("Field Layout",
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(
                       shiny::downloadButton(
                         ns("downloadCsv.sspd"), 
                         label = "CSV + metadata (ZIP)",
                         icon = shiny::icon("download"),
                         width = 'auto',
                         style="color: #337ab7; background-color: #fff; border-color: #2e6da4")),
                     fieldhub_spinner(
                       plotly::plotlyOutput(ns("layouts"), 
                                            width = "97%", 
                                            height = "580px"),
                       type = 5
                     ),
                     shiny::br(),
                     shiny::column(12,
                            shiny::uiOutput(ns("well_panel_layout_SSPD"))
                            )
            ),
            shiny::tabPanel("Field Book",
                     fieldhub_spinner(
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
  shiny::moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    shinyjs::useShinyjs()
    
   wp <- paste("IRR_", c("NO", "Yes"), sep = "") 
   sp <- c("NFung", paste("Fung", 1:4, sep = "")) 
   ssp <- paste("Beans", 1:10, sep = "") 
   entryListFormat_SSPD <- data.frame(list(WHOLPLOT = c(wp, rep("", 8)), 
                                            SUBPLOT = c(sp, rep("", 5)),
                                            SUB_SUBPLOT = ssp))            
  
    entriesInfoModal_SSPD <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(entryListFormat_SSPD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        easyClose = FALSE
      )
    }
    
    toListen <- shiny::reactive({
      list(input$owndataSSPD)
    })
    
    shiny::observeEvent(toListen(), {
      if (input$owndataSSPD == "Yes") {
        shiny::showModal(
          entriesInfoModal_SSPD()
        )
      }
    })
    
    get_data_sspd <- shiny::reactive({
      if (input$owndataSSPD == "Yes") {
        shiny::req(input$file.SSPD)
        inFile <- input$file.SSPD
        data_ingested <- load_file(name = inFile$name, 
                                   path = inFile[["datapath"]],
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
        } else {
          app_upload_error(data_ingested,
                           missing_columns = "Data input needs at least two column: WHOLEPLOT, SUBPLOT, and SUB_SUBPLOT")
          return(NULL)
        }
      } else {
        shiny::req(input$mp.sspd, input$sp.sspd, input$ssp.sspd)
        wp <- as.numeric(input$mp.sspd)
        sp <- as.numeric(input$sp.sspd)
        ssp <- as.numeric(input$ssp.sspd)
        treatments <- c(wp, sp, ssp)
        data_spd <- NULL
        return(list(data_spd = data_spd, treatments = treatments))
      }
    }) |> 
      shiny::bindEvent(input$RUN.sspd)
    
    sspd_inputs <- shiny::reactive({
      
      shiny::req(get_data_sspd())
      
      shiny::req(input$plot_start.sspd)
      shiny::req(input$Location.sspd)
      shiny::req(input$seed.sspd)
      shiny::req(input$l.sspd)
      shiny::req(input$reps.sspd)
      
      sites <- as.numeric(input$l.sspd)
      seed <- as.numeric(input$seed.sspd)
      plot_start <- validate_design(read_whole_numbers(
        input$plot_start.sspd, "Starting Plot Number"
      ))
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
      shiny::bindEvent(input$RUN.sspd)
    
    
    sspd_reactive <- shiny::reactive({
      
      shiny::req(sspd_inputs())
      
      shinyjs::show(id = "downloadCsv.sspd")
      
      validate_design(split_split_plot(
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
      ))
      
    }) |> 
      shiny::bindEvent(input$RUN.sspd)
  
    output$well_panel_layout_SSPD <- shiny::renderUI({
      shiny::req(sspd_reactive()$fieldBook)
      obj_sspd <- sspd_reactive()
      layoutOptions_sspd <- validate_design(layout_choices(x = obj_sspd))
      df <- sspd_reactive()$fieldBook
      stacked_sspd <- c("Vertical Stack Panel" = "vertical", 
                          "Horizontal Stack Panel" = "horizontal")
      sites <- 1:length(levels(as.factor(df$LOCATION)))
      shiny::wellPanel(
        shiny::column(2,
               shiny::radioButtons(ns("typlotsspd"), "Type of Plot:",
                            c("Entries/Treatments" = 1,
                              "Plots" = 2,
                              "Heatmap" = 3), selected = 1)
        ),
        shiny::fluidRow(
          shiny::column(3,
                 shiny::selectInput(inputId = ns("stackedSSPD"),
                             label = "Reps layout:", 
                             choices = stacked_sspd),
          ),
          shiny::column(3,
                 shiny::selectInput(inputId = ns("layoutO_sspd"),
                             label = "Layout option:", 
                             choices = layoutOptions_sspd)
          ),
          shiny::column(3,
                 shiny::selectInput(inputId = ns("locLayout_sspd"),
                             label = "Location:", 
                             choices = sites) 
          )
        )
      )
    })
    
    shiny::observeEvent(input$stackedSSPD, {
      shiny::req(input$stackedSSPD)
      obj_sspd <- sspd_reactive()
      NewlayoutOptions <- validate_design(layout_choices(x = obj_sspd, stacked = input$stackedSSPD))
      shiny::updateSelectInput(session = session, inputId = 'layoutO_sspd',
                        label = "Layout option:",
                        choices = NewlayoutOptions,
                        selected = 1
      )
    })
    
    reset_selection <- shiny::reactiveValues(reset = 0)
    
    shiny::observeEvent(input$stackedSSPD, {
      reset_selection$reset <- 1
    })
    
    shiny::observeEvent(input$layoutO_sspd, {
      reset_selection$reset <- 0
    })
    
    reactive_layoutSSPD <- shiny::reactive({
      shiny::req(input$layoutO_sspd)
      shiny::req(sspd_reactive())
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
    
    
    simulation_settings <- app_simulation_controls(input, session,
      ids = c(trait = "TrialsRowCol", other = "Otherspd", minimum = "min.sspd", maximum = "max.sspd", submit = "ok.sspd"),
      field_book = function() reactive_layoutSSPD()$allSitesFieldbook
    )
    
    simuModal.sspd <- function(failed = FALSE) {
      app_simulation_modal(ns,
        ids = c(trait = "TrialsRowCol", other = "Otherspd", minimum = "min.sspd", maximum = "max.sspd", submit = "ok.sspd"),
        failed = failed)
    }
    
    shiny::observeEvent(input$Simulate.sspd, {
      shiny::req(sspd_reactive()$fieldBook)
      shiny::showModal(
        simuModal.sspd()
      )
    })
    
    
    simuData_sspd <- shiny::reactive({
      shiny::req(sspd_reactive()$fieldBook)
      
      if (!is.null(simulation_settings())) {
        max <- as.numeric(simulation_settings()$max_value)
        min <- as.numeric(simulation_settings()$min_value)
        df.sspd <- reactive_layoutSSPD()$allSitesFieldbook
        simulation <- validate_design(simulate_classic_field_book(
          field_book = df.sspd, min_value = min, max_value = max,
          response_name = simulation_settings()$response_name, seed = sspd_inputs()$seed,
          order_by_id = TRUE
        ))
        df.sspd <- simulation$field_book
      }else {
        simulation <- NULL
        df.sspd <- reactive_layoutSSPD()$allSitesFieldbook
      }
      return(list(df = df.sspd, simulation = simulation))
    })
    
    
    heatmapInfoModal_SSPD <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    locNum <- shiny::reactive(
      return(as.numeric(input$locLayout_sspd))
    )
    
    heatmap_obj <- shiny::reactive({
      shiny::req(simuData_sspd()$df)
      book <- simuData_sspd()$df
      response <- as.character(simulation_settings()$response_name)
      if (length(response) == 1L && response %in% names(book)) {
        validate_design(app_field_heatmap(
          book, response_name = response, selected = locNum(),
          label_column = "TRT_COMB", label_title = "TRT_COMB",
          include_site = TRUE, include_checks = FALSE,
          height = 580
        ))
      } else {
        shiny::showModal(heatmapInfoModal_SSPD())
        NULL
      }
    })
    
    output$layouts <- plotly::renderPlotly({
      shiny::req(reactive_layoutSSPD())
      shiny::req(sspd_reactive())
      shiny::req(input$typlotsspd)
      if (input$typlotsspd == 1) {
        reactive_layoutSSPD()$out_layout
      } else if (input$typlotsspd == 2) {
        reactive_layoutSSPD()$out_layoutPlots
      } else {
        shiny::req(heatmap_obj())
        heatmap_obj()
      }
    })
    
    output$SSPD.output  <- DT::renderDataTable({
      
      df <- simuData_sspd()$df
      validate_design(app_field_book_table(
        df, factor_columns = c("LOCATION", "PLOT", "ROW", "COLUMN", "REP", "WHOLE_PLOT", "SUB_PLOT", "SUB_SUB_PLOT", "TRT_COMB" ),
        height = 500
      ))
    })
    
    output$downloadData.sspd <- app_csv_archive(
      filename = function() {
        loc <- paste("Split-Split-Plot_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      data = function() as.data.frame(simuData_sspd()$df),
      design = sspd_reactive,
      field_book = function() simuData_sspd()$df,
      simulation = function() simuData_sspd()$simulation,
      layout = function() reactive_layoutSSPD()$layout_metadata,
      kind = "field_book"
    )
    csv_data <- shiny::reactive({
      shiny::req(simuData_sspd()$df)
      df <- simuData_sspd()$df
      shiny::req(input$typlotsspd)
      if (input$typlotsspd == 2) {
        export_layout(df, locNum(), TRUE)
      } else {
        export_layout(df, locNum())
      }
    })
    
    
    # Downloadable csv of selected dataset ----
    output$downloadCsv.sspd <- app_csv_archive(
      filename = function() {
        loc <- paste("Split_Split_Plot_Layout", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      data = function() as.data.frame(csv_data()$file),
      design = sspd_reactive,
      field_book = function() simuData_sspd()$df,
      simulation = function() simuData_sspd()$simulation,
      layout = function() reactive_layoutSSPD()$layout_metadata,
      kind = "layout"
    )
    app_reproduction_outputs(output, sspd_reactive)
  })
}
