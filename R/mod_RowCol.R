#' RowCol UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_RowCol_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Row-Column Design"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 4,

                   shiny::radioButtons(inputId = ns("owndataRCD"),
                                label = "Import entries' list?",
                                choices = c("Yes", "No"), 
                                selected = "No",
                                inline = TRUE, 
                                width = NULL, 
                                choiceNames = NULL, 
                                choiceValues = NULL),
                   
                   shiny::conditionalPanel(
                     condition = "input.owndataRCD == 'Yes'", 
                     ns = ns,
                     shiny::fluidRow(
                       shiny::column(8, style=list("padding-right: 28px;"),
                              shiny::fileInput(ns("file.RCD"),
                                        label = "Upload a csv File:", 
                                        multiple = FALSE)),
                       shiny::column(4,style=list("padding-left: 5px;"),
                              shiny::radioButtons(ns("sep.rcd"), "Separator",
                                           choices = c(Comma = ",",
                                                       Semicolon = ";",
                                                       Tab = "\t"),
                                           selected = ","
                              )
                       )
                    )
                   ),
                   shiny::conditionalPanel(
                     condition = "input.owndataRCD != 'Yes'",
                     ns = ns,
                     shiny::numericInput(ns("t.rcd"),
                                  label = "Input # of Treatments:",
                                  value = 42,
                                  min = 2),
                   ),
                   shiny::fluidRow(
                     shiny::column(6, style=list("padding-right: 28px;"),
                            shiny::selectInput(inputId = ns("k.rcd"),
                                        label = "Input # of Rows:",
                                        choices = ""),
                     ),
                     shiny::column(6,style=list("padding-left: 5px;"),
                            shiny::numericInput(ns("r.rcd"),
                                         label = "Input # of Full Reps:",
                                         value = 2, 
                                         min = 2)
                     )
                   ),
                   shiny::numericInput(inputId = ns("l.rcd"),
                                label = "Input # of Locations:", 
                                value = 1, min = 1),
                   shiny::selectInput(inputId = ns("planter_mov_rcd"),
                               label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"),
                               multiple = FALSE,
                               selected = "serpentine"),
                   shiny::fluidRow(
                     shiny::column(6, style=list("padding-right: 28px;"),
                            shiny::textInput(ns("plot_start.rcd"),
                                      "Starting Plot Number:", 
                                      value = 101)
                     ),
                     shiny::column(6, style=list("padding-left: 5px;"),
                            shiny::textInput(ns("Location.rcd"),
                                      "Input Location:", 
                                      value = "FARGO")
                     )
                   ),
                   shiny::numericInput(ns("seed.rcd"),
                                label = "Random Seed:", 
                                value = 2437),
                   shiny::fluidRow(
                     shiny::column(6,
                            shiny::actionButton(
                              inputId = ns("RUN.rcd"), 
                              label = "Run!", 
                              icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                              width = '100%'),
                     ),
                     shiny::column(6,
                            shiny::actionButton(
                              ns("Simulate.RowCol"), 
                              label = "Simulate!", 
                              icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                              width = '100%'),
                     )
                   ), 
                   shiny::br(),
                   shiny::downloadButton(ns("downloadData.rowcolD"),
                                  "Save experiment (ZIP)",
                                  style = "width:100%")
      ),
      shiny::mainPanel(
        width = 8,
        shiny::fluidRow(
          shiny::tabsetPanel(
            shiny::tabPanel(
              "Summary Design",
              shiny::br(),
              fieldhub_spinner(
                shiny::verbatimTextOutput(outputId = ns("summary_row_column"),
                                   placeholder = FALSE), 
                type = 4
              ),
              style = "padding-right: 40px;"
            ),
            shiny::tabPanel("Field Layout",
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(shiny::downloadButton(ns("downloadCsv.rcd"),
                                                    label = "CSV + metadata (ZIP)",
                                                    icon = shiny::icon("download"),
                                                    width = 'auto',
                                                    style="color: #337ab7; background-color: #fff; border-color: #2e6da4")),
                     fieldhub_spinner(
                       plotly::plotlyOutput(ns("layouts"), 
                                            width = "97%", 
                                            height = "550px"),
                       type = 5),
                     shiny::br(),
                     shiny::column(12,shiny::uiOutput(ns("well_panel_layout_ROWCOL")))
            ),
            shiny::tabPanel("Field Book",
                     fieldhub_spinner(DT::DTOutput(ns("rowcolD")),
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
  shiny::moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    shinyjs::useShinyjs()
    
    entryListFormat_RCD <- data.frame(ENTRY = 1:9, 
                                       NAME = c(paste("Genotype", 
                                                      LETTERS[1:9], 
                                                      sep = "")))
    entriesInfoModal_RCD <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(entryListFormat_RCD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        shiny::h4("Entry numbers can be any set of consecutive positive numbers."),
        easyClose = FALSE
      )
    }
    
    toListen <- shiny::reactive({
      list(input$owndataRCD)
    })
    
    shiny::observeEvent(toListen(), {
      if (input$owndataRCD == "Yes") {
        shiny::showModal(
          entriesInfoModal_RCD()
        )
      }
    })
    
    init_data_rcd <- shiny::reactive({
      
      if (input$owndataRCD == "Yes") {
        shiny::req(input$file.RCD)
        inFile <- input$file.RCD
        data_ingested <- load_file(name = inFile$name, 
                                   path = inFile[["datapath"]],
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
        } else {
          app_upload_error(data_ingested,
                           missing_columns = "Data input needs at least two columns: ENTRY and NAME")
          return(NULL)
        }
      } else {
        shiny::req(input$t.rcd)
        nt <- as.numeric(input$t.rcd)
        df <- default_entries(nt)
        data_rcd <- df
        treatments = nrow(data_rcd)
        return(list(data_rcd = data_rcd, treatments = treatments))
      }
    })
    
    list_to_observe <- shiny::reactive({
      shiny::req(init_data_rcd())
      list(
        entry_list = input$owndataRCD,
        entries = init_data_rcd()$treatments
      )
    })
    
    shiny::observeEvent(list_to_observe(), {
      shiny::req(init_data_rcd())
      options <- valid_block_sizes(
        as.numeric(init_data_rcd()$treatments),
        "row_column"
      )
      k <- if (length(options) == 0L) "No Options Available" else options
      
      if (length(options) > 2L) {
        selected <- options[ceiling(length(options) / 2)]
      } else selected <- k[1]
      
      shiny::updateSelectInput(session = session,
                        inputId = 'k.rcd', 
                        label = "Input # of Rows:",
                        choices = k, 
                        selected = selected)
      
    })
    
    
    get_data_rcd <- shiny::reactive({
      if (is.null(init_data_rcd())) {
        shinyalert::shinyalert(
          "Error!!", 
          "Check input file and try again!", 
          type = "error")
        return(NULL)
      } else return(init_data_rcd())
    }) |>
      shiny::bindEvent(input$RUN.rcd)
    
    
    rcd_inputs <- shiny::reactive({
      shiny::req(get_data_rcd())
      shiny::req(input$k.rcd)
      shiny::req(input$r.rcd)
      shiny::req(input$plot_start.rcd)
      shiny::req(input$Location.rcd)
      shiny::req(input$seed.rcd)
      shiny::req(input$l.rcd)
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
      plot_start <- validate_design(read_whole_numbers(
        input$plot_start.rcd, "Starting Plot Number"
      ))
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
      shiny::bindEvent(input$RUN.rcd)
    
    RowCol_reactive <- shiny::reactive({
      
      shiny::req(rcd_inputs())
      shiny::req(get_data_rcd())
      
      shinyjs::show(id = "downloadCsv.rcd")
      
      data_rcd <- get_data_rcd()$data_rcd
      
      if (rcd_inputs()$r < 2) {
        shinyalert::shinyalert(
          "Error!!", 
          "Resolvable Row Columns Design needs at least 2 replicates.", 
          type = "error")
        return(NULL)
      }

      validate_design(row_column(
        t = rcd_inputs()$t, 
        nrows = rcd_inputs()$k, 
        reps = rcd_inputs()$r,
        l = rcd_inputs()$sites, 
        plotNumber = rcd_inputs()$plot_start, 
        seed = rcd_inputs()$seed,
        locationNames = rcd_inputs()$site_names, 
        data = data_rcd
      ))
      
    }) |>
      shiny::bindEvent(input$RUN.rcd)
    
    output$summary_row_column <- shiny::renderPrint({
      shiny::req(RowCol_reactive())
      cat("Randomization was successful!", "\n", "\n")
      print(RowCol_reactive(), n = 6)
    })
    
    upDateSites <- shiny::reactive({
      shiny::req(input$l.rcd)
      locs <- as.numeric(input$l.rcd)
      sites <- 1:locs
      return(list(sites = sites))
    }) |>
      shiny::bindEvent(input$RUN.rcd)
    
    output$well_panel_layout_ROWCOL <- shiny::renderUI({
      shiny::req(RowCol_reactive()$fieldBook)
      obj_rcd <- RowCol_reactive()
      layoutOptions_rcd <- validate_design(layout_choices(x = obj_rcd))
      stacked <- c("Vertical Stack Panel" = "vertical", 
                     "Horizontal Stack Panel" = "horizontal")
      shiny::wellPanel(
        shiny::column(2,
               shiny::radioButtons(ns("typlotrcd"), "Type of Plot:",
                            c("Entries/Treatments" = 1,
                              "Plots" = 2,
                              "Heatmap" = 3))
        ),
        shiny::fluidRow(
 
          shiny::column(3,
                 shiny::selectInput(inputId = ns("stackedRowCol"),
                             label = "Reps layout:", 
                             choices = stacked)
          ),
          shiny::column(3, #align="center",
                 shiny::selectInput(inputId = ns("layoutO_rcd"),
                             label = "Layout option:", 
                             choices = layoutOptions_rcd)
          ),
          shiny::column(3, #align="center",
                 shiny::selectInput(inputId = ns("locLayout_rcd"),
                             label = "Location:", 
                             choices = as.numeric(upDateSites()$sites))
          )
        )
      )
    })
    
    reactive_layoutROWCOL <- app_layout_selection(input, session,
      design = function() RowCol_reactive(),
      planter = function() rcd_inputs()$planter,
      ids = c(layout = "layoutO_rcd", stacked = "stackedRowCol", location = "locLayout_rcd")
    )
    
    simulation_settings <- app_simulation_controls(input, session,
      ids = c(trait = "trailsRowCol", other = "OtherRowCol", minimum = "min.RowCol", maximum = "max.RowCol", submit = "ok.RowCol"),
      field_book = function() reactive_layoutROWCOL()$allSitesFieldbook
    )
    
    simuModal.RowCol <- function(failed = FALSE) {
      app_simulation_modal(ns,
        ids = c(trait = "trailsRowCol", other = "OtherRowCol", minimum = "min.RowCol", maximum = "max.RowCol", submit = "ok.RowCol"),
        failed = failed)
    }
    
    shiny::observeEvent(input$Simulate.RowCol, {
      shiny::req(RowCol_reactive()$fieldBook)
      shiny::showModal(
        simuModal.RowCol()
      )
    })
    
    
    simuData_RowCol <- shiny::reactive({
      shiny::req(RowCol_reactive()$fieldBook)
      if (!is.null(simulation_settings())) {
        max <- as.numeric(simulation_settings()$max_value)
        min <- as.numeric(simulation_settings()$min_value)
        df.RowCol <- reactive_layoutROWCOL()$allSitesFieldbook
        simulation <- validate_design(simulate_classic_field_book(
          field_book = df.RowCol, min_value = min, max_value = max,
          response_name = simulation_settings()$response_name, seed = rcd_inputs()$seed,
          order_by_id = FALSE
        ))
        df.RowCol <- simulation$field_book
        a <- ncol(df.RowCol)
      }else {
        simulation <- NULL
        df.RowCol <- reactive_layoutROWCOL()$allSitesFieldbook
        a <- ncol(df.RowCol)
      }
      return(list(df = df.RowCol, a = a, simulation = simulation))
    })
    
    heatmapInfoModal_RCD <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    locNum <- shiny::reactive(
      return(as.numeric(input$locLayout_rcd))
    )
    
    heatmap_obj <- shiny::reactive({
      shiny::req(simuData_RowCol()$df)
      book <- simuData_RowCol()$df
      response <- as.character(simulation_settings()$response_name)
      if (length(response) == 1L && response %in% names(book)) {
        validate_design(app_field_heatmap(
          book, response_name = response, selected = locNum(),
          label_column = "ENTRY", label_title = "Entry",
          include_site = TRUE, include_checks = FALSE
        ))
      } else {
        shiny::showModal(heatmapInfoModal_RCD())
        NULL
      }
    })
    
    output$layouts <- plotly::renderPlotly({
      shiny::req(RowCol_reactive())
      shiny::req(input$typlotrcd)
      if (input$typlotrcd == 1) {
        reactive_layoutROWCOL()$out_layout
      } else if (input$typlotrcd == 2) {
        reactive_layoutROWCOL()$out_layoutPlots
      } else {
        shiny::req(heatmap_obj())
        heatmap_obj()
      }
    })
    
    output$rowcolD <- DT::renderDataTable({
      df <- simuData_RowCol()$df
      validate_design(app_field_book_table(
        df, factor_columns = c("LOCATION", "PLOT", "ROW", "COLUMN", "REP", "ENTRY"),
        height = 490
      ))
    })

    output$downloadData.rowcolD <- app_csv_archive(
      filename = function() {
        loc <- paste("Row-Column_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      data = function() as.data.frame(simuData_RowCol()$df),
      design = RowCol_reactive,
      field_book = function() simuData_RowCol()$df,
      simulation = function() simuData_RowCol()$simulation,
      layout = function() reactive_layoutROWCOL()$layout_metadata,
      kind = "field_book"
    )
    
    csv_data <- shiny::reactive({
      shiny::req(simuData_RowCol()$df)
      df <- simuData_RowCol()$df
      shiny::req(input$typlotrcd)
      if (input$typlotrcd == 2) {
        export_layout(df, locNum(), TRUE)
      } else {
        export_layout(df, locNum())
      }
    })
    
    
    # Downloadable csv of selected dataset ----
    output$downloadCsv.rcd <- app_csv_archive(
      filename = function() {
        loc <- paste("Resolvable_Row-Column_Layout", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      data = function() as.data.frame(csv_data()$file),
      design = RowCol_reactive,
      field_book = function() simuData_RowCol()$df,
      simulation = function() simuData_RowCol()$simulation,
      layout = function() reactive_layoutROWCOL()$layout_metadata,
      kind = "layout"
    )
    app_reproduction_outputs(output, RowCol_reactive)
  })
}
