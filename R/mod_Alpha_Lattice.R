#' Alpha_Lattice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom utils write.csv
mod_Alpha_Lattice_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::h4("Alpha Lattice Design"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 4,
                   shiny::radioButtons(ns("owndata_alpha"), label = "Import entries' list?", choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                   
                   shiny::conditionalPanel("input.owndata_alpha != 'Yes'", ns = ns,
                                    shiny::numericInput(ns("t.alpha"), label = "Input # of Treatments:",
                                                 value = 36, min = 2)
                                    
                   ),
                   shiny::conditionalPanel("input.owndata_alpha == 'Yes'", ns = ns,
                                    shiny::fluidRow(
                                      shiny::column(8, style=list("padding-right: 28px;"),
                                             shiny::fileInput(inputId = ns("file.alpha"), label = "Upload a CSV File:", multiple = FALSE)),
                                      shiny::column(4, style=list("padding-left: 5px;"),
                                             shiny::radioButtons(inputId = ns("sep.alpha"), "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t"),
                                                          selected = ","))
                                    )        
                   ),
                   shiny::numericInput(inputId = ns("r.alpha"), label = "Input # of Full Reps:", value = 3, min = 2),
                   shiny::selectInput(inputId = ns("k.alpha"), label = "Input # of Plots per IBlock:", choices = ""),
                   shiny::numericInput(inputId = ns("l.alpha"), label = "Input # of Locations:", value = 1, min = 1),
                   
                   shiny::selectInput(inputId = ns("planter_mov_alpha"), label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"), multiple = FALSE,
                               selected = "serpentine"),
                   
                   shiny::fluidRow(
                     shiny::column(6, style=list("padding-right: 28px;"),
                            shiny::textInput(inputId = ns("plot_start.alpha"), "Starting Plot Number:", value = 101)
                     ),
                     shiny::column(6,style=list("padding-left: 5px;"),
                            shiny::textInput(inputId = ns("Location.alpha"), "Input Location:", value = "FARGO")
                     )
                   ),  
                   shiny::numericInput(inputId = ns("myseed.alpha"), label = "Random Seed:",
                                value = 16, min = 1),
                   shiny::fluidRow(
                     shiny::column(6,
                            shiny::actionButton(
                              inputId = ns("RUN.alpha"), 
                              "Run!", 
                              icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                              width = '100%'),
                     ),
                     shiny::column(6,
                            shiny::actionButton(
                              inputId = ns("Simulate.alpha"), 
                              "Simulate!", 
                              icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                              width = '100%'),
                     )
                     
                   ), 
                   shiny::br(),
                   shiny::downloadButton(ns("downloadData.alpha"), "Save experiment (ZIP)", style = "width:100%")
      ),
      
      shiny::mainPanel(
        width = 8,
        shiny::fluidRow(
          shiny::tabsetPanel(
            shiny::tabPanel(
              "Summary Design",
              shiny::br(),
              fieldhub_spinner(
                shiny::verbatimTextOutput(outputId = ns("summary_alpha_lattice"),
                                   placeholder = FALSE), 
                type = 4
              ),
              style = "padding-right: 40px;"
            ),
            shiny::tabPanel("Field Layout",
                     
                     # hidden .csv download button
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(shiny::downloadButton(ns("downloadCsv.alpha"),
                                    label = "CSV + metadata (ZIP)",
                                    icon = shiny::icon("download"),
                                    width = 'auto',
                                    style="color: #337ab7; background-color: #fff; border-color: #2e6da4")),
                     
                     fieldhub_spinner(
                       plotly::plotlyOutput(ns("random_layout"), width = "97%", height = "550px"),type = 5
                     ),
                     shiny::br(),
                     shiny::column(12, shiny::uiOutput(ns("well_panel_layout")))
            ),
            shiny::tabPanel("Field Book",
                     fieldhub_spinner(DT::DTOutput(ns("ALPHA_fieldbook")), type = 5)
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
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # for showing .csv button on run
    shinyjs::useShinyjs()
    
    init_data_alpha <- shiny::reactive({
      if (input$owndata_alpha == "Yes") {
        shiny::req(input$file.alpha)
        inFile <- input$file.alpha
        data_ingested <- load_file(name = inFile$name,
                                   path = inFile[["datapath"]],
                                   sep = input$sep.alpha,
                                   check = TRUE, design = "alpha")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_up <- as.data.frame(data_up[,1:2])
          data_alpha <- na.omit(data_up)
          colnames(data_alpha) <- c("ENTRY", "NAME")
          treatments = nrow(data_alpha)
          return(list(data_alpha = data_alpha, treatments = treatments))
        } else {
          app_upload_error(data_ingested,
                           missing_columns = "Data input needs at least two columns: ENTRY and NAME")
          return(NULL)
        }
      } else {
        shiny::req(input$t.alpha)
        nt <- as.numeric(input$t.alpha)
        df <- default_entries(nt)
        data_alpha <- df
        treatments = nrow(data_alpha)
        return(list(data_alpha = data_alpha, treatments = treatments))
      }
    })
    
    
    list_to_observe <- shiny::reactive({
      shiny::req(init_data_alpha())
      list(
        entry_list = input$owndata_alpha,
        entries = init_data_alpha()$treatments
      )
    })
    
    shiny::observeEvent(list_to_observe(), {
      shiny::req(init_data_alpha())
      options <- valid_block_sizes(
        as.numeric(init_data_alpha()$treatments),
        "alpha_lattice"
      )
      k <- if (length(options) == 0L) "No Options Available" else options
      
      if (length(options) > 2L) {
        selected <- options[ceiling(length(options) / 2)]
      } else selected <- k[1]
      
      shiny::updateSelectInput(session = session, inputId = 'k.alpha',
                        label = "Input # of Plots per IBlock:",
                        choices = k, selected = selected)
    })
    
    get_data_alpha <- shiny::reactive({
      if (is.null(init_data_alpha())) {
        shinyalert::shinyalert(
          "Error!!", 
          "Check input file and try again!", 
          type = "error")
        return(NULL)
      } else return(init_data_alpha())
    }) |>
      shiny::bindEvent(input$RUN.alpha)

    alpha_inputs <- shiny::reactive({
      shiny::req(get_data_alpha())
      shiny::req(input$planter_mov_alpha)
      shiny::req(input$k.alpha)
      shiny::req(input$r.alpha)
      shiny::req(input$plot_start.alpha)
      shiny::req(input$Location.alpha)
      shiny::req(input$myseed.alpha)
      shiny::req(input$l.alpha)
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
      plot_start <- validate_design(read_whole_numbers(
        input$plot_start.alpha, "Starting Plot Number"
      ))
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
      shiny::bindEvent(input$RUN.alpha)


    entryListFormatreatments <- data.frame(ENTRY = 1:9, 
                                        NAME = c(paste("Genotype", LETTERS[1:9], sep = "")))
    entriesInfoModal_ALPHA <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(entryListFormatreatments,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        shiny::h4("Entry numbers can be any set of consecutive positive numbers."),
        easyClose = FALSE
      )
    }
    
    toListen <- shiny::reactive({
      list(input$owndata_alpha)
    })
    
    shiny::observeEvent(toListen(), {
      if (input$owndata_alpha == "Yes") {
        shiny::showModal(
          entriesInfoModal_ALPHA()
        )
      }
    })
    

    ALPHA_reactive <- shiny::eventReactive(input$RUN.alpha, {
      shiny::req(get_data_alpha())
      shiny::req(alpha_inputs())

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
      
      validate_design(alpha_lattice(
        t = alpha_inputs()$t, 
        k = alpha_inputs()$k, 
        reps = alpha_inputs()$r,
        l = alpha_inputs()$sites, 
        plotNumber = alpha_inputs()$plot_start, 
        seed = alpha_inputs()$seed,
        locationNames = alpha_inputs()$site_names, 
        data = data_alpha
      ))
    })
    
    output$summary_alpha_lattice <- shiny::renderPrint({
      shiny::req(ALPHA_reactive())
      cat("Randomization was successful!", "\n", "\n")
      print(ALPHA_reactive(), n = 6)
    })
    
    upDateSites <- shiny::reactive({
      shiny::req(alpha_inputs())
      locs <- alpha_inputs()$sites
      sites <- 1:locs
      return(list(sites = sites))
    })
    
    
    output$well_panel_layout <- shiny::renderUI({
      shiny::req(ALPHA_reactive()$fieldBook)
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
      shiny::wellPanel(
        shiny::fluidPage(
          shiny::column(3,
                 shiny::radioButtons(ns("typlotALPHA"), "Type of Plot:",
                              c("Entries/Treatments" = 1,
                                "Plots" = 2,
                                "Heatmap" = 3), selected = 1)
          ),
          shiny::column(3,
                 shiny::selectInput(inputId = ns("stackedAlpha"),
                             label = "Reps layout:", 
                             choices = stacked)
          ),
          shiny::column(2,
                 shiny::selectInput(inputId = ns("layoutO"),
                             label = "Layout option:",
                             choices = layoutOptions, 
                             selected = 1)
          ),
          shiny::column(2,
                 shiny::selectInput(inputId = ns("locLayout"),
                             label = 'Location:', 
                             choices = as.numeric(upDateSites()$sites))
          )
        )
      )
    })
    
    shiny::observeEvent(input$stackedAlpha, {
      shiny::req(input$stackedAlpha)
      obj <- ALPHA_reactive()
      allBooks <- plot_layout(x = obj, 
                              layout = 1, 
                              stacked = input$stackedAlpha)$newBooks
      nBooks <- length(allBooks)
      NewlayoutOptions <- 1:nBooks
      shiny::updateSelectInput(session = session, inputId = 'layoutO',
                        label = "Layout option:",
                        choices = NewlayoutOptions,
                        selected = 1
      )
    })
    
    reset_selection <- shiny::reactiveValues(reset = 0)
    
    shiny::observeEvent(input$stackedAlpha, {
      reset_selection$reset <- 1
    })
    
    shiny::observeEvent(input$layoutO, {
      reset_selection$reset <- 0
    })
    
    reactive_layoutAlpha <- shiny::reactive({
      shiny::req(input$stackedAlpha)
      shiny::req(alpha_inputs()$planter)
      shiny::req(input$layoutO)
      shiny::req(ALPHA_reactive())
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
    
    
    valsALPHA <- shiny::reactiveValues(maxV.alpha = NULL, minV.alpha = NULL, trail.alpha = NULL)
    
    simuModal.alpha <- function(failed = FALSE) {
      shiny::modalDialog(
        shiny::selectInput(inputId = ns("trailsALPHA"), label = "Select One:", choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        shiny::conditionalPanel("input.trailsALPHA == 'Other'", ns = ns,
                         shiny::textInput(inputId = ns("OtherALPHA"), label = "Input the Trial Name:", value = NULL)
        ),
        shiny::fluidRow(
          shiny::column(6,
                 shiny::numericInput(inputId = ns("min.alpha"), "Input the min value", value = NULL)
          ),
          shiny::column(6,
                 shiny::numericInput(inputId = ns("max.alpha"), "Input the max value", value = NULL)
                 
          )
          
        ),
        
        if (failed)
          shiny::div(shiny::tags$b("Invalid input of data max and min", style = "color: red;")),
        
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(inputId = ns("ok.alpha"), "GO")
        )
        
      )
      
    }
    
    shiny::observeEvent(input$Simulate.alpha, {
      shiny::req(input$k.alpha)
      shiny::req(input$r.alpha)
      shiny::req(reactive_layoutAlpha()$fieldBookXY)
      shiny::showModal(
        simuModal.alpha()
      )
    })
    
    shiny::observeEvent(input$ok.alpha, {
      shiny::req(input$max.alpha, input$min.alpha)
      if (input$max.alpha > input$min.alpha && input$min.alpha != input$max.alpha) {
        valsALPHA$maxV.alpha <- input$max.alpha
        valsALPHA$minV.alpha <- input$min.alpha
        if(input$trailsALPHA == "Other") {
          shiny::req(input$OtherALPHA)
          if(!is.null(input$OtherALPHA)) {
            valsALPHA$trail.alpha <- as.character(input$OtherALPHA)
          }else shiny::showModal(simuModal.alpha(failed = TRUE))
        }else {
          valsALPHA$trail.alpha <- as.character(input$trailsALPHA)
        }
        shiny::removeModal()
      }else {
        shiny::showModal(
          simuModal.alpha(failed = TRUE)
        )
      }
    })
    
    simuDataALPHA <- shiny::reactive({
      shiny::req(reactive_layoutAlpha())
      if(!is.null(valsALPHA$maxV.alpha) && !is.null(valsALPHA$minV.alpha) && !is.null(valsALPHA$trail.alpha)) {
        max <- as.numeric(valsALPHA$maxV.alpha)
        min <- as.numeric(valsALPHA$minV.alpha)
        df.alpha <- reactive_layoutAlpha()$allSitesFieldbook
        simulation <- validate_design(simulate_classic_field_book(
          field_book = df.alpha, min_value = min, max_value = max,
          response_name = valsALPHA$trail.alpha, seed = alpha_inputs()$seed,
          order_by_id = FALSE
        ))
        df.alpha <- simulation$field_book
        a <- ncol(df.alpha)
      }else {
        simulation <- NULL
        df.alpha <- reactive_layoutAlpha()$allSitesFieldbook
        a <- ncol(df.alpha)
      }
      return(list(df = df.alpha, a = a, simulation = simulation))
    })
    
    heatmapInfoModal_ALPHA <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    locNum <- shiny::reactive(
      return(as.numeric(input$locLayout))
    )
    
    heatmap_obj <- shiny::reactive({
      shiny::req(simuDataALPHA()$df)
      book <- simuDataALPHA()$df
      response <- as.character(valsALPHA$trail.alpha)
      if (length(response) == 1L && response %in% names(book)) {
        validate_design(app_field_heatmap(
          book, response_name = response, selected = locNum(),
          label_column = "ENTRY", label_title = "Entry",
          include_site = TRUE, include_checks = FALSE
        ))
      } else {
        shiny::showModal(heatmapInfoModal_ALPHA())
        NULL
      }
    })
    
    output$random_layout <- plotly::renderPlotly({
      shiny::req(reactive_layoutAlpha())
      shiny::req(ALPHA_reactive())
      shiny::req(input$typlotALPHA)
      if (input$typlotALPHA == 1) {
        reactive_layoutAlpha()$out_layout
      } else if (input$typlotALPHA == 2) {
        reactive_layoutAlpha()$out_layoutPlots
      } else {
        shiny::req(heatmap_obj())
        heatmap_obj()
      }
    })
    
    output$ALPHA_fieldbook <- DT::renderDataTable({
      shiny::req(simuDataALPHA()$df)
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
      table_options <- list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px")
      
      DT::datatable(df,
                    filter = 'top',
                    rownames = FALSE, 
                    options = utils::modifyList(table_options, list(
        columnDefs = list(list(className = 'dt-center', targets = "_all")))))
      
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData.alpha <- app_csv_archive(
      filename = function() {
        loc <- paste("Alpha_Lattice_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      data = function() as.data.frame(simuDataALPHA()$df),
      design = ALPHA_reactive,
      field_book = function() simuDataALPHA()$df,
      simulation = function() simuDataALPHA()$simulation,
      layout = function() reactive_layoutAlpha()$layout_metadata,
      kind = "field_book"
    )
    
    csv_data <- shiny::reactive({
      shiny::req(simuDataALPHA()$df)
      df <- simuDataALPHA()$df
      shiny::req(input$typlotALPHA)
      if (input$typlotALPHA == 2) {
        export_layout(df, locNum(), TRUE)
      } else {
        export_layout(df, locNum())
      }
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadCsv.alpha <- app_csv_archive(
      filename = function() {
        loc <- paste("Alpha_Lattice_Layout", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      data = function() as.data.frame(csv_data()$file),
      design = ALPHA_reactive,
      field_book = function() simuDataALPHA()$df,
      simulation = function() simuDataALPHA()$simulation,
      layout = function() reactive_layoutAlpha()$layout_metadata,
      kind = "layout"
    )
    
    
    app_reproduction_outputs(output, ALPHA_reactive)
  })
}
