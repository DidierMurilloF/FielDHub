#' IBD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#'
mod_IBD_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Incomplete Blocks Design"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        shiny::radioButtons(ns("owndataibd"),
                     label = "Import entries' list?", 
                     choices = c("Yes", "No"), 
                     selected = "No",
                     inline = TRUE, 
                     width = NULL, 
                     choiceNames = NULL, 
                     choiceValues = NULL),
        
        shiny::conditionalPanel(
          condition = "input.owndataibd != 'Yes'", 
          ns = ns,
          shiny::numericInput(ns("t.ibd"),
                       label = "Input # of Treatments:",
                       value = 15, 
                       min = 2)
        ),
        shiny::conditionalPanel(
          condition = "input.owndataibd == 'Yes'", 
          ns = ns,
          shiny::fluidRow(
            shiny::column(8, style=list("padding-right: 28px;"),
                   shiny::fileInput(inputId = ns("file.IBD"),
                             label = "Upload a CSV File:", 
                             multiple = FALSE)),
            shiny::column(4, style=list("padding-left: 5px;"),
                   shiny::radioButtons(inputId = ns("sep.ibd"), "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","))
          )        
        ),
        
        shiny::numericInput(inputId = ns("r.ibd"),
                     label = "Input # of Full Reps:", 
                     value = 4, 
                     min = 2),
        
        shiny::selectInput(inputId = ns("k.ibd"),
                    label = "Input # of Plots per IBlock:", 
                    choices = ""),
        
        shiny::numericInput(inputId = ns("l.ibd"),
                     label = "Input # of Locations:",
                     value = 1, 
                     min = 1),
        shiny::selectInput(inputId = ns("planter_mov_ibd"),
          label = "Plot Order Layout:",
          choices = c("serpentine", "cartesian"), 
          multiple = FALSE,
          selected = "serpentine"),
        shiny::fluidRow(
          shiny::column(6, style=list("padding-right: 28px;"),
                 shiny::textInput(inputId = ns("plot_start.ibd"),
                           "Starting Plot Number:", 
                           value = 101)
          ),
          shiny::column(6,style=list("padding-left: 5px;"),
                 shiny::textInput(inputId = ns("Location.ibd"),
                           "Input Location:", 
                           value = "FARGO")
          )
        ), 
        shiny::numericInput(inputId = ns("seed.ibd"),
                     label = "Random Seed:",
                     value = 4),
        shiny::fluidRow(
          shiny::column(6,
                 shiny::actionButton(
                   inputId = ns("RUN.ibd"), 
                   label = "Run!", 
                   icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                   width = '100%'),
          ),
          shiny::column(6,
                 shiny::actionButton(
                   ns("Simulate.ibd"), 
                   label = "Simulate!", 
                   icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                   width = '100%'),
          )
          
        ), 
        shiny::br(),
        shiny::downloadButton(ns("downloadData.ibd"),
                       "Save Experiment!", 
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
                shiny::verbatimTextOutput(outputId = ns("summary_ibd"),
                                   placeholder = FALSE), 
                type = 4
              ),
              style = "padding-right: 40px;"
            ),
            shiny::tabPanel("Field Layout",
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(
                       shiny::downloadButton(
                         ns("downloadCsv.ibd"), 
                         label =  "CSV",
                         icon = shiny::icon("file-csv"),
                         width = '10%',
                         style="color: #337ab7; background-color: #fff; border-color: #2e6da4")
                      ),
                     fieldhub_spinner(
                       plotly::plotlyOutput(ns("layouts"), 
                                            width = "97%", 
                                            height = "550px"),
                       type = 5
                     ),
                     shiny::br(),
                     shiny::column(12,
                            shiny::uiOutput(ns("well_panel_layout_IBD"))
                            )
            ),
            shiny::tabPanel("Field Book",
                     fieldhub_spinner(
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
  shiny::moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    shinyjs::useShinyjs()
    treatments <- paste("TX-", 1:9, sep = "")
    entryListFormat_IBD <- data.frame(ENTRY = 1:9, 
                                      NAME = treatments)
    entriesInfoModal_IBD <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(entryListFormat_IBD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        shiny::h4("Entry numbers can be any set of consecutive positive numbers."),
        easyClose = FALSE
      )
    }
    
    toListen <- shiny::reactive({
      list(input$owndataibd)
    })
    
    shiny::observeEvent(toListen(), {
      if (input$owndataibd == "Yes") {
        shiny::showModal(
          entriesInfoModal_IBD()
        )
      }
    })
    
    init_data_ibd <- shiny::reactive({
      
      if(input$owndataibd == "Yes") {
        shiny::req(input$file.IBD)
        inFile <- input$file.IBD
        data_ingested <- load_file(name = inFile$name, 
                                   path = inFile[["datapath"]],
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
        } else {
          app_upload_error(data_ingested,
                           missing_columns = "Data input needs at least two columns: ENTRY and NAME")
          return(NULL)
        }
      } else {
        shiny::req(input$t.ibd)
        nt <- as.numeric(input$t.ibd)
        df <- default_entries(nt)
        data_ibd <- df
        treatments = nrow(data_ibd)
        return(list(data_ibd = data_ibd, treatments = treatments))
      }
    })
    
    list_to_observe <- shiny::reactive({
      shiny::req(init_data_ibd())
      list(
        entry_list = input$owndataibd,
        entries = init_data_ibd()$treatments
      )
    })
    
    shiny::observeEvent(list_to_observe(), {
      shiny::req(init_data_ibd())
      options <- valid_block_sizes(
        as.numeric(shiny::req(init_data_ibd())$treatments),
        "incomplete_blocks"
      )
      k <- if (length(options) == 0L) "No Options Available" else options
      
      if (length(options) > 2L) {
        selected <- options[ceiling(length(options) / 2)]
      } else selected <- k[1]
      
      shiny::updateSelectInput(session = session,
                        inputId = 'k.ibd', 
                        label = "Input # of Plots per IBlock:",
                        choices = k, selected = selected)
      
    })
    
    get_data_ibd <- shiny::reactive({
      if (is.null(init_data_ibd())) {
        shinyalert::shinyalert(
          "Error!!", 
          "Check input file and try again!", 
          type = "error")
        return(NULL)
      } else return(init_data_ibd())
    }) |>
      shiny::bindEvent(input$RUN.ibd)
    
    ibd_inputs <- shiny::reactive({
      
      shiny::req(get_data_ibd())
      
      shiny::req(input$r.ibd)
      shiny::req(input$k.ibd)
      shiny::req(input$seed.ibd)
      shiny::req(input$plot_start.ibd)
      shiny::req(input$Location.ibd)
      shiny::req(input$l.ibd)
      shiny::req(input$planter_mov_ibd)
      
      r.ibd <- as.numeric(input$r.ibd)
      k.ibd <- as.numeric(input$k.ibd)
      treatments <- as.numeric(get_data_ibd()$treatments)
      planter <- input$planter_mov_ibd
      plot_start <- validate_design(read_whole_numbers(
        input$plot_start.ibd, "Starting Plot Number"
      ))
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
    }) |>
      shiny::bindEvent(input$RUN.ibd)
    
    IBD_reactive <- shiny::reactive({
      shiny::req(get_data_ibd())
      shiny::req(ibd_inputs())
      
      shinyjs::show(id = "downloadCsv.ibd")
      
      data_ibd <- get_data_ibd()$data_ibd
      
      if (ibd_inputs()$r < 2) {
        shinyalert::shinyalert(
          "Error!!", 
          "Incomplete Blocks Design needs at least 2 replicates.", 
          type = "error")
        return(NULL)
      }
      
      validate_design(incomplete_blocks(
        t = ibd_inputs()$t, 
        k = ibd_inputs()$k, 
        reps = ibd_inputs()$r,
        l = ibd_inputs()$sites, 
        plotNumber = ibd_inputs()$plot_start, 
        seed = ibd_inputs()$seed,
        locationNames = ibd_inputs()$site_names, 
        data = data_ibd
      )) 
      
    }) |>
      shiny::bindEvent(input$RUN.ibd)
    
    output$summary_ibd <- shiny::renderPrint({
      shiny::req(IBD_reactive())
      cat("Randomization was successful!", "\n", "\n")
      print(IBD_reactive(), n = 6)
    })
    
    upDateSites <- shiny::eventReactive(input$RUN.ibd, {
      shiny::req(input$l.ibd)
      locs <- as.numeric(input$l.ibd)
      sites <- 1:locs
      return(list(sites = sites))
    })
    
    output$well_panel_layout_IBD <- shiny::renderUI({
      shiny::req(IBD_reactive()$fieldBook)
      obj_ibd <- IBD_reactive()
      allBooks_ibd<- plot_layout(x = obj_ibd, layout = 1)$newBooks
      nBooks_ibd <- length(allBooks_ibd)
      layoutOptions_ibd <- 1:nBooks_ibd
      stacked <- c("Vertical Stack Panel" = "vertical", 
                     "Horizontal Stack Panel" = "horizontal")
      shiny::wellPanel(
        shiny::column(2,
               shiny::radioButtons(ns("typlotibd"), "Type of Plot:",
                            c("Entries/Treatments" = 1,
                              "Plots" = 2,
                              "Heatmap" = 3), selected = 1)
        ),
        shiny::fluidRow(
          shiny::column(3,
                 shiny::selectInput(inputId = ns("stackedibd"),
                             label = "Reps layout:", 
                             choices = stacked)
          ),
          shiny::column(3, #align="center",
                 shiny::selectInput(inputId = ns("layoutO_ibd"),
                             label = "Layout option:", 
                             choices = layoutOptions_ibd)
          ),
          shiny::column(3, #align="center",
                 shiny::selectInput(inputId = ns("locLayout_ibd"),
                             label = "Location:", 
                             choices = as.numeric(upDateSites()$sites))
          )
        )
      )
    })
    
    shiny::observeEvent(input$stackedibd, {
      shiny::req(input$stackedibd)
      obj <- IBD_reactive()
      allBooks <- plot_layout(x = obj, 
                              layout = 1, 
                              stacked = input$stackedibd)$newBooks
      nBooks <- length(allBooks)
      NewlayoutOptions <- 1:nBooks
      shiny::updateSelectInput(session = session, inputId = 'layoutO_ibd',
                        label = "Layout option:",
                        choices = NewlayoutOptions,
                        selected = 1
      )
    })
    
    reset_selection <- shiny::reactiveValues(reset = 0)
    
    shiny::observeEvent(input$stackedibd, {
      reset_selection$reset <- 1
    })
    
    shiny::observeEvent(input$layoutO_ibd, {
      reset_selection$reset <- 0
    })
    
    reactive_layoutIBD <- shiny::reactive({
      shiny::req(input$layoutO_ibd)
      shiny::req(IBD_reactive())
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
    
    
    valsIBD <- shiny::reactiveValues(maxV.ibd = NULL,
                              minV.ibd = NULL, 
                              trail.ibd = NULL)
    
    simuModal.ibd <- function(failed = FALSE) {
      shiny::modalDialog(
        shiny::selectInput(inputId = ns("trailsIBD"),
                    label = "Select One:", 
                    choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        shiny::conditionalPanel(
          condition = "input.trailsIBD == 'Other'", 
          ns = ns,
          shiny::textInput(inputId = ns("OtherIBD"),
                    label = "Input Trial Name:", 
                    value = NULL)
        ),
        shiny::fluidRow(
          shiny::column(6,
                 shiny::numericInput(inputId = ns("min.ibd"),
                              "Input the min value", 
                              value = NULL)
          ),
          shiny::column(6,
                 shiny::numericInput(inputId = ns("max.ibd"),
                              "Input the max value", 
                              value = NULL)
                 
          )
          
        ),
        
        if (failed)
          shiny::div(shiny::tags$b("Invalid input of data max and min",
                     style = "color: red;")),
        
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(inputId = ns("ok.ibd"), "GO")
        )
        
      )
      
    }
    
    shiny::observeEvent(input$Simulate.ibd, {
      shiny::req(IBD_reactive()$fieldBook)
      shiny::showModal(
        simuModal.ibd()
      )
    })
    
    shiny::observeEvent(input$ok.ibd, {
      shiny::req(input$max.ibd, input$min.ibd)
      if (input$max.ibd > input$min.ibd && input$min.ibd != input$max.ibd) {
        valsIBD$maxV.ibd <- input$max.ibd
        valsIBD$minV.ibd <- input$min.ibd
        if(input$trailsIBD == "Other") {
          shiny::req(input$OtherIBD)
          if(!is.null(input$OtherIBD)) {
            valsIBD$trail.ibd <- as.character(input$OtherIBD)
          }else shiny::showModal(simuModal.ibd(failed = TRUE))
        }else {
          valsIBD$trail.ibd <- as.character(input$trailsIBD)
        }
        shiny::removeModal()
      }else {
        shiny::showModal(
          simuModal.ibd(failed = TRUE)
        )
      }
    })
    
    
    simuDataIBD <- shiny::reactive({
      shiny::req(IBD_reactive()$fieldBook)
      if(!is.null(valsIBD$maxV.ibd) && !is.null(valsIBD$minV.ibd) && 
         !is.null(valsIBD$trail.ibd)) {
        max <- as.numeric(valsIBD$maxV.ibd)
        min <- as.numeric(valsIBD$minV.ibd)
        df.ibd <- reactive_layoutIBD()$allSitesFieldbook
        simulation <- validate_design(simulate_classic_field_book(
          field_book = df.ibd, min_value = min, max_value = max,
          response_name = valsIBD$trail.ibd, seed = ibd_inputs()$seed,
          order_by_id = FALSE
        ))
        df.ibd <- simulation$field_book
        a <- ncol(df.ibd)
      }else {
        simulation <- NULL
        df.ibd <- reactive_layoutIBD()$allSitesFieldbook
        a <- ncol(df.ibd)
      }
      return(list(df = df.ibd, a = a, simulation = simulation))
    })
    
    heatmapInfoModal_IBD <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    locNum <- shiny::reactive(
      return(as.numeric(input$locLayout_ibd))
    )
    
    heatmap_obj <- shiny::reactive({
      shiny::req(simuDataIBD()$df)
      book <- simuDataIBD()$df
      response <- as.character(valsIBD$trail.ibd)
      if (length(response) == 1L && response %in% names(book)) {
        validate_design(app_field_heatmap(
          book, response_name = response, selected = locNum(),
          label_column = "ENTRY", label_title = "Entry",
          include_site = TRUE, include_checks = FALSE
        ))
      } else {
        shiny::showModal(heatmapInfoModal_IBD())
        NULL
      }
    })
    
    output$layouts <- plotly::renderPlotly({
      shiny::req(reactive_layoutIBD())
      shiny::req(IBD_reactive())
      shiny::req(input$typlotibd)
      if (input$typlotibd == 1) {
        reactive_layoutIBD()$out_layout
      } else if (input$typlotibd == 2) {
        reactive_layoutIBD()$out_layoutPlots
      } else {
        shiny::req(heatmap_obj())
        heatmap_obj()
      }
    })
    
    output$IBD.output <- DT::renderDataTable({
      
      shiny::req(simuDataIBD()$df)
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
      table_options <- list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px")
      DT::datatable(df,
                    filter = 'top',
                    rownames = FALSE, 
                    options = utils::modifyList(table_options, list(
                      columnDefs = list(list(className = 'dt-center', 
                                             targets = "_all")))))
    })
    
    
    output$downloadData.ibd <- shiny::downloadHandler(
      filename = function() {
        loc <- paste("IBD_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(simuDataIBD()$df)
        write.csv(df, file, row.names = FALSE)
      }
    )
    csv_data <- shiny::reactive({
      shiny::req(simuDataIBD()$df)
      df <- simuDataIBD()$df
      shiny::req(input$typlotibd)
      if (input$typlotibd == 2) {
        export_layout(df, locNum(), TRUE)
      } else {
        export_layout(df, locNum())
      }
    })
    
    
    # Downloadable csv of selected dataset ----
    output$downloadCsv.ibd <- shiny::downloadHandler(
      filename = function() {
        loc <- paste("Incomplete_Block_Layout", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(csv_data()$file)
        write.csv(df, file, row.names = FALSE)
      }
    )
    app_reproduction_outputs(output, IBD_reactive)
  })
}
