#' SPD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_SPD_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Split-Plot Design"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        shiny::radioButtons(inputId = ns("owndataSPD"),
                     label = "Import entries' list?", 
                     choices = c("Yes", "No"), 
                     selected = "No",
                     inline = TRUE, 
                     width = NULL, 
                     choiceNames = NULL, 
                     choiceValues = NULL),
        shiny::selectInput(inputId = ns("kindSPD"),
                    label = "Select SPD Type:",
                    choices = c("Split-Plot in a RCBD" = "SPD_RCBD", 
                                "Split-Plot in a CRD" = "SPD_CRD"),
                    multiple = FALSE),
        
        shiny::conditionalPanel("input.owndataSPD == 'Yes'", ns = ns,
                         shiny::fluidRow(
                           shiny::column(8, style=list("padding-right: 28px;"),
                                  shiny::fileInput(ns("file.SPD"),
                                            label = "Upload a csv File:", 
                                            multiple = FALSE)),
                           shiny::column(4,style=list("padding-left: 5px;"),
                                  shiny::radioButtons(ns("sep.spd"), "Separator",
                                               choices = c(Comma = ",",
                                                           Semicolon = ";",
                                                           Tab = "\t"),
                                               selected = ","))
                         )
        ),
        shiny::conditionalPanel("input.owndataSPD != 'Yes'", ns = ns,
                          shiny::numericInput(ns("mp.spd"),
                                       label = "Whole-plots:",
                                       value = 4, min = 2),
                          shiny::numericInput(ns("sp.spd"),
                                       label = "Sub-plots Within Whole-plots:",
                                       value = 3, min = 2)
        ),
        
        shiny::fluidRow(
          shiny::column(6, style=list("padding-right: 28px;"),
                 shiny::numericInput(ns("reps.spd"), label = "Input # of Full Reps:",
                              value = 3, min = 2), 
          ),
          shiny::column(6,style=list("padding-left: 5px;"),
                 shiny::numericInput(ns("l.spd"), label = "Input # of Locations:",
                              value = 1, min = 1)
          )
        ),
        # The RCBD-type layouts number whole plots in a fixed order, so the
        # plot order only applies to the CRD type
        shiny::conditionalPanel("input.kindSPD == 'SPD_CRD'", ns = ns,
          shiny::selectInput(inputId = ns("planter_mov_spd"),
            label = "Plot Order Layout:",
            choices = c("serpentine", "cartesian"), 
            multiple = FALSE,
            selected = "serpentine")
        ),
        shiny::fluidRow(
          shiny::column(6, style=list("padding-right: 28px;"),
                 shiny::textInput(ns("plot_start.spd"), "Starting Plot Number:",
                           value = 101)
          ),
          shiny::column(6,style=list("padding-left: 5px;"),
                 shiny::textInput(ns("Location.spd"), "Input the Location:",
                           value = "FARGO")
          )
        ),
        shiny::numericInput(inputId = ns("seed.spd"), label = "Random Seed:",
                     value = 118, min = 1),
        
        shiny::fluidRow(
          shiny::column(6,
                 shiny::actionButton(
                   inputId = ns("RUN.spd"), 
                   "Run!", 
                   icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                   width = '100%'),
          ),
          shiny::column(6,
                 shiny::actionButton(
                   ns("Simulate.spd"), 
                   "Simulate!", 
                   icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                   width = '100%'),
          )
          
        ), 
        shiny::br(),
        shiny::downloadButton(ns("downloadData.spd"), "Save Experiment!",
                      style = "width:100%")
      ),
      
      shiny::mainPanel(
        width = 8,
        shiny::fluidRow(
          shiny::tabsetPanel(
            shiny::tabPanel("Field Layout",
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(shiny::downloadButton(ns("downloadCsv.spd"),
                                                    label =  "CSV",
                                                    icon = shiny::icon("file-csv"),
                                                    width = '10%',
                                                    style="color: #337ab7; background-color: #fff; border-color: #2e6da4")),
                     fieldhub_spinner(
                       plotly::plotlyOutput(ns("layouts"), 
                                            width = "97%", 
                                            height = "560px"),
                       type = 5
                     ),
                     shiny::br(),
                     shiny::column(12,shiny::uiOutput(ns("well_panel_layout_SPD")))
            ),
            shiny::tabPanel("Field Book",
                     fieldhub_spinner(
                       DT::DTOutput(ns("SPD.output")), 
                       type = 5
                       )
            )
          )
        )
      )
    )    
  )
}
    
#' SPD Server Functions
#'
#' @noRd 
mod_SPD_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    shinyjs::useShinyjs()
    
    wp <- c("NFung", paste("Fung", 1:4, sep = "")) 
    sp <- paste("Beans", 1:10, sep = "")            
    entryListFormat_SPD <- data.frame(list(WHOLEPLOT = c(wp, rep("", 5)), 
                                           SUBPLOT = sp))
    
    entriesInfoModal_SPD<- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(entryListFormat_SPD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        #h4("Note that reps might be unbalanced."),
        easyClose = FALSE
      )
    }
    
    toListen <- shiny::reactive({
      list(input$owndataSPD)
    })
    
    shiny::observeEvent(toListen(), {
      if (input$owndataSPD == "Yes") {
        shiny::showModal(
          entriesInfoModal_SPD()
        )
      }
    })
    
    get_data_spd <- shiny::reactive({
      if (input$owndataSPD == "Yes") {
        shiny::req(input$file.SPD)
        inFile <- input$file.SPD
        data_ingested <- load_file(name = inFile$name, 
                                   path = inFile[["datapath"]],
                                   sep = input$sep.spd, 
                                   check = TRUE, 
                                   design = "spd")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_spd <- as.data.frame(data_up[,1:2])
          colnames(data_spd) <- c("WHOLEPLOT", "SUBPLOT")
          wp <- as.vector(na.omit(data_spd[,1]))
          sp <- as.vector(na.omit(data_spd[,2]))
          treatments <- c(wp, sp)
          return(list(data_spd = data_spd, treatments = treatments))
        } else {
          app_upload_error(data_ingested,
                           missing_columns = "Data input needs at least two column: WHOLEPLOT and SUBPLOT")
          return(NULL)
        }
      } else {
        shiny::req(input$mp.spd, input$sp.spd)
        wp <- as.numeric(input$mp.spd)
        sp <- as.numeric(input$sp.spd)
        treatments <- c(wp, sp)
        data_spd <- NULL
        return(list(data_spd = data_spd, treatments = treatments))
      }
    }) |> 
      shiny::bindEvent(input$RUN.spd)
    
    spd_inputs <- shiny::reactive({
      
      shiny::req(get_data_spd())
      
      shiny::req(input$plot_start.spd)
      shiny::req(input$Location.spd)
      shiny::req(input$seed.spd)
      shiny::req(input$l.spd)
      shiny::req(input$reps.spd)
      
      sites <- as.numeric(input$l.spd)
      seed <- as.numeric(input$seed.spd)
      plot_start <- validate_design(read_whole_numbers(
        input$plot_start.spd, "Starting Plot Number"
      ))
      site_names <-  as.vector(unlist(strsplit(input$Location.spd, ",")))
      reps <- as.numeric(input$reps.spd)
      planter <- input$planter_mov_spd
      data_spd <- get_data_spd()$data_spd
      
      if (input$kindSPD == "SPD_RCBD") {
        type_design <- 2
      } else type_design <- 1
      
      return(
        list(
          wp = get_data_spd()$treatments[1], 
          sp = get_data_spd()$treatments[2], 
          r = reps, 
          sites = sites,
          seed = seed,
          planter = planter,
          plot_start = plot_start,
          site_names = site_names, 
          type_design = type_design,
          data = data_spd
        )
      )
    }) |>
      shiny::bindEvent(input$RUN.spd)
    
    
    spd_reactive <- shiny::reactive({
      
      shiny::req(spd_inputs())
      
      shinyjs::show(id = "downloadCsv.spd")
      
      validate_design(split_plot(
        wp = spd_inputs()$wp, 
        sp = spd_inputs()$sp, 
        reps = spd_inputs()$r, 
        l = spd_inputs()$sites,
        plotNumber = spd_inputs()$plot_start,
        seed = spd_inputs()$seed,
        type = spd_inputs()$type_design, 
        locationNames = spd_inputs()$site_names, 
        data = spd_inputs()$data
      ))
    }) |> 
      shiny::bindEvent(input$RUN.spd)
    
    
    upDateSites <- shiny::eventReactive(input$RUN.spd, {
      shiny::req(input$l.spd)
      locs <- as.numeric(input$l.spd)
      sites <- 1:locs
      return(list(sites = sites))
    })
    
    
    output$well_panel_layout_SPD <- shiny::renderUI({
      shiny::req(spd_reactive()$fieldBook)
      obj_spd <- spd_reactive()
      allBooks_spd<- plot_layout(x = obj_spd, 
                                 layout = 1, 
                                 stacked = "vertical")$newBooks
      nBooks_spd <- length(allBooks_spd)
      layoutOptions_spd <- 1:nBooks_spd
      sites <- as.numeric(input$l.spd)
      stacked_spd <- c("Vertical Stack Panel" = "vertical", 
                         "Horizontal Stack Panel" = "horizontal")
      shiny::wellPanel(
        shiny::column(2,
               shiny::radioButtons(ns("typlotspd"), "Type of Plot:",
                            c("Entries/Treatments" = 1,
                              "Plots" = 2,
                              "Heatmap" = 3))
        ),
        shiny::fluidRow(
          shiny::column(3,
                 shiny::selectInput(inputId = ns("stackedSPD"), label = "Reps layout:",
                             choices = stacked_spd),
          ),
          shiny::column(2,
                 shiny::selectInput(inputId = ns("layoutO_spd"),
                             label = "Layout option:", 
                             choices = layoutOptions_spd)
          ),
          shiny::column(2,
                 shiny::selectInput(inputId = ns("locLayout_spd"),
                             label = "Location:", 
                             choices = as.numeric(upDateSites()$sites))
          )
        )
      )
    })
    
    shiny::observeEvent(input$stackedSPD, {
      shiny::req(input$stackedSPD)
      shiny::req(input$l.spd)
      obj_spd <- spd_reactive()
      allBooks <- plot_layout(x = obj_spd, 
                              layout = 1, 
                              stacked = input$stackedSPD)$newBooks
      nBooks <- length(allBooks)
      NewlayoutOptions <- 1:nBooks
      shiny::updateSelectInput(session = session, inputId = 'layoutO_spd',
                        label = "Layout option:",
                        choices = NewlayoutOptions,
                        selected = 1
      )
    })
    
    reset_selection <- shiny::reactiveValues(reset = 0)
    
    shiny::observeEvent(input$stackedSPD, {
      reset_selection$reset <- 1
    })
    
    shiny::observeEvent(input$layoutO_spd, {
      reset_selection$reset <- 0
    })
    
    reactive_layoutSPD <- shiny::reactive({
      shiny::req(input$layoutO_spd)
      shiny::req(input$stackedSPD)
      shiny::req(spd_reactive())
      obj_spd <- spd_reactive()
      
      planting_spd <- spd_inputs()$planter
      
      if (reset_selection$reset == 1) {
        opt_spd <- 1
      } else opt_spd <- as.numeric(input$layoutO_spd)
      
      locSelected_spd <- as.numeric(input$locLayout_spd)
      try(plot_layout(x = obj_spd, 
                      layout = opt_spd, 
                      planter = planting_spd, 
                      stacked = input$stackedSPD,
                      l = locSelected_spd), 
          silent = TRUE)
    })

    valspd <- shiny::reactiveValues(maxV.spd = NULL, minV.spd = NULL, trail.spd = NULL)
    
    simuModal.spd <- function(failed = FALSE) {
      shiny::modalDialog(
        shiny::selectInput(inputId = ns("trailsspd"), label = "Select One:", choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
        shiny::conditionalPanel("input.trailsspd == 'Other'", ns = ns,
                         shiny::textInput(inputId = ns("Otherspd"), label = "Input Trial Name:", value = NULL)
        ),
        shiny::fluidRow(
          shiny::column(6,
                 shiny::numericInput(ns("min.spd"), "Input the min value", value = NULL)
          ),
          shiny::column(6,
                 shiny::numericInput(ns("max.spd"), "Input the max value", value = NULL)
                 
          )
          
        ),
        
        if (failed)
          shiny::div(shiny::tags$b("Invalid input of data max and min", style = "color: red;")),
        
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(ns("ok.spd"), "GO")
        )
        
      )
      
    }
    
    shiny::observeEvent(input$Simulate.spd, {
      shiny::req(spd_reactive()$fieldBook)
      shiny::showModal(
        simuModal.spd()
      )
    })
    
    shiny::observeEvent(input$ok.spd, {
      shiny::req(input$max.spd, input$min.spd)
      if (input$max.spd > input$min.spd && input$min.spd != input$max.spd) {
        valspd$maxV.spd <- input$max.spd
        valspd$minV.spd <- input$min.spd
        if(input$trailsspd == "Other") {
          shiny::req(input$Otherspd)
          if(!is.null(input$Otherspd)) {
            valspd$trail.spd <- input$Otherspd
          }else shiny::showModal(simuModal.spd(failed = TRUE))
        }else {
          valspd$trail.spd <- as.character(input$trailsspd)
        }
        shiny::removeModal()
      }else {
        shiny::showModal(
          simuModal.spd(failed = TRUE)
        )
      }
    })
    
    simuData_spd <- shiny::reactive({
      shiny::req(spd_reactive()$fieldBook)
      
      if(!is.null(valspd$maxV.spd) && !is.null(valspd$minV.spd) && !is.null(valspd$trail.spd)) {
        max <- as.numeric(valspd$maxV.spd)
        min <- as.numeric(valspd$minV.spd)
        df.spd <- reactive_layoutSPD()$allSitesFieldbook
        simulation <- validate_design(simulate_classic_field_book(
          field_book = df.spd, min_value = min, max_value = max,
          response_name = valspd$trail.spd, seed = spd_inputs()$seed,
          order_by_id = TRUE
        ))
        df.spd <- simulation$field_book
      }else {
        simulation <- NULL
        df.spd <- reactive_layoutSPD()$allSitesFieldbook
      }
      return(list(df = df.spd, simulation = simulation))
    })
    
    heatmapInfoModal_SPD <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Simulate some data to see a heatmap!"),
        easyClose = TRUE
      )
    }
    
    locNum <- shiny::reactive(
      return(as.numeric(input$locLayout_spd))
    )
    
    heatmap_obj <- shiny::reactive({
      shiny::req(simuData_spd()$df)
      book <- simuData_spd()$df
      response <- as.character(valspd$trail.spd)
      if (length(response) == 1L && response %in% names(book)) {
        validate_design(app_field_heatmap(
          book, response_name = response, selected = locNum(),
          label_column = "TRT_COMB", label_title = "Treatment",
          include_site = TRUE, include_checks = FALSE
        ))
      } else {
        shiny::showModal(heatmapInfoModal_SPD())
        NULL
      }
    })
    
    output$layouts <- plotly::renderPlotly({
      shiny::req(reactive_layoutSPD())
      shiny::req(spd_reactive())
      shiny::req(input$typlotspd)
      if (input$typlotspd == 1) {
        reactive_layoutSPD()$out_layout
      } else if (input$typlotspd == 2) {
        reactive_layoutSPD()$out_layoutPlots
      } else {
        shiny::req(heatmap_obj())
        heatmap_obj()
      }
    })
    
    
    output$SPD.output <- DT::renderDataTable({
      
      df <- simuData_spd()$df
      df$LOCATION <- as.factor(df$LOCATION)
      df$PLOT <- as.factor(df$PLOT)
      df$ROW <- as.factor(df$ROW)
      df$COLUMN <- as.factor(df$COLUMN)
      df$REP <- as.factor(df$REP)
      df$WHOLE_PLOT <- as.factor(df$WHOLE_PLOT)
      df$SUB_PLOT <- as.factor(df$SUB_PLOT)
      df$TRT_COMB <- as.factor(df$TRT_COMB)
      table_options <- list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px")
      
      DT::datatable(df, 
                    filter = 'top', 
                    rownames = FALSE, 
                    options = utils::modifyList(table_options, list(
        columnDefs = list(list(className = 'dt-center', targets = "_all")))))
      
    })
    
    output$downloadData.spd <- shiny::downloadHandler(
      filename = function() {
        loc <- paste("Split-Plot_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(simuData_spd()$df)
        write.csv(df, file, row.names = FALSE)
      }
    )
    csv_data <- shiny::reactive({
      shiny::req(simuData_spd()$df)
      df <- simuData_spd()$df
      shiny::req(input$typlotspd)
      if (input$typlotspd == 2) {
        export_layout(df, locNum(), TRUE)
      } else {
        export_layout(df, locNum())
      }
    })
    
    
    # Downloadable csv of selected dataset ----
    output$downloadCsv.spd <- shiny::downloadHandler(
      filename = function() {
        loc <- paste("Split_Plot_Layout", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        df <- as.data.frame(csv_data()$file)
        write.csv(df, file, row.names = FALSE)
      }
    )
    app_reproduction_outputs(output, spd_reactive)
  })
}
