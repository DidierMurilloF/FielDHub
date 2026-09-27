#' Optim UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_Optim_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Unreplicated Optimized Arrangement"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        shiny::radioButtons(inputId = ns("owndataOPTIM"),
                    label = "Import Entries' List?", 
                    choices = c("Yes", "No"), 
                    selected = "No",
                    inline = TRUE, 
                    width = NULL, 
                    choiceNames = NULL, 
                    choiceValues = NULL),
       shiny::conditionalPanel(
         condition = "input.owndataOPTIM == 'Yes'", 
         ns = ns,
         shiny::fluidRow(
          shiny::column(7, style=list("padding-right: 28px;"),
                  shiny::fileInput(ns("file3"),
                            label = "Upload a CSV File:", 
                            multiple = FALSE)),
          shiny::column(5,style=list("padding-left: 5px;"),
                  shiny::radioButtons(ns("sep.OPTIM"), "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              selected = ","))
         )
       ),
       shiny::conditionalPanel(
         "input.owndataOPTIM != 'Yes'", 
          ns = ns,
          shiny::numericInput(ns("checks.s"),
                        label = "Input # of Checks:", 
                        value = 4,
                        min = 1),
          shiny::textInput(ns("amount.checks"),
                    "Input # Check's Reps:",
                    value = "8,8,8,8"),
          shiny::numericInput(ns("lines.s"),
                      label = "Input # of Entries:",
                      value = 280, min = 5)           
       ),
       shiny::selectInput(ns("planter_mov.spatial"),
                   label = "Plot Order Layout:",
                   choices = c("serpentine", "cartesian"),
                   multiple = FALSE, 
                   selected = "serpentine"),
       shiny::fluidRow(
         shiny::column(6,
                style=list("padding-right: 28px;"),
                shiny::numericInput(inputId = ns("l.optim"),
                             label = "Input # of Locations:", 
                             value = 1,
                             min = 1)
         ),
         shiny::column(6,style=list("padding-left: 5px;"),
                shiny::selectInput(inputId = ns("locView.optim"),
                            label = "Choose location to view:", 
                            choices = 1:1, 
                            selected = 1,
                            multiple = FALSE)
         )
       ),
       shiny::fluidRow(
         shiny::column(6,style=list("padding-right: 28px;"),
                shiny::textInput(
                    ns("plot_start.spatial"), 
                    "Starting Plot Number:", 
                    value = 1
                )
         ),
         shiny::column(6,style=list("padding-left: 5px;"),
                shiny::textInput(ns("expt_name.spatial"),
                          "Input Experiment Name:", 
                          value = "Expt1")
         )
       ),  
       
       shiny::fluidRow(
         shiny::column(
            width = 6,
            style=list("padding-right: 28px;"),
            shiny::numericInput(
                ns("seed.spatial"), 
                label = "Random Seed:", 
                value = 5,
                min = 1
            )
         ),
         shiny::column(6,style=list("padding-left: 5px;"),
                shiny::textInput(ns("Location.spatial"),
                          "Input Location:", 
                          value = "FARGO")
         )
       ),
       shiny::fluidRow(
         shiny::column(6,
                shiny::actionButton(
                  inputId = ns("RUN.optim"), 
                  label = "Run!", 
                  icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                  width = '100%'),
         ),
         shiny::column(6,
                shiny::actionButton(
                  ns("Simulate.optim"), 
                  label = "Simulate!", 
                  icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                  width = '100%'),
         )
       ),
       shiny::br(),
       shiny::uiOutput(ns("download_expt_optim"))
      ),
      shiny::mainPanel(
        width = 8,
        shinyjs::useShinyjs(),
        shiny::tabsetPanel(id = ns("tabset_optim"),
        shiny::tabPanel("Get Random", value = "tabPanel_optim",
          shiny::br(),
          shinyjs::hidden(
            shiny::selectInput(inputId = ns("dimensions.s"),
                            label = "Select dimensions of field:",
                            choices = "")
          ),
          shinyjs::hidden(
            shiny::actionButton(ns("get_random_optim"), label = "Randomize!")
          ),
          shiny::br(),
          shiny::br(),
          fieldhub_spinner(
            shiny::verbatimTextOutput(outputId = ns("summary_optim"),
                               placeholder = FALSE), 
              type = 4
           )
        ),
          shiny::tabPanel("Data Input",
                   shiny::fluidRow(
                     shiny::column(6,DT::DTOutput(ns("data_input"))),
                     shiny::column(6,DT::DTOutput(ns("table_checks")))
                   )
          ),
          shiny::tabPanel("Randomized Field", DT::DTOutput(ns("RFIELD"))),
          shiny::tabPanel("Plot Number Field", DT::DTOutput(ns("PLOTFIELD"))),
          shiny::tabPanel("Field Book", DT::DTOutput(ns("OPTIMOUTPUT"))),
          shiny::tabPanel("Heatmap",
            plotly::plotlyOutput(ns("heatmap"), width = "97%"))
         )
      )
    )
  )
}
#' Optim Server Functions
#'
#' @noRd 
mod_Optim_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    shinyjs::useShinyjs()

    optim_inputs <- shiny::eventReactive(input$RUN.optim, {
      planter_mov <- input$planter_mov.spatial
      expt_name <- as.character(input$expt_name.spatial)
      plotNumber <- validate_design(read_whole_numbers(
        input$plot_start.spatial, "Starting Plot Number"
      ))
      site_names <- as.character(as.vector(unlist(strsplit(input$Location.spatial, ","))))
      seed_number <- as.numeric(input$seed.spatial)
      sites = as.numeric(input$l.optim)
      return(list(sites = sites, 
                  location_names = site_names, 
                  seed_number = seed_number, 
                  plotNumber = plotNumber,
                  planter_mov = planter_mov,
                  expt_name = expt_name)) 
    })

    shiny::observeEvent(optim_inputs()$sites, {
      loc_user_view <- 1:as.numeric(optim_inputs()$sites)
      shiny::updateSelectInput(inputId = "locView.optim",
                        choices = loc_user_view, 
                        selected = loc_user_view[1])
    })

    shiny::observeEvent(input$owndataOPTIM,
                 handlerExpr = shiny::updateTabsetPanel(session,
                                                 "tabset_optim",
                                                 selected = "tabPanel_optim"))
    shiny::observeEvent(input$RUN.optim,
                 handlerExpr = shiny::updateTabsetPanel(session,
                                                 "tabset_optim",
                                                 selected = "tabPanel_optim"))

    get_data_optim <- shiny::eventReactive(input$RUN.optim, {
      if (input$owndataOPTIM == "Yes") {
        shiny::req(input$file3)
        inFile <- input$file3
        data_ingested <- load_file(name = inFile$name, 
                                   path = inFile[["datapath"]],
                                   sep = input$sep.OPTIM,
                                   check = TRUE, 
                                   design = "optim")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_up <- na.omit(data_up)
          data_up <- as.data.frame(data_up)
          if (ncol(data_up) < 3) {
            shinyalert::shinyalert(
              "Error!!", 
              "Data input needs at least three columns with: ENTRY, NAME and REPS.", 
              type = "error")
            return(NULL)
          } 
          data_up <- as.data.frame(data_up[,1:3])
          data_up <- na.omit(data_up)
          colnames(data_up) <- c("ENTRY", "NAME", "REPS")
          if(!is.numeric(data_up$REPS) || !is.integer(data_up$REPS) ||
             is.factor(data_up$REPS)) shiny::validate("'REPS' must be numeric.")
          total_plots <- sum(data_up$REPS)
        } else {
          app_upload_error(data_ingested,
                           missing_columns = "Data input needs at least three columns with: ENTRY, NAME and REPS.")
          return(NULL)
        }
      } else {
        shiny::req(input$amount.checks)
        shiny::req(input$lines.s)
        shiny::req(input$checks.s)
        r.checks <- as.numeric(unlist(strsplit(input$amount.checks, ",")))
        checks.s <- as.numeric(input$checks.s)
        if(checks.s != length(r.checks)) {
          shinyalert::shinyalert(
            "Error!!",
            "The number of checks and the length of the reps vector must be equal.",
            type = "error"
          )
          return(NULL)
        } 
        total.checks <- sum(r.checks)
        n.checks <- as.numeric(input$checks.s)
        lines <- as.numeric(input$lines.s)
        if (lines <= sum(total.checks)) {
          shinyalert::shinyalert(
            "Error!!",
            "Number of lines should be greater then the number of checks.",
            type = "error"
          )
          return(NULL)
        }
        entries <- dplyr::bind_rows(
          default_entries(n.checks, prefix = "CH"),
          default_entries(lines, prefix = "G", start = n.checks + 1)
        )
        reps.checks <- r.checks
        REPS <- c(reps.checks, rep(1, lines))
        gen.list <- data.frame(entries, REPS = REPS)
        data_up <- gen.list
        total_plots <- sum(data_up$REPS)
      }
      dimension_choices <- validate_design(optimized_dimension_choices(total_plots))
      if (length(dimension_choices) == 0L) {
        shiny::updateSelectInput(inputId = "dimensions.s", choices = character(),
                          selected = character())
        shinyalert::shinyalert(
          "No field dimensions available",
          "Please try a different number of treatments or checks.",
          type = "error"
        )
        return(NULL)
      }
      return(list(data_up.spatial = data_up, total_plots = total_plots,
                  dimension_choices = dimension_choices))
    })
    
    list_inputs <- shiny::eventReactive(input$RUN.optim, {
      shiny::req(get_data_optim())
      if (input$owndataOPTIM != 'Yes') {
        shiny::req(input$amount.checks)
        shiny::req(input$lines.s)
        r.checks <- as.numeric(unlist(strsplit(input$amount.checks, ",")))
        lines <- as.numeric(input$lines.s)
        return(list(r.checks=r.checks, lines = lines, input$owndataOPTIM))
      } else {
        n_plots <- get_data_optim()$total_plots
        return(list(n_plots = n_plots, input$owndataOPTIM))
      }
    })
    
    shiny::observeEvent(list_inputs(), {
      shiny::req(get_data_optim())
      choices <- get_data_optim()$dimension_choices
      shiny::updateSelectInput(inputId = "dimensions.s",
                        choices = choices, 
                        selected = head(choices, 1))
    })
    
    field_dimensions_optim <- shiny::eventReactive(input$get_random_optim, {
      shiny::req(get_data_optim())
      dims <- unlist(strsplit(input$dimensions.s," x "))
      d_row <- as.numeric(dims[1])
      d_col <- as.numeric(dims[2])
      return(list(d_row = d_row, d_col = d_col))
    })

    randomize_hit_optim <- shiny::reactiveValues(times = 0)
 
    shiny::observeEvent(input$RUN.optim, {
      randomize_hit_optim$times <- 0
    })

    user_tries_optim <- shiny::reactiveValues(tries_optim = 0)

    shiny::observeEvent(input$get_random_optim, {
      user_tries_optim$tries_optim <- user_tries_optim$tries_optim + 1
      randomize_hit_optim$times <- randomize_hit_optim$times + 1
    })

    shiny::observeEvent(input$dimensions.s, {
      user_tries_optim$tries_optim <- 0
    })

    list_to_observe_optim <- shiny::reactive({
      list(randomize_hit_optim$times, user_tries_optim$tries_optim)
    })

    shiny::observeEvent(list_to_observe_optim(), {
      output$download_expt_optim <- shiny::renderUI({
        if (randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0) {
          shiny::downloadButton(ns("downloadData.spatial"),
                          "Save experiment (ZIP)",
                          style = "width:100%")
        }
      })
    })

    entryListFormat_OPTIM <- data.frame(ENTRY = 1:9, 
                                        NAME = c(c("CHECK1", "CHECK2","CHECK3"), 
                                                 paste("Genotype", LETTERS[1:6], sep = "")),
                                        REPS = as.factor(c(rep(10, times = 3), rep(1,6))))
    
    entriesInfoModal_OPTIM <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(entryListFormat_OPTIM,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        shiny::h4("Note that the controls must be in the first rows of the CSV file."),
        easyClose = FALSE
      )
    }
    
    toListen <- shiny::reactive({
      list(input$owndataOPTIM)
    })
    
    shiny::observeEvent(toListen(), {
      if (input$owndataOPTIM == "Yes") {
        shiny::showModal(
          entriesInfoModal_OPTIM()
        )
      }
    })

    shiny::observeEvent(input$RUN.optim, {
      shiny::req(get_data_optim())
      shinyjs::show(id = "dimensions.s")
      shinyjs::show(id = "get_random_optim")

    })

    output$data_input <- DT::renderDT({
      shiny::req(get_data_optim())
      if (input$dimensions.s == "No options available"){
        shiny::validate("No options available for this number of treatments")
      }
      test <- randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0
      if (!test) return(NULL)
      shiny::req(get_data_optim()$data_up.spatial)
      data_entry <- get_data_optim()$data_up.spatial
      df <- as.data.frame(data_entry)
      df$ENTRY <- as.factor(df$ENTRY)
      df$NAME <- as.factor(df$NAME)
      df$REPS <- as.factor(df$REPS)
      table_options <- list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "600px")
      DT::datatable(df,
                    filter = "top",
                    rownames = FALSE, 
                    caption = 'List of Entries.', 
                    options = utils::modifyList(table_options, list(columnDefs = list(list(className = 'dt-center', targets = "_all"))))
      )
    })
    
    output$table_checks <- DT::renderDT({
      shiny::req(get_data_optim())
      if (input$dimensions.s == "No options available") {
        shiny::validate("No options available for this number of treatments")
      }
      test <- randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0
      if (!test) return(NULL)
      shiny::req(get_data_optim()$data_up.spatial)
        data_entry <- get_data_optim()$data_up.spatial
        checks_input <- data_entry[data_entry$REPS > 1, ]
        df <- as.data.frame(checks_input)
        table_options <- list(pageLength = nrow(df), autoWidth = FALSE,
                                  scrollX = TRUE, scrollY = "350px")
        a <- ncol(df) - 1
        DT::datatable(df, rownames = FALSE, caption = 'Table of checks.', options = utils::modifyList(table_options, list(
          columnDefs = list(list(className = 'dt-left', targets = 0:a)))))
    })
    
    optimized_arrang <- shiny::eventReactive(input$get_random_optim, {
      shiny::req(get_data_optim())
      if (input$dimensions.s == "No options available") {
        shiny::validate("No options available for this number of treatments")
      }
      shiny::req(get_data_optim()$data_up.spatial)
      nrows <- field_dimensions_optim()$d_row
      ncols <- field_dimensions_optim()$d_col
      niter <- 1000
      
      data.spatial <- get_data_optim()$data_up.spatial
      sites <- optim_inputs()$sites
      site_names <- optim_inputs()$location_names
      seed.spatial <- optim_inputs()$seed_number
      plotNumber <- optim_inputs()$plotNumber
      movement_planter <- optim_inputs()$planter_mov
      expt_name <- optim_inputs()$expt_name

      optimized <- validate_design(optimized_arrangement(
        nrows = nrows,
        ncols = ncols, 
        locationNames = site_names,
        planter = movement_planter,
        plotNumber = plotNumber,
        l = sites, 
        exptName = expt_name,
        spread_reps = TRUE,
        seed = seed.spatial, 
        data = data.spatial
      ))
    })

    output$summary_optim <- shiny::renderPrint({
      shiny::req(get_data_optim())
      test <- randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0
      #if (!test) return(NULL)
      if (test) {
        cat("Randomization was successful!", "\n", "\n")
        # len <- length(optimized_arrang()$infoDesign)
        #  optimized_arrang()$infoDesign[1:(len - 1)]
        print(optimized_arrang())
      }
    })

    user_site_selection <- shiny::reactive({
      return(as.numeric(input$locView.optim))
    })

    output$BINARY <- DT::renderDT({
      shiny::req(get_data_optim())
      test <- randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0
      if (!test) return(NULL)
      # if (user_tries_optim$tries_optim < 1) return(NULL)
      shiny::req(optimized_arrang())
      B <- optimized_arrang()$binaryField[[user_site_selection()]]
      df <- as.data.frame(B)
      rownames(df) <- nrow(df):1
      table_options <- list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "700px")
      DT::datatable(df,
                    extensions = 'FixedColumns',
                    options = utils::modifyList(table_options, list(
                      dom = 't',
                      scrollX = TRUE,
                      fixedColumns = TRUE
                    ))) |>
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                        backgroundColor = DT::styleEqual(1, 
                                                         c("gray")))
    })
    
    output$RFIELD <- DT::renderDT({
      test <- randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0
      if (!test) return(NULL)
      # if (user_tries_optim$tries_optim < 1) return(NULL)
      shiny::req(optimized_arrang())
      w_map <- optimized_arrang()$layoutRandom[[user_site_selection()]]
      checks = as.vector(optimized_arrang()$genEntries[[1]])
      len_checks <- length(checks)
      colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                   'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
      gens <- as.vector(optimized_arrang()$genEntries[[2]])
      df <- as.data.frame(w_map)
      rownames(df) <- nrow(df):1
      colnames(df) <- paste0('V', 1:ncol(df))
      DT::datatable(df,
                    extensions = c('Buttons', 'FixedColumns'),
                    options = list(dom = 'Blfrtip',
                                   scrollX = TRUE,
                                   fixedColumns = TRUE,
                                   pageLength = nrow(df),
                                   scrollY = "700px",
                                   class = 'compact cell-border stripe',  rownames = FALSE,
                                   server = FALSE,
                                   filter = list( position = 'top', clear = FALSE, plain =TRUE ),
                                   buttons = app_table_export_buttons(optimized_arrang(), "Entry layout", user_site_selection()),
                                   lengthMenu = list(c(10,25,50,-1),
                                                     c(10,25,50,"All")))) |> 
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                    backgroundColor = DT::styleEqual(checks,
                                                 c(colores[1:len_checks])
                    )
        )
    })
    
    output$PLOTFIELD <- DT::renderDT({
      test <- randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0
      if (!test) return(NULL)
      # if (user_tries_optim$tries_optim < 1) return(NULL)
      shiny::req(optimized_arrang())
      plot_num <- optimized_arrang()$plotNumber[[user_site_selection()]]
      a <- as.vector(as.matrix(plot_num))
      len_a <- length(a)
      df <- as.data.frame(plot_num)
      rownames(df) <- nrow(df):1
      DT::datatable(df,
                    extensions = c('Buttons', 'FixedColumns'),
                    options = list(dom = 'Blfrtip',
                                   scrollX = TRUE,
                                   fixedColumns = TRUE,
                                   pageLength = nrow(df),
                                   scrollY = "700px",
                                   class = 'compact cell-border stripe',  rownames = FALSE,
                                   server = FALSE,
                                   filter = list( position = 'top', clear = FALSE, plain =TRUE ),
                                   buttons = app_table_export_buttons(optimized_arrang(), "Plot numbers", user_site_selection()),
                                   lengthMenu = list(c(10,25,50,-1),
                                                     c(10,25,50,"All")))
                    )
    })
    
    valsOPTIM <- shiny::reactiveValues(ROX = NULL, ROY = NULL, trail.optim = NULL, minValue = NULL,
                                maxValue = NULL)
    
    simuModal.OPTIM <- function(failed = FALSE) {
      shiny::modalDialog(
        shiny::fluidRow(
          shiny::column(6,
                 shiny::selectInput(inputId = ns("trailsOPTIM"), label = "Select One:",
                             choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
          ),
          shiny::column(6,
                 shiny::checkboxInput(inputId = ns("heatmap_s"), label = "Include a Heatmap", value = TRUE),
          )
        ),
        shiny::conditionalPanel("input.trailsOPTIM == 'Other'", ns = ns,
                         shiny::textInput(inputId = ns("OtherOPTIM"), label = "Input Trial Name:", value = NULL)
        ),
        app_spatial_correlations(ns, ".O"),
        shiny::fluidRow(
          shiny::column(6,
                 shiny::numericInput(inputId = ns("min.optim"), "Input the min value:", value = NULL)
          ),
          shiny::column(6,
                 shiny::numericInput(inputId = ns("max.optim"), "Input the max value:", value = NULL)
                 
          )
        ),
        if (failed)
          shiny::div(shiny::tags$b("Invalid input of data max and min", style = "color: red;")),
        
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(inputId = ns("ok.optim"), "GO")
        )
      )
    }
    
    shiny::observeEvent(input$Simulate.optim, {
      shiny::req(optimized_arrang()$fieldBook)
      test <- randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0
      if (test) {
        shiny::showModal(
          simuModal.OPTIM()
        )
      }
    })
    
    shiny::observeEvent(input$ok.optim, {
      shiny::req(input$min.optim, input$max.optim)
      if (input$max.optim > input$min.optim & input$min.optim != input$max.optim) {
        valsOPTIM$maxValue <- input$max.optim
        valsOPTIM$minValue  <- input$min.optim
        valsOPTIM$ROX <- as.numeric(input$ROX.O)
        valsOPTIM$ROY <- as.numeric(input$ROY.O)
        if(input$trailsOPTIM == "Other") {
          shiny::req(input$OtherOPTIM)
          if(!is.null(input$OtherOPTIM)) {
            valsOPTIM$trail.optim <- as.character(input$OtherOPTIM)
          }else shiny::showModal(simuModal.OPTIM(failed = TRUE))
        }else {
          valsOPTIM$trail.optim <- as.character(input$trailsOPTIM)
        }
        shiny::removeModal()
      }else {
        shiny::showModal(
          simuModal.OPTIM(failed = TRUE)
        )
      }
    })
    
    simuDataOPTIM <- shiny::reactive({
      shiny::req(optimized_arrang()$fieldBook)
      field_book <- optimized_arrang()$fieldBook
      if (is.null(valsOPTIM$maxValue) || is.null(valsOPTIM$minValue) ||
          is.null(valsOPTIM$trail.optim)) {
        return(list(df = field_book, simulation = NULL))
      }
      simulation <- validate_design(simulate_spatial_field_book(
        field_book = field_book,
        nrows = max(field_book$ROW), ncols = max(field_book$COLUMN),
        correlation_x = as.numeric(valsOPTIM$ROX),
        correlation_y = as.numeric(valsOPTIM$ROY),
        min_value = as.numeric(valsOPTIM$minValue),
        max_value = as.numeric(valsOPTIM$maxValue),
        response_name = as.character(valsOPTIM$trail.optim),
        seed = as.numeric(input$seed.spatial)
      ))
      list(df = simulation$field_book, dfSimulation = simulation$simulations,
           simulation = simulation)
    })

    heat_map_optim <- shiny::reactiveValues(heat_map_option = FALSE)
    
    shiny::observeEvent(input$ok.optim, {
      shiny::req(input$min.optim, input$max.optim)
      if (input$max.optim > input$min.optim & input$min.optim != input$max.optim) {
        heat_map_optim$heat_map_option <- TRUE
      }
    })
    
    shiny::observeEvent(heat_map_optim$heat_map_option, {
      if (heat_map_optim$heat_map_option == FALSE) {
        shiny::hideTab(inputId = "tabset_optim", target = "Heatmap")
      } else {
        shiny::showTab(inputId = "tabset_optim", target = "Heatmap")
      }
    })
    
    output$OPTIMOUTPUT <- DT::renderDT({
      test <- randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0
      if (!test) return(NULL)
      # if (user_tries_optim$tries_optim < 1) return(NULL)
      shiny::req(simuDataOPTIM()$df)
      df <- simuDataOPTIM()$df
      validate_design(app_field_book_table(
        df, factor_columns = c("EXPT", "LOCATION", "PLOT", "ROW", "COLUMN", "CHECKS", "ENTRY", "TREATMENT"),
        height = 600
      ))
    })
    
    
    heatmap_obj <- shiny::reactive({
      shiny::req(simuDataOPTIM()$dfSimulation)
      shiny::req(input$heatmap_s)
      validate_design(app_spatial_heatmap(
        simuDataOPTIM()$dfSimulation,
        response_name = as.character(valsOPTIM$trail.optim),
        selected = user_site_selection(), height = 740, show_title = TRUE
      ))
    })
    
    output$heatmap <- plotly::renderPlotly({
      test <- randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0
      if (!test) return(NULL)
      shiny::req(heatmap_obj())
      heatmap_obj()
    })
    
    output$downloadData.spatial <- app_csv_archive(
      filename = function() {
        shiny::req(input$Location.spatial)
        loc <- input$Location.spatial
        loc <- paste(loc, "_", "Optim_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      data = function() as.data.frame(simuDataOPTIM()$df),
      design = optimized_arrang,
      field_book = function() simuDataOPTIM()$df,
      simulation = function() simuDataOPTIM()$simulation,
      kind = "field_book"
    )
    
    app_reproduction_outputs(output, optimized_arrang)
  })
}
