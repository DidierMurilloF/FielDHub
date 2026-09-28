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
        app_upload_ui(ns, "optim"),
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
            app_seed_input(ns("seed.spatial"), value = 5)
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

    optim_inputs <- shiny::eventReactive(input$RUN.optim, {
      planter_mov <- input$planter_mov.spatial
      expt_name <- as.character(input$expt_name.spatial)
      plotNumber <- validate_design(parse_whole_numbers(
        input$plot_start.spatial, "Starting Plot Number"
      ))
      site_names <- as.character(as.vector(unlist(strsplit(input$Location.spatial, ","))))
      seed_number <- validate_design(app_design_seed(input$seed.spatial))
      sites = as.numeric(input$l.optim)
      return(list(sites = sites, 
                  location_names = site_names, 
                  seed_number = seed_number, 
                  plotNumber = plotNumber,
                  planter_mov = planter_mov,
                  expt_name = expt_name)) 
    })

    shiny::observeEvent(optim_inputs()$sites, {
      # location_view_choices() validates the count before building the range.
      loc_user_view <- validate_design(location_view_choices(optim_inputs()$sites), report = TRUE)
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
        data_ingested <- app_read_upload(input, "optim")
        if (!is.null(data_ingested)) {
          data_up <- data_ingested$data
          data_up <- na.omit(data_up)
          data_up <- as.data.frame(data_up)
          if (ncol(data_up) < 3) {
            app_report_problem("Data input needs at least three columns with: ENTRY, NAME and REPS.")
            return(NULL)
          } 
          data_up <- as.data.frame(data_up[,1:3])
          data_up <- na.omit(data_up)
          colnames(data_up) <- c("ENTRY", "NAME", "REPS")
          if (!is.numeric(data_up$REPS) || !is.integer(data_up$REPS) ||
              is.factor(data_up$REPS)) {
            app_report_problem("'REPS' must be numeric.")
            return(NULL)
          }
          total_plots <- sum(data_up$REPS)
          counts <- NULL
        } else {
          return(NULL)
        }
      } else {
        shiny::req(input$amount.checks)
        shiny::req(input$lines.s)
        shiny::req(input$checks.s)
        # Text such as "8,x" is explained instead of becoming an NA that
        # fails the comparisons below
        r.checks <- app_attempt(parse_whole_numbers(input$amount.checks, "Input # Check's Reps"))
        if (is.null(r.checks)) return(NULL)
        checks.s <- as.numeric(input$checks.s)
        if (checks.s != length(r.checks)) {
          app_report_problem("The number of checks and the length of the reps vector must be equal.")
          return(NULL)
        } 
        total.checks <- sum(r.checks)
        n.checks <- as.numeric(input$checks.s)
        lines <- as.numeric(input$lines.s)
        if (lines <= sum(total.checks)) {
          app_report_problem("Number of lines should be greater then the number of checks.")
          return(NULL)
        }
        # optimized_arrangement() builds the CH/G entry list from the counts
        data_up <- NULL
        counts <- list(lines = lines, checks = n.checks, rep_checks = r.checks)
        total_plots <- sum(r.checks) + lines
      }
      dimension_choices <- validate_design(optimized_dimension_choices(total_plots))
      if (length(dimension_choices) == 0L) {
        shiny::updateSelectInput(inputId = "dimensions.s", choices = character(),
                          selected = character())
        app_report_problem("Please try a different number of treatments or checks.",
                           title = "No field dimensions available")
        return(NULL)
      }
      return(list(data_up.spatial = data_up, counts = counts, total_plots = total_plots,
                  dimension_choices = dimension_choices))
    })
    
    list_inputs <- shiny::eventReactive(input$RUN.optim, {
      shiny::req(get_data_optim())
      if (input$owndataOPTIM != 'Yes') {
        # get_data_optim() has already parsed the counts
        r.checks <- get_data_optim()$counts$rep_checks
        lines <- get_data_optim()$counts$lines
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

    app_upload_dialog_observer(input, "optim")

    shiny::observeEvent(input$RUN.optim, {
      shiny::req(get_data_optim())
      shinyjs::show(id = "dimensions.s")
      shinyjs::show(id = "get_random_optim")

    })

    output$data_input <- DT::renderDT({
      shiny::req(get_data_optim())
      if (input$dimensions.s == "No options available"){
        validate_design(fieldhub_abort("No options available for this number of treatments."))
      }
      test <- randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0
      if (!test) return(NULL)
      # The entry list of the design, generated or uploaded
      shiny::req(optimized_arrang())
      data_entry <- optimized_arrang()$dataEntry
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
        validate_design(fieldhub_abort("No options available for this number of treatments."))
      }
      test <- randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0
      if (!test) return(NULL)
      shiny::req(optimized_arrang())
        data_entry <- optimized_arrang()$dataEntry
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
        validate_design(fieldhub_abort("No options available for this number of treatments."))
      }
      values <- c(get_data_optim()$counts, list(
        nrows = field_dimensions_optim()$d_row,
        ncols = field_dimensions_optim()$d_col,
        planter = optim_inputs()$planter_mov,
        l = optim_inputs()$sites,
        plot_start = optim_inputs()$plotNumber,
        seed = optim_inputs()$seed_number,
        expt_name = optim_inputs()$expt_name,
        location_names = optim_inputs()$location_names
      ))
      data <- get_data_optim()$data_up.spatial
      validate_design(do.call(optimized_arrangement, design_args_Optim(values, data)))
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
      # Explain a design that has not been randomized (or failed)
      app_plot_state(if (test) app_design_state(optimized_arrang), NULL, "layout")
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
    
    app_spatial_workflow(input, output, session,
      design = function() optimized_arrang(),
      seed = function() validate_design(read_app_seed(input$seed.spatial)),
      dimensions = function(field_book) list(nrows = max(field_book$ROW), ncols = max(field_book$COLUMN)),
      selected = function() user_site_selection(),
      filename = function() {
        shiny::req(input$Location.spatial)
        loc <- input$Location.spatial
        loc <- paste(loc, "_", "Optim_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      visible = function() randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0,
      simulation_ready = function() {
        shiny::req(optimized_arrang()$fieldBook)
        test <- randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0
        if (test) {
            TRUE
        }
      },
      book_ready = function() {
        shiny::req(optimized_arrang()$fieldBook)
      },
      spec = spatial_workflow_spec("Optim")
    )

  })
}
