#' pREPS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_pREPS_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Single and Multi-Location P-rep Design"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        app_upload_ui(ns, "prep"),
        shiny::conditionalPanel(
			condition = "input.owndataPREPS == 'No'", 
			ns = ns,
			shiny::textInput(
				ns("repGens.preps"), 
				label = "# of Entries Per Rep Group:", 
				value = "75,150"
			),
			shiny::textInput(
				inputId = ns("repUnits.preps"),
				label = "# of Rep Per Group:",
				value = "2,1")
			),
			shiny::checkboxInput(
				inputId = ns("allow_fillers.preps"),
				label = "Allow filler plots",
				value = FALSE
			),
# 		sliderInput(ns("border_penalization"), 
# 		            label = "Border Penalization", 
# 		            min = 0.00, 
# 		            max = 1.00, 
# 		            value = 0.3),
# 		selectInput(
#       ns("optimization_distance_method"), 
#       label = "Optimization Distance Method:", 
#       choices = c("Euclidean" = "euclidean", "Manhattan" = "manhattan"), 
#       selected = "manhattan"
#       ),
			shiny::fluidRow(
				shiny::column(
					width = 6,
					shiny::numericInput(
						inputId = ns("l.preps"), 
						label = "Input # of Locations:", 
						value = 1, 
						min = 1)
				),
				shiny::column(
					width = 6,
					shiny::selectInput(
						inputId = ns("locView.preps"), 
						label = "Choose Location to View:", 
						choices = 1:1, 
						selected = 1,
						multiple = FALSE
					)
				)
			),
			shiny::selectInput(
				ns("planter_mov.preps"), 
				label = "Plot Order Layout:",
				choices = c("serpentine", "cartesian"), 
				multiple = FALSE,
				selected = "serpentine"
			),
			shiny::fluidRow(
				shiny::column(
					width = 6,
					shiny::textInput(
						ns("plot_start.preps"), 
						"Starting Plot Number:", 
						value = 1
					)
				),
				shiny::column(
					width = 6,
					shiny::textInput(
						ns("expt_name.preps"), 
						"Input Experiment Name:", 
						value = "Expt1"
					)
				)
			),  
			shiny::fluidRow(
				shiny::column(
					width = 6,
          app_seed_input(ns("seed.preps"), value = 4095)
				),
				shiny::column(
					width = 6,
					shiny::textInput(
						ns("Location.preps"), 
						"Input Location Name:", 
						value = "FARGO"
					)
				)
			),
			shiny::fluidRow(
				shiny::column(
					width = 6,
					shiny::actionButton(
						inputId = ns("RUN.prep"), 
						label = "Run!", 
						icon = shiny::icon("circle-nodes", verify_fa = FALSE),
						width = '100%'
					),
				),
				shiny::column(
					width = 6,
					shiny::actionButton(
						ns("Simulate.prep"), 
						label = "Simulate!", 
						icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
						width = '100%'
					),
				)
			),
			shiny::br(),
			shiny::uiOutput(ns("download_prep"))
		),
		shiny::mainPanel(
			width = 8,
			shinyjs::useShinyjs(),
			shiny::tabsetPanel(
				id = ns("tabset_prep"),
				shiny::tabPanel("Get Random", value = "tabPanel_prep",
					shiny::br(),
					shinyjs::hidden(
						shiny::selectInput(inputId = ns("dimensions.preps"),
									label = "Select dimensions of field:", 
									choices = "")
					),
					shinyjs::hidden(
					shiny::actionButton(ns("get_random_prep"), label = "Randomize!")
					),
					shiny::br(),
					shiny::br(),
					shiny::div(
					  fieldhub_spinner(
					    shiny::verbatimTextOutput(
					      outputId = ns("summary_prep"), 
					      placeholder = FALSE
					     ), 
					    type = 4
					  ),
					  style = "padding-right: 40px;"
					)
				),
				shiny::tabPanel("Data Input", DT::DTOutput(ns("dataup.preps"))),
				shiny::tabPanel("Randomized Field",
						fieldhub_spinner(
							DT::DTOutput(ns("dtpREPS")), 
							type = 4)
						),
				shiny::tabPanel("Plot Number Field", DT::DTOutput(ns("PREPSPLOTFIELD"))),
				shiny::tabPanel("Field Book", DT::DTOutput(ns("pREPSOUTPUT"))),
				shiny::tabPanel("Heatmap", plotly::plotlyOutput(ns("heatmap_prep"), width = "97%"))
			)
		)
    )
  )
}
#' pREPS Server Functions
#'
#' @noRd 
mod_pREPS_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    prep_inputs <- shiny::eventReactive(input$RUN.prep, {
      planter_mov <- input$planter_mov.preps
      expt_name <- as.character(input$expt_name.preps)
      plotNumber <- validate_design(parse_whole_numbers(
        input$plot_start.preps, "Starting Plot Number"
      ))
      site_names <- as.character(as.vector(unlist(strsplit(input$Location.preps, ","))))
      seed_number <- validate_design(app_design_seed(input$seed.preps))
      sites = as.numeric(input$l.preps)
      return(list(sites = sites, 
                  location_names = site_names, 
                  seed_number = seed_number, 
                  plotNumber = plotNumber,
                  planter_mov = planter_mov,
                  expt_name = expt_name)) 
    })

    shiny::observeEvent(prep_inputs()$sites, {
      # location_view_choices() validates the count before building the range.
      loc_user_view <- validate_design(location_view_choices(prep_inputs()$sites), report = TRUE)
      shiny::updateSelectInput(inputId = "locView.preps",
                        choices = loc_user_view,
                        selected = loc_user_view[1])
    })

    shiny::observeEvent(input$owndataPREPS,
                 handlerExpr = shiny::updateTabsetPanel(session,
                                                 "tabset_prep",
                                                 selected = "tabPanel_prep"))
    shiny::observeEvent(input$RUN.prep,
                 handlerExpr = shiny::updateTabsetPanel(session,
                                                 "tabset_prep",
                                                 selected = "tabPanel_prep"))
    
    get_data_prep <- shiny::eventReactive(input$RUN.prep, {
      if (input$owndataPREPS == 'Yes') {
        data_ingested <- app_read_upload(input, "prep")
        if (!is.null(data_ingested)) {
          data_up <- data_ingested$data
          data_up <- na.omit(data_up)
          data_preps <- as.data.frame(data_up)
          if (ncol(data_preps) < 3) {
            app_report_problem("Data input needs at least three columns with: ENTRY, NAME and REPS.")
            return(NULL)
          }
          data_preps <- as.data.frame(data_preps[,1:3])
          colnames(data_preps) <- c("ENTRY", "NAME", "REPS")
          if (!is.numeric(data_preps$REPS) || !is.integer(data_preps$REPS) ||
              is.factor(data_preps$REPS)) {
            app_report_problem("'REPS' must be numeric.")
            return(NULL)
          }
          total_plots <- sum(data_preps$REPS)
        } else {
          return(NULL)
        }
      } else {
        # parse_rep_groups() reads both inputs as whole numbers of the same
        # length, so text such as "75,abc" is explained instead of reaching
        # partially_replicated() as an NA.
        groups <- app_attempt(parse_rep_groups(input$repGens.preps, input$repUnits.preps))
        if (is.null(groups)) return(NULL)
        # partially_replicated() builds the G1.. entry list from the counts
        return(list(data_up.preps = NULL, repGens = groups$repGens,
                    repUnits = groups$repUnits, total_plots = groups$total_plots))
      }
      return(list(data_up.preps = data_preps, total_plots = total_plots))
    })
    
    list_input_plots <- shiny::eventReactive(input$RUN.prep, {
      shiny::req(get_data_prep())
      # get_data_prep() has already parsed the group inputs (or the file)
      list(n_plots = get_data_prep()$total_plots, input$owndataPREPS)
    })
    
    shiny::observeEvent(list(list_input_plots(), input$allow_fillers.preps), {
      shiny::req(get_data_prep())
      shiny::req(input$owndataPREPS)
      shiny::req(get_data_prep()$total_plots)
      options <- validate_design(prep_dimension_options(
        total_plots = get_data_prep()$total_plots,
        allow_fillers = isTRUE(input$allow_fillers.preps),
        max_fillers = .prep_max_fillers
      ), report = TRUE)
      if (is.null(options)) {
        choices <- "No options available"
      } else {
        choices <- stats::setNames(options$value, options$label)
      }
      shiny::updateSelectInput(inputId = "dimensions.preps",
                        choices = choices,
                        selected = choices[1])
      if (is.null(options)) {
        shinyjs::hide(id = "get_random_prep")
        problem <- prep_no_dimensions_problem(input$allow_fillers.preps)
        app_report_problem(problem$message, severity = problem$severity,
                           title = problem$title)
      } else {
        shinyjs::show(id = "get_random_prep")
      }
    })
    
    field_dimensions_prep <- shiny::eventReactive(input$get_random_prep, {
      shiny::req(get_data_prep())
      if (input$dimensions.preps == "No options available") return(NULL)
      dims <- unlist(strsplit(input$dimensions.preps," x "))
      d_row <- as.numeric(dims[1])
      d_col <- as.numeric(dims[2])
      return(list(d_row = d_row, d_col = d_col))
    })

    randomize_hit_prep <- shiny::reactiveValues(times = 0)
 
    shiny::observeEvent(input$RUN.prep, {
      randomize_hit_prep$times <- 0
    })

    user_tries_prep <- shiny::reactiveValues(tries_prep = 0)

    shiny::observeEvent(input$get_random_prep, {
      user_tries_prep$tries_prep <- user_tries_prep$tries_prep + 1
      randomize_hit_prep$times <- randomize_hit_prep$times + 1
    })

    shiny::observeEvent(input$dimensions.preps, {
      user_tries_prep$tries_prep <- 0
    })

    list_to_observe_prep <- shiny::reactive({
      list(randomize_hit_prep$times, user_tries_prep$tries_prep)
    })

    shiny::observeEvent(list_to_observe_prep(), {
      output$download_prep <- shiny::renderUI({
        if (randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0) {
          shiny::downloadButton(ns("downloadData.preps"),
                          "Save experiment (ZIP)",
                          style = "width:100%")
        }
      })
    })

    app_upload_dialog_observer(input, "prep")

    shiny::observeEvent(input$RUN.prep, {
      shiny::req(get_data_prep())
      shinyjs::show(id = "dimensions.preps")
    })

    ###### Plotting the data ##############
    output$dataup.preps <- DT::renderDT({
      shiny::req(get_data_prep())
      test <- randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0
      if (!test) return(NULL)
      # The entry list of the design, generated or uploaded
      shiny::req(pREPS_reactive())
      data_entry.preps <- pREPS_reactive()$dataEntry
      df <- as.data.frame(data_entry.preps)
      df$ENTRY <- as.factor(df$ENTRY)
      df$NAME <- as.factor(df$NAME)
      df$REPS <- as.factor(df$REPS)
      table_options <- list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px")
      DT::datatable(df,
                    rownames = FALSE, 
                    filter = 'top',
                    options = utils::modifyList(table_options, list(
        columnDefs = list(list(className = 'dt-center', targets = "_all")))))
    })
    
    pREPS_reactive <- shiny::reactive({
      shiny::req(get_data_prep())
      values <- list(
        nrows = field_dimensions_prep()$d_row,
        ncols = field_dimensions_prep()$d_col,
        repGens = get_data_prep()$repGens,
        repUnits = get_data_prep()$repUnits,
        planter = prep_inputs()$planter_mov,
        l = prep_inputs()$sites,
        plot_start = prep_inputs()$plotNumber,
        seed = prep_inputs()$seed_number,
        expt_name = prep_inputs()$expt_name,
        location_names = prep_inputs()$location_names,
        allow_fillers = isTRUE(input$allow_fillers.preps)
      )
      data <- get_data_prep()$data_up.preps
      shiny::withProgress(message = 'Running p-rep optimization ...', {
          validate_design(do.call(partially_replicated, design_args_pREPS(values, data)))
      })
    }) |> 
      shiny::bindEvent(input$get_random_prep)

    output$summary_prep <- shiny::renderPrint({
      shiny::req(get_data_prep())
      test <- randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0
      if (test) {
        cat("Randomization was successful!", "\n", "\n")
        print(pREPS_reactive(), n = 6)
      }
    })
    
     user_site_selection <- shiny::reactive({
       return(as.numeric(input$locView.preps))
     })

    output$BINARYpREPS <- DT::renderDT({
      if (user_tries_prep$tries_prep < 1) return(NULL)
      shiny::req(pREPS_reactive())
      selection <- as.numeric(user_site_selection())
      B <- pREPS_reactive()$binaryField[[selection]]
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
                        backgroundColor = DT::styleEqual(1, "gray"))
    })

    output$dtpREPS <- DT::renderDataTable({
      test <- randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0
      # Explain a design that has not been randomized (or failed)
      app_plot_state(if (test) app_design_state(pREPS_reactive), NULL, "layout")
      shiny::req(pREPS_reactive())
      selection <- as.numeric(user_site_selection())
      w_map <- pREPS_reactive()$layoutRandom[[selection]]
      w_map[pREPS_reactive()$fillerField[[selection]]] <- "Filler"
      checks = as.vector(pREPS_reactive()$genEntries[[1]])
      len_checks <- length(checks)
      colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                   'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
      
      df <- as.data.frame(w_map)
      
      gens <- as.vector(unlist(pREPS_reactive()$genEntries[[2]]))
      
      rownames(df) <- nrow(df):1
      colnames(df) <- paste0('V', 1:ncol(df))
      table_options <- list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollY = "700px")
      DT::datatable(df,
                    extensions = 'Buttons', 
                     options = utils::modifyList(table_options, list(dom = 'Blfrtip',
                     scrollX = TRUE,
                     fixedColumns = TRUE,
                     pageLength = nrow(df),
                     scrollY = "700px",
                     class = 'compact cell-border stripe',  rownames = FALSE,
                     server = FALSE,
                     filter = list( position = 'top', clear = FALSE, plain =TRUE ),
                     buttons = app_table_export_buttons(pREPS_reactive(), "Entry layout", user_site_selection()),
                     lengthMenu = list(c(10,25,50,-1),
                                       c(10,25,50,"All"))))) |>
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                    backgroundColor = DT::styleEqual(c(checks), # c(checks,gens)
                                                 c(rep(colores[3], len_checks)) # , rep('yellow', length(gens))
                    )
      )
    })
    
    output$PREPSPLOTFIELD <- DT::renderDT({
      test <- randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0
      if (!test) return(NULL)
      shiny::req(pREPS_reactive())
      plot_num <- pREPS_reactive()$plotNumber[[user_site_selection()]]
      a <- as.vector(as.matrix(plot_num))
      len_a <- length(a)
      df <- as.data.frame(plot_num)
      rownames(df) <- nrow(df):1
      table_options <- list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "700px")
      DT::datatable(df,
                    extensions = 'Buttons', 
                    options = utils::modifyList(table_options, list(dom = 'Blfrtip',
                                   scrollX = TRUE,
                                   fixedColumns = TRUE,
                                   pageLength = nrow(df),
                                   scrollY = "700px",
                                   class = 'compact cell-border stripe',  rownames = FALSE,
                                   server = FALSE,
                                   filter = list( position = 'top', clear = FALSE, plain =TRUE ),
                                   buttons = app_table_export_buttons(pREPS_reactive(), "Plot numbers", user_site_selection()),
                                   lengthMenu = list(c(10,25,50,-1),
                                                     c(10,25,50,"All"))))
                    
                    )
    })

    app_spatial_workflow(input, output, session,
      design = function() pREPS_reactive(),
      seed = function() validate_design(read_app_seed(prep_inputs()$seed_number)),
      dimensions = function(field_book) list(nrows = field_dimensions_prep()$d_row, ncols = field_dimensions_prep()$d_col),
      selected = function() user_site_selection(),
      filename = function() {
        shiny::req(input$Location.preps)
        loc <- input$Location.preps
        loc <- paste(loc, "_", "pREP_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      visible = function() randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0,
      simulation_ready = function() {
        shiny::req(pREPS_reactive()$fieldBook)
        test <- randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0
        if (test) {
            TRUE
        }
      },
      book_ready = function() {
        shiny::req(pREPS_reactive()$fieldBook)
        shiny::req(prep_inputs())
      },
      spec = spatial_workflow_spec("pREPS")
    )

  })
}
