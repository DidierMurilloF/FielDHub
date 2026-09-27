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
        shiny::radioButtons(
			inputId = ns("owndataPREPS"), 
			label = "Import entries' list?", 
			choices = c("Yes", "No"), 
			selected = "No",
			inline = TRUE, 
			width = NULL, 
			choiceNames = NULL, 
			choiceValues = NULL
		),
    shiny::conditionalPanel(
			condition = "input.owndataPREPS == 'Yes'", 
			ns = ns,
			shiny::fluidRow(
			shiny::column(
				width = 7,
				shiny::fileInput(
					ns("file.preps"), 
					label = "Upload a CSV File:", 
					multiple = FALSE
				)
			),
			shiny::column(
				width = 5,
				shiny::radioButtons(
					ns("sep.preps"), 
					"Separator",
					choices = c(Comma = ",",
								Semicolon = ";",
								Tab = "\t"),
					selected = ",")
				)
			),             
        ),
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

    shinyjs::useShinyjs()
    
    prep_inputs <- shiny::eventReactive(input$RUN.prep, {
      planter_mov <- input$planter_mov.preps
      expt_name <- as.character(input$expt_name.preps)
      plotNumber <- validate_design(read_whole_numbers(
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
      loc_user_view <- 1:prep_inputs()$sites
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
        shiny::req(input$file.preps)
        inFile <- input$file.preps
        data_ingested <- load_file(
           name = inFile$name,
           path = inFile[["datapath"]],
           sep = input$sep.preps, 
           check = TRUE, 
           design = "prep"
        )
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_up <- na.omit(data_up)
          data_preps <- as.data.frame(data_up)
          if (ncol(data_preps) < 3) {
            shinyalert::shinyalert(
              "Error!!", 
              "Data input needs at least three columns with: ENTRY, NAME and REPS.", 
              type = "error")
            return(NULL)
          } 
          data_preps <- as.data.frame(data_preps[,1:3])
          colnames(data_preps) <- c("ENTRY", "NAME", "REPS")
          if(!is.numeric(data_preps$REPS) || !is.integer(data_preps$REPS) ||
             is.factor(data_preps$REPS)) shiny::validate("'REPS' must be numeric.")
          total_plots <- sum(data_preps$REPS)
        } else {
          app_upload_error(data_ingested,
                           missing_columns = "Data input needs at least three columns with: ENTRY, NAME and REPS.")
          return(NULL)
        }
      } else {
        shiny::req(input$repGens.preps)
        shiny::req(input$repUnits.preps)
        repGens <- as.numeric(as.vector(unlist(strsplit(input$repGens.preps, ","))))
        repUnits <- as.numeric(as.vector(unlist(strsplit(input$repUnits.preps, ","))))
        if (length(repGens) != length(repUnits)) shiny::validate("Input repGens and repUnits must be of the same length.")
        ENTRY <- 1:sum(repGens)
        NAME <- paste(rep("G", sum(repGens)), 1:sum(repGens), sep = "")
        # REPS <- sort(rep(repUnits, times = repGens), decreasing = TRUE)
        REPS <- rep(repUnits, times = repGens)
        data_preps <- data.frame(
            ENTRY = ENTRY, 
            NAME = NAME, 
            REPS = REPS
        )
        colnames(data_preps) <- c("ENTRY", "NAME", "REPS")
        total_plots <- sum(data_preps$REPS)
      }
      return(list(data_up.preps = data_preps, total_plots = total_plots))
    })
    
    list_input_plots <- shiny::eventReactive(input$RUN.prep, {
      shiny::req(get_data_prep())
      if (input$owndataPREPS != 'Yes') {
        shiny::req(input$repGens.preps)
        shiny::req(input$repUnits.preps)
        repGens <- as.numeric(as.vector(unlist(strsplit(input$repGens.preps, ","))))
        repUnits <- as.numeric(as.vector(unlist(strsplit(input$repUnits.preps, ","))))
        n_plots <- sum(repGens * repUnits)
        return(list(n_plots = n_plots, input$owndataPREPS))
      } else {
        n_plots <- get_data_prep()$total_plots
        return(list(n_plots = n_plots, input$owndataPREPS))
      }
    })
    
    shiny::observeEvent(list(list_input_plots(), input$allow_fillers.preps), {
      shiny::req(get_data_prep())
      shiny::req(input$owndataPREPS)
      if (input$owndataPREPS != 'Yes') {
        repGens <- as.numeric(as.vector(unlist(strsplit(input$repGens.preps, ","))))
        repUnits <- as.numeric(as.vector(unlist(strsplit(input$repUnits.preps, ","))))
        n <- sum(repGens * repUnits)
        options <- prep_dimension_options(
          total_plots = n,
          allow_fillers = isTRUE(input$allow_fillers.preps),
          max_fillers = .prep_max_fillers
        )
      } else {
        shiny::req(get_data_prep()$total_plots)
        n <- get_data_prep()$total_plots
        options <- prep_dimension_options(
          total_plots = n,
          allow_fillers = isTRUE(input$allow_fillers.preps),
          max_fillers = .prep_max_fillers
        )
      }
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
        if (!isTRUE(input$allow_fillers.preps)) {
          shinyalert::shinyalert(
            "Filler plots required",
            sprintf(paste(
              "The current design does not fit any supported rectangular",
              "field dimensions without unused cells. Select 'Allow filler",
              "plots' to continue. FielDHub will then offer nearby valid",
              "dimensions requiring no more than %d filler plots and place",
              "the fillers at the end of the selected planter path."
            ), .prep_max_fillers),
            type = "info"
          )
        } else {
          shinyalert::shinyalert(
            "No dimensions within the filler limit",
            sprintf(paste(
              "FielDHub could not find supported rectangular field dimensions",
              "requiring %d or fewer filler plots. Adjust the number of entries",
              "or replication settings and try again."
            ), .prep_max_fillers),
            type = "warning"
          )
        }
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

    entryListFormat_pREP <- data.frame(
		ENTRY = 1:9, 
		NAME = c(paste("Genotype", LETTERS[1:9], sep = "")),
		REPS = as.factor(c(rep(2, times = 3), rep(1,6)))
	)

    entriesInfoModal_pREP <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(entryListFormat_pREP,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        easyClose = FALSE
      )
    }
    
    toListen <- shiny::reactive({
      list(input$owndataPREPS)
    })
    
    shiny::observeEvent(toListen(), {
      if (input$owndataPREPS == 'Yes'){
        shiny::showModal(
          entriesInfoModal_pREP()
        )
      }
    })

    shiny::observeEvent(input$RUN.prep, {
      shiny::req(get_data_prep())
      shinyjs::show(id = "dimensions.preps")
    })

    ###### Plotting the data ##############
    output$dataup.preps <- DT::renderDT({
      shiny::req(get_data_prep())
      test <- randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0
      if (!test) return(NULL)
      shiny::req(get_data_prep()$data_up.preps)
      data_entry.preps <- get_data_prep()$data_up.preps
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
      shiny::req(get_data_prep()$data_up.preps)
      gen.list <- get_data_prep()$data_up.preps
      nrows <- field_dimensions_prep()$d_row
      ncols <- field_dimensions_prep()$d_col
      niter <- 10000
      prep <- TRUE
  
      locs_preps <- prep_inputs()$sites
      site_names <- prep_inputs()$location_names
      preps.seed <- prep_inputs()$seed_number
      plotNumber <- prep_inputs()$plotNumber
      movement_planter <- prep_inputs()$planter_mov
      expt_name <- prep_inputs()$expt_name
      shiny::withProgress(message = 'Running p-rep optimization ...', {
          pREPS <- validate_design(partially_replicated(
            nrows = rep(nrows, locs_preps), 
            ncols = rep(ncols, locs_preps), 
            l = locs_preps, 
            seed = preps.seed, 
            plotNumber = plotNumber, 
            exptName =  expt_name,
            locationNames = site_names, 
            planter = movement_planter,
            border_penalization = 0.5, #input$border_penalization,
            dist_method = "euclidean", # input$optimization_distance_method,
            data = gen.list,
            allow_fillers = isTRUE(input$allow_fillers.preps)
          ))
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
      if (!test) return(NULL)
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
