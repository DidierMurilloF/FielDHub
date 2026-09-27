#' pREPS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pREPS_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("Single and Multi-Location P-rep Design"),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        radioButtons(
			inputId = ns("owndataPREPS"), 
			label = "Import entries' list?", 
			choices = c("Yes", "No"), 
			selected = "No",
			inline = TRUE, 
			width = NULL, 
			choiceNames = NULL, 
			choiceValues = NULL
		),
    conditionalPanel(
			condition = "input.owndataPREPS == 'Yes'", 
			ns = ns,
			fluidRow(
			column(
				width = 7,
				fileInput(
					ns("file.preps"), 
					label = "Upload a CSV File:", 
					multiple = FALSE
				)
			),
			column(
				width = 5,
				radioButtons(
					ns("sep.preps"), 
					"Separator",
					choices = c(Comma = ",",
								Semicolon = ";",
								Tab = "\t"),
					selected = ",")
				)
			),             
        ),
        conditionalPanel(
			condition = "input.owndataPREPS == 'No'", 
			ns = ns,
			textInput(
				ns("repGens.preps"), 
				label = "# of Entries Per Rep Group:", 
				value = "75,150"
			),
			textInput(
				inputId = ns("repUnits.preps"),
				label = "# of Rep Per Group:",
				value = "2,1")
			),
			checkboxInput(
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
			fluidRow(
				column(
					width = 6,
					numericInput(
						inputId = ns("l.preps"), 
						label = "Input # of Locations:", 
						value = 1, 
						min = 1)
				),
				column(
					width = 6,
					selectInput(
						inputId = ns("locView.preps"), 
						label = "Choose Location to View:", 
						choices = 1:1, 
						selected = 1,
						multiple = FALSE
					)
				)
			),
			selectInput(
				ns("planter_mov.preps"), 
				label = "Plot Order Layout:",
				choices = c("serpentine", "cartesian"), 
				multiple = FALSE,
				selected = "serpentine"
			),
			fluidRow(
				column(
					width = 6,
					textInput(
						ns("plot_start.preps"), 
						"Starting Plot Number:", 
						value = 1
					)
				),
				column(
					width = 6,
					textInput(
						ns("expt_name.preps"), 
						"Input Experiment Name:", 
						value = "Expt1"
					)
				)
			),  
			fluidRow(
				column(
					width = 6,
          numericInput(
						ns("seed.preps"), 
						label = "Random Seed:", 
						value = 4095, 
						min = 1
          )
				),
				column(
					width = 6,
					textInput(
						ns("Location.preps"), 
						"Input Location Name:", 
						value = "FARGO"
					)
				)
			),
			fluidRow(
				column(
					width = 6,
					actionButton(
						inputId = ns("RUN.prep"), 
						label = "Run!", 
						icon = icon("circle-nodes", verify_fa = FALSE),
						width = '100%'
					),
				),
				column(
					width = 6,
					actionButton(
						ns("Simulate.prep"), 
						label = "Simulate!", 
						icon = icon("greater-than-equal", verify_fa = FALSE),
						width = '100%'
					),
				)
			),
			br(),
			uiOutput(ns("download_prep"))
		),
		mainPanel(
			width = 8,
			shinyjs::useShinyjs(),
			tabsetPanel(
				id = ns("tabset_prep"),
				tabPanel("Get Random", value = "tabPanel_prep",
					br(),
					shinyjs::hidden(
						selectInput(inputId = ns("dimensions.preps"), 
									label = "Select dimensions of field:", 
									choices = "")
					),
					shinyjs::hidden(
					actionButton(ns("get_random_prep"), label = "Randomize!")
					),
					br(),
					br(),
					div(
					  fieldhub_spinner(
					    verbatimTextOutput(
					      outputId = ns("summary_prep"), 
					      placeholder = FALSE
					     ), 
					    type = 4
					  ),
					  style = "padding-right: 40px;"
					)
				),
				tabPanel("Data Input", DT::DTOutput(ns("dataup.preps"))),
				tabPanel("Randomized Field",
						fieldhub_spinner(
							DT::DTOutput(ns("dtpREPS")), 
							type = 4)
						),
				tabPanel("Plot Number Field", DT::DTOutput(ns("PREPSPLOTFIELD"))),
				tabPanel("Field Book", DT::DTOutput(ns("pREPSOUTPUT"))),
				tabPanel("Heatmap", plotly::plotlyOutput(ns("heatmap_prep"), width = "97%"))
			)
		)
    )
  )
}
#' pREPS Server Functions
#'
#' @noRd 
mod_pREPS_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shinyjs::useShinyjs()
    
    prep_inputs <- eventReactive(input$RUN.prep, {
      planter_mov <- input$planter_mov.preps
      expt_name <- as.character(input$expt_name.preps)
      plotNumber <- validate_design(read_whole_numbers(
        input$plot_start.preps, "Starting Plot Number"
      ))
      site_names <- as.character(as.vector(unlist(strsplit(input$Location.preps, ","))))
      seed_number <- as.numeric(input$seed.preps)
      sites = as.numeric(input$l.preps)
      return(list(sites = sites, 
                  location_names = site_names, 
                  seed_number = seed_number, 
                  plotNumber = plotNumber,
                  planter_mov = planter_mov,
                  expt_name = expt_name)) 
    })

    observeEvent(prep_inputs()$sites, {
      loc_user_view <- 1:prep_inputs()$sites
      updateSelectInput(inputId = "locView.preps", 
                        choices = loc_user_view, 
                        selected = loc_user_view[1])
    })

    observeEvent(input$owndataPREPS,
                 handlerExpr = updateTabsetPanel(session,
                                                 "tabset_prep",
                                                 selected = "tabPanel_prep"))
    observeEvent(input$RUN.prep,
                 handlerExpr = updateTabsetPanel(session,
                                                 "tabset_prep",
                                                 selected = "tabPanel_prep"))
    
    get_data_prep <- eventReactive(input$RUN.prep, {
      if (input$owndataPREPS == 'Yes') {
        req(input$file.preps)
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
             is.factor(data_preps$REPS)) validate("'REPS' must be numeric.")
          total_plots <- sum(data_preps$REPS)
        } else {
          app_upload_error(data_ingested,
                           missing_columns = "Data input needs at least three columns with: ENTRY, NAME and REPS.")
          return(NULL)
        }
      } else {
        req(input$repGens.preps)
        req(input$repUnits.preps)
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
    
    list_input_plots <- eventReactive(input$RUN.prep, {
      req(get_data_prep())
      if (input$owndataPREPS != 'Yes') {
        req(input$repGens.preps)
        req(input$repUnits.preps)
        repGens <- as.numeric(as.vector(unlist(strsplit(input$repGens.preps, ","))))
        repUnits <- as.numeric(as.vector(unlist(strsplit(input$repUnits.preps, ","))))
        n_plots <- sum(repGens * repUnits)
        return(list(n_plots = n_plots, input$owndataPREPS))
      } else {
        n_plots <- get_data_prep()$total_plots
        return(list(n_plots = n_plots, input$owndataPREPS))
      }
    })
    
    observeEvent(list(list_input_plots(), input$allow_fillers.preps), {
      req(get_data_prep())
      req(input$owndataPREPS)
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
        req(get_data_prep()$total_plots)
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
      updateSelectInput(inputId = "dimensions.preps",
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
    
    field_dimensions_prep <- eventReactive(input$get_random_prep, {
      req(get_data_prep())
      if (input$dimensions.preps == "No options available") return(NULL)
      dims <- unlist(strsplit(input$dimensions.preps," x "))
      d_row <- as.numeric(dims[1])
      d_col <- as.numeric(dims[2])
      return(list(d_row = d_row, d_col = d_col))
    })

    randomize_hit_prep <- reactiveValues(times = 0)
 
    observeEvent(input$RUN.prep, {
      randomize_hit_prep$times <- 0
    })

    user_tries_prep <- reactiveValues(tries_prep = 0)

    observeEvent(input$get_random_prep, {
      user_tries_prep$tries_prep <- user_tries_prep$tries_prep + 1
      randomize_hit_prep$times <- randomize_hit_prep$times + 1
    })

    observeEvent(input$dimensions.preps, {
      user_tries_prep$tries_prep <- 0
    })

    list_to_observe_prep <- reactive({
      list(randomize_hit_prep$times, user_tries_prep$tries_prep)
    })

    observeEvent(list_to_observe_prep(), {
      output$download_prep <- renderUI({
        if (randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0) {
          downloadButton(ns("downloadData.preps"),
                          "Save Experiment",
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
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        renderTable(entryListFormat_pREP,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        easyClose = FALSE
      )
    }
    
    toListen <- reactive({
      list(input$owndataPREPS)
    })
    
    observeEvent(toListen(), {
      if (input$owndataPREPS == 'Yes'){
        showModal(
          entriesInfoModal_pREP()
        )
      }
    })

    observeEvent(input$RUN.prep, {
      req(get_data_prep())
      shinyjs::show(id = "dimensions.preps")
    })

    ###### Plotting the data ##############
    output$dataup.preps <- DT::renderDT({
      req(get_data_prep())
      test <- randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0
      if (!test) return(NULL)
      req(get_data_prep()$data_up.preps)
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
    
    pREPS_reactive <- reactive({
      req(get_data_prep())
      req(get_data_prep()$data_up.preps)
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
      withProgress(message = 'Running p-rep optimization ...', {
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
      bindEvent(input$get_random_prep)

    output$summary_prep <- renderPrint({
      req(get_data_prep())
      test <- randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0
      if (test) {
        cat("Randomization was successful!", "\n", "\n")
        print(pREPS_reactive(), n = 6)
      }
    })
    
     user_site_selection <- reactive({
       return(as.numeric(input$locView.preps))
     })

    
    output$BINARYpREPS <- DT::renderDT({
      if (user_tries_prep$tries_prep < 1) return(NULL)
      req(pREPS_reactive())
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
      req(pREPS_reactive())
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
                     buttons = c('copy', 'excel'),
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
      req(pREPS_reactive())
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
                                   buttons = c('copy', 'excel'),
                                   lengthMenu = list(c(10,25,50,-1),
                                                     c(10,25,50,"All"))))
                    
                    )
    })

    valsPREP <- reactiveValues(ROX = NULL, ROY = NULL, trail.prep = NULL, minValue = NULL,
                                maxValue = NULL)
    
    simuModal.PREP <- function(failed = FALSE) {
      modalDialog(
        fluidRow(
          column(6, 
                 selectInput(inputId = ns("trailsPREP"), label = "Select One:", 
                             choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
          ),
          column(6, 
                 checkboxInput(inputId = ns("heatmap_PREP"), label = "Include a Heatmap", value = TRUE),
          )
        ),
        conditionalPanel("input.trailsPREP == 'Other'", ns = ns,
                         textInput(inputId = ns("OtherPREP"), label = "Input Trial Name:", value = NULL)
        ),
        fluidRow(
          column(6, 
                 selectInput(inputId = ns("ROX.PREP"), "Select the Correlation in Rows:", 
                             choices = seq(0.1, 0.9, 0.1),  selected = 0.5)
          ),
          column(6, 
                 selectInput(inputId = ns("ROY.PREP"), "Select the Correlation in Cols:", 
                             choices = seq(0.1, 0.9, 0.1),  selected = 0.5)
          )
        ),
        fluidRow(
          column(6, 
                 numericInput(inputId = ns("min.prep"), "Input the min value", value = NULL)
          ),
          column(6, 
                 numericInput(inputId = ns("max.prep"), "Input the max value", value = NULL)
                 
          )
        ),
        if (failed)
          div(tags$b("Invalid input of data max and min", style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(inputId = ns("ok.prep"), "GO")
        )
      )
    }
    
    observeEvent(input$Simulate.prep, {
      req(pREPS_reactive()$fieldBook)
      test <- randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0
      if (test) {
        showModal(
          simuModal.PREP()
        )
      }
    })
    
    observeEvent(input$ok.prep, {
      req(input$min.prep, input$max.prep)
      if (input$max.prep > input$min.prep & input$min.prep != input$max.prep) {
        valsPREP$maxValue <- input$max.prep
        valsPREP$minValue  <- input$min.prep
        valsPREP$ROX <- as.numeric(input$ROX.PREP)
        valsPREP$ROY <- as.numeric(input$ROY.PREP)
        if(input$trailsPREP == "Other") {
          req(input$OtherPREP)
          if(!is.null(input$OtherPREP)) {
            valsPREP$trail.prep <- as.character(input$OtherPREP)
          }else showModal(simuModal.PREP(failed = TRUE))
        }else {
          valsPREP$trail.prep <- as.character(input$trailsPREP)
        }
        removeModal()
      }else {
        showModal(
          simuModal.PREP(failed = TRUE)
        )
      }
    })
    
    simuDataPREP <- reactive({
      req(pREPS_reactive()$fieldBook)
      req(prep_inputs())
      field_book <- pREPS_reactive()$fieldBook
      if (is.null(valsPREP$maxValue) || is.null(valsPREP$minValue) ||
          is.null(valsPREP$trail.prep)) {
        return(list(df = field_book))
      }
      simulation <- validate_design(simulate_spatial_field_book(
        field_book = as.data.frame(field_book),
        nrows = field_dimensions_prep()$d_row, ncols = field_dimensions_prep()$d_col,
        correlation_x = as.numeric(valsPREP$ROX),
        correlation_y = as.numeric(valsPREP$ROY),
        min_value = as.numeric(valsPREP$minValue),
        max_value = as.numeric(valsPREP$maxValue),
        response_name = as.character(valsPREP$trail.prep),
        seed = as.numeric(prep_inputs()$seed_number)
      ))
      list(df = simulation$field_book, dfSimulationList = simulation$simulations)
    })

    heat_map_prep <- reactiveValues(heat_map_option = FALSE)
    
    observeEvent(input$ok.prep, {
      req(input$min.prep, input$max.prep)
      if (input$max.prep > input$min.prep & input$min.prep != input$max.prep) {
        heat_map_prep$heat_map_option <- TRUE
      }
    })
    
    observeEvent(heat_map_prep$heat_map_option, {
      if (heat_map_prep$heat_map_option == FALSE) {
        hideTab(inputId = "tabset_prep", target = "Heatmap")
      } else {
        showTab(inputId = "tabset_prep", target = "Heatmap")
      }
    })
    
    heatmap_obj <- reactive({
      req(simuDataPREP()$dfSimulationList)
      loc_user <- user_site_selection()
      if(input$heatmap_PREP) {
        w <- as.character(valsPREP$trail.prep)
        df <- simuDataPREP()$dfSimulationList[[loc_user]]
        df <- as.data.frame(df)
        p1 <- ggplot2::ggplot(df, ggplot2::aes(x = df[,4], y = df[,3], fill = df[,7], text = df[,8])) + 
          ggplot2::geom_tile() +
          ggplot2::xlab("COLUMN") +
          ggplot2::ylab("ROW") +
          ggplot2::labs(fill = w) +
          fieldhub_viridis_scale()
        
        p2 <- plotly::ggplotly(p1, tooltip="text", height = 700)
        return(p2)
      }
    }) 
    
    output$heatmap_prep <- plotly::renderPlotly({
      test <- randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0
      if (!test) return(NULL)
      req(heatmap_obj())
      heatmap_obj()
    }) 
    
    
    output$pREPSOUTPUT <- DT::renderDT({
      test <- randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0
      if (!test) return(NULL)
      df <- simuDataPREP()$df
      df$EXPT <- as.factor(df$EXPT)
      df$LOCATION <- as.factor(df$LOCATION)
      df$PLOT <- as.factor(df$PLOT)
      df$ROW <- as.factor(df$ROW)
      df$COLUMN <- as.factor(df$COLUMN)
      df$CHECKS <- as.factor(df$CHECKS)
      df$ENTRY <- as.factor(df$ENTRY)
      df$TREATMENT <- as.factor(df$TREATMENT)
      table_options <- list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px")
      DT::datatable(df, 
                    filter = "top",
                    rownames = FALSE, 
                    options = utils::modifyList(table_options, list(
                      columnDefs = list(list(className = 'dt-center', targets = "_all"))))
      )
    })
    
    output$downloadData.preps <- downloadHandler(
      filename = function() {
        req(input$Location.preps)
        loc <- input$Location.preps
        loc <- paste(loc, "_", "pREP_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(simuDataPREP()$df, file, row.names = FALSE)
      }
    )
 
  })
}
