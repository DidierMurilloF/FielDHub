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
					shinycssloaders::withSpinner(
					verbatimTextOutput(outputId = ns("summary_prep"), 
										placeholder = FALSE), 
					type = 4
					)
				),
				tabPanel("Data Input", DT::DTOutput(ns("dataup.preps"))),
				tabPanel("Randomized Field",
						shinycssloaders::withSpinner(
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
      plotNumber <- as.numeric(as.vector(unlist(strsplit(input$plot_start.preps, ","))))
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

    observeEvent(input$input.owndataPREPS,
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
           path = inFile$datapat,
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
        } else if (names(data_ingested) == "bad_format") {
          shinyalert::shinyalert(
            "Error!!", 
            "Invalid file; Please upload a .csv file.", 
            type = "error")
          return(NULL)
        } else if (names(data_ingested) == "duplicated_vals") {
          shinyalert::shinyalert(
            "Error!!", 
            "Check input file for duplicate values.", 
            type = "error")
          return(NULL)
        } else if (names(data_ingested) == "missing_cols") {
          shinyalert::shinyalert(
            "Error!!", 
            "Data input needs at least three columns with: ENTRY, NAME and REPS.",
            type = "error")
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
      prime_factors <- numbers::primeFactors(total_plots)
      if (length(prime_factors) == 2) {
        if (prime_factors[1] < 4 & numbers::isPrime(prime_factors[2])) {
          shinyalert::shinyalert(
            "Error!!",
            "There are no options available for field dimensions. Please try a different number of treatments.",
            type = "error"
          )
          return(NULL)
        }
      }
      if (numbers::isPrime(total_plots)) {
        shinyalert::shinyalert(
          "Error!!",
          "The number of field plots results in a prime number. Please try a different number of treatments.",
          type = "error"
        )
        return(NULL)
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
    
    observeEvent(list_input_plots(), {
      req(get_data_prep())
      req(input$owndataPREPS)
      if (input$owndataPREPS != 'Yes') {
        repGens <- as.numeric(as.vector(unlist(strsplit(input$repGens.preps, ","))))
        repUnits <- as.numeric(as.vector(unlist(strsplit(input$repUnits.preps, ","))))
        n <- sum(repGens * repUnits)
        choices <- factor_subsets(n)$labels
      } else {
        req(get_data_prep()$total_plots)
        n <- get_data_prep()$total_plots
        choices <- factor_subsets(n)$labels
      }
      if(is.null(choices)){
        choices <- "No options available"
      }

      if (!is.null(choices)) {
            dif <- vector(mode = "numeric", length = length(choices))
            for (option in 1:length(choices)) {
                dims <- unlist(strsplit(choices[[option]], " x "))
                dif[option] <- abs(as.numeric(dims[1]) - as.numeric(dims[2]))
            }
            df_choices <- data.frame(choices = unlist(choices), diff_dim = dif)
            df_choices <- df_choices[order(df_choices$diff_dim, decreasing = FALSE), ]
            choices <- as.vector(df_choices$choices)
      }
      updateSelectInput(inputId = "dimensions.preps",
                        choices = choices,
                        selected = choices[1])
    })
    
    field_dimensions_prep <- eventReactive(input$get_random_prep, {
      req(get_data_prep())
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
      shinyjs::show(id = "get_random_prep")
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
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      DT::datatable(df,
                    rownames = FALSE, 
                    filter = 'top',
                    options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
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
          pREPS <- partially_replicated(
            nrows = rep(nrows, locs_preps), 
            ncols = rep(ncols, locs_preps), 
            l = locs_preps, 
            seed = preps.seed, 
            plotNumber = plotNumber, 
            exptName =  expt_name,
            locationNames = site_names, 
            planter = movement_planter,
            data = gen.list 
          )
      })
    }) |> 
      bindEvent(input$get_random_prep)

    output$summary_prep <- renderPrint({
      req(get_data_prep())
      test <- randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0
      if (test) {
        cat("Randomization was successful!", "\n", "\n")
        print(pREPS_reactive())
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
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "700px"))
      DT::datatable(df,
                    extensions = 'FixedColumns',
                    options = list(
                      dom = 't',
                      scrollX = TRUE,
                      fixedColumns = TRUE
                    )) |>
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                        backgroundColor = DT::styleEqual(1, "gray"))
    })
    
    
    output$dtpREPS <- DT::renderDataTable({
      test <- randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0
      if (!test) return(NULL)
      req(pREPS_reactive())
      selection <- as.numeric(user_site_selection())
      w_map <- pREPS_reactive()$layoutRandom[[selection]]
      checks = as.vector(pREPS_reactive()$genEntries[[1]])
      len_checks <- length(checks)
      colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                   'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
      
      df <- as.data.frame(w_map)
      
      gens <- as.vector(unlist(pREPS_reactive()$genEntries[[2]]))
      
      rownames(df) <- nrow(df):1
      colnames(df) <- paste0('V', 1:ncol(df))
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, 
                                scrollY = "700px"))
      DT::datatable(df,
                    extensions = 'Buttons', 
                     options = list(dom = 'Blfrtip',
                     scrollX = TRUE,
                     fixedColumns = TRUE,
                     pageLength = nrow(df),
                     scrollY = "700px",
                     class = 'compact cell-border stripe',  rownames = FALSE,
                     server = FALSE,
                     filter = list( position = 'top', clear = FALSE, plain =TRUE ),
                     buttons = c('copy', 'excel'),
                     lengthMenu = list(c(10,25,50,-1),
                                       c(10,25,50,"All")))) |>
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
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "700px"))
      DT::datatable(df,
                    extensions = 'Buttons', 
                    options = list(dom = 'Blfrtip',
                                   scrollX = TRUE,
                                   fixedColumns = TRUE,
                                   pageLength = nrow(df),
                                   scrollY = "700px",
                                   class = 'compact cell-border stripe',  rownames = FALSE,
                                   server = FALSE,
                                   filter = list( position = 'top', clear = FALSE, plain =TRUE ),
                                   buttons = c('copy', 'excel'),
                                   lengthMenu = list(c(10,25,50,-1),
                                                     c(10,25,50,"All")))
                    
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
      if(!is.null(valsPREP$maxValue) & !is.null(valsPREP$minValue) & !is.null(valsPREP$trail.prep)) {
        maxVal <- as.numeric(valsPREP$maxValue)
        minVal <- as.numeric(valsPREP$minValue)
        ROX_PREP <- as.numeric(valsPREP$ROX)
        ROY_PREP <- as.numeric(valsPREP$ROY)
        df.prep <- pREPS_reactive()$fieldBook
        loc_levels_factors <- levels(factor(df.prep$LOCATION, unique(df.prep$LOCATION)))
        locs <- prep_inputs()$sites
        nrows_prep <- field_dimensions_prep()$d_row
        ncols_prep <- field_dimensions_prep()$d_col
        seed_prep <- prep_inputs()$seed_number
        df.prep_list <- vector(mode = "list", length = locs)
        dfSimulationList <- vector(mode = "list", length = locs)
        w <- 1
        set.seed(seed_prep)
        for (sites in 1:locs) {
            df_loc <- subset(df.prep, LOCATION == loc_levels_factors[w])
            fieldBook <- df_loc[, c(1,6,7,9)]
            dfSimulation <- AR1xAR1_simulation(
                nrows = nrows_prep, 
                ncols = ncols_prep, 
                ROX = ROX_PREP, 
                ROY = ROY_PREP, 
                minValue = minVal, 
                maxValue = maxVal, 
                fieldbook = fieldBook, 
                trail = valsPREP$trail.prep, 
                seed = NULL
            )
          
          dfSimulation <- dfSimulation$outOrder
          dfSimulationList[[sites]] <- dfSimulation
          dataPrep <- df_loc
          df_prep <- cbind(dataPrep, round(dfSimulation[,7],2))
          colnames(df_prep)[11] <- as.character(valsPREP$trail.prep)
          df.prep_list[[sites]] <- df_prep
          w <- w + 1
        }
        df.prep_locs <- dplyr::bind_rows(df.prep_list)
        v <- 1
      }else {
        dataPrep <- pREPS_reactive()$fieldBook
        v <- 2
      }
      if (v == 1) {
        return(list(df = df.prep_locs, dfSimulationList = dfSimulationList))
      }else if (v == 2) {
        return(list(df = dataPrep))
      }
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
          viridis::scale_fill_viridis(discrete = FALSE)
        
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
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      DT::datatable(df, 
                    filter = "top",
                    rownames = FALSE, 
                    options = list(
                      columnDefs = list(list(className = 'dt-center', targets = "_all")))
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
