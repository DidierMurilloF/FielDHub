#' multi_loc_preps UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_multi_loc_preps_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("Optimized Multi-Location P-rep Design"),
    sidebarLayout(
        sidebarPanel(
            width = 4,
            radioButtons(
                inputId = ns("multi_prep_data"), 
                label = "Import entries' list?", 
                choices = c("Yes", "No"), 
                selected = "No",
                inline = TRUE, 
                width = NULL, 
                choiceNames = NULL, 
                choiceValues = NULL
            ),
            conditionalPanel(
                condition = "input.multi_prep_data == 'Yes'", 
                ns = ns,
                fluidRow(
                column(
                    width = 7, # style=list("padding-right: 28px;"),
                    fileInput(
                        ns("file_multi_prep"), 
                        label = "Upload a CSV File:", 
                        multiple = FALSE
                    )
                ),
                column(
                    width = 5, #style=list("padding-left: 5px;"),
                    radioButtons(
                        ns("sep_multi_prep"), "Separator",
                        choices = c(Comma = ",",
                                    Semicolon = ";",
                                    Tab = "\t"),
                        selected = ",")
                    )
                )             
            ),
            numericInput(
                inputId = ns("gens_prep"), 
                label = "Input # of Entries:",
                value = 312,
                min = 1
            ),
            radioButtons(
                inputId = ns("include_checks"), 
                label = "Include checks?", 
                choices = c("Yes", "No"), 
                selected = "No",
                inline = TRUE, 
                width = NULL, 
                choiceNames = NULL, 
                choiceValues = NULL
            ),
            conditionalPanel(
                condition = "input.include_checks  == 'Yes'", 
                ns = ns,
                fluidRow(
                    column(
                        width = 6,
                        numericInput(
                            inputId = ns("prep_checks_met"),
                            label = "Input # of Checks:",
                            min = 1,
                            max = 10,
                            value = 3
                        )
                    ),
                    column(
                        width = 6,
                        textInput(
                            inputId = ns("prep_checks"), 
                            label = "Input # Check's Reps:", 
                            value = "8,8,8"
                        )
                    )
                )
            ),
            fluidRow(
                column(
                    width = 6, 
                    numericInput(
                        inputId = ns("locs_prep"), 
                        label = "Input # of Locations:", 
                        value = 6, 
                        min = 2
                    )
                ),
                column(
                    width = 6,
                    selectInput(
                        inputId = ns("loc_to_view_preps"), 
                        label = "Choose Location to View:", 
                        choices = 1:1, 
                        selected = 1,
                        multiple = FALSE
                    )
                )
            ),
            selectInput(
                inputId = ns("plant_copies_preps"), 
                label = "# of Copies Per Entry:",
                choices = 1:6
            ),
            selectInput(
                ns("planter_preps"), 
                label = "Plot Order Layout:",
                choices = c("serpentine", "cartesian"), 
                multiple = FALSE,
                selected = "serpentine"
            ),
            fluidRow(
                column(
                    width = 6,
                    textInput(
                        ns("plot_start_preps"), 
                        "Starting Plot Number:", 
                        value = 1
                    )
                ),
                column(
                    width = 6, 
                    textInput(
                        ns("expt_name_preps"), 
                        "Input Experiment Name:", 
                        value = "Expt1"
                    )
                )
            ),  
            fluidRow(
                column(
                    width = 6,
                    numericInput(
                        ns("seed_preps"), 
                        label = "Random Seed:", 
                        value = 1, 
                        min = 1
                    ) 
                ),
                column(
                    width = 6, 
                    textInput(
                        ns("loc_name_preps"), 
                        "Input Location Name:", 
                        value = "FARGO"
                    )
                )
            ),
            fluidRow(
                column(
                    width = 6,
                    actionButton(
                        inputId = ns("run_prep"), 
                        label = "Run!", 
                        icon = icon("circle-nodes", verify_fa = FALSE),
                        width = '100%'
                    )
                ),
                column(
                    width = 6,
                    actionButton(
                        ns("simulate_prep_data"), 
                        label = "Simulate!", 
                        icon = icon("greater-than-equal", verify_fa = FALSE),
                        width = '100%'
                    )
                )
            ),
            br(),
            uiOutput(ns("download_prep_avg"))
        ),
        mainPanel(
            width = 8,
            shinyjs::useShinyjs(),
            tabsetPanel(id = ns("tabset_prep_avg"),
            tabPanel("Get Random", value = "tabPanel_prep_avg",
                br(),
                fluidRow(
                  column(
                    width = 4,
                    shinyjs::hidden(
                        selectInput(
                          inputId = ns("dimensions_preps"), 
                          label = "Select dimensions of field:", 
                          choices = "")
                    ),
                    shinyjs::hidden(
                        actionButton(
                          inputId = ns("multi_dimension_button"),
                          label = "Select multiple dimensions",
                          width = "80%",
                          style = "margin-top:25px;
                                  margin-bottom:15px")
                    )
                  ),
                  column(
                    4,
                    br(),
                    shinyjs::hidden(
                      checkboxInput(inputId = ns("multi_dimension_toggle"),
                                    label = "Set different dimensions across locations",
                                    value = FALSE)
                    )
                  )
                ),
                fluidRow(
                  column(
                    4,
                    shinyjs::hidden(
                        actionButton(
                          ns("get_random_prep"), 
                          label = "Randomize!",
                          style = "margin-bottom:12px")
                    )
                  )
                ),
                shinycssloaders::withSpinner(
                    DT::DTOutput(ns("prep_allocation")),
                    type = 4
                )
            ),
            tabPanel("Data Input", DT::DTOutput(ns("multi_prep_data_input"))),
            tabPanel("Randomized Field",
                    shinycssloaders::withSpinner(
                        DT::DTOutput(ns("avg_field_preps")), 
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
#' multi_loc_preps Server Functions
#'
#' @noRd 
mod_multi_loc_preps_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shinyjs::useShinyjs()

    observe({
        req(input$locs_prep)
        prep_locs <- as.numeric(input$locs_prep)
        start <- prep_locs + 1
        plant_reps <- start:(prep_locs * 2 - 1)
        updateSelectInput(inputId = "plant_copies_preps", 
                            choices = plant_reps, 
                            selected = plant_reps[2])
    })

    prep_inputs <- eventReactive(input$run_prep, {
        req(input$gens_prep)
        if (input$include_checks == "Yes"){
            prep_checks <- as.numeric(as.vector(unlist(strsplit(input$prep_checks, ","))))
            checks <- as.numeric(input$prep_checks_met)
            if (length(prep_checks) != checks) {
                shinyalert::shinyalert(
                    "Error!!", 
                    "Length does not match with the input for number of checks!", 
                    type = "error"
                )
                return(NULL)
            }
        } else {
            prep_checks <- NULL
            checks <- NULL
        }
        input_lines <- as.numeric(input$gens_prep)
        planter_mov <- input$planter_preps
        expt_name <- as.vector(unlist(strsplit(input$expt_name_preps, ",")))
        plotNumber <- as.numeric(as.vector(unlist(strsplit(input$plot_start_preps, ","))))
        site_names <- as.character(as.vector(unlist(strsplit(input$loc_name_preps, ","))))
        seed_number <- as.numeric(input$seed_preps)
        sites = as.numeric(input$locs_prep)
        if (length(site_names) == 0 || length(site_names) != sites) {
            site_names <- paste0("LOC", 1:sites)
        }
        if (length(plotNumber) == 0 || length(plotNumber) != sites) {
            plotNumber <- seq(1, 1000 * sites, by = 1000)[1:sites]
        }
        if (length(expt_name) == 0) {
            expt_name <- "expt_prep"
        }
        return(
            list(
                prep_lines = input_lines,
                sites = sites, 
                location_names = site_names, 
                seed_number = seed_number, 
                plotNumber = plotNumber,
                planter_mov = planter_mov,
                expt_name = expt_name,
                checks = checks,
                prep_checks = prep_checks
            )
        ) 
    })

    observeEvent(prep_inputs()$sites, {
        loc_user_view <- 1:prep_inputs()$sites
        updateSelectInput(
            inputId = "loc_to_view_preps", 
            choices = loc_user_view, 
            selected = loc_user_view[1])
    })

    observeEvent(input$input.input_prep_data,
                 handlerExpr = updateTabsetPanel(session,
                                                 "tabset_prep_avg",
                                                 selected = "tabPanel_prep_avg"))

    observeEvent(input$run_prep,
                 handlerExpr = updateTabsetPanel(session,
                                                 "tabset_prep_avg",
                                                 selected = "tabPanel_prep_avg"))
    
    get_multi_loc_prep <- reactive({
        req(input$locs_prep)
        if (input$locs_prep < 2) {
            shinyalert::shinyalert(
                "Error!!", 
                "The system requires at least 2 locations to proceed.",
                type = "error"
            )
            return(NULL)
        }
        if (input$multi_prep_data == 'Yes') {
            req(input$file_multi_prep)
            inFile <- input$file_multi_prep
            data_ingested <- load_file(
                name = inFile$name,
                path = inFile$datapat,
                sep = input$sep_multi_prep, 
                check = TRUE, 
                design = "sdiag"
            )
            if (names(data_ingested) == "dataUp") {
                data_up <- data_ingested$dataUp
                data_preps <- as.data.frame(data_up)
                if (ncol(data_preps) < 2) {
                    shinyalert::shinyalert(
                    "Error!!", 
                    "Data input needs at least three columns with: ENTRY, NAME and REPS.", 
                    type = "error")
                    return(NULL)
                } 
                data_preps <- na.omit(data_preps[,1:2])
                colnames(data_preps) <- c("ENTRY", "NAME")
                if (!is.numeric(data_preps$ENTRY)) {
                    shinyalert::shinyalert(
                        "Error!!", 
                        "Column ENTRY should be numeric (integer numbers).", 
                        type = "error"
                    )
                    return(NULL)
                }
                if (input$include_checks == "Yes") {
                    prep_checks <- as.numeric(as.vector(unlist(strsplit(input$prep_checks, ","))))
                    checks <- as.numeric(input$prep_checks_met)
                    if (length(prep_checks) != checks) {
                        shinyalert::shinyalert(
                            "Error!!", 
                            "Length of check's reps does not match with the input for number of checks!", 
                            type = "error"
                        )
                        return(NULL)
                    }
                    entries_in_file <- nrow(data_preps[(length(prep_checks) + 1):nrow(data_preps), ])
                    input_lines <- as.numeric(input$gens_prep)
                    data_without_checks <- data_preps[(length(prep_checks) + 1):nrow(data_preps), ]
                    if (entries_in_file != input_lines) {
                        shinyalert::shinyalert(
                            "Error!!", 
                            "Number of entries in file does not match with the input value.", 
                            type = "error"
                        )
                        return(NULL)
                    }
                } else {
                    entries_in_file <- nrow(data_preps)
                    input_lines <- as.numeric(input$gens_prep)
                    data_without_checks <- data_preps
                    if (entries_in_file != input_lines) {
                        shinyalert::shinyalert(
                            "Error!!", 
                            "Number of entries in file does not match with the input value.", 
                            type = "error"
                        )
                        return(NULL)
                    }
                }
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
            req(input$prep_checks_met)
            req(input$prep_checks)
            if (input$include_checks == "Yes") {
                prep_checks <- as.numeric(as.vector(unlist(strsplit(input$prep_checks, ","))))
                checks <- as.numeric(input$prep_checks_met)
                if (length(prep_checks) != checks) {
                    shinyalert::shinyalert(
                        "Error!!", 
                        "Length of check's reps does not match with the input for number of checks!", 
                        type = "error"
                    )
                    return(NULL)
                }
                input_lines <- as.numeric(input$gens_prep)
                max_entry <- input_lines
                df_checks <- data.frame(
                    ENTRY = (max_entry + 1):((max_entry + checks)), 
                    NAME = paste0("CH-", (max_entry + 1):((max_entry + checks)))
                )
                NAME <- c(paste(rep("Gen-", input_lines), 1:input_lines, sep = ""))
                data_without_checks <- data.frame(list(ENTRY = 1:input_lines, NAME = NAME))
                input_entries <- as.numeric(data_without_checks$ENTRY)
                data_preps <- dplyr::bind_rows(df_checks, data_without_checks)
                colnames(data_preps) <- c("ENTRY", "NAME")
            } else {
                input_lines <- as.numeric(input$gens_prep)
                NAME <- c(paste(rep("Gen-", input_lines), 1:input_lines, sep = ""))
                data_preps <- data.frame(list(ENTRY = 1:input_lines, NAME = NAME))
                input_entries <- as.numeric(data_preps$ENTRY)
                colnames(data_preps) <- c("ENTRY", "NAME")
                data_without_checks <- data_preps
            }
        }
        return(
            list(
                multi_loc_preps_data = data_preps, 
                data_without_checks = data_without_checks
                )
            )
    }) %>%
        bindEvent(input$run_prep)

    setup_optim_prep <- reactive({
        req(get_multi_loc_prep())
        if (is.null(get_multi_loc_prep())) return(NULL)
        req(prep_inputs())
        input_lines <- as.numeric(input$gens_prep)
        locs <- as.numeric(input$locs_prep)
        checks <- as.numeric(prep_inputs()$checks)
        prep_checks <- prep_inputs()$prep_checks
        prep_data_input <- get_multi_loc_prep()$multi_loc_preps_data
        add_checks <- FALSE
        if (input$include_checks == "Yes") add_checks <- TRUE
        withProgress(message = 'Optimization in progress ...', {
            optim_out <- do_optim(
                design = "prep",
                lines = input$gens_prep, 
                l = locs, 
                copies_per_entry = as.numeric(input$plant_copies_preps), 
                add_checks = add_checks,
                checks = checks, 
                rep_checks = prep_inputs()$prep_checks,
                seed = prep_inputs()$seed_number,
                data = prep_data_input
            )
        })
        ### Do the merge with the user data ###
        if (input$multi_prep_data == "Yes") {
            data_prep_no_checks <- get_multi_loc_prep()$data_without_checks
            optim_out <- merge_user_data(
                optim_out = optim_out, 
                data = prep_data_input, 
                lines = input_lines, 
                add_checks = add_checks, 
                checks = checks, 
                rep_checks = prep_inputs()$prep_checks
            )
        }
        plots_for_treatments <- as.numeric(optim_out$size_locations[1])
        prep_checks <- as.numeric(prep_inputs()$prep_checks)
        if (!is.null(prep_inputs()$prep_checks)) {
          prep_checks <- as.numeric(prep_inputs()$prep_checks)
        } else {
          prep_checks <- 0
        }
        total_plots <- plots_for_treatments + sum(prep_checks)
        prime_factors <- numbers::primeFactors(total_plots)
        if (length(prime_factors) == 2) {
          if (prime_factors[1] < 4 & numbers::isPrime(prime_factors[2])) {
            shinyalert::shinyalert(
              "Error!!",
              "There are no options available for field dimensions. Please try a different number of treatments or checks.",
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
        choices <- factor_subsets(total_plots)$labels
        if (length(choices) == 0) {
          shinyalert::shinyalert(
            "Error!!",
            "Number of entries is too small!",
            type = "error"
          )
          return(NULL)
        } else return(optim_out)
    }) %>%
        bindEvent(input$run_prep)

    list_input_plots <- eventReactive(input$run_prep, {
        req(setup_optim_prep())
        req(get_multi_loc_prep())
        req(prep_inputs())
        if (!is.null(prep_inputs()$prep_checks)) {
            prep_checks <- as.numeric(prep_inputs()$prep_checks)
        } else {
            prep_checks <- 0
        }
        prep_checks <- as.numeric(prep_inputs()$prep_checks)
        plots_for_treatments <- as.numeric(setup_optim_prep()$size_locations[1])
        total_plots <- plots_for_treatments + sum(prep_checks)
        return(
            list(total_plots = total_plots)
        )
    })
    
    observeEvent(list_input_plots(), {
        req(setup_optim_prep())
        req(prep_inputs())
        plots_for_treatments <- as.numeric(setup_optim_prep()$size_locations[1])
        prep_checks <- as.numeric(prep_inputs()$prep_checks)
        if (!is.null(prep_inputs()$prep_checks)) {
            prep_checks <- as.numeric(prep_inputs()$prep_checks)
        } else {
            prep_checks <- 0
        }
        total_plots <- plots_for_treatments + sum(prep_checks)
        choices <- factor_subsets(total_plots)$labels
        if (is.null(choices)) {
            sort_choices <- "No options available"
        } else {
            dif <- vector(mode = "numeric", length = length(choices))
            for (option in 1:length(choices)) {
                dims <- unlist(strsplit(choices[[option]], " x "))
                dif[option] <- abs(as.numeric(dims[1]) - as.numeric(dims[2]))
            }
            df_choices <- data.frame(choices = unlist(choices), diff_dim = dif)
            df_choices <- df_choices[order(df_choices$diff_dim, decreasing = FALSE), ]
            sort_choices <- as.vector(df_choices$choices)
        }
        updateSelectInput(
            inputId = "dimensions_preps",
            choices = sort_choices,
            selected = sort_choices[1])
    })

    dimensions <-  reactiveValues()

    observeEvent(input$prep_randomize_multi_loc, {
      prep_number_of_locs <- input$locs_prep
      
      for (i in 1:prep_number_of_locs) {
        req(input[[paste0("dimensions_loc_", i)]])
        loc_field_dimension <- input[[paste0("dimensions_loc_", i)]]
        dimensions$i <- loc_field_dimension
      }
      # Close the modal after processing the input values
       removeModal()
    }) 

    field_dimensions_prep <- eventReactive(input$get_random_prep, {
      req(setup_optim_prep())
      if (input$dimensions_preps == "No options available") return(NULL)
      prep_number_of_locs <- input$locs_prep
      if (input$multi_dimension_toggle) {
        d_row <- vector(mode = "numeric", length = prep_number_of_locs)
        d_col <- vector(mode = "numeric", length = prep_number_of_locs)
        for (i in 1:prep_number_of_locs) {
          req(input[[paste0("dimensions_loc_", i)]])
          dims <- unlist(strsplit(input[[paste0("dimensions_loc_", i)]]," x "))
          d_row[i] <- as.numeric(dims[1])
          d_col[i] <- as.numeric(dims[2])
        }
      } else {
        dims <- unlist(strsplit(input$dimensions_preps," x "))
        d_row <- rep(as.numeric(dims[1]), prep_number_of_locs)
        d_col <- rep(as.numeric(dims[2]), prep_number_of_locs)
      }
      return(list(d_row = d_row, d_col = d_col))
    })

    format_list_no_checks <- data.frame(
      ENTRY = 1:10, 
      NAME = c(paste0("Genotype-", LETTERS[1:10]))
    )

    info_modal_multi_prep <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        renderTable(
			format_list_no_checks,
			bordered = TRUE,
			align = 'c',
			striped = TRUE
			),
		h5("Remark: If you want to include checks, please add them in the first rows of the file."),
        easyClose = FALSE
      )
    }
    
    toListen <- reactive({
      list(input$multi_prep_data)
    })
    
    observeEvent(toListen(), {
      if (input$multi_prep_data == 'Yes'){
        showModal(
          shinyjqui::jqui_draggable(
            info_modal_multi_prep()
          )
        )
      }
    })

    dimension_choices <- function() {
      req(setup_optim_prep())
      req(prep_inputs())
      plots_for_treatments <- as.numeric(setup_optim_prep()$size_locations[1])
      prep_checks <- as.numeric(prep_inputs()$prep_checks)
      if (!is.null(prep_inputs()$prep_checks)) {
          prep_checks <- as.numeric(prep_inputs()$prep_checks)
      } else {
          prep_checks <- 0
      }
      total_plots <- plots_for_treatments + sum(prep_checks)
      choices <- factor_subsets(total_plots)$labels
      if (is.null(choices)) {
          sort_choices <- "No options available"
      } else {
          dif <- vector(mode = "numeric", length = length(choices))
          for (option in 1:length(choices)) {
              dims <- unlist(strsplit(choices[[option]], " x "))
              dif[option] <- abs(as.numeric(dims[1]) - as.numeric(dims[2]))
          }
          df_choices <- data.frame(choices = unlist(choices), diff_dim = dif)
          df_choices <- df_choices[order(df_choices$diff_dim, decreasing = FALSE), ]
          sort_choices <- as.vector(df_choices$choices)
      }
      return(sort_choices)
    }

    multi_dimension_modal <- function() {
      modalDialog(
        title = div(tags$h3("Select different dimensions across multiple locations")),
        shiny::uiOutput(ns("additional_inputs")),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(inputId = ns("prep_randomize_multi_loc"), "GO")
        )
      )
    }
  
    output$additional_inputs <- shiny::renderUI({
      prep_number_of_locs <- input$locs_prep
      req(input$run_prep)
      # Create a list to store the UI elements
      ui_list <- lapply(1:prep_number_of_locs, function(i) {
        div(
          class = "multi-dimension-container",
          style = "display: flex; justify-content: left; align-items: left;",
          div(
            class = "col-12",
            style = "padding-top: 0.65em; hover: #f1f1f1;",
            selectInput(
              ns(paste0("dimensions_loc_", i)),
              paste0("Select dimension for location ", i),
              choices = dimension_choices()
            )
          )
        )
      })
      # Return the list of UI elements as a tagList (so that it renders correctly)
      do.call(tagList, ui_list)
    })

    observeEvent(input$multi_dimension_button, {
      showModal(
        shinyjqui::jqui_draggable(
            multi_dimension_modal()
          )
      )
    })

    randomize_hit_prep <- reactiveValues(times = 0)
 
    observeEvent(input$run_prep, {
      randomize_hit_prep$times <- 0
    })

    user_tries_prep <- reactiveValues(tries_prep = 0)

    observeEvent(input$get_random_prep, {
      user_tries_prep$tries_prep <- user_tries_prep$tries_prep + 1
      randomize_hit_prep$times <- randomize_hit_prep$times + 1
    })

    observeEvent(input$dimensions_preps, {
      user_tries_prep$tries_prep <- 0
    })

    list_to_observe_prep <- reactive({
      list(randomize_hit_prep$times, user_tries_prep$tries_prep)
    })

    observeEvent(list_to_observe_prep(), {
      output$download_prep_avg <- renderUI({
        if (randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0) {
          downloadButton(
            ns("downloadData.preps"),
            "Save Experiment",
            style = "width:100%")
        }
      })
    })

    observeEvent(input$run_prep, {
        req(setup_optim_prep())
        shinyjs::show(id = "dimensions_preps")
        shinyjs::show(id = "get_random_prep")
    })



  observeEvent(input$run_prep, {
        req(setup_optim_prep())
       #shinyjs::show(id = "dimensions_preps")
        shinyjs::show(id = "multi_dimension_toggle")
        shinyjs::show(id = "get_random_prep")

        observeEvent(input$multi_dimension_toggle, {
          if (input$multi_dimension_toggle == TRUE) {
            shinyjs::show(id = "multi_dimension_button")
            shinyjs::hide(id = "dimensions_preps")
          } else if (input$multi_dimension_toggle == FALSE) {
            shinyjs::show(id = "dimensions_preps")
            shinyjs::hide(id = "multi_dimension_button")
          }
        })
    })

    output$prep_allocation <- DT::renderDT({
        req(setup_optim_prep())
        req(get_multi_loc_prep())
        data_without_checks <- get_multi_loc_prep()$data_without_checks
        prep_lines <- prep_inputs()$prep_lines

        gen_names <- data_without_checks %>%
            dplyr::mutate(sparse_entry = 1:prep_lines) %>%
            dplyr::arrange(sparse_entry) %>%
            dplyr::select(NAME) %>%
            dplyr::pull()

        locs <- prep_inputs()$sites
        df <- as.data.frame(setup_optim_prep()$allocation)
        df <- df %>% 
            dplyr::mutate(
                Copies = rowSums(.),
                Avg = round(Copies / locs, 1)
            ) %>%
            dplyr::bind_rows(colSums(.))
        rownames(df) <- c(gen_names, "Total")
        df[nrow(df), ncol(df)] <- NA
        DT::datatable(   
            df,
            caption = 'Table 1: Genotype Allocation Across Environments.',
            extensions = 'Buttons',
            options = list(
                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                dom = 'Bfrtip',
                scrollY = "350px",
                lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                pageLength = nrow(df),
                buttons = c('copy', 'excel', 'print')
            )
        )
    })
    ###### Plotting the data ##############
    output$multi_prep_data_input <- DT::renderDT({
        req(setup_optim_prep())
        test <- randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0
        if (!test) return(NULL)
        req(setup_optim_prep())
        multi_loc_data <- setup_optim_prep()$multi_location_data
        df <- as.data.frame(multi_loc_data)
        # Combine the data frames into a single data frame with 
        # a new column for the list element name
        if (input$multi_prep_data == 'Yes') {
            req(setup_optim_prep())
            list_locs <- setup_optim_prep()$list_locs
            df <- dplyr::bind_rows(
                lapply(names(list_locs), function(name) {
                    dplyr::mutate(list_locs[[name]], LOCATION = name)
            })) %>% 
                dplyr::select(LOCATION, ENTRY, NAME, REPS)
        }
        df$LOCATION <- as.factor(df$LOCATION)
        df$ENTRY <- as.factor(df$ENTRY)
        df$NAME <- as.factor(df$NAME)
        df$REPS <- as.factor(df$REPS)
        options(DT.options = list(
            pageLength = nrow(df), 
            autoWidth = FALSE,
            scrollX = TRUE, scrollY = "500px"))
        DT::datatable(
            df,
            rownames = FALSE, 
            filter = 'top',
            options = list(
            columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    })
    
    pREPS_reactive <- reactive({
        req(setup_optim_prep())
        req(field_dimensions_prep())
        entry_list <- setup_optim_prep()$list_locs
        nrows <- field_dimensions_prep()$d_row
        ncols <- field_dimensions_prep()$d_col
        niter <- 1000
        prep <- TRUE
        locs_preps <- prep_inputs()$sites
        site_names <- prep_inputs()$location_names
        preps_seed <- prep_inputs()$seed_number
        plotNumber <- prep_inputs()$plotNumber
        if (length(site_names) != locs_preps) {
            site_names <- paste0("LOC", 1:locs_preps)
        }
        if (length(plotNumber) != locs_preps) {
            plotNumber <- seq(1, 1000 * locs_preps, by = 1000)
        }
        movement_planter <- prep_inputs()$planter_mov
        expt_name <- prep_inputs()$expt_name
        locations_preps <- vector(mode = "list", length = locs_preps)
        withProgress(message = 'Running p-rep optimization ...', {
            locations_preps <- partially_replicated(
                nrows = nrows, 
                ncols = ncols, 
                l = locs_preps, 
                plotNumber = plotNumber, 
                exptName =  expt_name,
                locationNames = site_names, 
                planter = movement_planter,
                seed = preps_seed, 
                multiLocationData = TRUE,
                data = entry_list
            )
        })
        return(locations_preps)
    }) %>% 
      bindEvent(input$get_random_prep)
    
     user_site_selection <- reactive({
       return(as.numeric(input$loc_to_view_preps))
     })
    
    output$avg_field_preps <- DT::renderDataTable({
      test <- randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0
      if (!test) return(NULL)
      req(pREPS_reactive())
      selection <- as.numeric(user_site_selection())
      w_map <- pREPS_reactive()$layoutRandom[[selection]]
      checks = as.vector(pREPS_reactive()$treatments_with_reps[[selection]])
      len_checks <- length(checks)
      colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                   'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
      
      df <- as.data.frame(w_map)
      
      rownames(df) <- nrow(df):1
      colnames(df) <- paste0('V', 1:ncol(df))
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, 
                                scrollY = "700px"))
      DT::datatable(
        df,
        extensions = 'Buttons', 
            options = list(dom = 'Blfrtip',
            scrollX = TRUE,
            fixedColumns = TRUE,
            pageLength = nrow(df),
            scrollY = "620px",
            class = 'compact cell-border stripe',  rownames = FALSE,
            server = FALSE,
            filter = list( position = 'top', clear = FALSE, plain =TRUE ),
            buttons = c('copy', 'excel'),
            lengthMenu = list(c(10,25,50,-1),
                            c(10,25,50,"All")))) %>%
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                    backgroundColor = DT::styleEqual(c(checks),
                                                 c(rep(colores[3], len_checks))
                    )
      )
    })
    
    output$PREPSPLOTFIELD <- DT::renderDT({
      test <- randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0
      if (!test) return(NULL)
      req(pREPS_reactive())
      selection <- as.numeric(user_site_selection())
      plot_num <- pREPS_reactive()$plotNumber[[user_site_selection()]]
      df <- as.data.frame(plot_num)
      rownames(df) <- nrow(df):1
      colnames(df) <- paste0("V", 1:ncol(df))
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE))
      DT::datatable(
        df,
        extensions = 'Buttons', 
        options = list(
            dom = 'Blfrtip',
            scrollX = TRUE,
            fixedColumns = TRUE,
            pageLength = nrow(df),
            scrollY = "620px",
            class = 'compact cell-border stripe', rownames = FALSE,
            server = FALSE,
            filter = list( position = 'top', clear = FALSE, plain = TRUE ),
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
    
    observeEvent(input$simulate_prep_data, {
      req(pREPS_reactive()$fieldBook[[1]])
      test <- randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0
      if (test) {
        showModal(
          shinyjqui::jqui_draggable(
            simuModal.PREP()
          )
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
          shinyjqui::jqui_draggable(
            simuModal.PREP(failed = TRUE)
          )
        )
      }
    })
    
    simuDataPREP <- reactive({
      req(pREPS_reactive())
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
                nrows = nrows_prep[sites], 
                ncols = ncols_prep[sites], 
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
        df.prep_locs$ID <- 1:nrow(df.prep_locs)
        v <- 1
      }else {
        dataPrep <- pREPS_reactive()$fieldBook
        dataPrep$ID <- 1:nrow(dataPrep)
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
        hideTab(inputId = "tabset_prep_avg", target = "Heatmap")
      } else {
        showTab(inputId = "tabset_prep_avg", target = "Heatmap")
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
        req(input$loc_name_preps)
        loc <- input$loc_name_preps
        loc <- paste(loc, "_", "pREP_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(simuDataPREP()$df, file, row.names = FALSE)
      }
    )
 
  })
}
