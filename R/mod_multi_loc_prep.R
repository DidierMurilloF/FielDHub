#' multi_loc_preps UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_multi_loc_preps_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Optimized Multi-Location P-rep Design"),
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            width = 4,
            shiny::radioButtons(
                inputId = ns("multi_prep_data"), 
                label = "Import entries' list?", 
                choices = c("Yes", "No"), 
                selected = "No",
                inline = TRUE, 
                width = NULL, 
                choiceNames = NULL, 
                choiceValues = NULL
            ),
            shiny::conditionalPanel(
                condition = "input.multi_prep_data == 'Yes'", 
                ns = ns,
                shiny::fluidRow(
                shiny::column(
                    width = 7, # style=list("padding-right: 28px;"),
                    shiny::fileInput(
                        ns("file_multi_prep"), 
                        label = "Upload a CSV File:", 
                        multiple = FALSE
                    )
                ),
                shiny::column(
                    width = 5, #style=list("padding-left: 5px;"),
                    shiny::radioButtons(
                        ns("sep_multi_prep"), "Separator",
                        choices = c(Comma = ",",
                                    Semicolon = ";",
                                    Tab = "\t"),
                        selected = ",")
                    )
                )             
            ),
            shiny::numericInput(
                inputId = ns("gens_prep"), 
                label = "Input # of Entries:",
                value = 312,
                min = 1
            ),
            shiny::radioButtons(
                inputId = ns("include_checks"), 
                label = "Include checks?", 
                choices = c("Yes", "No"), 
                selected = "No",
                inline = TRUE, 
                width = NULL, 
                choiceNames = NULL, 
                choiceValues = NULL
            ),
            shiny::conditionalPanel(
                condition = "input.include_checks  == 'Yes'", 
                ns = ns,
                shiny::fluidRow(
                    shiny::column(
                        width = 6,
                        shiny::numericInput(
                            inputId = ns("prep_checks_met"),
                            label = "Input # of Checks:",
                            min = 1,
                            max = 10,
                            value = 3
                        )
                    ),
                    shiny::column(
                        width = 6,
                        shiny::textInput(
                            inputId = ns("prep_checks"), 
                            label = "Input # Check's Reps:", 
                            value = "8,8,8"
                        )
                    )
                )
            ),
            
            # sliderInput(ns("border_penalization_prep"), 
            #             label = "Border Penalization", 
            #             min = 0.00, 
            #             max = 1.00, 
            #             value = 0.3),
            # selectInput(
            #   ns("optimization_distance_method_prep"), 
            #   label = "Optimization Distance Method:", 
            #   choices = c("Euclidean" = "euclidean", "Manhattan" = "manhattan"), 
            #   selected = "manhattan"
            # ),
            shiny::fluidRow(
                shiny::column(
                    width = 6, 
                    shiny::numericInput(
                        inputId = ns("locs_prep"), 
                        label = "Input # of Locations:", 
                        value = 6, 
                        min = 2
                    )
                ),
                shiny::column(
                    width = 6,
                    shiny::selectInput(
                        inputId = ns("loc_to_view_preps"), 
                        label = "Choose Location to View:", 
                        choices = 1:1, 
                        selected = 1,
                        multiple = FALSE
                    )
                )
            ),
            shiny::selectInput(
                inputId = ns("plant_copies_preps"), 
                label = "# of Copies Per Entry:",
                choices = 1:6
            ),
            shiny::selectInput(
                ns("planter_preps"), 
                label = "Plot Order Layout:",
                choices = c("serpentine", "cartesian"), 
                multiple = FALSE,
                selected = "serpentine"
            ),
            shiny::checkboxInput(
                inputId = ns("allow_fillers_prep"),
                label = "Allow filler plots",
                value = FALSE
            ),
            shiny::fluidRow(
                shiny::column(
                    width = 6,
                    shiny::textInput(
                        ns("plot_start_preps"), 
                        "Starting Plot Number:", 
                        value = 1
                    )
                ),
                shiny::column(
                    width = 6, 
                    shiny::textInput(
                        ns("expt_name_preps"), 
                        "Input Experiment Name:", 
                        value = "Expt1"
                    )
                )
            ),  
            shiny::fluidRow(
                shiny::column(
                    width = 6,
                    app_seed_input(ns("seed_preps"), value = 1)
                ),
                shiny::column(
                    width = 6, 
                    shiny::textInput(
                        ns("loc_name_preps"), 
                        "Input Location Name:", 
                        value = "FARGO"
                    )
                )
            ),
            shiny::fluidRow(
                shiny::column(
                    width = 6,
                    shiny::actionButton(
                        inputId = ns("run_prep"), 
                        label = "Run!", 
                        icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                        width = '100%'
                    )
                ),
                shiny::column(
                    width = 6,
                    shiny::actionButton(
                        ns("simulate_prep_data"), 
                        label = "Simulate!", 
                        icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                        width = '100%'
                    )
                )
            ),
            shiny::br(),
            shiny::uiOutput(ns("download_prep_avg"))
        ),
        shiny::mainPanel(
            width = 8,
            shinyjs::useShinyjs(),
            shiny::tabsetPanel(id = ns("tabset_prep_avg"),
            shiny::tabPanel("Get Random", value = "tabPanel_prep_avg",
                shiny::br(),
                shiny::fluidRow(
                  shiny::column(
                    width = 4,
                    shinyjs::hidden(
                        shiny::selectInput(
                          inputId = ns("dimensions_preps"), 
                          label = "Select dimensions of field:", 
                          choices = "")
                    ),
                    shinyjs::hidden(
                        shiny::actionButton(
                          inputId = ns("multi_dimension_button"),
                          label = "Select multiple dimensions",
                          width = "80%",
                          style = "margin-top:25px;
                                  margin-bottom:15px")
                    )
                  ),
                  shiny::column(
                    4,
                    shiny::br(),
                    shinyjs::hidden(
                      shiny::checkboxInput(inputId = ns("multi_dimension_toggle"),
                                    label = "Set different dimensions across locations",
                                    value = FALSE)
                    )
                  )
                ),
                shiny::fluidRow(
                  shiny::column(
                    4,
                    shinyjs::hidden(
                        shiny::actionButton(
                          ns("get_random_prep"), 
                          label = "Randomize!",
                          style = "margin-bottom:12px")
                    )
                  )
                ),
                fieldhub_spinner(
                    DT::DTOutput(ns("prep_allocation")),
                    type = 4
                )
            ),
            shiny::tabPanel("Data Input", DT::DTOutput(ns("multi_prep_data_input"))),
            shiny::tabPanel("Randomized Field",
                    fieldhub_spinner(
                        DT::DTOutput(ns("avg_field_preps")), 
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
#' multi_loc_preps Server Functions
#'
#' @noRd 
mod_multi_loc_preps_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns

    shiny::observe({
        # validate_locations_input() validates the count before the existing
        # choices formula runs.
        prep_locs <- validate_design(validate_locations_input(input$locs_prep))
        start <- prep_locs + 1
        plant_reps <- start:(prep_locs * 2 - 1)
        shiny::updateSelectInput(inputId = "plant_copies_preps",
                            choices = plant_reps,
                            selected = plant_reps[2])
    })

    prep_inputs <- shiny::eventReactive(input$run_prep, {
        shiny::req(input$gens_prep)
        if (input$include_checks == "Yes"){
            prep_checks <- as.numeric(as.vector(unlist(strsplit(input$prep_checks, ","))))
            checks <- validate_design(read_n_checks(input$prep_checks_met))
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
        plotNumber <- validate_design(read_whole_numbers(
          input$plot_start_preps, "Starting Plot Number"
        ))
        site_names <- as.character(as.vector(unlist(strsplit(input$loc_name_preps, ","))))
        seed_number <- validate_design(app_design_seed(input$seed_preps))
        sites = as.numeric(input$locs_prep)
        # The values of design_args_multi_loc_preps_optim() and
        # design_args_multi_loc_preps(); multi_location_prep() applies its
        # own defaults to starting plots, location names or a blank
        # experiment name that do not fit
        return(
            list(
                lines = input_lines,
                l = sites, 
                copies_per_entry = as.numeric(input$plant_copies_preps),
                checks = checks,
                rep_checks = prep_checks,
                seed = seed_number, 
                planter = planter_mov,
                plot_start = plotNumber,
                expt_name = expt_name,
                location_names = site_names
            )
        ) 
    })

    shiny::observeEvent(prep_inputs()$l, {
        # location_view_choices() validates the count before building the range.
        loc_user_view <- validate_design(location_view_choices(prep_inputs()$l))
        shiny::updateSelectInput(
            inputId = "loc_to_view_preps",
            choices = loc_user_view,
            selected = loc_user_view[1])
    })

    shiny::observeEvent(input$multi_prep_data,
                 handlerExpr = shiny::updateTabsetPanel(session,
                                                 "tabset_prep_avg",
                                                 selected = "tabPanel_prep_avg"))

    shiny::observeEvent(input$run_prep,
                 handlerExpr = shiny::updateTabsetPanel(session,
                                                 "tabset_prep_avg",
                                                 selected = "tabPanel_prep_avg"))
    
    get_multi_loc_prep <- shiny::reactive({
        shiny::req(input$locs_prep)
        if (input$locs_prep < 2) {
            shinyalert::shinyalert(
                "Error!!", 
                "The system requires at least 2 locations to proceed.",
                type = "error"
            )
            return(NULL)
        }
        if (input$multi_prep_data == 'Yes') {
            shiny::req(input$file_multi_prep)
            shiny::req(input$gens_prep)
            inFile <- input$file_multi_prep
            data_ingested <- load_file(
                name = inFile$name,
                path = inFile[["datapath"]],
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
                    checks <- validate_design(read_n_checks(input$prep_checks_met))
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
            } else {
              app_upload_error(data_ingested,
                               missing_columns = "Data input needs at least three columns with: ENTRY, NAME and REPS.")
              return(NULL)
            }
        } else {
            shiny::req(input$prep_checks_met)
            shiny::req(input$prep_checks)
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
            }
            # do_optim() generates and names the entries (G-1, ...) and checks
            data_preps <- NULL
            data_without_checks <- NULL
        }
        return(
            list(
                multi_loc_preps_data = data_preps, 
                data_without_checks = data_without_checks
                )
            )
    }) |>
        shiny::bindEvent(input$run_prep)

    setup_optim_prep <- shiny::reactive({
        shiny::req(get_multi_loc_prep())
        shiny::req(prep_inputs())
        # An uploaded list is only validated here: multi_location_prep()
        # merges it into the locations at Randomize!
        data <- get_multi_loc_prep()$multi_loc_preps_data
        shiny::withProgress(message = 'Optimization in progress ...', {
            optim_out <- validate_design(
                do.call(do_optim, design_args_multi_loc_preps_optim(prep_inputs(), data))
            )
        })
        return(optim_out)
    }) |>
        shiny::bindEvent(input$run_prep)

    list_input_plots <- shiny::eventReactive(input$run_prep, {
        shiny::req(setup_optim_prep())
        shiny::req(get_multi_loc_prep())
        shiny::req(prep_inputs())
        if (!is.null(prep_inputs()$rep_checks)) {
            prep_checks <- as.numeric(prep_inputs()$rep_checks)
        } else {
            prep_checks <- 0
        }
        plots_for_treatments <- as.numeric(setup_optim_prep()$size_locations)
        total_plots <- plots_for_treatments + sum(prep_checks)
        return(
            list(total_plots = total_plots)
        )
    })
    
    shiny::observeEvent(list(list_input_plots(), input$allow_fillers_prep), {
        shiny::req(setup_optim_prep())
        shiny::req(prep_inputs())
        if (!is.null(prep_inputs()$rep_checks)) {
            prep_checks <- as.numeric(prep_inputs()$rep_checks)
        } else {
            prep_checks <- 0
        }
        plots_for_treatments <- as.numeric(setup_optim_prep()$size_locations)
        total_plots <- plots_for_treatments + sum(prep_checks)
        options <- validate_design(prep_dimension_options(
            total_plots = total_plots[1],
            allow_fillers = isTRUE(input$allow_fillers_prep),
            max_fillers = .prep_max_fillers
        ))
        if (is.null(options)) {
            sort_choices <- "No options available"
        } else {
            sort_choices <- stats::setNames(options$value, options$label)
        }
        shiny::updateSelectInput(
            inputId = "dimensions_preps",
            choices = sort_choices,
            selected = sort_choices[1])
        if (is.null(options)) {
            shinyjs::hide(id = "get_random_prep")
            if (!isTRUE(input$allow_fillers_prep)) {
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

    dimensions <-  shiny::reactiveValues()

    shiny::observeEvent(input$prep_randomize_multi_loc, {
      prep_number_of_locs <- validate_design(validate_locations_input(input$locs_prep))

      for (i in seq_len(prep_number_of_locs)) {
        shiny::req(input[[paste0("dimensions_loc_", i)]])
        loc_field_dimension <- input[[paste0("dimensions_loc_", i)]]
        dimensions$i <- loc_field_dimension
      }
      # Close the modal after processing the input values
       shiny::removeModal()
    }) 

    field_dimensions_prep <- shiny::eventReactive(input$get_random_prep, {
      shiny::req(setup_optim_prep())
      if (input$dimensions_preps == "No options available") return(NULL)
      prep_number_of_locs <- validate_design(validate_locations_input(input$locs_prep))
      if (input$multi_dimension_toggle) {
        d_row <- vector(mode = "numeric", length = prep_number_of_locs)
        d_col <- vector(mode = "numeric", length = prep_number_of_locs)
        for (i in seq_len(prep_number_of_locs)) {
          shiny::req(input[[paste0("dimensions_loc_", i)]])
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
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(
			format_list_no_checks,
			bordered = TRUE,
			align = 'c',
			striped = TRUE
			),
		shiny::h5("Remark: If you want to include checks, please add them in the first rows of the file."),
        easyClose = FALSE
      )
    }
    
    toListen <- shiny::reactive({
      list(input$multi_prep_data)
    })
    
    shiny::observeEvent(toListen(), {
      if (input$multi_prep_data == 'Yes'){
        shiny::showModal(
          info_modal_multi_prep()
        )
      }
    })

    dimension_choices <- function(site = 1) {
      shiny::req(setup_optim_prep())
      shiny::req(prep_inputs())
      if (!is.null(prep_inputs()$rep_checks)) {
          prep_checks <- as.numeric(prep_inputs()$rep_checks)
      } else {
          prep_checks <- 0
      }
      plots_for_treatments <- as.numeric(setup_optim_prep()$size_locations[site])
      total_plots <- plots_for_treatments + sum(prep_checks)
      options <- validate_design(prep_dimension_options(
        total_plots = total_plots,
        allow_fillers = isTRUE(input$allow_fillers_prep),
        max_fillers = .prep_max_fillers
      ))
      if (is.null(options)) {
          sort_choices <- "No options available"
      } else {
          sort_choices <- stats::setNames(options$value, options$label)
      }
      return(sort_choices)
    }

    multi_dimension_modal <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Select different dimensions across multiple locations")),
        shiny::uiOutput(ns("additional_inputs")),
        easyClose = FALSE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(inputId = ns("prep_randomize_multi_loc"), "GO")
        )
      )
    }
  
    output$additional_inputs <- shiny::renderUI({
      shiny::req(input$run_prep)
      prep_number_of_locs <- validate_design(validate_locations_input(input$locs_prep))
      # Create a list to store the UI elements
      ui_list <- lapply(seq_len(prep_number_of_locs), function(i) {
        shiny::div(
          class = "multi-dimension-container",
          style = "display: flex; justify-content: left; align-items: left;",
          shiny::div(
            class = "col-12",
            style = "padding-top: 0.65em; hover: #f1f1f1;",
            shiny::selectInput(
              ns(paste0("dimensions_loc_", i)),
              paste0("Select dimension for location ", i),
              choices = dimension_choices(i)
            )
          )
        )
      })
      # Return the list of UI elements as a tagList (so that it renders correctly)
      do.call(shiny::tagList, ui_list)
    })

    shiny::observeEvent(input$multi_dimension_button, {
      shiny::showModal(
        multi_dimension_modal()
      )
    })

    randomize_hit_prep <- shiny::reactiveValues(times = 0)
 
    shiny::observeEvent(input$run_prep, {
      randomize_hit_prep$times <- 0
    })

    user_tries_prep <- shiny::reactiveValues(tries_prep = 0)

    shiny::observeEvent(input$get_random_prep, {
      user_tries_prep$tries_prep <- user_tries_prep$tries_prep + 1
      randomize_hit_prep$times <- randomize_hit_prep$times + 1
    })

    shiny::observeEvent(input$dimensions_preps, {
      user_tries_prep$tries_prep <- 0
    })

    list_to_observe_prep <- shiny::reactive({
      list(randomize_hit_prep$times, user_tries_prep$tries_prep)
    })

    shiny::observeEvent(list_to_observe_prep(), {
      output$download_prep_avg <- shiny::renderUI({
        if (randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0) {
          shiny::downloadButton(
            ns("downloadData.preps"),
            "Save experiment (ZIP)",
            style = "width:100%")
        }
      })
    })

    shiny::observeEvent(input$run_prep, {
        shiny::req(setup_optim_prep())
        shinyjs::show(id = "dimensions_preps")
    })

  show_dimension_inputs <- function() {
    if (isTRUE(input$multi_dimension_toggle)) {
      shinyjs::show(id = "multi_dimension_button")
      shinyjs::hide(id = "dimensions_preps")
    } else {
      shinyjs::show(id = "dimensions_preps")
      shinyjs::hide(id = "multi_dimension_button")
    }
  }

  shiny::observeEvent(input$run_prep, {
        shiny::req(setup_optim_prep())
       #shinyjs::show(id = "dimensions_preps")
        shinyjs::show(id = "multi_dimension_toggle")
        show_dimension_inputs()
    })

  # Registered once; it used to be nested in the run_prep observer, which
  # added a new copy of it on every click
  shiny::observeEvent(input$multi_dimension_toggle, show_dimension_inputs(),
               ignoreInit = TRUE)

    output$prep_allocation <- DT::renderDT({
        shiny::req(setup_optim_prep())
        shiny::req(get_multi_loc_prep())
        # Uploaded names (as parsed at Run!), or the names do_optim() gave
        # the generated entries
        if (!is.null(get_multi_loc_prep()$data_without_checks)) {
            gen_names <- get_multi_loc_prep()$data_without_checks$NAME
        } else {
            gen_names <- allocation_entry_names(setup_optim_prep(), prep_inputs()$lines)
        }

        locs <- prep_inputs()$l
        df <- as.data.frame(setup_optim_prep()$allocation)
        df <- df |> 
            dplyr::mutate(
                Copies = rowSums(dplyr::across(dplyr::everything())),
                Avg = round(Copies / locs, 1)
            ) 
        df_sumCols <- colSums(df) 
        df <- dplyr::bind_rows(df, df_sumCols) 
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
                buttons = app_table_export_buttons(setup_optim_prep(), "Allocation", print = TRUE)
            )
        )
    })
    ###### Plotting the data ##############
    output$multi_prep_data_input <- DT::renderDT({
        test <- randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0
        if (!test) return(NULL)
        shiny::req(pREPS_reactive())
        multi_loc_data <- pREPS_reactive()$multi_location_data
        df <- as.data.frame(multi_loc_data)
        # With a list uploaded at Run! (the same snapshot as the allocation
        # table), the uploaded names as multi_location_prep() merged them
        # into each location: combine the data frames into a single data
        # frame with a new column for the list element name
        if (!is.null(get_multi_loc_prep()$data_without_checks)) {
            list_locs <- pREPS_reactive()$list_locs
            df <- dplyr::bind_rows(
                lapply(names(list_locs), function(name) {
                    dplyr::mutate(list_locs[[name]], LOCATION = name)
            })) |> 
                dplyr::select(LOCATION, ENTRY, NAME, REPS)
        }
        df$LOCATION <- as.factor(df$LOCATION)
        df$ENTRY <- as.factor(df$ENTRY)
        df$NAME <- as.factor(df$NAME)
        df$REPS <- as.factor(df$REPS)
        table_options <- list(
            pageLength = nrow(df), 
            autoWidth = FALSE,
            scrollX = TRUE, scrollY = "500px")
        DT::datatable(
            df,
            rownames = FALSE, 
            filter = 'top',
            options = utils::modifyList(table_options, list(
            columnDefs = list(list(className = 'dt-center', targets = "_all")))))
    })
    
    pREPS_reactive <- shiny::reactive({
        shiny::req(setup_optim_prep())
        shiny::req(field_dimensions_prep())
        values <- c(prep_inputs(), list(
            nrows = field_dimensions_prep()$d_row,
            ncols = field_dimensions_prep()$d_col,
            optim_list = setup_optim_prep(),
            allow_fillers = isTRUE(input$allow_fillers_prep)
        ))
        data <- get_multi_loc_prep()$multi_loc_preps_data
        shiny::withProgress(message = 'Running p-rep optimization ...', {
            locations_preps <- validate_design(
                do.call(multi_location_prep, design_args_multi_loc_preps(values, data))
            )
        })
        return(locations_preps)
    }) |> 
      shiny::bindEvent(input$get_random_prep)
    
     user_site_selection <- shiny::reactive({
       return(as.numeric(input$loc_to_view_preps))
     })
    
    output$avg_field_preps <- DT::renderDataTable({
      test <- randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0
      if (!test) return(NULL)
      shiny::req(pREPS_reactive())
      selection <- as.numeric(user_site_selection())
      w_map <- pREPS_reactive()$layoutRandom[[selection]]
      w_map[pREPS_reactive()$fillerField[[selection]]] <- "Filler"
      checks = as.vector(pREPS_reactive()$treatments_with_reps[[selection]])
      len_checks <- length(checks)
      colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                   'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
      
      df <- as.data.frame(w_map)
      
      rownames(df) <- nrow(df):1
      colnames(df) <- paste0('V', 1:ncol(df))
      table_options <- list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollY = "700px")
      DT::datatable(
        df,
        extensions = 'Buttons', 
            options = utils::modifyList(table_options, list(dom = 'Blfrtip',
            scrollX = TRUE,
            fixedColumns = TRUE,
            pageLength = nrow(df),
            scrollY = "620px",
            class = 'compact cell-border stripe',  rownames = FALSE,
            server = FALSE,
            filter = list( position = 'top', clear = FALSE, plain =TRUE ),
            buttons = app_table_export_buttons(pREPS_reactive(), "Entry layout", user_site_selection()),
            lengthMenu = list(c(10,25,50,-1),
                            c(10,25,50,"All"))))) |>
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                    backgroundColor = DT::styleEqual(c(checks),
                                                 c(rep(colores[3], len_checks))
                    )
      )
    })
    
    output$PREPSPLOTFIELD <- DT::renderDT({
      test <- randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0
      if (!test) return(NULL)
      shiny::req(pREPS_reactive())
      selection <- as.numeric(user_site_selection())
      plot_num <- pREPS_reactive()$plotNumber[[user_site_selection()]]
      df <- as.data.frame(plot_num)
      rownames(df) <- nrow(df):1
      colnames(df) <- paste0("V", 1:ncol(df))
      table_options <- list(pageLength = nrow(df), autoWidth = FALSE)
      DT::datatable(
        df,
        extensions = 'Buttons', 
        options = utils::modifyList(table_options, list(
            dom = 'Blfrtip',
            scrollX = TRUE,
            fixedColumns = TRUE,
            pageLength = nrow(df),
            scrollY = "620px",
            class = 'compact cell-border stripe', rownames = FALSE,
            server = FALSE,
            filter = list( position = 'top', clear = FALSE, plain = TRUE ),
            buttons = app_table_export_buttons(pREPS_reactive(), "Plot numbers", user_site_selection()),
            lengthMenu = list(c(10,25,50,-1),
                                c(10,25,50,"All"))))
        
        )
    })

    app_spatial_workflow(input, output, session,
      design = function() pREPS_reactive(),
      seed = function() validate_design(read_app_seed(prep_inputs()$seed)),
      dimensions = function(field_book) list(nrows = field_dimensions_prep()$d_row, ncols = field_dimensions_prep()$d_col),
      selected = function() user_site_selection(),
      filename = function() {
        shiny::req(input$loc_name_preps)
        loc <- input$loc_name_preps
        loc <- paste(loc, "_", "pREP_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      visible = function() randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0,
      simulation_ready = function() {
        shiny::req(pREPS_reactive()$fieldBook[[1]])
        test <- randomize_hit_prep$times > 0 & user_tries_prep$tries_prep > 0
        if (test) {
            TRUE
        }
      },
      book_ready = function() {
        shiny::req(pREPS_reactive()$fieldBook)
        shiny::req(prep_inputs())
      },
      spec = spatial_workflow_spec("multi_loc_preps")
    )

  })
}
