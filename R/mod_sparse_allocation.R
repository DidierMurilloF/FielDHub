#' sparse_allocation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_sparse_allocation_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Unreplicated Designs: Sparse Allocation"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        shiny::radioButtons(
            inputId = ns("input_sparse_data"),
            label = "Import entries' list?",
            choices = c("Yes", "No"), 
            selected = "No",
            inline = TRUE,
            width = NULL,
            choiceNames = NULL,
            choiceValues = NULL
        ),
        shiny::conditionalPanel(
            condition = "input.input_sparse_data == 'Yes'", 
            ns = ns,
            shiny::fluidRow(
                shiny::column(
                    width = 7, 
                    style=list("padding-right: 28px;"),
                    shiny::fileInput(
                        ns("sparse_file"), 
                        label = "Upload a CSV File:", 
                        multiple = FALSE
                    )
                ),
                shiny::column(
                    width = 5,
                    tyle=list("padding-left: 5px;"),
                    shiny::radioButtons(
                        ns("sparse_file_sep"), "Separator",
                        choices = c(Comma = ",",
                                    Semicolon = ";",
                                    Tab = "\t"),
                        selected = ",")
                )
            )              
        ),
        shiny::numericInput(
            inputId = ns("sparse_lines"), 
            label = "Input # of Entries:",
            value = 380, 
            min = 50
        ),
        shiny::selectInput(
            inputId = ns("sparse_checks"),
            label = "Input # of Checks:",
            choices = c(1:10),
            multiple = FALSE,
            selected = 4
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            style=list("padding-right: 28px;"),
            shiny::numericInput(
                inputId = ns("sparse_locations"), 
                label = "Input # of Locations:", 
                value = 5,
                min = 3
            )
          ),
          shiny::column(
            width = 6,
            style=list("padding-left: 5px;"),
            shiny::selectInput(
                inputId = ns("sparse_loc_view"), 
                label = "Choose Location to View:", 
                choices = 1, 
                selected = 1, 
                multiple = FALSE
            )
        )
        ),
        shiny::selectInput(
            inputId = ns("plant_reps"), 
            label = "# of Copies Per Entry:",
            choices = 1:6
        ),
        shiny::selectInput(
            inputId = ns("sparse_planter"), 
            label = "Plot Order Layout:",
            choices = c("serpentine", "cartesian"), 
            multiple = FALSE,
            selected = "serpentine"
        ),
        shiny::fluidRow(
            shiny::column(
                width = 6,
                style=list("padding-right: 28px;"),
                shiny::textInput(
                    ns("sparse_plot_start"), 
                    "Starting Plot Number:", 
                    value = 1
                )
            ),
            shiny::column(
                width = 6,
                style=list("padding-left: 5px;"),
                shiny::textInput(
                    ns("sparse_expt_name"), 
                    "Input Experiment Name:", 
                    value = "Expt1"
                )
            )
        ),    
        shiny::fluidRow(
            shiny::column(
                width = 6,
                style=list("padding-right: 28px;"),
                shiny::numericInput(
                    inputId = ns("seed_single"), 
                    label = "Random Seed:", 
                    value = 17, 
                    min = 1
                )
            ),
            shiny::column(
                width = 6,
                style=list("padding-left: 5px;"),
                shiny::textInput(
                    ns("sparse_loc_names"), 
                    "Input the Location:",
                    value = "FARGO"
                )
            )
        ),
        shiny::fluidRow(
            shiny::column(
                width = 6,
                shiny::actionButton(
                    inputId = ns("sparse_run"), 
                    "Run!", 
                    icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                    width = '100%'
                )
            ),
            shiny::column(
                width = 6,
                shiny::actionButton(
                    ns("sparse_simulate"),
                    "Simulate!",
                    icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                    width = '100%'
                )
            )
        ),
        shiny::br(),
        shiny::uiOutput(ns("sparse_download"))
      ),
      shiny::mainPanel(
        width = 8,
        shinyjs::useShinyjs(),
        shiny::tabsetPanel(
            id = ns("sparse_tabset_single"),
            shiny::tabPanel(
                title = "Expt Design Info", 
                value = "tabPanel1",
                shiny::br(),
                shinyjs::hidden(
                  shiny::selectInput(inputId = ns("sparse_dims"),
                              label = "Select dimensions of field:", 
                              choices = "", width = '400px')
                ),
                shinyjs::hidden(
                  shiny::actionButton(inputId = ns("sparse_get_random"),
                               label = "Randomize!")
                ),
                shiny::tags$br(),
                shiny::tags$br(),
                fieldhub_spinner(
                    DT::DTOutput(ns("sparse_allocation")),
                    type = 4
                )
            ),
            shiny::tabPanel("Data Input",
                     DT::DTOutput(ns("multi_loc_data_input"))),
            shiny::tabPanel("Randomized Field",
                        shiny::br(),
                        shinyjs::hidden(
                        shiny::selectInput(inputId = ns("percent_checks"),
                                    label = "Choose % of Checks:",
                                    choices = 1:9, width = '400px')
                        ),
                        DT::DTOutput(ns("randomized_layout"))),
            shiny::tabPanel("Plot Number Field",
                        DT::DTOutput(ns("plot_number_layout"))),
            shiny::tabPanel("Field Book",
                        DT::DTOutput(ns("fieldBook_diagonal"))),
            shiny::tabPanel("Heatmap", fieldhub_spinner(
                plotly::plotlyOutput(ns("heatmap_diag"),  width = "97%"), 
                type = 5)
            )
        )      
      )
    )
  )
}
    
#' sparse_allocation Server Functions
#'
#' @noRd 
mod_sparse_allocation_server <- function(id){
  shiny::moduleServer( id, function(input, output, session) {
    ns <- session$ns

    shinyjs::useShinyjs()
    
    # Evaluate a call to a FielDHub function, showing its errors in an alert
    # (and returning NULL) and its warnings in an alert after it finishes
    call_api <- function(expr) {
      warnings_found <- character(0)
      out <- tryCatch(
        withCallingHandlers(
          expr,
          warning = function(w) {
            warnings_found <<- c(warnings_found, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        ),
        error = function(e) {
          if (inherits(e, "shiny.silent.error")) stop(e)
          shinyalert::shinyalert(
            "Error!!",
            conditionMessage(e),
            type = "error"
          )
          NULL
        }
      )
      if (!is.null(out) && length(warnings_found) > 0) {
        shinyalert::shinyalert(
          "Warning!",
          paste(unique(warnings_found), collapse = "\n"),
          type = "warning"
        )
      }
      out
    }

    shiny::observe({
        shiny::req(input$sparse_locations)
        sparse_locs <- as.numeric(input$sparse_locations)
        start <- ceiling(sparse_locs / 2)
        plant_reps <- start:(sparse_locs - 1)
        shiny::updateSelectInput(
            inputId = "plant_reps", 
            choices = plant_reps, 
            selected = plant_reps[length(plant_reps)]
        )
    })
    

    counts <- shiny::reactiveValues(trigger = 0)
    
    shiny::observeEvent(input$sparse_run, {
      counts$trigger <- counts$trigger + 1
    })
    
    kindExpt_single <- "SUDC"

    randomize_hit <- shiny::reactiveValues(times = 0)
 
    shiny::observeEvent(input$sparse_run, {
      randomize_hit$times <- 0
    })

    user_tries <- shiny::reactiveValues(tries = 0)

    shiny::observeEvent(input$sparse_get_random, {
      randomize_hit$times <- randomize_hit$times + 1
      user_tries$tries <- user_tries$tries + 1
    })

    shiny::observeEvent(input$sparse_dims, {
      user_tries$tries <- 0
    })

    list_to_observe <- shiny::reactive({
      list(randomize_hit$times, user_tries$tries)
    })
    
    single_inputs <- shiny::eventReactive(input$sparse_run, {
        shiny::req(input$sparse_lines)
        shiny::req(input$sparse_plot_start)
        shiny::req(input$sparse_loc_names)
        shiny::req(input$sparse_locations)
        input_sparse_lines <- as.numeric(input$sparse_lines)
        planter_mov <- input$sparse_planter
        Name_expt <- as.vector(unlist(strsplit(input$sparse_expt_name, ",")))
        plotNumber <- validate_design(read_whole_numbers(
          input$sparse_plot_start, "Starting Plot Number"
        ))
        seed_number <- as.numeric(input$seed_single)
        location_names <- as.vector(unlist(strsplit(input$sparse_loc_names, ",")))
        sites = as.numeric(input$sparse_locations)
        if (length(location_names) == 0 || length(location_names) != sites) {
            location_names <- paste0("LOC", 1:sites)
        }
        if (length(plotNumber) == 0 || length(plotNumber) != sites) {
            plotNumber <- seq(1, 1000 * sites, by = 1000)[1:sites]
        }
        name_expt <- Name_expt
        if (length(Name_expt) == 0) {
            name_expt <- "expt_sparse"
        }
        return(
            list(
                sparse_lines = input_sparse_lines,
                sites = sites, 
                location_names = location_names, 
                seed_number = seed_number, 
                plotNumber = plotNumber,
                planter_mov = planter_mov,
                expt_name = name_expt,
                plant_reps = as.numeric(input$plant_reps)
            )
        )
    })
    
    shiny::observeEvent(single_inputs()$sites, {
      loc_user_view <- 1:as.numeric(input$sparse_locations)
      shiny::updateSelectInput(inputId = "sparse_loc_view",
                        choices = loc_user_view, 
                        selected = loc_user_view[1])
      plant_reps <- 1:(as.numeric(input$sparse_locations) - 1)
      shiny::updateSelectInput(inputId = "plant_reps",
                        choices = plant_reps, 
                        selected = plant_reps[length(plant_reps)])
    })
    
    shiny::observeEvent(kindExpt_single,
                 handlerExpr = shiny::updateTabsetPanel(session,
                                                 "sparse_tabset_single",
                                                 selected = "tabPanel1"))
    shiny::observeEvent(input$stacked,
                 handlerExpr = shiny::updateTabsetPanel(session,
                                                 "sparse_tabset_single",
                                                 selected = "tabPanel1"))
    shiny::observeEvent(input$sparse_checks,
                 handlerExpr = shiny::updateTabsetPanel(session,
                                                 "sparse_tabset_single",
                                                 selected = "tabPanel1"))
    shiny::observeEvent(input$sparse_dims,
                 handlerExpr = shiny::updateTabsetPanel(session,
                                                 "sparse_tabset_single",
                                                 selected = "tabPanel1"))
    shiny::observeEvent(single_inputs()$planter_mov,
                 handlerExpr = shiny::updateTabsetPanel(session,
                                                 "sparse_tabset_single",
                                                 selected = "tabPanel1"))
    shiny::observeEvent(input$sparse_lines,
                 handlerExpr = shiny::updateTabsetPanel(session,
                                                 "sparse_tabset_single",
                                                 selected = "tabPanel1"))
    shiny::observeEvent(input$sparse_locations,
                 handlerExpr = shiny::updateTabsetPanel(session,
                                                 "sparse_tabset_single",
                                                 selected = "tabPanel1"))
    shiny::observeEvent(input$input_sparse_data,
                 handlerExpr = shiny::updateTabsetPanel(session,
                                                 "sparse_tabset_single",
                                                 selected = "tabPanel1"))
    shiny::observeEvent(input$sparse_run,
                 handlerExpr = shiny::updateTabsetPanel(session,
                                                 "sparse_tabset_single",
                                                 selected = "tabPanel1"))
                                                  
    get_sparse_data <- shiny::reactive({
        shiny::req(input$sparse_locations)
        sparse_lines <- as.numeric(input$sparse_lines)
        if (input$sparse_locations < 3) {
            shinyalert::shinyalert(
                "Error!!", 
                "The system requires at least 3 locations to proceed.",
                type = "error"
            )
            return(NULL)
        }
        if (input$sparse_lines < 60) {
            shinyalert::shinyalert(
                "Error!!", 
                "The system requires at least 60 entries/lines to proceed!",
                type = "error"
            )
            return(NULL)
        }
        Option_NCD <- TRUE
        if (input$input_sparse_data == "Yes") {
            shiny::req(input$sparse_lines)
            shiny::req(input$sparse_checks)
            shiny::req(input$sparse_file)
            sparse_checks <- as.numeric(input$sparse_checks)
            inFile <- input$sparse_file
            data_ingested <- load_file(
                name = inFile$name, 
                path = inFile[["datapath"]],
                sep = input$sparse_file_sep, 
                check = TRUE, 
                design = "sdiag"
            )
            if (names(data_ingested) == "dataUp") {
                data_up <- data_ingested$dataUp
                if (ncol(data_up) < 2) {
                    shiny::validate("Data input needs at least two Columns with the ENTRY and NAME.")
                } 
                data_entry_UP <- na.omit(data_up[, 1:2])
                colnames(data_entry_UP) <- c("ENTRY", "NAME")
                checksEntries <- suppressWarnings(as.numeric(data_entry_UP[1:sparse_checks,1]))
                # sparse_allocation() needs the checks to be a range of
                # consecutive entries, in any order
                if (anyNA(checksEntries) || any(diff(sort(checksEntries)) != 1)) {
                    shinyalert::shinyalert(
                        "Error!!", 
                        paste(
                            "The checks (the first", sparse_checks, "rows of the file)",
                            "must have consecutive ENTRY numbers, for example 1, 2, 3, 4."
                        ),
                        type = "error"
                    )
                    return(NULL)
                }
                input_entries_column <- data_entry_UP[(sparse_checks + 1):nrow(data_entry_UP),1]
                input_entries <- as.numeric(input_entries_column)
                dim_data_entry <- nrow(data_entry_UP)
                entries_in_file <- nrow(data_entry_UP[(length(checksEntries) + 1):nrow(data_entry_UP), ])
                input_lines <- as.numeric(input$sparse_lines)
                data_without_checks <- data_entry_UP[(length(checksEntries) + 1):nrow(data_entry_UP), ]
                if (entries_in_file != input_lines) {
                    shinyalert::shinyalert(
                        "Error!!", 
                        "Number of entries in file does not match with the input value.", 
                        type = "error"
                    )
                    return(NULL)
                }
                return(
                    list(
                        data_entry = data_entry_UP,
                        data_without_checks = data_without_checks,
                        input_entries = input_entries,
                        dim_data_entry = dim_data_entry, 
                        dim_without_checks = entries_in_file,
                        upload = TRUE))
            } else {
              app_upload_error(data_ingested,
                               missing_columns = "Data input needs at least two columns: ENTRY and NAME")
              return(NULL)
            }
        } else {
            shiny::req(input$sparse_lines)
            shiny::req(input$sparse_checks)
            sparse_checks <- as.numeric(input$sparse_checks)
            checksEntries <- 1:sparse_checks
            lines <- input$sparse_lines
            max_entry <- lines
            df_checks <- default_entries(
                sparse_checks,
                prefix = "CH-",
                start = max_entry + 1
            )
            # Same names that do_optim() gives the entries
            gen.list <- default_entries(lines)
            input_entries <- as.numeric(gen.list$ENTRY)
            data_entry_UP <- dplyr::bind_rows(df_checks, gen.list)
            dim_data_entry <- nrow(data_entry_UP)
            entries_in_file <- nrow(data_entry_UP[(length(checksEntries) + 1):nrow(data_entry_UP), ])
            data_without_checks <- gen.list
            return(
                list(
                    data_entry = data_entry_UP,
                    data_without_checks = data_without_checks, 
                    input_entries = input_entries,
                    dim_data_entry = dim_data_entry, 
                    dim_without_checks = entries_in_file,
                    upload = FALSE
                )
            )
        }
    }) |>
        shiny::bindEvent(input$sparse_run)
    
    # Allocation of the entries to the locations, computed as
    # sparse_allocation() computes it. The design is built later from this
    # object (its sparse_list argument), so the allocation can be shown and
    # the field dimensions offered before randomizing. The uploaded data is
    # passed only to validate it: it does not change the allocation, and
    # sparse_allocation() merges it into the locations.
    sparse_setup <- shiny::reactive({
        shiny::req(input$input_sparse_data)
        shiny::req(get_sparse_data())
        sparse_data_input <- NULL
        if (get_sparse_data()$upload) {
            sparse_data_input <- get_sparse_data()$data_entry
        }
        input_lines <- get_sparse_data()$dim_without_checks
        checks <- as.numeric(input$sparse_checks)
        locs <- single_inputs()$sites
        shiny::withProgress(message = 'Optimization in progress ...', {
          optim_out <- call_api(
            do_optim(
              design = "sparse",
              lines = input_lines,
              l = locs,
              copies_per_entry = single_inputs()$plant_reps,
              add_checks = TRUE,
              checks = checks,
              seed = single_inputs()$seed_number,
              data = sparse_data_input
            )
          )
        })
        if (is.null(optim_out)) return(NULL)
        lines_within_loc <- as.numeric(optim_out$size_locations[1])
        choices_list <- field_dimensions(lines_within_loc = lines_within_loc)
        if (length(choices_list) == 0) {
          shinyalert::shinyalert(
            "Error!!",
            "Number of entries is too small!",
            type = "error"
          )
          return(NULL)
        } else return(optim_out)
    }) |>
        shiny::bindEvent(input$sparse_run)
    
    getChecks <- shiny::eventReactive(input$sparse_run, {
        shiny::req(sparse_setup())
        sparse_checks <- as.numeric(input$sparse_checks)
        data <- get_sparse_data()$data_entry
        # The design sorts the check entries, as sparse_allocation() does
        checksEntries <- sort(as.numeric(data[1:sparse_checks,1]))
        list(checksEntries = checksEntries, sparse_checks = sparse_checks)
    })
    
    list_inputs_diagonal <- shiny::eventReactive(input$sparse_run, {
        shiny::req(sparse_setup())
        shiny::req(getChecks())
        shiny::req(sparse_setup()$size_locations)
        sparse_checks <- as.numeric(getChecks()$sparse_checks)
        lines <- as.numeric(sparse_setup()$size_locations[1])
        return(list(lines, input$input_sparse_data, kindExpt_single, 
                    input$sparse_run))
    })

    shiny::observeEvent(list_inputs_diagonal(), {
        shiny::req(sparse_setup())
        shiny::req(get_sparse_data())
        shiny::req(sparse_setup()$size_locations)
        lines_within_loc <- as.numeric(sparse_setup()$size_locations[1])
        sort_choices <- validate_design(diagonal_dimension_choices(
            lines = lines_within_loc, checks = as.vector(getChecks()$checksEntries),
            kindExpt = kindExpt_single, planter = single_inputs()$planter_mov
        ))

        shiny::updateSelectInput(inputId = "sparse_dims",
                          choices = sort_choices,
                          selected = head(sort_choices, 1))
        if (length(sort_choices) == 0L) {
            shinyalert::shinyalert("No field dimensions available",
                                  "No feasible field was found for these entries and checks.",
                                  type = "error")
        }
    })
    
    shiny::observeEvent(input$sparse_run, {
        shiny::req(sparse_setup())
        shiny::req(get_sparse_data()$dim_data_entry)
        shinyjs::show(id = "sparse_dims")
        shinyjs::show(id = "sparse_get_random")
    })

    output$sparse_allocation <- DT::renderDT({
        shiny::req(get_sparse_data())
        shiny::req(sparse_setup())
        data_without_checks <- get_sparse_data()$data_without_checks
        sparse_lines <- single_inputs()$sparse_lines

        gen_names <- data_without_checks |>
            dplyr::mutate(sparse_entry = 1:sparse_lines) |>
            dplyr::arrange(sparse_entry) |>
            dplyr::select(NAME) |>
            dplyr::pull()

        locs <- single_inputs()$sites
        df <- as.data.frame(sparse_setup()$allocation)
        df <- df |> 
          dplyr::mutate(
            Copies = rowSums(dplyr::across(dplyr::everything()))
          ) 
        df_sumCols <- colSums(df) 
        df <- dplyr::bind_rows(df, df_sumCols) 
        rownames(df) <- c(gen_names, "Total")
        DT::datatable(
          df,
          caption = 'Table 1: Genotype Allocation Across Environments.',
          extensions = 'Buttons',
          options = list(
            columnDefs = list(list(className = 'dt-center', targets = "_all")),
            dom = 'Bfrtip',
            scrollY = "400px",
            lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
            pageLength = nrow(df),
            buttons = c('copy', 'excel', 'print')
          )
        )
    })
    
    ###### Display multi-location data ##############
    output$multi_loc_data_input <- DT::renderDT({
      test <- randomize_hit$times > 0 & user_tries$tries > 0
      if (!test) return(NULL)
      shiny::req(sparse_design())
      # Entries of each location, with the uploaded data merged in
      list_locs <- sparse_design()$list_locs
      # Combine the data frames into a single data frame with
      # a new column for the list element name
      df <- dplyr::bind_rows(
        lapply(names(list_locs), function(name) {
          dplyr::mutate(list_locs[[name]], LOCATION = name)
        })) |>
        dplyr::select(LOCATION, ENTRY, NAME)
      df$LOCATION <- as.factor(df$LOCATION)
      df$ENTRY <- as.factor(df$ENTRY)
      df$NAME <- as.factor(df$NAME)
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

    
    field_dimensions_diagonal <- shiny::eventReactive(input$sparse_get_random, {
      shiny::req(sparse_setup())
      shiny::req(input$sparse_dims)
      dims <- unlist(strsplit(input$sparse_dims, " x "))
      d_row <- as.numeric(dims[1])
      d_col <- as.numeric(dims[2])
      return(list(d_row = d_row, d_col = d_col))
    })

    entryListFormat_SUDC <- data.frame(
      ENTRY = 1:9, 
      NAME = c(c("CHECK1", "CHECK2","CHECK3"), paste("Genotype", LETTERS[1:6], 
                                                     sep = ""))
    )
    
    toListen <- shiny::reactive({
      list(input$input_sparse_data, kindExpt_single)
    })
    
    entriesInfoModal_SUDC <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(entryListFormat_SUDC,
                    bordered = TRUE,
                    align  = 'c',
                    striped = TRUE),
        shiny::h4("Note that the controls must be in the first rows of the CSV file."),
        easyClose = FALSE
      )
    }

    shiny::observeEvent(toListen(), {
      if (input$input_sparse_data == "Yes" && kindExpt_single == "SUDC") {
        shiny::showModal(
          entriesInfoModal_SUDC()
        )
      }
    })

    available_percent_table <- shiny::eventReactive(input$sparse_get_random, {
      shiny::req(input$sparse_dims)
      shiny::req(sparse_setup()$size_locations)
      sparse_checks <- as.numeric(getChecks()$sparse_checks)
      lines_within_loc <- as.numeric(sparse_setup()$size_locations[1])
      shiny::req(field_dimensions_diagonal())
      Option_NCD <- TRUE
      checksEntries <- as.vector(getChecks()$checksEntries)
      planter_mov <- single_inputs()$planter_mov
      n_rows <- field_dimensions_diagonal()$d_row
      n_cols <- field_dimensions_diagonal()$d_col
      diagonal_check_options(
          n_rows = n_rows,
          n_cols = n_cols,
          checks = checksEntries,
          Option_NCD = Option_NCD,
          kindExpt = kindExpt_single,
          planter_mov1 = planter_mov,
          data = NULL, 
          dim_data = lines_within_loc + sparse_checks,
          dim_data_1 = lines_within_loc,
          Block_Fillers = NULL
      )
    })

    shiny::observeEvent(available_percent_table()$dt, {
          my_out <- available_percent_table()$dt
          my_percent <- my_out[,2]
          len <- length(my_percent)
          selected <- my_percent[len]
          
          shiny::updateSelectInput(session = session,
                            inputId = 'percent_checks',
                            label = "Choose % of Checks:",
                            choices = my_percent,
                            selected = selected)
    })
    
    shiny::observeEvent(list_to_observe(), {
      if (randomize_hit$times > 0 & user_tries$tries > 0) {
        shinyjs::show(id = "percent_checks")
      } else {
        shinyjs::hide(id = "percent_checks")
      }
    })

    shiny::observeEvent(list_to_observe(), { #  user_tries$tries
      output$sparse_download <- shiny::renderUI({
        if (randomize_hit$times > 0 & user_tries$tries > 0) {
          shiny::downloadButton(ns("downloadData_Diagonal"),
                          "Save experiment (ZIP)",
                          style = "width:100%")
        }
      })
    })

    plot_number_sites <- shiny::reactive({
      shiny::req(single_inputs())
      if (is.null(single_inputs()$plotNumber)) {
        shiny::validate("Plot starting number is missing.")
      }
      l <- single_inputs()$sites
      plotNumber <- single_inputs()$plotNumber
      if(!is.numeric(plotNumber) && !is.integer(plotNumber)) {
        shiny::validate("plotNumber should be an integer or a numeric vector.")
      }

      if (anyNA(plotNumber) || any(plotNumber %% 1 != 0)) {
        shiny::validate("plotNumber should be integers.")
      }
      if (!is.null(l)) {
        if (is.null(plotNumber) || length(plotNumber) != l) {
          if (l > 1){
            plotNumber <- seq(1001, 1000*(l+1), 1000)
          } else plotNumber <- 1001
        }
      }else shiny::validate("Number of locations/sites is missing")

      return(plotNumber)
    })

    # The design of every location, built by sparse_allocation() from the
    # allocation computed at Run! and the dimensions and percentage of
    # checks chosen by the user. Every output is taken from this object.
    sparse_design <- shiny::reactive({
      # The field dimensions and options table are those of the last
      # Randomize!, which must come after the last Run!
      shiny::req(randomize_hit$times > 0 & user_tries$tries > 0)
      shiny::req(input$sparse_dims)
      shiny::req(sparse_setup())
      shiny::req(get_sparse_data())
      shiny::req(field_dimensions_diagonal())
      shiny::req(available_percent_table()$dt)
      shiny::req(single_inputs()$seed_number)
      # Wait until the percentage selector holds one of the options of the
      # current field (it is updated after the options table)
      percent <- suppressWarnings(as.numeric(input$percent_checks))
      options_percent <- as.numeric(available_percent_table()$dt[,2])
      shiny::req(shiny::isTruthy(percent), any(abs(options_percent - percent) < 1e-6))
      sparse_data_input <- NULL
      if (get_sparse_data()$upload) {
        sparse_data_input <- get_sparse_data()$data_entry
      }
      plotNumber <- plot_number_sites()
      sparse_list <- sparse_setup()
      design <- call_api(
        sparse_allocation(
          lines = get_sparse_data()$dim_without_checks,
          nrows = field_dimensions_diagonal()$d_row,
          ncols = field_dimensions_diagonal()$d_col,
          l = single_inputs()$sites,
          planter = single_inputs()$planter_mov,
          plotNumber = plotNumber,
          copies_per_entry = single_inputs()$plant_reps,
          checks = getChecks()$sparse_checks,
          exptName = single_inputs()$expt_name[1],
          locationNames = single_inputs()$location_names,
          sparse_list = sparse_list,
          seed = as.numeric(single_inputs()$seed_number),
          data = sparse_data_input,
          checksPercent = percent
        )
      )
      if (is.null(design)) return(NULL)
      if (is.null(design$fieldBook)) {
        shinyalert::shinyalert(
          "Error!!",
          "The field dimensions do not fit the entries. Please, choose other dimensions.",
          type = "error"
        )
        return(NULL)
      }
      return(design)
    })

    output$randomized_layout <- DT::renderDT({
      test <- randomize_hit$times > 0 & user_tries$tries > 0
      if (!test) return(NULL)
      shiny::req(input$sparse_dims)
      shiny::req(sparse_design())
      user_site <- as.numeric(input$sparse_loc_view)
      shiny::req(user_site <= length(sparse_design()$layoutRandom))
      r_map <- sparse_design()$layoutRandom[[user_site]]
      if (is.null(r_map))
        return(NULL)
      sparse_checks <- sparse_design()$infoDesign$entry_checks[[user_site]]
      len_checks <- length(sparse_checks)
      df <- as.data.frame(r_map)
      colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                    'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
      colnames(df) <- paste0("V", 1:ncol(df))
      rownames(df) <- nrow(df):1
      DT::datatable(
        df,#,
        extensions = c('Buttons'),# , 'FixedColumns'
        options = list(dom = 'Blfrtip',
                        autoWidth = FALSE,
                        scrollX = TRUE,
                        fixedColumns = TRUE,
                        pageLength = nrow(df),
                        scrollY = "590px",
                        class = 'compact cell-border stripe',
                        rownames = FALSE,
                        server = FALSE,
                        filter = list( position = 'top',
                                        clear = FALSE,
                                        plain =TRUE ),
                        buttons = c('copy', 'excel'),
                        lengthMenu = list(c(10,25,50,-1),
                                            c(10,25,50,"All")))
      ) |>
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                        backgroundColor = DT::styleEqual(c(sparse_checks),
                                                          colores[1:len_checks]))
      #}
    })

    output$plot_number_layout <- DT::renderDT({
      test <- randomize_hit$times > 0 & user_tries$tries > 0
      if (!test) return(NULL)
      shiny::req(sparse_design())
      user_site <- as.numeric(input$sparse_loc_view)
      shiny::req(user_site <= length(sparse_design()$plotsNumber))
      plot_num <- sparse_design()$plotsNumber[[user_site]]
      if (is.null(plot_num))
        return(NULL)
      df <- as.data.frame(plot_num)
      colnames(df) <- paste0("V", 1:ncol(df))
      rownames(df) <- nrow(df):1
      DT::datatable(df,
                    extensions = c('Buttons'),
                    options = list(dom = 'Blfrtip',
                                   autoWidth = FALSE,
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

    valsDIAG <- shiny::reactiveValues(ROX = NULL, ROY = NULL, trail = NULL, minValue = NULL,
                               maxValue = NULL)
    
    simuModal_DIAG <- function(failed = FALSE) {
      shiny::modalDialog(
        shiny::fluidRow(
          shiny::column(6,
                 shiny::selectInput(inputId = ns("trailsDIAG"), label = "Select One:",
                             choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
          )
        ),
        shiny::conditionalPanel("input.trailsDIAG == 'Other'", ns = ns,
                         shiny::textInput(inputId = ns("OtherDIAG"), label = "Input Trial Name:", value = NULL)
        ),
        app_spatial_correlations(ns, ".DIAG"),
        shiny::fluidRow(
          shiny::column(6,
                 shiny::numericInput(inputId = ns("min.diag"), "Input the min value:", value = NULL)
          ),
          shiny::column(6,
                 shiny::numericInput(inputId = ns("max.diag"), "Input the max value:", value = NULL)
          )
        ),
        if (failed)
          shiny::div(shiny::tags$b("Invalid input of data max and min", style = "color: red;")),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(inputId = ns("ok_simu_single"), "GO")
        )
      )
    }
    
    shiny::observeEvent(input$sparse_simulate, {
      shiny::req(sparse_design()$fieldBook)
      shiny::showModal(
        simuModal_DIAG()
      )
    })
    
    shiny::observeEvent(input$ok_simu_single, {
      shiny::req(input$min.diag, input$max.diag)
      if (input$max.diag > input$min.diag && input$min.diag != input$max.diag) {
        valsDIAG$maxValue <- input$max.diag
        valsDIAG$minValue  <- input$min.diag
        valsDIAG$ROX <- as.numeric(input$ROX.DIAG)
        valsDIAG$ROY <- as.numeric(input$ROY.DIAG)
        if(input$trailsDIAG == "Other") {
          shiny::req(input$OtherDIAG)
          if(!is.null(input$OtherDIAG)) {
            valsDIAG$trail <- as.character(input$OtherDIAG)
          }else shiny::showModal(simuModal_DIAG(failed = TRUE))
        }else {
          valsDIAG$trail <- as.character(input$trailsDIAG)
        }
        shiny::removeModal()
      }else {
        shiny::showModal(
          simuModal_DIAG(failed = TRUE)
        )
      }
    })
    
    simudata_DIAG <- shiny::reactive({
      shiny::req(sparse_design()$fieldBook)
      field_book <- sparse_design()$fieldBook
      if (is.null(valsDIAG$maxValue) || is.null(valsDIAG$minValue) ||
          is.null(valsDIAG$trail)) {
        return(list(df = field_book, simulation = NULL))
      }
      simulation <- validate_design(simulate_spatial_field_book(
        field_book = field_book,
        nrows = sparse_design()$infoDesign$rows, ncols = sparse_design()$infoDesign$columns,
        correlation_x = as.numeric(valsDIAG$ROX),
        correlation_y = as.numeric(valsDIAG$ROY),
        min_value = as.numeric(valsDIAG$minValue),
        max_value = as.numeric(valsDIAG$maxValue),
        response_name = as.character(valsDIAG$trail),
        seed = as.numeric(single_inputs()$seed_number)
      ))
      list(df = simulation$field_book, dfSimulationList = simulation$simulations,
           simulation = simulation)
    })

    heat_map <- shiny::reactiveValues(heat_map_option = FALSE)
    
    shiny::observeEvent(input$ok_simu_single, {
      shiny::req(input$min.diag, input$max.diag)
      if (input$max.diag > input$min.diag && input$min.diag != input$max.diag) {
        heat_map$heat_map_option <- TRUE
      }
    })

    shiny::observeEvent(heat_map$heat_map_option, {
      if (heat_map$heat_map_option == FALSE) {
        shiny::hideTab(inputId = "sparse_tabset_single", target = "Heatmap")
      } else {
        shiny::showTab(inputId = "sparse_tabset_single", target = "Heatmap")
      }
    })

    output$fieldBook_diagonal <- DT::renderDT({
      test <- randomize_hit$times > 0 & user_tries$tries > 0
      if (!test) return(NULL)
      shiny::req(simudata_DIAG()$df)
      df <- simudata_DIAG()$df
      df$EXPT <- as.factor(df$EXPT)
      df$LOCATION <- as.factor(df$LOCATION)
      df$PLOT <- as.factor(df$PLOT)
      df$ROW <- as.factor(df$ROW)
      df$COLUMN <- as.factor(df$COLUMN)
      df$CHECKS <- as.factor(df$CHECKS)
      df$ENTRY <- as.factor(df$ENTRY)
      df$TREATMENT <- as.factor(df$TREATMENT)
      table_options <- list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "600px")
      DT::datatable(df,
                    filter = "top",
                    rownames = FALSE,
                    options = utils::modifyList(table_options, list(
                      columnDefs = list(list(className = 'dt-center', targets = "_all")))))
    })
    
    
    heatmap_obj_D <- shiny::reactive({
      shiny::req(simudata_DIAG()$dfSimulationList)
      validate_design(app_spatial_heatmap(
        simudata_DIAG()$dfSimulationList,
        response_name = as.character(valsDIAG$trail),
        selected = as.numeric(input$sparse_loc_view), height = 720
      ))
    })
    
    output$heatmap_diag <- plotly::renderPlotly({
      test <- randomize_hit$times > 0 & user_tries$tries > 0
      if (!test) return(NULL)
      shiny::req(heatmap_obj_D())
      heatmap_obj_D()
    })
    
    output$downloadData_Diagonal <- app_csv_archive(
      filename = function() {
        shiny::req(input$sparse_loc_names)
        loc <- input$sparse_loc_names
        loc <- paste(loc, "_", "Diagonal_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      data = function() as.data.frame(simudata_DIAG()$df),
      design = sparse_design,
      field_book = function() simudata_DIAG()$df,
      simulation = function() simudata_DIAG()$simulation,
      kind = "field_book"
    )
    
    app_reproduction_outputs(output, sparse_design)
  })
}
    
## To be copied in the UI
# mod_sparse_allocation_ui("sparse_allocation_1")
    
## To be copied in the server
# mod_sparse_allocation_server("sparse_allocation_1")
