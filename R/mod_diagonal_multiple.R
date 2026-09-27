#' diagonal_multiple UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom utils write.csv
mod_diagonal_multiple_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Unreplicated Multiple Diagonal Arrangement"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        shiny::radioButtons(inputId = ns("list_entries_multiple"),
                     label = "Import entries' list?", 
                     choices = c("Yes", "No"), 
                     selected = "No",
                     inline = TRUE, 
                     width = NULL, 
                     choiceNames = NULL, 
                     choiceValues = NULL),
        shiny::checkboxInput(inputId = ns("sameEntries"),
                      label = "Repeat entries across experiments", 
                      value = FALSE),
        shiny::conditionalPanel(
          condition = "input.list_entries_multiple == 'Yes'", 
          ns = ns,
          shiny::fluidRow(
            shiny::column(7, style=list("padding-right: 28px;"),
                   shiny::fileInput(ns("file_multiple"),
                             label = "Upload a CSV File:", 
                             multiple = FALSE)),
            shiny::column(5,style=list("padding-left: 5px;"),
                   shiny::radioButtons(ns("sep.DIAGONALS"), "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","))
          )              
        ),
        shiny::conditionalPanel(
          condition = "input.list_entries_multiple == 'No'", 
          ns = ns,
          shiny::numericInput(inputId = ns("lines.db"),
                      label = "Input # of Entries:",
                      value = 300, 
                      min = 50)
        ),
        shiny::textInput(ns("blocks.db"),
                  "Input # Entries per Expt:",
                  value = "100,120,80"),
        
        shiny::selectInput(inputId = ns("checks.db"),
                    label = "Input # of Checks:",
                    choices = c(1:20),
                    multiple = FALSE, 
                    selected = 4), 
        shiny::fluidRow(
          shiny::column(6,style=list("padding-right: 28px;"),
                 shiny::numericInput(inputId = ns("locs_db"),
                              label = "Input # of Locations:", 
                              value = 1,
                              min = 1)
          ),
          shiny::column(6,style=list("padding-left: 5px;"),
                 shiny::selectInput(inputId = ns("locView_diagonal_db"),
                             label = "Choose location to view:", 
                             choices = 1, 
                             selected = 1, 
                             multiple = FALSE)
          )
        ),
        shiny::fluidRow(
          shiny::column(6,style=list("padding-right: 28px;"),
                 shiny::selectInput(inputId = ns("stacked"),
                             label = "Blocks Layout:",
                             choices = c("By Column", "By Row"), 
                             multiple = FALSE,
                             selected = "By Row")
          ),
          shiny::column(6,style=list("padding-left: 5px;"),
                 shiny::selectInput(inputId = ns("planter_multiple"),
                             label = "Plot Order Layout:",
                             choices = c("serpentine", "cartesian"), 
                             multiple = FALSE,
                             selected = "serpentine")
          )
        ),
        shiny::fluidRow(
          shiny::column(6,
                 style=list("padding-right: 28px;"),
                  shiny::textInput(ns("plot_start_multiple"),
                            "Starting Plot Number:", 
                            value = 1)
          ),
          shiny::column(6,
                 style=list("padding-left: 5px;"),
                 shiny::textInput(ns("expt_name_multiple"),
                           "Input Experiment Name:", 
                           value = c("Expt1, Expt2, Expt3"))
          )
        ),    
        shiny::fluidRow(
          shiny::column(6,
                 style=list("padding-right: 28px;"),
                  shiny::numericInput(inputId = ns("seed_multiple"),
                              label = "Random Seed:", 
                              value = 17, 
                              min = 1)
          ),
          shiny::column(6,
                 style=list("padding-left: 5px;"),
                 shiny::textInput(ns("location_multiple"),
                           "Input the Location:",
                           value = "FARGO")
          )
        ),
        shiny::fluidRow(
          shiny::column(6,
                 shiny::actionButton(
                   inputId = ns("RUN_multiple"), 
                   "Run!", 
                   icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                   width = '100%'),
          ),
          shiny::column(6,
                 shiny::actionButton(
                   ns("simulate_multiple"),
                   "Simulate!",
                   icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                   width = '100%')
          )
        ),
        shiny::br(),
        shiny::uiOutput(ns("download_multi"))
      ),
      shiny::mainPanel(
        width = 8,
        shinyjs::useShinyjs(),
        shiny::tabsetPanel(id = ns("tabset_multi"),
                    shiny::tabPanel(title = "Expt Design Info", value = "tabPanel1",
                             shiny::br(),
                             shinyjs::hidden(
                               shiny::selectInput(inputId = ns("dimensions_multiple"),
                                           label = "Select dimensions of field:", 
                                           choices = "", width = '400px')
                             ),
                             shinyjs::hidden(
                               shiny::actionButton(inputId = ns("get_random_multi"),
                                            label = "Randomize!")
                             ),
                             shiny::br(),
                             shiny::br(),
                             DT::DTOutput(ns("options_table_multi"))
                    ),
                    shiny::tabPanel("Input Data",
                             shiny::fluidRow(
                               shiny::column(6,DT::DTOutput(ns("data_input"))),
                               shiny::column(6,DT::DTOutput(ns("checks_table")))
                             )
                    ),
                    shiny::tabPanel("Randomized Field",
                             shiny::br(),
                             shinyjs::hidden(
                               shiny::selectInput(inputId = ns("percent_checks_multi"),
                                           label = "Choose % of Checks:",
                                           choices = 1:9, width = '400px')
                             ),
                             DT::DTOutput(ns("randomized_layout"))),
                    shiny::tabPanel("Expt Layout", DT::DTOutput(ns("name_layout"))),
                    shiny::tabPanel("Plot Number Field", DT::DTOutput(ns("plot_number_layout"))),
                    shiny::tabPanel("Field Book", DT::DTOutput(ns("fieldBook_diagonal"))),
                    shiny::tabPanel("Heatmap",
                             fieldhub_spinner(
                               plotly::plotlyOutput(
                                 ns("heatmap_diag"),
                                 width = "97%"
                                 ), 
                               type = 5
                               )
                    )
        )      
      )
    )
  )
}
#' Diagonal Server Functions
#'
#' @noRd 
mod_diagonal_multiple_server <- function(id) {
    shiny::moduleServer( id, function(input, output, session) {
        ns <- session$ns
        
        counts_multi <- shiny::reactiveValues(trigger_multi = 0)
        
        shiny::observeEvent(input$RUN_multiple, {
            counts_multi$trigger_multi <- counts_multi$trigger_multi + 1
        })
        
        kindExpt = "DBUDC"
        
        randomize_hit_multi <- shiny::reactiveValues(times_multi = 0)
        
        shiny::observeEvent(input$RUN_multiple, {
            randomize_hit_multi$times_multi <- 0
        })
        
        user_tries_multi <- shiny::reactiveValues(tries = 1)
        
        shiny::observeEvent(input$get_random_multi, {
            randomize_hit_multi$times_multi <- randomize_hit_multi$times_multi + 1
            user_tries_multi$tries <- user_tries_multi$tries + 1
        })
        
        shiny::observeEvent(input$dimensions_multiple, {
        user_tries_multi$tries <- 0
        })
        
        list_to_observe_multi <- shiny::reactive({
            list(randomize_hit_multi$times_multi, user_tries_multi$tries)
        })
        
        shinyjs::useShinyjs()
        
        multiple_inputs <- shiny::eventReactive(input$RUN_multiple, {
            stacked <- input$stacked
            planter_mov <- input$planter_multiple
            blocks <- as.vector(unlist(strsplit(input$blocks.db, ",")))
            n_blocks <- length(blocks)
            Name_expt <- as.vector(unlist(strsplit(input$expt_name_multiple, ",")))
            Name_expt <- gsub(" ", "", Name_expt)
            if (length(Name_expt) == n_blocks) {
                expe_names <- Name_expt
            }else {
                expe_names = paste0(rep("Block", times = n_blocks), 1:n_blocks)
            }
            plotNumber <- validate_design(read_whole_numbers(
              input$plot_start_multiple, "Starting Plot Number"
            ))
            seed_number <- as.numeric(input$seed_multiple)
            location_names <- trimws(as.vector(unlist(strsplit(input$location_multiple, ","))))
            sites = as.numeric(input$locs_db)
            return(list(sites = sites, 
                        location_names = location_names, 
                        seed_number = seed_number, 
                        plotNumber = plotNumber,
                        planter_mov = planter_mov,
                        expt_name = expe_names, 
                        blocks = blocks,
                        stacked  = stacked)) 
        })
        
        shiny::observeEvent(multiple_inputs()$sites, {
            loc_user_view <- 1:as.numeric(input$locs_db)
            shiny::updateSelectInput(inputId = "locView_diagonal_db",
                                choices = loc_user_view, 
                                selected = loc_user_view[1])
        })
        
        shiny::observeEvent(kindExpt,
                    handlerExpr = shiny::updateTabsetPanel(session,
                                                    "tabset_multi",
                                                    selected = "tabPanel1"))
        shiny::observeEvent(input$stacked,
                    handlerExpr = shiny::updateTabsetPanel(session,
                                                    "tabset_multi",
                                                    selected = "tabPanel1"))
        shiny::observeEvent(input$checks.db,
                    handlerExpr = shiny::updateTabsetPanel(session,
                                                    "tabset_multi",
                                                    selected = "tabPanel1"))
        shiny::observeEvent(input$dimensions_multiple,
                    handlerExpr = shiny::updateTabsetPanel(session,
                                                    "tabset_multi",
                                                    selected = "tabPanel1"))
        shiny::observeEvent(input$planter_multiple,
                    handlerExpr = shiny::updateTabsetPanel(session,
                                                    "tabset_multi",
                                                    selected = "tabPanel1"))
        shiny::observeEvent(input$lines.db,
                    handlerExpr = shiny::updateTabsetPanel(session,
                                                    "tabset_multi",
                                                    selected = "tabPanel1"))
        shiny::observeEvent(input$locs_db,
                    handlerExpr = shiny::updateTabsetPanel(session,
                                                    "tabset_multi",
                                                    selected = "tabPanel1"))
        shiny::observeEvent(input$list_entries_multiple,
                    handlerExpr = shiny::updateTabsetPanel(session,
                                                    "tabset_multi",
                                                    selected = "tabPanel1"))
        shiny::observeEvent(input$RUN_multiple,
                    handlerExpr = shiny::updateTabsetPanel(session,
                                                    "tabset_multi",
                                                    selected = "tabPanel1"))
        
        
        get_data_multiple <- shiny::eventReactive(input$RUN_multiple, {
            Option_NCD <- TRUE
            if (input$list_entries_multiple == "Yes") {
                checking_entry_list = TRUE
                if (input$sameEntries) {
                    checking_entry_list = FALSE
                }
                shiny::req(input$checks.db)
                shiny::req(input$file_multiple)
                inFile <- input$file_multiple
                data_ingested <- load_file(
                    name = inFile$name, 
                    path = inFile[["datapath"]],
                    sep = input$sep.DIAGONALS, 
                    check = checking_entry_list, 
                    design = "mdiag"
                )
                if (names(data_ingested) == "dataUp") {
                    data_up <- data_ingested$dataUp
                    data_entry <- na.omit(data_up)
                    if (ncol(data_entry) < 2) {
                        shinyalert::shinyalert(
                        "Error!!", 
                        "Data input needs at least three Columns with the ENTRY and NAME.", 
                        type = "error")
                        return(NULL)
                    } 
                    data_entry_UP <- data_entry[,1:2] 
                    checks <- as.numeric(input$checks.db)
                    checksEntries <- sort(as.numeric(data_entry_UP[1:checks,1]))
                    # diagonal_arrangement() takes the checks from the first rows
                    # and needs their ENTRY numbers to be consecutive
                    if (anyNA(checksEntries) || any(diff(checksEntries) != 1)) {
                        shinyalert::shinyalert(
                        "Error!!",
                        paste0("The checks, the first ", checks, " rows of the file, ",
                               "must have consecutive ENTRY numbers, for example 1, 2, 3, 4."),
                        type = "error")
                        return(NULL)
                    }
                    lines.db <- nrow(data_entry_UP) - length(checksEntries)
                    blocks <- as.numeric(as.vector(unlist(strsplit(input$blocks.db, ","))))
                    if(sum(blocks) != lines.db) {
                        shinyalert::shinyalert(
                        "Error!!", 
                        "Number of treatments in blocks does not match with the data input file.", 
                        type = "error")
                        return(NULL)
                    }
                    if (as.numeric(lines.db) < 50) {
                        shinyalert::shinyalert(
                        "Error!!", 
                        "Larger field size is recommended for this experiment type", 
                        type = "error")
                        return(NULL)
                    }
                    if (input$sameEntries && any(blocks != blocks[1])) {
                        shinyalert::shinyalert(
                        "Error!!",
                        "Blocks should have the same size",
                        type = "error")
                        return(NULL)
                    }
                    data_entry_UP$BLOCK <- c(rep("ALL", checks), rep(1:length(blocks), times = blocks))
                    ## ----- NEW ------------------
                    colnames(data_entry_UP) <- c("ENTRY", "NAME", "BLOCK")
                    if (Option_NCD == TRUE) {
                        data_entry1 <- data_entry_UP[(length(checksEntries) + 1):nrow(data_entry_UP), ]
                        Block_levels <- suppressWarnings(as.numeric(levels(as.factor(data_entry1$BLOCK))))
                        Block_levels <- na.omit(Block_levels)
                        data_dim_each_block <- numeric()
                        for (i in Block_levels){ 
                        data_dim_each_block[i] <- nrow(subset(data_entry_UP, data_entry_UP$BLOCK == i))
                        }
                        dim_data <- sum(data_dim_each_block)
                        input_blocks <- as.numeric(sort(Block_levels))
                        if (any(input_blocks < 1) || any(diff(input_blocks) != 1)) {
                        shinyalert::shinyalert(
                            "Error!!", 
                            "Data input does not fit the requirements!", 
                            type = "error")
                        return(NULL)
                        }
                        selected <- length(Block_levels)
                    }
                    dim_data_entry <- nrow(data_entry_UP)
                    choices_list <- field_dimensions(lines_within_loc = dim_data_entry)
                        if (length(choices_list) == 0) {
                            shinyalert::shinyalert(
                                "Error!!", 
                                "Insufficient number of entries provided!",
                                type = "error"
                            )
                            return(NULL)
                        }
                    dim_data_1 <- nrow(data_entry_UP[(length(checksEntries) + 1):nrow(data_entry_UP), ])
                    return(list(data_entry = data_entry_UP, 
                                dim_data_entry = dim_data_entry, 
                                dim_data_1 = dim_data_1,
                                data_api = data_entry_UP[, c("ENTRY", "NAME")],
                                lines = NULL,
                                same_entries = input$sameEntries))
                } else {
                  app_upload_error(data_ingested,
                                   missing_columns = "Data input needs at least two columns: ENTRY and NAME")
                  return(NULL)
                }
            } else {
                shiny::req(input$checks.db)
                shiny::req(input$blocks.db)
                shiny::req(input$lines.db)
                lines.db <- as.numeric(input$lines.db)
                choices_list <- field_dimensions(lines_within_loc = lines.db)
                if (length(choices_list) == 0) {
                    shinyalert::shinyalert(
                        "Error!!", 
                        "Insufficient number of entries provided!",
                        type = "error"
                    )
                    return(NULL)
                }
                checks <- as.numeric(input$checks.db)
                checksEntries <- 1:checks
                blocks <- as.numeric(as.vector(unlist(strsplit(input$blocks.db, ","))))
                if (lines.db != sum(blocks)) {
                    shinyalert::shinyalert(
                        "Error!!", 
                        "Number of treatments in blocks does match with the data input file.", 
                        type = "error")
                    return(NULL)
                }
                if (as.numeric(input$lines.db) < 50) {
                    shinyalert::shinyalert(
                        "Error!!", 
                        "Larger field size is recommended for this experiment type", 
                        type = "error")
                    return(NULL)
                }
                if (input$sameEntries && any(blocks != blocks[1])) {
                    shinyalert::shinyalert(
                        "Error!!", 
                        "Blocks should have the same size", 
                        type = "error")
                    return(NULL)
                }
                # The entries and names diagonal_arrangement() generates when
                # no data is given; with sameEntries every block holds the
                # entries numbered after the checks
                if (input$sameEntries) {
                    ENTRY <- c(checksEntries,
                               rep((checks + 1):(checks + blocks[1]), times = length(blocks)))
                } else {
                    ENTRY <- 1:(lines.db + checks)
                }
                NAME <- c(paste0(rep("Check-", checks), 1:checks),
                          paste0(rep("Gen-", lines.db), ENTRY[-(1:checks)]))
                data_entry_UP <- data.frame(ENTRY = ENTRY, NAME = NAME)
                data_entry_UP$BLOCK <- c(rep("ALL", checks), rep(1:length(blocks), times = blocks))
                colnames(data_entry_UP) <- c("ENTRY", "NAME", "BLOCK")
                if (Option_NCD == TRUE) {
                    data_entry1 <- data_entry_UP[(checks + 1):nrow(data_entry_UP), ]
                    Block_levels <- suppressWarnings(as.numeric(levels(as.factor(data_entry1$BLOCK))))
                    Block_levels <- na.omit(Block_levels)
                    data_dim_each_block <- numeric()
                    for (i in Block_levels){ 
                        data_dim_each_block[i] <- nrow(subset(data_entry_UP, data_entry_UP$BLOCK == i))
                    }
                    dim_data <- sum(data_dim_each_block)
                    selected <- length(Block_levels)
                }
                dim_data_entry <- nrow(data_entry_UP)
                dim_data_1 <- nrow(data_entry_UP[(length(checksEntries) + 1):nrow(data_entry_UP), ])
                return(list(data_entry = data_entry_UP, 
                            dim_data_entry = dim_data_entry, 
                            dim_data_1 = dim_data_1,
                            data_api = NULL,
                            lines = lines.db,
                            same_entries = input$sameEntries))
            }
            
        })
        
        getChecks <- shiny::eventReactive(input$RUN_multiple, {
            shiny::req(get_data_multiple()$data_entry)
            data <- as.data.frame(get_data_multiple()$data_entry)
            checksEntries <- sort(as.numeric(data[1:input$checks.db,1]))
            checks <- as.numeric(input$checks.db)
            list(checksEntries = checksEntries, checks = checks)
        })
        
        blocks_length <- shiny::eventReactive(input$RUN_multiple, {
            shiny::req(get_data_multiple()$data_entry)
            df <- get_data_multiple()$data_entry
            Block_levels <- suppressWarnings(as.numeric(levels(as.factor(df$BLOCK))))
            Block_levels <- na.omit(Block_levels)
            len_blocks <- length(Block_levels)
            return(len_blocks)
        })
        
        list_inputs_multiple <- shiny::eventReactive(input$RUN_multiple, {
            shiny::req(get_data_multiple()$dim_data_entry)
            checks <- as.numeric(getChecks()$checks)
            lines <- as.numeric(get_data_multiple()$dim_data_entry)
            return(list(lines, input$list_entries_multiple, kindExpt, 
                        input$stacked, input$RUN_multiple))
        })
        
        shiny::observeEvent(list_inputs_multiple(), {
            shiny::req(get_data_multiple()$dim_data_entry)
            checks <- as.numeric(getChecks()$checks)
            total_entries <- as.numeric(get_data_multiple()$dim_data_entry)
            lines <- total_entries - checks
            shiny::withProgress(message = 'Getting field dimensions ...', {
                sort_choices <- validate_design(diagonal_dimension_choices(
                    lines = lines, checks = as.vector(getChecks()$checksEntries),
                    kindExpt = kindExpt, stacked = multiple_inputs()$stacked,
                    planter = multiple_inputs()$planter_mov,
                    data = get_data_multiple()$data_entry
                ))
            })
            
            shiny::updateSelectInput(inputId = "dimensions_multiple",
                                choices = sort_choices,
                                selected = head(sort_choices, 1))
            if (length(sort_choices) == 0L) {
                shinyalert::shinyalert("No field dimensions available",
                                      "No feasible field was found for these entries and checks.",
                                      type = "error")
            }
        })
        
        shiny::observeEvent(input$RUN_multiple, {
            shiny::req(get_data_multiple()$dim_data_entry)
            shinyjs::show(id = "dimensions_multiple")
            shinyjs::show(id = "get_random_multi")
        })
        
        field_dimensions_diagonal <-  shiny::eventReactive(input$get_random_multi, {
            shiny::req(input$dimensions_multiple)
            dims <- unlist(strsplit(input$dimensions_multiple, " x "))
            d_row <- as.numeric(dims[1])
            d_col <- as.numeric(dims[2])
            return(list(d_row = d_row, d_col = d_col))
        })
        
        entryListFormat_DBUDC <- data.frame(
            ENTRY = 1:9, 
            NAME = c(c("CHECK1", "CHECK2","CHECK3"), paste("Genotype", LETTERS[1:6], sep = ""))
        )
        
        toListen <- shiny::reactive({
            list(input$list_entries_multiple)
        })
        
        entriesInfoModal_DBUDC <- function() {
        shiny::modalDialog(
            title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
            shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
            shiny::renderTable(entryListFormat_DBUDC,
                        bordered = TRUE,
                        align = 'c',
                        striped = TRUE),
            shiny::h4("Note that the controls must be in the first rows of the CSV file."),
            easyClose = FALSE
        )
        }
        
        shiny::observeEvent(toListen(), {
        if (input$list_entries_multiple == "Yes") {
            shiny::showModal(
              entriesInfoModal_DBUDC()
            )
        }
        })
        
        available_percent_multi <- shiny::eventReactive(input$get_random_multi, {
            shiny::req(input$dimensions_multiple)
            shiny::req(get_data_multiple())
            Option_NCD <- TRUE
            checksEntries <- as.vector(getChecks()$checksEntries)
            planter_multiple <- multiple_inputs()$planter_mov
            n_rows <- field_dimensions_diagonal()$d_row
            n_cols <- field_dimensions_diagonal()$d_col
            diagonal_check_options(
                n_rows = n_rows, 
                n_cols = n_cols, 
                checks = checksEntries, 
                Option_NCD = Option_NCD, 
                kindExpt = kindExpt, 
                stacked = multiple_inputs()$stacked, 
                planter_mov1 = planter_multiple, 
                data = get_data_multiple()$data_entry, 
                dim_data = get_data_multiple()$dim_data_entry,
                dim_data_1 = get_data_multiple()$dim_data_1, 
                Block_Fillers = blocks_length()
            )
        }) 
        
        shiny::observeEvent(available_percent_multi()$dt, {
            my_out <- available_percent_multi()$dt
            my_percent <- my_out[,2]
            len <- length(my_percent)
            selected <- my_percent[len]
            shiny::updateSelectInput(session = session,
                                inputId = 'percent_checks_multi', 
                                label = "Choose % of Checks:",
                                choices = my_percent, 
                                selected = selected)
        })
        
        shiny::observeEvent(list_to_observe_multi(), {
            if (randomize_hit_multi$times_multi > 0 & user_tries_multi$tries > 0) {
                shinyjs::show(id = "percent_checks_multi")
            } else {
                shinyjs::hide(id = "percent_checks_multi")
            }
        })
        
        shiny::observeEvent(list_to_observe_multi(), {
            output$download_multi <- shiny::renderUI({
                if (randomize_hit_multi$times_multi > 0 & user_tries_multi$tries > 0) {
                shiny::downloadButton(ns("download_fieldbook_multiple"),
                                "Save experiment (ZIP)",
                                style = "width:100%")
                }
            })
        })
        
        # The design comes from diagonal_arrangement(), so the app and the
        # R function give the same design for the same inputs and seed. Every
        # output below is derived from its result.
        diagonal_design <- shiny::reactive({
            shiny::req(get_data_multiple())
            shiny::req(field_dimensions_diagonal())
            shiny::req(available_percent_multi()$dt)
            shiny::req(multiple_inputs()$seed_number)
            # The selector keeps its previous value until the options of the
            # current field reach the browser; wait for one of them.
            percent <- as.numeric(input$percent_checks_multi)
            options_percent <- as.numeric(available_percent_multi()$dt[,2])
            shiny::req(shiny::isTruthy(percent), any(abs(options_percent - percent) < 1e-6))
            locs <- as.numeric(multiple_inputs()$sites)
            blocks <- as.numeric(multiple_inputs()$blocks)
            plotNumber <- multiple_inputs()$plotNumber
            if (length(plotNumber) == 0 || anyNA(plotNumber)) {
                shiny::validate("Plot starting number is missing.")
            }
            if (any(plotNumber %% 1 != 0)) {
                shiny::validate("plotNumber should be integers.")
            }
            # Same starting plots in every location: one start for all the
            # experiments or one per experiment. Any other number of starts
            # keeps the previous default of 1001.
            if (length(plotNumber) > 1 && length(plotNumber) != length(blocks)) {
                plotNumber <- seq(1001, 1000 * (locs + 1), 1000)
            }
            if (length(plotNumber) == length(blocks)) {
                plot_starts <- rep(list(plotNumber), locs)
            } else {
                plot_starts <- rep(plotNumber[1], locs)
            }
            location_names <- multiple_inputs()$location_names
            if (length(location_names) != locs) location_names <- NULL
            if (multiple_inputs()$stacked == "By Row") {
                split_by <- "row"
            } else split_by <- "column"
            design <- tryCatch(
                suppressWarnings(
                    diagonal_arrangement(
                        nrows = field_dimensions_diagonal()$d_row,
                        ncols = field_dimensions_diagonal()$d_col,
                        lines = get_data_multiple()$lines,
                        checks = as.numeric(getChecks()$checks),
                        planter = multiple_inputs()$planter_mov,
                        l = locs,
                        plotNumber = plot_starts,
                        kindExpt = kindExpt,
                        splitBy = split_by,
                        seed = as.numeric(multiple_inputs()$seed_number),
                        blocks = blocks,
                        exptName = multiple_inputs()$expt_name,
                        locationNames = location_names,
                        data = get_data_multiple()$data_api,
                        checksPercent = percent,
                        sameEntries = get_data_multiple()$same_entries
                    )
                ),
                error = function(e) e
            )
            if (inherits(design, "error")) {
                shinyalert::shinyalert(
                    "Error!!",
                    conditionMessage(design),
                    type = "error")
                return(NULL)
            }
            if (is.null(design)) {
                shinyalert::shinyalert(
                    "Error!!",
                    "Data input does not fit to field dimensions",
                    type = "error")
                return(NULL)
            }
            return(design)
        }) 
        
        user_location <- shiny::reactive({
            user_site <- as.numeric(input$locView_diagonal_db)
            shiny::req(user_site %in% seq_along(diagonal_design()$layoutRandom))
            return(list(user_site = user_site))
        })
        
        output$options_table_multi <- DT::renderDT({
            test <- randomize_hit_multi$times_multi > 0 & user_tries_multi$tries > 0
            if (!test) return(NULL)
            Option_NCD <- TRUE
            if (is.null(available_percent_multi()$dt)) {
                shiny::validate("Data input does not fit to field dimensions")
                return(NULL)
            }
            my_out <- available_percent_multi()$dt
            df <- as.data.frame(my_out)
            table_options <- list(pageLength = nrow(df), autoWidth = FALSE,
                                        scrollX = TRUE, scrollY = "460px")
            DT::datatable(
                df, rownames = FALSE, 
                caption = 'Reference guide to design your experiment. Choose the percentage (%)
            of checks based on the total number of plots you want to have in the final layout.', 
                options = utils::modifyList(table_options, list(
                columnDefs = list(list(className = 'dt-center', targets = "_all")))))
        })
        
        
        output$data_input <- DT::renderDT({
            test <- randomize_hit_multi$times_multi > 0 & user_tries_multi$tries > 0
            if (!test) return(NULL)
            shiny::req(diagonal_design())
            df <- diagonal_design()$data_entry[[1]]
            df$ENTRY <- as.factor(df$ENTRY)
            df$NAME <- as.factor(df$NAME)
            df$BLOCK <- as.factor(df$BLOCK)
            a <- ncol(df) - 1
            table_options <- list(pageLength = nrow(df), autoWidth = FALSE,
                                        scrollX = TRUE, scrollY = "600px")
            DT::datatable(
                df,
                filter = "top",
                rownames = FALSE, 
                caption = 'List of Entries.', 
                options = utils::modifyList(table_options, list(
                columnDefs = list(
                    list(className = 'dt-center', targets = "_all"))))
            )
        })
        
        output$checks_table <- DT::renderDT({
            test <- randomize_hit_multi$times_multi > 0 & user_tries_multi$tries > 0
            if (!test) return(NULL)
            shiny::req(diagonal_design())
            data_entry <- diagonal_design()$data_entry[[1]]
            table_type <- as.data.frame(table(data_entry$BLOCK))
            colnames(table_type) <- c("SUB-BLOCKS", "FREQUENCY")
            df <- table_type
            table_options <- list(pageLength = nrow(df), autoWidth = FALSE,
                                        scrollX = TRUE, scrollY = "350px")
            DT::datatable(df, rownames = FALSE, options = table_options)
        })
        
        output$randomized_layout <- DT::renderDT({
            test <- randomize_hit_multi$times_multi > 0 & user_tries_multi$tries > 0
            if (!test) return(NULL)
            shiny::req(diagonal_design())
            user_site <- user_location()$user_site
            r_map <- unname(diagonal_design()$layoutRandom[[user_site]])
            if (is.null(r_map))
                return(NULL)
            checks <- diagonal_design()$infoDesign$entry_checks[[user_site]]
            len_checks <- length(checks)
            colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                        'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
            df <- as.data.frame(r_map)
            rownames(df) <- nrow(df):1
            DT::datatable(
                df,
                extensions = 'Buttons',
                options = list(dom = 'Blfrtip',
                            autoWidth = FALSE,
                            scrollX = TRUE,
                            fixedColumns = TRUE,
                            pageLength = nrow(df),
                            scrollY = "600px",
                            class = 'compact cell-border stripe',  rownames = FALSE,
                            server = FALSE,
                            filter = list( position = 'top', clear = FALSE, plain =TRUE ),
                            buttons = app_table_export_buttons(diagonal_design(), "Entry layout", user_location()$user_site),
                            lengthMenu = list(c(10,25,50,-1),
                                                c(10,25,50,"All")))
            ) |> 
                DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                                backgroundColor = DT::styleEqual(c(checks),
                                                                colores[1:len_checks]))
        })
        
        # Experiment names by ROW and COLUMN from the field book, oriented as
        # layoutRandom (the first matrix row is the last field row)
        expt_layout_sites <- shiny::reactive({
            shiny::req(diagonal_design())
            validate_design(field_book_location_grids(
                diagonal_design()$fieldBook, "EXPT", reverse_rows = TRUE
            ))
        })
        
        output$name_layout <- DT::renderDT({
            test <- randomize_hit_multi$times_multi > 0 & user_tries_multi$tries > 0
            if (!test) return(NULL)
            shiny::req(expt_layout_sites())
            my_names <- expt_layout_sites()[[user_location()$user_site]]
            if (is.null(my_names)) return(NULL)
            blocks <- length(multiple_inputs()$blocks)
            Name_expt <- multiple_inputs()$expt_name
            if (length(Name_expt) == blocks) { 
                name_expt <- Name_expt 
            } else { 
                name_expt = paste0(rep("Block", times = blocks), 1:blocks) 
            } 
            colores_back <- c('snow', 'cadetblue', 'lightgreen', 'grey', 
                                'tan', 'lightcyan',
                                'violet', 'thistle') 
            df <- as.data.frame(my_names) 
            rownames(df) <- nrow(df):1
            DT::datatable(
                df,
                extensions = 'Buttons',
                options = list(dom = 'Blfrtip',
                                autoWidth = FALSE,
                                scrollX = TRUE,
                                fixedColumns = TRUE,
                                pageLength = nrow(df),
                                scrollY = "600px",
                                class = 'compact cell-border stripe', rownames = FALSE,
                                server = FALSE,
                                filter = list( position = 'top', clear = FALSE, plain =TRUE ),
                                buttons = app_table_export_buttons(diagonal_design(), "Experiment layout", user_location()$user_site),
                                lengthMenu = list(c(10,25,50,-1),
                                                    c(10,25,50,"All")))
            ) |> 
                DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                                backgroundColor = DT::styleEqual(name_expt, 
                                                                colores_back[1:blocks])
                ) 
        })
        
        output$plot_number_layout <- DT::renderDT({
            test <- randomize_hit_multi$times_multi > 0 & user_tries_multi$tries > 0
            if (!test) return(NULL)
            shiny::req(diagonal_design())
            plot_num <- unname(diagonal_design()$plotsNumber[[user_location()$user_site]])
            if (is.null(plot_num))
                return(NULL)
            df <- as.data.frame(plot_num)
            rownames(df) <- nrow(df):1
            DT::datatable(
                df,
                extensions = c('Buttons'),
                options = list(
                    dom = 'Blfrtip',
                    autoWidth = FALSE,
                    scrollX = TRUE,
                    fixedColumns = TRUE,
                    pageLength = nrow(df),
                    scrollY = "700px",
                    class = 'compact cell-border stripe',  rownames = FALSE,
                    server = FALSE,
                    filter = list( position = 'top', clear = FALSE, plain =TRUE ),
                    buttons = app_table_export_buttons(diagonal_design(), "Plot numbers", user_location()$user_site),
                    lengthMenu = list(c(10,25,50,-1),
                                        c(10,25,50,"All"))
                )
            )
        })
        
        simulation_settings <- app_simulation_controls(input, session,
          ids = c(trait = "trailsDIAG", other = "OtherDIAG", minimum = "min.diag", maximum = "max.diag", submit = "ok_simu_multi"),
          field_book = function() diagonal_design()$fieldBook,
          correlation_ids = c(x = "ROX.DIAG", y = "ROY.DIAG")
        )
        
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
                    shiny::actionButton(inputId = ns("ok_simu_multi"), "GO")
                )
            )
        }
        
        shiny::observeEvent(input$simulate_multiple, {
            shiny::req(diagonal_design()$fieldBook)
            shiny::showModal(
              simuModal_DIAG()
            )
        })
        
        
        simudata_DIAG <- shiny::reactive({
            shiny::req(diagonal_design()$fieldBook)
            field_book <- diagonal_design()$fieldBook
            if (is.null(simulation_settings())) {
              return(list(df = field_book, simulation = NULL))
            }
            simulation <- validate_design(simulate_spatial_field_book(
                field_book = field_book,
                nrows = diagonal_design()$infoDesign$rows,
                ncols = diagonal_design()$infoDesign$columns,
                correlation_x = as.numeric(simulation_settings()$correlation_x),
                correlation_y = as.numeric(simulation_settings()$correlation_y),
                min_value = as.numeric(simulation_settings()$min_value),
                max_value = as.numeric(simulation_settings()$max_value),
                response_name = as.character(simulation_settings()$response_name),
                seed = as.numeric(multiple_inputs()$seed_number)
            ))
            list(df = simulation$field_book, dfSimulationList = simulation$simulations,
                 simulation = simulation)
        })

        heat_map <- shiny::reactiveValues(heat_map_option = FALSE)
        
        shiny::observeEvent(simulation_settings(), {
          heat_map$heat_map_option <- TRUE
        })
        
        shiny::observeEvent(heat_map$heat_map_option, {
        if (heat_map$heat_map_option == FALSE) {
            shiny::hideTab(inputId = "tabset_multi", target = "Heatmap")
        } else {
            shiny::showTab(inputId = "tabset_multi", target = "Heatmap")
        }
        })
        
        output$fieldBook_diagonal <- DT::renderDT({
            test <- randomize_hit_multi$times_multi > 0 & user_tries_multi$tries > 0
            if (!test) return(NULL)
            shiny::req(simudata_DIAG()$df)
            df <- simudata_DIAG()$df
            validate_design(app_field_book_table(
              df, factor_columns = c("EXPT", "LOCATION", "PLOT", "ROW", "COLUMN", "CHECKS", "ENTRY", "TREATMENT"),
              height = 600
            ))
        })
        
        heatmap_obj_D <- shiny::reactive({
          shiny::req(simudata_DIAG()$dfSimulationList)
          validate_design(app_spatial_heatmap(
            simudata_DIAG()$dfSimulationList,
            response_name = as.character(simulation_settings()$response_name),
            selected = user_location()$user_site, height = 700
          ))
        })
        
        output$heatmap_diag <- plotly::renderPlotly({
            test <- randomize_hit_multi$times_multi > 0 & user_tries_multi$tries > 0
            if (!test) return(NULL)
            shiny::req(heatmap_obj_D())
            heatmap_obj_D()
        })
        
        output$download_fieldbook_multiple <- app_csv_archive(
            filename = function() {
                shiny::req(multiple_inputs()$location_names)
                loc <- multiple_inputs()$location_names
                loc <- paste(loc, "_", "Diagonal_Multi", sep = "")
                paste(loc, Sys.Date(), ".csv", sep = "")
            },
          data = function() as.data.frame(simudata_DIAG()$df),
          design = diagonal_design,
          field_book = function() simudata_DIAG()$df,
          simulation = function() simudata_DIAG()$simulation,
          kind = "field_book"
        )
        app_reproduction_outputs(output, diagonal_design)
    })
}
