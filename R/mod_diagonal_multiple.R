#' diagonal_multiple UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom utils write.csv
mod_diagonal_multiple_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Unreplicated Multiple Diagonal Arrangement"),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        radioButtons(inputId = ns("list_entries_multiple"), 
                     label = "Import entries' list?", 
                     choices = c("Yes", "No"), 
                     selected = "No",
                     inline = TRUE, 
                     width = NULL, 
                     choiceNames = NULL, 
                     choiceValues = NULL),
        checkboxInput(inputId = ns("sameEntries"), 
                      label = "Repeat entries across experiments", 
                      value = FALSE),
        conditionalPanel(
          condition = "input.list_entries_multiple == 'Yes'", 
          ns = ns,
          fluidRow(
            column(7, style=list("padding-right: 28px;"),
                   fileInput(ns("file_multiple"), 
                             label = "Upload a CSV File:", 
                             multiple = FALSE)),
            column(5,style=list("padding-left: 5px;"),
                   radioButtons(ns("sep.DIAGONALS"), "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","))
          )              
        ),
        conditionalPanel(
          condition = "input.list_entries_multiple == 'No'", 
          ns = ns,
          numericInput(inputId = ns("lines.db"), 
                      label = "Input # of Entries:",
                      value = 300, 
                      min = 50)
        ),
        textInput(ns("blocks.db"), 
                  "Input # Entries per Expt:",
                  value = "100,120,80"),
        
        selectInput(inputId = ns("checks.db"),
                    label = "Input # of Checks:",
                    choices = c(1:20),
                    multiple = FALSE, 
                    selected = 4), 
        fluidRow(
          column(6,style=list("padding-right: 28px;"),
                 numericInput(inputId = ns("locs_db"), 
                              label = "Input # of Locations:", 
                              value = 1,
                              min = 1)
          ),
          column(6,style=list("padding-left: 5px;"),
                 selectInput(inputId = ns("locView_diagonal_db"), 
                             label = "Choose location to view:", 
                             choices = 1, 
                             selected = 1, 
                             multiple = FALSE)
          )
        ),
        fluidRow(
          column(6,style=list("padding-right: 28px;"),
                 selectInput(inputId = ns("stacked"), 
                             label = "Blocks Layout:",
                             choices = c("By Column", "By Row"), 
                             multiple = FALSE,
                             selected = "By Row")
          ),
          column(6,style=list("padding-left: 5px;"),
                 selectInput(inputId = ns("planter_multiple"), 
                             label = "Plot Order Layout:",
                             choices = c("serpentine", "cartesian"), 
                             multiple = FALSE,
                             selected = "serpentine")
          )
        ),
        fluidRow(
          column(6,
                 style=list("padding-right: 28px;"),
                  textInput(ns("plot_start_multiple"), 
                            "Starting Plot Number:", 
                            value = 1)
          ),
          column(6,
                 style=list("padding-left: 5px;"),
                 textInput(ns("expt_name_multiple"), 
                           "Input Experiment Name:", 
                           value = c("Expt1, Expt2, Expt3"))
          )
        ),    
        fluidRow(
          column(6,
                 style=list("padding-right: 28px;"),
                  numericInput(inputId = ns("seed_multiple"), 
                              label = "Random Seed:", 
                              value = 17, 
                              min = 1)
          ),
          column(6,
                 style=list("padding-left: 5px;"),
                 textInput(ns("location_multiple"), 
                           "Input the Location:",
                           value = "FARGO")
          )
        ),
        fluidRow(
          column(6,
                 actionButton(
                   inputId = ns("RUN_multiple"), 
                   "Run!", 
                   icon = icon("circle-nodes", verify_fa = FALSE),
                   width = '100%'),
          ),
          column(6,
                 actionButton(
                   ns("simulate_multiple"),
                   "Simulate!",
                   icon = icon("greater-than-equal", verify_fa = FALSE),
                   width = '100%')
          )
        ),
        br(),
        uiOutput(ns("download_multi"))        
      ),
      mainPanel(
        width = 8,
        shinyjs::useShinyjs(),
        tabsetPanel(id = ns("tabset_multi"),
                    tabPanel(title = "Expt Design Info", value = "tabPanel1",
                             br(),
                             shinyjs::hidden(
                               selectInput(inputId = ns("dimensions_multiple"),
                                           label = "Select dimensions of field:", 
                                           choices = "", width = '400px')
                             ),
                             shinyjs::hidden(
                               actionButton(inputId = ns("get_random_multi"), 
                                            label = "Randomize!")
                             ),
                             br(),
                             br(),
                             DT::DTOutput(ns("options_table_multi"))
                    ),
                    tabPanel("Input Data",
                             fluidRow(
                               column(6,DT::DTOutput(ns("data_input"))),
                               column(6,DT::DTOutput(ns("checks_table")))
                             )
                    ),
                    tabPanel("Randomized Field",
                             br(),
                             shinyjs::hidden(
                               selectInput(inputId = ns("percent_checks_multi"),
                                           label = "Choose % of Checks:",
                                           choices = 1:9, width = '400px')
                             ),
                             DT::DTOutput(ns("randomized_layout"))),
                    tabPanel("Expt Layout", DT::DTOutput(ns("name_layout"))),
                    tabPanel("Plot Number Field", DT::DTOutput(ns("plot_number_layout"))),
                    tabPanel("Field Book", DT::DTOutput(ns("fieldBook_diagonal"))),
                    tabPanel("Heatmap", 
                             shinycssloaders::withSpinner(
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
    moduleServer( id, function(input, output, session) {
        ns <- session$ns
        
        counts_multi <- reactiveValues(trigger_multi = 0)
        
        observeEvent(input$RUN_multiple, {
            counts_multi$trigger_multi <- counts_multi$trigger_multi + 1
        })
        
        kindExpt = "DBUDC"
        
        randomize_hit_multi <- reactiveValues(times_multi = 0)
        
        observeEvent(input$RUN_multiple, {
            randomize_hit_multi$times_multi <- 0
        })
        
        user_tries_multi <- reactiveValues(tries = 1)
        
        observeEvent(input$get_random_multi, {
            randomize_hit_multi$times_multi <- randomize_hit_multi$times_multi + 1
            user_tries_multi$tries <- user_tries_multi$tries + 1
        })
        
        observeEvent(input$dimensions_multiple, {
        user_tries_multi$tries <- 0
        })
        
        list_to_observe_multi <- reactive({
            list(randomize_hit_multi$times_multi, user_tries_multi$tries)
        })
        
        shinyjs::useShinyjs()
        
        multiple_inputs <- eventReactive(input$RUN_multiple, {
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
            plotNumber <- as.numeric(as.vector(unlist(strsplit(input$plot_start_multiple, ","))))
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
        
        observeEvent(multiple_inputs()$sites, {
            loc_user_view <- 1:as.numeric(input$locs_db)
            updateSelectInput(inputId = "locView_diagonal_db",  
                                choices = loc_user_view, 
                                selected = loc_user_view[1])
        })
        
        observeEvent(kindExpt,
                    handlerExpr = updateTabsetPanel(session,
                                                    "tabset_multi",
                                                    selected = "tabPanel1"))
        observeEvent(input$stacked,
                    handlerExpr = updateTabsetPanel(session,
                                                    "tabset_multi",
                                                    selected = "tabPanel1"))
        observeEvent(input$checks.db,
                    handlerExpr = updateTabsetPanel(session,
                                                    "tabset_multi",
                                                    selected = "tabPanel1"))
        observeEvent(input$dimensions_multiple,
                    handlerExpr = updateTabsetPanel(session,
                                                    "tabset_multi",
                                                    selected = "tabPanel1"))
        observeEvent(input$planter_multiple,
                    handlerExpr = updateTabsetPanel(session,
                                                    "tabset_multi",
                                                    selected = "tabPanel1"))
        observeEvent(input$lines.db,
                    handlerExpr = updateTabsetPanel(session,
                                                    "tabset_multi",
                                                    selected = "tabPanel1"))
        observeEvent(input$locs_db,
                    handlerExpr = updateTabsetPanel(session,
                                                    "tabset_multi",
                                                    selected = "tabPanel1"))
        observeEvent(input$list_entries_multiple,
                    handlerExpr = updateTabsetPanel(session,
                                                    "tabset_multi",
                                                    selected = "tabPanel1"))
        observeEvent(input$RUN_multiple,
                    handlerExpr = updateTabsetPanel(session,
                                                    "tabset_multi",
                                                    selected = "tabPanel1"))
        
        
        get_data_multiple <- eventReactive(input$RUN_multiple, {
            Option_NCD <- TRUE
            if (input$list_entries_multiple == "Yes") {
                checking_entry_list = TRUE
                if (input$sameEntries) {
                    checking_entry_list = FALSE
                }
                req(input$checks.db)
                req(input$file_multiple)
                inFile <- input$file_multiple
                data_ingested <- load_file(
                    name = inFile$name, 
                    path = inFile$datapat, 
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
                        "Data input needs at least two columns: ENTRY and NAME",
                        type = "error")
                    return(NULL)
                    }
            } else {
                req(input$checks.db)
                req(input$blocks.db)
                req(input$lines.db)
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
        
        getChecks <- eventReactive(input$RUN_multiple, {
            req(get_data_multiple()$data_entry)
            data <- as.data.frame(get_data_multiple()$data_entry)
            checksEntries <- sort(as.numeric(data[1:input$checks.db,1]))
            checks <- as.numeric(input$checks.db)
            list(checksEntries = checksEntries, checks = checks)
        })
        
        blocks_length <- eventReactive(input$RUN_multiple, {
            req(get_data_multiple()$data_entry)
            df <- get_data_multiple()$data_entry
            Block_levels <- suppressWarnings(as.numeric(levels(as.factor(df$BLOCK))))
            Block_levels <- na.omit(Block_levels)
            len_blocks <- length(Block_levels)
            return(len_blocks)
        })
        
        list_inputs_multiple <- eventReactive(input$RUN_multiple, {
            req(get_data_multiple()$dim_data_entry)
            checks <- as.numeric(getChecks()$checks)
            lines <- as.numeric(get_data_multiple()$dim_data_entry)
            return(list(lines, input$list_entries_multiple, kindExpt, 
                        input$stacked, input$RUN_multiple))
        })
        
        observeEvent(list_inputs_multiple(), {
            req(get_data_multiple()$dim_data_entry)
            checks <- as.numeric(getChecks()$checks)
            total_entries <- as.numeric(get_data_multiple()$dim_data_entry)
            lines <- total_entries - checks
            t1 <- floor(lines + lines * 0.10)
            t2 <- ceiling(lines + lines * 0.20)
            t <- t1:t2
            withProgress(message = 'Getting field dimensions ...', {
                choices_list <- list()
                i <- 1
                for (n in t) {
                    choices_list[[i]] <- factor_subsets(n, diagonal = TRUE)$labels
                    i <- i + 1
                }
                choices <- unlist(choices_list[!sapply(choices_list, is.null)])
                if (is.null(choices)) {
                    choices <- "No options available"
                } 
                Option_NCD <- TRUE
                checksEntries <- as.vector(getChecks()$checksEntries)
                new_choices <- list()
                v <- 1
                by_choices <- 1:length(choices)
                for (dim_options in by_choices) {
                    planter_multiple <- multiple_inputs()$planter_mov
                    dims <- unlist(strsplit(choices[[dim_options]], " x "))
                    n_rows <- as.numeric(dims[1])
                    n_cols  <- as.numeric(dims[2])
                    dt_options <- available_percent(
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
                    if (!is.null(dt_options$dt)) {
                        new_choices[[v]] <- choices[[dim_options]]
                        v <- v + 1
                    }
                }
                dif <- vector(mode = "numeric", length = length(new_choices))
                for (option in 1:length(new_choices)) {
                    dims <- unlist(strsplit(new_choices[[option]], " x "))
                    dif[option] <- abs(as.numeric(dims[1]) - as.numeric(dims[2]))
                }
                df_choices <- data.frame(choices = unlist(new_choices), diff_dim = dif)
                df_choices <- df_choices[order(df_choices$diff_dim, decreasing = FALSE), ]
                sort_choices <- as.vector(df_choices$choices)
            })
            
            updateSelectInput(inputId = "dimensions_multiple",
                                choices = sort_choices,
                                selected = sort_choices[1])
        })
        
        observeEvent(input$RUN_multiple, {
            req(get_data_multiple()$dim_data_entry)
            shinyjs::show(id = "dimensions_multiple")
            shinyjs::show(id = "get_random_multi")
        })
        
        field_dimensions_diagonal <-  eventReactive(input$get_random_multi, {
            req(input$dimensions_multiple)
            dims <- unlist(strsplit(input$dimensions_multiple, " x "))
            d_row <- as.numeric(dims[1])
            d_col <- as.numeric(dims[2])
            return(list(d_row = d_row, d_col = d_col))
        })
        
        entryListFormat_DBUDC <- data.frame(
            ENTRY = 1:9, 
            NAME = c(c("CHECK1", "CHECK2","CHECK3"), paste("Genotype", LETTERS[1:6], sep = ""))
        )
        
        toListen <- reactive({
            list(input$list_entries_multiple)
        })
        
        entriesInfoModal_DBUDC <- function() {
        modalDialog(
            title = div(tags$h3("Important message", style = "color: red;")),
            h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
            renderTable(entryListFormat_DBUDC,
                        bordered = TRUE,
                        align = 'c',
                        striped = TRUE),
            h4("Note that the controls must be in the first rows of the CSV file."),
            easyClose = FALSE
        )
        }
        
        observeEvent(toListen(), {
        if (input$list_entries_multiple == "Yes") {
            showModal(
              entriesInfoModal_DBUDC()
            )
        }
        })
        
        available_percent_multi <- eventReactive(input$get_random_multi, {
            req(input$dimensions_multiple)
            req(get_data_multiple())
            Option_NCD <- TRUE
            checksEntries <- as.vector(getChecks()$checksEntries)
            planter_multiple <- multiple_inputs()$planter_mov
            n_rows <- field_dimensions_diagonal()$d_row
            n_cols <- field_dimensions_diagonal()$d_col
            available_percent(
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
        
        observeEvent(available_percent_multi()$dt, {
            my_out <- available_percent_multi()$dt
            my_percent <- my_out[,2]
            len <- length(my_percent)
            selected <- my_percent[len]
            updateSelectInput(session = session, 
                                inputId = 'percent_checks_multi', 
                                label = "Choose % of Checks:",
                                choices = my_percent, 
                                selected = selected)
        })
        
        observeEvent(list_to_observe_multi(), {
            if (randomize_hit_multi$times_multi > 0 & user_tries_multi$tries > 0) {
                shinyjs::show(id = "percent_checks_multi")
            } else {
                shinyjs::hide(id = "percent_checks_multi")
            }
        })
        
        observeEvent(list_to_observe_multi(), {
            output$download_multi <- renderUI({
                if (randomize_hit_multi$times_multi > 0 & user_tries_multi$tries > 0) {
                downloadButton(ns("download_fieldbook_multiple"),
                                "Save Experiment",
                                style = "width:100%")
                }
            })
        })
        
        # The design comes from diagonal_arrangement(), so the app and the
        # R function give the same design for the same inputs and seed. Every
        # output below is derived from its result.
        diagonal_design <- reactive({
            req(get_data_multiple())
            req(field_dimensions_diagonal())
            req(available_percent_multi()$dt)
            req(multiple_inputs()$seed_number)
            # The selector keeps its previous value until the options of the
            # current field reach the browser; wait for one of them.
            percent <- as.numeric(input$percent_checks_multi)
            options_percent <- as.numeric(available_percent_multi()$dt[,2])
            req(isTruthy(percent), any(abs(options_percent - percent) < 1e-6))
            locs <- as.numeric(multiple_inputs()$sites)
            blocks <- as.numeric(multiple_inputs()$blocks)
            plotNumber <- multiple_inputs()$plotNumber
            if (length(plotNumber) == 0 || anyNA(plotNumber)) {
                validate("Plot starting number is missing.")
            }
            if (any(plotNumber %% 1 != 0)) {
                validate("plotNumber should be integers.")
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
        
        user_location <- reactive({
            user_site <- as.numeric(input$locView_diagonal_db)
            req(user_site %in% seq_along(diagonal_design()$layoutRandom))
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
            options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                        scrollX = TRUE, scrollY = "460px"))
            DT::datatable(
                df, rownames = FALSE, 
                caption = 'Reference guide to design your experiment. Choose the percentage (%)
            of checks based on the total number of plots you want to have in the final layout.', 
                options = list(
                columnDefs = list(list(className = 'dt-center', targets = "_all"))))
        })
        
        
        output$data_input <- DT::renderDT({
            test <- randomize_hit_multi$times_multi > 0 & user_tries_multi$tries > 0
            if (!test) return(NULL)
            req(diagonal_design())
            df <- diagonal_design()$data_entry[[1]]
            df$ENTRY <- as.factor(df$ENTRY)
            df$NAME <- as.factor(df$NAME)
            df$BLOCK <- as.factor(df$BLOCK)
            a <- ncol(df) - 1
            options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                        scrollX = TRUE, scrollY = "600px"))
            DT::datatable(
                df,
                filter = "top",
                rownames = FALSE, 
                caption = 'List of Entries.', 
                options = list(
                columnDefs = list(
                    list(className = 'dt-center', targets = "_all")))
            )
        })
        
        output$checks_table <- DT::renderDT({
            test <- randomize_hit_multi$times_multi > 0 & user_tries_multi$tries > 0
            if (!test) return(NULL)
            req(diagonal_design())
            data_entry <- diagonal_design()$data_entry[[1]]
            table_type <- as.data.frame(table(data_entry$BLOCK))
            colnames(table_type) <- c("SUB-BLOCKS", "FREQUENCY")
            df <- table_type
            options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                        scrollX = TRUE, scrollY = "350px"))
            DT::datatable(df, rownames = FALSE)
        })
        
        output$randomized_layout <- DT::renderDT({
            test <- randomize_hit_multi$times_multi > 0 & user_tries_multi$tries > 0
            if (!test) return(NULL)
            req(diagonal_design())
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
                            buttons = c('copy', 'excel'),
                            lengthMenu = list(c(10,25,50,-1),
                                                c(10,25,50,"All")))
            ) |> 
                DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                                backgroundColor = DT::styleEqual(c(checks),
                                                                colores[1:len_checks]))
        })
        
        # Experiment names by ROW and COLUMN from the field book, oriented as
        # layoutRandom (the first matrix row is the last field row)
        expt_layout_sites <- reactive({
            req(diagonal_design())
            fieldBook <- diagonal_design()$fieldBook
            n_rows <- diagonal_design()$infoDesign$rows
            n_cols <- diagonal_design()$infoDesign$columns
            n_plots <- n_rows * n_cols
            locs <- diagonal_design()$infoDesign$locations
            expt_layouts <- vector(mode = "list", length = locs)
            for (sites in 1:locs) {
                fieldBook_site <- fieldBook[((sites - 1) * n_plots + 1):(sites * n_plots), ]
                my_names <- matrix(data = NA, nrow = n_rows, ncol = n_cols)
                my_names[cbind(n_rows - fieldBook_site$ROW + 1, fieldBook_site$COLUMN)] <- fieldBook_site$EXPT
                expt_layouts[[sites]] <- my_names
            }
            return(expt_layouts)
        })
        
        output$name_layout <- DT::renderDT({
            test <- randomize_hit_multi$times_multi > 0 & user_tries_multi$tries > 0
            if (!test) return(NULL)
            req(expt_layout_sites())
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
                                buttons = c('copy', 'excel'),
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
            req(diagonal_design())
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
                    buttons = c('copy', 'excel'),
                    lengthMenu = list(c(10,25,50,-1),
                                        c(10,25,50,"All"))
                )
            )
        })
        
        valsDIAG <- reactiveValues(ROX = NULL, ROY = NULL, trail = NULL, minValue = NULL,
                                maxValue = NULL)
        
        simuModal_DIAG <- function(failed = FALSE) {
            modalDialog(
                fluidRow(
                column(6, 
                        selectInput(inputId = ns("trailsDIAG"), label = "Select One:", 
                                    choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
                )
                ),
                conditionalPanel("input.trailsDIAG == 'Other'", ns = ns,
                                textInput(inputId = ns("OtherDIAG"), label = "Input Trial Name:", value = NULL)
                ),
                fluidRow(
                column(6, 
                        selectInput(inputId = ns("ROX.DIAG"), "Select the Correlation in Rows:", 
                                    choices = seq(0.1, 0.9, 0.1), selected = 0.5)
                ),
                column(6, 
                        selectInput(inputId = ns("ROY.DIAG"), "Select the Correlation in Cols:", 
                                    choices = seq(0.1, 0.9, 0.1), selected = 0.5)
                )
                ),
                fluidRow(
                column(6, 
                        numericInput(inputId = ns("min.diag"), "Input the min value:", value = NULL)
                ),
                column(6, 
                        numericInput(inputId = ns("max.diag"), "Input the max value:", value = NULL)
                        
                )
                ),
                if (failed)
                div(tags$b("Invalid input of data max and min", style = "color: red;")),
                
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton(inputId = ns("ok_simu_multi"), "GO")
                )
            )
        }
        
        observeEvent(input$simulate_multiple, {
            req(diagonal_design()$fieldBook)
            showModal(
              simuModal_DIAG()
            )
        })
        
        observeEvent(input$ok_simu_multi, {
            req(input$min.diag, input$max.diag)
            if (input$max.diag > input$min.diag && input$min.diag != input$max.diag) {
                valsDIAG$maxValue <- input$max.diag
                valsDIAG$minValue  <- input$min.diag
                valsDIAG$ROX <- as.numeric(input$ROX.DIAG)
                valsDIAG$ROY <- as.numeric(input$ROY.DIAG)
                if (input$trailsDIAG == "Other") {
                    req(input$OtherDIAG)
                    if (!is.null(input$OtherDIAG)) {
                        valsDIAG$trail <- as.character(input$OtherDIAG)
                    } else showModal(simuModal_DIAG(failed = TRUE))
                } else {
                    valsDIAG$trail <- as.character(input$trailsDIAG)
                }
                removeModal()
            } else {
                showModal(
                  simuModal_DIAG(failed = TRUE)
                )
            }
        })
        
        simudata_DIAG <- reactive({
            req(diagonal_design()$fieldBook)
            if (!is.null(valsDIAG$maxValue) && 
            !is.null(valsDIAG$minValue) && !is.null(valsDIAG$trail)) {
                maxVal <- as.numeric(valsDIAG$maxValue)
                minVal <- as.numeric(valsDIAG$minValue)
                ROX_DIAG <- as.numeric(valsDIAG$ROX)
                ROY_DIAG <- as.numeric(valsDIAG$ROY)
                df_diag <- diagonal_design()$fieldBook
                loc_levels_factors <- levels(factor(df_diag$LOCATION, unique(df_diag$LOCATION)))
                nrows_diag <- diagonal_design()$infoDesign$rows
                ncols_diag <- diagonal_design()$infoDesign$columns
                seed_diag <- as.numeric(multiple_inputs()$seed_number)
                locs_diag <- as.numeric(multiple_inputs()$sites)
                df_diag_list <- vector(mode = "list", length = locs_diag)
                df_simulation_list <- vector(mode = "list", length = locs_diag)
                w <- 1
                set.seed(seed_diag)
                for (sites in 1:locs_diag) {
                    df_loc <- subset(df_diag, LOCATION == loc_levels_factors[w])
                    fieldBook <- df_loc[, c(1,6,7,9)]
                    dfSimulation <- AR1xAR1_simulation(nrows = nrows_diag, ncols = ncols_diag, 
                                                        ROX = ROX_DIAG, ROY = ROY_DIAG, 
                                                        minValue = minVal, maxValue = maxVal, 
                                                        fieldbook = fieldBook, 
                                                        trail = valsDIAG$trail, 
                                                        seed = NULL)
                    dfSimulation <- dfSimulation$outOrder
                    df_simulation_list[[sites]] <- dfSimulation
                    dataPrep <- df_loc
                    df_DIAG <- cbind(dataPrep, round(dfSimulation[,7],2))
                    colnames(df_DIAG)[11] <- as.character(valsDIAG$trail)
                    df_diag_list[[sites]] <- df_DIAG
                    w <- w + 1
                }
                df_diag_locs <- dplyr::bind_rows(df_diag_list)
                v <- 1
            } else {
                df_DIAG <- diagonal_design()$fieldBook
                v <- 2
            }
            if (v == 1) {
                return(list(df = df_diag_locs, dfSimulationList = df_simulation_list))
            }else if (v == 2) {
                return(list(df = df_DIAG))
            }
        })
        
        heat_map <- reactiveValues(heat_map_option = FALSE)
        
        observeEvent(input$ok_simu_multi, {
            req(input$min.diag, input$max.diag)
            if (input$max.diag > input$min.diag && input$min.diag != input$max.diag) {
                heat_map$heat_map_option <- TRUE
            }
        })
        
        observeEvent(heat_map$heat_map_option, {
        if (heat_map$heat_map_option == FALSE) {
            hideTab(inputId = "tabset_multi", target = "Heatmap")
        } else {
            showTab(inputId = "tabset_multi", target = "Heatmap")
        }
        })
        
        output$fieldBook_diagonal <- DT::renderDT({
            test <- randomize_hit_multi$times_multi > 0 & user_tries_multi$tries > 0
            if (!test) return(NULL)
            req(simudata_DIAG()$df)
            df <- simudata_DIAG()$df
            df$EXPT <- as.factor(df$EXPT)
            df$LOCATION <- as.factor(df$LOCATION)
            df$PLOT <- as.factor(df$PLOT)
            df$ROW <- as.factor(df$ROW)
            df$COLUMN <- as.factor(df$COLUMN)
            df$CHECKS <- as.factor(df$CHECKS)
            df$ENTRY <- as.factor(df$ENTRY)
            df$TREATMENT <- as.factor(df$TREATMENT)
            options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                        scrollX = TRUE, scrollY = "600px"))
            DT::datatable(
                df,
                filter = "top",
                rownames = FALSE, 
                options = list(
                columnDefs = list(list(className = 'dt-center', targets = "_all")))
            )
        })
        
        heatmap_obj_D <- reactive({
            req(simudata_DIAG()$dfSimulation)
            loc_user <- user_location()$user_site
            w <- as.character(valsDIAG$trail)
            df <- simudata_DIAG()$dfSimulationList[[loc_user]]
            p1 <- ggplot2::ggplot(df, ggplot2::aes(x = df[,4], y = df[,3], fill = df[,7], text = df[,8])) + 
                ggplot2::geom_tile() +
                ggplot2::xlab("COLUMN") +
                ggplot2::ylab("ROW") +
                ggplot2::labs(fill = w) +
                viridis::scale_fill_viridis(discrete = FALSE)
            
            p2 <- plotly::ggplotly(p1, tooltip="text", height = 700)
            
            return(p2)
        })
        
        output$heatmap_diag <- plotly::renderPlotly({
            test <- randomize_hit_multi$times_multi > 0 & user_tries_multi$tries > 0
            if (!test) return(NULL)
            req(heatmap_obj_D())
            heatmap_obj_D()
        })
        
        output$download_fieldbook_multiple <- downloadHandler(
            filename = function() {
                req(multiple_inputs()$location_names)
                loc <- multiple_inputs()$location_names
                loc <- paste(loc, "_", "Diagonal_Multi", sep = "")
                paste(loc, Sys.Date(), ".csv", sep = "")
            },
            content = function(file) {
                write.csv(simudata_DIAG()$df, file, row.names = FALSE)
            }
        )
    })
}
