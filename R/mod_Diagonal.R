#' Diagonal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom utils write.csv
mod_Diagonal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Unreplicated Single Diagonal Arrangement"),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        radioButtons(inputId = ns("owndataDIAGONALS"),
                     label = "Import entries' list?",
                     choices = c("Yes", "No"), 
                     selected = "No",
                     inline = TRUE,
                     width = NULL,
                     choiceNames = NULL,
                     choiceValues = NULL),
        conditionalPanel(
          condition = "input.owndataDIAGONALS == 'Yes'", 
          ns = ns,
          fluidRow(
            column(7, style=list("padding-right: 28px;"),
                   fileInput(ns("file1"), 
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
          condition = "input.owndataDIAGONALS == 'No'", 
          ns = ns,
          numericInput(inputId = ns("lines.d"), 
                       label = "Input # of Entries:",
                       value = 287, 
                       min = 50),
        ),
        selectInput(inputId = ns("checks"),
                    label = "Input # of Checks:",
                    choices = c(1:10),
                    multiple = FALSE,
                    selected = 4),
        fluidRow(
          column(6,style=list("padding-right: 28px;"),
                 numericInput(inputId = ns("l.diagonal"), 
                              label = "Input # of Locations:", 
                              value = 1,
                              min = 1)
          ),
          column(6,style=list("padding-left: 5px;"),
                 selectInput(inputId = ns("locView.diagonal"), 
                             label = "Choose location to view:", 
                             choices = 1, 
                             selected = 1, 
                             multiple = FALSE)
          )
        ),
        selectInput(inputId = ns("planter_single"), 
                    label = "Plot Order Layout:",
                    choices = c("serpentine", "cartesian"), 
                    multiple = FALSE,
                    selected = "serpentine"),
        fluidRow(
            column(
                width = 6,
                style=list("padding-right: 28px;"),
                textInput(
                    ns("plot_start"), 
                    "Starting Plot Number:", 
                    value = 1
                )
            ),
            column(6,
                    style=list("padding-left: 5px;"),
                    textInput(ns("expt_name"), 
                            "Input Experiment Name:", 
                            value = "Expt1")
            )
        ),    
        fluidRow(
            column(6,
                    style=list("padding-right: 28px;"),
                    numericInput(
                        inputId = ns("seed_single"), 
                        label = "Random Seed:", 
                        value = 17, 
                        min = 1)
            ),
            column(6,
                    style=list("padding-left: 5px;"),
                    textInput(ns("Location"), 
                            "Input the Location:",
                            value = "FARGO")
            )
        ),
        fluidRow(
          column(6,
                 actionButton(
                   inputId = ns("RUN.diagonal"), 
                   "Run!", 
                   icon = icon("circle-nodes", verify_fa = FALSE),
                   width = '100%'),
          ),
          column(6,
                 actionButton(
                   ns("Simulate_Diagonal"),
                   "Simulate!",
                   icon = icon("greater-than-equal", verify_fa = FALSE),
                   width = '100%')
          )
        ),
        br(),
        uiOutput(ns("download_single"))
      ),
      mainPanel(
        width = 8,
        shinyjs::useShinyjs(),
        tabsetPanel(id = ns("tabset_single"),
                    tabPanel(title = "Expt Design Info", value = "tabPanel1",
                             br(),
                             shinyjs::hidden(
                                selectInput(inputId = ns("dimensions.d"),
                                            label = "Select dimensions of field:", 
                                            choices = "", width = '400px')
                             ),
                             shinyjs::hidden(
                               actionButton(inputId = ns("get_random"), 
                                            label = "Randomize!")
                             ),
                             br(),
                             br(),
                             #uiOutput(ns("checks_percent")),
                             DT::DTOutput(ns("options_table"))
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
                               selectInput(inputId = ns("percent_checks"),
                                           label = "Choose % of Checks:",
                                           choices = 1:9, width = '400px')
                             ),
                             DT::DTOutput(ns("randomized_layout"))),
                    tabPanel("Plot Number Field", 
                             DT::DTOutput(ns("plot_number_layout"))),
                    tabPanel("Field Book", 
                             DT::DTOutput(ns("fieldBook_diagonal"))),
                    tabPanel("Heatmap", shinycssloaders::withSpinner(
                      plotly::plotlyOutput(ns("heatmap_diag"),  width = "97%"), 
                      type = 5)
                    )
        )      
      )
    )
  )
}

#' Diagonal Server Functions
#'
#' @noRd 
mod_Diagonal_server <- function(id) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns

    counts <- reactiveValues(trigger = 0)
    
    observeEvent(input$RUN.diagonal, {
      counts$trigger <- counts$trigger + 1
    })
    
    kindExpt_single <- "SUDC"

    randomize_hit <- reactiveValues(times = 0)
 
    observeEvent(input$RUN.diagonal, {
      randomize_hit$times <- 0
    })

    user_tries <- reactiveValues(tries = 0)

    observeEvent(input$get_random, {
      randomize_hit$times <- randomize_hit$times + 1
      user_tries$tries <- user_tries$tries + 1
    })

    observeEvent(input$dimensions.d, {
      user_tries$tries <- 0
    })

    list_to_observe <- reactive({
      list(randomize_hit$times, user_tries$tries)
    })

    shinyjs::useShinyjs()
    
    single_inputs <- eventReactive(input$RUN.diagonal, {
      planter_mov <- input$planter_single
      Name_expt <- as.vector(unlist(strsplit(input$expt_name, ",")))
      blocks <- 1
      if (length(Name_expt) == blocks & !missing(Name_expt)) {
        name_expt <- Name_expt
      }else{
        name_expt = paste0(rep("Block", times = blocks), 1:blocks)
      }
      plotNumber <- as.numeric(as.vector(unlist(strsplit(input$plot_start, ","))))
      seed_number <- as.numeric(input$seed_single)
      location_names <- as.vector(unlist(strsplit(input$Location, ",")))
      sites = as.numeric(input$l.diagonal)
      return(list(sites = sites, 
                  location_names = location_names, 
                  seed_number = seed_number, 
                  plotNumber = plotNumber,
                  planter_mov = planter_mov,
                  expt_name = name_expt))
    })
    
    observeEvent(single_inputs()$sites, {
      loc_user_view <- 1:as.numeric(input$l.diagonal)
      updateSelectInput(inputId = "locView.diagonal", 
                        choices = loc_user_view, 
                        selected = loc_user_view[1])
    })
    
    observeEvent(kindExpt_single,
                 handlerExpr = updateTabsetPanel(session,
                                                 "tabset_single",
                                                 selected = "tabPanel1"))
    observeEvent(input$stacked,
                 handlerExpr = updateTabsetPanel(session,
                                                 "tabset_single",
                                                 selected = "tabPanel1"))
    observeEvent(input$checks,
                 handlerExpr = updateTabsetPanel(session,
                                                 "tabset_single",
                                                 selected = "tabPanel1"))
    observeEvent(input$dimensions.d,
                 handlerExpr = updateTabsetPanel(session,
                                                 "tabset_single",
                                                 selected = "tabPanel1"))
    observeEvent(single_inputs()$planter_mov,
                 handlerExpr = updateTabsetPanel(session,
                                                 "tabset_single",
                                                 selected = "tabPanel1"))
    observeEvent(input$lines.d,
                 handlerExpr = updateTabsetPanel(session,
                                                 "tabset_single",
                                                 selected = "tabPanel1"))
    observeEvent(input$l.diagonal,
                 handlerExpr = updateTabsetPanel(session,
                                                 "tabset_single",
                                                 selected = "tabPanel1"))
    observeEvent(input$owndataDIAGONALS,
                 handlerExpr = updateTabsetPanel(session,
                                                 "tabset_single",
                                                 selected = "tabPanel1"))
    observeEvent(input$RUN.diagonal,
                 handlerExpr = updateTabsetPanel(session,
                                                 "tabset_single",
                                                 selected = "tabPanel1"))
                                                  
    getData <- eventReactive(input$RUN.diagonal, {
        Option_NCD <- TRUE
        if (input$owndataDIAGONALS == "Yes") {
            req(input$file1)
            inFile <- input$file1
            data_ingested <- load_file(
                name = inFile$name, 
                path = inFile$datapat, 
                sep = input$sep.DIAGONALS, 
                check = TRUE, 
                design = "sdiag"
            )
            if (names(data_ingested) == "dataUp") {
                data_up <- data_ingested$dataUp
                if (ncol(data_up) < 2) {
                    validate("Data input needs at least two Columns with the ENTRY and NAME.")
                } 
                data_entry_UP <- na.omit(data_up[,1:2])
                colnames(data_entry_UP) <- c("ENTRY", "NAME")
                checks <- as.numeric(input$checks)
                checksEntries <- suppressWarnings(
                    sort(as.numeric(data_entry_UP[1:checks,1]), na.last = TRUE)
                )
                # diagonal_arrangement() takes the checks from the first rows
                # of the list and requires them to be consecutive numbers
                if (any(is.na(checksEntries)) || any(diff(checksEntries) != 1)) {
                    shinyalert::shinyalert(
                        "Error!!",
                        paste0("The check entries (the ENTRY of the first ", checks,
                               " rows of the file) must be consecutive numbers, ",
                               "such as 1, 2, 3, 4."),
                        type = "error"
                    )
                    return(NULL)
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
                            dim_without_checks = dim_data_1,
                            uploaded = TRUE))
            } else if (names(data_ingested) == "bad_format") {
            shinyalert::shinyalert(
                "Error!!", 
                "Invalid file; Please upload a .csv file.", 
                type = "error")
            error_message <- "Invalid file; Please upload a .csv file."
            return(NULL)
            } else if (names(data_ingested) == "duplicated_vals") {
            shinyalert::shinyalert(
                "Error!!", 
                "Check input file for duplicate values.", 
                type = "error")
            error_message <- "Check input file for duplicate values."
            return(NULL)
            } else if (names(data_ingested) == "missing_cols") {
            shinyalert::shinyalert(
                "Error!!", 
                "Data input needs at least two columns: ENTRY and NAME",
                type = "error")
            return(NULL)
            }
        } else {
            req(input$lines.d)
            req(input$checks)
            checks <- as.numeric(input$checks)
            checksEntries <- 1:checks
            lines <- input$lines.d
            choices_list <- field_dimensions(lines_within_loc = lines)
            if (length(choices_list) == 0) {
                shinyalert::shinyalert(
                    "Error!!", 
                    "Insufficient number of entries provided!",
                    type = "error"
                )
                return(NULL)
            }
            # Same entry list as diagonal_arrangement() generates from lines
            NAME <- c(paste0(rep("Check-", checks), 1:checks),
                    paste0(rep("Gen-", lines), (checks + 1):(lines + checks)))
            gen.list <- data.frame(list(ENTRY = 1:(lines + checks),	NAME = NAME))
            data_entry_UP <- gen.list
            colnames(data_entry_UP) <- c("ENTRY", "NAME")
            dim_data_entry <- nrow(data_entry_UP)
            dim_data_1 <- nrow(data_entry_UP[(length(checksEntries) + 1):nrow(data_entry_UP), ])
            return(list(data_entry = data_entry_UP, 
                    dim_data_entry = dim_data_entry, 
                    dim_without_checks = dim_data_1,
                    lines = lines,
                    uploaded = FALSE))
        }
    })
    
    getChecks <- eventReactive(input$RUN.diagonal, {
      req(getData()$data_entry)
      data <- as.data.frame(getData()$data_entry)
      checksEntries <- sort(as.numeric(data[1:input$checks,1]))
      checks <- as.numeric(input$checks)
      list(checksEntries = checksEntries, checks = checks)
    })
    
    list_inputs_diagonal <- eventReactive(input$RUN.diagonal, {
      req(getData()$dim_data_entry)
      checks <- as.numeric(getChecks()$checks)
      lines <- as.numeric(getData()$dim_data_entry)
      return(list(lines, input$owndataDIAGONALS, kindExpt_single, 
                  input$stacked, input$RUN.diagonal))
    })

    observeEvent(list_inputs_diagonal(), {
      req(getData()$dim_data_entry)
      checks <- as.numeric(getChecks()$checks)
      total_entries <- as.numeric(getData()$dim_data_entry)
      lines <- total_entries - checks
      t1 <- floor(lines + lines * 0.11)
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
        if(is.null(choices)) {
          choices <- "No options available"
        } 
        Option_NCD <- TRUE
        checksEntries <- as.vector(getChecks()$checksEntries)
        new_choices <- list()
        v <- 1
        by_choices <- 1:length(choices)
        for (dim_options in by_choices) {
          
          planter_mov <- single_inputs()$planter_mov
          
          dims <- unlist(strsplit(choices[[dim_options]], " x "))
          n_rows <- as.numeric(dims[1])
          n_cols  <- as.numeric(dims[2])
          
          dt_options <- available_percent(
            n_rows = n_rows,
            n_cols = n_cols,
            checks = checksEntries,
            Option_NCD = Option_NCD,
            kindExpt = kindExpt_single,
            stacked = input$stacked,
            planter_mov1 = planter_mov,
            data = getData()$data_entry,
            dim_data = getData()$dim_data_entry,
            dim_data_1 = getData()$dim_without_checks,
            Block_Fillers = NULL
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
      updateSelectInput(inputId = "dimensions.d",
                        choices = sort_choices,
                        selected = sort_choices[1])
    })
    
    observeEvent(input$RUN.diagonal, {
      req(getData()$dim_data_entry)
      shinyjs::show(id = "dimensions.d")
      shinyjs::show(id = "get_random")
    })
    
    field_dimensions_diagonal <- eventReactive(input$get_random, {
      req(input$dimensions.d)
      dims <- unlist(strsplit(input$dimensions.d, " x "))
      d_row <- as.numeric(dims[1])
      d_col <- as.numeric(dims[2])
      return(list(d_row = d_row, d_col = d_col))
    })

    entryListFormat_SUDC <- data.frame(
      ENTRY = 1:9, 
      NAME = c(c("CHECK1", "CHECK2","CHECK3"), paste("Genotype", LETTERS[1:6], 
                                                     sep = ""))
    )
    
    toListen <- reactive({
      list(input$owndataDIAGONALS,kindExpt_single)
    })
    
    entriesInfoModal_SUDC <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        renderTable(entryListFormat_SUDC,
                    bordered = TRUE,
                    align  = 'c',
                    striped = TRUE),
        h4("Note that the controls must be in the first rows of the CSV file."),
        easyClose = FALSE
      )
    }

    observeEvent(toListen(), {
      if (input$owndataDIAGONALS == "Yes" && kindExpt_single == "SUDC") {
        showModal(
          entriesInfoModal_SUDC()
        )
      }
    })

    available_percent_table <- eventReactive(input$get_random, {
      req(input$dimensions.d)
      req(getData())
      req(field_dimensions_diagonal())
      Option_NCD <- TRUE
      checksEntries <- as.vector(getChecks()$checksEntries)
      planter_mov <- single_inputs()$planter_mov
      n_rows <- field_dimensions_diagonal()$d_row
      n_cols <- field_dimensions_diagonal()$d_col
      available_percent(
          n_rows = n_rows, 
          n_cols = n_cols, 
          checks = checksEntries, 
          Option_NCD = Option_NCD, 
          kindExpt = kindExpt_single, 
          stacked = input$stacked, 
          planter_mov1 = planter_mov,
          data = getData()$data_entry, 
          dim_data = getData()$dim_data_entry,
          dim_data_1 = getData()$dim_without_checks, 
          Block_Fillers = NULL
      )
    }) 

    # Percentage of checks passed to diagonal_arrangement(). Each Randomize
    # starts from the last option, the API default
    checks_percent <- reactiveVal(NULL)

    observeEvent(available_percent_table()$dt, {
          my_out <- available_percent_table()$dt
          my_percent <- my_out[,2]
          len <- length(my_percent)
          selected <- my_percent[len]
          checks_percent(as.numeric(selected))

          updateSelectInput(session = session, 
                            inputId = 'percent_checks', 
                            label = "Choose % of Checks:",
                            choices = my_percent, 
                            selected = selected)
    }, priority = 10)

    observeEvent(input$percent_checks, {
      req(available_percent_table()$dt)
      options_percent <- as.numeric(available_percent_table()$dt[,2])
      percent <- suppressWarnings(as.numeric(input$percent_checks))
      match_percent <- which(abs(options_percent - percent) < 1e-6)
      # Ignore a value left from the placeholder or from other field dimensions
      req(length(match_percent) > 0)
      checks_percent(options_percent[match_percent[1]])
    }, ignoreInit = TRUE)
    
    observeEvent(list_to_observe(), {
      if (randomize_hit$times > 0 & user_tries$tries > 0) {
        shinyjs::show(id = "percent_checks")
      } else {
        shinyjs::hide(id = "percent_checks")
      }
    })

    observeEvent(list_to_observe(), { #  user_tries$tries
      output$download_single <- renderUI({
        if (randomize_hit$times > 0 & user_tries$tries > 0) {
          downloadButton(ns("downloadData_Diagonal"),
                          "Save Experiment",
                          style = "width:100%")
        }
      })
    })

    # Arguments of diagonal_arrangement(), taken when Randomize is clicked
    diagonal_inputs <- eventReactive(input$get_random, {
      req(getData())
      req(field_dimensions_diagonal())
      req(single_inputs()$seed_number)
      sites <- single_inputs()$sites
      plot_starts <- single_inputs()$plotNumber
      if (any(is.na(plot_starts))) {
        shinyalert::shinyalert(
          "Error!!",
          "The starting plot numbers must be integers separated by commas.",
          type = "error"
        )
        return(NULL)
      }
      # One starting plot per location, otherwise 1001, 2001, ...
      if (length(plot_starts) != sites) {
        if (sites > 1) {
          plot_starts <- seq(1001, 1000*(sites+1), 1000)
        } else plot_starts <- 1001
      }
      # One name per location, otherwise the locations are numbered
      location_names <- single_inputs()$location_names
      if (length(location_names) != sites) location_names <- NULL
      args <- list(
        nrows = field_dimensions_diagonal()$d_row,
        ncols = field_dimensions_diagonal()$d_col,
        checks = as.numeric(getChecks()$checks),
        planter = single_inputs()$planter_mov,
        l = sites,
        plotNumber = plot_starts,
        kindExpt = kindExpt_single,
        seed = as.numeric(single_inputs()$seed_number),
        exptName = single_inputs()$expt_name,
        locationNames = location_names
      )
      if (isTRUE(getData()$uploaded)) {
        args$data <- getData()$data_entry
      } else {
        args$lines <- as.numeric(getData()$lines)
      }
      return(args)
    })

    # The design comes from diagonal_arrangement(), so the app and the R
    # function give the same design for the same inputs and seed
    diagonal_design <- reactive({
      req(diagonal_inputs())
      req(available_percent_table()$dt)
      percent <- checks_percent()
      req(percent)
      options_percent <- as.numeric(available_percent_table()$dt[,2])
      req(any(abs(options_percent - percent) < 1e-6))
      args <- c(diagonal_inputs(), list(checksPercent = percent))
      design <- NULL
      # diagonal_arrangement() prints a message and returns NULL when the
      # field dimensions do not fit the entries
      console_out <- utils::capture.output(
        design <- tryCatch(
          suppressWarnings(do.call(diagonal_arrangement, args)),
          error = function(e) e
        )
      )
      if (inherits(design, "error")) {
        shinyalert::shinyalert(
          "Error!!",
          conditionMessage(design),
          type = "error"
        )
        return(NULL)
      }
      if (is.null(design)) {
        shinyalert::shinyalert(
          "Error!!",
          "The field dimensions do not fit the entries. Please, choose other dimensions.",
          type = "error"
        )
        return(NULL)
      }
      return(design)
    })
    
    user_location <- reactive({
      req(diagonal_design())
      user_site <- as.numeric(input$locView.diagonal)
      req(user_site >= 1, user_site <= length(diagonal_design()$layoutRandom))
      return(user_site)
    })
    
    output$options_table <- DT::renderDT({
      test <- randomize_hit$times > 0 & user_tries$tries > 0
      if (!test) return(NULL)
        Option_NCD <- TRUE
        if (is.null(available_percent_table()$dt)) {
          shiny::validate("Data input does not fit to field dimensions")
          return(NULL)
        }
        my_out <- available_percent_table()$dt
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
      test <- randomize_hit$times > 0 & user_tries$tries > 0
      if (!test) return(NULL)
        req(diagonal_design())
        df <- diagonal_design()$data_entry[[user_location()]]
        df$ENTRY <- as.factor(df$ENTRY)
        df$NAME <- as.factor(df$NAME)
        options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                  scrollX = TRUE, scrollY = "600px"))

        DT::datatable(df,
                      filter = "top",
                      rownames = FALSE, 
                      caption = 'List of Entries.', 
                      options = list(
                        columnDefs = list(
                          list(className = 'dt-center', targets = "_all")))
        )
    })
    
    output$checks_table <- DT::renderDT({
      test <- randomize_hit$times > 0 & user_tries$tries > 0
      if (!test) return(NULL)
        req(diagonal_design())
        user_site <- user_location()
        info_design <- diagonal_design()$infoDesign
        data_entry <- diagonal_design()$data_entry[[user_site]]
        entry_checks <- info_design$entry_checks[[user_site]]
        df <- data.frame(
          ENTRY = entry_checks,
          NAME = data_entry$NAME[match(entry_checks, data_entry$ENTRY)],
          TIMES = info_design$rep_checks[[user_site]]
        )
        options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                  scrollX = TRUE, scrollY = "350px"))
        DT::datatable(df, rownames = FALSE, caption = 'Table of Checks.', 
                      options = list(
                        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    })
    
    output$randomized_layout <- DT::renderDT({
      test <- randomize_hit$times > 0 & user_tries$tries > 0
      if (!test) return(NULL)
      req(diagonal_design())
      user_site <- user_location()
      r_map <- diagonal_design()$layoutRandom[[user_site]]
      if (is.null(r_map))
        return(NULL)
      checks <- diagonal_design()$infoDesign$entry_checks[[user_site]]
      len_checks <- length(checks)
      df <- as.data.frame(unname(r_map))
      colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                    'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
      rownames(df) <- nrow(df):1
      DT::datatable(
        df,
        extensions = c('Buttons'),
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
                        backgroundColor = DT::styleEqual(c(checks),
                                                          colores[1:len_checks]))
    })
    
    output$plot_number_layout <- DT::renderDT({
      test <- randomize_hit$times > 0 & user_tries$tries > 0
      if (!test) return(NULL)
      req(diagonal_design())
      plot_num <- diagonal_design()$plotsNumber[[user_location()]]
      if (is.null(plot_num))
        return(NULL)
      df <- as.data.frame(unname(plot_num))
      rownames(df) <- nrow(df):1
      DT::datatable(
        df,
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
          actionButton(inputId = ns("ok_simu_single"), "GO")
        )
      )
    }
    
    observeEvent(input$Simulate_Diagonal, {
      req(diagonal_design()$fieldBook)
      showModal(
        simuModal_DIAG()
      )
    })
    
    observeEvent(input$ok_simu_single, {
      req(input$min.diag, input$max.diag)
      if (input$max.diag > input$min.diag && input$min.diag != input$max.diag) {
        valsDIAG$maxValue <- input$max.diag
        valsDIAG$minValue  <- input$min.diag
        valsDIAG$ROX <- as.numeric(input$ROX.DIAG)
        valsDIAG$ROY <- as.numeric(input$ROY.DIAG)
        if(input$trailsDIAG == "Other") {
          req(input$OtherDIAG)
          if(!is.null(input$OtherDIAG)) {
            valsDIAG$trail <- as.character(input$OtherDIAG)
          }else showModal(simuModal_DIAG(failed = TRUE))
        }else {
          valsDIAG$trail <- as.character(input$trailsDIAG)
        }
        removeModal()
      }else {
        showModal(
          simuModal_DIAG(failed = TRUE)
        )
      }
    })
    
    simudata_DIAG <- reactive({
      req(diagonal_design()$fieldBook)
      if(!is.null(valsDIAG$maxValue) && !is.null(valsDIAG$minValue) && !is.null(valsDIAG$trail)) {
        maxVal <- as.numeric(valsDIAG$maxValue)
        minVal <- as.numeric(valsDIAG$minValue)
        ROX_DIAG <- as.numeric(valsDIAG$ROX)
        ROY_DIAG <- as.numeric(valsDIAG$ROY)
        df_diag <- diagonal_design()$fieldBook
        loc_levels_factors <- levels(factor(df_diag$LOCATION, unique(df_diag$LOCATION)))
        nrows_diag <- diagonal_design()$infoDesign$rows
        ncols_diag <- diagonal_design()$infoDesign$columns
        seed_diag <- as.numeric(diagonal_design()$infoDesign$seed)
        locs_diag <- as.numeric(diagonal_design()$infoDesign$locations)
        df_diag_list <- vector(mode = "list", length = locs_diag)
        df_simulation_list <- vector(mode = "list", length = locs_diag)
        w <- 1
        set.seed(seed_diag)
        for (sites in 1:locs_diag) {
          df_loc <- subset(df_diag, LOCATION == loc_levels_factors[w])
          fieldBook <- df_loc[, c(1,6,7,9)]
          dfSimulation <- AR1xAR1_simulation(
            nrows = nrows_diag, ncols = ncols_diag, 
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
      }else {
        df_DIAG <- diagonal_design()$fieldBook
        v <- 2
      }
      if (v == 1) {
        return(list(df = df_diag_locs, dfSimulationList = df_simulation_list))
      } else if (v == 2) {
        return(list(df = df_DIAG))
      }
    })
    
    heat_map <- reactiveValues(heat_map_option = FALSE)
    
    observeEvent(input$ok_simu_single, {
      req(input$min.diag, input$max.diag)
      if (input$max.diag > input$min.diag && input$min.diag != input$max.diag) {
        heat_map$heat_map_option <- TRUE
      }
    })
    
    observeEvent(heat_map$heat_map_option, {
      if (heat_map$heat_map_option == FALSE) {
        hideTab(inputId = "tabset_single", target = "Heatmap")
      } else {
        showTab(inputId = "tabset_single", target = "Heatmap")
      }
    })

    output$fieldBook_diagonal <- DT::renderDT({
      test <- randomize_hit$times > 0 & user_tries$tries > 0
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
      DT::datatable(df,
                    filter = "top",
                    rownames = FALSE, 
                    options = list(
                      columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    })
    
    
    heatmap_obj_D <- reactive({
      req(simudata_DIAG()$dfSimulationList)
      loc_user <- user_location()
      w <- as.character(valsDIAG$trail)
      df <- simudata_DIAG()$dfSimulationList[[loc_user]]
      p1 <- ggplot2::ggplot(df, ggplot2::aes(x = df[,4], y = df[,3], fill = df[,7], text = df[,8])) + 
        ggplot2::geom_tile() +
        ggplot2::xlab("COLUMN") +
        ggplot2::ylab("ROW") +
        ggplot2::labs(fill = w) +
        viridis::scale_fill_viridis(discrete = FALSE)
      
      p2 <- plotly::ggplotly(p1, tooltip="text", height = 720)
      
      return(p2)
    })
    
    output$heatmap_diag <- plotly::renderPlotly({
      test <- randomize_hit$times > 0 & user_tries$tries > 0
      if (!test) return(NULL)
      req(heatmap_obj_D())
      heatmap_obj_D()
    })
    
    output$downloadData_Diagonal <- downloadHandler(
      filename = function() {
        req(input$Location)
        loc <- input$Location
        loc <- paste(loc, "_", "Diagonal_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(simudata_DIAG()$df, file, row.names = FALSE)
        
      }
    )
  })
}
