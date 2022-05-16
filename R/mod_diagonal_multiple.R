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
        conditionalPanel(
          condition = "input.list_entries_multiple == 'No'", 
          ns = ns,
          checkboxInput(inputId = ns("sameEntries"), 
                        label = "Use the same entries across experiments!", 
                        value = FALSE)
        ),
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
        
        fluidRow(
          column(6,style=list("padding-right: 28px;"),
                 numericInput(inputId = ns("lines.db"), 
                              label = "Input # of Entries:",
                              value = 270, 
                              min = 50)
          ),
          column(6,style=list("padding-left: 5px;"),
                 textInput(ns("blocks.db"), 
                           "Input # Entries per Expt:",
                           value = "100,100,70")
          )
        ),
        selectInput(inputId = ns("checks.db"),
                    label = "Input # of Checks:",
                    choices = c(1:10),
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
                 numericInput(inputId = ns("seed_multiple"), 
                              label = "Seed Number:", 
                              value = 17, 
                              min = 1)
          ),
          column(6,
                 style=list("padding-left: 5px;"),
                 textInput(ns("expt_name_multiple"), 
                           "Input Experiment Name:", 
                           value = "Expt1")
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
                 textInput(ns("location_multiple"), 
                           "Input the Location:",
                           value = "FARGO")
          )
        ),
        fluidRow(
          column(6,
                 actionButton(inputId = ns("RUN_multiple"), 
                              "Run!", 
                              icon = icon("cocktail"), 
                              width = '100%'),
          ),
          column(6,
                 actionButton(ns("simulate_multiple"),
                              "Simulate!",
                              icon = icon("cocktail"),
                              width = '100%')
          )
        ),
        br(),
        downloadButton(ns("download_fieldbook_multiple"),
                       "Save Experiment",
                       style = "width:100%")
      ),
      mainPanel(
        width = 8,
        shinyjs::useShinyjs(),
        tabsetPanel(id = ns("Tabset"),
                    tabPanel(title = "Expt Design Info", value = "tabPanel1",
                             shinyjs::hidden(
                               selectInput(inputId = ns("dimensions_multiple"),
                                           label = "Select dimensions of field:", 
                                           choices = "", width = '400px')
                             ),
                             shinyjs::hidden(
                               selectInput(inputId = ns("percent_checks_multi"),
                                           label = "Choose % of Checks:", 
                                           choices = "", width = '400px')
                             ),
                             DT::DTOutput(ns("options_table"))
                    ),
                    tabPanel("Input Data",
                             fluidRow(
                               column(6,DT::DTOutput(ns("data_input"))),
                               column(6,DT::DTOutput(ns("checks_table")))
                             )
                    ),
                    tabPanel("Randomized Field", 
                             DT::DTOutput(ns("randomized_layout"))),
                    tabPanel("Plot Number Field", DT::DTOutput(ns("plot_number_layout"))),
                    tabPanel("Expt Layout", DT::DTOutput(ns("name_layout"))),
                    tabPanel("Field Book", DT::DTOutput(ns("fieldBook_diagonal"))),
                    tabPanel("Heatmap", shinycssloaders::withSpinner(
                      plotly::plotlyOutput(ns("heatmap_diag")), 
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
mod_diagonal_multiple_server <- function(id) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
    kindExpt = "DBUDC"

    reactive(print(kindExpt))
    
    
    # observeEvent(kindExpt, {
    #   if (kindExpt != "DBUDC") {
    #     hideTab(inputId = "Tabset", target = "Expt Layout")
    #   } else {
    #     showTab(inputId = "Tabset", target = "Expt Layout")
    #   }
    # })
    
    shinyjs::useShinyjs()
    
    loc_inputs <- eventReactive(input$RUN_multiple, {
      return(list(sites = input$locs_db))
    })
    
    observeEvent(loc_inputs()$sites, {
      loc_user_view <- 1:as.numeric(input$locs_db)
      updateSelectInput(inputId = "locView_diagonal_db",  
                        choices = loc_user_view, 
                        selected = loc_user_view[1])
    })
    observeEvent(kindExpt,
                 handlerExpr = updateTabsetPanel(session,
                                                 "Tabset",
                                                 selected = "tabPanel1"))
    observeEvent(input$stacked,
                 handlerExpr = updateTabsetPanel(session,
                                                 "Tabset",
                                                 selected = "tabPanel1"))
    observeEvent(input$checks.db,
                 handlerExpr = updateTabsetPanel(session,
                                                 "Tabset",
                                                 selected = "tabPanel1"))
    observeEvent(input$dimensions_multiple,
                 handlerExpr = updateTabsetPanel(session,
                                                 "Tabset",
                                                 selected = "tabPanel1"))
    observeEvent(input$planter_multiple,
                 handlerExpr = updateTabsetPanel(session,
                                                 "Tabset",
                                                 selected = "tabPanel1"))
    observeEvent(input$lines.d,
                 handlerExpr = updateTabsetPanel(session,
                                                 "Tabset",
                                                 selected = "tabPanel1"))
    observeEvent(input$locs_db,
                 handlerExpr = updateTabsetPanel(session,
                                                 "Tabset",
                                                 selected = "tabPanel1"))
    observeEvent(input$list_entries_multiple,
                 handlerExpr = updateTabsetPanel(session,
                                                 "Tabset",
                                                 selected = "tabPanel1"))
    observeEvent(input$RUN_multiple,
                 handlerExpr = updateTabsetPanel(session,
                                                 "Tabset",
                                                 selected = "tabPanel1"))
    
    get_data_multiple <- eventReactive(input$RUN_multiple, {
      #Sys.sleep(2)
      Option_NCD <- TRUE
      if (input$list_entries_multiple == "Yes") {
        req(input$file_multiple)
        inFile <- input$file_multiple
        data_entry <- load_file(name = inFile$name, 
                                path = inFile$datapat, 
                                sep = input$sep.DIAGONALS)
        data_entry <- na.omit(data_entry)
        if (ncol(data_entry) < 3) {
          validate("Data input needs at least three Columns with the ENTRY, NAME and BLOCK.")
        } 
        data_entry_UP <- data_entry[,1:3] 
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
            validate("Data input does not fit the requirements!")
          }
          selected <- length(Block_levels)
        }
       # }
      } else {
         #if (kindExpt == "DBUDC") {
          req(input$checks.db)
          req(input$blocks.db)
          req(input$lines.db)
          lines.db <- as.numeric(input$lines.db)
          checks <- as.numeric(input$checks.db)
          checksEntries <- 1:checks
          NAME <- c(paste(rep("CH", checks), 1:checks, sep = ""),
                    paste(rep("G", lines.db), (checks + 1):(lines.db + checks), sep = ""))
          data_entry_UP <- data.frame(
            list(ENTRY = 1:(lines.db + checks),	NAME = NAME)
          )
          blocks <- as.numeric(as.vector(unlist(strsplit(input$blocks.db, ","))))
          if (lines.db != sum(blocks)) shiny::validate('Sum of blocks may be equal to number of lines.')
          if (as.numeric(input$lines.db)<50) shiny::validate('Larger field size is recommended for this experiment type')
          data_entry_UP$BLOCK <- c(rep("ALL", checks), rep(1:length(blocks), times = blocks))
          colnames(data_entry_UP) <- c("ENTRY", "NAME", "BLOCK")
          if (input$sameEntries) {
            if (any(blocks != blocks[1])) shiny::validate("Blocks should have the same size")
            # Names
            ChecksNames <- paste(rep("CH", checks), 1:checks, sep = "")
            nameLines <- rep(c(paste(rep("G", blocks[1]), (2 + 1):(blocks[1] + 2), sep = "")), times = length(blocks))
            NAMES <- c(ChecksNames, nameLines)
            # Entries
            ChecksENTRIS <- 1:checks
            nameEntries <- rep((checks + 1):(blocks[1] + checks), times = length(blocks))
            ENTRIES <- c(ChecksENTRIS, nameEntries)
            data_entry_UP$NAME <- NAMES
            data_entry_UP$ENTRY <- ENTRIES
          }
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
        #}
      }
      dim_data_entry <- nrow(data_entry_UP)
      dim_data_1 <- nrow(data_entry_UP[(length(checksEntries) + 1):nrow(data_entry_UP), ])
      list(data_entry = data_entry_UP, 
           dim_data_entry = dim_data_entry, 
           dim_data_1 = dim_data_1)
    })
    
    getChecks <- eventReactive(input$RUN_multiple, {
      req(get_data_multiple()$data_entry)
      print(get_data_multiple()$data_entry)
      data <- as.data.frame(get_data_multiple()$data_entry)
      checksEntries <- as.numeric(data[1:input$checks.db,1])
      checks <- as.numeric(input$checks.db)
      list(checksEntries = checksEntries, checks = checks)
    })
    
    blocks_length <- eventReactive(input$RUN_multiple, {
      req(get_data_multiple()$data_entry)
      if (kindExpt == "DBUDC") {
        df <- get_data_multiple()$data_entry
        Block_levels <- suppressWarnings(as.numeric(levels(as.factor(df$BLOCK))))
        Block_levels <- na.omit(Block_levels)
        len_blocks <- length(Block_levels)
        return(len_blocks)
      } else return(NULL)
    })
    
    list_inputs_multiple <- eventReactive(input$RUN_multiple, {
      req(get_data_multiple()$dim_data_entry)
      checks <- as.numeric(getChecks()$checks)
      lines <- as.numeric(get_data_multiple()$dim_data_entry)
      return(list(lines, input$list_entries_multiple, kindExpt, 
                  input$stacked, input$RUN_multiple))
    })
    
    
    # counts <- reactiveValues(trigger = 0)
    # 
    # observeEvent(input$RUN_multiple, {
    #   counts$trigger <- counts$trigger + 1
    # })
    # 
    # trigger_list <- reactive({
    #   list(input$RUN_multiple, counts$trigger)
    # })
    # 
    #  observeEvent(trigger_list(), {
    #    if (counts$trigger > 0) {
    #      print(counts$trigger)
    #      output$field_dimensions <- renderUI({
    #        tagList(
    #          selectInput(inputId = ns("dimensions_multiple"),
    #                      label = "Select dimensions of field:",
    #                      choices = "", width = '400px'),
    #          selectInput(inputId = ns("percent_checks_multi"),
    #                      label = "Choose of diagonal checks:",
    #                      choices = "", width = '400px'),
    #        )
    #      })
    #    }
    #  })
    
    observeEvent(list_inputs_multiple(), {
      req(get_data_multiple()$dim_data_entry)
      checks <- as.numeric(getChecks()$checks)
      total_entries <- as.numeric(get_data_multiple()$dim_data_entry)
      lines <- total_entries - checks
      t1 <- floor(lines + lines * 0.10)
      t2 <- ceiling(lines + lines * 0.20)
      t <- t1:t2
      n <- t[-numbers::isPrime(t)]
      withProgress(message = 'Calculation in progress', {
        choices_list <- list()
        i <- 1
        for (n in t) {
          choices_list[[i]] <- factor_subsets(n, diagonal = TRUE)$labels
          i <- i + 1
        }
        #})
        choices <- unlist(choices_list[!sapply(choices_list, is.null)])
        if(is.null(choices)) {
          choices <- "No options available"
        } 
        Option_NCD <- TRUE
        checksEntries <- as.vector(getChecks()$checksEntries)
        new_choices <- list()
        v <- 1
        by_choices <- 1:length(choices)
        # withProgress(message = 'Calculation in progress', {
        for (dim_options in by_choices) {
          planter_multiple <- input$planter_multiple
          dims <- unlist(strsplit(choices[[dim_options]], " x "))
          n_rows <- as.numeric(dims[1])
          n_cols  <- as.numeric(dims[2])
          dt_options <- available_percent(n_rows = n_rows,
                                          n_cols = n_cols,
                                          checks = checksEntries,
                                          Option_NCD = Option_NCD,
                                          kindExpt = kindExpt,
                                          stacked = input$stacked,
                                          planter_mov1 = planter_multiple,
                                          data = get_data_multiple()$data_entry,
                                          dim_data = get_data_multiple()$dim_data_entry,
                                          dim_data_1 = get_data_multiple()$dim_data_1,
                                          Block_Fillers = blocks_length())
          if (!is.null(dt_options$dt)) {
            new_choices[[v]] <- choices[[dim_options]]
            v <- v + 1
          }
        }
      })
      
      updateSelectInput(inputId = "dimensions_multiple",
                        choices = new_choices,
                        selected = new_choices[1])
      #shinyjs::show(id = "dimensions_multiple")
      #shinyjs::show(id = "percent_checks_multi")
    })
    
    
    observeEvent(input$RUN_multiple, {
      req(get_data_multiple()$dim_data_entry)
      shinyjs::show(id = "dimensions_multiple")
      shinyjs::show(id = "percent_checks_multi")
    })
    
    field_dimensions_diagonal <- reactive({
      req(input$dimensions_multiple)
      dims <- unlist(strsplit(input$dimensions_multiple, " x "))
      d_row <- as.numeric(dims[1])
      d_col <- as.numeric(dims[2])
      return(list(d_row = d_row, d_col = d_col))
    })

    entryListFormat_DBUDC <- data.frame(
      ENTRY = 1:9, 
      NAME = c(c("CHECK1", "CHECK2","CHECK3"), paste("Genotype", LETTERS[1:6], sep = "")),
      BLOCK = c(rep("ALL", 3), rep(1:3, each = 2))
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
          shinyjqui::jqui_draggable(
            entriesInfoModal_DBUDC()
          )
        )
      }
    })

    
    available_percent_table <- reactive({
      req(input$dimensions_multiple)
      req(get_data_multiple())
      Option_NCD <- TRUE
      checksEntries <- as.vector(getChecks()$checksEntries)
      planter_multiple <- input$planter_multiple
      n_rows <- field_dimensions_diagonal()$d_row
      n_cols <- field_dimensions_diagonal()$d_col
      available_percent(n_rows = n_rows, 
                        n_cols = n_cols, 
                        checks = checksEntries, 
                        Option_NCD = Option_NCD, 
                        kindExpt = kindExpt, 
                        stacked = input$stacked, 
                        planter_mov1 = planter_multiple, 
                        data = get_data_multiple()$data_entry, 
                        dim_data = get_data_multiple()$dim_data_entry,
                        dim_data_1 = get_data_multiple()$dim_data_1, 
                        Block_Fillers = blocks_length())
    }) 
    
    rand_checks <- reactive({
      req(input$dimensions_multiple)
      req(get_data_multiple())
      req(field_dimensions_diagonal())
      Option_NCD <- TRUE
      req(input$seed_multiple)
      seed <- as.numeric(input$seed_multiple)
      req(available_percent_table()$dt)
      req(available_percent_table()$d_checks)
      req(available_percent_table()$P)
      checksEntries <- as.vector(getChecks()$checksEntries)
      planter_multiple <- input$planter_multiple
      locs <- as.numeric(input$locs_db)
      percent <- as.numeric(input$percent_checks_multi)
      diag_locs <- vector(mode = "list", length = locs)
      random_checks_locs <- vector(mode = "list", length = locs)
      set.seed(seed)
      for (sites in 1:locs) {
        random_checks_locs[[sites]] <- random_checks(
          dt = available_percent_table()$dt, 
          d_checks = available_percent_table()$d_checks, 
          p = available_percent_table()$P, 
          percent = percent, 
          kindExpt = kindExpt, 
          planter_mov = planter_multiple, 
          Checks = checksEntries,
          stacked = input$stacked, 
          data = get_data_multiple()$data_entry, 
          data_dim_each_block = available_percent_table()$data_dim_each_block,
          n_reps = input$n_reps, seed = NULL)
      }
      return(random_checks_locs)
    }) 
    
    user_location <- reactive({
      user_site <- as.numeric(input$locView_diagonal_db)
      loc_user_out <- rand_checks()[[user_site]]
      return(list(map_checks = loc_user_out$map_checks, 
                  col_checks = loc_user_out$col_checks,
                  user_site = user_site))
    })
    
    output$options_table <- DT::renderDT({
      Option_NCD <- TRUE
      if (is.null(available_percent_table()$dt)) {
        shiny::validate("Data input does not fit to field dimensions")
        return(NULL)
      }
      my_out <- available_percent_table()$dt
      my_percent <- my_out[,2]
      len <- length(my_percent)
      selected <- my_percent[len]
      updateSelectInput(session = session, 
                        inputId = 'percent_checks_multi', 
                        label = "Choose % of Checks:",
                        choices = my_percent, 
                        selected = selected)
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
      df <- get_data_multiple()$data_entry
      df$ENTRY <- as.factor(df$ENTRY)
      df$NAME <- as.factor(df$NAME)
      df$BLOCK <- as.factor(df$BLOCK)
      a <- ncol(df) - 1
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
      Option_NCD <- TRUE
      req(get_data_multiple()$data_entry)
      if (kindExpt == "DBUDC") {
        data_entry <- get_data_multiple()$data_entry
        table_type <- as.data.frame(table(data_entry[,3]))
        colnames(table_type) <- c("SUB-BLOCKS", "FREQUENCY")
        df <- table_type
        options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                  scrollX = TRUE, scrollY = "350px"))
        DT::datatable(df, rownames = FALSE)
      }
    })
    
    rand_lines <- reactive({ 
      req(input$dimensions_multiple)
      req(get_data_multiple())
      req(field_dimensions_diagonal())
      Option_NCD <- TRUE
      req(available_percent_table()$dt)
      req(available_percent_table()$d_checks)
      req(get_data_multiple()$data_entry)
      data_entry <- get_data_multiple()$data_entry
      n_rows <- field_dimensions_diagonal()$d_row
      n_cols <- field_dimensions_diagonal()$d_col
      checksEntries <- getChecks()$checksEntries
      checks <- as.numeric(input$checks.db)
      # if(kindExpt == "DBUDC") multi <- TRUE else multi <- FALSE
      locs <- as.numeric(input$locs_db)
      diag_locs <- vector(mode = "list", length = locs)
      random_entries_locs <- vector(mode = "list", length = locs)
      for (sites in 1:locs) {
        map_checks <- rand_checks()[[sites]]$map_checks
        w_map <- rand_checks()[[sites]]$map_checks
        my_split_r <- rand_checks()[[sites]]$map_checks
        #if (multi == TRUE) {
          req(get_data_multiple()$data_entry)
          data_entry <- get_data_multiple()$data_entry
          if (input$stacked == "By Row") {
            req(available_percent_table()$data_dim_each_block)
            data_dim_each_block <- available_percent_table()$data_dim_each_block
            my_row_sets <- automatically_cuts(data = map_checks, 
                                              planter_mov = input$planter_multiple,
                                              way = "By Row", 
                                              dim_data = data_dim_each_block)[[1]]
            if(is.null(my_row_sets)) return(NULL)
            n_blocks <- length(my_row_sets)
          }else if (input$stacked == "By Column") {
            req(available_percent_table()$data_dim_each_block)
            data_dim_each_block <- available_percent_table()$data_dim_each_block
            cuts_by_c <- automatically_cuts(data = map_checks, 
                                            planter_mov = input$planter_multiple, 
                                            way = "By Column",
                                            dim_data = data_dim_each_block) 
            if(is.null(cuts_by_c)) return(NULL)
            n_blocks <- length(cuts_by_c)
            m = diff(cuts_by_c)
            my_col_sets = c(cuts_by_c[1], m)
          }
          if(input$stacked == "By Column") {
            n_rows <- field_dimensions_diagonal()$d_row
            n_cols <- field_dimensions_diagonal()$d_col
            data_random <- get_random_stacked(stacked = "By Column", 
                                              n_rows = n_rows,
                                              n_cols = n_cols,
                                              matrix_checks = map_checks,
                                              Fillers = FALSE,
                                              checks = checksEntries,
                                              data = data_entry,
                                              data_dim_each_block = data_dim_each_block)
          }else {
            n_rows <- field_dimensions_diagonal()$d_row
            n_cols <- field_dimensions_diagonal()$d_col
            if(Option_NCD == FALSE) {
              data_entry1 <- data_entry[(checks + 1):nrow(data_entry), ]
              data_random <- get_DBrandom(binaryMap = w_map, 
                                          data_dim_each_block = data_dim_each_block, 
                                          data_entries = data_entry1,
                                          planter = input$planter_multiple)
            }else if(Option_NCD == TRUE) {
              req(available_percent_table()$data_dim_each_block)
              Block_Fillers <- as.numeric(blocks_length())
              data_random <- get_random(n_rows = n_rows, 
                                        n_cols = n_cols, 
                                        d_checks = my_split_r,
                                        Fillers = FALSE, 
                                        row_sets = my_row_sets,
                                        checks = checksEntries, 
                                        data = data_entry, 
                                        planter_mov = input$planter_multiple,
                                        Multi.Fillers = TRUE, 
                                        which.blocks = Block_Fillers)
            }
          }
        #}
        random_entries_locs[[sites]] <- data_random
      }
      return(random_entries_locs)
    })
    
    output$randomized_layout <- DT::renderDT({
      req(input$dimensions_multiple)
      req(get_data_multiple())
      req(rand_lines())
      VisualCheck <- FALSE
      user_site <- as.numeric(input$locView_diagonal_db)
      loc_view_user <- rand_lines()[[user_site]]
      r_map <- loc_view_user$rand
      checksEntries <- getChecks()$checksEntries
      if (is.null(r_map))
        return(NULL)
      checks = checksEntries
      len_checks <- length(checks)
      req(get_data_multiple()$data_entry)
      x <- loc_view_user$Entries
      sub_len <- numeric()
      for (i in 1:length(x)){
        sub_len[i] <- length(x[[i]]) 
      }
      a <- as.vector(unlist(x))
      a <- c(a, checks)
      colores_back <- c('yellow', 'cadetblue', 'lightgreen', 'grey', 'tan', 'lightcyan',
                        'violet', 'thistle')
      my_colors <- list()
      b = 1
      for (i in sub_len){
        my_colors[[b]] <- rep(colores_back[b], i)
        b = b + 1
      }
      colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                    'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
      my_colors <- unlist(my_colors)
      my_colors <- c(my_colors, colores[1:len_checks])
      df <- as.data.frame(r_map)
      rownames(df) <- nrow(df):1
      DT::datatable(df,
                    extensions = 'Buttons',
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
      ) %>% 
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                        backgroundColor = DT::styleEqual(c(checks),
                                                          colores[1:len_checks]))
    })
    
    
    split_name_reactive <- reactive({
      checksEntries <- getChecks()$checksEntries
      checks <- checksEntries
      req(rand_lines())
      data_entry <- get_data_multiple()$data_entry
      w_map <- rand_checks()[[1]]$map_checks
      if ("Filler" %in% w_map) Option_NCD <- TRUE else Option_NCD <- FALSE
      n_rows <- field_dimensions_diagonal()$d_row
      n_cols <- field_dimensions_diagonal()$d_col
      if (input$stacked == "By Row") {
        map_letters <- rand_lines()[[1]]$w_map_letter
        data_dim_each_block <- available_percent_table()$data_dim_each_block
        Name_expt <- as.vector(unlist(strsplit(input$expt_name_multiple, ",")))
        blocks <- length(data_dim_each_block)
        if (length(Name_expt) == blocks) {
          name_expt <- Name_expt
        }else{
          name_expt = paste0(rep("Block", times = blocks), 1:blocks)
        }
        map_letters <- rand_lines()[[1]]$w_map_letter
        checksEntries <- as.vector(getChecks()$checksEntries)
        split_name_diagonal1 <- names_dbrows(w_map = w_map, 
                                             myWay = "By Row",
                                             kindExpt = "DBUDC",
                                             data_dim_each_block = data_dim_each_block,
                                             w_map_letters = map_letters,
                                             expt_name = name_expt,
                                             Checks = checksEntries)
      }else if (input$stacked == "By Column") {
        map_letters <- rand_lines()[[1]]$w_map_letter
        data_dim_each_block <- available_percent_table()$data_dim_each_block
        Name_expt <- as.vector(unlist(strsplit(input$expt_name_multiple, ",")))
        blocks <- length(data_dim_each_block)
        if (length(Name_expt) == blocks) {
          name_expt <- Name_expt
        }else{
          name_expt = paste0(rep("Block", times = blocks), 1:blocks)
        }
        map_letters <- rand_lines()[[1]]$w_map_letter
        split_name_diagonal1 <- names_diagonal(nrows = n_rows,
                                               ncols = n_cols,
                                               randomChecksMap = w_map,
                                               kindExpt = kindExpt,
                                               checks = 1:input$checks.db,
                                               myWay = input$stacked,
                                               Option_NCD = Option_NCD,
                                               expt_name = name_expt,
                                               data_entry = data_entry,
                                               reps = NULL,
                                               data_dim_each_block = data_dim_each_block,
                                               w_map_letters1 = map_letters)
      }
    })
    
    output$name_layout <- DT::renderDT({
      Option_NCD <- TRUE
      req(split_name_reactive()$my_names)
      my_names <- split_name_reactive()$my_names
      if (is.null(my_names)) return(NULL)
      w_map <- rand_checks()[[1]]$map_checks
      if("Filler" %in% w_map) Option_NCD <- TRUE else Option_NCD <- FALSE
      if(kindExpt == "DBUDC") multi <- TRUE else multi <- FALSE
      if(kindExpt == "DBUDC") { 
        if (input$stacked == "By Row") { 
          data_dim_each_block <- available_percent_table()$data_dim_each_block 
          my_row_sets <- automatically_cuts(data = w_map, 
                                            planter_mov = input$planter_multiple,
                                            way = "By Row", 
                                            dim_data = data_dim_each_block)[[1]]
          blocks <- length(my_row_sets) 
        }else { 
          data_dim_each_block <- available_percent_table()$data_dim_each_block 
          cuts_by_c <- automatically_cuts(data = w_map,
                                          planter_mov = NULL,
                                          way = "By Column",
                                          dim_data = data_dim_each_block)  
          blocks <- length(cuts_by_c) 
        }  
        Name_expt <- as.vector(unlist(strsplit(input$expt_name_multiple, ","))) 
        if (length(Name_expt) == blocks) { 
          name_expt <- Name_expt 
        } else { 
          name_expt = paste0(rep("Block", times = blocks), 1:blocks) 
        } 
        colores_back <- c('yellow', 'cadetblue', 'lightgreen', 'grey', 
                          'tan', 'lightcyan',
                          'violet', 'thistle') 
        df <- as.data.frame(my_names) 
        rownames(df) <- nrow(df):1
        options(DT.options = list(pageLength = nrow(df), 
                                  autoWidth = FALSE,
                                  scrollY = "700px"))
        DT::datatable(df,
                      extensions = 'FixedColumns',
                      options = list(
                        dom = 't',
                        scrollX = TRUE,
                        fixedColumns = TRUE
                      )) %>% 
          DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                          backgroundColor = DT::styleEqual(name_expt, 
                                                            colores_back[1:blocks])
          ) 
        } 
    })
    
    plot_number_sites <- reactive({
      if (is.null(input$plot_start_multiple) || input$plot_start_multiple == " ") validate("Plot starting number is missing.")
      l <- as.numeric(input$locs_db)
      plotNumber <- as.numeric(as.vector(unlist(strsplit(input$plot_start_multiple, ","))))
      if(!is.numeric(plotNumber) && !is.integer(plotNumber)) {
        validate("plotNumber should be an integer or a numeric vector.")
      }
      
      if (any(plotNumber %% 1 != 0)) {
        validate("plotNumber should be integers.")
      }
      
      if (!is.null(l)) {
        if (is.null(plotNumber) || length(plotNumber) != l) {
          if (l > 1){
            plotNumber <- seq(1001, 1000*(l+1), 1000)
          } else plotNumber <- 1001
        }
      }else validate("Number of locations/sites is missing")
      
      return(plotNumber)
      
    })
    
    plot_number_reactive <- reactive({
      req(rand_lines())
      req(input$plot_start_multiple)
      # req(input$n_rows, input$n_cols)
      req(split_name_reactive()$my_names)
      
      datos_name <- split_name_reactive()$my_names 
      datos_name = as.matrix(datos_name) 
      # n_rows = input$n_rows; n_cols = input$n_cols 
      n_rows <- field_dimensions_diagonal()$d_row
      n_cols <- field_dimensions_diagonal()$d_col
      movement_planter = input$planter_multiple
      w_map <- rand_checks()[[1]]$map_checks
      
      if("Filler" %in% w_map) Option_NCD <- TRUE else Option_NCD <- FALSE
      if(kindExpt == "DBUDC") multi <- TRUE else multi <- FALSE
      plot_n_start <- plot_number_sites()
      locs_diagonal <- as.numeric(input$locs_db)
      plots_number_sites <- vector(mode = "list", length = locs_diagonal)
      # start for loop
      for (sites in 1:locs_diagonal) {
        if (multi == TRUE && Option_NCD == FALSE) { 
          if (kindExpt == "DBUDC") { 
            req(get_data_multiple()$data_entry) 
            req(available_percent_table()$data_dim_each_block) 
            if (input$stacked == "By Row") { 
              data_dim_each_block <- available_percent_table()$data_dim_each_block 
              my_row_sets <- automatically_cuts(data = w_map, 
                                                planter_mov = input$planter_multiple,
                                                way = "By Row", 
                                                dim_data = data_dim_each_block)[[1]]
              n_blocks <- length(my_row_sets) 
            }else { 
              data_dim_each_block <- available_percent_table()$data_dim_each_block 
              cuts_by_c <- automatically_cuts(data = w_map, 
                                              planter_mov = NULL,
                                              way = "By Column",
                                              dim_data = data_dim_each_block)  
              n_blocks <- length(cuts_by_c) 
              m = diff(cuts_by_c) 
              my_col_sets = c(cuts_by_c[1], m) 
            } 
            Name_expt <- as.vector(unlist(strsplit(input$expt_name_multiple, ","))) 
            if (length(Name_expt) == n_blocks) { 
              expe_names <- Name_expt 
            }else { 
              expe_names = paste0(rep("Block", times = n_blocks), 1:n_blocks) 
            } 
            if (length(plot_n_start) > 1 && length(plot_n_start) < n_blocks) return(NULL) 
            
            if (input$stacked == "By Column") {
              my_split_plot_nub <- plot_number(movement_planter = input$planter_multiple, 
                                               n_blocks = n_blocks,
                                               n_rows = n_rows,
                                               n_cols = n_cols, 
                                               plot_n_start = plot_n_start[sites],
                                               datos = datos_name,
                                               expe_name = expe_names,
                                               ByRow = FALSE,
                                               my_row_sets = NULL,
                                               ByCol = TRUE,
                                               my_col_sets = my_col_sets) 
            }else{
              req(split_name_reactive()$my_names)
              datos_name <- split_name_reactive()$my_names 
              data.dim.each <- available_percent_table()$data_dim_each_block
              Block_Fillers <- as.numeric(blocks_length()) 
              
              my_split_plot_nub <- plot_number_fillers(movement_planter = movement_planter, 
                                                       plot_n_start = plot_n_start[sites],
                                                       datos = datos_name,
                                                       expe_names = expe_names, 
                                                       ByRow = TRUE, 
                                                       my_row_sets = my_row_sets,
                                                       ByCol = FALSE, 
                                                       my_col_sets = NULL,
                                                       which.blocks = Block_Fillers, 
                                                       n_blocks = n_blocks,
                                                       data.dim.each = data.dim.each)
            }
          }
        } else if (multi == TRUE && Option_NCD == TRUE) {
          req(get_data_multiple()$data_entry) 
          if (input$stacked == "By Row") { 
            data_dim_each_block <- available_percent_table()$data_dim_each_block 
            my_row_sets <- automatically_cuts(data = w_map, 
                                              planter_mov = input$planter_multiple,
                                              way = "By Row", dim_data = data_dim_each_block)[[1]]
            n_blocks <- length(my_row_sets) 
          }else { 
            data_dim_each_block <- available_percent_table()$data_dim_each_block 
            cuts_by_c <- automatically_cuts(data = w_map, 
                                            planter_mov = NULL, 
                                            way = "By Column",
                                            dim_data = data_dim_each_block)  
            n_blocks <- length(cuts_by_c) 
            m = diff(cuts_by_c)
            my_col_sets = c(cuts_by_c[1], m) 
          } 
          w_map_letters1 <- rand_lines()[[1]]$w_map_letters1 
          Name_expt <- as.vector(unlist(strsplit(input$expt_name_multiple, ","))) 
          if (length(Name_expt) == n_blocks) { 
            expe_names <- Name_expt 
          }else { 
            expe_names = paste0(rep("Block", times = n_blocks), 1:n_blocks) 
          } 
          if(input$stacked == "By Row") { 
            datos_name <- split_name_reactive()$my_names 
            data.dim.each <- available_percent_table()$data_dim_each_block
            Block_Fillers <- as.numeric(blocks_length()) 
            
            my_split_plot_nub <- plot_number_fillers(movement_planter = input$planter_multiple, 
                                                     plot_n_start = plot_n_start[sites],
                                                     datos = datos_name, expe_names = expe_names, ByRow = TRUE,
                                                     my_row_sets = my_row_sets, ByCol = FALSE, my_col_sets = NULL,
                                                     which.blocks = Block_Fillers, n_blocks = n_blocks,
                                                     data.dim.each = data.dim.each) 
          }else { 
            return(NULL) 
          } 
        }else { 
          n_blocks <- 1 
          if (input$expt_name_multiple != "") { 
            Name_expt <- input$expt_name_multiple  
          }else Name_expt = paste0(rep("Expt", times = n_blocks), 1:n_blocks)
          w_map <- rand_checks()[[1]]$map_checks
          if("Filler" %in% w_map) Option_NCD <- TRUE else Option_NCD <- FALSE 
          my_split_plot_nub <- plot_number(movement_planter = input$planter_multiple, n_blocks = 1, 
                                           n_rows = n_rows, n_cols = n_cols, 
                                           plot_n_start = plot_n_start[sites], datos = datos_name,
                                           expe_name =  Name_expt, ByRow = NULL, 
                                           my_row_sets = NULL, ByCol = NULL,
                                           my_col_sets = NULL)
          if (Option_NCD == TRUE) { 
            r_map <- rand_lines()[[1]]$rand
            Fillers <- sum(r_map == "Filler") 
            if (n_rows %% 2 == 0) { 
              if(input$planter_multiple == "serpentine") { 
                my_split_plot_nub[[1]][1, 1:Fillers] <- 0 
              }else{ 
                my_split_plot_nub[[1]][1,((n_cols + 1) - Fillers):n_cols] <- 0 
              } 
            }else { 
              my_split_plot_nub[[1]][1,((n_cols + 1) - Fillers):n_cols] <- 0 
            } 
          } 
        }
        plots_number_sites[[sites]] <- my_split_plot_nub$w_map_letters1
      }
      return(list(plots_number_sites = plots_number_sites))
    })
    
    
    output$plot_number_layout <- DT::renderDT({
      req(plot_number_reactive())
      plot_num <- plot_number_reactive()$plots_number_sites[[user_location()$user_site]]
      if (is.null(plot_num))
        return(NULL)
      w_map <- rand_checks()[[1]]$map_checks
      if("Filler" %in% w_map) Option_NCD <- TRUE else Option_NCD <- FALSE
      if(kindExpt == "DBUDC") multi <- TRUE else multi <- FALSE
      df <- as.data.frame(plot_num)
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
    
    export_diagonal_design <- reactive({
      
      ## pre for
      locs_diagonal <- as.numeric(input$locs_db)
      final_expt_fieldbook <- vector(mode = "list",length = locs_diagonal)
      location_names <- as.vector(unlist(strsplit(input$location_multiple, ",")))
      if (length(location_names) != locs_diagonal) location_names <- 1:locs_diagonal
      ## start for
      for (user_site in 1:locs_diagonal) {
        
        
        if(kindExpt == "DBUDC") multi <- TRUE else multi <- FALSE
        if(multi) req(get_data_multiple()$data_entry)
        loc_user_out_rand <- rand_checks()[[user_site]]
        w_map <- as.matrix(loc_user_out_rand$col_checks)
        if("Filler" %in% w_map) Option_NCD <- TRUE else Option_NCD <- FALSE
        req(split_name_reactive()$my_names)
        req(plot_number_reactive())
        movement_planter = input$planter_multiple
        my_data_VLOOKUP <- get_data_multiple()$data_entry
        COLNAMES_DATA <- colnames(my_data_VLOOKUP)
        if(Option_NCD == TRUE) {
          if(kindExpt != "DBUDC") {
            Entry_Fillers <- data.frame(list(0,"Filler"))
          }else {
            Entry_Fillers <- data.frame(list(0,"Filler", "NA"))
          }
          colnames(Entry_Fillers) <- COLNAMES_DATA
          my_data_VLOOKUP <- rbind(my_data_VLOOKUP, Entry_Fillers)
        }
        plot_number <- plot_number_reactive()$plots_number_sites[[user_site]]
        plot_number <- apply(plot_number, 2 ,as.numeric)
        my_names <- split_name_reactive()$my_names
        if (multi == FALSE && Option_NCD == TRUE) {
          my_names <- put_Filler_in_name()$name_with_Fillers
        }
        
        loc_user_out_checks <- rand_checks()[[user_site]]
        Col_checks <- as.matrix(loc_user_out_checks$col_checks)
        loc_user_out_rand <- rand_lines()[[user_site]]
        random_entries_map <- loc_user_out_rand$rand
        random_entries_map[random_entries_map == "Filler"] <- 0
        random_entries_map <- apply(random_entries_map, 2 ,as.numeric)
        if (kindExpt != "RDC") {
          results_to_export <- list(random_entries_map, plot_number, Col_checks, my_names)
          final_expt_export <- export_design(G = results_to_export, movement_planter = movement_planter,
                                             location = location_names[user_site], Year = NULL,
                                             data_file = my_data_VLOOKUP, reps = FALSE)
          final_expt_fieldbook[[user_site]] <- as.data.frame(final_expt_export)
          
        } else {
          if(is.null(split_name_reactive()$my_reps)) return(NULL)
          
          my_reps <- as.matrix(split_name_reactive()$my_reps)
          my_reps <- apply(my_reps, 2 ,as.numeric)
          results_to_export <- list(random_entries_map, plot_number, Col_checks, my_names, my_reps)
          
          final_expt_export <- export_design(G = results_to_export, movement_planter = movement_planter,
                                             location = input$location_multiple, Year = NULL,
                                             data_file = my_data_VLOOKUP, reps = TRUE)
          final_expt_fieldbook[[user_site]] <- as.data.frame(final_expt_export)
        }
      }
      
      ## end for
      
      final_fieldbook <- dplyr::bind_rows(final_expt_fieldbook)
      
      if(Option_NCD == TRUE) {
        final_fieldbook$CHECKS <- ifelse(final_fieldbook$NAME == "Filler", 0, final_fieldbook$CHECKS)
        final_fieldbook$EXPT <- ifelse(final_fieldbook$EXPT == "Filler", 0, final_fieldbook$EXPT)
      }
      if(kindExpt == "DBUDC") {
        final_fieldbook <- final_fieldbook[,-11]
      }
      
      ID <- 1:nrow(final_fieldbook)
      final_fieldbook <- final_fieldbook[, c(6,7,9,4,2,3,5,1,10)]
      final_fieldbook_all_sites <- cbind(ID, final_fieldbook)
      colnames(final_fieldbook_all_sites)[10] <- "TREATMENT"
      
      return(list(final_expt = final_fieldbook_all_sites))
      
    })
    
    
    valsDIAG <- reactiveValues(ROX = NULL, ROY = NULL, trail = NULL, minValue = NULL,
                               maxValue = NULL)
    
    simuModal_DIAG <- function(failed = FALSE) {
      modalDialog(
        fluidRow(
          column(6, 
                 selectInput(inputId = ns("trailsDIAG"), label = "Select One:", 
                             choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
          ),
          column(6, 
                 checkboxInput(inputId = ns("heatmap_Diagonal"), label = "Include a Heatmap", value = TRUE),
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
          actionButton(inputId = ns("ok.diag"), "GO")
        )
      )
    }
    
    observeEvent(input$simulate_multiple, {
      req(export_diagonal_design()$final_expt)
      showModal(
        shinyjqui::jqui_draggable(
          simuModal_DIAG()
        )
      )
    })
    
    observeEvent(input$ok.diag, {
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
          shinyjqui::jqui_draggable(
            simuModal_DIAG(failed = TRUE)
          )
        )
      }
    })
    
    simudata_DIAG <- reactive({
      req(export_diagonal_design()$final_expt)
      if(!is.null(valsDIAG$maxValue) && !is.null(valsDIAG$minValue) && !is.null(valsDIAG$trail)) {
        maxVal <- as.numeric(valsDIAG$maxValue)
        minVal <- as.numeric(valsDIAG$minValue)
        ROX_DIAG <- as.numeric(valsDIAG$ROX)
        ROY_DIAG <- as.numeric(valsDIAG$ROY)
        df_diag <- export_diagonal_design()$final_expt
        loc_levels_factors <- levels(factor(df_diag$LOCATION, unique(df_diag$LOCATION)))
        nrows_diag <- field_dimensions_diagonal()$d_row
        ncols_diag <- field_dimensions_diagonal()$d_col
        # nrows_diag <- as.numeric(input$n_rows)
        # ncols_diag <- as.numeric(input$n_cols)
        seed_diag <- as.numeric(input$seed_multiple)
        locs_diag <- as.numeric(input$locs_db)
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
      }else {
        df_DIAG <- export_diagonal_design()$final_expt
        v <- 2
      }
      if (v == 1) {
        return(list(df = df_diag_locs, dfSimulationList = df_simulation_list))
      }else if (v == 2) {
        return(list(df = df_DIAG))
      }
    })
    
    
    output$fieldBook_diagonal <- DT::renderDT({
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
      req(simudata_DIAG()$dfSimulation)
      loc_user <- user_location()$user_site
      if(input$heatmap_Diagonal) {
        w <- as.character(valsDIAG$trail)
        df <- simudata_DIAG()$dfSimulationList[[loc_user]]
        p1 <- ggplot2::ggplot(df, ggplot2::aes(x = df[,4], y = df[,3], fill = df[,7], text = df[,8])) + 
          ggplot2::geom_tile() +
          ggplot2::xlab("COLUMN") +
          ggplot2::ylab("ROW") +
          ggplot2::labs(fill = w) +
          viridis::scale_fill_viridis(discrete = FALSE)
        
        p2 <- plotly::ggplotly(p1, tooltip="text", width = 1250, height = 750)
        
        return(p2)
      }
    })
    
    output$heatmap_diag <- plotly::renderPlotly({
      req(heatmap_obj_D())
      heatmap_obj_D()
    })
    
    output$download_fieldbook_multiple <- downloadHandler(
      filename = function() {
        req(input$location_multiple)
        loc <- input$location_multiple
        loc <- paste(loc, "_", "Diagonal_Multi", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(simudata_DIAG()$df, file, row.names = FALSE)
        
      }
    )
  })
=======
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
        conditionalPanel(
          condition = "input.list_entries_multiple == 'No'", 
          ns = ns,
          checkboxInput(inputId = ns("sameEntries"), 
                        label = "Use the same entries across experiments!", 
                        value = FALSE)
        ),
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
          fluidRow(
            column(6,style=list("padding-right: 28px;"),
                  numericInput(inputId = ns("lines.db"), 
                                label = "Input # of Entries:",
                                value = 270, 
                                min = 50)
            ),
            column(6,style=list("padding-left: 5px;"),
                  textInput(ns("blocks.db"), 
                            "Input # Entries per Expt:",
                            value = "100,100,70")
            )
          )
       ),
        selectInput(inputId = ns("checks.db"),
                    label = "Input # of Checks:",
                    choices = c(1:10),
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
                 numericInput(inputId = ns("seed_multiple"), 
                              label = "Seed Number:", 
                              value = 17, 
                              min = 1)
          ),
          column(6,
                 style=list("padding-left: 5px;"),
                 textInput(ns("expt_name_multiple"), 
                           "Input Experiment Name:", 
                           value = "Expt1")
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
                 textInput(ns("location_multiple"), 
                           "Input the Location:",
                           value = "FARGO")
          )
        ),
        fluidRow(
          column(6,
                 actionButton(inputId = ns("RUN_multiple"), 
                              "Run!", 
                              icon = icon("cocktail"), 
                              width = '100%'),
          ),
          column(6,
                 actionButton(ns("simulate_multiple"),
                              "Simulate!",
                              icon = icon("cocktail"),
                              width = '100%')
          )
        ),
        br(),
        downloadButton(ns("download_fieldbook_multiple"),
                       "Save Experiment",
                       style = "width:100%")
      ),
      mainPanel(
        width = 8,
        shinyjs::useShinyjs(),
        tabsetPanel(id = ns("tabset_multi"),
                    tabPanel(title = "Expt Design Info", value = "tabPanel1",
                             shinyjs::hidden(
                               selectInput(inputId = ns("dimensions_multiple"),
                                           label = "Select dimensions of field:", 
                                           choices = "", width = '400px')
                             ),
                             shinyjs::hidden(
                               selectInput(inputId = ns("percent_checks_multi"),
                                           label = "Choose % of Checks:", 
                                           choices = "", width = '400px')
                             ),
                             DT::DTOutput(ns("options_table"))
                    ),
                    tabPanel("Input Data",
                             fluidRow(
                               column(6,DT::DTOutput(ns("data_input"))),
                               column(6,DT::DTOutput(ns("checks_table")))
                             )
                    ),
                    tabPanel("Randomized Field", 
                             DT::DTOutput(ns("randomized_layout"))),
                    tabPanel("Expt Layout", DT::DTOutput(ns("name_layout"))),
                    tabPanel("Plot Number Field", DT::DTOutput(ns("plot_number_layout"))),
                    tabPanel("Field Book", DT::DTOutput(ns("fieldBook_diagonal"))),
                    tabPanel("Heatmap", shinycssloaders::withSpinner(
                      plotly::plotlyOutput(ns("heatmap_diag")), 
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
mod_diagonal_multiple_server <- function(id) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
    kindExpt = "DBUDC"

    reactive(print(kindExpt))
    
    shinyjs::useShinyjs()

    multiple_inputs <- eventReactive(input$RUN_multiple, {
      stacked <- input$stacked
      planter_mov <- input$planter_multiple
      Name_expt <- as.vector(unlist(strsplit(input$expt_name_multiple, ",")))
      plotNumber <- as.numeric(as.vector(unlist(strsplit(input$plot_start_multiple, ","))))
      seed_number <- as.numeric(input$seed_multiple)
      location_names <- as.vector(unlist(strsplit(input$location_multiple, ",")))
      sites = as.numeric(input$locs_db)
      return(list(sites = sites, 
                  location_names = location_names, 
                  seed_number = seed_number, 
                  plotNumber = plotNumber,
                  planter_mov = planter_mov,
                  expt_name = Name_expt, 
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
      #Sys.sleep(2)
      Option_NCD <- TRUE
      if (input$list_entries_multiple == "Yes") {
        req(input$checks.db)
        req(input$file_multiple)
        inFile <- input$file_multiple
        data_entry <- load_file(name = inFile$name, 
                                path = inFile$datapat, 
                                sep = input$sep.DIAGONALS)
        data_entry <- na.omit(data_entry)
        if (ncol(data_entry) < 3) {
          validate("Data input needs at least three Columns with the ENTRY, NAME and BLOCK.")
        } 
        data_entry_UP <- data_entry[,1:3] 
        checksEntries <- as.numeric(data_entry_UP[1:input$checks.db,1])
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
            validate("Data input does not fit the requirements!")
          }
          selected <- length(Block_levels)
        }
      } else {
        req(input$checks.db)
        req(input$blocks.db)
        req(input$lines.db)
        lines.db <- as.numeric(input$lines.db)
        checks <- as.numeric(input$checks.db)
        checksEntries <- 1:checks
        NAME <- c(paste(rep("CH", checks), 1:checks, sep = ""),
                  paste(rep("G", lines.db), (checks + 1):(lines.db + checks), sep = ""))
        data_entry_UP <- data.frame(
          list(ENTRY = 1:(lines.db + checks),	NAME = NAME)
        )
        blocks <- as.numeric(as.vector(unlist(strsplit(input$blocks.db, ","))))
        if (lines.db != sum(blocks)) shiny::validate('Sum of blocks may be equal to number of lines.')
        if (as.numeric(input$lines.db) < 50) {
          shiny::validate('Larger field size is recommended for this experiment type')
        }
        data_entry_UP$BLOCK <- c(rep("ALL", checks), rep(1:length(blocks), times = blocks))
        colnames(data_entry_UP) <- c("ENTRY", "NAME", "BLOCK")
        if (input$sameEntries) {
          if (any(blocks != blocks[1])) shiny::validate("Blocks should have the same size")
          # Names
          ChecksNames <- paste(rep("CH", checks), 1:checks, sep = "")
          nameLines <- rep(c(paste(rep("G", blocks[1]), (2 + 1):(blocks[1] + 2), sep = "")), times = length(blocks))
          NAMES <- c(ChecksNames, nameLines)
          # Entries
          ChecksENTRIS <- 1:checks
          nameEntries <- rep((checks + 1):(blocks[1] + checks), times = length(blocks))
          ENTRIES <- c(ChecksENTRIS, nameEntries)
          data_entry_UP$NAME <- NAMES
          data_entry_UP$ENTRY <- ENTRIES
        }
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
      }
      dim_data_entry <- nrow(data_entry_UP)
      dim_data_1 <- nrow(data_entry_UP[(length(checksEntries) + 1):nrow(data_entry_UP), ])
      list(data_entry = data_entry_UP, 
           dim_data_entry = dim_data_entry, 
           dim_data_1 = dim_data_1)
    })
    
    getChecks <- eventReactive(input$RUN_multiple, {
      req(get_data_multiple()$data_entry)
      data <- as.data.frame(get_data_multiple()$data_entry)
      checksEntries <- as.numeric(data[1:input$checks.db,1])
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
      n <- t[-numbers::isPrime(t)]
      withProgress(message = 'Calculation in progress', {
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
          planter_multiple <- multiple_inputs()$planter_mov
          dims <- unlist(strsplit(choices[[dim_options]], " x "))
          n_rows <- as.numeric(dims[1])
          n_cols  <- as.numeric(dims[2])
          dt_options <- available_percent(n_rows = n_rows,
                                          n_cols = n_cols,
                                          checks = checksEntries,
                                          Option_NCD = Option_NCD,
                                          kindExpt = kindExpt,
                                          stacked = multiple_inputs()$stacked,
                                          planter_mov1 = planter_multiple,
                                          data = get_data_multiple()$data_entry,
                                          dim_data = get_data_multiple()$dim_data_entry,
                                          dim_data_1 = get_data_multiple()$dim_data_1,
                                          Block_Fillers = blocks_length())
          if (!is.null(dt_options$dt)) {
            new_choices[[v]] <- choices[[dim_options]]
            v <- v + 1
          }
        }
      })
      
      updateSelectInput(inputId = "dimensions_multiple",
                        choices = new_choices,
                        selected = new_choices[1])
    })
    
    
    observeEvent(input$RUN_multiple, {
      req(get_data_multiple()$dim_data_entry)
      shinyjs::show(id = "dimensions_multiple")
      shinyjs::show(id = "percent_checks_multi")
    })
    
    field_dimensions_diagonal <- reactive({
      req(input$dimensions_multiple)
      dims <- unlist(strsplit(input$dimensions_multiple, " x "))
      d_row <- as.numeric(dims[1])
      d_col <- as.numeric(dims[2])
      return(list(d_row = d_row, d_col = d_col))
    })

    entryListFormat_DBUDC <- data.frame(
      ENTRY = 1:9, 
      NAME = c(c("CHECK1", "CHECK2","CHECK3"), paste("Genotype", LETTERS[1:6], sep = "")),
      BLOCK = c(rep("ALL", 3), rep(1:3, each = 2))
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
          shinyjqui::jqui_draggable(
            entriesInfoModal_DBUDC()
          )
        )
      }
    })

    available_percent_table <- reactive({
      req(input$dimensions_multiple)
      req(get_data_multiple())
      Option_NCD <- TRUE
      checksEntries <- as.vector(getChecks()$checksEntries)
      planter_multiple <- multiple_inputs()$planter_mov
      n_rows <- field_dimensions_diagonal()$d_row
      n_cols <- field_dimensions_diagonal()$d_col
      available_percent(n_rows = n_rows, 
                        n_cols = n_cols, 
                        checks = checksEntries, 
                        Option_NCD = Option_NCD, 
                        kindExpt = kindExpt, 
                        stacked = multiple_inputs()$stacked, 
                        planter_mov1 = planter_multiple, 
                        data = get_data_multiple()$data_entry, 
                        dim_data = get_data_multiple()$dim_data_entry,
                        dim_data_1 = get_data_multiple()$dim_data_1, 
                        Block_Fillers = blocks_length())
    }) 
    
    rand_checks <- reactive({
      req(input$dimensions_multiple)
      req(get_data_multiple())
      req(field_dimensions_diagonal())
      Option_NCD <- TRUE
      req(multiple_inputs()$seed_number)
      seed <- as.numeric(multiple_inputs()$seed_number)
      req(available_percent_table()$dt)
      req(available_percent_table()$d_checks)
      req(available_percent_table()$P)
      checksEntries <- as.vector(getChecks()$checksEntries)
      planter_multiple <- multiple_inputs()$planter_mov
      locs <- as.numeric(multiple_inputs()$sites)
      percent <- as.numeric(input$percent_checks_multi)
      diag_locs <- vector(mode = "list", length = locs)
      random_checks_locs <- vector(mode = "list", length = locs)
      if (isTruthy(available_percent_table()$d_checks)) {
        set.seed(seed)
        for (sites in 1:locs) {
          random_checks_locs[[sites]] <- random_checks(
            dt = available_percent_table()$dt, 
            d_checks = available_percent_table()$d_checks, 
            p = available_percent_table()$P, 
            percent = percent, 
            kindExpt = kindExpt, 
            planter_mov = planter_multiple, 
            Checks = checksEntries,
            stacked = multiple_inputs()$stacked, 
            data = get_data_multiple()$data_entry, 
            data_dim_each_block = available_percent_table()$data_dim_each_block,
            n_reps = input$n_reps, seed = NULL)
        }
      }
      return(random_checks_locs)
    }) 
    
    user_location <- reactive({
      user_site <- as.numeric(input$locView_diagonal_db)
      loc_user_out <- rand_checks()[[user_site]]
      return(list(map_checks = loc_user_out$map_checks, 
                  col_checks = loc_user_out$col_checks,
                  user_site = user_site))
    })
    
    output$options_table <- DT::renderDT({
      Option_NCD <- TRUE
      if (is.null(available_percent_table()$dt)) {
        shiny::validate("Data input does not fit to field dimensions")
        return(NULL)
      }
      my_out <- available_percent_table()$dt
      my_percent <- my_out[,2]
      len <- length(my_percent)
      selected <- my_percent[len]
      updateSelectInput(session = session, 
                        inputId = 'percent_checks_multi', 
                        label = "Choose % of Checks:",
                        choices = my_percent, 
                        selected = selected)
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
      df <- get_data_multiple()$data_entry
      df$ENTRY <- as.factor(df$ENTRY)
      df$NAME <- as.factor(df$NAME)
      df$BLOCK <- as.factor(df$BLOCK)
      a <- ncol(df) - 1
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
      Option_NCD <- TRUE
      req(get_data_multiple()$data_entry)
      data_entry <- get_data_multiple()$data_entry
      table_type <- as.data.frame(table(data_entry[,3]))
      colnames(table_type) <- c("SUB-BLOCKS", "FREQUENCY")
      df <- table_type
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                  scrollX = TRUE, scrollY = "350px"))
      DT::datatable(df, rownames = FALSE)
    })
    
    rand_lines <- reactive({ 
      req(input$dimensions_multiple)
      req(get_data_multiple())
      req(field_dimensions_diagonal())
      Option_NCD <- TRUE
      req(available_percent_table()$dt)
      req(available_percent_table()$d_checks)
      req(get_data_multiple()$data_entry)
      data_entry <- get_data_multiple()$data_entry
      n_rows <- field_dimensions_diagonal()$d_row
      n_cols <- field_dimensions_diagonal()$d_col
      checksEntries <- getChecks()$checksEntries
      checks <- as.numeric(getChecks()$checks)
      locs <- as.numeric(multiple_inputs()$sites)
      diag_locs <- vector(mode = "list", length = locs)
      random_entries_locs <- vector(mode = "list", length = locs)
      for (sites in 1:locs) {
        map_checks <- rand_checks()[[sites]]$map_checks
        w_map <- rand_checks()[[sites]]$map_checks
        my_split_r <- rand_checks()[[sites]]$map_checks
        req(get_data_multiple()$data_entry)
        data_entry <- get_data_multiple()$data_entry
        if (multiple_inputs()$stacked == "By Row") {
          req(available_percent_table()$data_dim_each_block)
          data_dim_each_block <- available_percent_table()$data_dim_each_block
          my_row_sets <- automatically_cuts(data = map_checks, 
                                            planter_mov = multiple_inputs()$planter_mov,
                                            way = "By Row", 
                                            dim_data = data_dim_each_block)[[1]]
          if(is.null(my_row_sets)) return(NULL)
          n_blocks <- length(my_row_sets)
        }else if (multiple_inputs()$stacked == "By Column") {
          req(available_percent_table()$data_dim_each_block)
          data_dim_each_block <- available_percent_table()$data_dim_each_block
          cuts_by_c <- automatically_cuts(data = map_checks, 
                                          planter_mov = multiple_inputs()$planter_mov, 
                                          way = "By Column",
                                          dim_data = data_dim_each_block) 
          if(is.null(cuts_by_c)) return(NULL)
          n_blocks <- length(cuts_by_c)
          m = diff(cuts_by_c)
          my_col_sets = c(cuts_by_c[1], m)
        }
        if(multiple_inputs()$stacked == "By Column") {
          n_rows <- field_dimensions_diagonal()$d_row
          n_cols <- field_dimensions_diagonal()$d_col
          data_random <- get_random_stacked(stacked = "By Column", 
                                            n_rows = n_rows,
                                            n_cols = n_cols,
                                            matrix_checks = map_checks,
                                            Fillers = FALSE,
                                            checks = checksEntries,
                                            data = data_entry,
                                            data_dim_each_block = data_dim_each_block)
        }else {
          n_rows <- field_dimensions_diagonal()$d_row
          n_cols <- field_dimensions_diagonal()$d_col
          if(Option_NCD == FALSE) {
            data_entry1 <- data_entry[(checks + 1):nrow(data_entry), ]
            data_random <- get_DBrandom(binaryMap = w_map, 
                                        data_dim_each_block = data_dim_each_block, 
                                        data_entries = data_entry1,
                                        planter = multiple_inputs()$planter_mov)
          }else if(Option_NCD == TRUE) {
            req(available_percent_table()$data_dim_each_block)
            Block_Fillers <- as.numeric(blocks_length())
            data_random <- get_random(n_rows = n_rows, 
                                      n_cols = n_cols, 
                                      d_checks = my_split_r,
                                      Fillers = FALSE, 
                                      row_sets = my_row_sets,
                                      checks = checksEntries, 
                                      data = data_entry, 
                                      planter_mov = multiple_inputs()$planter_mov,
                                      Multi.Fillers = TRUE, 
                                      which.blocks = Block_Fillers)
          }
        }
        random_entries_locs[[sites]] <- data_random
      }
      return(random_entries_locs)
    })
    
    output$randomized_layout <- DT::renderDT({
      req(input$dimensions_multiple)
      req(get_data_multiple())
      req(rand_lines())
      VisualCheck <- FALSE
      user_site <- as.numeric(input$locView_diagonal_db)
      loc_view_user <- rand_lines()[[user_site]]
      r_map <- loc_view_user$rand
      checksEntries <- getChecks()$checksEntries
      if (is.null(r_map))
        return(NULL)
      checks = checksEntries
      len_checks <- length(checks)
      req(get_data_multiple()$data_entry)
      x <- loc_view_user$Entries
      sub_len <- numeric()
      for (i in 1:length(x)){
        sub_len[i] <- length(x[[i]]) 
      }
      a <- as.vector(unlist(x))
      a <- c(a, checks)
      colores_back <- c('yellow', 'cadetblue', 'lightgreen', 'grey', 'tan', 'lightcyan',
                        'violet', 'thistle')
      my_colors <- list()
      b = 1
      for (i in sub_len){
        my_colors[[b]] <- rep(colores_back[b], i)
        b = b + 1
      }
      colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                    'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
      my_colors <- unlist(my_colors)
      my_colors <- c(my_colors, colores[1:len_checks])
      df <- as.data.frame(r_map)
      rownames(df) <- nrow(df):1
      DT::datatable(df,
                    extensions = 'Buttons',
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
      ) %>% 
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                        backgroundColor = DT::styleEqual(c(checks),
                                                          colores[1:len_checks]))
    })
    
    split_name_reactive <- reactive({
      checksEntries <- getChecks()$checksEntries
      checks <- checksEntries
      req(rand_lines())
      data_entry <- get_data_multiple()$data_entry
      w_map <- rand_checks()[[1]]$map_checks
      if ("Filler" %in% w_map) Option_NCD <- TRUE else Option_NCD <- FALSE
      n_rows <- field_dimensions_diagonal()$d_row
      n_cols <- field_dimensions_diagonal()$d_col
      if (multiple_inputs()$stacked == "By Row") {
        map_letters <- rand_lines()[[1]]$w_map_letter
        data_dim_each_block <- available_percent_table()$data_dim_each_block
        # Name_expt <- as.vector(unlist(strsplit(input$expt_name_multiple, ",")))
        Name_expt <- multiple_inputs()$expt_name
        blocks <- length(data_dim_each_block)
        if (length(Name_expt) == blocks) {
          name_expt <- Name_expt
        }else{
          name_expt = paste0(rep("Block", times = blocks), 1:blocks)
        }
        map_letters <- rand_lines()[[1]]$w_map_letter
        checksEntries <- as.vector(getChecks()$checksEntries)
        split_name_diagonal1 <- names_dbrows(w_map = w_map, 
                                             myWay = "By Row",
                                             kindExpt = "DBUDC",
                                             data_dim_each_block = data_dim_each_block,
                                             w_map_letters = map_letters,
                                             expt_name = name_expt,
                                             Checks = checksEntries)
      }else if (multiple_inputs()$stacked == "By Column") {
        map_letters <- rand_lines()[[1]]$w_map_letter
        data_dim_each_block <- available_percent_table()$data_dim_each_block
        # Name_expt <- as.vector(unlist(strsplit(input$expt_name_multiple, ",")))
        Name_expt <- multiple_inputs()$expt_name
        blocks <- length(data_dim_each_block)
        if (length(Name_expt) == blocks) {
          name_expt <- Name_expt
        }else{
          name_expt = paste0(rep("Block", times = blocks), 1:blocks)
        }
        map_letters <- rand_lines()[[1]]$w_map_letter
        split_name_diagonal1 <- names_diagonal(nrows = n_rows,
                                               ncols = n_cols,
                                               randomChecksMap = w_map,
                                               kindExpt = kindExpt,
                                               checks = 1:getChecks()$checks,
                                               myWay = multiple_inputs()$stacked,
                                               Option_NCD = Option_NCD,
                                               expt_name = name_expt,
                                               data_entry = data_entry,
                                               reps = NULL,
                                               data_dim_each_block = data_dim_each_block,
                                               w_map_letters1 = map_letters)
      }
    })
    
    output$name_layout <- DT::renderDT({
      Option_NCD <- TRUE
      req(split_name_reactive()$my_names)
      my_names <- split_name_reactive()$my_names
      if (is.null(my_names)) return(NULL)
      w_map <- rand_checks()[[1]]$map_checks
      if("Filler" %in% w_map) Option_NCD <- TRUE else Option_NCD <- FALSE
      if (multiple_inputs()$stacked == "By Row") { 
        data_dim_each_block <- available_percent_table()$data_dim_each_block 
        my_row_sets <- automatically_cuts(data = w_map, 
                                          planter_mov = multiple_inputs()$planter_mov,
                                          way = "By Row", 
                                          dim_data = data_dim_each_block)[[1]]
        blocks <- length(my_row_sets) 
      }else { 
        data_dim_each_block <- available_percent_table()$data_dim_each_block 
        cuts_by_c <- automatically_cuts(data = w_map,
                                        planter_mov = NULL,
                                        way = "By Column",
                                        dim_data = data_dim_each_block)  
        blocks <- length(cuts_by_c) 
      }  
      # Name_expt <- as.vector(unlist(strsplit(input$expt_name_multiple, ","))) 
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
      options(DT.options = list(pageLength = nrow(df), 
                                autoWidth = FALSE,
                                scrollY = "700px"))
      DT::datatable(df,
                    extensions = 'FixedColumns',
                    options = list(
                      dom = 't',
                      scrollX = TRUE,
                      fixedColumns = TRUE
                    )) %>% 
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                        backgroundColor = DT::styleEqual(name_expt, 
                                                          colores_back[1:blocks])
        ) 
    })
    
    plot_number_sites <- reactive({
      if (is.null(multiple_inputs()$plotNumber) || multiple_inputs()$plotNumber == " ") validate("Plot starting number is missing.")
      l <- as.numeric(multiple_inputs()$sites)
      # plotNumber <- as.numeric(as.vector(unlist(strsplit(input$plot_start_multiple, ","))))
      plotNumber <- multiple_inputs()$plotNumber
      if(!is.numeric(plotNumber) && !is.integer(plotNumber)) {
        validate("plotNumber should be an integer or a numeric vector.")
      }
      
      if (any(plotNumber %% 1 != 0)) {
        validate("plotNumber should be integers.")
      }
      
      if (!is.null(l)) {
        if (is.null(plotNumber) || length(plotNumber) != l) {
          if (l > 1){
            plotNumber <- seq(1001, 1000*(l+1), 1000)
          } else plotNumber <- 1001
        }
      }else validate("Number of locations/sites is missing")
      
      return(plotNumber)
      
    })
    
    plot_number_reactive <- reactive({
      req(rand_lines())
      req(split_name_reactive()$my_names)
      datos_name <- split_name_reactive()$my_names 
      datos_name = as.matrix(datos_name) 
      n_rows <- field_dimensions_diagonal()$d_row
      n_cols <- field_dimensions_diagonal()$d_col
      movement_planter = multiple_inputs()$planter_mov
      w_map <- rand_checks()[[1]]$map_checks
      
      if("Filler" %in% w_map) Option_NCD <- TRUE else Option_NCD <- FALSE
      plot_n_start <- plot_number_sites()
      locs_diagonal <- as.numeric(multiple_inputs()$sites)
      plots_number_sites <- vector(mode = "list", length = locs_diagonal)
      # start for loop
      for (sites in 1:locs_diagonal) {
        if (Option_NCD == FALSE) { 
          req(get_data_multiple()$data_entry) 
          req(available_percent_table()$data_dim_each_block) 
          if (multiple_inputs()$stacked == "By Row") { 
            data_dim_each_block <- available_percent_table()$data_dim_each_block 
            my_row_sets <- automatically_cuts(data = w_map, 
                                              planter_mov = multiple_inputs()$planter_mov,
                                              way = "By Row", 
                                              dim_data = data_dim_each_block)[[1]]
            n_blocks <- length(my_row_sets) 
          }else { 
            data_dim_each_block <- available_percent_table()$data_dim_each_block 
            cuts_by_c <- automatically_cuts(data = w_map, 
                                            planter_mov = NULL,
                                            way = "By Column",
                                            dim_data = data_dim_each_block)  
            n_blocks <- length(cuts_by_c) 
            m = diff(cuts_by_c) 
            my_col_sets = c(cuts_by_c[1], m) 
          } 
          # Name_expt <- as.vector(unlist(strsplit(input$expt_name_multiple, ","))) 
          Name_expt <- multiple_inputs()$expt_name
          if (length(Name_expt) == n_blocks) { 
            expe_names <- Name_expt 
          }else { 
            expe_names = paste0(rep("Block", times = n_blocks), 1:n_blocks) 
          } 
          if (length(plot_n_start) > 1 && length(plot_n_start) < n_blocks) return(NULL) 
          
          if (multiple_inputs()$stacked == "By Column") {
            my_split_plot_nub <- plot_number(movement_planter = multiple_inputs()$planter_mov, 
                                              n_blocks = n_blocks,
                                              n_rows = n_rows,
                                              n_cols = n_cols, 
                                              plot_n_start = plot_n_start[sites],
                                              datos = datos_name,
                                              expe_name = expe_names,
                                              ByRow = FALSE,
                                              my_row_sets = NULL,
                                              ByCol = TRUE,
                                              my_col_sets = my_col_sets) 
          }else{
            req(split_name_reactive()$my_names)
            datos_name <- split_name_reactive()$my_names 
            data.dim.each <- available_percent_table()$data_dim_each_block
            Block_Fillers <- as.numeric(blocks_length()) 
            
            my_split_plot_nub <- plot_number_fillers(movement_planter = movement_planter, 
                                                      plot_n_start = plot_n_start[sites],
                                                      datos = datos_name,
                                                      expe_names = expe_names, 
                                                      ByRow = TRUE, 
                                                      my_row_sets = my_row_sets,
                                                      ByCol = FALSE, 
                                                      my_col_sets = NULL,
                                                      which.blocks = Block_Fillers, 
                                                      n_blocks = n_blocks,
                                                      data.dim.each = data.dim.each)
          }
        } else if (Option_NCD == TRUE) {
          req(get_data_multiple()$data_entry) 
          if (multiple_inputs()$stacked == "By Row") { 
            data_dim_each_block <- available_percent_table()$data_dim_each_block 
            my_row_sets <- automatically_cuts(data = w_map, 
                                              planter_mov = multiple_inputs()$planter_mov,
                                              way = "By Row", dim_data = data_dim_each_block)[[1]]
            n_blocks <- length(my_row_sets) 
          }else { 
            data_dim_each_block <- available_percent_table()$data_dim_each_block 
            cuts_by_c <- automatically_cuts(data = w_map, 
                                            planter_mov = NULL, 
                                            way = "By Column",
                                            dim_data = data_dim_each_block)  
            n_blocks <- length(cuts_by_c) 
            m = diff(cuts_by_c)
            my_col_sets = c(cuts_by_c[1], m) 
          } 
          w_map_letters1 <- rand_lines()[[1]]$w_map_letters1 
          # Name_expt <- as.vector(unlist(strsplit(input$expt_name_multiple, ","))) 
          Name_expt <- multiple_inputs()$expt_name
          if (length(Name_expt) == n_blocks) { 
            expe_names <- Name_expt 
          }else { 
            expe_names = paste0(rep("Block", times = n_blocks), 1:n_blocks) 
          } 
          if(multiple_inputs()$stacked == "By Row") { 
            datos_name <- split_name_reactive()$my_names 
            data.dim.each <- available_percent_table()$data_dim_each_block
            Block_Fillers <- as.numeric(blocks_length()) 
            
            my_split_plot_nub <- plot_number_fillers(movement_planter = multiple_inputs()$planter_mov, 
                                                     plot_n_start = plot_n_start[sites],
                                                     datos = datos_name, expe_names = expe_names, ByRow = TRUE,
                                                     my_row_sets = my_row_sets, ByCol = FALSE, my_col_sets = NULL,
                                                     which.blocks = Block_Fillers, n_blocks = n_blocks,
                                                     data.dim.each = data.dim.each) 
          } else { 
            return(NULL) 
          } 
        }
        plots_number_sites[[sites]] <- my_split_plot_nub$w_map_letters1
      }
      return(list(plots_number_sites = plots_number_sites))
    })
    
    output$plot_number_layout <- DT::renderDT({
      req(plot_number_reactive())
      plot_num <- plot_number_reactive()$plots_number_sites[[user_location()$user_site]]
      if (is.null(plot_num))
        return(NULL)
      w_map <- rand_checks()[[1]]$map_checks
      if("Filler" %in% w_map) Option_NCD <- TRUE else Option_NCD <- FALSE
      df <- as.data.frame(plot_num)
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
    
    export_diagonal_design <- reactive({
      locs_diagonal <- as.numeric(multiple_inputs()$sites)
      final_expt_fieldbook <- vector(mode = "list",length = locs_diagonal)
      #location_names <- as.vector(unlist(strsplit(input$location_multiple, ",")))
      location_names <- multiple_inputs()$location_names
      if (length(location_names) != locs_diagonal) location_names <- 1:locs_diagonal
      ## start for
      for (user_site in 1:locs_diagonal) {
        req(get_data_multiple()$data_entry)
        loc_user_out_rand <- rand_checks()[[user_site]]
        w_map <- as.matrix(loc_user_out_rand$col_checks)
        if("Filler" %in% w_map) Option_NCD <- TRUE else Option_NCD <- FALSE
        req(split_name_reactive()$my_names)
        req(plot_number_reactive())
        movement_planter = multiple_inputs()$planter_mov
        my_data_VLOOKUP <- get_data_multiple()$data_entry
        COLNAMES_DATA <- colnames(my_data_VLOOKUP)
        if(Option_NCD == TRUE) {
          Entry_Fillers <- data.frame(list(0,"Filler", "NA"))
          colnames(Entry_Fillers) <- COLNAMES_DATA
          my_data_VLOOKUP <- rbind(my_data_VLOOKUP, Entry_Fillers)
        }
        plot_number <- plot_number_reactive()$plots_number_sites[[user_site]]
        plot_number <- apply(plot_number, 2 ,as.numeric)
        my_names <- split_name_reactive()$my_names
        # if (multi == FALSE && Option_NCD == TRUE) {
        #   my_names <- put_Filler_in_name()$name_with_Fillers
        # }
        loc_user_out_checks <- rand_checks()[[user_site]]
        Col_checks <- as.matrix(loc_user_out_checks$col_checks)
        loc_user_out_rand <- rand_lines()[[user_site]]
        random_entries_map <- loc_user_out_rand$rand
        random_entries_map[random_entries_map == "Filler"] <- 0
        random_entries_map <- apply(random_entries_map, 2 ,as.numeric)
        results_to_export <- list(random_entries_map, plot_number, Col_checks, my_names)
        final_expt_export <- export_design(G = results_to_export, movement_planter = movement_planter,
                                            location = location_names[user_site], Year = NULL,
                                            data_file = my_data_VLOOKUP, reps = FALSE)
        final_expt_fieldbook[[user_site]] <- as.data.frame(final_expt_export)
      }
      ## end for
      final_fieldbook <- dplyr::bind_rows(final_expt_fieldbook)
      if(Option_NCD == TRUE) {
        final_fieldbook$CHECKS <- ifelse(final_fieldbook$NAME == "Filler", 0, final_fieldbook$CHECKS)
        final_fieldbook$EXPT <- ifelse(final_fieldbook$EXPT == "Filler", 0, final_fieldbook$EXPT)
      }
      if(kindExpt == "DBUDC") {
        final_fieldbook <- final_fieldbook[,-11]
      }
      
      ID <- 1:nrow(final_fieldbook)
      final_fieldbook <- final_fieldbook[, c(6,7,9,4,2,3,5,1,10)]
      final_fieldbook_all_sites <- cbind(ID, final_fieldbook)
      colnames(final_fieldbook_all_sites)[10] <- "TREATMENT"
      
      return(list(final_expt = final_fieldbook_all_sites))
      
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
          # column(6, 
          #        checkboxInput(inputId = ns("heatmap_Diagonal"), label = "Include a Heatmap", value = TRUE),
          # )
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
      req(export_diagonal_design()$final_expt)
      showModal(
        shinyjqui::jqui_draggable(
          simuModal_DIAG()
        )
      )
    })
    
    observeEvent(input$ok_simu_multi, {
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
          shinyjqui::jqui_draggable(
            simuModal_DIAG(failed = TRUE)
          )
        )
      }
    })
    
    simudata_DIAG <- reactive({
      req(export_diagonal_design()$final_expt)
      if(!is.null(valsDIAG$maxValue) && !is.null(valsDIAG$minValue) && !is.null(valsDIAG$trail)) {
        maxVal <- as.numeric(valsDIAG$maxValue)
        minVal <- as.numeric(valsDIAG$minValue)
        ROX_DIAG <- as.numeric(valsDIAG$ROX)
        ROY_DIAG <- as.numeric(valsDIAG$ROY)
        df_diag <- export_diagonal_design()$final_expt
        loc_levels_factors <- levels(factor(df_diag$LOCATION, unique(df_diag$LOCATION)))
        nrows_diag <- field_dimensions_diagonal()$d_row
        ncols_diag <- field_dimensions_diagonal()$d_col
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
      }else {
        df_DIAG <- export_diagonal_design()$final_expt
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
      
      p2 <- plotly::ggplotly(p1, tooltip="text", width = 1250, height = 750)
      
      return(p2)
    })
    
    output$heatmap_diag <- plotly::renderPlotly({
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
>>>>>>> 95420d99a4eaa9f0902ced80ed4f323f5abcf300
}