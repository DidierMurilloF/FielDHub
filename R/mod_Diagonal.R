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
mod_Diagonal_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 4,

                   selectInput(inputId = ns("kindExpt"), label = "Select Experiment Type:",
                               choices = c("Single Unreplicated Design with Diagonal Checks" = "SUDC",
                                           #"Replicated Design with Diagonal Checks" = "RDC",
                                           "Decision Block Unreplicated Design with Diagonal Checks" = "DBUDC"),
                               multiple = FALSE),
                   radioButtons(inputId = ns("owndataDIAGONALS"), label = "Import entries' list?", 
                                choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                   
                   conditionalPanel("input.owndataDIAGONALS == 'Yes'", ns = ns,
                                    fluidRow(
                                      column(7, style=list("padding-right: 28px;"),
                                             fileInput(ns("file1"), label = "Upload a CSV File:", multiple = FALSE)),
                                      column(5,style=list("padding-left: 5px;"),
                                             radioButtons(ns("sep.DIAGONALS"), "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t"),
                                                          selected = ","))
                                    )              
                   ),
                   
                   conditionalPanel("input.owndataDIAGONALS == 'No'", ns = ns,
                                    conditionalPanel(condition = "input.kindExpt !='DBUDC'", ns = ns,
                                                     numericInput(inputId = ns("lines.d"), label = "Input # of Entries:",
                                                                  value = 270, min = 5)  
                                    ),
                                    conditionalPanel(condition = "input.kindExpt =='DBUDC'", ns = ns,
                                                     fluidRow(
                                                       column(6,style=list("padding-right: 28px;"),
                                                              numericInput(inputId = ns("lines.db"), label = "Input # of Entries:",
                                                                           value = 270, min = 5)
                                                       ),
                                                       column(6,style=list("padding-left: 5px;"),
                                                              textInput(ns("blocks.db"), "Input # Entries per Expt:",
                                                                        value = "100,100,70")
                                                       )
                                                     )       
                                    )
                   ),
                   
                   fluidRow(
                     column(6,style=list("padding-right: 28px;"),
                            numericInput(inputId = ns("n_rows"), label = "Input # of Rows:",
                                         value = 15, min = 5)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            numericInput(inputId = ns("n_cols"), label = "Input # of Columns:",
                                         value = 20, min = 5)
                     )
                   ),

                   fluidRow(
                     column(6,style=list("padding-right: 28px;"),
                            selectInput(inputId = ns("Dropdown"), label = "Choose of diagonal checks:", 
                                        choices = "")
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            selectInput(inputId = ns("checks"), label = "Input # of Checks:",
                                        choices = c(1:10), multiple = FALSE, selected = 4)
                     )
                   ),
                   fluidRow(
                     column(6,style=list("padding-right: 28px;"),
                            numericInput(inputId = ns("l.diagonal"), label = "Input # of Locations:", 
                                         value = 1, min = 1)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            selectInput(inputId = ns("locView.diagonal"), label = "Choose location to view:", 
                                        choices = 1:1, selected = 1, multiple = FALSE)
                     )
                   ),
                   conditionalPanel(condition = "input.kindExpt !='SUDC'", ns = ns,
                                    
                      fluidRow(
                        column(6,style=list("padding-right: 28px;"),
                               selectInput(inputId = ns("myWay"), label = "Blocks Layout:",
                                           choices = c("By Column", "By Row"), multiple = FALSE,
                                           selected = "By Row")
                        ),
                        column(6,style=list("padding-left: 5px;"),
                               selectInput(inputId = ns("planter_mov"), label = "Plot Order Layout:",
                                           choices = c("serpentine", "cartesian"), multiple = FALSE,
                                           selected = "serpentine")
                        )
                        
                      ),
                      # conditionalPanel(condition = "input.kindExpt == 'DBUDC'", ns = ns,
                      #                  
                      #                  conditionalPanel(condition = "input.myWay == 'By Row'", ns = ns,
                      #                                   
                      #                                   selectInput(inputId = ns("Block_Fillers"), label = "Which Blocks:",
                      #                                               choices = "Block_Fillers", multiple = TRUE, selected = "")               
                      #                  )
                      # )
                   ),
                   conditionalPanel(condition = "input.kindExpt == 'SUDC'", ns = ns,
                                    selectInput(inputId = ns("planter_mov1"), label = "Plot Order Layout:",
                                                choices = c("serpentine", "cartesian"), multiple = FALSE,
                                                selected = "serpentine")
                                    
                   ),
                   
                   fluidRow(
                     column(6,style=list("padding-right: 28px;"),
                            numericInput(inputId = ns("myseed"), label = "Seed Number:", value = 17, min = 1)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            textInput(ns("expt_name"), "Input Experiment Name:", value = "Expt1")
                     )
                   ),    
                   fluidRow(
                     column(6,style=list("padding-right: 28px;"),
                            textInput(ns("plot_start"), "Starting Plot Number:", value = 1)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            textInput(ns("Location"), "Input the Location:", value = "FARGO")
                     )
                     
                   ),
                   
                   fluidRow(
                     column(6,
                            downloadButton(ns("downloadData_Diagonal"), "Save Experiment", style = "width:100%")
                     ),
                     column(6,
                            actionButton(ns("Simulate_Diagonal"), "Simulate!", icon = icon("cocktail"),
                                         width = '100%')
                     )

                   ),
      ),
      
      mainPanel(
        width = 8,
        tabsetPanel(
          tabPanel("Expt Design Info", DT::DTOutput(ns("options_table"))),
          tabPanel("Input Data",
                   fluidRow(
                     column(6,DT::DTOutput(ns("data_input"))),
                     column(6,DT::DTOutput(ns("checks_table")))
                   )
          ),
          tabPanel("Diagonal Checks Layout", DT::DTOutput(ns("checks_layout"))),
          tabPanel("Randomized Field", DT::DTOutput(ns("randomized_layout"))),
          tabPanel("Plot Number Field", DT::DTOutput(ns("plot_number_layout"))),
          tabPanel("Expt Name", DT::DTOutput(ns("name_layout"))),
          tabPanel("Field Book", DT::DTOutput(ns("dt5"))),
          tabPanel("Heatmap", shinycssloaders::withSpinner(plotly::plotlyOutput(ns("heatmap_diag")), type = 5))
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
    
    observeEvent(input$l.diagonal, {
      loc_user_view <- 1:as.numeric(input$l.diagonal)
      updateSelectInput(inputId = "locView.diagonal", choices = loc_user_view, selected = loc_user_view[1])
    })
    
    getData <- reactive({
      Option_NCD <- TRUE
      if (input$owndataDIAGONALS == "Yes") {
        req(input$file1)
        inFile <- input$file1
        data_entry <- load_file(name = inFile$name, path = inFile$datapat, sep = input$sep.DIAGONALS)
        data_entry <- na.omit(data_entry)
        if (ncol(data_entry) < 2) validate("Data input needs at least two Columns with the ENTRY and NAME.")
        data_entry_UP <- data_entry[,1:2]
        colnames(data_entry_UP) <- c("ENTRY", "NAME")
        checksEntries <- as.numeric(data_entry_UP[1:input$checks,1])
        if (input$kindExpt == "DBUDC") {
          if (ncol(data_entry) < 3) validate("Data input needs at least three Columns with the ENTRY, NAME and BLOCK.")
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
            selected <- length(Block_levels)
            # updateSelectInput(session, inputId = 'Block_Fillers', label = "Which Blocks will have Fil:",
            #                   choices = Block_levels, selected = Block_levels[selected])
          }
        }
      }else {
        if (input$kindExpt != "DBUDC") {
          req(input$lines.d)
          req(input$checks)
          checks <- as.numeric(input$checks)
          checksEntries <- 1:checks
          lines <- input$lines.d
          NAME <- c(paste(rep("CH", checks), 1:checks, sep = ""),
                    paste(rep("G", lines), (checks + 1):(lines + checks), sep = ""))
          gen.list <- data.frame(list(ENTRY = 1:(lines + checks),	NAME = NAME))
          data_entry_UP <- gen.list
          colnames(data_entry_UP) <- c("ENTRY", "NAME")
        }else if (input$kindExpt == "DBUDC") {
          req(input$checks)
          req(input$blocks.db)
          req(input$lines.db)
          lines.db <- as.numeric(input$lines.db)
          checks <- as.numeric(input$checks)
          checksEntries <- 1:checks
          NAME <- c(paste(rep("CH", checks), 1:checks, sep = ""),
                    paste(rep("G", lines.db), (checks + 1):(lines.db + checks), sep = ""))
          data_entry_UP <- data.frame(list(ENTRY = 1:(lines.db + checks),	NAME = NAME))
          blocks <- as.numeric(as.vector(unlist(strsplit(input$blocks.db, ","))))
          if (lines.db != sum(blocks)) shiny::validate('Sum of blocks may be equal to number of lines.')
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
            # updateSelectInput(session, inputId = 'Block_Fillers', label = "Which Blocks will have Fil:",
            #                   choices = Block_levels, selected = Block_levels[selected])
          }
        }
        
      }
      
      dim_data_entry <- nrow(data_entry_UP)
      dim_data_1 <- nrow(data_entry_UP[(length(checksEntries) + 1):nrow(data_entry_UP), ])
      
      list(data_entry = data_entry_UP, dim_data_entry = dim_data_entry, dim_data_1 = dim_data_1)
      
    })
    
    blocks_length <- reactive({
      req(getData()$data_entry)
      if (input$kindExpt == "DBUDC") {
        df <- getData()$data_entry
        Block_levels <- suppressWarnings(as.numeric(levels(as.factor(df$BLOCK))))
        Block_levels <- na.omit(Block_levels)
        len_blocks <- length(Block_levels)
        return(len_blocks)
      } else return(NULL)
    })
    
    # blocks_to_fillers <- as.numeric(blocks_length())
    
    entryListFormat_SUDC <- data.frame(ENTRY = 1:9, NAME = c(c("CHECK1", "CHECK2","CHECK3"), paste("Genotype", LETTERS[1:6], sep = "")))
    entryListFormat_DBUDC <- data.frame(ENTRY = 1:9, NAME = c(c("CHECK1", "CHECK2","CHECK3"), paste("Genotype", LETTERS[1:6], sep = "")),
                                        BLOCK = c(rep("ALL", 3), rep(1:3, each = 2)))
    
    toListen <- reactive({
      list(input$owndataDIAGONALS,input$kindExpt)
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
      if (input$owndataDIAGONALS == "Yes" && input$kindExpt == "SUDC") {
        showModal(
          shinyjqui::jqui_draggable(
            entriesInfoModal_SUDC()
          )
        )
      }else if (input$owndataDIAGONALS == "Yes" && input$kindExpt == "DBUDC") {
        showModal(
          shinyjqui::jqui_draggable(
            entriesInfoModal_DBUDC()
          )
        )
      }
    })
    
    getChecks <- reactive({
      req(getData()$data_entry)
      data <- as.data.frame(getData()$data_entry)
      checksEntries <- as.numeric(data[1:input$checks,1])
      checks <- as.numeric(input$checks)
      list(checksEntries = checksEntries, checks = checks)
    })
    
    #available_percent_table <- eventReactive(input$RUN.diag, {
    available_percent_table <- reactive({
      Option_NCD <- TRUE
      checksEntries <- as.vector(getChecks()$checksEntries)
      if(input$kindExpt == "DBUDC" && input$myWay == "By Column") {
        Option_NCD <- FALSE
      }
      
      if (input$kindExpt != "SUDC") {
        planter_mov <- input$planter_mov
      }else planter_mov <- input$planter_mov1
      
      available_percent(n_rows = input$n_rows, n_cols = input$n_cols, checks = checksEntries, 
                        Option_NCD = Option_NCD, Visual_ch = input$Visual_ch, visualCheck = FALSE, 
                        kindExpt = input$kindExpt, myWay = input$myWay, planter_mov1 = planter_mov, 
                        data = getData()$data_entry, dim_data = getData()$dim_data_entry,
                        dim_data_1 = getData()$dim_data_1, Block_Fillers = blocks_length())
    }) 
    
    rand_checks <- reactive({
      Option_NCD <- TRUE
      req(input$myseed)
      seed <- as.numeric(input$myseed)
      req(available_percent_table()$dt)
      req(available_percent_table()$d_checks)
      req(available_percent_table()$P)
      checksEntries <- as.vector(getChecks()$checksEntries)
      if (input$kindExpt != "SUDC") {
        planter_mov <- input$planter_mov
      }else planter_mov <- input$planter_mov1
      
      locs <- as.numeric(input$l.diagonal)
      diag_locs <- vector(mode = "list", length = locs)
      random_checks_locs <- vector(mode = "list", length = locs)
      set.seed(seed )
      for (sites in 1:locs) {
        random_checks_locs[[sites]] <- random_checks(dt = available_percent_table()$dt, d_checks = available_percent_table()$d_checks, 
                                                     p = available_percent_table()$P, percent = input$Dropdown, kindExpt = input$kindExpt, 
                                                     planter_mov = planter_mov, Checks = checksEntries, myWay = input$myWay, 
                                                     data = getData()$data_entry, data_dim_each_block = available_percent_table()$data_dim_each_block,
                                                     n_reps = input$n_reps, seed = NULL)
      }
      # print(random_checks_locs[[1]])
      return(random_checks_locs)
    }) 
    
    
    user_location <- reactive({
      user_site <- as.numeric(input$locView.diagonal)
      loc_user_out <- rand_checks()[[user_site]]
      return(list(map_checks = loc_user_out$map_checks, 
                  col_checks = loc_user_out$col_checks,
                  user_site = user_site))
    })
    
    
    output$options_table <- DT::renderDT({
      Option_NCD <- TRUE
      req(available_percent_table()$dt)
      if (is.null(available_percent_table()$dt)) shiny::validate(":smth is wromg with 'available_percent_table()$dt'")
      my_out <- available_percent_table()$dt
      my_percent <- my_out[,2]
      updateSelectInput(session = session, inputId = 'Dropdown', label = "Choose % of Checks:",
                        choices = my_percent)
      df <- as.data.frame(my_out)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "460px"))
      DT::datatable(df, rownames = FALSE, caption = 'Reference guide to design your experiment. Choose the percentage (%)
                    of checks based on the total number of plots you want to have in the final layout.', options = list(
                      columnDefs = list(list(className = 'dt-center', targets = "_all"))))
      
    })
    
    output$data_input <- DT::renderDT({
      my_data <- getData()$data_entry
      df <- my_data
      a <- ncol(df) - 1
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "800px"))
      DT::datatable(df, rownames = FALSE, caption = 'List of Entries.', options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    })
    
    output$checks_layout <- DT::renderDT({
      Option_NCD <- TRUE
      multi <- input$kindExpt == "RDC" || input$kindExpt == "DBUDC"
      if (multi) req(getData()$data_entry)
      req(user_location()$map_checks)
      w_map <- user_location()$map_checks
      # print(w_map)
      if (is.null(w_map))
        return(NULL)
      
      checks <- as.vector(getChecks()$checksEntries)
      len_checks <- length(checks)
      colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                   'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
      
      df <- as.data.frame(w_map)
      rownames(df) <- nrow(df):1
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "700px"))
      DT::datatable(df,
                    extensions = 'FixedColumns',
                    options = list(
                      dom = 't',
                      scrollX = TRUE,
                      fixedColumns = TRUE
                    )) %>% 
      DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                      backgroundColor = DT::styleEqual(c(checks,0,"Filler"), 
                                                       c(colores[1:len_checks],'yellow', 'snow')))
    })
    
    output$checks_table <- DT::renderDT({
      Option_NCD <- TRUE
      req(getData()$data_entry)
      if (input$kindExpt == "DBUDC") {
        data_entry <- getData()$data_entry
        table_type <- as.data.frame(table(data_entry[,3]))
        colnames(table_type) <- c("SUB-BLOCKS", "FREQUENCY")
        df <- table_type
        options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                  scrollX = TRUE, scrollY = "350px"))
        DT::datatable(df, rownames = FALSE)
      }else {
        data_entry <- getData()$data_entry
        req(user_location()$map_checks)
        if(is.null(user_location()$map_checks)) return(NULL)
        # w_map <- rand_checks()$map_checks
        w_map <- user_location()$map_checks
        table_checks <- data_entry[1:(input$checks),]
        df <- table_checks
        info_checks <- base::table(w_map[w_map > 0])
        times_checks <- data.frame(list(ENTRY = base::names(info_checks), 
                                        TIMES = base::as.matrix(info_checks)))
        df <- base::merge(df, times_checks, by.x = "ENTRY")
        options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                  scrollX = TRUE, scrollY = "350px"))
        #a <- ncol(df) - 1
        DT::datatable(df, rownames = FALSE, caption = 'Table of Checks.', options = list(
          columnDefs = list(list(className = 'dt-center', targets = "_all"))))
      }
    })
    
    # rand_lines <- eventReactive(input$RUN.diagonal, {
    rand_lines <- reactive({ 
      Option_NCD <- TRUE
      req(input$n_rows, input$n_cols)
      req(available_percent_table()$dt)
      req(available_percent_table()$d_checks)
      # req(rand_checks()$map_checks)
      # req(rand_checks())
      
      # req(user_location()$map_checks)
      req(getData()$data_entry)
      data_entry <- getData()$data_entry
      # w_map <- rand_checks()$map_checks
      #w_map <- user_location()$map_checks
    
      n_rows <- input$n_rows; n_cols <- input$n_cols
      # my_split_r <- rand_checks()$map_checks
      # my_split_r <- user_location()$map_checks
      
      checksEntries <- getChecks()$checksEntries
      checks <- as.numeric(input$checks)
      if(input$kindExpt == "DBUDC") multi <- TRUE else multi <- FALSE
      
      locs <- as.numeric(input$l.diagonal)
      diag_locs <- vector(mode = "list", length = locs)
      random_entries_locs <- vector(mode = "list", length = locs)
      
      for (sites in 1:locs) {
        map_checks <- rand_checks()[[sites]]$map_checks
        w_map <- rand_checks()[[sites]]$map_checks
        my_split_r <- rand_checks()[[sites]]$map_checks
        if (multi == TRUE) {
          # map_checks <- rand_checks()$map_checks
          # map_checks <- user_location()$map_checks
          req(getData()$data_entry)
          data_entry <- getData()$data_entry
          #req(available_percent_table()$data_dim_each_block)
          if (input$kindExpt == "DBUDC" && input$myWay == "By Row") {
            req(available_percent_table()$data_dim_each_block)
            data_dim_each_block <- available_percent_table()$data_dim_each_block
            my_row_sets <- automatically_cuts(data = map_checks, planter_mov = input$planter_mov,
                                              way = "By Row", dim_data = data_dim_each_block)[[1]]
            if(is.null(my_row_sets)) return(NULL)
            n_blocks <- length(my_row_sets)
          }else if (input$kindExpt == "DBUDC" && input$myWay == "By Column") {
            req(available_percent_table()$data_dim_each_block)
            data_dim_each_block <- available_percent_table()$data_dim_each_block
            cuts_by_c <- automatically_cuts(data = map_checks, planter_mov = input$planter_mov, way = "By Column",
                                            dim_data = data_dim_each_block) 
            if(is.null(cuts_by_c)) return(NULL)
            n_blocks <- length(cuts_by_c)
            m = diff(cuts_by_c)
            my_col_sets = c(cuts_by_c[1], m)
          }
          if(input$myWay == "By Column") {
            Option_NCD <- FALSE
            if (input$kindExpt == "DBUDC" && Option_NCD == FALSE){
              data_random <- get_random(n_rows = input$n_rows, n_cols = input$n_cols, d_checks = my_split_r,
                                        reps = NULL, Fillers = FALSE, col_sets = my_col_sets, row_sets = NULL,
                                        checks = checksEntries, data = data_entry, data_dim_each_block = data_dim_each_block)
            }else if(input$kindExpt == "DBUDC" && Option_NCD == TRUE){
              req(available_percent_table()$data_dim_each_block)
              data_random <- get_random(n_rows = input$n_rows, n_cols = input$n_cols, d_checks = my_split_r,
                                        reps = NULL, Fillers = TRUE, col_sets = my_col_sets, row_sets = NULL,
                                        checks = checksEntries,data = data_entry)
            }
          }else {
            if(input$kindExpt == "DBUDC" && Option_NCD == FALSE) {
              data_entry1 <- data_entry[(checks + 1):nrow(data_entry), ]
              data_random <- get_DBrandom(binaryMap = w_map, data_dim_each_block = data_dim_each_block, data_entries = data_entry1,
                                          planter = input$planter_mov)
            }else if(input$kindExpt == "DBUDC" && Option_NCD == TRUE) {
              req(available_percent_table()$data_dim_each_block)
              Block_Fillers <- as.numeric(blocks_length())
              data_random <- get_random(n_rows = input$n_rows, n_cols = input$n_cols, d_checks = my_split_r,
                                        reps = NULL, Fillers = FALSE, col_sets = NULL, row_sets = my_row_sets,
                                        checks = checksEntries, data = data_entry, planter_mov  = input$planter_mov,
                                        Multi.Fillers = TRUE, which.blocks = Block_Fillers)
            }
          }
        }else {
          if("Filler" %in% my_split_r) Option_NCD <- TRUE else Option_NCD <- FALSE
          if(Option_NCD == TRUE) {
            data_random <- get_random(n_rows = input$n_rows, n_cols = input$n_cols, d_checks = my_split_r,
                                      reps = NULL, Fillers = TRUE, col_sets = input$n_cols, row_sets = NULL,
                                      checks = checksEntries, data = data_entry, planter_mov  = input$planter_mov)
          }else {
            data_random <- get_random(n_rows = input$n_rows, n_cols = input$n_cols, d_checks = my_split_r,
                                      reps = NULL, Fillers = FALSE, col_sets = input$n_cols, row_sets = NULL,
                                      checks = checksEntries, data = data_entry, planter_mov  = input$planter_mov)
          }
        }
        random_entries_locs[[sites]] <- data_random
      }
      print(random_entries_locs[[1]])
      return(random_entries_locs)
    })
    
    output$randomized_layout <- DT::renderDT({
      req(rand_lines())
      VisualCheck <- FALSE
      user_site <- as.numeric(input$locView.diagonal)
      loc_view_user <- rand_lines()[[user_site]]
      r_map <- loc_view_user$rand
      checksEntries <- getChecks()$checksEntries
      if (is.null(r_map))
        return(NULL)
      checks = checksEntries
      len_checks <- length(checks)
      if (input$kindExpt == "DBUDC") multi <- TRUE else multi <- FALSE
      
      if (multi == FALSE) {
        df <- as.data.frame(r_map)
        colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                     'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
        s <- unlist(loc_view_user$Entries)
        rownames(df) <- nrow(df):1
        #options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "800px"))
        DT::datatable(df,
                      extensions = c('Buttons', 'FixedColumns'),
                      options = list(dom = 'Blfrtip',
                                     autoWidth = FALSE,
                                     scrollX = TRUE,
                                     fixedColumns = TRUE,
                                     pageLength = nrow(df),
                                     scrollY = "800px",
                                     class = 'compact cell-border stripe',  rownames = FALSE,
                                     server = FALSE,
                                     filter = list( position = 'top', clear = FALSE, plain =TRUE ),
                                     buttons = c('copy', 'excel'),
                                     lengthMenu = list(c(10,25,50,-1),
                                                       c(10,25,50,"All")))) %>% 
          DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                          backgroundColor = DT::styleEqual(c(checks), 
                                                           colores[1:len_checks])) %>% 
          DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                          backgroundColor = DT::styleEqual(s, 
                                                           rep('gray', length(s)
                                                           )
                          )
          )
      } else {
        req(getData()$data_entry)
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
        #options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "850px"))
        DT::datatable(df,
                      extensions = c('Buttons', 'FixedColumns'),
                      options = list(dom = 'Blfrtip',
                                     autoWidth = FALSE,
                                     scrollX = TRUE,
                                     fixedColumns = TRUE,
                                     pageLength = nrow(df),
                                     scrollY = "840px",
                                     class = 'compact cell-border stripe',  rownames = FALSE,
                                     server = FALSE,
                                     filter = list( position = 'top', clear = FALSE, plain =TRUE ),
                                     buttons = c('copy', 'excel'),
                                     lengthMenu = list(c(10,25,50,-1),
                                                       c(10,25,50,"All")))) %>% 
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                        backgroundColor = DT::styleEqual(a, my_colors))
      }
    })
    
    
    split_name_reactive <- reactive({
      checksEntries <- getChecks()$checksEntries
      checks <- checksEntries
      req(rand_lines())
      req(input$n_rows, input$n_cols)
      data_entry <- getData()$data_entry
      w_map <- rand_checks()[[1]]$map_checks
      if ("Filler" %in% w_map) Option_NCD <- TRUE else Option_NCD <- FALSE
      n_rows = input$n_rows; n_cols = input$n_cols
      if(input$kindExpt == "DBUDC") multi <- TRUE else multi <- FALSE
      if (input$myWay == "By Row" && input$kindExpt == "DBUDC") {
        map_letters <- rand_lines()[[1]]$w_map_letter
        data_dim_each_block <- available_percent_table()$data_dim_each_block
        Name_expt <- as.vector(unlist(strsplit(input$expt_name, ",")))
        blocks <- length(data_dim_each_block)
        if (length(Name_expt) == blocks) {
          name_expt <- Name_expt
        }else{
          name_expt = paste0(rep("Block", times = blocks), 1:blocks)
        }
        map_letters <- rand_lines()[[1]]$w_map_letter
        checksEntries <- as.vector(getChecks()$checksEntries)
        split_name_diagonal1 <- names_dbrows(w_map = w_map, myWay = "By Row", kindExpt = "DBUDC", data_dim_each_block = data_dim_each_block,
                                             w_map_letters = map_letters, expt_name = name_expt, Checks = checksEntries)
      }else if (input$myWay == "By Column" && input$kindExpt == "DBUDC") {
        map_letters <- rand_lines()[[1]]$w_map_letter
        data_dim_each_block <- available_percent_table()$data_dim_each_block
        Name_expt <- as.vector(unlist(strsplit(input$expt_name, ",")))
        blocks <- length(data_dim_each_block)
        if (length(Name_expt) == blocks) {
          name_expt <- Name_expt
        }else{
          name_expt = paste0(rep("Block", times = blocks), 1:blocks)
        }
        map_letters <- rand_lines()[[1]]$w_map_letter
        split_name_diagonal1 <- names_diagonal(nrows = n_rows, ncols = n_cols, randomChecksMap = w_map, kindExpt = input$kindExpt, 
                                               checks = 1:input$checks, myWay = input$myWay, Option_NCD = Option_NCD, 
                                               expt_name = name_expt, data_entry = data_entry, reps = NULL,
                                               data_dim_each_block = data_dim_each_block, w_map_letters1 = map_letters)
      }else if (input$kindExpt == "SUDC") {
        Name_expt <- as.vector(unlist(strsplit(input$expt_name, ",")))
        blocks <- 1
        if (length(Name_expt) == blocks && !is.null(Name_expt)) {
          name_expt <- Name_expt
        }else{
          name_expt = paste0(rep("Block", times = blocks), 1:blocks)
        }
        map_letters <- rand_lines()[[1]]$w_map_letter
        split_name_diagonal1 <- names_diagonal(nrows = n_rows, ncols = n_cols, randomChecksMap = w_map, kindExpt = input$kindExpt, 
                                               checks = 1:input$checks, myWay = input$myWay, Option_NCD = Option_NCD, 
                                               expt_name = name_expt, data_entry = data_entry, reps = NULL,
                                               data_dim_each_block = NULL, w_map_letters1 = map_letters)
      }
    })
    
    put_Filler_in_name <- reactive({
      req(rand_lines())
      req(input$n_rows, input$n_cols)
      # r_map <- rand_lines()$rand
      r_map <- rand_lines()[[1]]$rand
      if("Filler" %in% r_map) Option_NCD <- TRUE else Option_NCD <- FALSE
      
      if (input$kindExpt != "DBUDC" && Option_NCD == TRUE) {
        blocks <- 1
        if (input$expt_name != "") {
          Name_expt <- input$expt_name 
        }else Name_expt = paste0(rep("Expt1", times = blocks), 1:blocks)
        
        split_names <- matrix(data = Name_expt, ncol = input$n_cols, nrow = input$n_rows)
        # r_map <- rand_lines()$rand
        r_map <- rand_lines()[[1]]$rand
        Fillers <- sum(r_map == "Filler")
        if (input$n_rows %% 2 == 0) {
          if(input$planter_mov1 == "serpentine") {
            split_names[1, 1:Fillers] <- "Filler"
          }else{
            split_names[1,((input$n_cols + 1) - Fillers):input$n_cols] <- "Filler"
          }
        }else {
          split_names[1,((input$n_cols + 1) - Fillers):input$n_cols] <- "Filler"
        }
      }
      list(name_with_Fillers = split_names)
    })
    
    
    output$name_layout <- DT::renderDT({
      Option_NCD <- TRUE
      req(split_name_reactive()$my_names)
      my_names <- split_name_reactive()$my_names
      if (is.null(my_names))
        return(NULL)
      
      # w_map <- rand_checks()$map_checks
      # w_map <- user_location()$map_checks
      w_map <- rand_checks()[[1]]$map_checks
      if("Filler" %in% w_map) Option_NCD <- TRUE else Option_NCD <- FALSE
      if(input$kindExpt == "DBUDC") multi <- TRUE else multi <- FALSE
      
      if (multi == FALSE){
        if(Option_NCD == TRUE){
          my_names <- put_Filler_in_name()$name_with_Fillers
          blocks = 1
          if (input$expt_name != ""){
            Name_expt <- input$expt_name 
          }else Name_expt = paste0(rep("Expt1", times = blocks), 1:blocks)
          df <- as.data.frame(my_names)
          rownames(df) <- nrow(df):1
          options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "700px"))
          DT::datatable(df,
                        extensions = 'FixedColumns',
                        options = list(
                          dom = 't',
                          scrollX = TRUE,
                          fixedColumns = TRUE
                        )) %>% 
          DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                          backgroundColor = DT::styleEqual(Name_expt, c('yellow')))
        }else{
          blocks = 1
          if (input$expt_name != ""){
            Name_expt <- input$expt_name 
          }else Name_expt = paste0(rep("Expt1", times = blocks), 1:blocks)
          df <- as.data.frame(my_names)
          rownames(df) <- nrow(df):1
          # options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
          #                           scrollX = TRUE, scrollY = scrollY(input$n_rows)))
          # DT::datatable(df) %>%
          options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "700px"))
          DT::datatable(df,
                        extensions = 'FixedColumns',
                        options = list(
                          dom = 't',
                          scrollX = TRUE,
                          fixedColumns = TRUE
                        )) %>% 
          DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                          backgroundColor = DT::styleEqual(Name_expt, c('yellow')))
        }
      }else if(multi == TRUE){
        if(input$kindExpt == "DBUDC") { 
          if (input$myWay == "By Row") { 
            data_dim_each_block <- available_percent_table()$data_dim_each_block 
            my_row_sets <- automatically_cuts(data = w_map, planter_mov = input$planter_mov,
                                              way = "By Row", dim_data = data_dim_each_block)[[1]]
            blocks <- length(my_row_sets) 
          }else { 
            data_dim_each_block <- available_percent_table()$data_dim_each_block 
            cuts_by_c <- automatically_cuts(data = w_map, planter_mov = NULL, way = "By Column",
                                            dim_data = data_dim_each_block)  
            blocks <- length(cuts_by_c) 
          }  
          Name_expt <- as.vector(unlist(strsplit(input$expt_name, ","))) 
          if (length(Name_expt) == blocks) { 
            name_expt <- Name_expt 
          }else{ 
            name_expt = paste0(rep("Block", times = blocks), 1:blocks) 
          } 
          colores_back <- c('yellow', 'cadetblue', 'lightgreen', 'grey', 'tan', 'lightcyan',
                            'violet', 'thistle') 
          df <- as.data.frame(my_names) 
          rownames(df) <- nrow(df):1
          # options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
          #                           scrollX = TRUE, scrollY = scrollY(input$n_rows)))
          # DT::datatable(df) %>%
          options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "700px"))
          DT::datatable(df,
                        extensions = 'FixedColumns',
                        options = list(
                          dom = 't',
                          scrollX = TRUE,
                          fixedColumns = TRUE
                        )) %>% 
          DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                          backgroundColor = DT::styleEqual(name_expt, colores_back[1:blocks])
          ) 
        }else if(input$kindExpt == "RDC") { 
          reps <- as.numeric(input$n_reps) 
          Name_expt <- as.vector(unlist(strsplit(input$expt_name, ",")))[1] 
          if (Name_expt != "") { 
            Name_expt <- paste(Name_expt, "rep", sep = ".") 
            expe_names = paste0(rep(Name_expt, times = reps), 1:reps) 
          }else { 
            expe_names = paste0(rep("EXPT1_Rep", times = reps), 1:reps) 
          } 
          colores_back <- c('yellow', 'cadetblue', 'lightgreen', 'grey', 'tan', 'lightcyan',
                            'violet', 'thistle') 
          df <- as.data.frame(my_names) 
          rownames(df) <- nrow(df):1
          # options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
          #                           scrollX = TRUE, scrollY = scrollY(input$n_rows)))
          # DT::datatable(df) %>%
          options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "700px"))
          DT::datatable(df,
                        extensions = 'FixedColumns',
                        options = list(
                          dom = 't',
                          scrollX = TRUE,
                          fixedColumns = TRUE
                        )) %>% 
          DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                          backgroundColor = DT::styleEqual(expe_names, colores_back[1:reps])
          ) 
        } 
      } 
    })
    
    plot_number_reactive <- reactive({
      req(rand_lines())
      req(input$plot_start)
      # req(rand_checks()$map_checks)
      
      # req(user_location()$map_checks)
      req(input$n_rows, input$n_cols)
      req(split_name_reactive()$my_names)
      
      datos_name <- split_name_reactive()$my_names 
      datos_name = as.matrix(datos_name) 
      n_rows = input$n_rows; n_cols = input$n_cols 
      movement_planter = input$planter_mov
      
     # w_map <- rand_checks()$map_checks 
      # w_map <- user_location()$map_checks
      w_map <- rand_checks()[[1]]$map_checks
      if("Filler" %in% w_map) Option_NCD <- TRUE else Option_NCD <- FALSE
      if(input$kindExpt == "DBUDC") multi <- TRUE else multi <- FALSE
      
      if (multi == TRUE && Option_NCD == FALSE) { 
        if (input$kindExpt == "DBUDC") { 
          req(getData()$data_entry) 
          req(available_percent_table()$data_dim_each_block) 
          if (input$myWay == "By Row") { 
            data_dim_each_block <- available_percent_table()$data_dim_each_block 
            my_row_sets <- automatically_cuts(data = w_map, planter_mov = input$planter_mov,
                                              way = "By Row", dim_data = data_dim_each_block)[[1]]
            n_blocks <- length(my_row_sets) 
          }else { 
            data_dim_each_block <- available_percent_table()$data_dim_each_block 
            cuts_by_c <- automatically_cuts(data = w_map, planter_mov = NULL, way = "By Column",
                                            dim_data = data_dim_each_block)  
            n_blocks <- length(cuts_by_c) 
            m = diff(cuts_by_c) 
            my_col_sets = c(cuts_by_c[1], m) 
          } 
          Name_expt <- as.vector(unlist(strsplit(input$expt_name, ","))) 
          if (length(Name_expt) == n_blocks) { 
            expe_names <- Name_expt 
          }else{ 
            expe_names = paste0(rep("Block", times = n_blocks), 1:n_blocks) 
          } 
          plot_n_start <- as.numeric(as.vector(unlist(strsplit(input$plot_start, ",")))) 
          if (length(plot_n_start) > 1 && length(plot_n_start) < n_blocks) return(NULL) 
          if(input$myWay == "By Column"){
            
            my_split_plot_nub <- plot_number(movement_planter = input$planter_mov, n_blocks = n_blocks,
                                             n_rows = input$n_rows, n_cols = input$n_cols, plot_n_start = plot_n_start,
                                             datos = datos_name, expe_name = expe_names, ByRow = FALSE,
                                             my_row_sets = NULL, ByCol = TRUE, my_col_sets = my_col_sets) 
          }else{
            req(split_name_reactive()$my_names)
            datos_name <- split_name_reactive()$my_names 
            plot_n_start <- as.numeric(as.vector(unlist(strsplit(input$plot_start, ",")))) 
            data.dim.each <- available_percent_table()$data_dim_each_block
            Block_Fillers <- as.numeric(blocks_length()) 
            
            my_split_plot_nub <- plot_number_fillers(movement_planter = movement_planter, plot_n_start = plot_n_start,
                                                     datos = datos_name, expe_names = expe_names, ByRow = TRUE,
                                                     my_row_sets = my_row_sets, ByCol = FALSE, my_col_sets = NULL,
                                                     which.blocks = Block_Fillers, n_blocks = n_blocks,
                                                     data.dim.each = data.dim.each)
          }
          
        }
      }else if(multi == TRUE && Option_NCD == TRUE) {
        
        req(getData()$data_entry) 
        if (input$myWay == "By Row") { 
          data_dim_each_block <- available_percent_table()$data_dim_each_block 
          my_row_sets <- automatically_cuts(data = w_map, planter_mov = input$planter_mov,
                                            way = "By Row", dim_data = data_dim_each_block)[[1]]
          n_blocks <- length(my_row_sets) 
        }else { 
          data_dim_each_block <- available_percent_table()$data_dim_each_block 
          cuts_by_c <- automatically_cuts(data = w_map, planter_mov = NULL, way = "By Column",
                                          dim_data = data_dim_each_block)  
          n_blocks <- length(cuts_by_c) 
          m = diff(cuts_by_c)
          my_col_sets = c(cuts_by_c[1], m) 
        } 
        # w_map_letters1 <- rand_lines()$w_map_letters1 
        w_map_letters1 <- rand_lines()[[1]]$w_map_letters1 
        Name_expt <- as.vector(unlist(strsplit(input$expt_name, ","))) 
        if (length(Name_expt) == n_blocks) { 
          expe_names <- Name_expt 
        }else { 
          expe_names = paste0(rep("Block", times = n_blocks), 1:n_blocks) 
        } 
        if(input$myWay == "By Row") { 
          datos_name <- split_name_reactive()$my_names 
          plot_n_start <- as.numeric(as.vector(unlist(strsplit(input$plot_start, ",")))) 
          data.dim.each <- available_percent_table()$data_dim_each_block
          Block_Fillers <- as.numeric(blocks_length()) 
          
          my_split_plot_nub <- plot_number_fillers(movement_planter = input$planter_mov, plot_n_start = plot_n_start,
                                                   datos = datos_name, expe_names = expe_names, ByRow = TRUE,
                                                   my_row_sets = my_row_sets, ByCol = FALSE, my_col_sets = NULL,
                                                   which.blocks = Block_Fillers, n_blocks = n_blocks,
                                                   data.dim.each = data.dim.each) 
        }else { 
          return(NULL) 
        } 
      }else { 
        plot_n_start <- as.numeric(as.vector(unlist(strsplit(input$plot_start, ",")))) 
        n_blocks <- 1 
        if (input$expt_name != "") { 
          Name_expt <- input$expt_name  
        }else Name_expt = paste0(rep("Expt", times = n_blocks), 1:n_blocks)
        # w_map <- rand_checks()$map_checks #############
        # w_map <- user_location()$map_checks
        w_map <- rand_checks()[[1]]$map_checks
        if("Filler" %in% w_map) Option_NCD <- TRUE else Option_NCD <- FALSE #############
        my_split_plot_nub <- plot_number(movement_planter = input$planter_mov1, n_blocks = 1, n_rows = input$n_rows,
                                         n_cols = input$n_cols, plot_n_start = plot_n_start, datos = datos_name,
                                         expe_name =  Name_expt, ByRow = NULL, my_row_sets = NULL, ByCol = NULL,
                                         my_col_sets = NULL)
        
        if (Option_NCD == TRUE) { 
          # r_map <- rand_lines()$rand
          r_map <- rand_lines()[[1]]$rand
          Fillers <- sum(r_map == "Filler") 
          if (input$n_rows %% 2 == 0) { 
            if(input$planter_mov1 == "serpentine") { 
              my_split_plot_nub[[1]][1, 1:Fillers] <- 0 
            }else{ 
              my_split_plot_nub[[1]][1,((input$n_cols + 1) - Fillers):input$n_cols] <- 0 
            } 
          }else { 
            my_split_plot_nub[[1]][1,((input$n_cols + 1) - Fillers):input$n_cols] <- 0 
          } 
        } 
        my_split_plot_nub 
      } 
    })
    
    
    output$plot_number_layout <- DT::renderDT({
      req(plot_number_reactive()$w_map_letters1)
      plot_num <- plot_number_reactive()$w_map_letters1 
      if (is.null(plot_num))
        return(NULL)
      
      # w_map <- rand_checks()$map_checks 
      # w_map <- user_location()$map_checks
      w_map <- rand_checks()[[1]]$map_checks
      if("Filler" %in% w_map) Option_NCD <- TRUE else Option_NCD <- FALSE
      if(input$kindExpt == "DBUDC") multi <- TRUE else multi <- FALSE
      if (multi == TRUE) {
        if(input$myWay == "By Row") Option_NCD <- TRUE else Option_NCD <- FALSE
        if(Option_NCD == FALSE) { 
          x <- plot_number_reactive()$l 
          sub_len <- numeric()
          for (i in 1:length(x)){
            sub_len[i] <- length(x[[i]]) 
          } 
          a <- as.vector(unlist(x)) 
          colores_back <- c('yellow', 'cadetblue', 'lightgreen', 'grey', 'tan', 'lightcyan',
                            'violet', 'thistle') 
          my_colors <- list() 
          s = 1 
          for (i in sub_len) { 
            my_colors[[s]] <- rep(colores_back[s], i) 
            s = s + 1 
          } 
          my_colors <- unlist(my_colors) 
          df <- as.data.frame(plot_num)
          rownames(df) <- nrow(df):1
          options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "700px"))
          DT::datatable(df,
                        extensions = 'FixedColumns',
                        options = list(
                          dom = 't',
                          scrollX = TRUE,
                          fixedColumns = TRUE
                        )) %>% 
            DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                            backgroundColor = DT::styleEqual(a, my_colors)) 
        }else { 
          w_map_letters1 <- plot_number_reactive()$w_map_letters1 
          x <- plot_number_reactive()$target_num1 
          sub_len <- numeric()
          for (i in 1:length(x)){
            sub_len[i] <- length(x[[i]]) 
          } 
          a <- as.vector(unlist(x)) 
          colores_back <- c('yellow', 'cadetblue', 'lightgreen', 'grey', 'tan', 'lightcyan',
                            'violet', 'thistle') 
          my_colors <- list() 
          b = 1 
          for (i in sub_len) { 
            my_colors[[b]] <- rep(colores_back[b], i)
            b = b + 1 
          }
          colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                       'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
          
          my_colors <- unlist(my_colors)
          df <- as.data.frame(w_map_letters1)
          rownames(df) <- nrow(df):1
          options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "700px"))
          DT::datatable(df,
                        extensions = 'FixedColumns',
                        options = list(
                          dom = 't',
                          scrollX = TRUE,
                          fixedColumns = TRUE
                        )) %>% 
            DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                            backgroundColor = DT::styleEqual(c(a,0), c(my_colors, "red")))
        }
      }else if (multi == FALSE){
        
        if(Option_NCD == TRUE) {
          a <- as.vector(as.matrix(plot_num))
          a <- a[-which(a == 0)]
          len_a <- length(a)
          df <- as.data.frame(plot_num)
          rownames(df) <- nrow(df):1
          options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "700px"))
          DT::datatable(df,
                        extensions = 'FixedColumns',
                        options = list(
                          dom = 't',
                          scrollX = TRUE,
                          fixedColumns = TRUE
                        )) %>% 
          DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                          backgroundColor = DT::styleEqual(c(a,0), 
                                                           c(rep(c('yellow'), len_a),"red"))
          )
        }else{
          a <- as.vector(as.matrix(plot_num))
          len_a <- length(a)
          df <- as.data.frame(plot_num)
          rownames(df) <- nrow(df):1
          options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "700px"))
          DT::datatable(df,
                        extensions = 'FixedColumns',
                        options = list(
                          dom = 't',
                          scrollX = TRUE,
                          fixedColumns = TRUE
                        )) %>% 
          DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                          backgroundColor = DT::styleEqual(a, 
                                                           rep('yellow', length(a)))
            )
        }
      }
    })
    
    
    export_diagonal_design <- reactive({
      
      ## pre for
      locs_diagonal <- as.numeric(input$l.diagonal)
      final_expt_fieldbook <- vector(mode = "list",length = locs_diagonal)
      location_names <- as.vector(unlist(strsplit(input$Location, ",")))
      if (length(location_names) != locs_diagonal) location_names <- 1:locs_diagonal
      ## start for
      for (user_site in 1:locs_diagonal) {
        
        
        if(input$kindExpt == "DBUDC") multi <- TRUE else multi <- FALSE
        if(multi) req(getData()$data_entry)
        # loc_user_out_rand <- rand_lines()[[1]]
        # w_map <- loc_user_out_rand$rand
        loc_user_out_rand <- rand_checks()[[user_site]]
        w_map <- as.matrix(loc_user_out_rand$col_checks)
        
        if("Filler" %in% w_map) Option_NCD <- TRUE else Option_NCD <- FALSE
        # req(rand_lines())
        # req(rand_checks())
        req(split_name_reactive()$my_names)
        req(plot_number_reactive()$w_map_letters1)
        
        # if(is.null(loc_user_out_rand$rand)) return(NULL)
        movement_planter = input$planter_mov
        my_data_VLOOKUP <- getData()$data_entry
        COLNAMES_DATA <- colnames(my_data_VLOOKUP)
        if(Option_NCD == TRUE) {
          if(input$kindExpt != "DBUDC") {
            Entry_Fillers <- data.frame(list(0,"Filler"))
          }else {
            Entry_Fillers <- data.frame(list(0,"Filler", "NA"))
          }
          colnames(Entry_Fillers) <- COLNAMES_DATA
          my_data_VLOOKUP <- rbind(my_data_VLOOKUP, Entry_Fillers)
        }
        
        plot_number <- as.matrix(plot_number_reactive()$w_map_letters1)
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
        if (input$kindExpt != "RDC") {
          results_to_export <- list(random_entries_map, plot_number, Col_checks, my_names)
          final_expt_export <- export_design(G = results_to_export, movement_planter = movement_planter,
                                             location = location_names[user_site], Year = NULL,
                                             data_file = my_data_VLOOKUP, reps = FALSE)
          final_expt_fieldbook[[user_site]] <- as.data.frame(final_expt_export)
          print(user_site)
          
        } else {
          if(is.null(split_name_reactive()$my_reps)) return(NULL)
          
          my_reps <- as.matrix(split_name_reactive()$my_reps)
          my_reps <- apply(my_reps, 2 ,as.numeric)
          results_to_export <- list(random_entries_map, plot_number, Col_checks, my_names, my_reps)
          
          final_expt_export <- export_design(G = results_to_export, movement_planter = movement_planter,
                                             location = input$Location, Year = NULL,
                                             data_file = my_data_VLOOKUP, reps = TRUE)
          final_expt_fieldbook[[user_site]] <- as.data.frame(final_expt_export)
          print(user_site)
        }
      }
      
      ## end for
      
      final_fieldbook <- dplyr::bind_rows(final_expt_fieldbook)
      
      if(Option_NCD == TRUE) {
        final_fieldbook$CHECKS <- ifelse(final_fieldbook$NAME == "Filler", 0, final_fieldbook$CHECKS)
        final_fieldbook$EXPT <- ifelse(final_fieldbook$EXPT == "Filler", 0, final_fieldbook$EXPT)
      }
      if(input$kindExpt == "DBUDC") {
        final_fieldbook <- final_fieldbook[,-11]
      }
      
      ID <- 1:nrow(final_fieldbook)
      final_fieldbook <- final_fieldbook[, c(6,7,9,4,2,3,5,1,10)]
      final_fieldbook_all_sites <- cbind(ID, final_fieldbook)
      colnames(final_fieldbook_all_sites)[10] <- "TREATMENT"
      
      # print(head(final_fieldbook_all_sites, 6))

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
    
    observeEvent(input$Simulate_Diagonal, {
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
      #req(export_diagonal_design()$final_expt)
      if(!is.null(valsDIAG$maxValue) && !is.null(valsDIAG$minValue) && !is.null(valsDIAG$trail)) {
        maxVal <- as.numeric(valsDIAG$maxValue)
        minVal <- as.numeric(valsDIAG$minValue)
        ROX_DIAG <- as.numeric(valsDIAG$ROX)
        ROY_DIAG <- as.numeric(valsDIAG$ROY)
        df_prep <- export_diagonal_design()$final_expt
        loc_levels_factors <- levels(factor(df_prep$LOCATION, unique(df_prep$LOCATION)))
        print(loc_levels_factors)
        # df_DIAG <- export_diagonal_design()$final_expt
        # fieldBook <- df_DIAG[, c(1,6,7,9)]
        nrows_diag <- as.numeric(input$n_rows)
        ncols_diag <- as.numeric(input$n_cols)
        seed_diag <- as.numeric(input$myseed)
        
        locs_diag <- as.numeric(input$l.diagonal)
        df_diag_list <- vector(mode = "list", length = locs_diag)
        df_simulation_list <- vector(mode = "list", length = locs_diag)
        w <- 1
        set.seed(seed_diag)
        for (sites in 1:locs_diag) {
          df_loc <- subset(df_prep, LOCATION == loc_levels_factors[w])
          fieldBook <- df_loc[, c(1,6,7,9)]
          dfSimulation <- AR1xAR1_simulation(nrows = nrows_diag, ncols = ncols_diag, ROX = ROX_DIAG, ROY = ROY_DIAG, 
                                             minValue = minVal, maxValue = maxVal, fieldbook = fieldBook, 
                                             trail = valsDIAG$trail, seed = NULL)
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

    
    output$dt5 <- DT::renderDT({
      df <- simudata_DIAG()$df
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "600px"))
      DT::datatable(df, rownames = FALSE, options = list(
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
    
    # Downloadable xlsx of selected dataset ----
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
    
    # output$reportRandom <- downloadHandler(
    #   filename = "report.pdf",
    #   content = function(file) {
    #     params <- list(n = rand_lines()$rand)
    #     tempReport <- file.path(tempdir(), "report.Rmd")
    #     file.copy("report.Rmd", tempReport, overwrite = TRUE)
    #     id <- showNotification(
    #       "Rendering report...", 
    #       duration = NULL, 
    #       closeButton = FALSE
    #     )
    #     on.exit(removeNotification(id), add = TRUE)
    #     
    #     rmarkdown::render("report.Rmd", 
    #                       output_file = file,
    #                       params = params,
    #                       envir = new.env(parent = globalenv())
    #     )
    #   }
    # )
  })
}