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
                   
                   #numericInput(inputId = ns("myseed"), label = "Seed Number:", value = 1, min = 1),
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
                   
                   #textInput(ns("expt_name"), "Input Experiment Name:", value = "Expt1"),
                   
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
                            selectInput(inputId = ns("Dropdown"), label = "Choose of diagonal checks:", choices = "")
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            selectInput(inputId = ns("checks"), label = "Input # of Checks:",
                                        choices = c(1:10), multiple = FALSE, selected = 4)
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
                                    conditionalPanel(condition = "input.kindExpt == 'DBUDC'", ns = ns,
                                                     
                                                     conditionalPanel(condition = "input.myWay == 'By Row'", ns = ns,
                                                                      
                                                                      selectInput(inputId = ns("Block_Fillers"), label = "Which Blocks:",
                                                                                  choices = "Block_Fillers", multiple = TRUE, selected = "")               
                                                     )
                                                     
                                                     # selectInput(inputId = ns("Block_Fillers"), label = "Which Blocks:",
                                                     #             choices = "Block_Fillers", multiple = TRUE, selected = "")               
                                    )
                                    # conditionalPanel(condition = "input.kindExpt == 'RDC'", ns = ns,
                                    #                  
                                    #                  selectInput(inputId = ns("n_reps"), label = "Input # of Reps:",
                                    #                              choices = c(2:20), multiple = FALSE),                
                                    # ),
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
          tabPanel("Expt Design Info", DT::DTOutput(ns("dt1"))),
          tabPanel("Input Data",
                   fluidRow(
                     column(6,DT::DTOutput(ns("dt8"))),
                     column(6,DT::DTOutput(ns("table1")))
                   )
          ),
          tabPanel("Diagonal Checks Layout", DT::DTOutput(ns("dt2"))),
          tabPanel("Randomized Field",
                   #column(width = 3, downloadButton(ns("reportRandom"))),
                   DT::DTOutput(ns("dt3"))),
          tabPanel("Plot Number Field", DT::DTOutput(ns("dt4"))),
          tabPanel("Expt Name", DT::DTOutput(ns("dt6"))),
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
            updateSelectInput(session, inputId = 'Block_Fillers', label = "Which Blocks will have Fil:",
                              choices = Block_levels, selected = Block_levels[selected])
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
            updateSelectInput(session, inputId = 'Block_Fillers', label = "Which Blocks will have Fil:",
                              choices = Block_levels, selected = Block_levels[selected])
          }
        }
        
      }
      
      dim_data_entry <- nrow(data_entry_UP)
      dim_data_1 <- nrow(data_entry_UP[(length(checksEntries) + 1):nrow(data_entry_UP), ])
      
      list(data_entry = data_entry_UP, dim_data_entry = dim_data_entry, dim_data_1 = dim_data_1)
      
    })
    
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
    
    available_percent1 <- reactive({
      Option_NCD <- TRUE
      checksEntries <- as.vector(getChecks()$checksEntries)
      if(input$kindExpt == "DBUDC" && input$myWay == "By Column") {
        Option_NCD <- FALSE
      }
      
      if (input$kindExpt != "SUDC") {
        planter_mov <- input$planter_mov
      }else planter_mov <- input$planter_mov1
      #auxavailable_percent
      available_percent(n_rows = input$n_rows, n_cols = input$n_cols, checks = checksEntries, Option_NCD = Option_NCD,
                        Visual_ch = input$Visual_ch, visualCheck = FALSE, kindExpt = input$kindExpt, myWay = input$myWay,
                        planter_mov1 = planter_mov, data = getData()$data_entry, dim_data = getData()$dim_data_entry,
                        dim_data_1 = getData()$dim_data_1, Block_Fillers = input$Block_Fillers)
    }) 
    
    rand_checks <- reactive({
      Option_NCD <- TRUE
      req(input$myseed)
      seed <- as.numeric(input$myseed)
      req(available_percent1()$dt)
      req(available_percent1()$d_checks)
      req(available_percent1()$P)
      checksEntries <- as.vector(getChecks()$checksEntries)
      if (input$kindExpt != "SUDC") {
        planter_mov <- input$planter_mov
      }else planter_mov <- input$planter_mov1
      random_checks(dt = available_percent1()$dt, d_checks = available_percent1()$d_checks, p = available_percent1()$P,
                    percent = input$Dropdown, kindExpt = input$kindExpt, planter_mov = planter_mov, Checks = checksEntries,
                    myWay = input$myWay, data = getData()$data_entry, data_dim_each_block = available_percent1()$data_dim_each_block,
                    n_reps = input$n_reps, seed = seed)
    }) 
    
    output$dt1 <- DT::renderDT({
      Option_NCD <- TRUE
      req(available_percent1()$dt)
      if (is.null(available_percent1()$dt)) shiny::validate(":smth is wromg with 'available_percent1()$dt'")
      my_out <- available_percent1()$dt
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
    
    output$dt2 <- DT::renderDT({
      Option_NCD <- TRUE
      multi <- input$kindExpt == "RDC" || input$kindExpt == "DBUDC"
      if (multi) req(getData()$data_entry)
      req(rand_checks()$map_checks)
      w_map <- rand_checks()$map_checks
      #w_map <- as.matrix(available_percent1()$d_checks[[19]])
      if (is.null(w_map))
        return(NULL)
      
      checks <- as.vector(getChecks()$checksEntries)
      len_checks <- length(checks)
      colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                   'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
      
      df <- as.data.frame(w_map)
      rownames(df) <- nrow(df):1
      # options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
      #                           scrollX = TRUE, scrollY = "1000px"))
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
                      backgroundColor = DT::styleEqual(c(checks,0,"Filler"), 
                                                       c(colores[1:len_checks],'yellow', 'snow')))
    })
    
    output$dt8 <- DT::renderDT({
      my_data <- getData()$data_entry
      df <- my_data
      a <- ncol(df) - 1
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "800px"))
      DT::datatable(df, rownames = FALSE, caption = 'List of Entries.', options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    })
    
    output$table1 <- DT::renderDT({
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
        req(rand_checks()$map_checks)
        if(is.null(rand_checks()$map_checks)) return(NULL)
        w_map <- rand_checks()$map_checks
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
    
    rand_lines <- reactive({ 
      Option_NCD <- TRUE
      req(input$n_rows, input$n_cols)
      req(available_percent1()$dt)
      req(available_percent1()$d_checks)
      req(rand_checks()$map_checks)
      req(getData()$data_entry)
      data_entry <- getData()$data_entry
      w_map <- rand_checks()$map_checks
      n_rows = input$n_rows; n_cols = input$n_cols
      my_split_r <- rand_checks()$map_checks
      checksEntries <- getChecks()$checksEntries
      checks <- as.numeric(input$checks)
      if(input$kindExpt == "DBUDC") multi <- TRUE else multi <- FALSE
      if (multi == TRUE) {
        map_checks <- rand_checks()$map_checks
        req(getData()$data_entry)
        data_entry <- getData()$data_entry
        #req(available_percent1()$data_dim_each_block)
        if (input$kindExpt == "DBUDC" && input$myWay == "By Row") {
          req(available_percent1()$data_dim_each_block)
          data_dim_each_block <- available_percent1()$data_dim_each_block
          my_row_sets <- automatically_cuts(data = map_checks, planter_mov = input$planter_mov,
                                            way = "By Row", dim_data = data_dim_each_block)[[1]]
          if(is.null(my_row_sets)) return(NULL)
          n_blocks <- length(my_row_sets)
        }else if (input$kindExpt == "DBUDC" && input$myWay == "By Column") {
          req(available_percent1()$data_dim_each_block)
          data_dim_each_block <- available_percent1()$data_dim_each_block
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
            req(available_percent1()$data_dim_each_block)
            data_random <- get_random(n_rows = input$n_rows, n_cols = input$n_cols, d_checks = my_split_r,
                                      reps = NULL, Fillers = TRUE, col_sets = my_col_sets, row_sets = NULL,
                                      checks = checksEntries,data = data_entry)
          }
        }else {
          if(input$kindExpt == "DBUDC" && Option_NCD == FALSE) {
            data_entry1 <- data_entry[(checks + 1):nrow(data_entry), ]
            data_random <- get_DBrandom(binaryMap = w_map, data_dim_each_block = data_dim_each_block, data_entries = data_entry1,
                                        planter = input$planter_mov)
            # data_random <- get_random(n_rows = input$n_rows, n_cols = input$n_cols, d_checks = my_split_r,
            #                           reps = NULL, Fillers = FALSE, col_sets = NULL, row_sets = my_row_sets,
            #                           checks = checksEntries,data = data_entry, data_dim_each_block = data_dim_each_block)
          }else if(input$kindExpt == "DBUDC" && Option_NCD == TRUE) {
            req(available_percent1()$data_dim_each_block)
            Block_Fillers <- as.numeric(input$Block_Fillers)
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
    })
    
    output$dt3 <- DT::renderDT({
      VisualCheck <- FALSE
      r_map <- rand_lines()$rand
      checksEntries <- getChecks()$checksEntries
      if (is.null(r_map))
        return(NULL)
      checks = checksEntries
      len_checks <- length(checks)
      if(input$kindExpt == "DBUDC") multi <- TRUE else multi <- FALSE
      
      if (multi == FALSE){
        df <- as.data.frame(r_map)
        colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                     'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
        s <- unlist(rand_lines()$Entries)
        rownames(df) <- nrow(df):1
        # options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
        #                           scrollX = TRUE, scrollY = "1000px"))
        options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "800px"))
        DT::datatable(df,
                      extensions = 'FixedColumns',
                      options = list(
                        dom = 't',
                        scrollX = TRUE,
                        fixedColumns = TRUE
                      )) %>% 
          DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                          backgroundColor = DT::styleEqual(c(checks), 
                                                           colores[1:len_checks])) %>% 
          DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                          backgroundColor = DT::styleEqual(s, 
                                                           rep('gray', length(s)
                                                           )
                          )
          )
      }else{
        req(getData()$data_entry)
        x <- rand_lines()$Entries
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
        if(VisualCheck == TRUE) {
          Visual_ch <- as.numeric(input$Visual_ch)
          df <- as.data.frame(r_map)
          rownames(df) <- nrow(df):1
          options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "850px"))
          DT::datatable(df,
                        extensions = 'FixedColumns',
                        options = list(
                          dom = 't',
                          scrollX = TRUE,
                          fixedColumns = TRUE
                        )) %>% 
          DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                          backgroundColor = DT::styleEqual(c(a, Visual_ch), c(my_colors, 'red')))
        }else{
          df <- as.data.frame(r_map)
          rownames(df) <- nrow(df):1
          options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "850px"))
          DT::datatable(df,
                        extensions = 'FixedColumns',
                        options = list(
                          dom = 't',
                          scrollX = TRUE,
                          fixedColumns = TRUE
                        )) %>% 
          DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                          backgroundColor = DT::styleEqual(a, my_colors))
        }
      }
    })
    
    
    split_name_reactive <- reactive({
      checksEntries <- getChecks()$checksEntries
      checks <- checksEntries
      req(rand_lines()$rand)
      req(input$n_rows, input$n_cols)
      req(rand_checks()$map_checks)
      data_entry <- getData()$data_entry
      w_map <- rand_checks()$map_checks
      if ("Filler" %in% w_map) Option_NCD <- TRUE else Option_NCD <- FALSE
      n_rows = input$n_rows; n_cols = input$n_cols
      
      if(input$kindExpt == "DBUDC") multi <- TRUE else multi <- FALSE
      
      if (input$myWay == "By Row" && input$kindExpt == "DBUDC") {
        map_letters <- rand_lines()$w_map_letter
        data_dim_each_block <- available_percent1()$data_dim_each_block
        Name_expt <- as.vector(unlist(strsplit(input$expt_name, ",")))
        blocks <- length(data_dim_each_block)
        if (length(Name_expt) == blocks) {
          name_expt <- Name_expt
        }else{
          name_expt = paste0(rep("Block", times = blocks), 1:blocks)
        }
        map_letters <- rand_lines()$w_map_letter
        checksEntries <- as.vector(getChecks()$checksEntries)
        split_name_diagonal1 <- names_dbrows(w_map = w_map, myWay = "By Row", kindExpt = "DBUDC", data_dim_each_block = data_dim_each_block,
                                             w_map_letters = map_letters, expt_name = name_expt, Checks = checksEntries)
      }else if (input$myWay == "By Column" && input$kindExpt == "DBUDC") {
        map_letters <- rand_lines()$w_map_letter
        data_dim_each_block <- available_percent1()$data_dim_each_block
        Name_expt <- as.vector(unlist(strsplit(input$expt_name, ",")))
        blocks <- length(data_dim_each_block)
        if (length(Name_expt) == blocks) {
          name_expt <- Name_expt
        }else{
          name_expt = paste0(rep("Block", times = blocks), 1:blocks)
        }
        map_letters <- rand_lines()$w_map_letter
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
        map_letters <- rand_lines()$w_map_letter
        split_name_diagonal1 <- names_diagonal(nrows = n_rows, ncols = n_cols, randomChecksMap = w_map, kindExpt = input$kindExpt, 
                                               checks = 1:input$checks, myWay = input$myWay, Option_NCD = Option_NCD, 
                                               expt_name = name_expt, data_entry = data_entry, reps = NULL,
                                               data_dim_each_block = NULL, w_map_letters1 = map_letters)
      }
    })
    
    put_Filler_in_name <- reactive({
      req(rand_lines()$rand)
      req(input$n_rows, input$n_cols)
      r_map <- rand_lines()$rand
      if("Filler" %in% r_map) Option_NCD <- TRUE else Option_NCD <- FALSE
      
      if (input$kindExpt != "DBUDC" && Option_NCD == TRUE) {
        blocks <- 1
        if (input$expt_name != "") {
          Name_expt <- input$expt_name 
        }else Name_expt = paste0(rep("Expt1", times = blocks), 1:blocks)
        
        split_names <- matrix(data = Name_expt, ncol = input$n_cols, nrow = input$n_rows)
        r_map <- rand_lines()$rand
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
    
    
    output$dt6 <- DT::renderDT({
      Option_NCD <- TRUE
      req(split_name_reactive()$my_names)
      my_names <- split_name_reactive()$my_names
      if (is.null(my_names))
        return(NULL)
      w_map <- rand_checks()$map_checks
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
            data_dim_each_block <- available_percent1()$data_dim_each_block 
            my_row_sets <- automatically_cuts(data = w_map, planter_mov = input$planter_mov,
                                              way = "By Row", dim_data = data_dim_each_block)[[1]]
            blocks <- length(my_row_sets) 
          }else { 
            data_dim_each_block <- available_percent1()$data_dim_each_block 
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
      req(rand_lines()$rand)
      req(input$plot_start)
      req(rand_checks()$map_checks)
      req(input$n_rows, input$n_cols)
      req(split_name_reactive()$my_names)
      
      datos_name <- split_name_reactive()$my_names 
      datos_name = as.matrix(datos_name) 
      n_rows = input$n_rows; n_cols = input$n_cols 
      movement_planter = input$planter_mov
      
      w_map <- rand_checks()$map_checks 
      if("Filler" %in% w_map) Option_NCD <- TRUE else Option_NCD <- FALSE
      if(input$kindExpt == "DBUDC") multi <- TRUE else multi <- FALSE
      
      if (multi == TRUE && Option_NCD == FALSE) { 
        if (input$kindExpt == "DBUDC") { 
          req(getData()$data_entry) 
          req(available_percent1()$data_dim_each_block) 
          if (input$myWay == "By Row") { 
            data_dim_each_block <- available_percent1()$data_dim_each_block 
            my_row_sets <- automatically_cuts(data = w_map, planter_mov = input$planter_mov,
                                              way = "By Row", dim_data = data_dim_each_block)[[1]]
            n_blocks <- length(my_row_sets) 
          }else { 
            data_dim_each_block <- available_percent1()$data_dim_each_block 
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
            data.dim.each <- available_percent1()$data_dim_each_block
            Block_Fillers <- as.numeric(input$Block_Fillers) 
            
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
          data_dim_each_block <- available_percent1()$data_dim_each_block 
          my_row_sets <- automatically_cuts(data = w_map, planter_mov = input$planter_mov,
                                            way = "By Row", dim_data = data_dim_each_block)[[1]]
          n_blocks <- length(my_row_sets) 
        }else { 
          data_dim_each_block <- available_percent1()$data_dim_each_block 
          cuts_by_c <- automatically_cuts(data = w_map, planter_mov = NULL, way = "By Column",
                                          dim_data = data_dim_each_block)  
          n_blocks <- length(cuts_by_c) 
          m = diff(cuts_by_c)
          my_col_sets = c(cuts_by_c[1], m) 
        } 
        w_map_letters1 <- rand_lines()$w_map_letters1 
        Name_expt <- as.vector(unlist(strsplit(input$expt_name, ","))) 
        if (length(Name_expt) == n_blocks) { 
          expe_names <- Name_expt 
        }else { 
          expe_names = paste0(rep("Block", times = n_blocks), 1:n_blocks) 
        } 
        if(input$myWay == "By Row") { 
          datos_name <- split_name_reactive()$my_names 
          plot_n_start <- as.numeric(as.vector(unlist(strsplit(input$plot_start, ",")))) 
          data.dim.each <- available_percent1()$data_dim_each_block
          Block_Fillers <- as.numeric(input$Block_Fillers) 
          
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
        w_map <- rand_checks()$map_checks #############
        if("Filler" %in% w_map) Option_NCD <- TRUE else Option_NCD <- FALSE #############
        my_split_plot_nub <- plot_number(movement_planter = input$planter_mov1, n_blocks = 1, n_rows = input$n_rows,
                                         n_cols = input$n_cols, plot_n_start = plot_n_start, datos = datos_name,
                                         expe_name =  Name_expt, ByRow = NULL, my_row_sets = NULL, ByCol = NULL,
                                         my_col_sets = NULL)
        
        if (Option_NCD == TRUE) { 
          r_map <- rand_lines()$rand 
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
    
    
    output$dt4 <- DT::renderDT({
      req(plot_number_reactive()$w_map_letters1)
      plot_num <- plot_number_reactive()$w_map_letters1 
      if (is.null(plot_num))
        return(NULL)
      
      w_map <- rand_checks()$map_checks 
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
          # options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
          #                           scrollX = TRUE, scrollY = "1000px"))
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
          # options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
          #                           scrollX = TRUE, scrollY = "1000px"))
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
                            backgroundColor = DT::styleEqual(c(a,0), c(my_colors, "red")))
        }
      }else if (multi == FALSE){
        
        if(Option_NCD == TRUE) {
          a <- as.vector(as.matrix(plot_num))
          a <- a[-which(a == 0)]
          len_a <- length(a)
          df <- as.data.frame(plot_num)
          rownames(df) <- nrow(df):1
          # options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
          #                           scrollX = TRUE, scrollY = "1000px"))
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
                          backgroundColor = DT::styleEqual(c(a,0), 
                                                           c(rep(c('yellow'), len_a),"red"))
          )
        }else{
          a <- as.vector(as.matrix(plot_num))
          len_a <- length(a)
          df <- as.data.frame(plot_num)
          rownames(df) <- nrow(df):1
          # options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
          #                           scrollX = TRUE, scrollY = "1000px"))
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
                          backgroundColor = DT::styleEqual(a, 
                                                           rep('yellow', length(a)))
            )
        }
      }
    })
    
    
    my_export <- reactive({
      if(input$kindExpt == "DBUDC") multi <- TRUE else multi <- FALSE
      if(multi) req(getData()$data_entry)
      w_map <- rand_checks()$map_checks 
      if("Filler" %in% w_map) Option_NCD <- TRUE else Option_NCD <- FALSE
      req(rand_lines()$rand)
      req(rand_checks()$col_checks)
      req(split_name_reactive()$my_names)
      req(plot_number_reactive()$w_map_letters1)
      
      if(is.null(rand_lines()$rand)) return(NULL)
      movement_planter = input$planter_mov
      random_entries_map <- rand_lines()$rand
      random_entries_map[random_entries_map == "Filler"] <- 0
      random_entries_map <- apply(random_entries_map, 2 ,as.numeric)
      my_data_VLOOKUP <- getData()$data_entry
      COLNAMES_DATA <- colnames(my_data_VLOOKUP)
      if(Option_NCD == TRUE) {
        #Entry_Fillers <- data.frame(list(0,"Filler", "NA", "NA", "NA"))
        if(input$kindExpt != "DBUDC") {
          Entry_Fillers <- data.frame(list(0,"Filler"))
        }else {
          Entry_Fillers <- data.frame(list(0,"Filler", "NA"))
        }
        colnames(Entry_Fillers) <- COLNAMES_DATA
        my_data_VLOOKUP <- rbind(my_data_VLOOKUP, Entry_Fillers)
      }
      Col_checks <- as.matrix(rand_checks()$col_checks)
      #Col_checks <- apply(Col_checks, 2 ,as.numeric)
      plot_number <- as.matrix(plot_number_reactive()$w_map_letters1)
      plot_number <- apply(plot_number, 2 ,as.numeric)
      my_names <- split_name_reactive()$my_names
      if (multi == FALSE && Option_NCD == TRUE) {
        my_names <- put_Filler_in_name()$name_with_Fillers
      }
      if(input$kindExpt != "RDC") {
        results_to_export <- list(random_entries_map, plot_number, Col_checks, my_names)
        final_expt_export <- export_design(G = results_to_export, movement_planter = movement_planter,
                                           location = input$Location, Year = 2020,
                                           data_file = my_data_VLOOKUP, reps = FALSE)
      }else {
        if(is.null(split_name_reactive()$my_reps)) return(NULL)
        
        my_reps <- as.matrix(split_name_reactive()$my_reps)
        my_reps <- apply(my_reps, 2 ,as.numeric)
        results_to_export <- list(random_entries_map, plot_number, Col_checks, my_names, my_reps)
        
        final_expt_export <- export_design(G = results_to_export, movement_planter = movement_planter,
                                           location = input$Location, Year = 2020,
                                           data_file = my_data_VLOOKUP, reps = TRUE)
      }
      if(Option_NCD == TRUE) {
        final_expt_export$CHECKS <- ifelse(final_expt_export$NAME == "Filler", 0, final_expt_export$CHECKS)
        final_expt_export$EXPT <- ifelse(final_expt_export$EXPT == "Filler", 0, final_expt_export$EXPT)
      }
      if(input$kindExpt == "DBUDC") {
        final_expt_export <- final_expt_export[,-11]
      }
      
      ID <- 1:nrow(final_expt_export)
      final_expt_export <- final_expt_export[, c(6,7,9,4,2,3,5,1,10)]
      final_expt_export_F <- cbind(ID, final_expt_export)
      colnames(final_expt_export_F)[10] <- "TREATMENT"
      
      list(final_expt = final_expt_export_F)
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
      req(my_export()$final_expt)
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
      req(my_export()$final_expt)
      if(!is.null(valsDIAG$maxValue) && !is.null(valsDIAG$minValue) && !is.null(valsDIAG$trail)) {
        maxVal <- as.numeric(valsDIAG$maxValue)
        minVal <- as.numeric(valsDIAG$minValue)
        ROX_DIAG <- as.numeric(valsDIAG$ROX)
        ROY_DIAG <- as.numeric(valsDIAG$ROY)
        df_DIAG <- my_export()$final_expt
        fieldBook <- df_DIAG[, c(1,6,7,9)]
        nrows_diag <- as.numeric(input$n_rows)
        ncols_diag <- as.numeric(input$n_cols)
        seed_diag <- as.numeric(input$myseed)
        dfSimulation <- AR1xAR1_simulation(nrows = nrows_diag, ncols = ncols_diag, ROX = ROX_DIAG, ROY = ROY_DIAG, 
                                           minValue = minVal, maxValue = maxVal, fieldbook = fieldBook, 
                                           trail = valsDIAG$trail, seed = seed_diag)
        dfSimulation <- dfSimulation$outOrder
        data_DIAG <- my_export()$final_expt
        df_DIAG <- cbind(data_DIAG, round(dfSimulation[,7],2))
        colnames(df_DIAG)[11] <- as.character(valsDIAG$trail)
        v <- 1
      }else {
        df_DIAG <- my_export()$final_expt
        v <- 2
      }
      if (v == 1) {
        return(list(df = df_DIAG, dfSimulation = dfSimulation))
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
      if(input$heatmap_Diagonal){
        w <- as.character(valsDIAG$trail)
        df <- simudata_DIAG()$dfSimulation
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