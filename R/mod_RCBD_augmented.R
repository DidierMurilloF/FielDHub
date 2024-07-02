#' RCBD_augmented UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_RCBD_augmented_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("Augmented RCBD"),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        radioButtons(inputId = ns("owndata_a_rcbd"), 
                     label = "Import entries' list?", 
                     choices = c("Yes", "No"), 
                     selected = "No",
                     inline = TRUE, 
                     width = NULL, 
                     choiceNames = NULL, 
                     choiceValues = NULL),
        conditionalPanel(
          condition = "input.owndata_a_rcbd == 'Yes'", ns = ns,
          fluidRow(
            column(7, style=list("padding-right: 28px;"),
                   fileInput(ns("file1_a_rcbd"), 
                             label = "Upload a CSV File:", 
                             multiple = FALSE)),
            column(5,style=list("padding-left: 5px;"),
                   radioButtons(ns("sep.a_rcbd"), "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","))
          )              
        ),
        fluidRow(
          column(6,
                 style=list("padding-right: 28px;"),
                 numericInput(inputId = ns("nExpt_a_rcbd"), 
                              label = "Input # of Stacked Expts:",
                              value = 1, 
                              min = 1, 
                              max = 100)
          ),
          column(6,
                 style=list("padding-left: 5px;"),
                  checkboxInput(inputId = ns("random"), 
                                label = "Randomize Entries?",
                                value = TRUE)
          )
        ),
        
        conditionalPanel(
          condition = "input.owndata_a_rcbd == 'No'", 
          ns = ns,
          numericInput(inputId = ns("lines_a_rcbd"), 
                       label = "Input # of Entries:", 
                       value = 180)
        ),
        fluidRow(
          column(6,
                 style=list("padding-right: 28px;"),
                 numericInput(inputId = ns("checks_a_rcbd"), 
                              label = "Checks per Block:",
                              value = 4,
                              min = 1, 
                              max = 10)
          ),
          column(6,
                 style=list("padding-left: 5px;"),
                 selectInput(inputId = ns("blocks_a_rcbd"), 
                 label = "", choices = 5)
          )
        ),
        fluidRow(
          column(6,
                 style=list("padding-right: 28px;"),
                 numericInput(inputId = ns("l.arcbd"), 
                              label = "Input # of Locations:",
                              value = 1,
                              min = 1, 
                              max = 100),
          ),
          column(6,
                 style=list("padding-left: 5px;"),
                 selectInput(inputId = ns("locView.arcbd"), 
                             label = "Choose location to view:",
                             choices = 1:1, 
                             selected = 1, 
                             multiple = FALSE),
          )
        ),
        selectInput(inputId = ns("planter_mov1_a_rcbd"),
                    label = "Plot Order Layout:",
                    choices = c("serpentine", "cartesian"), 
                    multiple = FALSE,
                    selected = "serpentine"),
        fluidRow(
          column(6,
                 style=list("padding-right: 28px;"),
                 textInput(ns("plot_start_a_rcbd"), 
                           label = "Starting Plot Number:", 
                           value = 1)
          ),
          column(6,
                 style=list("padding-left: 5px;"),
                 textInput(ns("expt_name_a_rcbd"), 
                           label = "Input Experiment Name:", 
                           value = "Expt1")
          )
        ),  
        fluidRow(
          column(6,
                 style=list("padding-right: 28px;"),
                 numericInput(inputId = ns("myseed_a_rcbd"), 
                              label = "Random Seed:",
                              value = 1, 
                              min = 1)
          ),
          column(6,style=list("padding-left: 5px;"),
                 textInput(ns("Location_a_rcbd"), 
                           label = "Input Location:", 
                           value = "FARGO")
          )
        ),
        fluidRow(
          column(6,
                 actionButton(
                   inputId = ns("RUN.arcbd"), 
                   label = "Run!", 
                   icon = icon("circle-nodes", verify_fa = FALSE),
                   width = '100%'),
          ),
          column(6,
                 actionButton(
                   ns("Simulate.arcbd"), 
                   label = "Simulate!", 
                   icon = icon("greater-than-equal", verify_fa = FALSE),
                   width = '100%'),
          )
          
        ), 
        br(),
        uiOutput(ns("download_arcbd"))
      ),
      mainPanel(
        width = 8,
        shinyjs::useShinyjs(),
         tabsetPanel(id = ns("tabset_arcbd"),
           tabPanel("Get Random", value = "tabPanel_augmented",
                    br(),
                    shinyjs::hidden(
                      selectInput(inputId = ns("field_dims"),
                                  label = "Select dimensions of field:",
                                  choices = "")
                    ),
                    shinyjs::hidden(
                      actionButton(ns("get_random_augmented"), 
                      label = "Randomize!")
                    ),
                     br(),
                     br(),
                    div(
                     shinycssloaders::withSpinner(
                       verbatimTextOutput(outputId = ns("summary_augmented"), 
                                          placeholder = FALSE),
                      type = 4
                     ),
                     style = "padding-right: 40px;"
                    )
           ),
           tabPanel("Input Data",
                    fluidRow(
                      column(6,DT::DTOutput(ns("data_input"))),
                      column(6,DT::DTOutput(ns("checks_table")))
                    )
           ),
           tabPanel("Field Layout", br(), plotOutput(ns("field_layout"), width = "97%")),
           tabPanel("Plot Number Field", br(), plotOutput(ns("plot_number_layout2"), width = "97%")),
           tabPanel("Field Book", DT::DTOutput(ns("fieldBook_ARCBD"))),
           tabPanel("Heatmap", plotly::plotlyOutput(ns("heatmap"), width = "97%"))
         )      
      )
    )
  )
}

#' RCBD_augmented Server Functions
#'
#' @noRd 
mod_RCBD_augmented_server <- function(id) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
    
    shinyjs::useShinyjs()
    
    observeEvent(input$random, {
      if (input$random == FALSE) {
        shinyalert::shinyalert(
          "Warning!!", 
          "By unchecking this option you will only randomized the check plots.", 
          type = "warning")
      }
    })
    
    observeEvent(input$owndata_a_rcbd,
                 handlerExpr = updateTabsetPanel(session,
                                                 "tabset_arcbd",
                                                 selected = "tabPanel_augmented"))
    observeEvent(input$RUN.arcbd,
                 handlerExpr = updateTabsetPanel(session,
                                                 "tabset_arcbd",
                                                 selected = "tabPanel_augmented"))
    
    
    init_data <- reactive({
      if (input$owndata_a_rcbd == "Yes") {
        req(input$file1_a_rcbd)
        inFile <- input$file1_a_rcbd
        data_ingested <- load_file(name = inFile$name, 
                                   path = inFile$datapat, 
                                   sep = input$sep.a_rcbd, check = TRUE, design = "arcbd")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          if (ncol(data_up) < 2) {
            shinyalert::shinyalert(
              "Error!!", 
              "Data input needs at least two columns: ENTRY and NAME.", 
              type = "error")
            return(NULL)
          } 
          checks <- as.numeric(input$checks_a_rcbd)
          data_up <- as.data.frame(data_up[,1:2])
          data_up <- na.omit(data_up)
          colnames(data_up) <- c("ENTRY", "NAME")
          lines <- nrow(data_up) - checks
          if (lines < 8) {
            shinyalert::shinyalert(
              "Error!!", 
              "At least ten treatments are required!!", 
              type = "error")
            return(NULL)
          }
          return(list(error = FALSE, 
                      dataUp_a_rcbd = data_up,
                      entries = lines))
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
            "Data input needs at least three columns with: ENTRY and NAME.",
            type = "error")
          return(NULL)
        }
      } else {
        req(input$checks_a_rcbd)
        req(input$lines_a_rcbd)
        if (input$lines_a_rcbd < 8) {
          shinyalert::shinyalert(
            "Error!!", 
            "At least ten treatments are required!!", 
            type = "error")
          return(NULL)
        }
        lines <- as.numeric(input$lines_a_rcbd)
        checks <- as.numeric(input$checks_a_rcbd)
        if(lines < 1 || checks <= 0) validate("Number of lines and checks should be greater than 1.")
        NAME <- c(paste(rep("CH", checks), 1:checks, sep = ""),
                  paste(rep("G", lines), (checks + 1):(lines + checks), sep = ""))
        gen.list <- data.frame(list(ENTRY = 1:(lines + checks),	NAME = NAME))
        data_up <- gen.list
        return(list(dataUp_a_rcbd = data_up, 
                    entries = lines))
      }
    }) 

    
    list_to_observe <- reactive({
      req(init_data())
      list(
        entry_list = input$owndata_a_rcbd,
        checks = input$checks_a_rcbd, 
        entries = init_data()$entries
      )
    })
    
    observeEvent(list_to_observe(), {
      req(init_data()$entries)
      lines_arcbd <- as.numeric(list_to_observe()$entries)
      checks_arcbd <- as.numeric(list_to_observe()$checks)
      set_blocks <- set_augmented_blocks(
        lines = lines_arcbd, 
        checks = checks_arcbd
      )
      blocks_arcbd <- set_blocks$b
      if (length(blocks_arcbd) == 0) {
        shinyalert::shinyalert(
          "Error!!", 
          "No options available for that amount of treatments!!.", 
          type = "error")
      }
      updateSelectInput(session = session,
                        inputId = "blocks_a_rcbd",
                        label = "Input # of Blocks:", 
                        choices = blocks_arcbd, 
                        selected = blocks_arcbd[1])
    })
    
    observeEvent(input$RUN.arcbd, {
      req(init_data())
      req(input$owndata_a_rcbd)
      if (input$owndata_a_rcbd != 'Yes') {
        req(input$checks_a_rcbd)
        req(input$lines_a_rcbd)
        checks <- as.numeric(input$checks_a_rcbd)
        lines <- as.numeric(input$lines_a_rcbd)
        b <- as.numeric(input$blocks_a_rcbd)
        set_dims <- set_augmented_blocks(lines = lines, checks = checks)
        dim_options <- set_dims$blocks_dims
        blocks_dims <- as.data.frame(dim_options)
        set_choices_dims <- as.vector(subset(blocks_dims, blocks_dims[,1] == b)[,2])
        choices <- set_choices_dims
      } else {
        checks <- as.numeric(input$checks_a_rcbd)
        lines <- as.numeric(init_data()$entries)
        b <- as.numeric(input$blocks_a_rcbd)
        set_dims <- set_augmented_blocks(lines = lines, checks = checks)
        blocks_dims <- as.data.frame(set_dims$blocks_dims)
        set_choices_dims <- as.vector(subset(blocks_dims, blocks_dims[,1] == b)[,2])
        choices <- set_choices_dims
      }
      if(is.null(choices)) {
        choices <- "No options available"
      }
      updateSelectInput(inputId = "field_dims",
                        choices = choices,
                        selected = choices[1])
    })
    
    
    getDataup_a_rcbd <- eventReactive(input$RUN.arcbd, {
      if (is.null(init_data())) {
        shinyalert::shinyalert(
          "Error!!", 
          "Check input file and try again!", 
          type = "error")
        return(NULL)
      } else return(init_data())
    })
    
    
    some_inputs <- eventReactive(input$RUN.arcbd, {
      return(list(blocks = input$blocks_a_rcbd, 
                  entries = input$lines_a_rcbd, 
                  checks = as.numeric(input$checks_a_rcbd),
                  sites = input$l.arcbd,
                  expts_a_rcbd = input$nExpt_a_rcbd)
      )
    })
  
    
    list_inputs <- eventReactive(input$RUN.arcbd, {
      if (input$owndata_a_rcbd != 'Yes') {
        req(input$checks_a_rcbd)
        req(input$lines_a_rcbd)
        checks <- as.numeric(input$checks_a_rcbd)
        lines <- as.numeric(input$lines_a_rcbd)
        b <- as.numeric(input$blocks_a_rcbd)
        return(list(b = b, checks = checks, lines = lines, input$owndata_a_rcbd))
      } else {
        checks <- as.numeric(input$checks_a_rcbd)
        lines <- as.numeric(some_inputs()$entries)
        b <- as.numeric(input$blocks_a_rcbd)
        return(list(b = b, checks = checks, lines = lines, input$owndata_a_rcbd))
      }
    })
    

    
    field_dims_augmented <- eventReactive(input$get_random_augmented, {
      dims <- unlist(strsplit(input$field_dims, " x "))
      d_row <- as.numeric(dims[1])
      d_col <- as.numeric(dims[2])
      return(list(d_row = d_row, d_col = d_col))
    })
    
    randomize_hit_arcbd <- reactiveValues(times = 0)
    
    observeEvent(input$RUN.arcbd, {
      randomize_hit_arcbd$times <- 0
    })
    
    user_tries_arcbd <- reactiveValues(tries_arcbd = 0)
    
    observeEvent(input$get_random_augmented, {
      user_tries_arcbd$tries_arcbd <- user_tries_arcbd$tries_arcbd + 1
      randomize_hit_arcbd$times <- randomize_hit_arcbd$times + 1
    })
    
    observeEvent(input$field_dims, {
      user_tries_arcbd$tries_arcbd <- 0
    })
    
    list_to_observe_arcbd <- reactive({
      list(randomize_hit_arcbd$times, user_tries_arcbd$tries_arcbd)
    })
    
    test_arcbd <- reactive(return(randomize_hit_arcbd$times > 0 & user_tries_arcbd$tries_arcbd > 0))
    
    observeEvent(list_to_observe_arcbd(), {
      output$download_arcbd <- renderUI({
        if (test_arcbd()) {
          downloadButton(ns("downloadData_a_rcbd"),
                         "Save Experiment",
                         style = "width:100%")
        }
      })
    })
    
    output$data_input <- DT::renderDT({
      if(!test_arcbd()) return(NULL)
      req(getDataup_a_rcbd()$dataUp_a_rcbd)
      df <- getDataup_a_rcbd()$dataUp_a_rcbd
      df$ENTRY <- as.factor(df$ENTRY)
      df$NAME <- as.factor(df$NAME)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "600px"))
      DT::datatable(df,
                    filter = "top",
                    rownames = FALSE, 
                    caption = 'List of Entries.', 
                    options = list(
                      columnDefs = list(list(className = 'dt-center', 
                                             targets = "_all"))))
    })
    
    entryListFormat_ARCBD <- data.frame(ENTRY = 1:9, 
                                        NAME = c(c("CHECK1", "CHECK2","CHECK3"), 
                                                 paste("Genotype", 
                                                       LETTERS[1:6], 
                                                       sep = "")))
    entriesInfoModal_ARCBD <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        renderTable(entryListFormat_ARCBD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        h4("Note that the controls must be in the first rows of the CSV file."),
        easyClose = FALSE
      )
    }
    
    toListen <- reactive({
      list(input$owndata_a_rcbd)
    })
    
    observeEvent(toListen(), {
      if (input$owndata_a_rcbd == "Yes") {
        showModal(
          entriesInfoModal_ARCBD()
        )
      }
    })
    
    observeEvent(input$RUN.arcbd, {
      req(getDataup_a_rcbd())
      shinyjs::show(id = "field_dims")
      shinyjs::show(id = "get_random_augmented")
      
    })
    
    output$checks_table <- DT::renderDT({
      req(getDataup_a_rcbd()$dataUp_a_rcbd)
        data_entry <- getDataup_a_rcbd()$dataUp_a_rcbd
        df <- data_entry[1:(as.numeric(input$checks_a_rcbd)),]
        options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                  scrollX = TRUE, scrollY = "350px"))
        a <- ncol(df) - 1
        DT::datatable(df, rownames = FALSE, caption = 'Table of checks.', options = list(
          columnDefs = list(list(className = 'dt-left', targets = 0:a))))
    })
    
    rcbd_augmented_reactive <- reactive({
      req(getDataup_a_rcbd()$dataUp_a_rcbd)
      req(input$checks_a_rcbd)
      req(input$lines_a_rcbd)
      req(input$blocks_a_rcbd)
      req(input$planter_mov1_a_rcbd)
      req(input$plot_start_a_rcbd)
      req(input$myseed_a_rcbd)
      req(input$Location_a_rcbd)
      loc <- as.numeric(input$l.arcbd)
      checks <- as.numeric(input$checks_a_rcbd)
      if (input$owndata_a_rcbd == "Yes") {
        gen.list <- getDataup_a_rcbd()$dataUp_a_rcbd
        lines <- as.numeric(nrow(gen.list) - checks)
      } else {
        lines <- as.numeric(input$lines_a_rcbd)
        gen.list <- getDataup_a_rcbd()$dataUp_a_rcbd
      }
      b <- as.numeric(input$blocks_a_rcbd)
      seed.number <- as.numeric(input$myseed_a_rcbd)
      planter <- input$planter_mov1_a_rcbd
      l.arcbd <- as.numeric(input$l.arcbd)
      if (length(loc) > l.arcbd) {
        validate("Length of vector with name of locations is greater than the number of locations.")
      } 
      repsExpt <- some_inputs()$expts_a_rcbd
      nameexpt <- as.vector(unlist(strsplit(input$expt_name_a_rcbd, ",")))
      if (length(nameexpt) != 0) {
        Name_expt <- nameexpt
      }else Name_expt <- paste(rep('Expt', repsExpt), 1:repsExpt, sep = "")
      plotNumber <- as.numeric(as.vector(unlist(strsplit(input$plot_start_a_rcbd, ","))))
      site_names <- as.character(as.vector(unlist(strsplit(input$Location_a_rcbd, ","))))
      random <- input$random
      nrows <- field_dims_augmented()$d_row
      ncols <- field_dims_augmented()$d_col
      ARCBD <- RCBD_augmented(
        lines = lines,
        checks = checks,
        b = b,
        l = l.arcbd,
        planter = planter,
        plotNumber = plotNumber,
        exptName = Name_expt,
        seed = seed.number,
        locationNames = site_names,
        repsExpt = repsExpt,
        random = random, 
        data = gen.list,
        nrows = nrows,
        ncols = ncols
      )
      return(ARCBD)
    }) |> 
      bindEvent(input$get_random_augmented)
    
    reactive_layoutARCBD <- reactive({
      req(rcbd_augmented_reactive())
      obj_arcbd <- rcbd_augmented_reactive()
      loc_to_view <- as.numeric(input$locView.arcbd)
      try(
        plot_layout(x = obj_arcbd, l = loc_to_view),
        silent = TRUE
      )
    })

    output$field_layout <- renderPlot({
      req(reactive_layoutARCBD())
      req(rcbd_augmented_reactive())
      reactive_layoutARCBD()$out_layout
    }, height = 620)
    
    output$plot_number_layout2 <- renderPlot({
      req(reactive_layoutARCBD())
      req(rcbd_augmented_reactive())
      print(reactive_layoutARCBD()$out_layoutPlots)
      reactive_layoutARCBD()$out_layoutPlots
    }, height = 620)
    
    # arcbd_plot <- reactive({
    #   req(rcbd_augmented_reactive())
    #   loc_to_view <- as.numeric(input$locView.arcbd)
    #   arcbd_design <- rcbd_augmented_reactive()
    #   loc_field_book <- arcbd_design$fieldBook
    # 
    #   loc_field_book <- loc_field_book |>
    #     dplyr::mutate(LOC = factor(LOCATION, levels = unique(LOCATION))) |>
    #     dplyr::mutate(LOC = as.numeric(LOC))
    # 
    #   temp_field_book <- loc_field_book |> dplyr::filter(LOC == loc_to_view)
    # 
    #   rows <- length(unique(temp_field_book$ROW))
    #   cols <- length(unique(temp_field_book$COLUMN))
    #   temp_field_book$BLOCK <- as.factor(temp_field_book$BLOCK)
    #   main <- paste0("Augmented RCBD Layout ", rows, " x ", cols)
    #   p1 <- desplot::ggdesplot(
    #     BLOCK ~ COLUMN + ROW,
    #     text = ENTRY,
    #     col = CHECKS,
    #     cex = 1.2,
    #     out1 = EXPT,
    #     out2 = BLOCK,
    #     data = temp_field_book,
    #     xlab = "COLUMNS",
    #     ylab = "ROWS",
    #     main = main,
    #     show.key = FALSE,
    #     gg = TRUE,
    #     out2.gpar=list(col = "gray50", lwd = 1, lty = 1))
    #   # Explicitly remove all legends
    #   p1 <- p1 + ggplot2::guides(
    #     fill = "none",   # Remove legend for fill
    #     color = "none",  # Remove legend for color (if used)
    #     text = "none"    # Remove legend for text labels
    #   )
    # 
    #   # Precompute the breaks based on the data
    #   x_breaks <- seq(floor(min(temp_field_book$COLUMN)), ceiling(max(temp_field_book$COLUMN)), by = 1)
    #   y_breaks <- seq(floor(min(temp_field_book$ROW)), ceiling(max(temp_field_book$ROW)), by = 1)
    # 
    #   # Apply breaks to the plot
    #   p1 <- p1 +
    #     ggplot2::scale_x_continuous(breaks = x_breaks) +
    #     ggplot2::scale_y_continuous(breaks = y_breaks)
    # 
    #   # Apply a minimal theme for better aesthetics
    #   p1 <- p1 + ggplot2::theme_minimal() +
    #     ggplot2::theme(
    #       plot.title = ggplot2::element_text(face = "bold", size = 15),
    #       axis.title = ggplot2::element_text(size = 12),
    #       axis.text = ggplot2::element_text(size = 11)
    #     )
    #   return(p1)
    # })
    # 
    # output$field_layout <- renderPlot({
    #   arcbd_plot()
    # }, height = 650)

    output$randomized_layout2 <- plotly::renderPlotly({
      arcbd_plot()
    })
    
    output$summary_augmented <- renderPrint({
      if (test_arcbd()) {
        cat("Randomization was successful!", "\n", "\n")
        print(rcbd_augmented_reactive(), n = 6)
      }
    })
    
    observeEvent(some_inputs()$sites, {
      sites <- as.numeric(some_inputs()$sites)
      sites_to_view <- 1:sites 
      updateSelectInput(session = session, 
                        inputId = "locView.arcbd", 
                        choices = sites_to_view, 
                        selected = sites_to_view[1])
      
    })
    
    locNum <- reactive(
      return(as.numeric(input$locView.arcbd))
    )
    
    output$randomized_layout <- DT::renderDT({
      if(!test_arcbd()) return(NULL)
       r_map <- rcbd_augmented_reactive()$layout_random_sites[[locNum()]]
       checks <- 1:(as.numeric(some_inputs()$checks))
       b <- as.numeric(some_inputs()$blocks)
       len_checks <- length(checks)
       df <- as.data.frame(r_map)
       rownames(df) <- paste0("Row", nrow(df):1)
       repsExpt <- some_inputs()$expts_a_rcbd
       colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                    'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
       colnames(df) <- paste("V", 1:ncol(df), sep = "")
       options(DT.options = list(pageLength = nrow(df), 
                                 autoWidth = FALSE, 
                                 scrollY = "700px"))
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
                     ) |>
         DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                         backgroundColor = DT::styleEqual(c(checks),
                                                          colores[1:len_checks]))
     })
    
    output$expt_name_layout <- DT::renderDT({
      if(!test_arcbd()) return(NULL)
      req(rcbd_augmented_reactive())
      b <- as.numeric(some_inputs()$blocks)
      repsExpt <- some_inputs()$expts_a_rcbd
      name_expt <- as.vector(unlist(strsplit(input$expt_name_a_rcbd, ",")))
      if (length(name_expt) == repsExpt) {
        Name_expt <- name_expt
      }else Name_expt <- paste(rep('EXPT', repsExpt), 1:repsExpt, sep = "")
      df <-  as.data.frame(rcbd_augmented_reactive()$exptNames)
      colnames(df) <- paste("V", 1:ncol(df), sep = "")
      colores_back <- c('yellow', 'cadetblue', 'lightgreen', 'grey', 'tan', 'lightcyan',
                        'violet', 'thistle') 
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "700px"))
      DT::datatable(df,
                    extensions = 'FixedColumns',
                    options = list(
                      dom = 't',
                      scrollX = TRUE,
                      fixedColumns = TRUE
                    )) |>
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                        backgroundColor = DT::styleEqual(Name_expt, colores_back[1:repsExpt]))
    })

     # output$plot_number_layout <- DT::renderDT({
     #   if(!test_arcbd()) return(NULL)
     #   req(rcbd_augmented_reactive())
     #   plot_num1 <- rcbd_augmented_reactive()$layout_plots_sites[[locNum()]]
     #   b <- as.numeric(some_inputs()$blocks)
     #   infoDesign <- rcbd_augmented_reactive()$infoDesign
     #   Fillers <- as.numeric(infoDesign$fillers)
     #   repsExpt <- some_inputs()$expts_a_rcbd
     #   rownames(plot_num1) <- paste0("Row",nrow(plot_num1):1)
     #   if (Fillers == 0) {
     #     a <- as.vector(as.matrix(plot_num1))
     #     len_a <- length(a)
     #     df <- as.data.frame(plot_num1)
     #     colnames(df) <- paste("V", 1:ncol(df), sep = "")
     #     DT::datatable(df,
     #                   extensions = c('Buttons'),
     #                   options = list(dom = 'Blfrtip',
     #                                  autoWidth = FALSE,
     #                                  scrollX = TRUE,
     #                                  fixedColumns = TRUE,
     #                                  pageLength = nrow(df),
     #                                  scrollY = "700px",
     #                                  class = 'compact cell-border stripe',  
     #                                  rownames = FALSE,
     #                                  server = FALSE,
     #                                  filter = list( position = 'top', 
     #                                                 clear = FALSE, 
     #                                                 plain =TRUE ),
     #                                  buttons = c('copy', 'excel'),
     #                                  lengthMenu = list(c(10,25,50,-1),
     #                                                    c(10,25,50,"All")))
     #     )
     #   }else {
     #     a <- as.vector(as.matrix(plot_num1))
     #     a <- a[-which(a == 0)]
     #     len_a <- length(a)
     #     df <- as.data.frame(plot_num1)
     #     rownames(df) <- paste0("Row",nrow(df):1)
     #     colnames(df) <- paste("V", 1:ncol(df), sep = "")
     #     DT::datatable(df,
     #                   extensions = c('Buttons'),
     #                   options = list(dom = 'Blfrtip',
     #                                  autoWidth = FALSE,
     #                                  scrollX = TRUE,
     #                                  fixedColumns = TRUE,
     #                                  pageLength = nrow(df),
     #                                  scrollY = "700px",
     #                                  class = 'compact cell-border stripe',  rownames = FALSE,
     #                                  server = FALSE,
     #                                  filter = list( position = 'top', clear = FALSE, plain =TRUE ),
     #                                  buttons = c('copy', 'excel'),
     #                                  lengthMenu = list(c(10,25,50,-1),
     #                                                    c(10,25,50,"All")))
     #     )
     #   }
     # })
     
     valsARCBD <- reactiveValues(ROX = NULL, ROY = NULL, trail.arcbd = NULL, minValue = NULL,
                                 maxValue = NULL)

     simuModal.ARCBD <- function(failed = FALSE) {
       modalDialog(
         fluidRow(
           column(6,
                  selectInput(inputId = ns("trailsARCBD"), label = "Select One:",
                              choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
           ),
           column(6,
                  checkboxInput(inputId = ns("heatmap_s"), label = "Include a Heatmap", value = TRUE),
           )
         ),
         conditionalPanel("input.trailsARCBD == 'Other'", ns = ns,
                          textInput(inputId = ns("OtherARCBD"), label = "Input Trial Name:", value = NULL)
         ),
         fluidRow(
           column(6,
                  selectInput(inputId = ns("ROX.O"), "Select the Correlation in Rows:",
                              choices = seq(0.1, 0.9, 0.1), selected = 0.5)
           ),
           column(6,
                  selectInput(inputId = ns("ROY.O"), "Select the Correlation in Cols:",
                              choices = seq(0.1, 0.9, 0.1), selected = 0.5)
           )
         ),
         fluidRow(
           column(6,
                  numericInput(inputId = ns("min.arcbd"), "Input the min value:", value = NULL)
           ),
           column(6,
                  numericInput(inputId = ns("max.arcbd"), "Input the max value:", value = NULL)

           )
         ),
         if (failed)
           div(tags$b("Invalid input of data max and min", style = "color: red;")),

         footer = tagList(
           modalButton("Cancel"),
           actionButton(inputId = ns("ok.arcbd"), "GO")
         )
       )
     }

     observeEvent(input$Simulate.arcbd, {
       req(rcbd_augmented_reactive()$fieldBook)
       if(test_arcbd()) {showModal(
         simuModal.ARCBD()
       )}
     })

     observeEvent(input$ok.arcbd, {
       req(input$min.arcbd, input$max.arcbd)
       if (input$max.arcbd > input$min.arcbd && input$min.arcbd != input$max.arcbd) {
         valsARCBD$maxValue <- input$max.arcbd
         valsARCBD$minValue  <- input$min.arcbd
         valsARCBD$ROX <- as.numeric(input$ROX.O)
         valsARCBD$ROY <- as.numeric(input$ROY.O)
         if(input$trailsARCBD == "Other") {
           req(input$OtherARCBD)
           if(!is.null(input$OtherARCBD)) {
             valsARCBD$trail.arcbd <- as.character(input$OtherARCBD)
           }else showModal(simuModal.ARCBD(failed = TRUE))
         }else {
           valsARCBD$trail.arcbd <- as.character(input$trailsARCBD)
         }
         removeModal()
       }else {
         showModal(
           simuModal.ARCBD(failed = TRUE)
         )
       }
     })

     simuDataARCBD <- reactive({
       req(rcbd_augmented_reactive()$fieldBook)
       if(!is.null(valsARCBD$maxValue) && !is.null(valsARCBD$minValue) && !is.null(valsARCBD$trail.arcbd)) {
         maxVal <- as.numeric(valsARCBD$maxValue)
         minVal <- as.numeric(valsARCBD$minValue)
         ROX_O <- as.numeric(valsARCBD$ROX)
         ROY_O <- as.numeric(valsARCBD$ROY)
         df_arcbd <- rcbd_augmented_reactive()$fieldBook
         nrows.s <- length(levels(as.factor(df_arcbd$ROW)))
         ncols.s <- length(levels(as.factor(df_arcbd$COLUMN)))
         loc_levels_factors <- levels(factor(df_arcbd$LOCATION, unique(df_arcbd$LOCATION)))
         seed.s <- as.numeric(input$myseed_a_rcbd)
         locs <- length(loc_levels_factors)
         df_arcbd_list <- vector(mode = "list", length = locs)
         dfSimulationList <- vector(mode = "list", length = locs)
         do_sites <- 1:(length(loc_levels_factors))
         z <- 1
         set.seed(seed.s)
         for (sites in do_sites) {
           df_loc <- subset(df_arcbd, LOCATION == loc_levels_factors[z])
           fieldBook <- df_loc[, c(1,6,7,10)]
           dfSimulation <- AR1xAR1_simulation(nrows = nrows.s, ncols = ncols.s,
                                              ROX = ROX_O, ROY = ROY_O, minValue = minVal,
                                              maxValue = maxVal, fieldbook = fieldBook,
                                              trail = valsARCBD$trail.arcbd,
                                              seed = NULL)
           dfSimulation <- dfSimulation$outOrder
           dfSimulationList[[sites]] <- dfSimulation
           dataArcbd_loc <- df_loc
           df_arcbd_simu <- cbind(dataArcbd_loc, round(dfSimulation[,7],2))
           colnames(df_arcbd_simu)[12] <- as.character(valsARCBD$trail.arcbd)
           df_arcbd_list[[sites]] <- df_arcbd_simu
           z <- z + 1
         }
         df_arcbd_locs <- dplyr::bind_rows(df_arcbd_list)
         v <- 1
       }else {
         dataArcbd <- rcbd_augmented_reactive()$fieldBook
         v <- 2
       }
       if (v == 1) {
         return(list(df = df_arcbd_locs, dfSimulation = dfSimulationList))
       }else if (v == 2) {
         return(list(df = dataArcbd))
       }
       
     })
     
     heat_map_arcbd <- reactiveValues(heat_map_option = FALSE)
     
     observeEvent(input$ok.arcbd, {
       req(input$min.arcbd, input$max.arcbd)
       if (input$max.arcbd > input$min.arcbd && input$min.arcbd != input$max.arcbd) {
         heat_map_arcbd$heat_map_option <- TRUE
       }
     })
     
     observeEvent(heat_map_arcbd$heat_map_option, {
       if (heat_map_arcbd$heat_map_option == FALSE) {
         hideTab(inputId = "tabset_arcbd", target = "Heatmap")
       } else {
         showTab(inputId = "tabset_arcbd", target = "Heatmap")
       }
     })


     output$fieldBook_ARCBD <- DT::renderDT({
       if(!test_arcbd()) return(NULL)
       df <- simuDataARCBD()$df
       df$EXPT <- as.factor(df$EXPT)
       df$LOCATION <- as.factor(df$LOCATION)
       df$PLOT <- as.factor(df$PLOT)
       df$ROW <- as.factor(df$ROW)
       df$COLUMN <- as.factor(df$COLUMN)
       df$CHECKS <- as.factor(df$CHECKS)
       df$BLOCK <- as.factor(df$BLOCK)
       df$ENTRY <- as.factor(df$ENTRY)
       df$TREATMENT <- as.factor(df$TREATMENT)
        
       options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                 scrollX = TRUE, scrollCollapse=TRUE, scrollY = "600px"))
       DT::datatable(df, 
                     filter = "top",
                     rownames = FALSE, 
                     options = list(
                       columnDefs = list(list(className = 'dt-center', targets = "_all")))
       )
     })
     


     heatmap_obj <- reactive({
       req(simuDataARCBD()$dfSimulation)
       if(input$heatmap_s) {
         w <- as.character(valsARCBD$trail.arcbd)
         df <- simuDataARCBD()$dfSimulation[[locNum()]]
         df <- as.data.frame(df)
         p1 <- ggplot2::ggplot(df, ggplot2::aes(x = df[,4], y = df[,3], fill = df[,7], text = df[,8])) +
           ggplot2::geom_tile() +
           ggplot2::xlab("COLUMN") +
           ggplot2::ylab("ROW") +
           ggplot2::labs(fill = w) +
           viridis::scale_fill_viridis(discrete = FALSE)

         p2 <- plotly::ggplotly(p1, tooltip="text", height = 740)

         return(p2)
       }
     })

     output$heatmap <- plotly::renderPlotly({
       req(heatmap_obj())
       if(!test_arcbd()) return(NULL)
       heatmap_obj()
     })
     
     output$downloadData_a_rcbd <- downloadHandler(
       filename = function() {
         req(input$Location_a_rcbd)
         loc <- input$Location_a_rcbd
         loc <- paste(loc, "_", "ARCBD_", sep = "")
         paste(loc, Sys.Date(), ".csv", sep = "")
       },
       content = function(file) {
         df <- as.data.frame(simuDataARCBD()$df)
         write.csv(df, file, row.names = FALSE)
       }
     )
 
  })
}
