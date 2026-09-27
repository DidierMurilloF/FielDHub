#' RCBD_augmented UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
mod_RCBD_augmented_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Augmented RCBD"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        shiny::radioButtons(inputId = ns("owndata_a_rcbd"),
                     label = "Import entries' list?", 
                     choices = c("Yes", "No"), 
                     selected = "No",
                     inline = TRUE, 
                     width = NULL, 
                     choiceNames = NULL, 
                     choiceValues = NULL),
        shiny::conditionalPanel(
          condition = "input.owndata_a_rcbd == 'Yes'", ns = ns,
          shiny::fluidRow(
            shiny::column(7, style=list("padding-right: 28px;"),
                   shiny::fileInput(ns("file1_a_rcbd"),
                             label = "Upload a CSV File:", 
                             multiple = FALSE)),
            shiny::column(5,style=list("padding-left: 5px;"),
                   shiny::radioButtons(ns("sep.a_rcbd"), "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t"),
                                selected = ","))
          )              
        ),
        shiny::fluidRow(
          shiny::column(6,
                 style=list("padding-right: 28px;"),
                 shiny::numericInput(inputId = ns("nExpt_a_rcbd"),
                              label = "Input # of Stacked Expts:",
                              value = 1, 
                              min = 1, 
                              max = 100)
          ),
          shiny::column(6,
                 style=list("padding-left: 5px;"),
                 shiny::checkboxInput(inputId = ns("random"),
                               label = "Randomize Entries?",
                               value = TRUE)
          )
        ),
        
        # after the row where you set nExpt_a_rcbd ...
        
        shiny::conditionalPanel(
          condition = "input.nExpt_a_rcbd > 1", ns = ns,
          shiny::selectInput(
            inputId = ns("repsStack_a_rcbd"),
            label = "Stack experiments:",
            choices = c("vertical", "horizontal"),
            selected = "vertical",
            multiple = FALSE
          )
        ),
        
        shiny::conditionalPanel(
          condition = "input.owndata_a_rcbd == 'No'", 
          ns = ns,
          shiny::numericInput(inputId = ns("lines_a_rcbd"),
                       label = "Input # of Entries:", 
                       value = 180)
        ),
        shiny::fluidRow(
          shiny::column(6,
                 style=list("padding-right: 28px;"),
                 shiny::numericInput(inputId = ns("checks_a_rcbd"),
                              label = "Checks per Block:",
                              value = 4,
                              min = 1, 
                              max = 10)
          ),
          shiny::column(6,
                 style=list("padding-left: 5px;"),
                 shiny::selectInput(inputId = ns("blocks_a_rcbd"),
                             label = "", choices = 5)
          )
        ),
        shiny::fluidRow(
          shiny::column(6,
                 style=list("padding-right: 28px;"),
                 shiny::numericInput(inputId = ns("l.arcbd"),
                              label = "Input # of Locations:",
                              value = 1,
                              min = 1, 
                              max = 100),
          ),
          shiny::column(6,
                 style=list("padding-left: 5px;"),
                 shiny::selectInput(inputId = ns("locView.arcbd"),
                             label = "Choose location to view:",
                             choices = 1:1, 
                             selected = 1, 
                             multiple = FALSE),
          )
        ),
        shiny::selectInput(inputId = ns("planter_mov1_a_rcbd"),
                    label = "Plot Order Layout:",
                    choices = c("serpentine", "cartesian"), 
                    multiple = FALSE,
                    selected = "serpentine"),
        shiny::fluidRow(
          shiny::column(6,
                 style=list("padding-right: 28px;"),
                 shiny::textInput(ns("plot_start_a_rcbd"),
                           label = "Starting Plot Number:", 
                           value = 1)
          ),
          shiny::column(6,
                 style=list("padding-left: 5px;"),
                 shiny::textInput(ns("expt_name_a_rcbd"),
                           label = "Input Experiment Name:", 
                           value = "Expt1")
          )
        ),  
        shiny::fluidRow(
          shiny::column(6,
                 style=list("padding-right: 28px;"),
                 shiny::numericInput(inputId = ns("myseed_a_rcbd"),
                              label = "Random Seed:",
                              value = 1, 
                              min = 1)
          ),
          shiny::column(6,style=list("padding-left: 5px;"),
                 shiny::textInput(ns("Location_a_rcbd"),
                           label = "Input Location:", 
                           value = "FARGO")
          )
        ),
        shiny::fluidRow(
          shiny::column(6,
                 shiny::actionButton(
                   inputId = ns("RUN.arcbd"), 
                   label = "Run!", 
                   icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                   width = '100%'),
          ),
          shiny::column(6,
                 shiny::actionButton(
                   ns("Simulate.arcbd"), 
                   label = "Simulate!", 
                   icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                   width = '100%'),
          )
          
        ), 
        shiny::br(),
        shiny::uiOutput(ns("download_arcbd"))
      ),
      shiny::mainPanel(
        width = 8,
        shinyjs::useShinyjs(),
        shiny::tabsetPanel(id = ns("tabset_arcbd"),
                    shiny::tabPanel("Get Random", value = "tabPanel_augmented",
                             shiny::br(),
                             shinyjs::hidden(
                               shiny::selectInput(inputId = ns("field_dims"),
                                           label = "Select dimensions of field:",
                                           choices = "")
                             ),
                             shinyjs::hidden(
                               shiny::actionButton(ns("get_random_augmented"),
                                            label = "Randomize!")
                             ),
                             shiny::br(),
                             shiny::br(),
                             shiny::div(
                               fieldhub_spinner(
                                 shiny::verbatimTextOutput(outputId = ns("summary_augmented"),
                                                    placeholder = FALSE),
                                 type = 4
                               ),
                               style = "padding-right: 40px;"
                             )
                    ),
                    shiny::tabPanel("Input Data",
                             shiny::fluidRow(
                               shiny::column(6,DT::DTOutput(ns("data_input"))),
                               shiny::column(6,DT::DTOutput(ns("checks_table")))
                             )
                    ),
                    shiny::tabPanel("Field Layout", shiny::br(), shiny::plotOutput(ns("field_layout"), width = "97%")),
                    shiny::tabPanel("Plot Number Field", shiny::br(), shiny::plotOutput(ns("plot_number_layout"), width = "97%")),
                    shiny::tabPanel("Field Book", DT::DTOutput(ns("fieldBook_ARCBD"))),
                    shiny::tabPanel("Heatmap", plotly::plotlyOutput(ns("heatmap"), width = "97%"))
        )      
      )
    )
  )
}

#' RCBD_augmented Server Functions
#'
#' @noRd 
mod_RCBD_augmented_server <- function(id) {
  shiny::moduleServer( id, function(input, output, session) {
    ns <- session$ns
    
    shinyjs::useShinyjs()
    
    shiny::observeEvent(input$random, {
      if (input$random == FALSE) {
        shinyalert::shinyalert(
          "Warning!!", 
          "By unchecking this option you will only randomized the check plots.", 
          type = "warning")
      }
    })
    
    shiny::observeEvent(input$owndata_a_rcbd,
                 handlerExpr = shiny::updateTabsetPanel(session,
                                                 "tabset_arcbd",
                                                 selected = "tabPanel_augmented"))
    shiny::observeEvent(input$RUN.arcbd,
                 handlerExpr = shiny::updateTabsetPanel(session,
                                                 "tabset_arcbd",
                                                 selected = "tabPanel_augmented"))
    
    
    init_data <- shiny::reactive({
      if (input$owndata_a_rcbd == "Yes") {
        shiny::req(input$file1_a_rcbd)
        inFile <- input$file1_a_rcbd
        data_ingested <- load_file(name = inFile$name, 
                                   path = inFile[["datapath"]],
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
        } else {
          app_upload_error(data_ingested,
                           missing_columns = "Data input needs at least three columns with: ENTRY and NAME.")
          return(NULL)
        }
      } else {
        shiny::req(input$checks_a_rcbd)
        shiny::req(input$lines_a_rcbd)
        if (input$lines_a_rcbd < 8) {
          shinyalert::shinyalert(
            "Error!!",
            "At least ten treatments are required!!",
            type = "error")
          return(NULL)
        }
        lines <- as.numeric(input$lines_a_rcbd)
        checks <- as.numeric(input$checks_a_rcbd)
        if(lines < 1 || checks <= 0) shiny::validate("Number of lines and checks should be greater than 1.")
        gen.list <- dplyr::bind_rows(
          default_entries(checks, prefix = "CH"),
          default_entries(lines, prefix = "G", start = checks + 1)
        )
        data_up <- gen.list
        return(list(dataUp_a_rcbd = data_up, 
                    entries = lines))
      }
    }) 
    # |> 
    #   bindEvent(input$RUN.arcbd)
    
    
    list_to_observe <- shiny::reactive({
      shiny::req(init_data())
      list(
        entry_list = input$owndata_a_rcbd,
        checks = input$checks_a_rcbd, 
        entries = init_data()$entries
      )
    })
    
    shiny::observeEvent(list_to_observe(), {
      shiny::req(init_data()$entries)
      lines_arcbd <- as.numeric(list_to_observe()$entries)
      checks_arcbd <- as.numeric(list_to_observe()$checks)
      set_blocks <- set_augmented_blocks(
        lines = lines_arcbd, 
        checks = checks_arcbd, 
        start = 3
      )
      # print(set_blocks)
      blocks_arcbd <- set_blocks$b
      if (length(blocks_arcbd) == 0) {
        shinyalert::shinyalert(
          "Error!!", 
          "No options available for that amount of treatments!!.", 
          type = "error")
      }
      shiny::updateSelectInput(session = session,
                        inputId = "blocks_a_rcbd",
                        label = "Input # of Blocks:", 
                        choices = blocks_arcbd, 
                        selected = blocks_arcbd[1])
    })
    
    shiny::observeEvent(input$RUN.arcbd, {
      shiny::req(init_data())
      shiny::req(input$owndata_a_rcbd)
      if (input$owndata_a_rcbd != 'Yes') {
        shiny::req(input$checks_a_rcbd)
        shiny::req(input$lines_a_rcbd)
        checks <- as.numeric(input$checks_a_rcbd)
        lines <- as.numeric(input$lines_a_rcbd)
        b <- as.numeric(input$blocks_a_rcbd)
        set_dims <- set_augmented_blocks(lines = lines, checks = checks, start = 3)
        dim_options <- set_dims$blocks_dims
        blocks_dims <- as.data.frame(dim_options)
        set_choices_dims <- as.vector(subset(blocks_dims, blocks_dims[,1] == b)[,2])
        choices <- set_choices_dims
      } else {
        checks <- as.numeric(input$checks_a_rcbd)
        lines <- as.numeric(init_data()$entries)
        b <- as.numeric(input$blocks_a_rcbd)
        set_dims <- set_augmented_blocks(lines = lines, checks = checks, start = 3)
        blocks_dims <- as.data.frame(set_dims$blocks_dims)
        set_choices_dims <- as.vector(subset(blocks_dims, blocks_dims[,1] == b)[,2])
        choices <- set_choices_dims
      }
      if(is.null(choices)) {
        choices <- "No options available"
      }
      shiny::updateSelectInput(inputId = "field_dims",
                        choices = choices,
                        selected = choices[1])
    })
    
    
    getDataup_a_rcbd <- shiny::eventReactive(input$RUN.arcbd, {
      shiny::req(init_data())
      if (is.null(init_data())) {
        shinyalert::shinyalert(
          "Error!!", 
          "Check input file and try again!", 
          type = "error")
        return(NULL)
      } else return(init_data())
    })
    
    
    some_inputs <- shiny::eventReactive(input$RUN.arcbd, {
      return(list(blocks = input$blocks_a_rcbd, 
                  entries = input$lines_a_rcbd, 
                  checks = as.numeric(input$checks_a_rcbd),
                  sites = input$l.arcbd,
                  expts_a_rcbd = input$nExpt_a_rcbd)
      )
    })
    
    
    field_dims_augmented <- shiny::eventReactive(input$get_random_augmented, {
      dims <- unlist(strsplit(input$field_dims, " x "))
      d_row <- as.numeric(dims[1])
      d_col <- as.numeric(dims[2])
      return(list(d_row = d_row, d_col = d_col))
    })
    
    randomize_hit_arcbd <- shiny::reactiveValues(times = 0)
    
    shiny::observeEvent(input$RUN.arcbd, {
      randomize_hit_arcbd$times <- 0
    })
    
    user_tries_arcbd <- shiny::reactiveValues(tries_arcbd = 0)
    
    shiny::observeEvent(input$get_random_augmented, {
      user_tries_arcbd$tries_arcbd <- user_tries_arcbd$tries_arcbd + 1
      randomize_hit_arcbd$times <- randomize_hit_arcbd$times + 1
    })
    
    shiny::observeEvent(input$field_dims, {
      user_tries_arcbd$tries_arcbd <- 0
    })
    
    list_to_observe_arcbd <- shiny::reactive({
      list(randomize_hit_arcbd$times, user_tries_arcbd$tries_arcbd)
    })
    
    test_arcbd <- shiny::reactive(return(randomize_hit_arcbd$times > 0 & user_tries_arcbd$tries_arcbd > 0))
    
    shiny::observeEvent(list_to_observe_arcbd(), {
      output$download_arcbd <- shiny::renderUI({
        if (test_arcbd()) {
          shiny::downloadButton(ns("downloadData_a_rcbd"),
                         "Save experiment (ZIP)",
                         style = "width:100%")
        }
      })
    })
    
    output$data_input <- DT::renderDT({
      if(!test_arcbd()) return(NULL)
      shiny::req(getDataup_a_rcbd()$dataUp_a_rcbd)
      df <- getDataup_a_rcbd()$dataUp_a_rcbd
      df$ENTRY <- as.factor(df$ENTRY)
      df$NAME <- as.factor(df$NAME)
      table_options <- list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "600px")
      DT::datatable(df,
                    filter = "top",
                    rownames = FALSE, 
                    caption = 'List of Entries.', 
                    options = utils::modifyList(table_options, list(
                      columnDefs = list(list(className = 'dt-center', 
                                             targets = "_all")))))
    })
    
    entryListFormat_ARCBD <- data.frame(ENTRY = 1:9, 
                                        NAME = c(c("CHECK1", "CHECK2","CHECK3"), 
                                                 paste("Genotype", 
                                                       LETTERS[1:6], 
                                                       sep = "")))
    entriesInfoModal_ARCBD <- function() {
      shiny::modalDialog(
        title = shiny::div(shiny::tags$h3("Important message", style = "color: red;")),
        shiny::h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        shiny::renderTable(entryListFormat_ARCBD,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        shiny::h4("Note that the controls must be in the first rows of the CSV file."),
        easyClose = FALSE
      )
    }
    
    toListen <- shiny::reactive({
      list(input$owndata_a_rcbd)
    })
    
    shiny::observeEvent(toListen(), {
      if (input$owndata_a_rcbd == "Yes") {
        shiny::showModal(
          entriesInfoModal_ARCBD()
        )
      }
    })
    
    shiny::observeEvent(input$RUN.arcbd, {
      shiny::req(getDataup_a_rcbd())
      shinyjs::show(id = "field_dims")
      shinyjs::show(id = "get_random_augmented")
      
    })
    
    output$checks_table <- DT::renderDT({
      shiny::req(getDataup_a_rcbd()$dataUp_a_rcbd)
      data_entry <- getDataup_a_rcbd()$dataUp_a_rcbd
      df <- data_entry[1:(as.numeric(input$checks_a_rcbd)),]
      table_options <- list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "350px")
      a <- ncol(df) - 1
      DT::datatable(df, rownames = FALSE, caption = 'Table of checks.', options = utils::modifyList(table_options, list(
        columnDefs = list(list(className = 'dt-left', targets = 0:a)))))
    })
    
    rcbd_augmented_reactive <- shiny::reactive({
      shiny::req(getDataup_a_rcbd()$dataUp_a_rcbd)
      shiny::req(input$checks_a_rcbd)
      shiny::req(input$lines_a_rcbd)
      shiny::req(input$blocks_a_rcbd)
      shiny::req(input$planter_mov1_a_rcbd)
      shiny::req(input$plot_start_a_rcbd)
      shiny::req(input$myseed_a_rcbd)
      shiny::req(input$Location_a_rcbd)
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
        shiny::validate("Length of vector with name of locations is greater than the number of locations.")
      } 
      
      repsExpt <- some_inputs()$expts_a_rcbd
      repsStack <- NULL
      if (repsExpt > 1) {
        repsStack <- input$repsStack_a_rcbd
      }
      nameexpt <- as.vector(unlist(strsplit(input$expt_name_a_rcbd, ",")))
      if (length(nameexpt) != 0) {
        Name_expt <- nameexpt
      }else Name_expt <- paste(rep('Expt', repsExpt), 1:repsExpt, sep = "")
      plotNumber <- validate_design(read_whole_numbers(
        input$plot_start_a_rcbd, "Starting Plot Number"
      ))
      site_names <- as.character(as.vector(unlist(strsplit(input$Location_a_rcbd, ","))))
      random <- input$random
      nrows <- field_dims_augmented()$d_row
      ncols <- field_dims_augmented()$d_col
      ARCBD <- validate_design(RCBD_augmented(
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
        repsStack = repsStack,
        data = gen.list,
        nrows = nrows,
        ncols = ncols
      ))
      return(ARCBD)
    }) |> 
      shiny::bindEvent(input$get_random_augmented)
    
    reactive_layoutARCBD <- shiny::reactive({
      shiny::req(rcbd_augmented_reactive())
      obj_arcbd <- rcbd_augmented_reactive()
      loc_to_view <- as.numeric(input$locView.arcbd)
      try(
        plot_layout(x = obj_arcbd, l = loc_to_view),
        silent = TRUE
      )
    })
    
    output$field_layout <- shiny::renderPlot({
      shiny::req(reactive_layoutARCBD())
      shiny::req(rcbd_augmented_reactive())
      reactive_layoutARCBD()$out_layout
    }, height = 620, res = 100)
    
    output$plot_number_layout <- shiny::renderPlot({
      shiny::req(reactive_layoutARCBD())
      shiny::req(rcbd_augmented_reactive())
      print(reactive_layoutARCBD()$out_layoutPlots)
      reactive_layoutARCBD()$out_layoutPlots
    }, height = 620, res = 100)
    
    output$summary_augmented <- shiny::renderPrint({
      if (test_arcbd()) {
        cat("Randomization was successful!", "\n", "\n")
        print(rcbd_augmented_reactive(), n = 6)
      }
    })
    
    shiny::observeEvent(some_inputs()$sites, {
      sites <- as.numeric(some_inputs()$sites)
      sites_to_view <- 1:sites 
      shiny::updateSelectInput(session = session,
                        inputId = "locView.arcbd", 
                        choices = sites_to_view, 
                        selected = sites_to_view[1])
      
    })
    
    locNum <- shiny::reactive(
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
      table_options <- list(pageLength = nrow(df),
                                autoWidth = FALSE, 
                                scrollY = "700px")
      DT::datatable(df,
                    extensions = 'Buttons',
                    options = utils::modifyList(table_options, list(dom = 'Blfrtip',
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
                                                     c(10,25,50,"All"))))
      ) |>
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                        backgroundColor = DT::styleEqual(c(checks),
                                                         colores[1:len_checks]))
    })
    
    output$expt_name_layout <- DT::renderDT({
      if(!test_arcbd()) return(NULL)
      shiny::req(rcbd_augmented_reactive())
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
      table_options <- list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "700px")
      DT::datatable(df,
                    extensions = 'FixedColumns',
                    options = utils::modifyList(table_options, list(
                      dom = 't',
                      scrollX = TRUE,
                      fixedColumns = TRUE
                    ))) |>
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                        backgroundColor = DT::styleEqual(Name_expt, colores_back[1:repsExpt]))
    })
    
    valsARCBD <- shiny::reactiveValues(ROX = NULL, ROY = NULL, trail.arcbd = NULL, minValue = NULL,
                                maxValue = NULL)
    
    simuModal.ARCBD <- function(failed = FALSE) {
      shiny::modalDialog(
        shiny::fluidRow(
          shiny::column(6,
                 shiny::selectInput(inputId = ns("trailsARCBD"), label = "Select One:",
                             choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
          ),
          shiny::column(6,
                 shiny::checkboxInput(inputId = ns("heatmap_s"), label = "Include a Heatmap", value = TRUE),
          )
        ),
        shiny::conditionalPanel("input.trailsARCBD == 'Other'", ns = ns,
                         shiny::textInput(inputId = ns("OtherARCBD"), label = "Input Trial Name:", value = NULL)
        ),
        app_spatial_correlations(ns, ".O"),
        shiny::fluidRow(
          shiny::column(6,
                 shiny::numericInput(inputId = ns("min.arcbd"), "Input the min value:", value = NULL)
          ),
          shiny::column(6,
                 shiny::numericInput(inputId = ns("max.arcbd"), "Input the max value:", value = NULL)
                 
          )
        ),
        if (failed)
          shiny::div(shiny::tags$b("Invalid input of data max and min", style = "color: red;")),
        
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(inputId = ns("ok.arcbd"), "GO")
        )
      )
    }
    
    shiny::observeEvent(input$Simulate.arcbd, {
      shiny::req(rcbd_augmented_reactive()$fieldBook)
      if(test_arcbd()) {shiny::showModal(
        simuModal.ARCBD()
      )}
    })
    
    shiny::observeEvent(input$ok.arcbd, {
      shiny::req(input$min.arcbd, input$max.arcbd)
      if (input$max.arcbd > input$min.arcbd && input$min.arcbd != input$max.arcbd) {
        valsARCBD$maxValue <- input$max.arcbd
        valsARCBD$minValue  <- input$min.arcbd
        valsARCBD$ROX <- as.numeric(input$ROX.O)
        valsARCBD$ROY <- as.numeric(input$ROY.O)
        if(input$trailsARCBD == "Other") {
          shiny::req(input$OtherARCBD)
          if(!is.null(input$OtherARCBD)) {
            valsARCBD$trail.arcbd <- as.character(input$OtherARCBD)
          }else shiny::showModal(simuModal.ARCBD(failed = TRUE))
        }else {
          valsARCBD$trail.arcbd <- as.character(input$trailsARCBD)
        }
        shiny::removeModal()
      }else {
        shiny::showModal(
          simuModal.ARCBD(failed = TRUE)
        )
      }
    })
    
    simuDataARCBD <- shiny::reactive({
      shiny::req(rcbd_augmented_reactive()$fieldBook)
      field_book <- rcbd_augmented_reactive()$fieldBook
      if (is.null(valsARCBD$maxValue) || is.null(valsARCBD$minValue) ||
          is.null(valsARCBD$trail.arcbd)) {
        return(list(df = field_book, simulation = NULL))
      }
      simulation <- validate_design(simulate_spatial_field_book(
        field_book = field_book,
        nrows = length(unique(field_book$ROW)), ncols = length(unique(field_book$COLUMN)),
        correlation_x = as.numeric(valsARCBD$ROX),
        correlation_y = as.numeric(valsARCBD$ROY),
        min_value = as.numeric(valsARCBD$minValue),
        max_value = as.numeric(valsARCBD$maxValue),
        response_name = as.character(valsARCBD$trail.arcbd),
        seed = as.numeric(input$myseed_a_rcbd)
      ))
      list(df = simulation$field_book, dfSimulation = simulation$simulations,
           simulation = simulation)
    })

    heat_map_arcbd <- shiny::reactiveValues(heat_map_option = FALSE)
    
    shiny::observeEvent(input$ok.arcbd, {
      shiny::req(input$min.arcbd, input$max.arcbd)
      if (input$max.arcbd > input$min.arcbd && input$min.arcbd != input$max.arcbd) {
        heat_map_arcbd$heat_map_option <- TRUE
      }
    })
    
    shiny::observeEvent(heat_map_arcbd$heat_map_option, {
      if (heat_map_arcbd$heat_map_option == FALSE) {
        shiny::hideTab(inputId = "tabset_arcbd", target = "Heatmap")
      } else {
        shiny::showTab(inputId = "tabset_arcbd", target = "Heatmap")
      }
    })
    
    
    output$fieldBook_ARCBD <- DT::renderDT({
      if(!test_arcbd()) return(NULL)
      df <- simuDataARCBD()$df
      validate_design(app_field_book_table(
        df, factor_columns = c("EXPT", "LOCATION", "PLOT", "ROW", "COLUMN", "CHECKS", "BLOCK", "ENTRY", "TREATMENT"),
        height = 600, collapse = TRUE
      ))
    })
    
    heatmap_obj <- shiny::reactive({
      shiny::req(simuDataARCBD()$dfSimulation)
      shiny::req(input$heatmap_s)
      validate_design(app_spatial_heatmap(
        simuDataARCBD()$dfSimulation,
        response_name = as.character(valsARCBD$trail.arcbd),
        selected = locNum(), height = 740
      ))
    })
    
    output$heatmap <- plotly::renderPlotly({
      shiny::req(heatmap_obj())
      if(!test_arcbd()) return(NULL)
      heatmap_obj()
    })
    
    output$downloadData_a_rcbd <- app_csv_archive(
      filename = function() {
        shiny::req(input$Location_a_rcbd)
        loc <- input$Location_a_rcbd
        loc <- paste(loc, "_", "ARCBD_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      data = function() as.data.frame(simuDataARCBD()$df),
      design = rcbd_augmented_reactive,
      field_book = function() simuDataARCBD()$df,
      simulation = function() simuDataARCBD()$simulation,
      kind = "field_book"
    )
    
    app_reproduction_outputs(output, rcbd_augmented_reactive)
  })
}
