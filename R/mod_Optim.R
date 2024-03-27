#' Optim UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Optim_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Unreplicated Optimized Arrangement"),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        radioButtons(inputId = ns("owndataOPTIM"), 
                    label = "Import Entries' List?", 
                    choices = c("Yes", "No"), 
                    selected = "No",
                    inline = TRUE, 
                    width = NULL, 
                    choiceNames = NULL, 
                    choiceValues = NULL),
       conditionalPanel(
         condition = "input.owndataOPTIM == 'Yes'", 
         ns = ns,
         fluidRow(
          column(7, style=list("padding-right: 28px;"),
                  fileInput(ns("file3"), 
                            label = "Upload a CSV File:", 
                            multiple = FALSE)),
          column(5,style=list("padding-left: 5px;"),
                  radioButtons(ns("sep.OPTIM"), "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              selected = ","))
         )
       ),
       conditionalPanel(
         "input.owndataOPTIM != 'Yes'", 
          ns = ns,
          numericInput(ns("checks.s"),
                        label = "Input # of Checks:", 
                        value = 4,
                        min = 1),
          textInput(ns("amount.checks"), 
                    "Input # Check's Reps:",
                    value = "8,8,8,8"),
          numericInput(ns("lines.s"), 
                      label = "Input # of Entries:",
                      value = 280, min = 5)           
       ),
       selectInput(ns("planter_mov.spatial"), 
                   label = "Plot Order Layout:",
                   choices = c("serpentine", "cartesian"),
                   multiple = FALSE, 
                   selected = "serpentine"),
       fluidRow(
         column(6,
                style=list("padding-right: 28px;"),
                numericInput(inputId = ns("l.optim"), 
                             label = "Input # of Locations:", 
                             value = 1,
                             min = 1)
         ),
         column(6,style=list("padding-left: 5px;"),
                selectInput(inputId = ns("locView.optim"), 
                            label = "Choose location to view:", 
                            choices = 1:1, 
                            selected = 1,
                            multiple = FALSE)
         )
       ),
       fluidRow(
         column(6,style=list("padding-right: 28px;"),
                textInput(
                    ns("plot_start.spatial"), 
                    "Starting Plot Number:", 
                    value = 1
                )
         ),
         column(6,style=list("padding-left: 5px;"),
                textInput(ns("expt_name.spatial"), 
                          "Input Experiment Name:", 
                          value = "Expt1")
         )
       ),  
       
       fluidRow(
         column(
            width = 6,
            style=list("padding-right: 28px;"),
            numericInput(
                ns("seed.spatial"), 
                label = "Random Seed:", 
                value = 5,
                min = 1
            )
         ),
         column(6,style=list("padding-left: 5px;"),
                textInput(ns("Location.spatial"), 
                          "Input Location:", 
                          value = "FARGO")
         )
       ),
       fluidRow(
         column(6,
                actionButton(
                  inputId = ns("RUN.optim"), 
                  label = "Run!", 
                  icon = icon("circle-nodes", verify_fa = FALSE),
                  width = '100%'),
         ),
         column(6,
                actionButton(
                  ns("Simulate.optim"), 
                  label = "Simulate!", 
                  icon = icon("greater-than-equal", verify_fa = FALSE),
                  width = '100%'),
         )
       ),
       br(),
       uiOutput(ns("download_expt_optim"))
      ),
      mainPanel(
        width = 8,
        shinyjs::useShinyjs(),
        tabsetPanel(id = ns("tabset_optim"),
        tabPanel("Get Random", value = "tabPanel_optim",
          br(),
          shinyjs::hidden(
            selectInput(inputId = ns("dimensions.s"),
                            label = "Select dimensions of field:",
                            choices = "")
          ),
          shinyjs::hidden(
            actionButton(ns("get_random_optim"), label = "Randomize!")
          ),
          br(),
          br(),
          shinycssloaders::withSpinner(
            verbatimTextOutput(outputId = ns("summary_optim"), 
                               placeholder = FALSE), 
              type = 4
           )
        ),
          tabPanel("Data Input",
                   fluidRow(
                     column(6,DT::DTOutput(ns("data_input"))),
                     column(6,DT::DTOutput(ns("table_checks")))
                   )
          ),
          tabPanel("Randomized Field", DT::DTOutput(ns("RFIELD"))),
          tabPanel("Plot Number Field", DT::DTOutput(ns("PLOTFIELD"))),
          tabPanel("Field Book", DT::DTOutput(ns("OPTIMOUTPUT"))),
          tabPanel("Heatmap", 
            plotly::plotlyOutput(ns("heatmap"), width = "97%"))
         )
      )
    )
  )
}
#' Optim Server Functions
#'
#' @noRd 
mod_Optim_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    shinyjs::useShinyjs()

    optim_inputs <- eventReactive(input$RUN.optim, {
      planter_mov <- input$planter_mov.spatial
      expt_name <- as.character(input$expt_name.spatial)
      plotNumber <- as.numeric(as.vector(unlist(strsplit(input$plot_start.spatial, ","))))
      site_names <- as.character(as.vector(unlist(strsplit(input$Location.spatial, ","))))
      seed_number <- as.numeric(input$seed.spatial)
      sites = as.numeric(input$l.optim)
      return(list(sites = sites, 
                  location_names = site_names, 
                  seed_number = seed_number, 
                  plotNumber = plotNumber,
                  planter_mov = planter_mov,
                  expt_name = expt_name)) 
    })

    observeEvent(optim_inputs()$sites, {
      loc_user_view <- 1:as.numeric(optim_inputs()$sites)
      updateSelectInput(inputId = "locView.optim", 
                        choices = loc_user_view, 
                        selected = loc_user_view[1])
    })

    observeEvent(input$input.owndataOPTIM,
                 handlerExpr = updateTabsetPanel(session,
                                                 "tabset_optim",
                                                 selected = "tabPanel_optim"))
    observeEvent(input$RUN.optim,
                 handlerExpr = updateTabsetPanel(session,
                                                 "tabset_optim",
                                                 selected = "tabPanel_optim"))

    get_data_optim <- eventReactive(input$RUN.optim, {
      if (input$owndataOPTIM == "Yes") {
        req(input$file3)
        inFile <- input$file3
        data_ingested <- load_file(name = inFile$name, 
                                   path = inFile$datapat, 
                                   sep = input$sep.OPTIM,
                                   check = TRUE, 
                                   design = "optim")
        
        if (names(data_ingested) == "dataUp") {
          data_up <- data_ingested$dataUp
          data_up <- na.omit(data_up)
          data_up <- as.data.frame(data_up)
          if (ncol(data_up) < 3) {
            shinyalert::shinyalert(
              "Error!!", 
              "Data input needs at least three columns with: ENTRY, NAME and REPS.", 
              type = "error")
            return(NULL)
          } 
          data_up <- as.data.frame(data_up[,1:3])
          data_up <- na.omit(data_up)
          colnames(data_up) <- c("ENTRY", "NAME", "REPS")
          if(!is.numeric(data_up$REPS) || !is.integer(data_up$REPS) ||
             is.factor(data_up$REPS)) validate("'REPS' must be numeric.")
          total_plots <- sum(data_up$REPS)
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
            "Data input needs at least three columns with: ENTRY, NAME and REPS.",
            type = "error")
          return(NULL)
        }
      } else {
        req(input$amount.checks)
        req(input$lines.s)
        req(input$checks.s)
        r.checks <- as.numeric(unlist(strsplit(input$amount.checks, ",")))
        checks.s <- as.numeric(input$checks.s)
        if(checks.s != length(r.checks)) {
          shinyalert::shinyalert(
            "Error!!",
            "The number of checks and the length of the reps vector must be equal.",
            type = "error"
          )
          return(NULL)
        } 
        total.checks <- sum(r.checks)
        n.checks <- as.numeric(input$checks.s)
        lines <- as.numeric(input$lines.s)
        if (lines <= sum(total.checks)) {
          shinyalert::shinyalert(
            "Error!!",
            "Number of lines should be greater then the number of checks.",
            type = "error"
          )
          return(NULL)
        }
        NAME <- c(paste0(rep("CH", n.checks), 1:n.checks),
                  paste(rep("G", lines), (n.checks + 1):(lines + n.checks), sep = ""))
        reps.checks <- r.checks
        REPS <- c(reps.checks, rep(1, lines))
        gen.list <- data.frame(list(ENTRY = 1:(lines + n.checks),	NAME = NAME,	REPS = REPS))
        data_up <- gen.list
        total_plots <- sum(data_up$REPS)
      }
      prime_factors <- numbers::primeFactors(total_plots)
      if (length(prime_factors) == 2) {
        if (prime_factors[1] < 4 & numbers::isPrime(prime_factors[2])) {
          shinyalert::shinyalert(
            "Error!!",
            "There are no options available for field dimensions. Please try a different number of treatments or checks.",
            type = "error"
          )
          return(NULL)
        }
      }
      if (numbers::isPrime(total_plots)) {
        shinyalert::shinyalert(
          "Error!!",
          "The number of field plots results in a prime number. Please try a different number of treatments.",
          type = "error"
        )
        return(NULL)
      }
      return(list(data_up.spatial = data_up, total_plots = total_plots))
    })
    
    list_inputs <- eventReactive(input$RUN.optim, {
      req(get_data_optim())
      if (input$owndataOPTIM != 'Yes') {
        req(input$amount.checks)
        req(input$lines.s)
        r.checks <- as.numeric(unlist(strsplit(input$amount.checks, ",")))
        lines <- as.numeric(input$lines.s)
        return(list(r.checks=r.checks, lines = lines, input$owndataOPTIM))
      } else {
        n_plots <- get_data_optim()$total_plots
        return(list(n_plots = n_plots, input$owndataOPTIM))
      }
    })
    
    observeEvent(list_inputs(), {
      req(get_data_optim())
      req(input$owndataOPTIM)
      if (input$owndataOPTIM != 'Yes') {
        req(input$amount.checks)
        req(input$lines.s)
        r.checks <- as.numeric(unlist(strsplit(input$amount.checks, ",")))
        lines <- as.numeric(input$lines.s)
        n <- sum(r.checks,lines)
        choices <- factor_subsets(n)$labels
      } else {
        req(get_data_optim()$total_plots)
        n <- get_data_optim()$total_plots
        choices <- factor_subsets(n)$labels
      }
      if(is.null(choices)){
        choices <- "No options available"
      }
      if (!is.null(choices)) {
        dif <- vector(mode = "numeric", length = length(choices))
        for (option in 1:length(choices)) {
          dims <- unlist(strsplit(choices[[option]], " x "))
          dif[option] <- abs(as.numeric(dims[1]) - as.numeric(dims[2]))
        }
        df_choices <- data.frame(choices = unlist(choices), diff_dim = dif)
        df_choices <- df_choices[order(df_choices$diff_dim, decreasing = FALSE), ]
        choices <- as.vector(df_choices$choices)
      }
      updateSelectInput(inputId = "dimensions.s", 
                        choices = choices, 
                        selected = choices[1])
    })
    
    field_dimensions_optim <- eventReactive(input$get_random_optim, {
      req(get_data_optim())
      dims <- unlist(strsplit(input$dimensions.s," x "))
      d_row <- as.numeric(dims[1])
      d_col <- as.numeric(dims[2])
      return(list(d_row = d_row, d_col = d_col))
    })

    randomize_hit_optim <- reactiveValues(times = 0)
 
    observeEvent(input$RUN.optim, {
      randomize_hit_optim$times <- 0
    })

    user_tries_optim <- reactiveValues(tries_optim = 0)

    observeEvent(input$get_random_optim, {
      user_tries_optim$tries_optim <- user_tries_optim$tries_optim + 1
      randomize_hit_optim$times <- randomize_hit_optim$times + 1
    })

    observeEvent(input$dimensions.s, {
      user_tries_optim$tries_optim <- 0
    })

    list_to_observe_optim <- reactive({
      list(randomize_hit_optim$times, user_tries_optim$tries_optim)
    })

    observeEvent(list_to_observe_optim(), {
      output$download_expt_optim <- renderUI({
        if (randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0) {
          downloadButton(ns("downloadData.spatial"),
                          "Save Experiment",
                          style = "width:100%")
        }
      })
    })

    entryListFormat_OPTIM <- data.frame(ENTRY = 1:9, 
                                        NAME = c(c("CHECK1", "CHECK2","CHECK3"), 
                                                 paste("Genotype", LETTERS[1:6], sep = "")),
                                        REPS = as.factor(c(rep(10, times = 3), rep(1,6))))
    
    entriesInfoModal_OPTIM <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        renderTable(entryListFormat_OPTIM,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        h4("Note that the controls must be in the first rows of the CSV file."),
        easyClose = FALSE
      )
    }
    
    toListen <- reactive({
      list(input$owndataOPTIM)
    })
    
    observeEvent(toListen(), {
      if (input$owndataOPTIM == "Yes") {
        showModal(
          entriesInfoModal_OPTIM()
        )
      }
    })

    observeEvent(input$RUN.optim, {
      req(get_data_optim())
      shinyjs::show(id = "dimensions.s")
      shinyjs::show(id = "get_random_optim")

    })

    output$data_input <- DT::renderDT({
      req(get_data_optim())
      if (input$dimensions.s == "No options available"){
        validate("No options available for this number of treatments")
      }
      test <- randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0
      if (!test) return(NULL)
      req(get_data_optim()$data_up.spatial)
      data_entry <- get_data_optim()$data_up.spatial
      df <- as.data.frame(data_entry)
      df$ENTRY <- as.factor(df$ENTRY)
      df$NAME <- as.factor(df$NAME)
      df$REPS <- as.factor(df$REPS)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "600px"))
      DT::datatable(df,
                    filter = "top",
                    rownames = FALSE, 
                    caption = 'List of Entries.', 
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")))
      )
    })
    
    output$table_checks <- DT::renderDT({
      req(get_data_optim())
      if (input$dimensions.s == "No options available") {
        validate("No options available for this number of treatments")
      }
      test <- randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0
      if (!test) return(NULL)
      req(get_data_optim()$data_up.spatial)
        data_entry <- get_data_optim()$data_up.spatial
        checks_input <- data_entry[data_entry$REPS > 1, ]
        df <- as.data.frame(checks_input)
        options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                  scrollX = TRUE, scrollY = "350px"))
        a <- ncol(df) - 1
        DT::datatable(df, rownames = FALSE, caption = 'Table of checks.', options = list(
          columnDefs = list(list(className = 'dt-left', targets = 0:a))))
    })
    
    optimized_arrang <- eventReactive(input$get_random_optim, { 
      req(get_data_optim())
      if (input$dimensions.s == "No options available") {
        validate("No options available for this number of treatments")
      }
      req(get_data_optim()$data_up.spatial)
      nrows <- field_dimensions_optim()$d_row
      ncols <- field_dimensions_optim()$d_col
      niter <- 1000
      
      data.spatial <- get_data_optim()$data_up.spatial
      sites <- optim_inputs()$sites
      site_names <- optim_inputs()$site_names
      seed.spatial <- optim_inputs()$seed_number
      plotNumber <- optim_inputs()$plotNumber
      movement_planter <- optim_inputs()$planter_mov
      expt_name <- optim_inputs()$expt_name

      optimized <- optimized_arrangement(
        nrows = nrows,
        ncols = ncols, 
        amountChecks = r.checks, 
        checks = n.checks,
        locationNames = site_names,
        planter = movement_planter,
        plotNumber = plotNumber,
        l = sites, 
        exptName = expt_name,
        optim = TRUE,
        seed = seed.spatial, 
        data = data.spatial
      )
    })

    output$summary_optim <- renderPrint({
      req(get_data_optim())
      test <- randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0
      #if (!test) return(NULL)
      if (test) {
        cat("Randomization was successful!", "\n", "\n")
        # len <- length(optimized_arrang()$infoDesign)
        #  optimized_arrang()$infoDesign[1:(len - 1)]
        print(optimized_arrang())
      }
    })

    user_site_selection <- reactive({
      return(as.numeric(input$locView.optim))
    })

    output$BINARY <- DT::renderDT({
      req(get_data_optim())
      test <- randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0
      if (!test) return(NULL)
      # if (user_tries_optim$tries_optim < 1) return(NULL)
      req(optimized_arrang())
      B <- optimized_arrang()$binaryField[[user_site_selection()]]
      df <- as.data.frame(B)
      rownames(df) <- nrow(df):1
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "700px"))
      DT::datatable(df,
                    extensions = 'FixedColumns',
                    options = list(
                      dom = 't',
                      scrollX = TRUE,
                      fixedColumns = TRUE
                    )) |> 
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                        backgroundColor = DT::styleEqual(1, 
                                                         c("gray")))
    })
    
    output$RFIELD <- DT::renderDT({
      test <- randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0
      if (!test) return(NULL)
      # if (user_tries_optim$tries_optim < 1) return(NULL)
      req(optimized_arrang())
      w_map <- optimized_arrang()$layoutRandom[[user_site_selection()]]
      checks = as.vector(optimized_arrang()$genEntries[[1]])
      len_checks <- length(checks)
      colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                   'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
      gens <- as.vector(optimized_arrang()$genEntries[[2]])
      df <- as.data.frame(w_map)
      rownames(df) <- nrow(df):1
      colnames(df) <- paste0('V', 1:ncol(df))
      DT::datatable(df,
                    extensions = c('Buttons', 'FixedColumns'),
                    options = list(dom = 'Blfrtip',
                                   scrollX = TRUE,
                                   fixedColumns = TRUE,
                                   pageLength = nrow(df),
                                   scrollY = "700px",
                                   class = 'compact cell-border stripe',  rownames = FALSE,
                                   server = FALSE,
                                   filter = list( position = 'top', clear = FALSE, plain =TRUE ),
                                   buttons = c('copy', 'excel'),
                                   lengthMenu = list(c(10,25,50,-1),
                                                     c(10,25,50,"All")))) |> 
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                    backgroundColor = DT::styleEqual(checks,
                                                 c(colores[1:len_checks])
                    )
        )
    })
    
    output$PLOTFIELD <- DT::renderDT({
      test <- randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0
      if (!test) return(NULL)
      # if (user_tries_optim$tries_optim < 1) return(NULL)
      req(optimized_arrang())
      plot_num <- optimized_arrang()$plotNumber[[user_site_selection()]]
      a <- as.vector(as.matrix(plot_num))
      len_a <- length(a)
      df <- as.data.frame(plot_num)
      rownames(df) <- nrow(df):1
      DT::datatable(df,
                    extensions = c('Buttons', 'FixedColumns'),
                    options = list(dom = 'Blfrtip',
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
    
    valsOPTIM <- reactiveValues(ROX = NULL, ROY = NULL, trail.optim = NULL, minValue = NULL,
                                maxValue = NULL)
    
    simuModal.OPTIM <- function(failed = FALSE) {
      modalDialog(
        fluidRow(
          column(6, 
                 selectInput(inputId = ns("trailsOPTIM"), label = "Select One:", 
                             choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
          ),
          column(6, 
                 checkboxInput(inputId = ns("heatmap_s"), label = "Include a Heatmap", value = TRUE),
          )
        ),
        conditionalPanel("input.trailsOPTIM == 'Other'", ns = ns,
                         textInput(inputId = ns("OtherOPTIM"), label = "Input Trial Name:", value = NULL)
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
                 numericInput(inputId = ns("min.optim"), "Input the min value:", value = NULL)
          ),
          column(6, 
                 numericInput(inputId = ns("max.optim"), "Input the max value:", value = NULL)
                 
          )
        ),
        if (failed)
          div(tags$b("Invalid input of data max and min", style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(inputId = ns("ok.optim"), "GO")
        )
      )
    }
    
    observeEvent(input$Simulate.optim, {
      req(optimized_arrang()$fieldBook)
      test <- randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0
      if (test) {
        showModal(
          simuModal.OPTIM()
        )
      }
    })
    
    observeEvent(input$ok.optim, {
      req(input$min.optim, input$max.optim)
      if (input$max.optim > input$min.optim & input$min.optim != input$max.optim) {
        valsOPTIM$maxValue <- input$max.optim
        valsOPTIM$minValue  <- input$min.optim
        valsOPTIM$ROX <- as.numeric(input$ROX.O)
        valsOPTIM$ROY <- as.numeric(input$ROY.O)
        if(input$trailsOPTIM == "Other") {
          req(input$OtherOPTIM)
          if(!is.null(input$OtherOPTIM)) {
            valsOPTIM$trail.optim <- as.character(input$OtherOPTIM)
          }else showModal(simuModal.OPTIM(failed = TRUE))
        }else {
          valsOPTIM$trail.optim <- as.character(input$trailsOPTIM)
        }
        removeModal()
      }else {
        showModal(
          simuModal.OPTIM(failed = TRUE)
        )
      }
    })
    
    simuDataOPTIM <- reactive({
      req(optimized_arrang()$fieldBook)
      if(!is.null(valsOPTIM$maxValue) & !is.null(valsOPTIM$minValue) & !is.null(valsOPTIM$trail.optim)) {
        maxVal <- as.numeric(valsOPTIM$maxValue)
        minVal <- as.numeric(valsOPTIM$minValue)
        ROX_O <- as.numeric(valsOPTIM$ROX)
        ROY_O <- as.numeric(valsOPTIM$ROY)
        df_optim <- optimized_arrang()$fieldBook
        locs <- length(levels(factor(df_optim$LOCATION)))
        nrows.s <- max(as.numeric(df_optim$ROW))
        ncols.s <- max(as.numeric(df_optim$COLUMN))
        loc_levels_factors <- levels(factor(df_optim$LOCATION, unique(df_optim$LOCATION)))
        
        seed.s <- as.numeric(input$seed.spatial)
        
        df_optim_list <- vector(mode = "list", length = locs)
        dfSimulationList <- vector(mode = "list", length = locs)
        do_sites <- 1:locs
        z <- 1
        set.seed(seed.s)
        for (sites in do_sites) {
          df_loc <- subset(df_optim, LOCATION == loc_levels_factors[z])
          fieldBook <- df_loc[, c(1,6,7,9)]
          dfSimulation <- AR1xAR1_simulation(nrows = nrows.s, ncols = ncols.s, 
                                             ROX = ROX_O, ROY = ROY_O, minValue = minVal, 
                                             maxValue = maxVal, fieldbook = fieldBook, 
                                             trail = valsOPTIM$trail.optim, 
                                             seed = NULL)
          dfSimulation <- dfSimulation$outOrder
          dfSimulationList[[sites]] <- dfSimulation
          dataOptim_loc <- df_loc
          df_optim_simu <- cbind(dataOptim_loc, round(dfSimulation[,7],2))
          colnames(df_optim_simu)[11] <- as.character(valsOPTIM$trail.optim)
          df_optim_list[[sites]] <- df_optim_simu 
          z <- z + 1
        }
        df_optim_locs <- dplyr::bind_rows(df_optim_list)
        v <- 1
      }else {
        dataOptim <- optimized_arrang()$fieldBook
        v <- 2
      }
      if (v == 1) {
        return(list(df = df_optim_locs, dfSimulation = dfSimulationList))
      }else if (v == 2) {
        return(list(df = dataOptim))
      }
    })

    heat_map_optim <- reactiveValues(heat_map_option = FALSE)
    
    observeEvent(input$ok.optim, {
      req(input$min.optim, input$max.optim)
      if (input$max.optim > input$min.optim & input$min.optim != input$max.optim) {
        heat_map_optim$heat_map_option <- TRUE
      }
    })
    
    observeEvent(heat_map_optim$heat_map_option, {
      if (heat_map_optim$heat_map_option == FALSE) {
        hideTab(inputId = "tabset_optim", target = "Heatmap")
      } else {
        showTab(inputId = "tabset_optim", target = "Heatmap")
      }
    })
    
    output$OPTIMOUTPUT <- DT::renderDT({
      test <- randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0
      if (!test) return(NULL)
      # if (user_tries_optim$tries_optim < 1) return(NULL)
      req(simuDataOPTIM()$df)
      df <- simuDataOPTIM()$df
      df$EXPT <- as.factor(df$EXPT)
      df$LOCATION <- as.factor(df$LOCATION)
      df$PLOT <- as.factor(df$PLOT)
      df$ROW <- as.factor(df$ROW)
      df$COLUMN <- as.factor(df$COLUMN)
      df$CHECKS <- as.factor(df$CHECKS)
      df$ENTRY <- as.factor(df$ENTRY)
      df$TREATMENT <- as.factor(df$TREATMENT)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "600px",
              columnDefs = list(list(className = 'dt-center', targets = "_all"))))
      DT::datatable(df,
        filter = "top",
        rownames = FALSE
      )
    })
    
    
    heatmap_obj <- reactive({
      req(simuDataOPTIM()$dfSimulation)
      if(input$heatmap_s) {
        w <- as.character(valsOPTIM$trail.optim)
        df <- simuDataOPTIM()$dfSimulation[[user_site_selection()]]
        df <- as.data.frame(df)
        p1 <- ggplot2::ggplot(df, ggplot2::aes(x = df[,4], y = df[,3], fill = df[,7], text = df[,8])) + 
          ggplot2::ggtitle("Heat map for yield") + 
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
      test <- randomize_hit_optim$times > 0 & user_tries_optim$tries_optim > 0
      if (!test) return(NULL)
      req(heatmap_obj())
      heatmap_obj()
    })
    
    output$downloadData.spatial <- downloadHandler(
      filename = function() {
        req(input$Location.spatial)
        loc <- input$Location.spatial
        loc <- paste(loc, "_", "Optim_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(simuDataOPTIM()$df, file, row.names = FALSE)
      }
    )
    
  })
}
