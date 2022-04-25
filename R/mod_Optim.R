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
       # numericInput(ns("checks.s"),
       #              label = "Input # of Checks:", 
       #              value = 3,
       #              min = 1),
       # 
       # numericInput(ns("tplots.s"), 
       #              label = "Input # of Total Check Plots:",
       #              value = 30,
       #              min = 1),
       conditionalPanel(
         "input.owndataOPTIM != 'Yes'", 
          ns = ns,
          
          numericInput(ns("checks.s"),
                        label = "Input # of Checks:", 
                        value = 3,
                        min = 1),
           
          numericInput(ns("tplots.s"), 
                        label = "Input # of Total Check Plots:",
                        value = 30,
                        min = 1),
            
          fluidRow(
            column(6,
                   style=list("padding-right: 28px;"),
                   textInput(ns("amount.checks"), 
                             "Input # Check's Reps:",
                             value = "10,10,10"),
            ),
            column(6,
                   style=list("padding-left: 5px;"),
                   numericInput(ns("lines.s"), 
                                label = "Input # of Entries:",
                                value = 270, min = 5)
            )
          )            
       ),
       
       fluidRow(
         column(6,
                style=list("padding-right: 28px;"),
                numericInput(ns("nrows.s"), 
                             label = "Input # of Rows:",
                             value = 15,
                             min = 5)
         ),
         column(6,
                style=list("padding-left: 5px;"),
                numericInput(ns("ncols.s"), 
                             label = "Input # of Columns:",
                             value = 20, 
                             min = 5)
         )
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
                numericInput(ns("seed.spatial"), 
                             label = "Seed Number:", 
                             value = 1,
                             min = 1)
         ),
         column(6,style=list("padding-left: 5px;"),
                textInput(ns("expt_name.spatial"), 
                          "Input Experiment Name:", 
                          value = "Expt1")
         )
       ),  
       
       fluidRow(
         column(6,style=list("padding-right: 28px;"),
                textInput(ns("plot_start.spatial"), 
                          "Starting Plot Number:", 
                          value = 1)
         ),
         column(6,style=list("padding-left: 5px;"),
                textInput(ns("Location.spatial"), 
                          "Input Location:", 
                          value = "FARGO")
         )
       ),
       fluidRow(
         column(6,
                actionButton(inputId = ns("RUN.optim"), 
                             "Run!", 
                             icon = icon("cocktail"), 
                             width = '100%'),
         ),
         column(6,
                actionButton(ns("Simulate.optim"), 
                             "Simulate!", 
                             icon = icon("cocktail"), 
                             width = '100%')
         )
       ),
       br(),
       downloadButton(ns("downloadData.spatial"), 
                      "Save Experiment", 
                      style = "width:100%")
       
       # fluidRow(
       #   column(6,
       #          downloadButton(ns("downloadData.spatial"), "Save Experiment", style = "width:100%")
       #   ),
       #   column(6,
       #          actionButton(ns("Simulate.optim"), "Simulate!", icon = icon("cocktail"), width = '100%')
       #   )
       # )
      ),
      
      mainPanel(
        width = 8,
        tabsetPanel(
          tabPanel("Data Input",
                   fluidRow(
                     column(6,DT::DTOutput(ns("data_input"))),
                     column(6,DT::DTOutput(ns("table_checks")))
                   )
          ),
          tabPanel("Matrix Checks", 
                   shinycssloaders::withSpinner(
                     DT::DTOutput(ns("BINARY")), 
                        type = 5)),
          tabPanel("Randomized Field", DT::DTOutput(ns("RFIELD"))),
          tabPanel("Plot Number Field", DT::DTOutput(ns("PLOTFIELD"))),
          tabPanel("Field Book", DT::DTOutput(ns("OPTIMOUTPUT"))),
          tabPanel(
            "Heatmap", 
            # selectInput(inputId = ns("locView.optim1"), 
            #             label = "Choose location to view:", 
            #             choices = 1:1, 
            #             selected = 1,
            #             multiple = FALSE),
            plotly::plotlyOutput(ns("heatmap")))
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
    
    some_inputs <- eventReactive(input$RUN.optim,{
      return(list(sites = input$l.optim))
    })
    
    observeEvent(some_inputs()$sites, {
      loc_user_view <- 1:as.numeric(some_inputs()$sites)
      updateSelectInput(inputId = "locView.optim", 
                        choices = loc_user_view, 
                        selected = loc_user_view[1])
    })
    getDataup.spatiaL <- eventReactive(input$RUN.optim, {  
      if (input$owndataOPTIM == "Yes") {
        req(input$file3)
        inFile <- input$file3
        data_up <- load_file(name = inFile$name, path = inFile$datapat, sep = input$sep.OPTIM)
        data_up <- as.data.frame(data_up)
        if (ncol(data_up) < 3) shiny::validate("Data input needs at least three columns with: ENTRY, NAME and REPS.")
        data_up <- as.data.frame(data_up[,1:3])
        data_up <- na.omit(data_up)
        colnames(data_up) <- c("ENTRY", "NAME", "REPS")
        if(!is.numeric(data_up$REPS) || !is.integer(data_up$REPS) ||
           is.factor(data_up$REPS)) validate("'REPS' must be numeric.")
      }else {
        req(input$checks.s)
        req(input$tplots.s)
        req(input$amount.checks)
        req(input$nrows.s, input$ncols.s)
        tplots <- as.numeric(input$tplots.s)
        r.checks <- as.numeric(unlist(strsplit(input$amount.checks, ",")))
        if (tplots != sum(r.checks)) validate("The number of total checks and the sum of replicates do not match.")
        checks.s <- as.numeric(input$checks.s)
        if(checks.s != length(r.checks)) validate("The number of checks and the length of the reps vector must be equal.")
        total.checks <- sum(r.checks)
        nrows <- as.numeric(input$nrows.s)
        ncols <- as.numeric(input$ncols.s)
        n.checks <- as.numeric(input$checks.s)
        lines <- as.numeric(input$lines.s)
        if (lines <= sum(total.checks)) validate("Number of lines should be greater then the number of checks.")
        NAME <- c(paste0(rep("CH", n.checks), 1:n.checks),
                  paste(rep("G", lines), (n.checks + 1):(lines + n.checks), sep = ""))
        reps.checks <- r.checks
        REPS <- c(reps.checks, rep(1, lines))
        gen.list <- data.frame(list(ENTRY = 1:(lines + n.checks),	NAME = NAME,	REPS = REPS))
        data_up <- gen.list
      }
     
      return(list(data_up.spatial = data_up))
      
    })
    
    
    output$data_input <- DT::renderDT({
      req(getDataup.spatiaL()$data_up.spatial)
      data_entry <- getDataup.spatiaL()$data_up.spatial
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
          shinyjqui::jqui_draggable(
            entriesInfoModal_OPTIM()
          )
        )
      }
    })
    
    output$table_checks <- DT::renderDT({
      req(getDataup.spatiaL()$data_up.spatial)
        data_entry <- getDataup.spatiaL()$data_up.spatial
        checks_input <- data_entry[data_entry$REPS > 1, ]
        df <- as.data.frame(checks_input)
        options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                  scrollX = TRUE, scrollY = "350px"))
        a <- ncol(df) - 1
        DT::datatable(df, rownames = FALSE, caption = 'Table of checks.', options = list(
          columnDefs = list(list(className = 'dt-left', targets = 0:a))))
    })
    
    Spatial_Checks <- eventReactive(input$RUN.optim, { 
      req(getDataup.spatiaL()$data_up.spatial)
      req(input$plot_start.spatial)
      req(input$nrows.s, input$ncols.s)
      req(input$seed.spatial)
      seed.spatial <- as.numeric(input$seed.spatial)
      nrows <- input$nrows.s
      ncols <- input$ncols.s
      niter <- 1000
      plotNumber <- as.numeric(input$plot_start.spatial)
      movement_planter <- input$planter_mov.spatial
      
      data.spatial <- getDataup.spatiaL()$data_up.spatial
      l.optim <- as.numeric(input$l.optim)
      expt_name <- as.character(input$expt_name.spatial)
      
      plotNumber <- as.numeric(as.vector(unlist(strsplit(input$plot_start.spatial, ","))))
      site_names <- as.character(as.vector(unlist(strsplit(input$Location.spatial, ","))))

      optimized <- optimized_arrangement(nrows = nrows,
                                         ncols = ncols, 
                                         amountChecks = r.checks, 
                                         checks = n.checks,
                                         locationNames = site_names,
                                         planter = movement_planter,
                                         plotNumber = plotNumber,
                                         l = l.optim, 
                                         exptName = expt_name,
                                         optim = TRUE,
                                         seed = seed.spatial, 
                                         data = data.spatial)
    })
    
    user_site_selection <- reactive({
      return(as.numeric(input$locView.optim))
    })

    output$BINARY <- DT::renderDT({
      req(Spatial_Checks())
      B <- Spatial_Checks()$binaryField[[user_site_selection()]]
      df <- as.data.frame(B)
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
                        backgroundColor = DT::styleEqual(1, 
                                                         c("gray")))
    })
    
    output$RFIELD <- DT::renderDT({
      req(Spatial_Checks())
      w_map <- Spatial_Checks()$layoutRandom[[user_site_selection()]]
      checks = as.vector(Spatial_Checks()$genEntries[[1]])
      len_checks <- length(checks)
      colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                   'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
      gens <- as.vector(Spatial_Checks()$genEntries[[2]])
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
                                                     c(10,25,50,"All")))) %>% 
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                    backgroundColor = DT::styleEqual(checks,
                                                 c(colores[1:len_checks])
                    )
        )
    })
    
    
    split_name_spatial <- reactive({
      req(Spatial_Checks())
      req(input$nrows.s, input$ncols.s)
      nrows <- as.numeric(input$nrows.s)
      ncols <- as.numeric(input$ncols.s)
      my_col_sets <- ncols
      blocks = 1
      if (input$expt_name.spatial != "") {
        Name_expt <- input$expt_name.spatial 
      }else Name_expt = paste0(rep("Expt1", times = blocks), 1:blocks)
      
      split_names <- split_name(n_rows = nrows, n_cols = ncols, Name_expt = Name_expt,
                                by_row = FALSE, col_sets = my_col_sets, row_sets = NULL)
      return(list(my_names = split_names))
    })
    
    output$PLOTFIELD <- DT::renderDT({
      req(Spatial_Checks())
      plot_num <- Spatial_Checks()$plotNumber[[user_site_selection()]]
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
      req(Spatial_Checks()$fieldBook)
      showModal(
        shinyjqui::jqui_draggable(
          simuModal.OPTIM()
        )
      )
    })
    
    observeEvent(input$ok.optim, {
      req(input$min.optim, input$max.optim)
      if (input$max.optim > input$min.optim && input$min.optim != input$max.optim) {
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
          shinyjqui::jqui_draggable(
            simuModal.OPTIM(failed = TRUE)
          )
        )
      }
    })
    
    simuDataOPTIM <- reactive({
      req(Spatial_Checks()$fieldBook)
      if(!is.null(valsOPTIM$maxValue) && !is.null(valsOPTIM$minValue) && !is.null(valsOPTIM$trail.optim)) {
        maxVal <- as.numeric(valsOPTIM$maxValue)
        minVal <- as.numeric(valsOPTIM$minValue)
        ROX_O <- as.numeric(valsOPTIM$ROX)
        ROY_O <- as.numeric(valsOPTIM$ROY)
        #locs <- as.numeric(input$l.optim)
        df_optim <- Spatial_Checks()$fieldBook
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
        dataOptim <- Spatial_Checks()$fieldBook
        v <- 2
      }
      if (v == 1) {
        return(list(df = df_optim_locs, dfSimulation = dfSimulationList))
      }else if (v == 2) {
        return(list(df = dataOptim))
      }
    })
    
    
    output$OPTIMOUTPUT <- DT::renderDT({
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
        
        p2 <- plotly::ggplotly(p1, tooltip="text", width = 1350, height = 740)
        
        return(p2)
      }
    })
    
    output$heatmap <- plotly::renderPlotly({
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
