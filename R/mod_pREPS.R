#' pREPS UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_pREPS_ui <- function(id){
  ns <- NS(id)
  tagList(
    h4("Partially Replicated Designs"),
    sidebarLayout(
      sidebarPanel(width = 4,
                   radioButtons(inputId = ns("owndataPREPS"), label = "Import entries' list?", 
                                choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                   
                   conditionalPanel("input.owndataPREPS == 'Yes'", ns = ns,
                                    fluidRow(
                                      column(7, style=list("padding-right: 28px;"),
                                             fileInput(ns("file.preps"), label = "Upload a CSV File:", multiple = FALSE)),
                                      
                                      column(5,style=list("padding-left: 5px;"),
                                             radioButtons(ns("sep.preps"), "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t"),
                                                          selected = ","))
                                    ),             
                   ),
                   checkboxInput(inputId = ns("Optim.pREPS"), label = "Get Optim :)", value = TRUE),
                   fluidRow(
                     column(6,style=list("padding-right: 28px;"),
                            numericInput(ns("nrows.preps"), label = "Input # of Rows:",
                                         value = 15, min = 5)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            numericInput(ns("ncols.preps"), label = "Input # of Columns:",
                                         value = 20, min = 5)
                     )
                   ),

                   conditionalPanel("input.owndataPREPS == 'No'", ns = ns,
                                    fluidRow(
                                      column(6, style=list("padding-right: 28px;"),
                                             textInput(ns("repGens.preps"), label = "# of Entries per Rep Group:", 
                                                       value = "75,150")
                                      ),
                                      column(6,style=list("padding-left: 5px;"),
                                             textInput(inputId = ns("repUnits.preps"), label = "# of Rep per Group:",
                                                       value = "2,1")
                                      )
                                    )
                   ),
                   selectInput(ns("planter_mov.preps"), label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"), multiple = FALSE,
                               selected = "serpentine"),
                   fluidRow(
                     column(6,style=list("padding-right: 28px;"),
                            numericInput(ns("s.seed.preps"), label = "Seed number:", value = 1, min = 1)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            textInput(ns("expt_name.preps"), "Input Experiment Name:", value = "Expt1")
                     )
                   ),  
                   fluidRow(
                     column(6,style=list("padding-right: 28px;"),
                            textInput(ns("plot_start.preps"), "Starting Plot Number:", value = 1)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            textInput(ns("Location.preps"), "Input Location Name:", value = "FARGO")
                     )
                   ),
                   
                   fluidRow(
                     column(6,
                            downloadButton(ns("downloadData.preps"), "Save Experiment", style = "width:100%")
                     ),
                     column(6,
                            actionButton(ns("Simulate.prep"), "Simulate!", icon = icon("cocktail"), width = '100%')
                     )
                   )
      ),
      
      mainPanel(
        width = 8,
        tabsetPanel(
           tabPanel("Data", DT::DTOutput(ns("dataup.preps"))),
           tabPanel("Matrix Checks", DT::DTOutput(ns("BINARYpREPS"))),
           tabPanel("Randomized Field", DT::DTOutput(ns("dtpREPS"))),
           tabPanel("Plot Number Field", DT::DTOutput(ns("PREPSPLOTFIELD"))),
           tabPanel("Field Book", DT::DTOutput(ns("pREPSOUTPUT"))),
           tabPanel("Heatmap", shinycssloaders::withSpinner(plotly::plotlyOutput(ns("heatmap_prep")), type = 5))
        )
      )
    )
  )
}
    
#' pREPS Server Functions
#'
#' @noRd 
mod_pREPS_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    getDataup <- reactive({
      if (input$owndataPREPS == 'Yes') {
        req(input$file.preps)
        inFile <- input$file.preps
        data_up.preps <- load_file(name = inFile$name, path = inFile$datapat, sep = input$sep.preps)
        data_up.preps <- na.omit(data_up.preps)
        if (ncol(data_up.preps) < 3) validate("Data input needs at least three columns with: ENTRY, NAME and REPS.")
        data_up.preps <- as.data.frame(data_up.preps[,1:3])
        colnames(data_up.preps) <- c("ENTRY", "NAME", "REPS")
        if(!is.numeric(data_up.preps$REPS) || !is.integer(data_up.preps$REPS) ||
           is.factor(data_up.preps$REPS)) validate("'REPS' must be numeric.")
      }else {
        # req(input$lines.preps)
        # req(input$amountChecks.preps)
        # req(input$repGens.preps)
        # req(input$repUnits.preps)
        # lines <- as.numeric(input$lines.preps)
        # RepChecks <- as.numeric(as.vector(unlist(strsplit(input$amountChecks.preps, ","))))
        # checks <- length(RepChecks)
        # if (length(checks) == 1 && checks > 1) {
        #   checksEntries <- 1:checks
        #   checks <- checks
        # }else if (length(checks) > 1) {
        #   checksEntries <- checks
        #   checks <- length(checks)
        # } else if (length(checks) == 1 && checks == 1) {
        #   checksEntries <- checks
        #   checks <- length(checks)
        # }
        # repGens <- as.numeric(as.vector(unlist(strsplit(input$repGens.preps, ","))))
        # repUnits <- as.numeric(as.vector(unlist(strsplit(input$repUnits.preps, ","))))
        # if (length(repGens) != length(repUnits)) shiny::validate("repGens and repUnits may have the same length.")
        # lessEntries <- sum(repGens*repUnits) - sum(repGens)
        # lessEntries1 <- sum(repGens*repUnits)
        # reps.checks <- c(RepChecks, rep(repUnits, times = repGens))
        # REPS <- c(reps.checks, rep(1, lines - lessEntries1))
        # NAME <- c(paste(rep("Check", checks), 1:checks),
        #           paste(rep("gen", sum(repGens)), (checksEntries[checks] + 1):(sum(repGens) + checksEntries[checks])),
        #           paste(rep("gen", length(REPS) - (sum(repGens) + checks)),
        #                 (sum(repGens) + checksEntries[checks] + 1):(checksEntries[1] + (lines  + checks - lessEntries) - 1)))
        # data_up.preps <- data.frame(list(ENTRY = checksEntries[1]:(checksEntries[1] + (lines  + checks - lessEntries) - 1),
        #                                  NAME = NAME,	REPS = REPS))
        # colnames(data_up.preps) <- c("ENTRY", "NAME", "REPS")
        req(input$nrows.preps)
        req(input$ncols.preps)
        req(input$repGens.preps)
        req(input$repUnits.preps)
        nrows <- as.numeric(input$nrows.preps)
        ncols <- as.numeric(input$ncols.preps)
        repGens <- as.numeric(as.vector(unlist(strsplit(input$repGens.preps, ","))))
        repUnits <- as.numeric(as.vector(unlist(strsplit(input$repUnits.preps, ","))))
        if (length(repGens) != length(repUnits)) shiny::validate("Input repGens and repUnits must be of the same length.")
        if (sum(repGens * repUnits) != nrows*ncols) shiny::validate("Data input does not match withn field dimentions provided.")
        ENTRY <- 1:sum(repGens)
        NAME <- paste(rep("G", sum(repGens)), 1:sum(repGens), sep = "")
        REPS <- sort(rep(repUnits, times = repGens), decreasing = TRUE)
        data_up.preps <- data.frame(list(ENTRY = ENTRY, 
                                         NAME = NAME, 
                                         REPS = REPS))
        colnames(data_up.preps) <- c("ENTRY", "NAME", "REPS")
      }
      return(list(data_up.preps = data_up.preps))
    })
    
    ###### Plotting the data ##############
    output$dataup.preps <- DT::renderDT({
      req(getDataup()$data_up.preps)
      data_entry.preps <- getDataup()$data_up.preps
      df <- as.data.frame(data_entry.preps)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      DT::datatable(df, rownames = FALSE, options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    })
    
    pREPS_reactive <- reactive({
      
      req(input$nrows.preps, input$ncols.preps)
      req(getDataup()$data_up.preps)
      preps.seed <- as.numeric(input$s.seed.preps)
      gen.list <- getDataup()$data_up.preps
      nrows <- input$nrows.preps
      ncols <- input$ncols.preps
      n.checks  <- NULL
      r.checks <- NULL
      niter <- 10000
      
      OPTIM <- input$Optim.pREPS
      pREPS <- pREP(nrows = nrows, ncols = ncols, RepChecks = r.checks, checks = n.checks, seed = preps.seed,
                    optim = OPTIM, niter = niter, data = gen.list) 
      
    })
    
    
    output$BINARYpREPS <- DT::renderDT({
      B <- pREPS_reactive()$binary.field
      df <- as.data.frame(B)
      #DT::datatable(df)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE))
      DT::datatable(df) %>% 
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                        backgroundColor = DT::styleEqual(c(1,0), 
                                                         c("gray",'yellow')))
    })
    
    output$dtpREPS <- DT::renderDataTable({
      
      w_map <- pREPS_reactive()$field.map
      checks = as.vector(unlist(pREPS_reactive()$gen.entries[[1]]))
      len_checks <- length(checks)
      colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                   'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
      
      df <- as.data.frame(w_map)
      gens <- as.vector(unlist(pREPS_reactive()$gen.entries[[2]]))
      
      rownames(df) <- nrow(df):1
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE))
      
      DT::datatable(df) %>% 
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                    backgroundColor = DT::styleEqual(c(checks,gens), 
                                                 c(rep(colores[3], len_checks), rep('yellow', length(gens)))
                    )
        )
      
    })
    
    
    split_name_PREPS <- reactive({
      req(pREPS_reactive()$field.map)
      req(input$nrows.preps, input$ncols.preps)
      nrows <- as.numeric(input$nrows.preps)
      ncols <- as.numeric(input$ncols.preps)
      
      my_col_sets <- ncols
      
      blocks = 1
      
      if (input$expt_name.preps != "") {
        
        Name_expt <- input$expt_name.preps 
        
      }else Name_expt = paste0(rep("Expt1", times = blocks), 1:blocks)
      
      split_names <- split_name(n_rows = nrows, n_cols = ncols, Name_expt = Name_expt, by_row = FALSE,
                                col_sets = my_col_sets, row_sets = NULL)
      
      return(list(my_names = split_names))
      
    })
    
    output$pREPSNAMES <- DT::renderDT({
      req(split_name_PREPS()$my_names)
      my_names <- split_name_PREPS()$my_names
      blocks = 1
      if (input$expt_name.preps != ""){
        Name_expt <- input$expt_name.preps 
      }else Name_expt = paste0(rep("Expt1", times = blocks), 1:blocks)
      df <- as.data.frame(my_names)
      rownames(df) <- nrow(df):1
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = scrollY(input$nrows.preps)))
      DT::datatable(df) %>% 
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                    backgroundColor = DT::styleEqual(Name_expt, c('yellow')))
      
    })
    
    plot_number_PREPS <- reactive({
      
      req(input$plot_start.preps)
      req(input$nrows.preps, input$ncols.preps)
      req(split_name_PREPS()$my_names)
      
      datos_name <- split_name_PREPS()$my_names
      datos_name < as.matrix(datos_name)
      nrows <- input$nrows.preps; ncols <- input$ncols.preps
      plot_n_start <- as.numeric(input$plot_start.preps)
      movement_planter <- input$planter_mov.preps
      
      blocks = 1
      
      if (input$expt_name.preps != "") {
        
        Name_expt <- input$expt_name.preps 
        
      }else Name_expt = paste0(rep("Expt1", times = blocks), 1:blocks)
      
      
      
      my_split_plot_nub <- plot_number(movement_planter = movement_planter, n_blocks = blocks,
                                       n_rows = nrows, n_cols = ncols, plot_n_start = plot_n_start,
                                       datos = datos_name, expe_name = Name_expt, ByRow = FALSE,
                                       my_row_sets = NULL, ByCol = TRUE, my_col_sets = ncols)
    })
    
    output$PREPSPLOTFIELD <- DT::renderDT({
      req(plot_number_PREPS()$w_map_letters1)
      plot_num <- plot_number_PREPS()$w_map_letters1
      a <- as.vector(as.matrix(plot_num))
      len_a <- length(a)
      df <- as.data.frame(plot_num)
      rownames(df) <- nrow(df):1
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "1000px"))
      DT::datatable(df) %>%
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                    backgroundColor = DT::styleEqual(a, 
                                                 rep('yellow', length(a)))
        )
    })
    
    export_PREPS <- reactive({
      
      req(getDataup()$data_up.preps)
      req(input$Location.preps)
      req(input$planter_mov.preps)
      movement_planter <- input$planter_mov.preps
      
      req(pREPS_reactive()$field.map)
      req(pREPS_reactive()$binary.field)
      req(split_name_PREPS()$my_names)
      req(plot_number_PREPS()$w_map_letters1)
      
      loc <- input$Location.preps
      
      random_entries_map <- as.matrix(pREPS_reactive()$field.map)
      plot_number <- as.matrix(plot_number_PREPS()$w_map_letters1)
      Col_checks <- as.matrix(pREPS_reactive()$binary.field)
      my_names <- as.matrix(split_name_PREPS()$my_names)
      
      my_data_VLOOKUP <- getDataup()$data_up.preps
      
      results_to_export <- list(random_entries_map, plot_number, Col_checks, my_names)
      
      final_expt_export <- export_design(G = results_to_export, movement_planter =  movement_planter,
                                         location = loc, Year = 2020, data_file = my_data_VLOOKUP,
                                         reps = FALSE)
      
      final_expt_export <- as.data.frame(final_expt_export)
      final_expt_export <- final_expt_export[, -11]
      
      ID <- 1:nrow(final_expt_export)
      final_expt_export <- final_expt_export[, c(6,7,9,4,2,3,5,1,10)]
      final_expt_export_F <- cbind(ID, final_expt_export)
      colnames(final_expt_export_F)[10] <- "TREATMENT"
      
      list(final_expt = final_expt_export_F)
      
    })
    
    
    
    valsPREP <- reactiveValues(ROX = NULL, ROY = NULL, trail.prep = NULL, minValue = NULL,
                                maxValue = NULL)
    
    simuModal.PREP <- function(failed = FALSE) {
      modalDialog(
        fluidRow(
          column(6, 
                 selectInput(inputId = ns("trailsPREP"), label = "Select One Trail:", 
                             choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
          ),
          column(6, 
                 checkboxInput(inputId = ns("heatmap_PREP"), label = "Include a Heatmap", value = TRUE),
          )
        ),
        conditionalPanel("input.trailsPREP == 'Other'", ns = ns,
                         textInput(inputId = ns("OtherPREP"), label = "Input Trail Name:", value = NULL)
        ),
        fluidRow(
          column(6, 
                 selectInput(inputId = ns("ROX.PREP"), "Select the Correlation in Rows:", 
                             choices = seq(0.1, 0.9, 0.1))
          ),
          column(6, 
                 selectInput(inputId = ns("ROY.PREP"), "Select the Correlation in Cols:", 
                             choices = seq(0.1, 0.9, 0.1))
          )
        ),
        fluidRow(
          column(6, 
                 numericInput(inputId = ns("min.prep"), "Input the min value", value = NULL)
          ),
          column(6, 
                 numericInput(inputId = ns("max.prep"), "Input the max value", value = NULL)
                 
          )
        ),
        if (failed)
          div(tags$b("Invalid input of data max and min", style = "color: red;")),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(inputId = ns("ok.prep"), "GO")
        )
      )
    }
    
    observeEvent(input$Simulate.prep, {
      req(export_PREPS()$final_expt)
      showModal(
        shinyjqui::jqui_draggable(
          simuModal.PREP()
        )
      )
    })
    
    observeEvent(input$ok.prep, {
      req(input$min.prep, input$max.prep)
      if (input$max.prep > input$min.prep && input$min.prep != input$max.prep) {
        valsPREP$maxValue <- input$max.prep
        valsPREP$minValue  <- input$min.prep
        valsPREP$ROX <- as.numeric(input$ROX.PREP)
        valsPREP$ROY <- as.numeric(input$ROY.PREP)
        if(input$trailsPREP == "Other") {
          req(input$OtherPREP)
          if(!is.null(input$OtherPREP)) {
            valsPREP$trail.prep <- as.character(input$OtherPREP)
          }else showModal(simuModal.PREP(failed = TRUE))
        }else {
          valsPREP$trail.prep <- as.character(input$trailsPREP)
        }
        removeModal()
      }else {
        showModal(
          shinyjqui::jqui_draggable(
            simuModal.PREP(failed = TRUE)
          )
        )
      }
    })
    
    simuDataPREP <- reactive({
      req(export_PREPS()$final_expt)
      if(!is.null(valsPREP$maxValue) && !is.null(valsPREP$minValue) && !is.null(valsPREP$trail.prep)) {
        maxVal <- as.numeric(valsPREP$maxValue)
        minVal <- as.numeric(valsPREP$minValue)
        ROX_PREP <- as.numeric(valsPREP$ROX)
        ROY_PREP <- as.numeric(valsPREP$ROY)
        df.prep <- export_PREPS()$final_expt
        fieldBook <- df.prep[, c(1,6,7,9)]
        req(input$nrows.preps)
        req(input$ncols.preps)
        req(input$repGens.preps)
        repGens <- as.numeric(as.vector(unlist(strsplit(input$repGens.preps, ","))))
        lines_prep <- sum(repGens)
        nrows_prep <- as.numeric(input$nrows.preps)
        ncols_prep <- as.numeric(input$ncols.preps)
        seed_prep <- as.numeric(input$s.seed.preps)
        dfSimulation <- AR1xAR1_simulation(nrows = nrows_prep, ncols = ncols_prep, ROX = ROX_PREP, ROY = ROY_PREP, 
                                           minValue = minVal, maxValue = maxVal, fieldbook = fieldBook, 
                                           trail = valsPREP$trail.prep, seed = seed_prep)
        dfSimulation <- dfSimulation$outOrder
        dataPrep <- export_PREPS()$final_expt
        df.prep <- cbind(dataPrep, round(dfSimulation[,7],2))
        colnames(df.prep)[11] <- as.character(valsPREP$trail.prep)
        v <- 1
      }else {
        dataPrep <- export_PREPS()$final_expt
        v <- 2
      }
      if (v == 1) {
        return(list(df = df.prep, dfSimulation = dfSimulation))
      }else if (v == 2) {
        return(list(df = dataPrep))
      }
    })
    
    
    output$pREPSOUTPUT <- DT::renderDT({
      df <- simuDataPREP()$df
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "500px"))
      DT::datatable(df, rownames = FALSE, options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    })
    
    
    heatmap_obj <- reactive({
      req(simuDataPREP()$dfSimulation)
      if(input$heatmap_PREP){
        w <- as.character(valsPREP$trail.prep)
        df <- simuDataPREP()$dfSimulation
        p1 <- ggplot2::ggplot(df, ggplot2::aes(x = df[,4], y = df[,3], fill = df[,7], text = df[,8])) + 
          ggplot2::geom_tile() +
          ggplot2::xlab("COLUMN") +
          ggplot2::ylab("ROW") +
          ggplot2::labs(fill = w) +
          viridis::scale_fill_viridis(discrete = FALSE)
        
        p2 <- plotly::ggplotly(p1, tooltip="text", width = 1150, height = 710)
        
        return(p2)
      }
    })
    
    output$heatmap_prep <- plotly::renderPlotly({
      req(heatmap_obj())
      heatmap_obj()
    })
    
    output$downloadData.preps <- downloadHandler(
      filename = function() {
        req(input$Location.preps)
        loc <- input$Location.preps
        loc <- paste(loc, "_", "pREP_", sep = "")
        paste(loc, Sys.Date(), ".csv", sep = "")
        #paste("Export_my_Single_pREPS", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(simuDataPREP()$df, file, row.names = FALSE)
      }
    )
 
  })
}
    
## To be copied in the UI
# mod_pREPS_ui("pREPS_ui_1")
    
## To be copied in the server
# mod_pREPS_server("pREPS_ui_1")
