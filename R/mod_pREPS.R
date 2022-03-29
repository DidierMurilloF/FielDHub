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
                   fluidRow(
                     column(6,style=list("padding-right: 28px;"),
                            numericInput(inputId = ns("l.preps"), label = "Input # of Locations:", value = 1, min = 1)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            selectInput(inputId = ns("locView.preps"), label = "Choose location to view:", choices = 1:1, selected = 1, multiple = FALSE)
                     )
                    ),
                     # column(6,style=list("padding-left: 5px;"),
                     #        selectInput(inputId = ns("locView.preps"),label = "Choose location to view:", choices = 1:1, selected = 1, multiple = FALSE)
                     # ),
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
                   
                   # fluidRow(
                   #   column(6,
                   #          actionButton(inputId = ns("RUN.prep"), "Run!", icon = icon("cocktail"), width = '100%'),
                   #   ),
                   #   column(6,
                   #          actionButton(ns("Simulate.prep"), "Simulate!", icon = icon("cocktail"), width = '100%')
                   #   )
                   #   
                   # ), 
                   # br(),
                   # downloadButton(ns("downloadData.preps"), "Save Experiment", style = "width:100%")
                   # 
                   
                   fluidRow(
                     column(6,
                            downloadButton(ns("downloadData.preps"), "Save Experiment", style = "width:100%")
                     ),
                     column(6,
                            actionButton(ns("Simulate.prep"), "Simulate!", icon = icon("cocktail"), width = '100%')
                     )
                   )
      #),
      ),
      mainPanel(
        width = 8,
        tabsetPanel(
           tabPanel("Data", DT::DTOutput(ns("dataup.preps"))),
           # tabPanel("Matrix Checks", DT::DTOutput(ns("BINARYpREPS"))),
           tabPanel("Matrix Checks", shinycssloaders::withSpinner(DT::DTOutput(ns("BINARYpREPS")), type = 5)),
           tabPanel("Randomized Field", DT::DTOutput(ns("dtpREPS"))),
           tabPanel("Plot Number Field", DT::DTOutput(ns("PREPSPLOTFIELD"))),
           tabPanel("Field Book", DT::DTOutput(ns("pREPSOUTPUT"))),
           #tabPanel("Heatmap", shinycssloaders::withSpinner(plotly::plotlyOutput(ns("heatmap_prep")), type = 5))
           tabPanel("Heatmap", plotly::plotlyOutput(ns("heatmap_prep")))
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
    
    observeEvent(input$l.preps, {
      loc_user_view <- 1:as.numeric(input$l.preps)
      updateSelectInput(inputId = "locView.preps", choices = loc_user_view, selected = loc_user_view[1])
    })
    
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
    
    entryListFormat_pREP <- data.frame(ENTRY = 1:9, 
                                       NAME = c(paste("Genotype", LETTERS[1:9], sep = "")),
                                       REPS = as.factor(c(rep(2, times = 3), rep(1,6))))
    
    entriesInfoModal_pREP <- function() {
      modalDialog(
        title = div(tags$h3("Important message", style = "color: red;")),
        h4("Please, follow the format shown in the following example. Make sure to upload a CSV file!"),
        renderTable(entryListFormat_pREP,
                    bordered = TRUE,
                    align = 'c',
                    striped = TRUE),
        #h4("Note that the controls must be in the first rows of the CSV file."),
        easyClose = FALSE
      )
    }
    
    toListen <- reactive({
      list(input$owndataPREPS)
    })
    
    observeEvent(toListen(), {
      if (input$owndataPREPS == 'Yes'){
        showModal(
          shinyjqui::jqui_draggable(
            entriesInfoModal_pREP()
          )
        )
      }
    })
    # pREPS_reactive <- eventReactive(input$RUN.prep, {
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
      locs <- as.numeric(input$l.preps)
      pREPS <- vector(mode = "list", length = locs)
      
      OPTIM <- input$Optim.pREPS
      set.seed(preps.seed)
      for (s in 1:locs) {
      pREPS[[s]] <- pREP(nrows = nrows, ncols = ncols, RepChecks = r.checks, checks = n.checks, seed = NULL,
                         optim = OPTIM, niter = niter, data = gen.list) 
      }
      return(pREPS)
    })
    
    user_location <- reactive({
      user_site <- as.numeric(input$locView.preps)
      #user_site <- 1
      loc_user_out <- pREPS_reactive()[[user_site]]
      w_map <- loc_user_out$field.map
      binary_field <- loc_user_out$binary.field
      gen.entries <- loc_user_out$gen.entries
      return(list(field.map = w_map, binary.field = binary_field, gen.entries = gen.entries, user_site = user_site))
    })
    
    
    output$BINARYpREPS <- DT::renderDT({
      req(user_location()$binary.field)
      B <- user_location()$binary.field
      df <- as.data.frame(B)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "700px"))
      DT::datatable(df,
                    extensions = 'FixedColumns',
                    options = list(
                      dom = 't',
                      scrollX = TRUE,
                      fixedColumns = TRUE
                    )) %>%
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                        backgroundColor = DT::styleEqual(c(1,0), 
                                                         c("gray",'yellow')))
    })
    
  
    
    output$dtpREPS <- DT::renderDataTable({
      w_map <- user_location()$field.map
      checks = as.vector(unlist(user_location()$gen.entries[[1]]))
      len_checks <- length(checks)
      colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                   'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
      
      df <- as.data.frame(w_map)
      gens <- as.vector(unlist(user_location()$gen.entries[[2]]))
      
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
                    backgroundColor = DT::styleEqual(c(checks,gens), 
                                                 c(rep(colores[3], len_checks), rep('yellow', length(gens)))
                    )
        )
      
    })
    
    
    split_name_PREPS <- reactive({
      first_loc <- pREPS_reactive()[[1]]
      w_map <- first_loc$field.map
      req(first_loc$field.map)
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
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "600px"))
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
    })
    
    export_PREPS <- reactive({
      
      req(getDataup()$data_up.preps)
      req(input$Location.preps)
      req(input$planter_mov.preps)
      movement_planter <- input$planter_mov.preps
      req(split_name_PREPS()$my_names)
      req(plot_number_PREPS()$w_map_letters1)
      
      loc <- input$Location.preps
      my_data_VLOOKUP <- getDataup()$data_up.preps
      ######## for #####
      locs <- as.numeric(input$l.preps)
      final_expt_fieldbook <- vector(mode = "list",length = locs)
      location_names <- as.vector(unlist(strsplit(input$Location.preps, ",")))
      if (length(location_names) != locs) location_names <- 1:locs
      
      for (user_site in 1:locs) {
        loc_user_out <- pREPS_reactive()[[user_site]]
        
        random_entries_map <- as.matrix(loc_user_out$field.map)
        #random_entries_map <- as.matrix(user_location()$field.map)
        plot_number <- as.matrix(plot_number_PREPS()$w_map_letters1)
        Col_checks <- as.matrix(loc_user_out$binary.field)
        #Col_checks <- as.matrix(user_location()$binary.field)
        my_names <- as.matrix(split_name_PREPS()$my_names)
        
        results_to_export <- list(random_entries_map, plot_number, Col_checks, my_names)
        
        final_expt_export <- export_design(G = results_to_export, movement_planter =  movement_planter,
                                           location = location_names[user_site], Year = NULL, data_file = my_data_VLOOKUP,
                                           reps = FALSE)
        
        final_expt_export <- as.data.frame(final_expt_export)
        final_expt_fieldbook[[user_site]] <- final_expt_export[, -11]
        print(user_site)
      }
      
      final_fieldbook <- dplyr::bind_rows(final_expt_fieldbook)
      
      ######### end for #################################
      
      ID <- 1:nrow(final_fieldbook )
      final_expt_export <- final_fieldbook[, c(6,7,9,4,2,3,5,1,10)]
      final_expt_export_F <- cbind(ID, final_expt_export)
      colnames(final_expt_export_F)[10] <- "TREATMENT"
      
      return(list(final_expt = final_expt_export_F))
      
    })
    
    
    
    valsPREP <- reactiveValues(ROX = NULL, ROY = NULL, trail.prep = NULL, minValue = NULL,
                                maxValue = NULL)
    
    simuModal.PREP <- function(failed = FALSE) {
      modalDialog(
        fluidRow(
          column(6, 
                 selectInput(inputId = ns("trailsPREP"), label = "Select One:", 
                             choices = c("YIELD", "MOISTURE", "HEIGHT", "Other")),
          ),
          column(6, 
                 checkboxInput(inputId = ns("heatmap_PREP"), label = "Include a Heatmap", value = TRUE),
          )
        ),
        conditionalPanel("input.trailsPREP == 'Other'", ns = ns,
                         textInput(inputId = ns("OtherPREP"), label = "Input Trial Name:", value = NULL)
        ),
        fluidRow(
          column(6, 
                 selectInput(inputId = ns("ROX.PREP"), "Select the Correlation in Rows:", 
                             choices = seq(0.1, 0.9, 0.1),  selected = 0.5)
          ),
          column(6, 
                 selectInput(inputId = ns("ROY.PREP"), "Select the Correlation in Cols:", 
                             choices = seq(0.1, 0.9, 0.1),  selected = 0.5)
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
        loc_levels_factors <- levels(factor(df.prep$LOCATION, unique(df.prep$LOCATION)))
        print(loc_levels_factors)
        locs <- as.numeric(input$l.preps)
        req(input$nrows.preps)
        req(input$ncols.preps)
        req(input$repGens.preps)
        repGens <- as.numeric(as.vector(unlist(strsplit(input$repGens.preps, ","))))
        lines_prep <- sum(repGens)
        nrows_prep <- as.numeric(input$nrows.preps)
        ncols_prep <- as.numeric(input$ncols.preps)
        seed_prep <- as.numeric(input$s.seed.preps)
        
        df.prep_list <- vector(mode = "list", length = locs)
        dfSimulationList <- vector(mode = "list", length = locs)
        w <- 1
        set.seed(seed_prep)
        for (sites in 1:locs) {
          df_loc <- subset(df.prep, LOCATION == loc_levels_factors[w])
          fieldBook <- df_loc[, c(1,6,7,9)]
          dfSimulation <- AR1xAR1_simulation(nrows = nrows_prep, ncols = ncols_prep, ROX = ROX_PREP, ROY = ROY_PREP, 
                                             minValue = minVal, maxValue = maxVal, fieldbook = fieldBook, 
                                             trail = valsPREP$trail.prep, seed = NULL)
          
          dfSimulation <- dfSimulation$outOrder
          dfSimulationList[[sites]] <- dfSimulation
          dataPrep <- df_loc
          df_prep <- cbind(dataPrep, round(dfSimulation[,7],2))
          colnames(df_prep)[11] <- as.character(valsPREP$trail.prep)
          df.prep_list[[sites]] <- df_prep
          w <- w + 1
        }
        df.prep_locs <- dplyr::bind_rows(df.prep_list)
        v <- 1
      }else {
        dataPrep <- export_PREPS()$final_expt
        v <- 2
      }
      if (v == 1) {
        return(list(df = df.prep_locs, dfSimulationList = dfSimulationList))
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
      req(simuDataPREP()$dfSimulationList)
      loc_user <- user_location()$user_site
      if(input$heatmap_PREP){
        w <- as.character(valsPREP$trail.prep)
        df <- simuDataPREP()$dfSimulationList[[loc_user]]
        df <- as.data.frame(df)
        print(head(df))
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
      },
      content = function(file) {
        write.csv(simuDataPREP()$df, file, row.names = FALSE)
      }
    )
 
  })
}
