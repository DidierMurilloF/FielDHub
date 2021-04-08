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
      sidebarPanel(width = 4,
                   radioButtons(inputId = ns("owndataOPTIM"), label = "Import Entries' List?", choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                   conditionalPanel("input.owndataOPTIM == 'Yes'", ns = ns,
                                    fluidRow(
                                      column(7, style=list("padding-right: 28px;"),
                                             fileInput(ns("file3"), label = "Upload a CSV File:", multiple = FALSE)),
                                      column(5,style=list("padding-left: 5px;"),
                                             radioButtons(ns("sep.OPTIM"), "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t"),
                                                          selected = ","))
                                    )            
                   ),
                   checkboxInput(inputId = ns("Optim.spatial"), label = "Get Optim :)", value = TRUE),
                   
                   numericInput(ns("checks.s"), label = "Input # of Checks:", value = 3, min = 1),
                   
                   numericInput(ns("tplots.s"), label = "Input # of Total Check Plots:",
                                value = 30, min = 1),
                   # fluidRow(
                   #   column(6,style=list("padding-right: 28px;"),
                   #          numericInput(ns("checks.s"), label = "Input # of Checks:", value = 3, min = 1),
                   #   ),
                   #   column(6,style=list("padding-left: 5px;"),
                   #           numericInput(ns("tplots.s"), label = "Input # of Total Check Plots:",
                   #                        value = 30, min = 1)
                   #   )
                   # ),
                   
                   conditionalPanel("input.owndataOPTIM != 'Yes'", ns = ns,
                                    
                                    fluidRow(
                                      column(6,style=list("padding-right: 28px;"),
                                             textInput(ns("amount.checks"), "Input # Check's Reps:", value = "10,10,10"),
                                      ),
                                      column(6,style=list("padding-left: 5px;"),
                                             numericInput(ns("lines.s"), label = "Input # of Entries:", value = 270, min = 5)
                                      )
                                    )            
                   ),
                   
                   fluidRow(
                     column(6,style=list("padding-right: 28px;"),
                            numericInput(ns("nrows.s"), label = "Input # of Rows:",
                                         value = 15, min = 5)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            numericInput(ns("ncols.s"), label = "Input # of Columns:",
                                         value = 20, min = 5)
                     )
                   ),
                   
                   
                   selectInput(ns("planter_mov.spatial"), label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"), multiple = FALSE, selected = "serpentine"),
                   fluidRow(
                     column(6,style=list("padding-right: 28px;"),
                            numericInput(ns("seed.spatial"), label = "Seed Number:", value = 1, min = 1)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            textInput(ns("expt_name.spatial"), "Input Experiment Name:", value = "Expt1")
                     )
                   ),  
                   
                   fluidRow(
                     column(6,style=list("padding-right: 28px;"),
                            textInput(ns("plot_start.spatial"), "Starting Plot Number:", value = 1)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            textInput(ns("Location.spatial"), "Input Location:", value = "FARGO")
                     )
                   ),
                   
                   fluidRow(
                     column(6,
                            downloadButton(ns("downloadData.spatial"), "Save Experiment", style = "width:100%")
                     ),
                     column(6,
                            actionButton(ns("Simulate.optim"), "Simulate!", icon = icon("cocktail"), width = '100%')
                     )
                   )
      ),
      
      mainPanel(
        width = 8,
        tabsetPanel(
          tabPanel("Input Data",
                   fluidRow(
                     column(6,DT::DTOutput(ns("data_up.s"))),
                     column(6,DT::DTOutput(ns("table_checks")))
                   )
          ),
          tabPanel("Matrix Checks", DT::DTOutput(ns("BINARY"))),
          tabPanel("Randomized Field", DT::DTOutput(ns("RFIELD"))),
          tabPanel("Plot Number Field", DT::DTOutput(ns("PLOTFIELD"))),
          tabPanel("Field Book", DT::DTOutput(ns("OPTIMOUTPUT"))),
          tabPanel("Heatmap", shinycssloaders::withSpinner(plotly::plotlyOutput(ns("heatmap")), type = 5))
          # conditionalPanel(condition = "input.heatmap_s==true", ns = ns,
          # 
          # )
         )
      )
    )
  )
}
    
#' Optim Server Functions
#'
#' @noRd 
mod_Optim_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    getDataup.spatiaL <- reactive({
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
        #lines <- nrows * ncols - total.checks
        lines <- as.numeric(input$lines.s)
        if (lines <= sum(total.checks)) validate("Number of lines should be greater then the number of checks.")
        NAME <- c(paste(rep("CH", n.checks), 1:n.checks),
                  paste(rep("G", lines), (n.checks + 1):(lines + n.checks), sep = ""))
        reps.checks <- r.checks
        REPS <- c(reps.checks, rep(1, lines))
        gen.list <- data.frame(list(ENTRY = 1:(lines + n.checks),	NAME = NAME,	REPS = REPS))
        data_up <- gen.list
      }
     
      return(list(data_up.spatial = data_up))
      
    })
    
    
    output$data_up.s <- DT::renderDT({
      req(getDataup.spatiaL()$data_up.spatial)
      data_entry <- getDataup.spatiaL()$data_up.spatial
      df <- as.data.frame(data_entry)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "600px"))
      DT::datatable(df, rownames = TRUE, options = list(
        columnDefs = list(list(className = 'dt-left', targets = 0:3))))
    })
    
    output$table_checks <- DT::renderDT({
      req(getDataup.spatiaL()$data_up.spatial)
        data_entry <- getDataup.spatiaL()$data_up.spatial
        checks_input <- as.numeric(input$checks.s)
        times_checks <- data_entry[1:checks_input,]
        df <- as.data.frame(times_checks)
        options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                  scrollX = TRUE, scrollY = "350px"))
        a <- ncol(df) - 1
        DT::datatable(df, rownames = FALSE, caption = 'Table of checks.', options = list(
          columnDefs = list(list(className = 'dt-left', targets = 0:a))))
    })
    
    Spatial_Checks <- reactive({
      
      req(getDataup.spatiaL()$data_up.spatial)
      req(input$nrows.s, input$ncols.s)
      req(input$seed.spatial)
      seed.spatial <- as.numeric(input$seed.spatial)
      nrows <- input$nrows.s
      ncols <- input$ncols.s
      niter <- 1000
      
      data.spatial <- getDataup.spatiaL()$data_up.spatial
      
      OPTIM <- input$Optim.spatial
      
      mydesign <- pREP(nrows = nrows, ncols = ncols, RepChecks = r.checks, checks = n.checks,
                       seed = seed.spatial, optim = OPTIM, niter = niter, data = data.spatial)

    })

    output$BINARY <- DT::renderDT({
      B <- Spatial_Checks()$binary.field
      df <- as.data.frame(B)
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "650px"))
      #DT::datatable(df)
      DT::datatable(df) %>% 
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                        backgroundColor = DT::styleEqual(c(1,0), 
                                                         c("gray",'yellow')))
    })
    
    output$RFIELD <- DT::renderDT({
      w_map <- Spatial_Checks()$field.map
      checks = as.vector(Spatial_Checks()$gen.entries[[1]])
      len_checks <- length(checks)
      colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                   'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
      gens <- as.vector(Spatial_Checks()$gen.entries[[2]])
      df <- as.data.frame(w_map)
      rownames(df) <- nrow(df):1
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "650px"))
      
      DT::datatable(df) %>%
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                    backgroundColor = DT::styleEqual(c(checks,gens),
                                                 c(colores[1:len_checks], rep('gray', length(gens)))
                    )
        )
    })
    
    
    split_name_spatial <- reactive({
      req(Spatial_Checks()$field.map)
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
    
    output$NAMESPATIAL <- DT::renderDT({
      req(split_name_spatial()$my_names)
      my_names <- split_name_spatial()$my_names
      blocks = 1
      if (input$expt_name.spatial != ""){
        Name_expt <- input$expt_name.spatial 
      }else Name_expt = paste0(rep("Expt1", times = blocks), 1:blocks)
      df <- as.data.frame(my_names)
      rownames(df) <- nrow(df):1
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = scrollY(input$nrows.s)))
      DT::datatable(df) %>% 
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                    backgroundColor = DT::styleEqual(Name_expt, c('yellow')))
      
    })
    
    plot_number_spatial <- reactive({
      req(input$plot_start.spatial)
      req(input$nrows.s, input$ncols.s)
      req(split_name_spatial()$my_names)
      datos_name <- split_name_spatial()$my_names
      datos_name < as.matrix(datos_name)
      nrows <- input$nrows.s; ncols <- input$ncols.s
      plot_n_start <- as.numeric(input$plot_start.spatial)
      movement_planter <- input$planter_mov.spatial
      blocks = 1
      if (input$expt_name.spatial != "") {
        Name_expt <- input$expt_name.spatial 
      }else Name_expt = paste0(rep("Expt1", times = blocks), 1:blocks)
      
      my_split_plot_nub <- plot_number(movement_planter = movement_planter, n_blocks = blocks,
                                         n_rows = nrows, n_cols = ncols, plot_n_start = plot_n_start,
                                         datos = datos_name, expe_name = Name_expt, ByRow = FALSE,
                                         my_row_sets = NULL, ByCol = TRUE, my_col_sets = ncols)
  
    })
    
    
    output$PLOTFIELD <- DT::renderDT({
      req(plot_number_spatial()$w_map_letters1)
      plot_num <- plot_number_spatial()$w_map_letters1
      a <- as.vector(as.matrix(plot_num))
      len_a <- length(a)
      df <- as.data.frame(plot_num)
      rownames(df) <- nrow(df):1
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "650px"))
      DT::datatable(df) %>%
        DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                    backgroundColor = DT::styleEqual(a, 
                                                 rep('yellow', length(a)))
        )
    })
    
    ####### Export the experiment ############################
    
    export_spatial <- reactive({
      req(getDataup.spatiaL()$data_up.spatial)
      req(input$Location.spatial)
      req(input$planter_mov.spatial)
      movement_planter <- input$planter_mov.spatial
      req(Spatial_Checks()$field.map)
      req(Spatial_Checks()$binary.field)
      req(split_name_spatial()$my_names)
      req(plot_number_spatial()$w_map_letters1)
      loc <- input$Location.spatial
      random_entries_map <- as.matrix(Spatial_Checks()$field.map)
      plot_number <- as.matrix(plot_number_spatial()$w_map_letters1)
      Col_checks <- as.matrix(Spatial_Checks()$binary.field)
      my_names <- as.matrix(split_name_spatial()$my_names)
    
      my_data_VLOOKUP <- getDataup.spatiaL()$data_up.spatial
      results_to_export <- list(random_entries_map, plot_number, Col_checks, my_names)
      final_expt_export <- export_design(G = results_to_export, movement_planter =  movement_planter,
                                             location = loc, Year = 2020,
                                             data_file = my_data_VLOOKUP, reps = FALSE)
      final_expt_export <- as.data.frame(final_expt_export)
      final_expt_export <- final_expt_export[, -11]
      ID <- 1:nrow(final_expt_export)
      final_expt_export <- final_expt_export[, c(6,7,9,4,2,3,5,1,10)]
      final_expt_export_F <- cbind(ID, final_expt_export)
      colnames(final_expt_export_F)[10] <- "TREATMENT"
      
      list(final_expt = final_expt_export_F)
      
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
      req(export_spatial()$final_expt)
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
      req(export_spatial()$final_expt)
      if(!is.null(valsOPTIM$maxValue) && !is.null(valsOPTIM$minValue) && !is.null(valsOPTIM$trail.optim)) {
        maxVal <- as.numeric(valsOPTIM$maxValue)
        minVal <- as.numeric(valsOPTIM$minValue)
        ROX_O <- as.numeric(valsOPTIM$ROX)
        ROY_O <- as.numeric(valsOPTIM$ROY)
        df.optim <- export_spatial()$final_expt
        fieldBook <- df.optim[, c(1,6,7,9)]
        nrows.s <- as.numeric(input$nrows.s)
        ncols.s <- as.numeric(input$ncols.s)
        seed.s <- as.numeric(input$seed.spatial)
        dfSimulation <- AR1xAR1_simulation(nrows = nrows.s, ncols = ncols.s, ROX = ROX_O, ROY = ROY_O, minValue = minVal, 
                                           maxValue = maxVal, fieldbook = fieldBook, trail = valsOPTIM$trail.optim, 
                                           seed = seed.s)
        dfSimulation <- dfSimulation$outOrder
        dataOptim <- export_spatial()$final_expt
        df.optim <- cbind(dataOptim, round(dfSimulation[,7],2))
        colnames(df.optim)[11] <- as.character(valsOPTIM$trail.optim)
        v <- 1
      }else {
        dataOptim <- export_spatial()$final_expt
        v <- 2
      }
      if (v == 1) {
        return(list(df = df.optim, dfSimulation = dfSimulation))
      }else if (v == 2) {
        return(list(df = dataOptim))
      }
    })
    
    
    output$OPTIMOUTPUT <- DT::renderDT({
      df <- simuDataOPTIM()$df
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "600px"))
      DT::datatable(df, rownames = FALSE, options = list(
        columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    })
    
    
    heatmap_obj <- reactive({
      req(simuDataOPTIM()$dfSimulation)
      
      if(input$heatmap_s){
        df <- simuDataOPTIM()$dfSimulation
        w <- as.character(valsOPTIM$trail.optim)
        p1 <- ggplot2::ggplot(df, ggplot2::aes(x = df[,4], y = df[,3], fill = df[,7], text = df[,8])) + 
          ggplot2::geom_tile() +
          ggplot2::xlab("COLUMN") +
          ggplot2::ylab("ROW") +
          ggplot2::labs(fill = w) +
          viridis::scale_fill_viridis(discrete = FALSE)
        
        p2 <- plotly::ggplotly(p1, tooltip="text", width = 1180, height = 740)
        
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
    
## To be copied in the UI
# mod_Optim_ui("Optim_ui_1")
    
## To be copied in the server
# mod_Optim_server("Optim_ui_1")
