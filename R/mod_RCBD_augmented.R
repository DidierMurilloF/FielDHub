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
    sidebarLayout(
      sidebarPanel(width = 4,
                   radioButtons(inputId = ns("owndata_a_rcbd"), label = "Import entries' list?", 
                                choices = c("Yes", "No"), selected = "No",
                                inline = TRUE, width = NULL, choiceNames = NULL, choiceValues = NULL),
                   
                   conditionalPanel("input.owndata_a_rcbd == 'Yes'", ns = ns,
                                    fluidRow(
                                      column(7, style=list("padding-right: 28px;"),
                                             fileInput(ns("file1_a_rcbd"), label = "Upload a CSV File:", multiple = FALSE)),
                                      column(5,style=list("padding-left: 5px;"),
                                             radioButtons(ns("sep.a_rcbd"), "Separator",
                                                          choices = c(Comma = ",",
                                                                      Semicolon = ";",
                                                                      Tab = "\t"),
                                                          selected = ","))
                                    )              
                   ),
                   
                   fluidRow(
                     column(6,style=list("padding-right: 28px;"),
                            numericInput(inputId = ns("nExpt_a_rcbd"), label = "Input # of Stacked Expts:",
                                         value = 1, min = 1, max = 100)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                             checkboxInput(inputId = ns("random"), label = "Randomize Entries?",
                                           value = TRUE)
                     )
                   ),
                   
                   conditionalPanel("input.owndata_a_rcbd == 'No'", ns = ns,
                                    numericInput(inputId = ns("lines_a_rcbd"), label = "Input # of Entries:", value = 50)
                   ),
                   fluidRow(
                     column(6,style=list("padding-right: 28px;"),
                            numericInput(inputId = ns("checks_a_rcbd"), label = "Checks per Block:",
                                        value = 4, min = 1, max = 10)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            numericInput(inputId = ns("blocks_a_rcbd"), label = "Input # of Blocks:",
                                         value = 10, min = 3, max = 100)
                     )
                     
                   ),
                   fluidRow(
                     column(6,style=list("padding-right: 28px;"),
                            numericInput(inputId = ns("l.arcbd"), label = "Input # of Locations:",
                                         value = 1, min = 1, max = 100),
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            selectInput(inputId = ns("locView.arcbd"), label = "Choose location to view:",
                                        choices = 1:1, selected = 1, multiple = FALSE),
                     )
                     
                   ),
                   
                   
                   selectInput(inputId = ns("planter_mov1_a_rcbd"), label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"), multiple = FALSE,
                               selected = "serpentine"),
                   fluidRow(
                     column(6,style=list("padding-right: 28px;"),
                            numericInput(inputId = ns("myseed_a_rcbd"), label = "Seed Number:",
                                         value = 1, min = 1)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            textInput(ns("expt_name_a_rcbd"), "Input Experiment Name:", value = "Expt1")
                     )
                   ),  
                   
                   fluidRow(
                     column(6,style=list("padding-right: 28px;"),
                            textInput(ns("plot_start_a_rcbd"), "Starting Plot Number:", value = 1)
                     ),
                     column(6,style=list("padding-left: 5px;"),
                            textInput(ns("Location_a_rcbd"), "Input Location:", value = "FARGO")
                     )
                     
                   ),
                   #actionButton("Simulate.crd", "Simulate!")
                   #downloadButton(ns("downloadData_a_rcbd"), "Save My Experiment")
                   fluidRow(
                     column(6,
                            downloadButton(ns("downloadData_a_rcbd"), "Save Experiment", style = "width:100%")
                     ),
                     column(6,
                            actionButton(ns("Simulate.arcbd"), "Simulate!", icon = icon("cocktail"), width = '100%')
                     )
                   )
                   
      ),
      
      mainPanel(
        width = 8,
         tabsetPanel(
           tabPanel("Input Data",
                    fluidRow(
                      column(6,DT::DTOutput(ns("dt8_a_rcbd"))),
                      column(6,DT::DTOutput(ns("table1_a_rcbd")))
                    )
           ),
           tabPanel("Randomized Field", DT::DTOutput(ns("dt2_a_rcbd"))),
           tabPanel("Plot Number Field", DT::DTOutput(ns("dt4_a_rcbd"))),
           tabPanel("Name Expt", DT::DTOutput(ns("dt_names_a"))),
           tabPanel("Field Book", DT::DTOutput(ns("ARCBDOUTPUT"))),
           tabPanel("Heatmap", shinycssloaders::withSpinner(plotly::plotlyOutput(ns("heatmap")), type = 5))
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
    
    observeEvent(input$l.arcbd, {
      loc_user_view <- 1:as.numeric(input$l.arcbd)
      updateSelectInput(inputId = "locView.arcbd", choices = loc_user_view, selected = loc_user_view[1])
    })
    
    observeEvent(input$random, {
      # Show a modal when the button is pressed
      if (input$random == FALSE) {
        shinyalert::shinyalert("Warning!!", "By unchecking this option you will only randomized the check plots.", 
                               type = "warning")
      }
      
    })
    
    getDataup_a_rcbd <- reactive({
      if (input$owndata_a_rcbd == "Yes") {
        req(input$file1_a_rcbd)
        inFile <- input$file1_a_rcbd
        data_up <- load_file(name = inFile$name, path = inFile$datapat, sep = input$sep.a_rcbd)
        if (ncol(data_up) < 2) shiny::validate("Data input needs at least two columns with: ENTRY and NAME.")
        data_up <- as.data.frame(data_up[,1:2])
        data_up <- na.omit(data_up)
        colnames(data_up) <- c("ENTRY", "NAME")
      }else {
        req(input$checks_a_rcbd)
        req(input$lines_a_rcbd)
        lines <- as.numeric(input$lines_a_rcbd)
        checks <- as.numeric(input$checks_a_rcbd)
        if(lines < 1 || checks <= 0) validate("Number of lines and checks should be greater than 1.")
        NAME <- c(paste(rep("CH", checks), 1:checks, sep = ""),
                  paste(rep("G", lines), (checks + 1):(lines + checks), sep = ""))
        gen.list <- data.frame(list(ENTRY = 1:(lines + checks),	NAME = NAME))
        data_up <- gen.list
      }
      
      return(list(dataUp_a_rcbd = data_up))
      
    })
    
    output$dt8_a_rcbd <- DT::renderDT({
      my_data <- getDataup_a_rcbd()$dataUp_a_rcbd
      df <- my_data
      options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                scrollX = TRUE, scrollY = "600px"))
      DT::datatable(df,
                    filter = "top",
                    rownames = FALSE, 
                    caption = 'List of Entries.', 
                    options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    })
    
    entryListFormat_ARCBD <- data.frame(ENTRY = 1:9, 
                                        NAME = c(c("CHECK1", "CHECK2","CHECK3"), paste("Genotype", LETTERS[1:6], sep = "")))
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
          shinyjqui::jqui_draggable(
            entriesInfoModal_ARCBD()
          )
        )
      }
    })
    
    output$table1_a_rcbd <- DT::renderDT({
      req(input$checks_a_rcbd)
      req(getDataup_a_rcbd()$dataUp_a_rcbd)
        data_entry <- getDataup_a_rcbd()$dataUp_a_rcbd
        df <- data_entry[1:input$checks_a_rcbd,]
        options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                  scrollX = TRUE, scrollY = "350px"))
        a <- ncol(df) - 1
        DT::datatable(df, rownames = FALSE, caption = 'Table of checks.', options = list(
          columnDefs = list(list(className = 'dt-left', targets = 0:a))))
    })
    
    rcbd_augmented_reactive <- reactive({
      
      req(input$checks_a_rcbd)
      req(input$lines_a_rcbd)
      req(input$blocks_a_rcbd)
      req(input$planter_mov1_a_rcbd)
      req(input$plot_start_a_rcbd)
      req(input$myseed_a_rcbd)
      req(getDataup_a_rcbd()$dataUp_a_rcbd)
      req(input$Location_a_rcbd)
      
      checks <- as.numeric(input$checks_a_rcbd)
      if (input$owndata_a_rcbd == "Yes") {
        gen.list <- getDataup_a_rcbd()$dataUp_a_rcbd
        lines <- as.numeric(nrow(gen.list) - checks)
      }else {
        lines <- as.numeric(input$lines_a_rcbd)
        gen.list <- getDataup_a_rcbd()$dataUp_a_rcbd
      }
      b <- as.numeric(input$blocks_a_rcbd)
      seed.number <- as.numeric(input$myseed_a_rcbd)
      planter <- input$planter_mov1_a_rcbd
      plot.number <- as.numeric(unlist(strsplit(input$plot_start_a_rcbd, ",")))
      loc <- as.vector(unlist(strsplit(input$Location_a_rcbd, ",")))
      l.arcbd <- as.numeric(input$l.arcbd)
      if (length(loc) > l.arcbd) validate("Length of vector with name of locations is greater than the number of locations.")
      repsExpt <- as.numeric(input$nExpt_a_rcbd)
      nameexpt <- as.vector(unlist(strsplit(input$expt_name_a_rcbd, ",")))
      if (length(nameexpt) != 0) {
        Name_expt <- nameexpt
      }else Name_expt <- paste(rep('Expt', repsExpt), 1:repsExpt, sep = "")
      
      if (length(plot.number) != length(Name_expt)) plot.number <- 1001
      random <- input$random
      ARCBD <- RCBD_augmented(lines = lines,
                              checks = checks,
                              b = b,
                              l = l.arcbd,
                              planter = planter,
                              plotNumber = plot.number,
                              exptName = Name_expt,
                              seed = seed.number,
                              locationNames = loc,
                              repsExpt = repsExpt,
                              random = random, 
                              data = gen.list)
    })
    
    locNum <- reactive(
      return(as.numeric(input$locView.arcbd))
    )
    
    output$dt2_a_rcbd <- DT::renderDT({
     # print(locNum())
       req(getDataup_a_rcbd()$dataUp_a_rcbd)
       req(input$blocks_a_rcbd)
       r_map <- rcbd_augmented_reactive()$layout_random_sites[[locNum()]]
       checks <- 1:input$checks_a_rcbd
       b <- as.numeric(input$blocks_a_rcbd)
       len_checks <- length(checks)
       df <- as.data.frame(r_map)
       repsExpt <- as.numeric(input$nExpt_a_rcbd)
       colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                    'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
       s <- rcbd_augmented_reactive()$infoDesign$entries
       B <- paste("Block", rep(b:1, repsExpt), sep = "")
       E <- paste("E", rep(repsExpt:1, each = b), sep = "")
       rownames(df) <- paste(B,E)
       colnames(df) <- paste("V", 1:ncol(df), sep = "")
       options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "700px"))
       DT::datatable(df,
                     # extensions = 'FixedColumns',
                     # options = list(
                     #   dom = 't',
                     #   scrollX = TRUE,
                     #   fixedColumns = TRUE
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
     
     split_name_reactive_a <- reactive({
       req(rcbd_augmented_reactive()$infoDesign)
       req(input$blocks_a_rcbd)
       req(rcbd_augmented_reactive()$layoutRandom)
       df <-  rcbd_augmented_reactive()$exptNames
       
     })
     
     output$dt_names_a <- DT::renderDT({
       req(rcbd_augmented_reactive()$infoDesign)
       req(input$blocks_a_rcbd)
       req(rcbd_augmented_reactive()$layoutRandom)
       req(input$expt_name_a_rcbd)
       b <- as.numeric(input$blocks_a_rcbd)
       repsExpt <- as.numeric(input$nExpt_a_rcbd)
       nameexpt <- as.vector(unlist(strsplit(input$expt_name_a_rcbd, ",")))
       if (length(nameexpt) == repsExpt) {
         Name_expt <- nameexpt
       }else Name_expt <- paste(rep('Expt', repsExpt), 1:repsExpt, sep = "")
       df <-  as.data.frame(rcbd_augmented_reactive()$exptNames)
       B <- paste("Block", rep(b:1, repsExpt), sep = "")
       E <- paste("E", rep(repsExpt:1, each = b), sep = "")
       rownames(df) <- paste(B,E)
       colnames(df) <- paste("V", 1:ncol(df), sep = "")
       options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "700px"))
       DT::datatable(df,
                     extensions = 'FixedColumns',
                     options = list(
                       dom = 't',
                       scrollX = TRUE,
                       fixedColumns = TRUE
                     )) %>%
         DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                         backgroundColor = DT::styleEqual(Name_expt, rep('yellow', length(Name_expt))
                                                          )
                         )
     })

     output$dt4_a_rcbd <- DT::renderDT({
       req(rcbd_augmented_reactive()$infoDesign)
       req(input$blocks_a_rcbd)
       plot_num1 <- rcbd_augmented_reactive()$plotNumber
       b <- as.numeric(input$blocks_a_rcbd)
       infoDesign <- rcbd_augmented_reactive()$infoDesign
       Fillers <- as.numeric(infoDesign$Fillers)
       repsExpt <- as.numeric(input$nExpt_a_rcbd)
       
       if (Fillers == 0) {
         a <- as.vector(as.matrix(plot_num1))
         len_a <- length(a)
         df <- as.data.frame(plot_num1)
         B <- paste("Block", rep(b:1, repsExpt), sep = "")
         E <- paste("E", rep(repsExpt:1, each = b), sep = "")
         rownames(df) <- paste(B,E)
         colnames(df) <- paste("V", 1:ncol(df), sep = "")
         #options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "700px"))
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
                       # extensions = 'FixedColumns',
                       # options = list(
                       #   dom = 't',
                       #   scrollX = TRUE,
                       #   fixedColumns = TRUE
                       # )
                       ) 
           # DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
           #                 backgroundColor = DT::styleEqual(a, 
           #                                                  rep('yellow', length(a)))
           # )
       }else {
         a <- as.vector(as.matrix(plot_num1))
         a <- a[-which(a == 0)]
         len_a <- length(a)
         df <- as.data.frame(plot_num1)
         B <- paste("Block", rep(b:1, repsExpt), sep = "")
         E <- paste("E", rep(repsExpt:1, each = b), sep = "")
         rownames(df) <- paste(B,E)
         colnames(df) <- paste("V", 1:ncol(df), sep = "")
         #options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE, scrollY = "700px"))
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
                       # extensions = 'FixedColumns',
                       # options = list(
                       #   dom = 't',
                       #   scrollX = TRUE,
                       #   fixedColumns = TRUE
                      # )
                       )
           # DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
           #                 backgroundColor = DT::styleEqual(c(a,0),c(rep(c('yellow'), len_a),"red"))
           #     )
       }
     })
     
     
     
     
     
     
     
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
       showModal(
         shinyjqui::jqui_draggable(
           simuModal.ARCBD()
         )
       )
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
           shinyjqui::jqui_draggable(
             simuModal.ARCBD(failed = TRUE)
           )
         )
       }
     })

     simuDataARCBD <- reactive({
       # req(Spatial_Checks()$fieldBook)
       if(!is.null(valsARCBD$maxValue) && !is.null(valsARCBD$minValue) && !is.null(valsARCBD$trail.arcbd)) {
         maxVal <- as.numeric(valsARCBD$maxValue)
         minVal <- as.numeric(valsARCBD$minValue)
         ROX_O <- as.numeric(valsARCBD$ROX)
         ROY_O <- as.numeric(valsARCBD$ROY)
         locs <- as.numeric(input$l.arcbd)
         df_arcbd <- rcbd_augmented_reactive()$fieldBook
         nrows.s <- length(levels(as.factor(df_arcbd$ROW)))
         ncols.s <- length(levels(as.factor(df_arcbd$COLUMN)))
         loc_levels_factors <- levels(factor(df_arcbd$LOCATION, unique(df_arcbd$LOCATION)))
         print(loc_levels_factors)
         # nrows.s <- as.numeric(input$nrows.s)
         # ncols.s <- as.numeric(input$ncols.s)
         seed.s <- as.numeric(input$myseed_a_rcbd)

         df_arcbd_list <- vector(mode = "list", length = locs)
         dfSimulationList <- vector(mode = "list", length = locs)
         do_sites <- 1:locs
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


     output$ARCBDOUTPUT <- DT::renderDT({
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
                                 scrollX = TRUE, scrollY = "600px",
                                 columnDefs = list(list(className = 'dt-center', targets = "_all"))))
       DT::datatable(df,
                     filter = "top",
                     rownames = FALSE
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

         p2 <- plotly::ggplotly(p1, tooltip="text", width = 1180, height = 740)

         return(p2)
       }
     })

     output$heatmap <- plotly::renderPlotly({
       req(heatmap_obj())
       heatmap_obj()
     })
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     # 
     # output$dt5_a <- DT::renderDT({
     #   if(is.null(rcbd_augmented_reactive()$fieldBook)) return(NULL)
     #   df <- simuDataARCBD()$df
     #   options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
     #                             scrollX = TRUE, scrollY = "600px"))
     #   DT::datatable(df, rownames = FALSE, options = list(
     #     columnDefs = list(list(className = 'dt-center', targets = "_all"))))
     # })
     
     output$downloadData_a_rcbd <- downloadHandler(
       filename = function() {
         req(input$Location_a_rcbd)
         loc <- input$Location_a_rcbd
         loc <- paste(loc, "_", "ARCBD_", sep = "")
         paste(loc, Sys.Date(), ".csv", sep = "")
       },
       content = function(file) {
         df <- as.data.frame(rcbd_augmented_reactive()$fieldBook)
         write.csv(df, file, row.names = FALSE)
       }
     )
 
  })
}