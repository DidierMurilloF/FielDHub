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
                   
                   # fluidRow(
                   #   column(6,style=list("padding-right: 28px;"),
                   #          numericInput(inputId = ns("myseed_a_rcbd"), label = "Seed Number:",
                   #                       value = 1, min = 1)
                   #   ),
                   #   column(6,style=list("padding-left: 5px;"),
                   #          # checkboxInput(inputId = ns("Option_NCD"), label = "Are you Including Filler Plots?",
                   #          #               value = FALSE)
                   #   )
                   # ),
                   
                   numericInput(inputId = ns("nExpt_a_rcbd"), label = "Input # of Stacked Expts:",
                                value = 1, min = 1, max = 100),
                   
                   # fluidRow(
                   #   column(6,style=list("padding-right: 28px;"),
                   #          textInput(ns("expt_name_a_rcbd"), "Input Experiment Name:", value = "Expt1"),
                   #   ),
                   #   column(6,style=list("padding-left: 5px;"),
                   #          numericInput(inputId = ns("nExpt_a_rcbd"), label = "Input # of Expts:",
                   #                       value = 1, min = 1, max = 100)
                   #   )
                   #   
                   # ),
                   
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
                   numericInput(inputId = ns("l.arcbd"), label = "Input # of Locations:",
                                value = 1, min = 1, max = 100),
                   
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
                   downloadButton(ns("downloadData_a_rcbd"), "Save My Experiment")
                   
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
           tabPanel("Field Book", DT::DTOutput(ns("dt5_a")))
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
    
    getDataup_a_rcbd <- reactive({
      if (input$owndata_a_rcbd == "Yes") {
        req(input$file1_a_rcbd)
        inFile <- input$file1_a_rcbd
        data_up <- load_file(name = inFile$name, path = inFile$datapat, sep = input$sep.a_rcbd)
        data_up <- as.data.frame(na.omit(data_up[,1:3]))
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
      DT::datatable(df, rownames = FALSE)
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
      
      gen.list <- getDataup_a_rcbd()$dataUp_a_rcbd
      lines <- as.numeric(input$lines_a_rcbd)
      checks <- as.numeric(input$checks_a_rcbd)
      b <- as.numeric(input$blocks_a_rcbd)
      seed.number <- as.numeric(input$myseed_a_rcbd)
      planter <- input$planter_mov1_a_rcbd
      #plot.number <- as.numeric(input$plot_start_a_rcbd)
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
      
      ARCBD <- RCBD_augmented(lines = lines, checks = checks, b = b, l = l.arcbd, planter = planter,
                              plotNumber = plot.number, exptName = Name_expt, seed = seed.number,
                              locationNames = loc, repsExpt = repsExpt, data = gen.list)

    })
    
    output$dt2_a_rcbd <- DT::renderDT({
       
       req(getDataup_a_rcbd()$dataUp_a_rcbd)
       req(input$blocks_a_rcbd)
       r_map <- rcbd_augmented_reactive()$layoutRandom
       checks <- 1:input$checks_a_rcbd
       b <- as.numeric(input$blocks_a_rcbd)
       len_checks <- length(checks)
       df <- as.data.frame(r_map)
       repsExpt <- as.numeric(input$nExpt_a_rcbd)
       colores <- c('royalblue','salmon', 'green', 'orange','orchid', 'slategrey',
                    'greenyellow', 'blueviolet','deepskyblue','gold','blue', 'red')
       s <- unlist(rcbd_augmented_reactive()$entriesTreatments)
       #rownames(df) <- paste("Block", rep(b:1, repsExpt))
       B <- paste("Block", rep(b:1, repsExpt), sep = "")
       E <- paste("E", rep(repsExpt:1, each = b), sep = "")
       rownames(df) <- paste(B,E)
       colnames(df) <- paste("V", 1:ncol(df), sep = "")
       options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                 scrollX = TRUE, scrollY = "1000px"))
       DT::datatable(df) %>% 
         DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                         backgroundColor = DT::styleEqual(c(checks), 
                                                          colores[1:len_checks])) %>% 
         DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                         backgroundColor = DT::styleEqual(s, 
                                                          rep('gray', length(s)
                                                          )
                         )
         )
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
       # r_map <- rcbd_augmented_reactive()$layoutRandom
       # ncols <- ncol(r_map)
       b <- as.numeric(input$blocks_a_rcbd)
       repsExpt <- as.numeric(input$nExpt_a_rcbd)
       nameexpt <- as.vector(unlist(strsplit(input$expt_name_a_rcbd, ",")))
       if (length(nameexpt) != 0) {
         Name_expt <- nameexpt
       }else Name_expt <- paste(rep('Expt', repsExpt), 1:repsExpt, sep = "")
       # infoDesign <- rcbd_augmented_reactive()$infoDesign
       # Fillers <- as.numeric(infoDesign$Fillers)
       # if (Fillers > 0) {
       #   my_names <- split_name_reactive_a()$my_names_Filles
       # }else  my_names <- split_name_reactive_a()$my_names
       
       #df <- as.data.frame(my_names)
       df <-  as.data.frame(rcbd_augmented_reactive()$exptNames)
       B <- paste("Block", rep(b:1, repsExpt), sep = "")
       E <- paste("E", rep(repsExpt:1, each = b), sep = "")
       rownames(df) <- paste(B,E)
       #rownames(df) <- paste("Block", rep(b:1, repsExpt))
       colnames(df) <- paste("V", 1:ncol(df), sep = "")
       options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                 scrollX = TRUE, scrollY = scrollY(b)))
       DT::datatable(df) %>% 
         DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                         backgroundColor = DT::styleEqual(Name_expt, rep(c('yellow'), length(Name_expt))))
     })
     
     # plot_number_reactive_a <- reactive({
     #   req(input$blocks_a_rcbd)
     #   req(input$plot_start_a_rcbd)
     #   req(split_name_reactive_a()$my_names)
     #   req(input$planter_mov1_a_rcbd)
     # 
     #   datos_name <- split_name_reactive_a()$my_names
     #   b <- as.numeric(input$blocks_a_rcbd)
     #   datos_name <- as.matrix(datos_name) 
     #   movement_planter <- input$planter_mov1_a_rcbd
     #   infoDesign <- rcbd_augmented_reactive()$infoDesign
     #   Fillers <- as.numeric(infoDesign$Fillers)
     #   plot_n_start <- as.numeric(as.vector(unlist(strsplit(input$plot_start_a_rcbd, ",")))) 
     #   planter <- input$planter_mov1_a_rcbd
     #   
     #   if (input$expt_name_a_rcbd != "") { 
     #     Name_expt <- input$expt_name_a_rcbd  
     #   }else Name_expt = paste0(rep("ARCBD", times = 1), 1)
     #   
     #   plotEXPT <- ARCBD_plot_number(plot.number = plot_n_start[1], planter = planter, b = b, name.expt = Name_expt,
     #                                 Fillers = Fillers, nameEXPT = datos_name)
     # })
     
     output$dt4_a_rcbd <- DT::renderDT({
       req(rcbd_augmented_reactive()$infoDesign)
       req(input$blocks_a_rcbd)
       #
       #plot_num1 <- plot_number_reactive_a()$plot_num
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
         options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                   scrollX = TRUE, scrollY = "1000px"))
         DT::datatable(df) %>%
           DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                           backgroundColor = DT::styleEqual(a, 
                                                            rep('yellow', length(a)))
           )
       }else {
         a <- as.vector(as.matrix(plot_num1))
         a <- a[-which(a == 0)]
         len_a <- length(a)
         df <- as.data.frame(plot_num1)
         B <- paste("Block", rep(b:1, repsExpt), sep = "")
         E <- paste("E", rep(repsExpt:1, each = b), sep = "")
         rownames(df) <- paste(B,E)
         colnames(df) <- paste("V", 1:ncol(df), sep = "")
         options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                   scrollX = TRUE, scrollY = "1000px"))
         DT::datatable(df) %>%
           DT::formatStyle(paste0(rep('V', ncol(df)), 1:ncol(df)),
                           backgroundColor = DT::styleEqual(c(a,0),
                                                            c(rep(c('yellow'), len_a),"red"))
           )
       }
     })
     
     output$dt5_a <- DT::renderDT({
       if(is.null(rcbd_augmented_reactive()$fieldbook)) return(NULL)
       df <- rcbd_augmented_reactive()$fieldbook
       options(DT.options = list(pageLength = nrow(df), autoWidth = FALSE,
                                 scrollX = TRUE, scrollY = "600px"))
       #DT::datatable(df, rownames = FALSE)
       DT::datatable(df, rownames = FALSE, options = list(
         columnDefs = list(list(className = 'dt-center', targets = "_all"))))
     })
     
     output$downloadData_a_rcbd <- downloadHandler(
       filename = function() {
         req(input$Location_a_rcbd)
         loc <- input$Location_a_rcbd
         loc <- paste(loc, "_", "ARCBD_", sep = "")
         paste(loc, Sys.Date(), ".csv", sep = "")
         #paste("RCBD_Augmented", ".csv", sep = "")
       },
       content = function(file) {
         df <- as.data.frame(rcbd_augmented_reactive()$fieldbook)
         write.csv(df, file, row.names = FALSE)
       }
     )
 
  })
}
    
## To be copied in the UI
# mod_RCBD_augmented_ui("RCBD_augmented_ui_1")
    
## To be copied in the server
# mod_RCBD_augmented_server("RCBD_augmented_ui_1")
