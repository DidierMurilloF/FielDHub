#FD first try

#' FD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
mod_FD_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Full Factorial Designs"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 4,
                   app_upload_ui(ns, "factorial"),
                   shiny::selectInput(inputId = ns("kindFD"),
                               label = "Select a Factorial Design Type:",
                               choices = c("Factorial in a RCBD" = "FD_RCBD",
                                           "Factorial in a CRD" = "FD_CRD"),
                               multiple = FALSE),

                   shiny::conditionalPanel("input.owndata != 'Yes'", ns = ns,
                                    shiny::textInput(inputId = ns("setfactors"),
                                              label = "Input # of Entries for Each Factor: (Separated by Comma)",
                                              value = "2,2,3")
                   ),

                   shiny::fluidRow(
                     shiny::column(6, style=list("padding-right: 28px;"),
                            shiny::numericInput(inputId = ns("reps.fd"), label = "Input # of Full Reps:",
                                         value = 3, min = 2)
                     ),
                     shiny::column(6,style=list("padding-left: 5px;"),
                            shiny::numericInput(ns("l.fd"), label = "Input # of Locations:",
                                         value = 1, min = 1)
                     )
                   ),
                   shiny::fluidRow(
                     shiny::column(6, style=list("padding-right: 28px;"),
                            shiny::textInput(ns("plot_start.fd"), "Starting Plot Number:", value = 101)
                     ),
                     shiny::column(6,style=list("padding-left: 5px;"),
                            shiny::textInput(ns("Location.fd"), "Input Location:", value = "FARGO")
                     )
                   ),
                   shiny::selectInput(inputId = ns("planter_mov_fd"), label = "Plot Order Layout:",
                               choices = c("serpentine", "cartesian"), multiple = FALSE,
                               selected = "serpentine"),
                   
                   app_seed_input(ns("seed.fd"), value = 123),
                   shiny::fluidRow(
                     shiny::column(6,
                            shiny::actionButton(
                              inputId = ns("RUN.fd"), "Run!", 
                              icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                              width = '100%'),
                     ),
                     shiny::column(6,
                            shiny::actionButton(
                              ns("Simulate.fd"), "Simulate!", 
                              icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                              width = '100%'),
                     )
                     
                   ), 
                   shiny::br(),
                   shiny::downloadButton(ns("downloadData.fd"), "Save experiment (ZIP)",
                                  style = "width:100%")
      ),
      
      shiny::mainPanel(
        width = 8,
        shiny::fluidRow(
          shiny::tabsetPanel(
            shiny::tabPanel("Field Layout",
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(shiny::downloadButton(ns("downloadCsv.fd"),
                                                    label = "CSV + metadata (ZIP)",
                                                    icon = shiny::icon("download"),
                                                    width = 'auto',
                                                    style="color: #337ab7; background-color: #fff; border-color: #2e6da4")),
                     fieldhub_spinner(
                       plotly::plotlyOutput(ns("layouts"), width = "97%", 
                                            height = "550px"), type = 5
                     ),
                     shiny::br(),
                     shiny::column(12, shiny::uiOutput(ns("well_panel_layout_FD")))
            ),
            shiny::tabPanel("Field Book",
                     fieldhub_spinner(DT::DTOutput(ns("FD.Output")), type = 5)
            )
          )
        )
      )
    ) 
  )
}
#' FD Server Functions
#'
#' @noRd 
mod_FD_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session){
    
    ns <- session$ns

    app_upload_dialog_observer(input, "factorial")

    get_data_factorial <- shiny::reactive({

      if (input$owndata == "Yes") {
        data_ingested <- app_read_upload(input, "factorial")
        if (is.null(data_ingested)) return(NULL)
        data_up <- data_ingested$data
        data_up <- as.data.frame(data_up[,1:2])
        data_factorial <- na.omit(data_up)
        colnames(data_factorial) <- c("FACTOR", "LEVEL")
        nt <- length(unique(data_factorial$FACTOR))
        if (nt < 2) {
          app_report_problem("More than one factor needs to be specified.")
          return(NULL)
        }
        return(list(data_fd = data_factorial, setfactors = NULL))
      } else {
        shiny::req(input$setfactors)
        setfactors.fd <- app_attempt(
          parse_whole_numbers(input$setfactors, "# of Entries for Each Factor")
        )
        if (is.null(setfactors.fd)) return(NULL)
        if (length(setfactors.fd) < 2) {
          app_report_problem("More than one factor needs to be specified.")
          return(NULL)
        }
        # No entry list is built here: full_factorial() expands its own
        # factor/level combinations from a bare per-factor level count
        # (design_args_FD()/full_factorial(setfactors = )).
        return(list(data_fd = NULL, setfactors = setfactors.fd))
      }
    }) |>
      shiny::bindEvent(input$RUN.fd)

    fd_inputs <- shiny::reactive({
      shiny::req(get_data_factorial())
      shiny::req(input$plot_start.fd)
      shiny::req(input$Location.fd)
      shiny::req(input$l.fd)
      shiny::req(input$kindFD)
      shiny::req(input$planter_mov_fd)

      plot_start <- validate_design(parse_whole_numbers(
        input$plot_start.fd, "Starting Plot Number"
      ))
      planter <- input$planter_mov_fd
      location_names <-  as.vector(unlist(strsplit(input$Location.fd, ",")))
      seed <- validate_design(app_design_seed(input$seed.fd))
      reps <- as.numeric(input$reps.fd)
      l <- as.numeric(input$l.fd)
      type <- if (input$kindFD == "FD_CRD") 1 else 2

      return(
        list(
        setfactors = get_data_factorial()$setfactors,
        reps = reps,
        planter = planter,
        plot_start = plot_start,
        l = l,
        location_names = location_names,
        type = type,
        seed = seed))
    }) |>
      shiny::bindEvent(input$RUN.fd)

    fd_reactive <- shiny::reactive({

      shiny::req(get_data_factorial())
      shiny::req(fd_inputs())

      shinyjs::show(id = "downloadCsv.fd")

      validate_design(do.call(
        full_factorial, design_args_FD(fd_inputs(), get_data_factorial()$data_fd)
      ))

    }) |>
      shiny::bindEvent(input$RUN.fd)

    upDateSites <- shiny::reactive({
      shiny::req(input$l.fd)
      locs <- as.numeric(input$l.fd)
      sites <- 1:locs
      return(list(sites = sites))
    })  |> 
      shiny::bindEvent(input$RUN.fd)
    
    
    reactive_layoutFD <- app_classic_layout(input, output, session,
      design = function() fd_reactive(),
      planter = function() fd_inputs()$planter,
      spec = classic_workflow_spec("FD"),
      locations = function() as.numeric(upDateSites()$sites)
    )

    app_classic_workflow(input, output, session,
      design = function() fd_reactive(),
      layout = function() reactive_layoutFD(),
      seed = function() fd_inputs()$seed,
      selected = function() return(as.numeric(input$locLayout_fd)),
      spec = classic_workflow_spec("FD"),
      simulation_ready = function() {
        shiny::req(fd_reactive()$fieldBook)
      }
    )

  })
}
