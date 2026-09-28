#' IBD UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#'
mod_IBD_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4("Incomplete Blocks Design"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 4,
        app_upload_ui(ns, "ibd"),
        shiny::conditionalPanel(
          condition = "input.owndataibd != 'Yes'",
          ns = ns,
          shiny::numericInput(ns("t.ibd"),
                       label = "Input # of Treatments:",
                       value = 15,
                       min = 2)
        ),

        shiny::numericInput(inputId = ns("r.ibd"),
                     label = "Input # of Full Reps:", 
                     value = 4, 
                     min = 2),
        
        shiny::selectInput(inputId = ns("k.ibd"),
                    label = "Input # of Plots per IBlock:", 
                    choices = ""),
        
        shiny::numericInput(inputId = ns("l.ibd"),
                     label = "Input # of Locations:",
                     value = 1, 
                     min = 1),
        shiny::selectInput(inputId = ns("planter_mov_ibd"),
          label = "Plot Order Layout:",
          choices = c("serpentine", "cartesian"), 
          multiple = FALSE,
          selected = "serpentine"),
        shiny::fluidRow(
          shiny::column(6, style=list("padding-right: 28px;"),
                 shiny::textInput(inputId = ns("plot_start.ibd"),
                           "Starting Plot Number:", 
                           value = 101)
          ),
          shiny::column(6,style=list("padding-left: 5px;"),
                 shiny::textInput(inputId = ns("Location.ibd"),
                           "Input Location:", 
                           value = "FARGO")
          )
        ), 
        app_seed_input(ns("seed.ibd"), value = 4),
        shiny::fluidRow(
          shiny::column(6,
                 shiny::actionButton(
                   inputId = ns("RUN.ibd"), 
                   label = "Run!", 
                   icon = shiny::icon("circle-nodes", verify_fa = FALSE),
                   width = '100%'),
          ),
          shiny::column(6,
                 shiny::actionButton(
                   ns("Simulate.ibd"), 
                   label = "Simulate!", 
                   icon = shiny::icon("greater-than-equal", verify_fa = FALSE),
                   width = '100%'),
          )
          
        ), 
        shiny::br(),
        shiny::downloadButton(ns("downloadData.ibd"),
                       "Save experiment (ZIP)",
                       style = "width:100%")
      ),
      
      shiny::mainPanel(
        width = 8,
        shiny::fluidRow(
          shiny::tabsetPanel(
            shiny::tabPanel(
              "Summary Design",
              shiny::br(),
              fieldhub_spinner(
                shiny::verbatimTextOutput(outputId = ns("summary_ibd"),
                                   placeholder = FALSE), 
                type = 4
              ),
              style = "padding-right: 40px;"
            ),
            shiny::tabPanel("Field Layout",
                     shinyjs::useShinyjs(),
                     shinyjs::hidden(
                       shiny::downloadButton(
                         ns("downloadCsv.ibd"), 
                         label = "CSV + metadata (ZIP)",
                         icon = shiny::icon("download"),
                         width = 'auto',
                         style="color: #337ab7; background-color: #fff; border-color: #2e6da4")
                      ),
                     fieldhub_spinner(
                       plotly::plotlyOutput(ns("layouts"), 
                                            width = "97%", 
                                            height = "550px"),
                       type = 5
                     ),
                     shiny::br(),
                     shiny::column(12,
                            shiny::uiOutput(ns("well_panel_layout_IBD"))
                            )
            ),
            shiny::tabPanel("Field Book",
                     fieldhub_spinner(
                       DT::DTOutput(ns("IBD.output")), 
                       type = 5
                       )
            )
          )
        )
      )
    )
  )
}

#' IBD Server Functions
#'
#' @noRd 
mod_IBD_server <- function(id) {
  shiny::moduleServer( id, function(input, output, session){
    
    ns <- session$ns

    app_upload_dialog_observer(input, "ibd")

    init_data_ibd <- shiny::reactive({

      if(input$owndataibd == "Yes") {
        data_ingested <- app_read_upload(input, "ibd")
        if (is.null(data_ingested)) return(NULL)
        data_up <- data_ingested$data
        data_up <- as.data.frame(data_up[,1:2])
        data_ibd <- na.omit(data_up)
        colnames(data_ibd) <- c("ENTRY", "NAME")
        treatments = nrow(data_ibd)
        return(list(data_ibd = data_ibd, treatments = treatments))
      } else {
        shiny::req(input$t.ibd)
        nt <- as.numeric(input$t.ibd)
        # No entry list is built here: incomplete_blocks() generates its own
        # "G-1".."G-n" labels from a bare treatment count
        # (design_args_IBD()/incomplete_blocks(t = )).
        return(list(data_ibd = NULL, treatments = nt))
      }
    })
    
    list_to_observe <- shiny::reactive({
      shiny::req(init_data_ibd())
      list(
        entry_list = input$owndataibd,
        entries = init_data_ibd()$treatments
      )
    })
    
    shiny::observeEvent(list_to_observe(), {
      shiny::req(init_data_ibd())
      options <- valid_block_sizes(
        as.numeric(shiny::req(init_data_ibd())$treatments),
        "incomplete_blocks"
      )
      k <- if (length(options) == 0L) "No Options Available" else options
      
      if (length(options) > 2L) {
        selected <- options[ceiling(length(options) / 2)]
      } else selected <- k[1]
      
      shiny::updateSelectInput(session = session,
                        inputId = 'k.ibd', 
                        label = "Input # of Plots per IBlock:",
                        choices = k, selected = selected)
      
    })
    
    get_data_ibd <- shiny::reactive({
      if (is.null(init_data_ibd())) {
        app_report_problem("Check input file and try again!")
        return(NULL)
      } else return(init_data_ibd())
    }) |>
      shiny::bindEvent(input$RUN.ibd)
    
    ibd_inputs <- shiny::reactive({
      
      shiny::req(get_data_ibd())
      
      shiny::req(input$r.ibd)
      shiny::req(input$k.ibd)
      shiny::req(input$plot_start.ibd)
      shiny::req(input$Location.ibd)
      shiny::req(input$l.ibd)
      shiny::req(input$planter_mov_ibd)
      
      reps <- as.numeric(input$r.ibd)
      k <- as.numeric(input$k.ibd)
      treatments <- as.numeric(get_data_ibd()$treatments)
      planter <- input$planter_mov_ibd
      plot_start <- validate_design(parse_whole_numbers(
        input$plot_start.ibd, "Starting Plot Number"
      ))
      location_names <-  as.vector(unlist(strsplit(input$Location.ibd, ",")))
      seed <- validate_design(app_design_seed(input$seed.ibd))
      l <- as.numeric(input$l.ibd)
      if (input$k.ibd == "No Options Available") {
        app_report_problem("No options for this combination of treatments!")
        return(NULL)
      }
      return(list(
        reps = reps,
        k = k,
        t = treatments,
        planter = planter,
        plot_start = plot_start,
        l = l,
        location_names = location_names,
        seed = seed))
    }) |>
      shiny::bindEvent(input$RUN.ibd)

    IBD_reactive <- shiny::reactive({
      shiny::req(get_data_ibd())
      shiny::req(ibd_inputs())

      shinyjs::show(id = "downloadCsv.ibd")

      # incomplete_blocks() itself rejects an under-replicated design (a
      # classed fieldhub_input_error surfaced below through
      # validate_design()); no duplicate reps < 2 check is needed here.
      validate_design(do.call(
        incomplete_blocks, design_args_IBD(ibd_inputs(), get_data_ibd()$data_ibd)
      ))

    }) |>
      shiny::bindEvent(input$RUN.ibd)
    
    output$summary_ibd <- shiny::renderPrint({
      shiny::req(IBD_reactive())
      cat("Randomization was successful!", "\n", "\n")
      print(IBD_reactive(), n = 6)
    })
    
    upDateSites <- shiny::eventReactive(input$RUN.ibd, {
      shiny::req(input$l.ibd)
      locs <- as.numeric(input$l.ibd)
      sites <- 1:locs
      return(list(sites = sites))
    })
    
    
    reactive_layoutIBD <- app_classic_layout(input, output, session,
      design = function() IBD_reactive(),
      planter = function() ibd_inputs()$planter,
      spec = classic_workflow_spec("IBD"),
      locations = function() as.numeric(upDateSites()$sites)
    )

    app_classic_workflow(input, output, session,
      design = function() IBD_reactive(),
      layout = function() reactive_layoutIBD(),
      seed = function() ibd_inputs()$seed,
      selected = function() return(as.numeric(input$locLayout_ibd)),
      spec = classic_workflow_spec("IBD"),
      simulation_ready = function() {
        shiny::req(IBD_reactive()$fieldBook)
      }
    )

  })
}
