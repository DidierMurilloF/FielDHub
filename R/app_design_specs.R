#' How each design page is built
#'
#' @description One entry per design, rendered and run by
#' \code{mod_design_ui()}/\code{mod_design_server()}. Every page has:
#' \itemize{
#'   \item \code{title}: heading of the page.
#'   \item \code{engine}: the public design function.
#'   \item \code{controls}: the sidebar, built from the shared \code{ctl_*()}
#'     constructors (R/app_controls.R), in display order.
#'   \item \code{upload}: key of the page's upload controls
#'     (\code{app_upload_spec()}), or \code{NULL}; \code{upload_shape} turns
#'     the file into the entries the engine reads.
#'   \item \code{data}: \code{function(upload, controls)}, the \code{data}
#'     sent to the engine (the shaped upload, or \code{NULL}).
#'   \item \code{values}: \code{function(controls, data)}, the \code{values}
#'     the argument builder reads, from the parsed controls
#'     (\code{read_design_controls()}) and \code{data}.
#'   \item \code{args}: the argument builder \code{design_args_<Module>()},
#'     so the page and a direct API call build the same design.
#'   \item \code{kind}: \code{"classic"} or \code{"spatial"}.
#'   \item \code{workflow}, \code{layout}: the result panels
#'     (\code{classic_workflow_spec()}, or \code{spatial_workflow_spec()}
#'     with no \code{layout}).
#'   \item \code{summary}: whether the page shows a "Summary Design" tab.
#'   \item \code{long_running}: whether a run may take long (used to run it
#'     without blocking other sessions).
#' }
#'
#' A spatial page (\code{kind = "spatial"}) builds its design in steps
#' (\code{app_spatial_page()}): Run! reads the controls, and where the
#' design needs one computes the allocation (\code{optim}: the engine and
#' argument builder of the \code{do_optim()} step, and the \code{values}
#' field its result goes to). It then offers its \code{steps} with
#' \code{stage = "run"} (the field size, from plain choice functions); each
#' Randomize! reads them, offers the \code{"randomize"} steps (the
#' percentage of checks) and builds the design. Its other fields describe
#' the result tabs: \code{setup} (what the first tab shows), \code{entries}
#' (the entry tables), \code{panels} (the field grids or plots),
#' \code{field_size} (the field the simulation fills), \code{accept} (a
#' last check of the design), \code{upload_repeats} (a flag that lets an
#' upload repeat entries), \code{randomizing} (the progress message) and
#' \code{file_tag} (the export file name).
#'
#' Every page has the same shared controls (locations where its engine
#' takes them, plot order, starting plots, location names, seed); counts
#' that describe the design keep the page's own defaults, and so do the
#' spatial pages' starting plot (1) and locations. A minimum differs from
#' the concept's only where the engine accepts a smaller value:
#' \code{CRD()} takes a single treatment, and \code{CRD()},
#' \code{latin_square()}, \code{split_plot()}, \code{split_split_plot()} and
#' \code{strip_plot()} a single replicate; the split-plot engines take a
#' single whole plot. \code{full_factorial()} takes a single replicate for
#' its CRD type only; a minimum cannot follow the design type the page
#' selects (it is a hint written into the page), so the page offers 1 and
#' the engine explains that the RCBD type needs 2. The spatial pages keep
#' the smallest number of entries and locations they have always offered.
#'
#' The specs are built once per R session (\code{design_app_spec()}).
#' @noRd
fieldhub_design_specs <- function() {
  if (is.null(design_spec_cache$specs)) design_spec_cache$specs <- build_design_specs()
  design_spec_cache$specs
}

#' Specs built by the first fieldhub_design_specs() call of the session
#' @noRd
design_spec_cache <- new.env(parent = emptyenv())

#' Build every design page spec (see fieldhub_design_specs())
#' @noRd
build_design_specs <- function() {
  classic <- function(module, title, engine, controls, upload, upload_columns,
                      values, omit_na = TRUE, upload_check = identity,
                      data = function(upload, controls) upload, summary = FALSE) {
    workflow <- classic_workflow_spec(module)
    list(
      module = module, title = title, engine = engine, controls = controls,
      upload = upload,
      upload_shape = function(data) upload_check(shape_design_upload(data, upload_columns, omit_na)),
      data = data, values = values,
      args = get(paste0("design_args_", module), mode = "function"),
      kind = "classic", workflow = workflow, layout = workflow$layout, summary = summary,
      long_running = FALSE
    )
  }
  spatial <- function(module, title, engine, controls, upload, upload_columns, values, steps,
                      setup, entries, panels, file_tag, upload_check = identity,
                      data = function(upload, controls) upload, optim = NULL,
                      field_size = function(design, values) list(nrows = values$nrows, ncols = values$ncols),
                      accept = identity, upload_repeats = NULL, long_running = TRUE,
                      randomizing = "Randomizing ...") {
    list(
      module = module, title = title, engine = engine, controls = controls, upload = upload,
      upload_shape = function(data) upload_check(shape_design_upload(data, upload_columns)),
      data = data, values = values,
      args = get(paste0("design_args_", module), mode = "function"),
      kind = "spatial", optim = optim, steps = steps, setup = setup, entries = entries,
      panels = panels, workflow = spatial_workflow_spec(module), layout = NULL, summary = FALSE,
      field_size = field_size, accept = accept, upload_repeats = upload_repeats,
      long_running = long_running, randomizing = randomizing, file_tag = file_tag
    )
  }
  # Result tabs of the spatial pages
  entry_table <- function(data, caption = NULL, height = "600px", filter = "top") {
    list(data = data, caption = caption, height = height, filter = filter)
  }
  grid_panel <- function(title, id, export, view) {
    list(type = "grid", title = title, id = id, export = export, view = view)
  }
  plot_panel <- function(title, id, view) list(type = "plot", title = title, id = id, view = view)
  field_panel <- function(view) grid_panel("Randomized Field", "field_layout", "Entry layout", view)
  numbers_panel <- function(component) {
    grid_panel("Plot Number Field", "plot_numbers", "Plot numbers", function(design, location, values) {
      field_grid_view(design[[component]][[location]])
    })
  }
  checks_view <- function(design, location, values) {
    checks <- design$infoDesign$entry_checks[[location]]
    field_grid_view(design$layoutRandom[[location]], highlight = checks,
                    colours = spatial_highlight_colours("checks", length(checks)))
  }
  summary_setup <- list(type = "summary")
  # The diagonal pages: the percentages of checks of the field, and each
  # location's entries and checks
  percent_setup <- list(
    type = "table", stage = "randomize",
    caption = paste("Reference guide to design your experiment. Choose the percentage (%)",
                    "of checks based on the total number of plots you want to have in the final layout."),
    view = function(values, choices) choices$checks_percent$table
  )
  diagonal_entry_tables <- list(
    function(design, location, values) {
      entry_table(entry_list_view(design$data_entry[[location]]), "List of Entries.")
    },
    function(design, location, values) {
      entry_table(diagonal_checks_view(design, location), "Table of Checks.", height = "350px",
                  filter = "none")
    }
  )
  # Location views, plot order, plot starts, experiment and location names
  # of a spatial page
  spatial_shared <- function(expt_name = ctl_expt_name(), trim_locations = FALSE) {
    list(ctl_location_view(), ctl_planter(), ctl_plot_start("1"), expt_name,
         ctl_location_names(trim = trim_locations))
  }
  # Entries come from the file on the upload path, from `t` otherwise
  entry_count <- function(controls, data) if (is.null(data)) controls$t else nrow(data)
  block_sizes <- function(design) {
    ctl_dependent_select("k", depends_on = "t", options = function(values, data) {
      block_size_choices(entry_count(values, data), design)
    })
  }
  incomplete_block_values <- function(controls, data) {
    list(reps = controls$reps, k = controls$k, t = entry_count(controls, data),
         planter = controls$planter, plot_start = controls$plot_start, l = controls$l,
         location_names = controls$location_names, seed = controls$seed)
  }
  lattice <- function(module, title, engine, design, upload, treatments) {
    classic(module, title, engine, upload = upload, upload_columns = c("ENTRY", "NAME"),
      summary = TRUE, values = incomplete_block_values,
      controls = list(
        ctl_count("t", treatments, generated_only = TRUE), ctl_reps(3), block_sizes(design),
        ctl_locations(), ctl_planter(), ctl_plot_start(), ctl_location_names(), ctl_seed()
      ))
  }
  split_types <- function(name) {
    stats::setNames(c("2", "1"), paste(name, c("in a RCBD", "in a CRD")))
  }
  crd_type_only <- "input.type == '1'"

  list(
    CRD = classic("CRD", "Completely Randomized Design", CRD, upload = "crd",
      upload_columns = "TREATMENT",
      controls = list(
        ctl_count("t", 15, min = 1, generated_only = TRUE), ctl_reps(4, min = 1),
        ctl_planter(), ctl_plot_start(), ctl_location_names(), ctl_seed()
      ),
      # CRD() reads each treatment's replicates from the file's REP column
      data = function(upload, controls) {
        if (is.null(upload)) return(NULL)
        upload$REP <- rep(controls$reps, times = nrow(upload))
        colnames(upload) <- c("TREATMENT", "REP")
        upload
      },
      values = function(controls, data) {
        design_values_CRD(treatment_count = entry_count(controls, data), reps = controls$reps,
          planter = controls$planter, plot_start = controls$plot_start[1],
          location_names = controls$location_names, seed = controls$seed, data = data)
      }),
    RCBD = classic("RCBD", "Randomized Complete Block Designs", RCBD, upload = "rcbd",
      upload_columns = "TREATMENT",
      controls = list(
        ctl_count("t", 18, generated_only = TRUE), ctl_reps(3),
        ctl_flag("use_checks"),
        ctl_checks(2, show_if = "input.use_checks == true", enabled_by = "use_checks"),
        ctl_rep_checks(show_if = "input.use_checks == true", enabled_by = "use_checks"),
        ctl_flag("spread_checks", show_if = "input.use_checks == true", enabled_by = "use_checks",
                 disabled_value = TRUE),
        ctl_preview("checks_note", show_if = "input.use_checks == true",
                    text = function(raw, uploaded) rcbd_checks_note(raw, uploaded)),
        ctl_locations(), ctl_planter(), ctl_plot_start(), ctl_flag("continuous"),
        ctl_location_names(), ctl_seed()
      ),
      values = function(controls, data) {
        list(reps = controls$reps, t = entry_count(controls, data), planter = controls$planter,
             plot_start = controls$plot_start, l = controls$l,
             location_names = controls$location_names, continuous = controls$continuous,
             seed = controls$seed, checks = controls$checks, rep_checks = controls$rep_checks,
             spread_checks = controls$spread_checks)
      }),
    LSD = classic("LSD", "Latin Square Design", latin_square, upload = "lsd",
      upload_columns = c("ROW", "COLUMN", "TREATMENT"),
      controls = list(
        ctl_count("t", 5, max = 10, generated_only = TRUE), ctl_reps(1, min = 1),
        ctl_planter(), ctl_plot_start(), ctl_location_names(), ctl_seed()
      ),
      values = function(controls, data) {
        list(t = if (is.null(data)) controls$t, reps = controls$reps,
             plot_start = controls$plot_start[1], planter = controls$planter,
             location_names = controls$location_names[1], seed = controls$seed)
      }),
    FD = classic("FD", "Full Factorial Designs", full_factorial, upload = "factorial",
      upload_columns = c("FACTOR", "LEVEL"), upload_check = check_factorial_upload,
      controls = list(
        ctl_select("type", split_types("Factorial"), convert = as.numeric),
        ctl_text_list("setfactors", "2,2,3", generated_only = TRUE,
                      parse = function(value, label, values) parse_control_factor_counts(value, label)),
        ctl_reps(3, min = 1), ctl_locations(), ctl_planter(), ctl_plot_start(), ctl_location_names(),
        ctl_seed()
      ),
      values = function(controls, data) {
        list(setfactors = if (is.null(data)) controls$setfactors, reps = controls$reps,
             planter = controls$planter, plot_start = controls$plot_start, l = controls$l,
             location_names = controls$location_names, type = controls$type,
             seed = controls$seed)
      }),
    SPD = classic("SPD", "Split-Plot Design", split_plot, upload = "spd",
      upload_columns = c("WHOLEPLOT", "SUBPLOT"), omit_na = FALSE,
      controls = list(
        ctl_select("type", split_types("Split-Plot"), convert = as.numeric),
        ctl_count("wp", 4, min = 1, generated_only = TRUE),
        ctl_count("sp", 3, generated_only = TRUE),
        ctl_reps(3, min = 1), ctl_locations(),
        # RCBD-type layouts number whole plots in a fixed order: the plot
        # order only applies to the CRD type
        ctl_planter(show_if = crd_type_only), ctl_plot_start(), ctl_location_names(), ctl_seed()
      ),
      values = function(controls, data) {
        design_values_SPD(wp_count = controls$wp, sp_count = controls$sp, reps = controls$reps,
          l = controls$l, seed = controls$seed, planter = controls$planter,
          plot_start = controls$plot_start, location_names = controls$location_names,
          type = controls$type, data = data)
      }),
    SSPD = classic("SSPD", "Split-Split-Plot Design", split_split_plot, upload = "sspd",
      upload_columns = c("WHOLEPLOT", "SUBPLOT", "SUB_SUBPLOT"), omit_na = FALSE,
      controls = list(
        ctl_select("type", split_types("Split-Split Plot"), convert = as.numeric),
        ctl_count("wp", 2, min = 1, generated_only = TRUE),
        ctl_count("sp", 2, generated_only = TRUE),
        ctl_count("ssp", 5, generated_only = TRUE),
        ctl_reps(3, min = 1), ctl_locations(),
        ctl_planter(show_if = crd_type_only), ctl_plot_start(), ctl_location_names(), ctl_seed()
      ),
      values = function(controls, data) {
        design_values_SSPD(wp_count = controls$wp, sp_count = controls$sp,
          ssp_count = controls$ssp, reps = controls$reps, l = controls$l, seed = controls$seed,
          planter = controls$planter, plot_start = controls$plot_start,
          location_names = controls$location_names, type = controls$type, data = data)
      }),
    STRIPD = classic("STRIPD", "Strip-Plot Design", strip_plot, upload = "strip",
      upload_columns = c("Hplot", "Vplot"), omit_na = FALSE,
      controls = list(
        ctl_count("Hplots", 5, generated_only = TRUE), ctl_count("Vplots", 5, generated_only = TRUE),
        ctl_reps(3, min = 1), ctl_locations(), ctl_planter(), ctl_plot_start(),
        ctl_location_names(), ctl_flag("randomizeH"), ctl_flag("randomizeV"), ctl_seed()
      ),
      values = function(controls, data) {
        # On the upload path the strips are counted from the file
        strips <- if (is.null(data)) c(controls$Hplots, controls$Vplots) else upload_level_counts(data)
        list(Hplots = strips[1], Vplots = strips[2], reps = controls$reps, l = controls$l,
             seed = controls$seed, planter = controls$planter,
             plot_start = controls$plot_start, location_names = controls$location_names,
             randomizeH = controls$randomizeH, randomizeV = controls$randomizeV)
      }),
    IBD = classic("IBD", "Incomplete Blocks Design", incomplete_blocks, upload = "ibd",
      upload_columns = c("ENTRY", "NAME"), summary = TRUE, values = incomplete_block_values,
      controls = list(
        ctl_count("t", 15, generated_only = TRUE), ctl_reps(4), block_sizes("incomplete_blocks"),
        ctl_locations(), ctl_planter(), ctl_plot_start(), ctl_location_names(), ctl_seed()
      )),
    RowCol = classic("RowCol", "Row-Column Design", row_column, upload = "rcd",
      upload_columns = c("ENTRY", "NAME"), summary = TRUE,
      controls = list(
        ctl_count("t", 42, generated_only = TRUE),
        ctl_dependent_select("nrows", depends_on = "t", options = function(values, data) {
          block_size_choices(entry_count(values, data), "row_column")
        }),
        ctl_reps(2), ctl_locations(), ctl_planter(), ctl_plot_start(), ctl_location_names(),
        ctl_seed()
      ),
      values = function(controls, data) {
        list(reps = controls$reps, nrows = controls$nrows, t = entry_count(controls, data),
             plot_start = controls$plot_start, planter = controls$planter, l = controls$l,
             location_names = controls$location_names, seed = controls$seed)
      }),
    Alpha_Lattice = lattice("Alpha_Lattice", "Alpha Lattice Design", alpha_lattice,
                            "alpha_lattice", "alpha", 36),
    Square_Lattice = lattice("Square_Lattice", "Square Lattice Design", square_lattice,
                             "square_lattice", "square", 49),
    Rectangular_Lattice = lattice("Rectangular_Lattice", "Rectangular Lattice Design",
                                  rectangular_lattice, "rectangular_lattice", "rect", 30),
    # optimized_arrangement() runs in well under a second
    Optim = spatial("Optim", "Unreplicated Optimized Arrangement", optimized_arrangement,
      upload = "optim", upload_columns = c("ENTRY", "NAME", "REPS"),
      upload_check = check_reps_upload, long_running = FALSE, file_tag = "Optim_",
      controls = c(
        list(ctl_checks(4, generated_only = TRUE), ctl_rep_checks("8,8,8,8", generated_only = TRUE),
             ctl_count("lines", 280, min = 5, generated_only = TRUE), ctl_locations()),
        spatial_shared(expt_name = ctl_expt_name(split = FALSE)), list(ctl_seed())
      ),
      values = function(controls, data) {
        list(lines = controls$lines, checks = controls$checks, rep_checks = controls$rep_checks,
             planter = controls$planter, l = controls$l, plot_start = controls$plot_start,
             seed = controls$seed, expt_name = controls$expt_name,
             location_names = controls$location_names,
             plots = if (is.null(data)) optim_total_plots(controls$lines, controls$rep_checks) else sum(data$REPS))
      },
      steps = list(ctl_dimensions(function(values, data) optim_field_choices(values$plots))),
      setup = summary_setup,
      entries = list(
        function(design, location, values) {
          entry_table(entry_list_view(design$dataEntry, c("ENTRY", "NAME", "REPS")), "List of Entries.")
        },
        function(design, location, values) {
          entries <- design$dataEntry
          entry_table(entries[entries$REPS > 1, ], "Table of checks.", height = "350px", filter = "none")
        }
      ),
      panels = list(
        field_panel(function(design, location, values) {
          checks <- as.vector(design$genEntries$entry_checks)
          field_grid_view(design$layoutRandom[[location]], highlight = checks,
                          colours = spatial_highlight_colours("checks", length(checks)))
        }),
        numbers_panel("plotNumber")
      ),
      field_size = function(design, values) {
        list(nrows = max(design$fieldBook$ROW), ncols = max(design$fieldBook$COLUMN))
      }),
    # A new choice of filler plots offers other field sizes without a new Run!
    pREPS = spatial("pREPS", "Single and Multi-Location P-rep Design", partially_replicated,
      upload = "prep", upload_columns = c("ENTRY", "NAME", "REPS"),
      upload_check = check_reps_upload, file_tag = "pREP_",
      randomizing = "Running p-rep optimization ...",
      controls = c(
        list(ctl_text_list("repGens", "75,150", generated_only = TRUE),
             ctl_text_list("repUnits", "2,1", generated_only = TRUE,
                           parse = function(value, label, values) {
                             parse_control_rep_units(value, label, values$repGens)
                           }),
             ctl_flag("allow_fillers"), ctl_locations()),
        spatial_shared(expt_name = ctl_expt_name(split = FALSE)), list(ctl_seed())
      ),
      values = function(controls, data) {
        list(repGens = controls$repGens, repUnits = controls$repUnits,
             allow_fillers = controls$allow_fillers, planter = controls$planter, l = controls$l,
             plot_start = controls$plot_start, seed = controls$seed,
             expt_name = controls$expt_name, location_names = controls$location_names,
             plots = if (is.null(data)) sum(controls$repGens * controls$repUnits) else sum(data$REPS))
      },
      steps = list(ctl_dimensions(function(values, data) {
        prep_field_choices(values$plots, values$allow_fillers)
      }, depends_on = "allow_fillers")),
      setup = summary_setup,
      entries = list(function(design, location, values) {
        entry_table(entry_list_view(design$dataEntry, c("ENTRY", "NAME", "REPS")), height = "500px")
      }),
      panels = list(
        field_panel(function(design, location, values) {
          replicated <- as.vector(design$genEntries$entry_checks)
          field_grid_view(design$layoutRandom[[location]], fillers = design$fillerField[[location]],
                          highlight = replicated,
                          colours = spatial_highlight_colours("replicated", length(replicated)))
        }),
        numbers_panel("plotNumber")
      )),
    # RCBD_augmented() runs in well under a second; its layouts are plots
    RCBD_augmented = spatial("RCBD_augmented", "Augmented RCBD", RCBD_augmented,
      upload = "arcbd", upload_columns = c("ENTRY", "NAME"), long_running = FALSE,
      file_tag = "ARCBD_",
      controls = c(
        list(
          ctl_count("lines", 180, generated_only = TRUE), ctl_checks(4, max = 10),
          # The blocks that fit the entries: the typed count, or the rows of
          # the file after its checks
          ctl_dependent_select("b", depends_on = c("lines", "checks"), options = function(values, data) {
            augmented_block_choices(augmented_lines(values$lines, values$checks, data), values$checks)
          }),
          ctl_count("repsExpt", 1, max = 100), ctl_select("repsStack", show_if = "input.repsExpt > 1"),
          ctl_flag("random"),
          ctl_preview("random_note", show_if = "input.random == false",
                      text = function(raw, uploaded) augmented_random_note(raw$random)),
          ctl_locations()
        ),
        spatial_shared(), list(ctl_seed())
      ),
      values = function(controls, data) {
        list(lines = augmented_lines(controls$lines, controls$checks, data), checks = controls$checks,
             b = controls$b, repsExpt = controls$repsExpt,
             repsStack = if (controls$repsExpt > 1) controls$repsStack, random = controls$random,
             l = controls$l, planter = controls$planter, plot_start = controls$plot_start,
             seed = controls$seed, expt_name = controls$expt_name,
             location_names = controls$location_names)
      },
      steps = list(ctl_dimensions(function(values, data) {
        augmented_field_choices(values$lines, values$checks, values$b)
      })),
      setup = summary_setup,
      entries = list(
        function(design, location, values) {
          entry_table(entry_list_view(design$data_entry), "List of Entries.")
        },
        function(design, location, values) {
          entry_table(design$data_entry[seq_len(values$checks), ], "Table of checks.",
                      height = "350px", filter = "none")
        }
      ),
      panels = list(
        plot_panel("Field Layout", "field_layout", function(design, location, values) {
          checked_layout_view(design, location = location)$out_layout
        }),
        plot_panel("Plot Number Field", "plot_numbers", function(design, location, values) {
          checked_layout_view(design, location = location)$out_layoutPlots
        })
      ),
      field_size = function(design, values) {
        list(nrows = length(unique(design$fieldBook$ROW)),
             ncols = length(unique(design$fieldBook$COLUMN)))
      }),
    # The field sizes are searched after Run!, the percentages of checks
    # after Randomize!
    Diagonal = spatial("Diagonal", "Unreplicated Single Diagonal Arrangement", diagonal_arrangement,
      upload = "sdiag", upload_columns = c("ENTRY", "NAME"), file_tag = "Diagonal_",
      controls = c(
        list(ctl_count("lines", 287, min = 50, generated_only = TRUE), ctl_checks(4, max = 10),
             ctl_locations()),
        spatial_shared(), list(ctl_seed())
      ),
      values = function(controls, data) {
        list(lines = controls$lines, checks = controls$checks, l = controls$l,
             planter = controls$planter, plot_start = controls$plot_start, seed = controls$seed,
             expt_name = controls$expt_name, location_names = controls$location_names,
             entries = diagonal_entries(controls$lines, controls$checks, data))
      },
      steps = list(
        ctl_dimensions(function(values, data) {
          entries <- values$entries
          diagonal_field_choices(entries$field_entries, entries$entries - values$checks,
                                 entries$checks_entries, planter = values$planter, data = data)
        }),
        ctl_checks_percent(function(values, data) {
          diagonal_percent_choices(values$nrows, values$ncols, values$entries$checks_entries,
                                   values$entries$entries, planter = values$planter, data = data)
        })
      ),
      setup = percent_setup,
      entries = diagonal_entry_tables,
      panels = list(field_panel(checks_view), numbers_panel("plotsNumber")),
      field_size = function(design, values) {
        list(nrows = design$infoDesign$rows, ncols = design$infoDesign$columns)
      })
  )
}

#' The page spec of one design
#'
#' @param module Registry name of the design (\code{"CRD"}, \code{"RCBD"},
#'   ...).
#' @return The spec (see \code{fieldhub_design_specs()}).
#' @noRd
design_app_spec <- function(module) {
  specs <- fieldhub_design_specs()
  if (!is.character(module) || !is.null(dim(module)) || length(module) != 1L ||
      is.na(module) || !module %in% names(specs)) {
    fieldhub_abort("Choose an available design page.", data = list(choices = names(specs)))
  }
  specs[[module]]
}
