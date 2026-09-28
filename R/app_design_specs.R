#' How each classic design page is built
#'
#' @description One entry per classic design, rendered and run by
#' \code{mod_design_ui()}/\code{mod_design_server()}:
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
#'   \item \code{workflow}, \code{layout}: the result panels
#'     (\code{classic_workflow_spec()}).
#'   \item \code{summary}: whether the page shows a "Summary Design" tab.
#'   \item \code{long_running}: whether a run may take long (used to run it
#'     without blocking other sessions).
#' }
#'
#' Every page has the same shared controls (locations where its engine
#' takes them, plot order, starting plots, location names, seed); counts
#' that describe the design keep the page's own defaults. A minimum differs
#' from the concept's only where the engine accepts a smaller value:
#' \code{CRD()} takes a single treatment, and \code{CRD()},
#' \code{latin_square()}, \code{split_plot()}, \code{split_split_plot()} and
#' \code{strip_plot()} a single replicate; the split-plot engines take a
#' single whole plot.
#' @noRd
fieldhub_design_specs <- function() {
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
      workflow = workflow, layout = workflow$layout, summary = summary,
      long_running = FALSE
    )
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
        ctl_count("checks", 2, show_if = "input.use_checks == true", enabled_by = "use_checks",
                  parse = function(value, label) parse_n_checks(value)),
        ctl_text_list("rep_checks", show_if = "input.use_checks == true", enabled_by = "use_checks",
                      parse = function(value, label, values) {
                        parse_control_rep_checks(value, label, values$checks)
                      }),
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
        ctl_reps(3), ctl_locations(), ctl_planter(), ctl_plot_start(), ctl_location_names(),
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
                                  rectangular_lattice, "rectangular_lattice", "rect", 30)
  )
}

#' The page spec of one classic design
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
