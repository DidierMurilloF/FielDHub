#' Internal catalogue of the app's existing design modules
#'
#' UI order follows the entries below. Server order is explicit because the
#' existing Strip-Plot server is registered after the IBD and Row-Column servers.
#' Designs with a page spec (\code{design_app_spec()}) are pages of the one
#' generic design module (\code{mod_design_ui()}/\code{mod_design_server()});
#' a design without one keeps a module of its own. This registry is
#' internal, not a public extension interface.
#' @noRd
fieldhub_app_registry <- function() {
  classic <- names(fieldhub_classic_workflows())
  generic_pages <- names(fieldhub_design_specs())
  groups <- c("Unreplicated Designs", "Partially Replicated Designs",
              "Lattice Designs", "Other Designs")
  entry <- function(label, module, engine, group, server_order) {
    generic <- module %in% generic_pages
    list(label = label, id = paste0(module, "_ui_1"),
         ui = if (generic) "mod_design_ui" else paste0("mod_", module, "_ui"),
         server = if (generic) "mod_design_server" else paste0("mod_", module, "_server"),
         engine = engine,
         group = groups[[group]], server_order = as.integer(server_order),
         workflow = module,
         workflow_family = if (module %in% classic) "classic" else "spatial",
         spec = if (generic) module)
  }
  list(
    entry("Single Diagonal Arrangement", "Diagonal", "diagonal_arrangement", 1, 1),
    entry("Multiple Diagonal Arrangement", "diagonal_multiple",
          "diagonal_arrangement", 1, 2),
    entry("Optimized Arrangement", "Optim", "optimized_arrangement", 1, 3),
    entry("Augmented RCBD", "RCBD_augmented", "RCBD_augmented", 1, 4),
    entry("New - Sparse Allocation", "sparse_allocation", "sparse_allocation", 1, 5),
    entry("Single and Multi-Location p-rep", "pREPS", "partially_replicated", 2, 6),
    entry("New - Optimized Multi-Location p-rep", "multi_loc_preps",
          "multi_location_prep", 2, 7),
    entry("Square Lattice", "Square_Lattice", "square_lattice", 3, 8),
    entry("Rectangular Lattice", "Rectangular_Lattice", "rectangular_lattice", 3, 9),
    entry("Alpha Lattice: alpha(0,1)", "Alpha_Lattice", "alpha_lattice", 3, 10),
    entry("Completely Randomized Design (CRD)", "CRD", "CRD", 4, 11),
    entry("Randomized Complete Block Designs (RCBD)", "RCBD", "RCBD", 4, 12),
    entry("Latin Square Design (LSD)", "LSD", "latin_square", 4, 13),
    entry("Factorial Designs", "FD", "full_factorial", 4, 14),
    entry("Split-Plot Design", "SPD", "split_plot", 4, 15),
    entry("Split-Split-Plot Design", "SSPD", "split_split_plot", 4, 16),
    entry("Strip-Plot Design", "STRIPD", "strip_plot", 4, 19),
    entry("Incomplete Blocks Design (IBD)", "IBD", "incomplete_blocks", 4, 17),
    entry("Resolvable Row-Column Design (RRCD)", "RowCol", "row_column", 4, 18)
  )
}

#' @noRd
fieldhub_app_title <- function() {
  paste0("FielDHub v", utils::packageVersion("FielDHub"))
}

#' Arguments a registry entry's UI and server functions are called with
#'
#' @description The module id, and for a page of the generic design module
#' its spec.
#' @noRd
app_module_args <- function(entry) {
  c(list(entry$id), if (!is.null(entry$spec)) list(design_app_spec(entry$spec)))
}

#' Build the existing navigation from the shared module catalogue
#' @noRd
fieldhub_design_menus <- function(registry = fieldhub_app_registry()) {
  groups <- vapply(registry, `[[`, character(1), "group")
  lapply(unique(groups), function(group) {
    tabs <- lapply(registry[groups == group], function(entry) {
      ui <- get(entry$ui, mode = "function")
      shiny::tabPanel(entry$label, do.call(ui, app_module_args(entry)),
                      app_reproduction_ui(shiny::NS(entry$id)))
    })
    do.call(shiny::navbarMenu, c(list(title = group), tabs))
  })
}
