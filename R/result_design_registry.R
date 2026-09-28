#' Core declarations for replay, result schema and optional generic presentation
#'
#' Built-in declarations are reviewed with the engines. Saved metadata only
#' selects a key here; it never supplies executable handlers. Existing S3
#' methods retain their historical layouts and presentation. New engines can
#' declare a fixed-coordinate layout and use the generic methods instead.
#' @noRd
fieldhub_design_registry <- function() {
  entry <- function(engine, columns = character(), title = NULL, layout = NULL,
                    render = NULL) {
    list(engine = engine, columns = columns, title = title, layout = layout, render = render)
  }
  classic <- c("REP", "TREATMENT")
  incomplete <- c("REP", "IBLOCK", "UNIT", "ENTRY", "TREATMENT")
  spatial <- c("EXPT", "YEAR", "ROW", "COLUMN", "CHECKS", "ENTRY", "TREATMENT")
  list(
    crd = entry("CRD", classic),
    rcbd = entry("RCBD", function(x) {
      c(classic, if (is.list(x$infoDesign) && !is.null(x$infoDesign$checks)) c("ENTRY", "CHECKS"))
    }),
    latin_square = entry("latin_square", c("SQUARE", "ROW", "COLUMN", "TREATMENT")),
    full_factorial = entry("full_factorial", function(x) {
      info <- if (is.list(x$infoDesign)) x$infoDesign else list()
      c("REP", "TRT_COMB", paste0("FACTOR_", info$factors))
    }),
    split_plot = entry("split_plot", c("REP", "WHOLE_PLOT", "SUB_PLOT", "TRT_COMB")),
    split_split_plot = entry("split_split_plot", c("REP", "WHOLE_PLOT", "SUB_PLOT", "SUB_SUB_PLOT", "TRT_COMB")),
    strip_plot = entry("strip_plot", c("REP", "HSTRIP", "VSTRIP", "TRT_COMB")),
    incomplete_blocks = entry("incomplete_blocks", incomplete),
    row_column = entry("row_column", c("REP", "ROW", "COLUMN", "ENTRY", "TREATMENT")),
    square_lattice = entry("square_lattice", incomplete),
    rectangular_lattice = entry("rectangular_lattice", incomplete),
    alpha_lattice = entry("alpha_lattice", incomplete),
    partially_replicated = entry("partially_replicated", c(spatial, "REP")),
    rcbd_augmented = entry("RCBD_augmented", c(spatial, "BLOCK")),
    diagonal_arrangement = entry("diagonal_arrangement", spatial),
    optimized_arrangement = entry("optimized_arrangement", spatial),
    split_families = entry("split_families"),
    sparse_allocation = entry("sparse_allocation", spatial),
    multi_location_prep = entry("multi_location_prep", c(spatial, "REP")),
    allocation_sparse = entry("do_optim"), allocation_prep = entry("do_optim"),
    pair_swap = entry("swap_pairs")
  )
}

#' Resolve only a known declaration, without accepting handlers from a result
#' @noRd
registered_design_entry <- function(x) {
  if (!is.list(x) || !is.list(x$metadata)) return(NULL)
  design <- x$metadata$design
  if (!is.character(design) || length(design) != 1L || is.na(design)) return(NULL)
  fieldhub_design_registry()[[design]]
}
