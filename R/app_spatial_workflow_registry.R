#' Presentation settings for the shared spatial workflow
#'
#' Keep each module's established controls and display conventions. This is an
#' internal application registry, not a public extension interface.
#' @noRd
fieldhub_spatial_workflows <- function() {
  entry <- function(ids, simulation_ids, correlation_ids, correlation_suffix,
                     heatmap_checkbox = NULL,
                     numeric_labels = c("Input the min value:", "Input the max value:"),
                     renumber_display = FALSE, coerce_book = FALSE,
                     table_columns = c("EXPT", "LOCATION", "PLOT", "ROW", "COLUMN", "CHECKS", "ENTRY", "TREATMENT"),
                     table_height = 600, table_collapse = NULL,
                     heatmap_height = 720, heatmap_title = FALSE) {
    list(ids = ids, simulation_ids = simulation_ids, correlation_ids = correlation_ids,
         correlation_suffix = correlation_suffix, heatmap_checkbox = heatmap_checkbox,
         numeric_labels = numeric_labels, renumber_display = renumber_display,
         coerce_book = coerce_book, table_columns = table_columns,
         table_height = table_height, table_collapse = table_collapse,
         heatmap_height = heatmap_height, heatmap_title = heatmap_title)
  }
  list(
    Diagonal = entry(
      ids = c(simulate = "Simulate_Diagonal", heatmap = "heatmap_diag", table = "fieldBook_diagonal",
        download = "downloadData_Diagonal", tabset = "tabset_single"),
      simulation_ids = c(trait = "trailsDIAG", other = "OtherDIAG", minimum = "min.diag", maximum = "max.diag",
        submit = "ok_simu_single"),
      correlation_ids = c(x = "ROX.DIAG", y = "ROY.DIAG"),
      correlation_suffix = ".DIAG"
    ),
    diagonal_multiple = entry(
      ids = c(simulate = "simulate_multiple", heatmap = "heatmap_diag", table = "fieldBook_diagonal",
        download = "download_fieldbook_multiple", tabset = "tabset_multi"),
      simulation_ids = c(trait = "trailsDIAG", other = "OtherDIAG", minimum = "min.diag", maximum = "max.diag",
        submit = "ok_simu_multi"),
      correlation_ids = c(x = "ROX.DIAG", y = "ROY.DIAG"),
      correlation_suffix = ".DIAG",
      heatmap_height = 700
    ),
    Optim = entry(
      ids = c(simulate = "Simulate.optim", heatmap = "heatmap", table = "OPTIMOUTPUT", download = "downloadData.spatial",
        tabset = "tabset_optim"),
      simulation_ids = c(trait = "trailsOPTIM", other = "OtherOPTIM", minimum = "min.optim", maximum = "max.optim",
        submit = "ok.optim"),
      correlation_ids = c(x = "ROX.O", y = "ROY.O"),
      correlation_suffix = ".O",
      heatmap_checkbox = "heatmap_s",
      heatmap_height = 740,
      heatmap_title = TRUE
    ),
    RCBD_augmented = entry(
      ids = c(simulate = "Simulate.arcbd", heatmap = "heatmap", table = "fieldBook_ARCBD", download = "downloadData_a_rcbd",
        tabset = "tabset_arcbd"),
      simulation_ids = c(trait = "trailsARCBD", other = "OtherARCBD", minimum = "min.arcbd", maximum = "max.arcbd",
        submit = "ok.arcbd"),
      correlation_ids = c(x = "ROX.O", y = "ROY.O"),
      correlation_suffix = ".O",
      heatmap_checkbox = "heatmap_s",
      table_columns = c("EXPT", "LOCATION", "PLOT", "ROW", "COLUMN", "CHECKS", "BLOCK", "ENTRY", "TREATMENT"
        ),
      table_collapse = TRUE,
      heatmap_height = 740
    ),
    pREPS = entry(
      ids = c(simulate = "Simulate.prep", heatmap = "heatmap_prep", table = "pREPSOUTPUT", download = "downloadData.preps",
        tabset = "tabset_prep"),
      simulation_ids = c(trait = "trailsPREP", other = "OtherPREP", minimum = "min.prep", maximum = "max.prep",
        submit = "ok.prep"),
      correlation_ids = c(x = "ROX.PREP", y = "ROY.PREP"),
      correlation_suffix = ".PREP",
      heatmap_checkbox = "heatmap_PREP",
      numeric_labels = c("Input the min value", "Input the max value"),
      coerce_book = TRUE,
      table_height = 500,
      heatmap_height = 700
    ),
    multi_loc_preps = entry(
      ids = c(simulate = "simulate_prep_data", heatmap = "heatmap_prep", table = "pREPSOUTPUT",
        download = "downloadData.preps", tabset = "tabset_prep_avg"),
      simulation_ids = c(trait = "trailsPREP", other = "OtherPREP", minimum = "min.prep", maximum = "max.prep",
        submit = "ok.prep"),
      correlation_ids = c(x = "ROX.PREP", y = "ROY.PREP"),
      correlation_suffix = ".PREP",
      heatmap_checkbox = "heatmap_PREP",
      numeric_labels = c("Input the min value", "Input the max value"),
      renumber_display = TRUE,
      table_height = 500,
      heatmap_height = 700
    ),
    sparse_allocation = entry(
      ids = c(simulate = "sparse_simulate", heatmap = "heatmap_diag", table = "fieldBook_diagonal",
        download = "downloadData_Diagonal", tabset = "sparse_tabset_single"),
      simulation_ids = c(trait = "trailsDIAG", other = "OtherDIAG", minimum = "min.diag", maximum = "max.diag",
        submit = "ok_simu_single"),
      correlation_ids = c(x = "ROX.DIAG", y = "ROY.DIAG"),
      correlation_suffix = ".DIAG"
    )
  )
}

#' @noRd
spatial_workflow_spec <- function(module) {
  registry <- fieldhub_spatial_workflows()
  if (!is.character(module) || !is.null(dim(module)) || length(module) != 1L ||
      is.na(module) || !module %in% names(registry)) {
    fieldhub_abort("Choose an available spatial workflow.",
                   data = list(choices = names(registry)))
  }
  registry[[module]]
}
