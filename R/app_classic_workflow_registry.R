#' Presentation settings for the shared classic workflow
#'
#' Keep established input/output identifiers and design-specific table columns.
#' This is an internal application registry, not a public extension interface.
#' @noRd
fieldhub_classic_workflows <- function() {
  layout_entry <- function(output, ids, widths = c(2, 3, 3, 3), container = "split",
                            grid = FALSE, select_layout = FALSE, select_location = FALSE,
                            current_planter = FALSE) {
    list(output = output, ids = ids, widths = widths, container = container,
         grid = grid, select_layout = select_layout, select_location = select_location,
         current_planter = current_planter)
  }
  entry <- function(ids, simulation_ids, export_prefixes, table_columns, layout,
                     label_column, label_title = "Treatment", order_by_id = FALSE,
                     include_site = TRUE, include_checks = FALSE,
                     book_component = "allSitesFieldbook", table_extensions = FALSE,
                     table_height = 500, table_renderer = "renderDataTable",
                     export_label = NULL) {
    list(ids = ids, simulation_ids = simulation_ids, layout = layout, book_component = book_component,
         order_by_id = order_by_id,
         heatmap = list(label_column = label_column, label_title = label_title,
                        include_site = include_site, include_checks = include_checks),
         table_columns = table_columns, table_extensions = table_extensions,
         table_height = table_height, table_renderer = table_renderer,
         export_prefixes = export_prefixes, export_label = export_label)
  }
  list(
    CRD = entry(
      layout = layout_entry("well_panel_layout_CRD", c(layout = "layoutO_crd"),
        widths = c(3, 3), container = "row", current_planter = TRUE),
      ids = c(plot_type = "typlotCRD", simulate = "Simulate.crd", plot = "layout_random", table = "CRD_fieldbook",
        field_book_download = "downloadData.crd", layout_download = "downloadCsv.crd"),
      simulation_ids = c(trait = "trailsCRD", other = "OtherCRD", minimum = "min.crd", maximum = "max.crd",
        submit = "ok.crd"),
      book_component = "fieldBookXY",
      order_by_id = TRUE,
      table_columns = c("LOCATION", "PLOT", "ROW", "COLUMN", "REP", "TREATMENT"),
      table_renderer = "renderDT",
      export_prefixes = c(field_book = "CRD_", layout = "Completely_Randomized_Layout"),
      label_column = "TREATMENT",
      label_title = "Entry",
      include_site = FALSE
    ),
    RCBD = entry(
      layout = layout_entry("well_panel_layout_RCBD",
        c(layout = "layoutO_rcbd", stacked = "stackedRCBD", location = "locLayout_rcbd"),
        widths = c(3, 3, 2, 2), select_layout = TRUE),
      ids = c(plot_type = "typlotRCBD", simulate = "Simulate.rcbd", plot = "layouts", table = "RCBD_fieldbook",
        field_book_download = "downloadData.rcbd", layout_download = "downloadCsv.rcbd"),
      simulation_ids = c(trait = "trailsRCBD", other = "OtherRCBD", minimum = "min.rcbd", maximum = "max.rcbd",
        submit = "ok.rcbd"),
      order_by_id = TRUE,
      table_columns = c("LOCATION", "PLOT", "ROW", "COLUMN", "REP", "TREATMENT"),
      export_prefixes = c(field_book = "RCBD_", layout = "Randomized_Complete_Block_Layout"),
      export_label = "TREATMENT",
      label_column = "TREATMENT",
      include_checks = TRUE
    ),
    LSD = entry(
      layout = layout_entry("well_panel_layout_LSD", c(layout = "layoutO_lsd", stacked = "stackedLSD"),
        widths = c(3, 3, 3)),
      ids = c(plot_type = "typlotLSD", simulate = "Simulate.lsd", plot = "layout_lsd", table = "LSD_fieldbook",
        field_book_download = "downloadData.lsd", layout_download = "downloadCsv.lsd"),
      simulation_ids = c(trait = "trailsLSD", other = "OtherLSD", minimum = "min.lsd", maximum = "max.lsd",
        submit = "ok.lsd"),
      order_by_id = TRUE,
      table_columns = c("LOCATION", "PLOT", "ROW", "COLUMN", "SQUARE", "TREATMENT"),
      export_prefixes = c(field_book = "Latin_Square_", layout = "Latin_Square_Layout"),
      label_column = "TREATMENT"
    ),
    Latin_Rectangle = entry(
      layout = layout_entry("layout_controls", c(layout = "layout", location = "location"),
        widths = c(4, 4, 4), container = "row", select_layout = TRUE, select_location = TRUE),
      ids = c(plot_type = "plot_type", simulate = "simulate", plot = "layout_plot", table = "field_book",
        field_book_download = "download_book", layout_download = "download_layout"),
      simulation_ids = c(trait = "trait", other = "other_trait", minimum = "minimum", maximum = "maximum",
        submit = "simulate_submit"),
      table_columns = c("LOCATION", "PLOT", "ROW", "COLUMN", "REP", "TREATMENT"),
      export_prefixes = c(field_book = "Latin_Rectangle_", layout = "Latin_Rectangle_Layout"),
      export_label = "TREATMENT", label_column = "TREATMENT"
    ),
    FD = entry(
      layout = layout_entry("well_panel_layout_FD",
        c(layout = "layoutO_fd", stacked = "stackedFD", location = "locLayout_fd"), select_location = TRUE),
      ids = c(plot_type = "typlotfd", simulate = "Simulate.fd", plot = "layouts", table = "FD.Output",
        field_book_download = "downloadData.fd", layout_download = "downloadCsv.fd"),
      simulation_ids = c(trait = "trailsfd", other = "Otherfd", minimum = "min.fd", maximum = "max.fd",
        submit = "ok.fd"),
      table_columns = c("LOCATION", "PLOT", "ROW", "COLUMN"),
      table_extensions = TRUE,
      export_prefixes = c(field_book = "Full_Factorial_", layout = "Factorial_Layout"),
      label_column = "TRT_COMB"
    ),
    SPD = entry(
      layout = layout_entry("well_panel_layout_SPD",
        c(layout = "layoutO_spd", stacked = "stackedSPD", location = "locLayout_spd"), widths = c(2, 3, 2, 2)),
      ids = c(plot_type = "typlotspd", simulate = "Simulate.spd", plot = "layouts", table = "SPD.output",
        field_book_download = "downloadData.spd", layout_download = "downloadCsv.spd"),
      simulation_ids = c(trait = "trailsspd", other = "Otherspd", minimum = "min.spd", maximum = "max.spd",
        submit = "ok.spd"),
      order_by_id = TRUE,
      table_columns = c("LOCATION", "PLOT", "ROW", "COLUMN", "REP", "WHOLE_PLOT", "SUB_PLOT", "TRT_COMB"
        ),
      export_prefixes = c(field_book = "Split-Plot_", layout = "Split_Plot_Layout"),
      label_column = "TRT_COMB"
    ),
    SSPD = entry(
      layout = layout_entry("well_panel_layout_SSPD",
        c(layout = "layoutO_sspd", stacked = "stackedSSPD", location = "locLayout_sspd")),
      ids = c(plot_type = "typlotsspd", simulate = "Simulate.sspd", plot = "layouts", table = "SSPD.output",
        field_book_download = "downloadData.sspd", layout_download = "downloadCsv.sspd"),
      simulation_ids = c(trait = "TrialsRowCol", other = "Otherspd", minimum = "min.sspd", maximum = "max.sspd",
        submit = "ok.sspd"),
      order_by_id = TRUE,
      table_columns = c("LOCATION", "PLOT", "ROW", "COLUMN", "REP", "WHOLE_PLOT", "SUB_PLOT", "SUB_SUB_PLOT",
        "TRT_COMB"),
      export_prefixes = c(field_book = "Split-Split-Plot_", layout = "Split_Split_Plot_Layout"),
      label_column = "TRT_COMB",
      label_title = "TRT_COMB"
    ),
    STRIPD = entry(
      layout = layout_entry("well_panel_layout_STRIP",
        c(layout = "layoutO_strip", stacked = "stackedSTRIP", location = "locLayout_strip"), container = "row"),
      ids = c(plot_type = "typlotstrip", simulate = "Simulate.strip", plot = "layout.strip",
        table = "STRIP.output", field_book_download = "downloadData.strip", layout_download = "downloadCsv.strip"
        ),
      simulation_ids = c(trait = "trailsStrip", other = "OtherStrip", minimum = "min.strip", maximum = "max.strip",
        submit = "ok.strip"),
      table_columns = c("LOCATION", "PLOT", "ROW", "COLUMN", "REP", "HSTRIP", "VSTRIP", "TRT_COMB"),
      export_prefixes = c(field_book = "Strip-Plot_", layout = "Strip_Plot_Layout"),
      label_column = "TRT_COMB"
    ),
    IBD = entry(
      layout = layout_entry("well_panel_layout_IBD",
        c(layout = "layoutO_ibd", stacked = "stackedibd", location = "locLayout_ibd")),
      ids = c(plot_type = "typlotibd", simulate = "Simulate.ibd", plot = "layouts", table = "IBD.output",
        field_book_download = "downloadData.ibd", layout_download = "downloadCsv.ibd"),
      simulation_ids = c(trait = "trailsIBD", other = "OtherIBD", minimum = "min.ibd", maximum = "max.ibd",
        submit = "ok.ibd"),
      table_columns = c("LOCATION", "PLOT", "ROW", "COLUMN", "REP", "IBLOCK", "UNIT", "ENTRY", "TREATMENT"
        ),
      export_prefixes = c(field_book = "IBD_", layout = "Incomplete_Block_Layout"),
      label_column = "ENTRY",
      label_title = "Entry"
    ),
    RowCol = entry(
      layout = layout_entry("well_panel_layout_ROWCOL",
        c(layout = "layoutO_rcd", stacked = "stackedRowCol", location = "locLayout_rcd")),
      ids = c(plot_type = "typlotrcd", simulate = "Simulate.RowCol", plot = "layouts", table = "rowcolD",
        field_book_download = "downloadData.rowcolD", layout_download = "downloadCsv.rcd"
        ),
      simulation_ids = c(trait = "trailsRowCol", other = "OtherRowCol", minimum = "min.RowCol", maximum = "max.RowCol",
        submit = "ok.RowCol"),
      table_columns = c("LOCATION", "PLOT", "ROW", "COLUMN", "REP", "ENTRY"),
      table_height = 490,
      export_prefixes = c(field_book = "Row-Column_", layout = "Resolvable_Row-Column_Layout"),
      label_column = "ENTRY",
      label_title = "Entry"
    ),
    Alpha_Lattice = entry(
      layout = layout_entry("well_panel_layout",
        c(layout = "layoutO", stacked = "stackedAlpha", location = "locLayout"), widths = c(3, 3, 2, 2),
        container = "page", grid = TRUE, select_layout = TRUE),
      ids = c(plot_type = "typlotALPHA", simulate = "Simulate.alpha", plot = "random_layout",
        table = "ALPHA_fieldbook", field_book_download = "downloadData.alpha", layout_download = "downloadCsv.alpha"
        ),
      simulation_ids = c(trait = "trailsALPHA", other = "OtherALPHA", minimum = "min.alpha", maximum = "max.alpha",
        submit = "ok.alpha"),
      table_columns = c("LOCATION", "PLOT", "ROW", "COLUMN", "REP", "IBLOCK", "UNIT", "ENTRY"),
      export_prefixes = c(field_book = "Alpha_Lattice_", layout = "Alpha_Lattice_Layout"),
      label_column = "ENTRY",
      label_title = "Entry"
    ),
    Square_Lattice = entry(
      layout = layout_entry("well_panel_layout_sq",
        c(layout = "layoutO_sq", stacked = "stacked_sq", location = "locLayout_sq"),
        widths = c(3, 3, 2, 2), grid = TRUE, select_layout = TRUE),
      ids = c(plot_type = "typlotSQ", simulate = "Simulate.square", plot = "random_layout", table = "square_fieldbook",
        field_book_download = "downloadData.square", layout_download = "downloadCsv.square"
        ),
      simulation_ids = c(trait = "trailsSQUARE", other = "OtherSQUARE", minimum = "min.square", maximum = "max.square",
        submit = "ok.square"),
      table_columns = c("LOCATION", "PLOT", "ROW", "COLUMN", "REP", "IBLOCK", "UNIT", "ENTRY", "TREATMENT"
        ),
      export_prefixes = c(field_book = "Square_Lattice_", layout = "Square_Lattice_Layout"),
      label_column = "ENTRY",
      label_title = "Entry"
    ),
    Rectangular_Lattice = entry(
      layout = layout_entry("well_panel_layout_rt",
        c(layout = "layoutO_rt", stacked = "stackedRT", location = "locLayout_rt"),
        widths = c(3, 3, 2, 2), grid = TRUE, select_layout = TRUE),
      ids = c(plot_type = "typlotRT", simulate = "Simulate.rectangular", plot = "random_layout",
        table = "rectangular_fieldbook", field_book_download = "downloadData.rectangular",
        layout_download = "downloadCsv.rectangular"),
      simulation_ids = c(trait = "trailsRECT", other = "OtherRECT", minimum = "min.rectangular", maximum = "max.rectangular",
        submit = "ok.rectangular"),
      table_columns = c("LOCATION", "PLOT", "ROW", "COLUMN", "REP", "IBLOCK", "UNIT", "ENTRY"),
      export_prefixes = c(field_book = "Rectangular_Lattice_", layout = "Rectangular_Lattice_Layout"),
      label_column = "ENTRY",
      label_title = "Entry"
    )
  )
}

#' Resolve an internal classic workflow without evaluating module code
#' @noRd
classic_workflow_spec <- function(module) {
  registry <- fieldhub_classic_workflows()
  if (!is.character(module) || !is.null(dim(module)) || length(module) != 1L ||
      is.na(module) || !module %in% names(registry)) {
    fieldhub_abort("Choose an available classic workflow.",
                   data = list(choices = names(registry)))
  }
  registry[[module]]
}
