#' Identify treatment and randomization units in a FielDHub field book
#' @noRd
simulation_design_terms <- function(x) {
  book <- x$fieldBook
  design <- sub("^fieldhub_", "", class(with_design_class(x))[1L])
  site <- "LOCATION"
  replicate <- c(site, "REP")
  factorial <- switch(design,
    full_factorial = paste0("FACTOR_", x$infoDesign$factors),
    split_plot = c("WHOLE_PLOT", "SUB_PLOT"),
    split_split_plot = c("WHOLE_PLOT", "SUB_PLOT", "SUB_SUB_PLOT"),
    strip_plot = c("HSTRIP", "VSTRIP")
  )
  treatment <- if (is.null(factorial)) {
    list(treatment = "TREATMENT")
  } else {
    main <- stats::setNames(as.list(factorial), paste0("treatment_", factorial))
    if (length(factorial) > 1L) c(main, list(treatment_interaction = factorial)) else main
  }

  parameters <- x$metadata$parameters
  blocked <- if (design == "full_factorial") {
    identical(x$infoDesign$kind, "RCBD") || isTRUE(parameters$type == 2)
  } else {
    identical(x$infoDesign$typeDesign, "RCBD") || isTRUE(x$infoDesign$typeDesign == 2) ||
      isTRUE(parameters$type == 2)
  }
  square_axes <- if (all(c("ROW_SQ", "COLUMN_SQ") %in% names(book))) {
    c("ROW_SQ", "COLUMN_SQ")
  } else c("ROW", "COLUMN")
  row_column_axes <- if (all(c("ROW_REP", "COLUMN_REP") %in% names(book))) {
    c("ROW_REP", "COLUMN_REP")
  } else c("ROW", "COLUMN")

  if (design == "row_column") {
    # The efficiency report counts the realized blocks separately at each
    # location, including e.g. a location that fell back from latinization.
    locations <- x$infoDesign$location_names
    shared <- vapply(x$blocksModel, function(model) {
      isTRUE(model$Blocks[match("Row", model$Level)] == x$infoDesign$rows)
    }, logical(1))
    use_shared <- as.character(book$LOCATION) %in% as.character(locations[which(shared)])
    book$.axis_rep <- ifelse(use_shared, 0L, book$REP)
  }

  units <- switch(design,
    crd = list(),
    rcbd = list(block = replicate),
    rcbd_augmented = list(block = c(site, "BLOCK")),
    augmented_row_column = list(row = c(site, "ROW"), column = c(site, "COLUMN")),
    diagonal_arrangement = if (anyDuplicated(unique(book[c(site, "EXPT")])[[site]])) {
      list(decision_block = c(site, "EXPT"))
    } else list(),
    optimized_arrangement = , sparse_allocation = ,
    partially_replicated = , multi_location_prep = list(),
    square_lattice = , rectangular_lattice = , alpha_lattice = ,
    incomplete_blocks = list(replicate = replicate, incomplete_block = c(replicate, "IBLOCK")),
    latin_square = list(square = c(site, "SQUARE"),
      row = c(site, "SQUARE", square_axes[1L]), column = c(site, "SQUARE", square_axes[2L])),
    latin_rectangle = list(row = c(site, "ROW"), column = c(site, "COLUMN")),
    full_factorial = if (blocked) list(block = replicate) else list(),
    split_plot = list(whole_plot = c(replicate, "WHOLE_PLOT")),
    split_split_plot = list(whole_plot = c(replicate, "WHOLE_PLOT"),
      subplot = c(replicate, "WHOLE_PLOT", "SUB_PLOT")),
    strip_plot = list(replicate = replicate, horizontal_strip = c(replicate, "HSTRIP"),
      vertical_strip = c(replicate, "VSTRIP")),
    row_column = list(replicate = replicate,
      row = c(site, ".axis_rep", row_column_axes[1L]),
      column = c(site, ".axis_rep", row_column_axes[2L])),
    fieldhub_abort("Simulation needs a FielDHub design with experimental plots.")
  )
  if (blocked && design %in% c("split_plot", "split_split_plot")) {
    units <- c(list(block = replicate), units)
  }

  # ENTRY 0 denotes an unplanted filler in spatial field books.
  planted <- if (all(c("EXPT", "ENTRY") %in% names(book))) book$ENTRY != 0 else rep(TRUE, nrow(book))
  list(
    treatment = lapply(treatment, function(columns) simulation_group(book, columns, planted)),
    design = lapply(units, function(columns) simulation_group(book, columns, planted, site)),
    planted = planted
  )
}

#' Group exact keys without concatenating or coercing user labels
#' @noRd
simulation_group <- function(book, columns, planted, within = NULL) {
  if (!all(columns %in% names(book))) {
    fieldhub_abort("The field book is missing simulation columns: ",
                   paste(setdiff(columns, names(book)), collapse = ", "), ".")
  }
  keys <- as.data.frame(book[planted, columns, drop = FALSE])
  if (anyNA(keys)) fieldhub_abort("Treatment and design unit identifiers must be present on planted plots.")
  out <- integer(nrow(book))
  if (!nrow(keys)) return(out)
  distinct <- unique(keys)
  # A single unit at a site has no within-site contrast.
  active <- if (is.null(within)) rep(TRUE, nrow(keys)) else {
    multiple <- distinct[[within]][duplicated(distinct[[within]])]
    keys[[within]] %in% multiple
  }
  if (any(active)) {
    grouped <- dplyr::group_by(keys[active, , drop = FALSE], dplyr::across(dplyr::everything()))
    out[which(planted)[active]] <- dplyr::group_indices(grouped)
  }
  out
}
