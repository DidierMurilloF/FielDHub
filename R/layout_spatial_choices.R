#' Choices the spatial design pages offer
#'
#' @description Plain functions behind the selects of the spatial design
#' pages (\code{design_app_spec()}): each takes parsed values and returns
#' \code{list(choices = , selected = )}, or raises a classed
#' \code{fieldhub_input_error} that explains why nothing fits. None of them
#' draws random numbers.
#' @name spatial_choices
#' @noRd
NULL

#' Offer field sizes, the first selected
#' @param labels Field sizes ("rows x columns"), in the order offered.
#' @param none Message when there is none.
#' @return \code{list(choices = , selected = )}.
#' @noRd
field_size_choices <- function(labels, none) {
  if (length(labels) == 0L) fieldhub_abort(none)
  list(choices = labels, selected = utils::head(labels, 1L))
}

#' Field sizes of an optimized arrangement
#' @param plots Number of plots, replicated checks included.
#' @return \code{list(choices = , selected = )}.
#' @noRd
optim_field_choices <- function(plots) {
  field_size_choices(optimized_dimension_choices(plots),
                     "Please try a different number of treatments or checks.")
}

#' Field sizes of a p-rep design, with filler plots where allowed
#'
#' @param plots Number of plots.
#' @param allow_fillers Whether filler plots may complete the field.
#' @return \code{list(choices = , selected = )}; each choice is labelled
#'   with its filler plots.
#' @noRd
prep_field_choices <- function(plots, allow_fillers) {
  if (!is.numeric(plots) || length(plots) != 1L || !is.finite(plots) || plots < 1 ||
      plots != trunc(plots)) {
    fieldhub_abort("The number of plots must be one positive whole number.")
  }
  options <- prep_dimension_options(total_plots = plots, allow_fillers = isTRUE(allow_fillers),
                                    max_fillers = .prep_max_fillers)
  if (is.null(options)) fieldhub_abort(prep_no_dimensions_problem(allow_fillers)$message)
  list(choices = stats::setNames(options$value, options$label),
       selected = utils::head(options$value, 1L))
}

#' Numbers of blocks of an augmented RCBD
#'
#' @param lines Number of entries, checks excluded.
#' @param checks Number of checks.
#' @return \code{list(choices = , selected = )}; "No Options Available"
#'   (\code{no_block_size_option()}) when no block count fits, which Run!
#'   then reports.
#' @noRd
augmented_block_choices <- function(lines, checks) {
  check_augmented_counts(lines, checks)
  blocks <- unique(set_augmented_blocks(lines = lines, checks = checks, start = 3)$b)
  if (length(blocks) == 0L) {
    return(list(choices = no_block_size_option(), selected = no_block_size_option()))
  }
  list(choices = blocks, selected = utils::head(blocks, 1L))
}

#' Field sizes of an augmented RCBD with a number of blocks
#' @inheritParams augmented_block_choices
#' @param b Number of blocks.
#' @return \code{list(choices = , selected = )}.
#' @noRd
augmented_field_choices <- function(lines, checks, b) {
  check_augmented_counts(lines, checks)
  if (!is.numeric(b) || length(b) != 1L || !is.finite(b) || b < 1) {
    fieldhub_abort("The number of blocks must be one positive whole number.")
  }
  options <- set_augmented_blocks(lines = lines, checks = checks, start = 3)$blocks_dims
  sizes <- if (!is.null(options)) {
    colnames(options) <- c("blocks", "size")
    as.vector(options[options[, "blocks"] == b, "size"])
  }
  field_size_choices(sizes, paste0("No field size holds ", b, " blocks of these entries."))
}

#' Check the counts an augmented RCBD's choices are computed from
#' @inheritParams augmented_block_choices
#' @return \code{NULL}, or a classed input error.
#' @noRd
check_augmented_counts <- function(lines, checks) {
  whole <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 1 && x == trunc(x)
  if (!whole(lines) || !whole(checks)) {
    fieldhub_abort("The numbers of entries and checks must be positive whole numbers.")
  }
  invisible(NULL)
}

#' Field sizes of a diagonal arrangement
#'
#' @param field_entries The count candidate fields are searched for.
#' @param lines Entries of one location, checks excluded.
#' @param checks ENTRY numbers of the checks.
#' @param kindExpt \code{"SUDC"} or \code{"DBUDC"}.
#' @param stacked Blocks layout of a multiple arrangement.
#' @param planter Plot order.
#' @param data Entry list with checks first (a BLOCK column for
#'   \code{"DBUDC"}), or \code{NULL}.
#' @return \code{list(choices = , selected = )}.
#' @noRd
diagonal_field_choices <- function(field_entries, lines, checks, kindExpt = "SUDC",
                                   stacked = "By Row", planter = "serpentine", data = NULL) {
  if (length(field_dimensions(lines_within_loc = field_entries)) == 0L) {
    fieldhub_abort("Insufficient number of entries provided!")
  }
  field_size_choices(
    diagonal_dimension_choices(lines = lines, checks = checks, kindExpt = kindExpt,
                               stacked = stacked, planter = planter, data = data),
    "No feasible field was found for these entries and checks."
  )
}

#' Percentages of checks a diagonal field offers
#'
#' @param nrows,ncols The field size.
#' @param checks ENTRY numbers of the checks.
#' @param entries All entries, checks included.
#' @param kindExpt,stacked,planter,data As for \code{diagonal_field_choices()}.
#' @param blocks Number of experiments of a multiple arrangement, or
#'   \code{NULL}.
#' @return \code{list(choices = , selected = , table = )}: the percentages,
#'   the last selected (the API default), and the reference table the page
#'   shows.
#' @noRd
diagonal_percent_choices <- function(nrows, ncols, checks, entries, kindExpt = "SUDC",
                                     stacked = "By Row", planter = "serpentine", data = NULL,
                                     blocks = NULL) {
  whole <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 1 && x == trunc(x)
  if (!whole(nrows) || !whole(ncols) || !whole(entries)) {
    fieldhub_abort("The field size and the number of entries must be positive whole numbers.")
  }
  options <- diagonal_check_options(
    n_rows = nrows, n_cols = ncols, checks = checks, Option_NCD = TRUE, kindExpt = kindExpt,
    stacked = stacked, planter_mov1 = planter, data = data, dim_data = entries,
    dim_data_1 = entries - length(checks), Block_Fillers = blocks
  )
  if (is.null(options$dt)) fieldhub_abort("Data input does not fit to field dimensions.")
  percents <- as.numeric(options$dt[["Percentage of Checks"]])
  list(choices = percents, selected = utils::tail(percents, 1L), table = options$dt)
}
