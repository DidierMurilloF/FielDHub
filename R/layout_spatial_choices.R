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
