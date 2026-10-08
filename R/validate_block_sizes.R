#' Find feasible incomplete-block sizes
#'
#' @param treatments Number of treatments.
#' @param design Design family whose block-size rule should be applied.
#' @return A sorted integer vector of feasible block sizes.
#' @noRd
valid_block_sizes <- function(treatments, design) {
  invalid_treatments <- !is.numeric(treatments) || length(treatments) != 1L ||
    is.na(treatments) || !is.finite(treatments) || treatments %% 1 != 0 ||
    treatments <= 1 || treatments > .Machine$integer.max
  if (invalid_treatments) {
    fieldhub_abort(
      "`treatments` must be one whole number greater than one.",
      call = sys.call()
    )
  }

  designs <- c(
    "incomplete_blocks", "row_column", "alpha_lattice",
    "square_lattice", "rectangular_lattice"
  )
  invalid_design <- !is.character(design) || length(design) != 1L ||
    is.na(design) || !design %in% designs
  if (invalid_design) {
    fieldhub_abort(
      "`design` must be one of: ",
      paste(designs, collapse = ", "),
      ".",
      call = sys.call()
    )
  }

  treatments <- as.integer(treatments)
  if (design %in% c("incomplete_blocks", "row_column", "alpha_lattice")) {
    options <- integer_divisors(treatments)
    return(options[options > 1L & options < treatments])
  }

  if (design == "square_lattice") {
    block_size <- as.integer(floor(sqrt(treatments)))
    if (block_size * block_size == treatments) return(block_size)
    return(integer())
  }

  block_size <- as.integer(floor((sqrt(1 + 4 * treatments) - 1) / 2))
  if (block_size * (block_size + 1) == treatments) block_size else integer()
}

#' The option a block-size select offers when no size fits the entries
#' @noRd
no_block_size_option <- function() "No Options Available"

#' Block-size (or row-count) choices offered for a number of entries
#'
#' @description The choices of the "# of Plots per IBlock" and "# of Rows"
#' selects: every feasible size (\code{valid_block_sizes()}), or
#' "No Options Available" when none fits. The middle size is selected when
#' there are more than two, the first otherwise.
#'
#' @param treatments Number of entries.
#' @param design Design family, as for \code{valid_block_sizes()}.
#' @return \code{list(choices = , selected = )}.
#' @noRd
block_size_choices <- function(treatments, design) {
  options <- valid_block_sizes(treatments, design)
  if (length(options) == 0L) {
    return(list(choices = no_block_size_option(), selected = no_block_size_option()))
  }
  selected <- if (length(options) > 2L) options[ceiling(length(options) / 2)] else options[1L]
  list(choices = options, selected = selected)
}
