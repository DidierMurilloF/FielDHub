#' Layout selector values without drawing a field map
#'
#' Empty options produce integer(), never the phantom sequence 1:0.
#' The application translates input conditions through validate_design().
#' @noRd
layout_choices <- function(x, planter = "serpentine", stacked = "vertical", location = 1) {
  if (!inherits(x, "FielDHub")) {
    fieldhub_abort("'x' must be a design created by FielDHub.")
  }
  if (!is.numeric(location) || length(location) != 1L || !is.finite(location) ||
      location < 1 || location %% 1 != 0) {
    fieldhub_abort("'location' must be a positive whole number.")
  }
  check_layout_arguments(planter, stacked)
  options <- layout_options(with_design_class(x), planter = planter, stacked = stacked)
  if (location > length(options)) {
    fieldhub_abort("Location ", location, " is not available for this design.",
                   data = list(options = seq_along(options)))
  }
  seq_along(options[[location]])
}

#' The layout options of a design, each named by the shape of its field
#'
#' @description The values of \code{layout_choices()}, named by the rows and
#' columns of each option ("4 \u00d7 15") and ordered from the most square
#' field on. Options of one shape (the same field with its blocks in another
#' direction) are numbered: "18 \u00d7 6 (1)", "18 \u00d7 6 (2)".
#' @inheritParams layout_choices
#' @return A named integer vector, for a select.
#' @noRd
layout_choice_labels <- function(x, planter = "serpentine", stacked = "vertical", location = 1) {
  choices <- layout_choices(x, planter = planter, stacked = stacked, location = location)
  if (!length(choices)) return(choices)
  options <- layout_options(with_design_class(x), planter = planter, stacked = stacked)[[location]]
  extent <- function(v) max(as.numeric(as.character(v)))
  sizes <- vapply(options, function(book) {
    if (!is.data.frame(book) || !all(c("ROW", "COLUMN") %in% names(book))) return(c(NA_real_, NA_real_))
    c(extent(book$ROW), extent(book$COLUMN))
  }, numeric(2))
  if (anyNA(sizes)) return(choices)
  shapes <- paste(sizes[1L, ], "\u00d7", sizes[2L, ])
  # options of one shape are told apart by a number, in the order of the core
  alike <- shapes %in% shapes[duplicated(shapes)]
  shapes[alike] <- paste0(shapes[alike], " (", stats::ave(choices[alike], shapes[alike], FUN = seq_along), ")")
  # The most square field first, the wider of two alike before the taller:
  # the order of the field dimensions of the spatial pages
  # (squarest_first()). The first is the one a page opens on.
  stats::setNames(choices, shapes)[order(abs(sizes[1L, ] - sizes[2L, ]), sizes[1L, ], choices)]
}
