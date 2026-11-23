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

# How the incomplete blocks of a layout lie in its field: each along a row,
# along a column, or as a rectangle of plots. NA for a layout without them.
layout_block_direction <- function(book) {
  if (!all(c("IBLOCK", "REP", "ROW", "COLUMN") %in% names(book))) return(NA_character_)
  block <- book[book$REP == book$REP[1L] & book$IBLOCK == book$IBLOCK[1L], , drop = FALSE]
  rows <- length(unique(block$ROW))
  columns <- length(unique(block$COLUMN))
  if (rows == 1L) "blocks in rows" else if (columns == 1L) "blocks in columns" else
    paste("blocks of", rows, "\u00d7", columns)
}

#' The layout options of a design, each named by the shape of its field
#'
#' @description The values of \code{layout_choices()}, named by the rows and
#' columns of each option ("4 \u00d7 15") and ordered from the most square
#' field on. Options of one shape (the same field with its blocks in another
#' direction) say how their blocks lie: "21 \u00d7 7 \u00b7 blocks in rows".
#' @inheritParams layout_choices
#' @param include_grids Append valid field sizes that the numbered options
#'   do not cover.
#' @return Named values for a select. The values are integers for numbered
#'   options and strings when additional grids are available.
#' @noRd
layout_choice_labels <- function(x, planter = "serpentine", stacked = "vertical", location = 1,
                                 include_grids = FALSE) {
  x <- with_design_class(x)
  choices <- layout_choices(x, planter = planter, stacked = stacked, location = location)
  options <- layout_options(x, planter = planter, stacked = stacked)[[location]]
  extent <- function(v) max(as.numeric(as.character(v)))
  sizes <- vapply(options, function(book) {
    if (!is.data.frame(book) || !all(c("ROW", "COLUMN") %in% names(book))) return(c(NA_real_, NA_real_))
    c(extent(book$ROW), extent(book$COLUMN))
  }, numeric(2))
  if (anyNA(sizes)) return(choices)
  shapes <- if (length(choices)) paste(sizes[1L, ], "\u00d7", sizes[2L, ]) else character()
  extra <- if (include_grids) additional_layout_choices(x, shapes)
  # Options of one shape differ in how the incomplete blocks lie in the
  # field: they say so. Where that does not tell them apart, a number does.
  alike <- shapes %in% shapes[duplicated(shapes)]
  if (any(alike)) {
    blocks <- vapply(options[alike], layout_block_direction, character(1))
    named <- shapes[alike]
    named[!is.na(blocks)] <- paste0(named[!is.na(blocks)], " \u00b7 ", blocks[!is.na(blocks)])
    again <- named %in% named[duplicated(named)]
    named[again] <- paste0(named[again], " (", stats::ave(choices[alike][again], named[again], FUN = seq_along), ")")
    shapes[alike] <- named
  }
  # The most square field first, the wider of two alike before the taller:
  # the order of the field dimensions of the spatial pages
  # (squarest_first()). The first is the one a page opens on.
  numbered <- stats::setNames(choices, shapes)[order(abs(sizes[1L, ] - sizes[2L, ]), sizes[1L, ], choices)]
  c(numbered, extra)
}

additional_layout_choices <- function(x, existing_shapes) {
  spec <- design_grid_spec(x)
  rcbd <- inherits(x, "fieldhub_rcbd")
  if (is.null(spec) && !rcbd) return(NULL)
  if (has_design_grid(x) || (rcbd && all(c("ROW", "COLUMN") %in% names(x$fieldBook)))) {
    return(NULL)
  }
  grids <- valid_field_grids(x$fieldBook, spec$axes)
  if (!nrow(grids)) return(NULL)
  grids <- grids[order(abs(grids[, 1L] - grids[, 2L]), grids[, 1L]), , drop = FALSE]
  labels <- paste(grids[, 1L], "\u00d7", grids[, 2L])
  more <- !labels %in% existing_shapes
  values <- sprintf("grid:%.0f:%.0f", grids[, 1L], grids[, 2L])
  stats::setNames(values[more], labels[more])
}
