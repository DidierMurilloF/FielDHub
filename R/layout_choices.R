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
#' @param include_grids Append the field grids (\code{nrows} by \code{ncols}
#'   of \code{field_layout()}) whose replicates lie as \code{stacked} says and
#'   whose shape no numbered option of that stacking has, as a group "Field
#'   grid" with labels such as "Grid 8 \u00d7 3". A design without replicates
#'   lists, under every stacking, the grids whose shape no numbered option has.
#' @return Named integer values for a select. With grids, a list for a select
#'   with an option group: the numbered options, then "Field grid" with the
#'   values "grid:<rows>:<columns>".
#' @noRd
layout_choice_labels <- function(x, planter = "serpentine", stacked = "vertical", location = 1,
                                 include_grids = FALSE) {
  x <- with_design_class(x)
  choices <- layout_choices(x, planter = planter, stacked = stacked, location = location)
  # A stacking without numbered options may still have its field grids
  if (!length(choices)) {
    extra <- if (include_grids) additional_layout_choices(x, character(), stacked)
    return(if (length(extra)) list("Field grid" = as.list(extra)) else choices)
  }
  options <- layout_options(x, planter = planter, stacked = stacked)[[location]]
  extent <- function(v) max(as.numeric(as.character(v)))
  sizes <- vapply(options, function(book) {
    if (!is.data.frame(book) || !all(c("ROW", "COLUMN") %in% names(book))) return(c(NA_real_, NA_real_))
    c(extent(book$ROW), extent(book$COLUMN))
  }, numeric(2))
  if (anyNA(sizes)) return(choices)
  shapes <- paste(sizes[1L, ], "\u00d7", sizes[2L, ])
  # A menu belongs to one "Reps layout": its grids are the fields of that
  # stacking that it does not already number. Without replicates a grid does
  # not depend on the stacking: the shapes that no stacking numbers.
  extra <- if (include_grids) {
    numbered <- if (is.null(grid_replicate_unit(x))) numbered_layout_shapes(x, planter, location) else shapes
    additional_layout_choices(x, numbered, stacked)
  }
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
  if (!length(extra)) return(numbered)
  # The grids are a group of their own in the select: the label of a grid is
  # never that of a numbered option.
  c(as.list(numbered), list("Field grid" = as.list(extra)))
}

#' Field shapes of the numbered options of every stacking
#' @noRd
numbered_layout_shapes <- function(x, planter = "serpentine", location = 1) {
  shapes <- lapply(c("vertical", "horizontal", "grid_panel"), function(stacked) {
    options <- tryCatch(layout_options(x, planter = planter, stacked = stacked),
                        error = function(e) NULL)
    if (length(options) < location) return(character())
    vapply(options[[location]], function(book) {
      if (!is.data.frame(book) || !all(c("ROW", "COLUMN") %in% names(book))) return(NA_character_)
      paste(max(as.numeric(as.character(book$ROW))), "\u00d7",
            max(as.numeric(as.character(book$COLUMN))))
    }, character(1))
  })
  unique(stats::na.omit(unlist(shapes)))
}

# The field grids of a stacking (of any, for a design without replicates)
# whose shape is not among `existing_shapes`, the most square first
additional_layout_choices <- function(x, existing_shapes, stacked = "vertical") {
  if (!inherits(x, "fieldhub_rcbd") && is.null(design_grid_spec(x))) return(NULL)
  if (stores_design_grid(x)) return(NULL)
  grids <- field_grid_dimensions(field_grid_candidates(x, stacked))
  if (!nrow(grids)) return(NULL)
  grids <- grids[order(abs(grids[, "rows"] - grids[, "cols"]), grids[, "rows"]), , drop = FALSE]
  shapes <- paste(grids[, "rows"], "\u00d7", grids[, "cols"])
  more <- !shapes %in% existing_shapes
  if (!any(more)) return(NULL)
  values <- sprintf("grid:%.0f:%.0f", grids[, "rows"], grids[, "cols"])
  stats::setNames(values[more], paste("Grid", shapes[more]))
}
