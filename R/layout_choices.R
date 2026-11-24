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

# How a unit of a layout (an incomplete block, a whole plot) lies in its
# field: along a row, along a column, or as a rectangle of plots. The unit is
# the first of the first replicate. NA for a layout without that unit.
layout_unit_direction <- function(book, unit, what) {
  if (!all(c(unit, "ROW", "COLUMN") %in% names(book))) return(NA_character_)
  replicate <- intersect(c("REP", "SQUARE"), names(book))
  if (length(replicate)) {
    book <- book[book[[replicate[1L]]] == book[[replicate[1L]]][1L], , drop = FALSE]
  }
  book <- book[book[[unit]] == book[[unit]][1L], , drop = FALSE]
  rows <- length(unique(book$ROW))
  columns <- length(unique(book$COLUMN))
  if (rows == 1L) paste(what, "in rows") else if (columns == 1L) paste(what, "in columns") else
    paste(what, rows, "\u00d7", columns)
}

# How the replicates of a panel lie in a field: `down` rows of `across`
# replicates, more than one of each. NA for a field that is not a panel: a
# stack of replicates is named by its size alone.
replicates_direction <- function(down, across) {
  ifelse(down > 1 & across > 1, paste("replicates", down, "\u00d7", across), NA_character_)
}

# The replicate (or the square) that a stacking moves, when the design has
# more than one to arrange in its field. NULL for a design without
# replicates, with a single one, or built on a field of given dimensions.
arranged_replicate_unit <- function(x) {
  unit <- grid_replicate_unit(x)
  if (is.null(unit) || stores_design_grid(x)) return(NULL)
  book <- x$fieldBook
  site <- book[book$LOCATION == book$LOCATION[1L], , drop = FALSE]
  if (length(unique(site[[unit]])) > 1L) unit
}

#' Whether a design has replicates to arrange in its field
#'
#' @description A design without replicates (a CRD, or a factorial or split
#' plot in a CRD), with a single one, or built on a field of given dimensions
#' has one list of fields and no "Replicate layout". The designs whose fields
#' are not field grids (a row-column design) keep the stackings of their
#' numbered layouts.
#' @noRd
layout_arranges_replicates <- function(x) {
  x <- with_design_class(x)
  if (stores_design_grid(x)) return(FALSE)
  if (!inherits(x, "fieldhub_rcbd") && is.null(design_grid_spec(x))) return(TRUE)
  !is.null(arranged_replicate_unit(x))
}

# How the replicates of a layout lie, as replicates_direction() says it. NA
# when they are not rectangles in rows and columns of replicates, more than
# one of each.
layout_replicates_direction <- function(book, unit) {
  if (!unit %in% names(book)) return(NA_character_)
  one <- book[book[[unit]] == book[[unit]][1L], , drop = FALSE]
  height <- diff(range(one$ROW)) + 1
  width <- diff(range(one$COLUMN)) + 1
  down <- max(book$ROW) / height
  across <- length(unique(book[[unit]])) / down
  if (nrow(one) != height * width || down != trunc(down) || across != trunc(across)) {
    return(NA_character_)
  }
  replicates_direction(down, across)
}

# The numbered options of a stacking: the value, the rows and columns of the
# field and the name of each. The same layout under two numbers is listed
# once. Options of one size say what tells them apart: how their incomplete
# blocks or their whole plots lie. With `replicates`, the menu is that of the
# grid panel: every option says how its replicates lie, and one whose
# replicates are not in rows and columns of replicates is left out. NULL when
# an option has no coordinates.
numbered_layout_menu <- function(x, planter, stacked, location, replicates = NULL) {
  none <- data.frame(value = integer(), rows = numeric(), cols = numeric(), label = character(),
                     stringsAsFactors = FALSE)
  options <- tryCatch(layout_options(x, planter = planter, stacked = stacked),
                      fieldhub_input_error = function(e) list())
  if (length(options) < location || !length(options[[location]])) return(none)
  options <- options[[location]]
  number <- function(v) as.numeric(as.character(v))
  placed <- vapply(options, function(book) {
    is.data.frame(book) && all(c("ROW", "COLUMN") %in% names(book))
  }, logical(1))
  if (!all(placed)) return(NULL)
  layouts <- lapply(options, function(book) {
    book$ROW <- number(book$ROW)
    book$COLUMN <- number(book$COLUMN)
    book <- book[order(book$ROW, book$COLUMN), , drop = FALSE]
    rownames(book) <- NULL
    book
  })
  keep <- which(!duplicated(layouts))
  if (!is.null(replicates)) {
    lie <- vapply(layouts[keep], layout_replicates_direction, character(1), unit = replicates)
    keep <- keep[!is.na(lie)]
  }
  layouts <- layouts[keep]
  rows <- vapply(layouts, function(book) max(book$ROW), numeric(1))
  cols <- vapply(layouts, function(book) max(book$COLUMN), numeric(1))
  size <- paste(rows, "\u00d7", cols)
  qualifier <- if (is.null(replicates)) rep(NA_character_, length(layouts)) else lie[!is.na(lie)]
  label <- ifelse(is.na(qualifier), size, paste0(size, " \u00b7 ", qualifier))
  units <- c(IBLOCK = "blocks", WHOLE_PLOT = "whole plots")
  for (i in which(label %in% label[duplicated(label)])) {
    alike <- setdiff(which(label == label[i]), i)
    # The first unit that lies in this option as in no other of its name
    for (unit in names(units)) {
      lies <- vapply(layouts[c(i, alike)], layout_unit_direction, character(1),
                     unit = unit, what = units[[unit]])
      if (!is.na(lies[1L]) && !lies[1L] %in% lies[-1L]) {
        qualifier[i] <- if (is.na(qualifier[i])) lies[1L] else paste0(qualifier[i], ", ", lies[1L])
        break
      }
    }
  }
  data.frame(value = keep, rows = rows, cols = cols,
             label = ifelse(is.na(qualifier), size, paste0(size, " \u00b7 ", qualifier)),
             stringsAsFactors = FALSE)
}

# The field grids (`nrows` by `ncols` of field_layout()) of a stacking: one
# for each size, that of field_grid_plan(). A grid is named by its size, as a
# numbered option is; a panel says how its replicates lie too. `numbered`
# holds the sizes that the numbered options of the stacking already have;
# they are left out. The value of a grid carries its stacking,
# "grid:<stacking>:<rows>:<columns>" ("none" for a design without
# replicates).
field_grid_menu <- function(x, stacked, numbered = character()) {
  none <- data.frame(value = character(), rows = numeric(), cols = numeric(), label = character(),
                     stringsAsFactors = FALSE)
  if (!inherits(x, "fieldhub_rcbd") && is.null(design_grid_spec(x))) return(none)
  if (stores_design_grid(x)) return(none)
  fields <- field_grid_candidates(x, stacked)
  if (!nrow(fields)) return(none)
  fields$stacked[is.na(fields$stacked)] <- "none"
  fields$size <- paste(fields$rows, "\u00d7", fields$cols)
  fields <- fields[!fields$size %in% numbered, , drop = FALSE]
  # Of the panels of one size, the one with the most square replicate, then
  # with the most rows of replicates: the field that field_layout() builds
  fields <- fields[order(abs(fields$height - fields$width), -fields$down), , drop = FALSE]
  fields <- fields[!duplicated(fields$size), , drop = FALSE]
  lie <- replicates_direction(fields$down, fields$across)
  fields$label <- ifelse(is.na(lie), fields$size, paste0(fields$size, " \u00b7 ", lie))
  data.frame(value = sprintf("grid:%s:%.0f:%.0f", fields$stacked, fields$rows, fields$cols),
             rows = fields$rows, cols = fields$cols, label = fields$label,
             stringsAsFactors = FALSE)
}

#' The layout options of a design, each named by the shape of its field
#'
#' @description The values of \code{layout_choices()}, named by the rows and
#' columns of each option ("4 \u00d7 15") and ordered from the most square
#' field on, the one with fewer rows first of two alike. A name has at most
#' one qualifier after " \u00b7 ". Options of one size say how their
#' incomplete blocks or whole plots lie ("21 \u00d7 7 \u00b7 blocks in
#' rows"); the same layout under two numbers is listed once. Every option of
#' a grid panel says how its replicates lie ("6 \u00d7 12 \u00b7 replicates
#' 2 \u00d7 2"), and the grid panel holds only fields with rows and columns of
#' replicates, more than one of each.
#' @inheritParams layout_choices
#' @param include_grids The menu of the application: the field grids
#'   (\code{nrows} by \code{ncols} of \code{field_layout()}) are listed too.
#'   A menu lists the grids of its own stacking alone, with its numbered
#'   options: those with the replicates one above another under the vertical
#'   stacking, side by side under the horizontal one, in rows and columns of
#'   replicates under the grid panel. A design without replicates to arrange
#'   (\code{layout_arranges_replicates()}) lists its grids after its numbered
#'   options. A grid of the size of a numbered option of its stacking is left
#'   out.
#' @return Named integer values. With \code{include_grids}, named character
#'   values for a select: the numbers, and "grid:<stacking>:<rows>:<columns>"
#'   for a field grid.
#' @noRd
layout_choice_labels <- function(x, planter = "serpentine", stacked = "vertical", location = 1,
                                 include_grids = FALSE) {
  x <- with_design_class(x)
  choices <- layout_choices(x, planter = planter, stacked = stacked, location = location)
  stacked <- unname(stacked)
  unit <- arranged_replicate_unit(x)
  panel <- identical(stacked, "grid_panel") && !is.null(unit)
  menu <- numbered_layout_menu(x, planter, stacked, location, replicates = if (panel) unit)
  if (is.null(menu)) return(choices)
  named <- function(menu) if (nrow(menu)) stats::setNames(menu$value, menu$label) else menu$value
  # The most square field first, the one with fewer rows of two alike
  sorted <- function(menu) menu[order(abs(menu$rows - menu$cols), menu$rows), , drop = FALSE]
  if (!include_grids) {
    return(named(sorted(menu)))
  }
  sizes <- function(menu) paste(menu$rows, "\u00d7", menu$cols)
  menu$value <- as.character(menu$value)
  if (!layout_arranges_replicates(x)) {
    # One list of fields, with no stacking to tell the grids apart: the
    # numbered options, the first of which a page opens on, then the grids
    only <- if (is.null(grid_replicate_unit(x))) "none" else "vertical"
    menu <- rbind(sorted(menu), sorted(field_grid_menu(x, only, sizes(menu))))
  } else {
    # The grids of the stacking of the menu, and of no other
    menu <- sorted(rbind(menu, field_grid_menu(x, stacked, sizes(menu))))
  }
  named(menu)
}
