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

# How the replicates lie in a field: `down` rows of `across` replicates. NA
# for a single replicate, which has nothing to say.
replicates_direction <- function(down, across) {
  ifelse(down == 1 & across == 1, NA_character_,
         ifelse(across == 1, "replicates stacked vertically",
                ifelse(down == 1, "replicates stacked horizontally",
                       paste("replicates", down, "\u00d7", across))))
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

# The numbered options of a stacking: the value, the rows and columns of the
# field and the name of each. The same layout under two numbers is listed
# once. Options of one size say what tells them apart: how their incomplete
# blocks or their whole plots lie. With `replicates`, every option says how
# its replicates lie (the "Grid panel" menu). NULL when an option has no
# coordinates.
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
  layouts <- layouts[keep]
  rows <- vapply(layouts, function(book) max(book$ROW), numeric(1))
  cols <- vapply(layouts, function(book) max(book$COLUMN), numeric(1))
  size <- paste(rows, "\u00d7", cols)
  qualifier <- rep(NA_character_, length(layouts))
  if (!is.null(replicates)) {
    qualifier <- vapply(layouts, function(book) {
      if (!replicates %in% names(book)) return(NA_character_)
      one <- book[book[[replicates]] == book[[replicates]][1L], , drop = FALSE]
      height <- diff(range(one$ROW)) + 1
      width <- diff(range(one$COLUMN)) + 1
      down <- max(book$ROW) / height
      across <- length(unique(book[[replicates]])) / down
      if (nrow(one) != height * width || down != trunc(down) || across != trunc(across)) {
        return(NA_character_)
      }
      replicates_direction(down, across)
    }, character(1))
  }
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

# The field grids (`nrows` by `ncols` of field_layout()) of the stackings
# asked for: one for each size and stacking, that of field_grid_plan(), named
# by its size and by how its replicates lie. `numbered` holds, by stacking,
# the sizes that the numbered options already have; they are left out. The
# value of a grid carries its stacking, "grid:<stacking>:<rows>:<columns>"
# ("none" for a design without replicates).
field_grid_menu <- function(x, stackings, numbered = list()) {
  none <- data.frame(value = character(), rows = numeric(), cols = numeric(), label = character(),
                     stringsAsFactors = FALSE)
  if (!inherits(x, "fieldhub_rcbd") && is.null(design_grid_spec(x))) return(none)
  if (stores_design_grid(x)) return(none)
  fields <- field_grid_candidates(x, stackings)
  if (!nrow(fields)) return(none)
  fields$stacked[is.na(fields$stacked)] <- "none"
  fields$size <- paste(fields$rows, "\u00d7", fields$cols)
  taken <- vapply(seq_len(nrow(fields)), function(i) {
    fields$size[i] %in% numbered[[fields$stacked[i]]]
  }, logical(1))
  fields <- fields[!taken, , drop = FALSE]
  # Of the panels of one size, the one with the most square replicate, then
  # with the most rows of replicates: the field that field_layout() builds
  fields <- fields[order(abs(fields$height - fields$width), -fields$down), , drop = FALSE]
  fields <- fields[!duplicated(fields[c("stacked", "size")]), , drop = FALSE]
  lie <- replicates_direction(fields$down, fields$across)
  fields$label <- ifelse(is.na(lie), fields$size, paste0(fields$size, " \u00b7 ", lie))
  fields <- fields[order(match(fields$stacked, c(stacking_choices, "none"))), , drop = FALSE]
  # A single replicate has the same fields under every stacking
  fields <- fields[!duplicated(fields$label), , drop = FALSE]
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
#' 2 \u00d7 2").
#' @inheritParams layout_choices
#' @param include_grids The menu of the application: the field grids
#'   (\code{nrows} by \code{ncols} of \code{field_layout()}) are listed too.
#'   A design with replicates to arrange lists them all under the grid panel,
#'   whatever their stacking, each with how its replicates lie; the vertical
#'   and horizontal menus hold the numbered options alone. A design without
#'   replicates to arrange (\code{layout_arranges_replicates()}) lists its
#'   grids after its numbered options, by size alone. A grid of the size of a
#'   numbered option of its stacking is left out.
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
    grids <- field_grid_menu(x, only, stats::setNames(list(sizes(menu)), only))
    menu <- rbind(sorted(menu), sorted(grids))
  } else if (panel) {
    numbered <- lapply(stats::setNames(nm = stacking_choices), function(stacking) {
      if (identical(stacking, stacked)) return(sizes(menu))
      sizes(numbered_layout_menu(x, planter, stacking, location))
    })
    menu <- sorted(rbind(menu, field_grid_menu(x, stacking_choices, numbered)))
  } else {
    menu <- sorted(menu)
  }
  named(menu)
}
