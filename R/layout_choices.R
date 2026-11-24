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

# How the units of a layout (its incomplete blocks, its whole plots) lie in
# its field: each along a row, each along a column, or each as a rectangle of
# plots of one shape. Every unit of every replicate is read: units that are
# not equal rectangles wrap along the planting path, and the layout says so.
# NA for a layout without that unit.
layout_unit_direction <- function(book, unit, what) {
  if (!all(c(unit, "ROW", "COLUMN") %in% names(book))) return(NA_character_)
  replicate <- intersect(c("REP", "SQUARE"), names(book))
  key <- as.character(book[[unit]])
  if (length(replicate)) key <- paste(book[[replicate[1L]]], key)
  extent <- function(v) diff(range(as.numeric(as.character(v)))) + 1
  shapes <- unique(vapply(split(book[c("ROW", "COLUMN")], key), function(one) {
    rows <- extent(one$ROW)
    columns <- extent(one$COLUMN)
    if (rows * columns != nrow(one)) "along the path" else if (rows == 1) "in rows" else
      if (columns == 1) "in columns" else paste(rows, "\u00d7", columns)
  }, character(1)))
  paste(what, if (length(shapes) == 1L) shapes else "along the path")
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

# How the incomplete blocks and the whole plots of a layout lie
# (layout_unit_direction()), NA for a unit that the layout does not have
layout_units <- c(IBLOCK = "blocks", WHOLE_PLOT = "whole plots")

layout_unit_directions <- function(book) {
  vapply(names(layout_units), function(unit) {
    layout_unit_direction(book, unit, layout_units[[unit]])
  }, character(1))
}

# The numbered options of a stacking: the value, the rows and columns of the
# field, how the replicates of a panel lie (`lie`) and the field book
# (`layout`) of each. The same layout under two numbers is listed once. With
# `replicates`, the menu is that of the grid panel: every option says how its
# replicates lie, and one whose replicates are not in rows and columns of
# replicates is left out. NULL when an option has no coordinates.
numbered_layout_menu <- function(x, planter, stacked, location, replicates = NULL) {
  none <- data.frame(value = integer(), rows = numeric(), cols = numeric(), lie = character(),
                     stringsAsFactors = FALSE)
  none$layout <- list()
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
  lie <- rep(NA_character_, length(keep))
  if (!is.null(replicates)) {
    lie <- vapply(layouts[keep], layout_replicates_direction, character(1), unit = replicates)
    keep <- keep[!is.na(lie)]
    lie <- lie[!is.na(lie)]
  }
  layouts <- layouts[keep]
  menu <- data.frame(value = keep,
                     rows = vapply(layouts, function(book) max(book$ROW), numeric(1)),
                     cols = vapply(layouts, function(book) max(book$COLUMN), numeric(1)),
                     lie = lie, stringsAsFactors = FALSE)
  menu$layout <- layouts
  menu
}

# The field grids (`nrows` by `ncols` of field_layout()) of a stacking: one
# for each size, that of field_grid_plan(). `numbered` holds the numbered
# options of the stacking (numbered_layout_menu()). A grid of the size of
# one of them is listed only when its incomplete blocks or its whole plots
# lie as in none of that size: it is then another field, and
# name_layout_menu() tells the two apart. The same field, or one that
# differs only in the path of the plots inside the replicate, is left out;
# so is every grid of a numbered size when `apart` is FALSE.
# The value of a grid carries its stacking,
# "grid:<stacking>:<rows>:<columns>" ("none" for a design without
# replicates).
field_grid_menu <- function(x, stacked, planter, location, numbered, apart = TRUE) {
  none <- numbered[0L, , drop = FALSE]
  none$value <- character()
  if (!inherits(x, "fieldhub_rcbd") && is.null(design_grid_spec(x))) return(none)
  if (stores_design_grid(x)) return(none)
  fields <- field_grid_candidates(x, stacked)
  if (!nrow(fields)) return(none)
  # Of the panels of one size, the one with the most square replicate, then
  # with the most rows of replicates: the field that field_layout() builds
  fields <- fields[order(abs(fields$height - fields$width), -fields$down), , drop = FALSE]
  fields <- fields[!duplicated(fields[c("rows", "cols")]), , drop = FALSE]
  sites <- unique(x$fieldBook$LOCATION)
  layouts <- lapply(seq_len(nrow(fields)), function(i) {
    alike <- numbered$layout[numbered$rows == fields$rows[i] & numbered$cols == fields$cols[i]]
    if (!length(alike)) return(NULL)
    if (!apart) return(NA)
    book <- design_field_grid(x, fields$rows[i], fields$cols[i], planter,
                              if (!is.na(fields$stacked[i])) fields$stacked[i])
    book <- book[book$LOCATION == sites[location], , drop = FALSE]
    lies <- layout_unit_directions(book)
    same <- vapply(alike, function(other) identical(layout_unit_directions(other), lies),
                   logical(1))
    if (any(same)) NA else book
  })
  listed <- !vapply(layouts, function(book) identical(book, NA), logical(1))
  fields <- fields[listed, , drop = FALSE]
  fields$stacked[is.na(fields$stacked)] <- "none"
  menu <- data.frame(value = sprintf("grid:%s:%.0f:%.0f", fields$stacked, fields$rows, fields$cols),
                     rows = fields$rows, cols = fields$cols,
                     lie = replicates_direction(fields$down, fields$across),
                     stringsAsFactors = FALSE)
  menu$layout <- layouts[listed]
  menu
}

# The names of the options of a menu: the size of the field, then how the
# replicates of a panel lie. Options that this does not tell apart, and a
# field grid with the numbered options of its size, say how their incomplete
# blocks or their whole plots lie: the first of the two that lies in an
# option as in no other of its size.
name_layout_menu <- function(menu) {
  size <- paste(menu$rows, "\u00d7", menu$cols)
  label <- ifelse(is.na(menu$lie), size, paste0(size, " \u00b7 ", menu$lie))
  group <- label
  grid <- grepl("^grid:", menu$value) & !vapply(menu$layout, is.null, logical(1))
  group[size %in% size[grid]] <- size[size %in% size[grid]]
  for (i in which(group %in% group[duplicated(group)])) {
    alike <- setdiff(which(group == group[i]), i)
    lies <- layout_unit_directions(menu$layout[[i]])
    others <- lapply(menu$layout[alike], layout_unit_directions)
    for (unit in names(layout_units)) {
      here <- lies[[unit]]
      elsewhere <- vapply(others, function(other) other[[unit]], character(1))
      if (!is.na(here) && !here %in% elsewhere) {
        label[i] <- paste0(label[i], " \u00b7 ", here)
        break
      }
    }
  }
  label
}

#' The layout options of a design, each named by the shape of its field
#'
#' @description The values of \code{layout_choices()}, named by the rows and
#' columns of each option ("4 \u00d7 15") and ordered from the most square
#' field on, the one with fewer rows first of two alike. Options of one size
#' say how their incomplete blocks or whole plots lie ("21 \u00d7 7 \u00b7
#' blocks in rows"); the same layout under two numbers is listed once. Every
#' option of a grid panel says how its replicates lie ("6 \u00d7 12 \u00b7
#' replicates 2 \u00d7 2"), and the grid panel holds only fields with rows
#' and columns of replicates, more than one of each.
#' @inheritParams layout_choices
#' @param include_grids The menu of the application: the field grids
#'   (\code{nrows} by \code{ncols} of \code{field_layout()}) are listed
#'   after the numbered options, so that a menu opens on a numbered layout.
#'   A menu lists the grids of its own stacking alone: those with the
#'   replicates one above another under the vertical stacking, side by side
#'   under the horizontal one, in rows and columns of replicates under the
#'   grid panel. A grid of the size of a numbered option of its stacking is
#'   listed when its incomplete blocks or its whole plots lie another way,
#'   and both then say how ("8 \u00d7 8 \u00b7 replicates 2 \u00d7 2
#'   \u00b7 blocks in rows"); the same field, or one that differs only in the
#'   path of its plots, is left out. A design without replicates to arrange
#'   (\code{layout_arranges_replicates()}) has one list of fields, with one
#'   field for each size.
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
  # The most square field first, the one with fewer rows of two alike
  sorted <- function(menu) menu[order(abs(menu$rows - menu$cols), menu$rows), , drop = FALSE]
  menu <- sorted(menu)
  if (include_grids) {
    # The grids of the stacking of the menu, and of no other; a design
    # without replicates to arrange has one list of fields, with no stacking
    # to tell its grids apart, and one field for each size: its whole plots
    # are not rectangles of a replicate to name
    arranged <- layout_arranges_replicates(x)
    only <- if (arranged) stacked else if (is.null(grid_replicate_unit(x))) "none" else "vertical"
    grids <- sorted(field_grid_menu(x, only, planter, location, menu, apart = arranged))
    # The numbered options first: a menu opens on the layout it always did
    menu$value <- as.character(menu$value)
    menu <- rbind(menu, grids)
  }
  if (nrow(menu)) stats::setNames(menu$value, name_layout_menu(menu)) else menu$value
}
