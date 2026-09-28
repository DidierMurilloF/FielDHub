#' Randomized cyclic Latin rectangle
#'
#' @description Arrange treatments in fewer complete rows than a Latin square,
#' with no treatment repeated in any column. Each row is a complete replicate.
#' @param t Number of treatments (at least two), or distinct character labels.
#' @param rows Number of complete rows, from two through the treatment count.
#' @param l Number of independently randomized locations.
#' @param plotNumber First plot number: one positive whole number, or one per
#'   location. Plot numbers restart at the supplied value for each location.
#' @param planter Plot-number traversal, \code{"serpentine"} or \code{"cartesian"}.
#' @param seed Randomization seed; \code{NULL} draws and records one automatic seed.
#' @param locationNames Distinct nonblank location labels, one per location;
#'   \code{NULL} generates \code{LOC1}, \code{LOC2}, and so on. Labels are preserved.
#' @details The construction takes the first \code{rows} consecutive shifts of
#'   the cyclic Latin square, then independently permutes rows, columns and
#'   treatment labels at each location. It samples this cyclic construction
#'   family, not uniformly from all Latin rectangles, and does not optimize
#'   efficiency. It uses \code{O(l * rows * t)} construction work with no search.
#'
#'   Rows are complete blocks; columns are incomplete when \code{rows < t}.
#'   This is not necessarily a balanced incomplete-block or Youden design.
#'   Consecutive cyclic shifts keep the additive row/column/treatment model
#'   connected. Its residual degrees of freedom at one location are
#'   \code{(rows - 2) * (t - 1)}: two-row rectangles are saturated and cannot
#'   independently estimate residual variation under that model. Interactions
#'   are not separately estimable. Choose replication and analysis with the
#'   intended scientific use in mind.
#'
#'   Field coordinates and plot numbering are fixed at construction. The
#'   \code{planter} and \code{stacked} arguments to \code{field_layout()} do not
#'   rearrange a saved rectangle. Supplied seeds preserve the caller's RNG;
#'   an automatic seed consumes exactly one recorded draw.
#' @return A schema-1 \code{FielDHub} result with \code{infoDesign}, a field book
#'   containing ID, LOCATION, PLOT, ROW, COLUMN, REP and TREATMENT, and replay
#'   metadata. REP is the complete row block. Use \code{reproduce_design()} for
#'   replay and \code{field_layout()} for the recorded coordinates.
#' @references Peter G. Doyle, \emph{The number of Latin rectangles}.
#'   \url{https://math.dartmouth.edu/~doyle/docs/latin/latin.pdf}.
#' @examples
#' x <- latin_rectangle(t = 5, rows = 3, seed = 27)
#' x$fieldBook
#' identical(reproduce_design(x), x)
#' @export
latin_rectangle <- function(t, rows = 3, l = 1, plotNumber = 101,
                            planter = "serpentine", seed = NULL, locationNames = NULL) {
  if (is.numeric(t)) {
    validate_iteration_budget(t, "t", minimum = 2)
    count <- t
  } else if (is.character(t) && is.null(dim(t)) && length(t) >= 2L) {
    validate_entry_labels(t, "t")
    if (anyDuplicated(t)) fieldhub_abort("`t` must contain distinct treatment labels.")
    count <- length(t)
  } else {
    fieldhub_abort("`t` must be a treatment count or at least two distinct character labels.")
  }
  validate_iteration_budget(rows, "rows", minimum = 2)
  if (rows > count) fieldhub_abort("`rows` cannot exceed the treatment count.")
  validate_locations(l)
  validate_design_size(c(count, rows, l))
  check_layout_arguments(planter, "vertical")
  validate_plot_starts(plotNumber)
  if (!length(plotNumber) %in% c(1L, l) || any(plotNumber < 1) ||
      any(plotNumber + count * rows - 1 > .Machine$integer.max)) {
    fieldhub_abort("`plotNumber` must give one positive start, or one per location, with the final plot in range.")
  }
  if (length(plotNumber) == 1L) plotNumber <- rep(plotNumber, l)
  if (is.null(locationNames)) locationNames <- paste0("LOC", seq_len(l))
  if (length(locationNames) != l) fieldhub_abort("`locationNames` must contain one label per location.")
  validate_location_labels(locationNames, l)
  labels <- if (is.numeric(t)) paste0("T", seq_len(count)) else t
  seed <- resolve_seed(seed)
  local_design_seed(seed)
  parameters <- list(t = t, rows = rows, l = l, plotNumber = plotNumber,
    planter = planter, seed = seed, locationNames = locationNames)
  path <- planting_path(rows, count, planter)
  cyclic <- outer(seq_len(rows) - 1L, seq_len(count) - 1L, "+") %% count + 1L
  size <- count * rows
  books <- lapply(seq_len(l), function(site) {
    positions <- cyclic[sample.int(rows), sample.int(count), drop = FALSE]
    treatments <- labels[sample.int(count)]
    data.frame(ID = as.integer(seq_len(size) + (site - 1L) * size),
      LOCATION = rep(locationNames[site], size), PLOT = plotNumber[site] + seq_len(size) - 1,
      ROW = path[, "ROW"], COLUMN = path[, "COLUMN"], REP = path[, "ROW"],
      TREATMENT = treatments[positions[path]], stringsAsFactors = FALSE)
  })
  new_fieldhub_design(list(
    infoDesign = list(nTrt = count, nRows = rows, l = l, planter = planter,
                      seed = seed, id_design = "latin_rectangle"),
    fieldBook = do.call(rbind, books)), "latin_rectangle", parameters)
}
