#' Count and validate plots in an RCBD block with repeated checks
#'
#' Shared by the entry resolver and the app preview. Does not allocate the
#' entries or a field book, so oversized inputs can be rejected cheaply.
#' @param n_test Number of test entries, each occurring once per block.
#' @param rep_checks Replications of each check within a block.
#' @return The number of plots per block.
#' @noRd
rcbd_block_size <- function(n_test, rep_checks) {
  if (!is.numeric(n_test) || length(n_test) != 1L || !is.finite(n_test) ||
      n_test < 1 || n_test != trunc(n_test)) {
    fieldhub_abort("RCBD() requires a positive whole number of test entries.")
  }
  if (!is.numeric(rep_checks) || length(rep_checks) == 0L ||
      any(!is.finite(rep_checks)) || any(rep_checks < 1) || any(rep_checks != trunc(rep_checks))) {
    fieldhub_abort("RCBD() requires positive whole-number replications for every check.")
  }
  n_units <- as.numeric(n_test) + sum(rep_checks)
  if (n_units > 10000) {
    fieldhub_abort("RCBD() would build a block of ",
                  format(n_units, big.mark = ",", scientific = FALSE),
                  " plots, which is not a plausible field block. Reduce 'rep_checks' or ",
                  "the number of entries (the limit is 10,000 plots per block).")
  }
  n_units
}

#' Describe the size of a manually entered RCBD with repeated checks
#'
#' @param t Number of test entries (excluding checks).
#' @param reps Number of blocks.
#' @param n_checks Number of checks.
#' @param rep_checks Raw comma-separated replications per check.
#' @return A description, or a classed input condition.
#' @noRd
rcbd_size_preview <- function(t, reps, n_checks, rep_checks) {
  n_test <- parse_whole_numbers(t, "Number of test entries")
  blocks <- parse_whole_numbers(reps, "Replicates")
  if (length(n_test) != 1L || length(blocks) != 1L) {
    fieldhub_abort("Number of test entries and replicates must each be one whole number.")
  }
  count <- parse_n_checks(n_checks)
  # Every check needs at least one plot. Apply the cap before recycling a
  # scalar replication count, even when the requested check count is huge.
  rcbd_block_size(n_test, count)
  n_units <- rcbd_block_size(n_test, parse_rep_checks(rep_checks, count))
  total <- n_units * blocks
  if (!is.finite(total)) fieldhub_abort("The total number of plots is too large.")
  sprintf("Block size: %.0f plots. Total: %.0f plots.", n_units, total)
}

#' The block-size note shown under the RCBD repeated-checks controls
#'
#' @description Nothing until repeated checks are enabled and the counts it
#' needs have been typed; on the upload path the block size depends on the
#' file, so a note says so instead of predicting it.
#'
#' @param values Named list of raw control values: \code{use_checks},
#'   \code{t}, \code{reps}, \code{checks}, \code{rep_checks}.
#' @param uploaded Whether the entries come from an uploaded file.
#' @return A description, \code{NULL}, or a classed input condition.
#' @noRd
rcbd_checks_note <- function(values, uploaded) {
  if (!isTRUE(values[["use_checks"]])) return(NULL)
  if (isTRUE(uploaded)) {
    return("Block size depends on the uploaded list: its first rows are taken as the checks.")
  }
  needed <- values[c("t", "reps", "checks", "rep_checks")]
  if (any(vapply(needed, is_blank_control_value, logical(1)))) return(NULL)
  rcbd_size_preview(values[["t"]], values[["reps"]], values[["checks"]], values[["rep_checks"]])
}
