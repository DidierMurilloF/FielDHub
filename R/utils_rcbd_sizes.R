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
  n_test <- read_whole_numbers(t, "Number of test entries")
  blocks <- read_whole_numbers(reps, "Replicates")
  if (length(n_test) != 1L || length(blocks) != 1L) {
    fieldhub_abort("Number of test entries and replicates must each be one whole number.")
  }
  count <- parse_n_checks(n_checks)
  if (!count$ok) fieldhub_abort(count$message)
  # Every check needs at least one plot. Apply the cap before recycling a
  # scalar replication count, even when the requested check count is huge.
  rcbd_block_size(n_test, count$value)
  parsed <- parse_rep_checks(rep_checks, count$value)
  if (!parsed$ok) fieldhub_abort(parsed$message)
  n_units <- rcbd_block_size(n_test, parsed$value)
  total <- n_units * blocks
  if (!is.finite(total)) fieldhub_abort("The total number of plots is too large.")
  sprintf("Block size: %.0f plots. Total: %.0f plots.", n_units, total)
}
