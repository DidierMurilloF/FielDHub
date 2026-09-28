#' Build a default entry list
#'
#' @param treatments Number of entries to build.
#' @param prefix Label prefix.
#' @param start First entry identifier and label suffix.
#' @return A data frame with integer `ENTRY` and character `NAME` columns.
#' @noRd
default_entries <- function(treatments, prefix = "G-", start = 1L) {
  invalid_treatments <- !is.numeric(treatments) || length(treatments) != 1L ||
    is.na(treatments) || !is.finite(treatments) || treatments %% 1 != 0 ||
    treatments < 1 || treatments > .Machine$integer.max
  if (invalid_treatments) {
    fieldhub_abort(
      "`treatments` must be one positive whole number.",
      call = sys.call()
    )
  }

  invalid_prefix <- !is.character(prefix) || length(prefix) != 1L ||
    is.na(prefix)
  if (invalid_prefix) {
    fieldhub_abort("`prefix` must be one character value.", call = sys.call())
  }

  invalid_start <- !is.numeric(start) || length(start) != 1L ||
    is.na(start) || !is.finite(start) || start %% 1 != 0 || start < 1 ||
    start > .Machine$integer.max
  if (invalid_start || treatments > .Machine$integer.max - start + 1) {
    fieldhub_abort(
      "`start` must define positive whole-number entry identifiers.",
      call = sys.call()
    )
  }

  entry <- seq.int(
    from = as.integer(start),
    length.out = as.integer(treatments)
  )
  data.frame(
    ENTRY = entry,
    NAME = paste0(prefix, entry),
    stringsAsFactors = FALSE
  )
}
