#' Version-aware arguments for restoring recorded random-number settings
#'
#' R 4.7 adds binom.kind. Three-setting records were made with the older
#' binomial algorithm; do not silently substitute the new default on replay.
#' Four-setting records cannot be faithfully replayed on older R versions.
#' @noRd
recorded_rng_arguments <- function(kind, supported = names(formals(RNGkind))) {
  if (!is.character(kind) || !length(kind) %in% c(3L, 4L) ||
      !is.null(dim(kind)) || anyNA(kind) || any(!nzchar(trimws(kind)))) {
    fieldhub_abort("Incomplete recorded random-number settings.", call. = FALSE)
  }
  args <- as.list(unname(kind))
  names(args) <- c("kind", "normal.kind", "sample.kind", "binom.kind")[seq_along(args)]
  if (length(kind) == 4L && !"binom.kind" %in% supported) {
    fieldhub_abort("The recorded binomial RNG requires R with binom.kind support (R >= 4.7).",
                   call. = FALSE)
  }
  if (length(kind) == 3L && "binom.kind" %in% supported) {
    args$binom.kind <- "Buggy BTPE"
  }
  args
}

#' Restore recorded RNG settings without dropping version-specific settings
#' @noRd
restore_recorded_rng <- function(kind) {
  do.call(RNGkind, recorded_rng_arguments(kind))
}
