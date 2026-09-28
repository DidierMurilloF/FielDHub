#' Validate the number of experimental units before allocating a CRD
#' @noRd
validate_crd_size <- function(treatments, replications) {
  validate_iteration_budget(treatments, "t")
  if (!is.numeric(replications) || !is.null(dim(replications)) ||
      length(replications) == 0L || any(!is.finite(replications)) ||
      any(replications < 1 | replications != trunc(replications))) {
    fieldhub_abort("CRD() requires positive whole-number replication counts.")
  }
  size <- as.double(treatments) * sum(replications)
  if (!is.finite(size) || size > .Machine$integer.max) {
    fieldhub_abort("CRD() requires at most ", .Machine$integer.max, " experimental units.")
  }
  invisible(size)
}

#' Validate CRD labels while retaining their original storage and ordering
#' @noRd
validate_crd_labels <- function(labels) {
  if (!is.atomic(labels) || is.complex(labels) || !is.null(dim(labels)) ||
      length(labels) == 0L || anyNA(labels) ||
      (is.numeric(labels) && any(!is.finite(labels))) ||
      ((is.character(labels) || is.factor(labels)) && any(!nzchar(trimws(as.character(labels)))))) {
    fieldhub_abort("CRD() requires nonmissing, nonblank treatment labels.")
  }
  check_unique_labels(labels, "CRD")
  invisible(labels)
}

#' Validate the single location represented by a CRD field book
#' @noRd
validate_crd_location <- function(location) {
  if (!is.atomic(location) || is.complex(location) || !is.null(dim(location)) ||
      length(location) != 1L || anyNA(location) ||
      (is.numeric(location) && !is.finite(location)) ||
      !nzchar(trimws(as.character(location)))) {
    fieldhub_abort("CRD() requires one nonmissing, nonblank locationNames value.")
  }
  invisible(location)
}
