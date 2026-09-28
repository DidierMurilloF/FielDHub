#' Restore the two process options changed by blocksdesign optimizers
#'
#' Keep their settings throughout design construction, including post-processing,
#' then restore only these options on success or error. Unrelated options and
#' dependency initialization are deliberately left alone.
#' @noRd
local_optimizer_options <- function(frame = parent.frame()) {
  previous <- options("warn", "contrasts")
  restore <- function() options(previous)
  do.call(base::on.exit, list(as.call(list(restore)), add = TRUE), envir = frame)
  invisible(previous)
}
