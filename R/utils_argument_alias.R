#' Resolve a renamed argument without changing positional calls
#'
#' @param value Value supplied using the new name.
#' @param legacy_value Value supplied using the old name.
#' @param new New argument name.
#' @param old Deprecated argument name.
#' @param new_supplied Whether the caller supplied the new argument.
#' @param old_supplied Whether the caller supplied the old argument.
#' @param call Call to report in conditions.
#' @return The value supplied, or the new argument's default.
#' @noRd
resolve_argument_alias <- function(value, legacy_value, new, old,
                                   new_supplied, old_supplied,
                                   call = sys.call(-1)) {
  if (new_supplied && old_supplied) {
    fieldhub_abort("Supply only `", new, "` or `", old, "`, not both.", call = call)
  }
  if (!old_supplied) return(value)
  fieldhub_warn(
    "`", old, "` is deprecated; use `", new, "` instead.",
    class = c("fieldhub_deprecated_warning", "deprecatedWarning"),
    data = list(old = old, new = new), call = call
  )
  legacy_value
}
