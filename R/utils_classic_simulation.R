#' Shared classic response simulation with recorded inputs
#'
#' Preserve the established location/plot ordering, or the ID ordering used by
#' selected classic app workflows. The input field book and simulation metadata
#' are retained separately from the unchanged output data-frame schema.
#' @noRd
simulate_classic_field_book <- function(field_book, min_value, max_value,
                                         response_name, seed = NULL,
                                         order_by_id = FALSE) {
  if (!is.character(response_name) || length(response_name) != 1L ||
      is.na(response_name) || !nzchar(trimws(response_name))) {
    fieldhub_abort("The simulated response must have one non-empty name.")
  }
  if (response_name %in% names(field_book) || response_name == "text") {
    fieldhub_abort("The response name '", response_name, "' is already present or reserved.")
  }
  if (!is.logical(order_by_id) || length(order_by_id) != 1L || is.na(order_by_id)) {
    fieldhub_abort("`order_by_id` must be TRUE or FALSE.")
  }
  if (order_by_id) {
    id <- if (is.data.frame(field_book)) field_book[["ID"]] else NULL
    if (!is.numeric(id) || is.complex(id) || !is.null(dim(id)) ||
        length(id) != nrow(field_book) || anyNA(id) || any(!is.finite(id))) {
      fieldhub_abort("ID ordering requires a finite numeric ID column.")
    }
  }
  local_rng_state()
  seed <- as.integer(resolve_seed(seed))
  result <- norm_trunc(min_value, max_value, field_book, seed = seed)
  names(result)[names(result) == "RESP"] <- response_name
  if (order_by_id) result <- result[order(result$ID), ]
  list(
    field_book = result,
    input_field_book = field_book,
    metadata = list(
      model = "truncated_normal", schema_version = 1L, seed = seed,
      rng_kind = RNGkind(), package_version = as.character(utils::packageVersion("FielDHub")),
      parameters = list(min_value = min_value, max_value = max_value,
                        response_name = response_name, seed = seed,
                        order_by_id = order_by_id)
    )
  )
}
