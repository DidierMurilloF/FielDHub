#' Named field-book data and tooltips for a selected location
#' @noRd
field_book_heatmap_data <- function(field_book, response_name, selected = 1L,
                                    label_column = "TREATMENT", label_title = "Treatment",
                                    include_site = TRUE, include_checks = FALSE) {
  for (value in list(response_name, label_column, label_title)) {
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
      fieldhub_abort("Heatmap column names and label titles must be non-empty strings.")
    }
  }
  for (value in list(include_site, include_checks)) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      fieldhub_abort("Heatmap tooltip flags must be TRUE or FALSE.")
    }
  }
  required <- c("LOCATION", "ROW", "COLUMN", response_name, label_column)
  if (!is.data.frame(field_book) || nrow(field_book) == 0L ||
      anyNA(names(field_book)) || anyDuplicated(names(field_book)) > 0L ||
      !all(required %in% names(field_book))) {
    fieldhub_abort("The heatmap needs a non-empty field book with locations, coordinates, labels, and the named response.")
  }
  if (include_checks && "CHECKS" %in% names(field_book)) required <- c(required, "CHECKS")
  for (name in unique(required)) {
    value <- field_book[[name]]
    if (!is.atomic(value) || !is.null(dim(value)) || is.complex(value)) {
      fieldhub_abort("Heatmap column '", name, "' must be an atomic vector.")
    }
    if (name %in% c("LOCATION", "ROW", "COLUMN") && anyNA(value)) {
      fieldhub_abort("Heatmap locations and coordinates cannot be missing.")
    }
  }
  response <- field_book[[response_name]]
  if (!is.numeric(response) || any(is.infinite(response))) {
    fieldhub_abort("The heatmap response must be numeric, with finite or missing values.")
  }
  locations <- as.character(field_book$LOCATION)
  available <- unique(locations)
  if (!is.numeric(selected) || is.complex(selected) || length(selected) != 1L ||
      !is.finite(selected) || !selected %in% seq_along(available)) {
    fieldhub_abort("Select one available location for the heatmap.")
  }
  data <- field_book[locations == available[selected], , drop = FALSE]
  site_text <- if (include_site) paste0("Site: ", available[selected], "\n") else ""
  check_text <- if (include_checks && "CHECKS" %in% names(data)) {
    paste0("Check: ", ifelse(data$CHECKS != 0, "yes", "no"), "\n")
  } else ""
  data$text <- paste0(
    site_text, "Row: ", data$ROW, "\n", "Col: ", data$COLUMN, "\n",
    label_title, ": ", data[[label_column]], "\n", check_text,
    paste(response_name, ": "), round(data[[response_name]], 2)
  )
  data$ROW <- as.factor(data$ROW)
  data$COLUMN <- as.factor(data$COLUMN)
  data
}
