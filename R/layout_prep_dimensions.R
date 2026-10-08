#' @noRd
.prep_max_fillers <- 10L

#' @noRd
prep_dimension_options <- function(
    total_plots,
    allow_fillers = FALSE,
    max_fillers = NULL) {

    if (length(total_plots) != 1 || is.na(total_plots) ||
        total_plots < 1 || total_plots %% 1 != 0) {
        fieldhub_abort("total_plots must be a positive integer.")
    }
    if (length(allow_fillers) != 1 || is.na(allow_fillers) ||
        !is.logical(allow_fillers)) {
        fieldhub_abort("allow_fillers must be TRUE or FALSE.")
    }
    if (is.null(max_fillers)) {
        max_fillers <- .prep_max_fillers
    }
    if (length(max_fillers) != 1 || is.na(max_fillers) ||
        max_fillers < 0 || max_fillers %% 1 != 0) {
        fieldhub_abort("max_fillers must be a non-negative integer.")
    }

    additional_plots <- if (allow_fillers) 0:max_fillers else 0
    options <- list()
    option_index <- 1
    source_order <- 1
    for (fillers in additional_plots) {
        capacity <- total_plots + fillers
        choices <- factor_subsets(capacity)$labels
        if (is.null(choices)) next
        for (choice in choices) {
            dimensions <- as.numeric(unlist(strsplit(choice, " x ")))
            options[[option_index]] <- data.frame(
                rows = dimensions[1],
                columns = dimensions[2],
                capacity = capacity,
                fillers = fillers,
                diff_dim = abs(dimensions[1] - dimensions[2]),
                source_order = source_order
            )
            option_index <- option_index + 1
            source_order <- source_order + 1
        }
    }
    if (length(options) == 0) return(NULL)

    options <- dplyr::bind_rows(options)
    options <- unique(options)
    options <- options[order(
        options$fillers,
        options$diff_dim,
        options$source_order
    ), ]
    options$value <- paste(options$rows, "x", options$columns)
    options$label <- ifelse(
        options$fillers == 0,
        options$value,
        paste0(
            options$value,
            " (+",
            options$fillers,
            ifelse(options$fillers == 1, " filler)", " fillers)")
        )
    )
    rownames(options) <- NULL
    options
}

#' Explain why a p-rep design has no field dimensions to offer
#'
#' @description Shared by the p-rep and multi-location p-rep modules when
#' \code{prep_dimension_options()} returns \code{NULL}.
#'
#' @param allow_fillers Whether filler plots were allowed.
#' @param max_fillers The most filler plots a field may have.
#' @return A list with the dialog \code{title}, the \code{message} and its
#'   \code{severity} (\code{"info"} when allowing fillers would help,
#'   \code{"error"} otherwise).
#' @noRd
prep_no_dimensions_problem <- function(allow_fillers, max_fillers = .prep_max_fillers) {
  if (!isTRUE(allow_fillers)) {
    return(list(
      title = "Filler plots required",
      message = sprintf(paste(
        "The current design does not fit any supported rectangular",
        "field dimensions without unused cells. Select 'Allow filler",
        "plots' to continue. FielDHub will then offer nearby valid",
        "dimensions requiring no more than %d filler plots and place",
        "the fillers at the end of the selected planter path."
      ), max_fillers),
      severity = "info"
    ))
  }
  list(
    title = "No dimensions within the filler limit",
    message = sprintf(paste(
      "FielDHub could not find supported rectangular field dimensions",
      "requiring %d or fewer filler plots. Adjust the number of entries",
      "or replication settings and try again."
    ), max_fillers),
    severity = "error"
  )
}
