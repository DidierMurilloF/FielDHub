#' Rectangular field choices for optimized arrangements
#'
#' @param plots Total number of plots, including replicated checks.
#' @return Character dimension labels with at least four rows and four columns,
#'   ordered by increasing difference between rows and columns. An empty vector
#'   means no option was found.
#' @noRd
optimized_dimension_choices <- function(plots) {
    invalid_plots <- !is.numeric(plots) || length(plots) != 1L ||
        is.na(plots) || !is.finite(plots) || plots < 1 || plots %% 1 != 0 ||
        plots > .Machine$integer.max
    if (invalid_plots) {
        fieldhub_abort("`plots` must be one positive whole number in the supported range.")
    }
    choices <- unlist(factor_subsets(plots)$labels, use.names = FALSE)
    if (length(choices) == 0L) return(character())
    dims <- do.call(rbind, strsplit(choices, " x ", fixed = TRUE))
    storage.mode(dims) <- "integer"
    choices[order(abs(dims[, 1] - dims[, 2]))]
}
