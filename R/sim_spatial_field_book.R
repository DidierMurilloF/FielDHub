#' Simulate responses for all locations of a spatial field book
#'
#' @param field_book Field book containing ID, LOCATION, ROW, COLUMN and ENTRY.
#' @param nrows,ncols Field dimensions, one value or one per location.
#' @param correlation_x,correlation_y Correlations along columns and rows.
#' @param min_value,max_value Response range used by the spatial simulator.
#' @param response_name Name of the new simulated-response column.
#' @param seed Simulation seed. When omitted, one integer is drawn from the
#'   current random-number stream and returned; the simulation's own
#'   randomization does not change the caller's stream.
#' @return A list containing the augmented field_book, per-location simulations,
#'   the seed, exact input field book, and reconstruction metadata. Locations
#'   retain their first-appearance order.
#' @noRd
simulate_spatial_field_book <- function(field_book, nrows, ncols,
                                        correlation_x, correlation_y,
                                        min_value, max_value,
                                        response_name, seed = NULL) {
    required <- c("ID", "LOCATION", "ROW", "COLUMN", "ENTRY")
    if (!is.data.frame(field_book) || nrow(field_book) == 0L ||
        !all(required %in% names(field_book))) {
        fieldhub_abort("The field book must contain rows with ID, LOCATION, ROW, COLUMN and ENTRY.")
    }
    if (anyDuplicated(names(field_book)) > 0L) {
        fieldhub_abort("The field book must have unique column names.")
    }
    for (name in c("ID", "ROW", "COLUMN", "ENTRY")) {
        value <- field_book[[name]]
        minimum <- if (name == "ENTRY") 0 else 1
        if (!is.numeric(value) || anyNA(value) || any(!is.finite(value)) ||
            any(value < minimum) || any(value %% 1 != 0)) {
            fieldhub_abort("`", name, "` must contain valid whole-number identifiers or coordinates.")
        }
    }
    location <- as.character(field_book$LOCATION)
    if (anyNA(location) || any(!nzchar(location))) {
        fieldhub_abort("Every field-book row must have a LOCATION.")
    }
    locations <- unique(location)
    dimensions <- list(nrows = nrows, ncols = ncols)
    for (name in names(dimensions)) {
        value <- dimensions[[name]]
        if (!is.numeric(value) || !length(value) %in% c(1L, length(locations)) ||
            anyNA(value) || any(!is.finite(value)) || any(value < 1) ||
            any(value %% 1 != 0)) {
            fieldhub_abort("`", name, "` must contain one positive whole number or one per location.")
        }
    }
    nrows <- rep(nrows, length.out = length(locations))
    ncols <- rep(ncols, length.out = length(locations))
    for (name in c("correlation_x", "correlation_y")) {
        value <- if (name == "correlation_x") correlation_x else correlation_y
        if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
            !is.finite(value) || abs(value) >= 1) {
            fieldhub_abort("`", name, "` must be one finite number between -1 and 1.")
        }
    }
    if (abs(correlation_x - correlation_y) >= 0.85) {
        fieldhub_abort("The two spatial correlations must differ by less than 0.85.")
    }
    if (!is.numeric(min_value) || length(min_value) != 1L ||
        !is.numeric(max_value) || length(max_value) != 1L ||
        !is.finite(min_value) || !is.finite(max_value) || min_value >= max_value) {
        fieldhub_abort("The simulated-response range must have finite min_value < max_value.")
    }
    if (!is.character(response_name) || length(response_name) != 1L ||
        is.na(response_name) || !nzchar(response_name)) {
        fieldhub_abort("The simulated response must have one non-empty name.")
    }
    if (response_name %in% names(field_book)) {
        fieldhub_abort("The field book already has a '", response_name, "' column.")
    }
    if (response_name %in% c("ZST", "genot", "text")) {
        fieldhub_abort("The response name '", response_name, "' is reserved by the spatial simulator.")
    }

    # Resolve the seed before scoping the stream, so a seedless call's one
    # draw reaches the caller and only the simulation's own randomization
    # (below) is undone on exit.
    seed <- resolve_seed(seed)
    local_rng_state()
    if (seed > .Machine$integer.max || seed < -.Machine$integer.max) {
        fieldhub_abort("'seed' must fit in an R integer.")
    }
    seed <- as.integer(seed)
    set.seed(seed)
    books <- simulations <- vector("list", length(locations))
    for (i in seq_along(locations)) {
        book <- field_book[location == locations[i], , drop = FALSE]
        if (anyDuplicated(book$ID) > 0L) {
            fieldhub_abort("ID must be unique within each location.")
        }
        if (nrow(book) != nrows[i] * ncols[i] || nrow(book) < 2L ||
            any(book$ROW > nrows[i]) || any(book$COLUMN > ncols[i]) ||
            anyDuplicated(book[c("ROW", "COLUMN")]) > 0L) {
            fieldhub_abort("The field-book coordinates must fill the dimensions of each location exactly.")
        }
        simulation <- AR1xAR1_simulation(
            nrows = nrows[i], ncols = ncols[i],
            ROX = correlation_x, ROY = correlation_y,
            minValue = min_value, maxValue = max_value,
            fieldbook = book[, c("ID", "ROW", "COLUMN", "ENTRY")],
            trail = response_name, seed = NULL
        )$outOrder
        simulations[[i]] <- simulation
        aligned <- simulation[match(book$ID, simulation$ID), , drop = FALSE]
        books[[i]] <- append_simulated_response(book, aligned, response_name)
    }
    list(
        field_book = dplyr::bind_rows(books), simulations = simulations, seed = seed,
        input_field_book = field_book,
        metadata = list(
            model = "ar1xar1", schema_version = 1L, seed = seed,
            rng_kind = RNGkind(), package_version = as.character(utils::packageVersion("FielDHub")),
            parameters = list(nrows = nrows, ncols = ncols,
                              correlation_x = correlation_x, correlation_y = correlation_y,
                              min_value = min_value, max_value = max_value,
                              response_name = response_name, seed = seed)
        )
    )
}

#' Prepare spatial workflow data independently of a reactive session
#' @noRd
spatial_workflow_book <- function(field_book, settings = NULL, nrows, ncols,
                                   seed = NULL, renumber_display = FALSE,
                                   coerce_book = FALSE) {
  if (!is.data.frame(field_book) || nrow(field_book) == 0L) {
    fieldhub_abort("The workflow needs a nonempty field book.")
  }
  validate_flag(renumber_display, "renumber_display")
  validate_flag(coerce_book, "coerce_book")
  simulation <- NULL
  if (!is.null(settings)) {
    required <- c("min_value", "max_value", "response_name", "correlation_x", "correlation_y")
    if (!is.list(settings) || !all(required %in% names(settings))) {
      fieldhub_abort("Spatial simulation settings must include bounds, a response name, and both correlations.")
    }
    simulation <- simulate_spatial_field_book(
      field_book = if (coerce_book) as.data.frame(field_book) else field_book,
      nrows = nrows, ncols = ncols,
      correlation_x = as.numeric(settings$correlation_x),
      correlation_y = as.numeric(settings$correlation_y),
      min_value = as.numeric(settings$min_value), max_value = as.numeric(settings$max_value),
      response_name = as.character(settings$response_name), seed = seed
    )
    field_book <- simulation$field_book
  }
  # Display numbering must not replace the source IDs in the simulation record.
  if (renumber_display) field_book$ID <- seq_len(nrow(field_book))
  list(df = field_book, simulation = simulation)
}
