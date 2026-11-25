#' Simulate a continuous trait on a FielDHub design
#'
#' Generate Gaussian plot responses using the treatment and experimental units
#' of the realized randomization. Repeated entries, including checks, share a
#' treatment effect across plots and locations. Each draw generates new random
#' effects. Filler plots remain in the field book with missing responses.
#'
#' @param object A FielDHub field design.
#' @param nsim Positive number of independent trials to draw.
#' @param seed Simulation seed. With `NULL`, one seed is drawn and recorded.
#'   If R is using Box-Muller normals, simulation draws from a private
#'   stream to preserve the caller's cached normal.
#' @param ... Reserved for future extensions.
#' @param trait Name of the response column for one draw. Multiple draws use
#'   this name with a numeric suffix.
#' @param mean Trait mean, or a named vector giving the mean for each location.
#' @param residual_sd Nonnegative plot-error standard deviation, or a named
#'   vector giving the standard deviation for each location.
#' @param treatment_sd Nonnegative standard deviation of the treatment signal.
#'   The default is 1.25 times the typical residual SD.
#'   For factorial treatments, its variance is divided equally among the main
#'   effects and a combination-specific interaction.
#' @param design_sd Nonnegative standard deviation for each design term, or
#'   a named vector overriding specific terms. Omitted terms use half the
#'   typical residual SD.
#' @param location_sd Standard deviation of independent location shifts when
#'   `mean` is a single unnamed value and the trial has several locations.
#'   Defaults to 0.625 times the typical residual SD.
#' @param spatial `FALSE` (default), `TRUE` for defaults, or a named list
#'   overriding `sd` (half the typical residual SD), `rho_row` and `rho_col`.
#'   The spatial effect is a separable AR1 x AR1 field
#'   with standard deviation `sd`: `rho_row` is the correlation between
#'   plots in adjacent rows of the same column, `rho_col` between plots in
#'   adjacent columns of the same row, and plots `dr` rows and `dc` columns
#'   apart have correlation `rho_row^dr * rho_col^dc`. Each correlation is
#'   either one fixed value or a range `c(low, high)`, strictly between -1
#'   and 1. A range is drawn from uniformly for every draw and every
#'   location, so each location of each simulated trial has its own field.
#'   The drawn values depend only on `seed`, `nsim` and the number of
#'   locations, so two layouts simulated with the same seed share them.
#'   A correlation that is not given is drawn from `c(0.15, 0.9)`; so is
#'   every correlation with `spatial = TRUE`. The values used are returned
#'   in `spatial_draws`. Spatial correlation
#'   requires physical `ROW` and `COLUMN` in the field book, with unique
#'   coordinate pairs within each location. A Latin square or row-column
#'   design stores its experimental rows and columns there, so first
#'   replace its field book with the physical layout:
#'   `x$fieldBook <- field_layout(x)`, then `simulate(x, spatial = TRUE)`.
#'   The layout keeps the experimental axes as `ROW_SQ`/`COLUMN_SQ` or
#'   `ROW_REP`/`COLUMN_REP`, so the design effects are unchanged.
#'
#' @details The typical residual SD is the arithmetic mean of the location
#'   residual SDs. These defaults scale the trait signal and the effects of
#'   the design when the user changes the residual SD to suit a trait's units.
#'
#' @return A `fieldhub_simulation` list with the field book augmented by
#'   the responses, the input field book, the response data frame, one list
#'   of plot-aligned effect contributions per draw, `spatial_draws` (with a
#'   spatial effect: the true `rho_row` and `rho_col` of every `draw` and
#'   `LOCATION`, whether drawn or fixed; otherwise `NULL`), and replay
#'   metadata.
#'   Standard deviations are in trait units and the Gaussian response is
#'   unbounded.
#' @seealso [reproduce_simulation()]
#' @examples
#' x <- RCBD(t = 6, reps = 3, nrows = 3, seed = 20)
#' sim <- stats::simulate(x, seed = 42, mean = 6, residual_sd = 0.4,
#'                        treatment_sd = 0.6, design_sd = 0.3)
#' head(sim$field_book)
#'
#' # Spatial simulation of a row-column design on its physical layout
#' rc <- row_column(t = 12, nrows = 3, reps = 2, seed = 1)
#' rc$fieldBook <- field_layout(rc)
#' sim_rc <- stats::simulate(rc, seed = 7, spatial = list(rho_row = 0.6, rho_col = 0.3))
#' sim_rc
#'
#' # Correlations drawn for each trial: the true values are in spatial_draws
#' sim_drawn <- stats::simulate(rc, nsim = 3, seed = 7,
#'                              spatial = list(rho_row = c(0.3, 0.9), rho_col = c(0.1, 0.5)))
#' sim_drawn$spatial_draws
#' @md
#' @importFrom stats simulate
#' @export
simulate.FielDHub <- function(object, nsim = 1, seed = NULL, ...,
                              trait = "YIELD", mean = 100, residual_sd = 8,
                              treatment_sd = NULL, design_sd = NULL, location_sd = NULL,
                              spatial = FALSE) {
  if (...length() > 0L) fieldhub_abort("Unrecognized simulation arguments in '...'.")
  book <- object$fieldBook
  problems <- field_book_problems(book)
  if (length(problems)) fieldhub_abort("Simulation requires a field design that ",
                                       paste(problems, collapse = ", "), ".")
  if (!identical(spatial, FALSE)) check_simulation_axes(object)
  terms <- simulation_design_terms(object)
  rng_kind <- RNGkind()
  isolated_rng <- if (rng_kind[2L] == "Box-Muller") rng_kind
  simulate_design_field_book(book, nsim, seed, trait, mean, residual_sd,
                             treatment_sd, design_sd, location_sd, spatial, terms,
                             isolated_rng = isolated_rng)
}

#' Draw responses from a resolved experimental design and optional spatial model
#' @noRd
simulate_design_field_book <- function(field_book, nsim, seed, trait, mean, residual_sd,
                                       treatment_sd, design_sd, location_sd, spatial, terms,
                                       isolated_rng = NULL) {
  validate_iteration_budget(nsim, "nsim", minimum = 1)
  response_names <- simulation_response_names(trait, nsim, field_book)
  location <- as.character(field_book$LOCATION)
  locations <- sort(unique(location), method = "radix")
  site <- match(location, locations)
  site_mean <- simulation_location_values(mean, locations, "mean")
  site_sd <- simulation_location_values(residual_sd, locations, "residual_sd", nonnegative = TRUE)
  typical_sd <- base::mean(site_sd)
  if (is.null(treatment_sd)) treatment_sd <- 1.25 * typical_sd
  if (is.null(location_sd)) location_sd <- 0.625 * typical_sd
  simulation_number(treatment_sd, "treatment_sd", nonnegative = TRUE)
  simulation_number(location_sd, "location_sd", nonnegative = TRUE)
  design_default <- 0.5 * typical_sd
  design_scales <- simulation_design_sd(design_sd, names(terms$design), default = design_default)
  spatial <- simulation_spatial_spec(spatial, default_sd = 0.5 * typical_sd)
  patches <- if (!identical(spatial, FALSE)) simulation_spatial_patches(field_book, site)
  n <- nrow(field_book)
  if (!is.logical(terms$planted) || length(terms$planted) != n || anyNA(terms$planted) ||
      !all(vapply(c(terms$treatment, terms$design), function(group) {
        is.integer(group) && length(group) == n && !anyNA(group) &&
          all(group >= 0L & group <= n)
      }, logical(1)))) fieldhub_abort("Invalid recorded simulation groups.")


  if (anyDuplicated(field_book[c("LOCATION", "ID")])) {
    fieldhub_abort("Plot IDs must be unique within each simulation location.")
  }
  # Assign the same seeded errors to the same plots if a layout reorders them.
  planted_rows <- which(terms$planted)
  planted_rows <- planted_rows[order(site[planted_rows], field_book$ID[planted_rows])]
  seed <- as.integer(resolve_seed(seed))
  draw_normal <- stats::rnorm
  draw_uniform <- stats::runif
  if (is.null(isolated_rng)) {
    local_rng_state()
    set.seed(seed)
  } else {
    stream <- simulation_rng_cpp(seed)
    draw_normal <- function(n, sd = 1) stats::qnorm(simulation_uniforms_cpp(stream, n), sd = sd)
    draw_uniform <- function(n) simulation_uniforms_cpp(stream, n)
  }
  # Every drawn correlation comes first, before any effect, so the values
  # depend only on the seed, nsim and the number of locations: layouts of
  # different designs drawn with one seed share them. Fixed correlations
  # consume no random numbers.
  spatial_draws <- NULL
  if (length(patches)) {
    rho_row <- rho_col <- matrix(0, nsim, length(patches))
    for (draw in seq_len(nsim)) {
      for (k in seq_along(patches)) {
        rho <- simulation_spatial_rho(spatial, draw_uniform)
        rho_row[draw, k] <- rho$rho_row
        rho_col[draw, k] <- rho$rho_col
      }
    }
    first_plot <- vapply(patches, function(patch) patch$rows[1L], integer(1))
    spatial_draws <- data.frame(draw = rep(seq_len(nsim), each = length(patches)),
                                LOCATION = rep(field_book$LOCATION[first_plot], nsim),
                                rho_row = as.vector(t(rho_row)), rho_col = as.vector(t(rho_col)))
  }
  effects <- vector("list", nsim)
  responses <- stats::setNames(vector("list", nsim), response_names)
  treatment_scale <- treatment_sd / sqrt(length(terms$treatment))
  for (draw in seq_len(nsim)) {
    location_shift <- if (length(locations) > 1L && is.null(names(mean))) {
      draw_normal(length(locations), sd = location_sd)
    } else numeric(length(locations))
    components <- c(
      list(location = location_shift[site]),
      lapply(terms$treatment, simulation_group_effect, sd = treatment_scale, draw_normal = draw_normal),
      Map(function(group, sd) simulation_group_effect(group, sd, draw_normal), terms$design, design_scales)
    )
    if (length(patches)) {
      components$spatial <- numeric(n)
      # Each location gets its own correlations: drawn when given as a range
      for (k in seq_along(patches)) {
        rho <- list(sd = spatial$sd, rho_row = rho_row[draw, k], rho_col = rho_col[draw, k])
        components$spatial[patches[[k]]$rows] <- simulation_spatial_draw(patches[[k]], rho, draw_normal)
      }
    }
    components$residual <- numeric(n)
    components$residual[planted_rows] <- draw_normal(length(planted_rows),
      sd = site_sd[site[planted_rows]])
    response <- Reduce(`+`, components, init = site_mean[site])
    if (any(!is.finite(response))) fieldhub_abort("The simulation scales produced non-finite responses.")
    response[!terms$planted] <- NA_real_
    responses[[draw]] <- response
    effects[[draw]] <- components
  }
  responses <- as.data.frame(responses, check.names = FALSE)
  result_book <- field_book
  for (name in response_names) result_book[[name]] <- responses[[name]]
  structure(list(
    field_book = result_book, input_field_book = field_book, responses = responses,
    effects = effects, spatial_draws = spatial_draws, metadata = list(
      model = "design_gaussian", schema_version = 1L, seed = seed,
      rng_kind = if (is.null(isolated_rng)) RNGkind() else isolated_rng,
      package_version = as.character(utils::packageVersion("FielDHub")),
      parameters = c(list(nsim = nsim, seed = seed, trait = trait, mean = mean,
        residual_sd = residual_sd, treatment_sd = treatment_sd,
        design_sd = if (length(design_scales)) design_scales else design_default,
        location_sd = location_sd, spatial = spatial, terms = terms),
        if (!is.null(isolated_rng)) list(isolated_rng = isolated_rng))
    )
  ), class = "fieldhub_simulation")
}

simulation_group_effect <- function(group, sd, draw_normal) {
  c(0, draw_normal(max(group), sd = sd))[group + 1L]
}

simulation_response_names <- function(trait, nsim, field_book) {
  if (!is.character(trait) || !is.null(dim(trait)) || length(trait) != 1L || is.na(trait) || !nzchar(trimws(trait))) {
    fieldhub_abort("'trait' must be one non-empty column name.")
  }
  response_names <- if (nsim == 1) trait else paste0(trait, "_", seq_len(nsim))
  if (any(response_names %in% c(names(field_book), "text"))) {
    fieldhub_abort("A simulated response name is already present in the field book or is reserved.")
  }
  response_names
}

#' Print a design-aware trait simulation
#'
#' Summarize the simulation settings and show the first rows of the
#' simulated field book. The full results stay in the object: `field_book`,
#' `responses`, `effects` (the plot-aligned effects behind each draw),
#' `spatial_draws` (the true spatial correlations) and `metadata`.
#'
#' @param x A `fieldhub_simulation` returned by [simulate.FielDHub()].
#' @param n Number of field-book rows to show.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @md
#' @export
print.fieldhub_simulation <- function(x, n = 6L, ...) {
  meta <- x$metadata
  p <- meta$parameters
  responses <- names(x$responses)
  draws <- length(responses)
  locations <- length(unique(x$field_book$LOCATION))
  number <- function(v) vapply(v, format, character(1), digits = 4)
  values <- function(v) {
    text <- number(v)
    if (!is.null(names(v))) text <- paste(names(v), "=", text)
    paste(text, collapse = ", ")
  }
  line <- function(...) cat("  ", paste0(...), "\n", sep = "")
  cat("FielDHub trait simulation (", meta$model, ")\n", sep = "")
  line("Plots: ", nrow(x$field_book), "  Locations: ", locations,
       "  Draws: ", draws, "  Seed: ", meta$seed)
  line("Trait: ", p$trait, if (draws > 1L) paste0(" (", responses[1L], " ... ", responses[draws], ")"))
  line("Mean: ", values(p$mean), "  Residual SD: ", values(p$residual_sd))
  line("Treatment SD: ", values(p$treatment_sd),
       if (locations > 1L && is.null(names(p$mean))) paste0("  Location SD: ", values(p$location_sd)))
  design <- names(p$terms$design)
  # A unit that is alone at its location has no within-location contrast.
  inactive <- design[!vapply(p$terms$design, function(group) any(group > 0L), logical(1))]
  line("Design effects: ", if (length(design)) values(p$design_sd[design]) else "none",
       if (length(inactive)) paste0(" (no effect: ", paste(inactive, collapse = ", "), ")"))
  rho <- function(v) if (length(v) == 1L) paste("=", v) else paste0("~ U(", v[1L], ", ", v[2L], ")")
  drawn <- !identical(p$spatial, FALSE) && length(c(p$spatial$rho_row, p$spatial$rho_col)) > 2L
  line("Spatial: ", if (identical(p$spatial, FALSE)) "none" else
    paste0("sd = ", number(p$spatial$sd), ", rho_row ", rho(p$spatial$rho_row),
           ", rho_col ", rho(p$spatial$rho_col)))
  if (drawn) {
    d <- x$spatial_draws
    line("  True correlations, drawn for each draw and location (all in x$spatial_draws):")
    shown <- utils::head(d, 3L)
    for (i in seq_len(nrow(shown))) {
      line("    draw ", shown$draw[i], if (locations > 1L) paste0(", ", shown$LOCATION[i]),
           ": rho_row = ", format(shown$rho_row[i], digits = 3),
           ", rho_col = ", format(shown$rho_col[i], digits = 3))
    }
    if (nrow(d) > 3L) line("    ... ", nrow(d) - 3L, " more")
  }
  line("Effects behind each draw: x$effects[[i]] (", paste(names(x$effects[[1L]]), collapse = ", "), ")")
  cat("\n")
  shown <- c(setdiff(names(x$field_book), responses), utils::head(responses, 3L))
  print(utils::head(x$field_book[shown], n))
  if (nrow(x$field_book) > n || draws > 3L) {
    cat("... x$field_book has ", nrow(x$field_book), " rows and ", draws,
        if (draws == 1L) " draw" else " draws", "\n", sep = "")
  }
  invisible(x)
}
