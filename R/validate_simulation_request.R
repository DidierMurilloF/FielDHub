#' Validate an app submission before committing new simulation settings
#' @noRd
simulation_request <- function(mean, residual_sd, trait, other = NULL,
                               field_book = data.frame(), spatial = FALSE) {
  simulation_number(mean, "Trait mean")
  simulation_number(residual_sd, "Plot-to-plot SD", nonnegative = TRUE)
  if (!is.character(trait) || !is.null(dim(trait)) || length(trait) != 1L ||
      is.na(trait) || !trait %in% c("YIELD", "MOISTURE", "HEIGHT", "Other")) {
    fieldhub_abort("Select a response trait, or choose Other to enter its name.")
  }
  trait <- if (trait == "Other") other else trait
  simulation_response_names(trait, 1L, field_book)
  spatial <- simulation_spatial_spec(spatial, default_sd = 0.5 * residual_sd)
  if (!identical(spatial, FALSE)) {
    site <- match(field_book$LOCATION, unique(field_book$LOCATION))
    simulation_spatial_patches(field_book, site, spatial)
  }
  list(mean = as.numeric(mean), residual_sd = as.numeric(residual_sd),
       trait = trait, spatial = spatial)
}
