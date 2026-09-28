#' Available replicate arrangements for classic layout controls
#' @noRd
classic_stacking_choices <- function(reps, grid = FALSE) {
  choices <- c("Vertical Stack Panel" = "vertical", "Horizontal Stack Panel" = "horizontal")
  if (grid) {
    validate_iteration_budget(reps, "reps")
    if (reps >= 4 && (reps %% 2 == 0 || sqrt(reps) %% 1 == 0)) {
      choices <- c(choices, "Grid Panel" = "grid_panel")
    }
  }
  choices
}
