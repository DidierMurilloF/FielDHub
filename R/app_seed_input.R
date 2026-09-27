#' Shared optional randomization seed control
#' @noRd
app_seed_input <- function(inputId, value) {
  shiny::numericInput(inputId, label = "Random seed (blank = automatic):",
                       value = value, min = -.Machine$integer.max,
                       max = .Machine$integer.max, step = 1)
}
