#' Shared optional randomization seed control
#'
#' @description The label is the seed concept's one label
#' (\code{fieldhub_control_concepts()}); blank (the default) means an
#' automatic seed, which the result records.
#' @noRd
app_seed_input <- function(inputId, value = NULL) {
  shiny::numericInput(inputId, label = fieldhub_control_concepts()$seed$label,
                       value = value, min = -.Machine$integer.max,
                       max = .Machine$integer.max, step = 1)
}
