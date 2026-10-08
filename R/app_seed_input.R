#' Shared optional randomization seed control
#'
#' @description Blank (the default) means an automatic seed, which the result
#' records. Original input panels supply their short label; the placeholder
#' and tooltip explain the unchanged automatic-seed behavior.
#' @noRd
app_seed_input <- function(inputId, value = NULL, label = fieldhub_control_concepts()$seed$label) {
  widget <- shiny::numericInput(inputId, label = label,
                       value = value, min = -.Machine$integer.max,
                       max = .Machine$integer.max, step = 1)
  htmltools::tagQuery(widget)$find("input")$addAttrs(
    placeholder = "Automatic", title = "Leave blank to generate and record a random seed.")$allTags()
}
