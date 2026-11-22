#' Check spacing on a page: its controls, its values and its arguments
#'
#' @description The pages whose design places checks on diagonals (Single
#' and Multiple Diagonal Arrangement, Sparse Allocation) share one
#' implementation: the two sidebar controls, the values read from them, the
#' arguments their engines take (\code{spread_checks}, \code{dist_method})
#' and the report behind the "Check spacing" button
#' (\code{app_check_spacing()}; a spec asks for it with
#' \code{report = "check_spacing"}). On a page the spacing is optimized unless
#' the user turns it off; the functions' own default stays \code{FALSE}.
#' @return \code{check_spacing_controls()}: the controls, for a spec.
#' @noRd
check_spacing_controls <- function() {
  list(ctl_flag("spread_checks", value = TRUE),
       ctl_select("dist_method", show_if = "input.spread_checks == true"))
}

#' @param controls The parsed controls of a page.
#' @return \code{check_spacing_values()}: the values of the two controls; a
#'   distance is read only while the optimization is on.
#' @noRd
check_spacing_values <- function(controls) {
  list(spread_checks = controls$spread_checks,
       dist_method = if (isTRUE(controls$spread_checks)) controls$dist_method else "euclidean")
}

#' @param values The values of a page.
#' @return \code{check_spacing_args()}: the engine's arguments, with the
#'   API defaults for values a caller left out.
#' @noRd
check_spacing_args <- function(values) {
  list(spread_checks = values[["spread_checks"]] %||% FALSE,
       dist_method = values[["dist_method"]] %||% "euclidean")
}

#' The check spacing of a design with diagonal checks, behind a button on its plot
#'
#' @description What \code{print_diagonal_spacing()} prints, as a small
#' structured view built from the design's own record
#' (\code{x$check_spacing}): the settings as facts, the four measures as a
#' table (before and after the search when the spacing was optimized), and
#' the notes as one tooltip. It opens from a "Check spacing" button
#' (layout-menu.js). A design at several locations shows one at a time, with
#' a small select to change it.
#'
#' @param x A design with \code{check_spacing}, or one without (\code{NULL}).
#' @param selected The location shown first.
#' @param id The id of the panel, for \code{aria-controls}.
#' @return A \code{shiny::div()}, or \code{NULL}.
#' @noRd
app_check_spacing <- function(x, selected = 1L, id = "check_spacing") {
  reports <- x$check_spacing
  if (is.null(reports) || !length(reports)) return(NULL)
  sites <- seq_along(reports)
  if (!selected %in% sites) selected <- 1L
  names <- unique(as.character(x$fieldBook$LOCATION))
  names <- if (length(names) == length(sites)) names else as.character(sites)
  notes <- paste(
    "Distance averages give equal weight to checks with repeated occurrences.",
    "Minimum distance is the smallest across checks.",
    "Neighbor percentage includes all check occurrences.",
    "A dash means that a same-check distance cannot be calculated.",
    "Spacing metrics do not measure statistical precision.")
  section <- function(site) {
    shiny::div(class = "fh-spacing-site", `data-fh-section` = site, hidden = if (site != selected) NA,
      shiny::tags$dl(class = "fh-facts", lapply(app_check_spacing_facts(x, site), function(fact) {
        shiny::div(shiny::tags$dt(fact$label), shiny::tags$dd(fact$value))
      })),
      app_summary_table(app_check_spacing_table(reports[[site]])))
  }
  shiny::div(class = "fh-view-menu",
    shiny::tags$button(type = "button", class = "btn fh-view-menu-toggle",
      `aria-haspopup` = "true", `aria-expanded` = "false", `aria-controls` = id,
      shiny::icon("ruler-horizontal", verify_fa = FALSE), shiny::span("Check spacing"),
      shiny::icon("chevron-down", class = "fh-view-menu-caret", verify_fa = FALSE)),
    shiny::div(id = id, class = "fh-view-menu-panel fh-spacing", role = "group",
      `aria-label` = "Check spacing", hidden = NA,
      shiny::div(class = "fh-spacing-head",
        shiny::h3("Check spacing"),
        shiny::span(class = "fh-tip fh-info", tabindex = "0", role = "img", `data-fh-tip` = NA,
                    title = notes, `aria-label` = paste("About these measures.", notes),
                    shiny::icon("circle-info", verify_fa = FALSE)),
        if (length(sites) > 1L) {
          shiny::tags$select(class = "form-control fh-spacing-location", `data-fh-switch` = NA,
            `aria-label` = "Location",
            lapply(sites, function(site) {
              shiny::tags$option(value = site, selected = if (site == selected) NA, names[[site]])
            }))
        } else shiny::span(class = "fh-spacing-where", names[[1L]])),
      lapply(sites, section)))
}

#' The settings of the spacing at one location, as label and value
#' @noRd
app_check_spacing_facts <- function(x, site) {
  report <- x$check_spacing[[site]]
  optimized <- !is.null(report$after)
  facts <- list(
    list(label = "Optimization", value = if (optimized) "On" else "Off"),
    list(label = "Distance",
         value = paste0(if (identical(report$dist_method, "euclidean")) "Euclidean" else "Manhattan",
                        ", in grid steps")),
    list(label = "Neighbor radius", value = paste(report$neighbor_radius, "grid steps")))
  diagnostics <- if (optimized) x$infoDesign$check_optimization[[site]]
  if (!is.null(diagnostics)) {
    facts <- c(facts, list(
      list(label = "Search", value = gsub("_", " ", diagnostics$stop_reason)),
      list(label = "Accepted swaps", value = format(diagnostics$swaps, big.mark = ",")),
      list(label = "Candidates evaluated",
           value = format(diagnostics$evaluations, big.mark = ",", scientific = FALSE))))
  }
  facts
}

#' The four spacing measures of one location, formatted
#' @noRd
app_check_spacing_table <- function(report) {
  figures <- function(metrics) {
    totals <- diagonal_spacing_totals(metrics)
    shown <- c(sprintf("%.2f", totals[1:3]), sprintf("%.1f%%", totals[4]))
    ifelse(is.na(totals), "–", shown)
  }
  table <- data.frame(Measure = c("Minimum same-check distance", "Mean nearest same-check distance",
    "Mean pairwise same-check distance", "Occurrences with a same-check neighbor"),
    stringsAsFactors = FALSE)
  if (is.null(report$after)) {
    table$Value <- figures(report$before)
  } else {
    table$Before <- figures(report$before)
    table$After <- figures(report$after)
  }
  table
}
