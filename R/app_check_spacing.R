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

#' The report of a spatial page, behind a button on its plot
#'
#' @param kind The spec's \code{report}: \code{"check_spacing"} or
#'   \code{"replicate_spacing"}.
#' @param x The design.
#' @param selected The location shown first.
#' @param id The id of the panel, for \code{aria-controls}.
#' @return A \code{shiny::div()}, or \code{NULL} for a design without it.
#' @noRd
app_design_report <- function(kind, x, selected = 1L, id = "design_report") {
  switch(kind,
    check_spacing = app_check_spacing(x, selected, id),
    replicate_spacing = app_replicate_spacing(x, selected, id))
}

#' A spacing report: a button on the plot's toolbar and the panel it opens
#'
#' @description The one view of the spacing reports (layout-menu.js): a
#' header with the title, the notes as one tooltip and the location; the
#' settings of each location as facts and its measures as a table. A design
#' at several locations shows one at a time, with a small select to change
#' it.
#'
#' @param title The name of the button and of the panel.
#' @param sections One \code{list(facts = , table = )} for each location:
#'   facts as \code{list(label = , value = )}, the table a data frame.
#' @param names The names of the locations.
#' @param notes What the measures mean, as sentences.
#' @param selected The location shown first.
#' @param id The id of the panel, for \code{aria-controls}.
#' @param icon The icon of the button.
#' @return A \code{shiny::div()}, or \code{NULL} without sections.
#' @noRd
app_spacing_report <- function(title, sections, names, notes, selected = 1L, id = "design_report",
                               icon = "ruler-horizontal") {
  if (!length(sections)) return(NULL)
  sites <- seq_along(sections)
  if (!selected %in% sites) selected <- 1L
  names <- if (length(names) == length(sites)) as.character(names) else as.character(sites)
  notes <- paste(notes, collapse = " ")
  section <- function(site) {
    shiny::div(class = "fh-spacing-site", `data-fh-section` = site, hidden = if (site != selected) NA,
      shiny::tags$dl(class = "fh-facts", lapply(sections[[site]]$facts, function(fact) {
        shiny::div(shiny::tags$dt(fact$label), shiny::tags$dd(fact$value))
      })),
      app_summary_table(sections[[site]]$table))
  }
  shiny::div(class = "fh-view-menu",
    shiny::tags$button(type = "button", class = "btn fh-view-menu-toggle",
      `aria-haspopup` = "true", `aria-expanded` = "false", `aria-controls` = id,
      shiny::icon(icon, verify_fa = FALSE), shiny::span(title),
      shiny::icon("chevron-down", class = "fh-view-menu-caret", verify_fa = FALSE)),
    shiny::div(id = id, class = "fh-view-menu-panel fh-spacing", role = "group",
      `aria-label` = title, hidden = NA,
      shiny::div(class = "fh-spacing-head",
        shiny::h3(title),
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

#' The check spacing of a design with diagonal checks
#'
#' @description What \code{print_diagonal_spacing()} prints, built from the
#' design's own record (\code{x$check_spacing}): the settings as facts and
#' the four measures as a table (before and after the search when the
#' spacing was optimized).
#'
#' @param x A design with \code{check_spacing}, or one without (\code{NULL}).
#' @param selected The location shown first.
#' @param id The id of the panel, for \code{aria-controls}.
#' @return A \code{shiny::div()}, or \code{NULL}.
#' @noRd
app_check_spacing <- function(x, selected = 1L, id = "check_spacing") {
  reports <- x$check_spacing
  if (is.null(reports) || !length(reports)) return(NULL)
  app_spacing_report("Check spacing",
    sections = lapply(seq_along(reports), function(site) {
      list(facts = app_check_spacing_facts(x, site), table = app_check_spacing_table(reports[[site]]))
    }),
    names = unique(as.character(x$fieldBook$LOCATION)),
    notes = c("Distance averages give equal weight to checks with repeated occurrences.",
              "Minimum distance is the smallest across checks.",
              "Neighbor percentage includes all check occurrences.",
              "A dash means that a same-check distance cannot be calculated.",
              "Spacing metrics do not measure statistical precision."),
    selected = selected, id = id)
}

#' The spacing of the copies of the replicated entries of a p-rep design
#'
#' @description Built from the design itself: the distance between every
#' pair of copies of an entry (\code{x$pairsDistance}), the entries of each
#' location (\code{x$reps_info}), its field (\code{x$infoDesign}) and the
#' recorded settings of the search (\code{x$metadata$parameters}). The
#' engine keeps the final placement only, so the measures have one value.
#' The parameters the page no longer lists under Field setup (the field,
#' its plots, the entries and the seed) are facts here.
#'
#' @param x A p-rep design (\code{partially_replicated()},
#'   \code{multi_location_prep()}).
#' @param selected The location shown first.
#' @param id The id of the panel, for \code{aria-controls}.
#' @return A \code{shiny::div()}, or \code{NULL} for a design without pairs.
#' @noRd
app_replicate_spacing <- function(x, selected = 1L, id = "replicate_spacing") {
  pairs <- x$pairsDistance
  if (is.null(pairs) || !length(pairs)) return(NULL)
  app_spacing_report("Replicate spacing",
    sections = lapply(seq_along(pairs), function(site) {
      list(facts = app_replicate_spacing_facts(x, site), table = app_replicate_spacing_table(pairs[[site]]))
    }),
    names = x$reps_info$LOCATION,
    notes = c("Distances are between the plots of two copies of the same entry, in grid steps.",
              "An entry with more than two copies counts once for each pair of its copies.",
              "Minimum distance is the smallest across all pairs.",
              "A pair shares a row or column when both copies lie in the same row or the same column.",
              "Spacing metrics do not measure statistical precision."),
    selected = selected, id = id)
}

#' The spacing settings of a p-rep design, as label and value
#' @noRd
app_replicate_spacing_facts <- function(x, site) {
  info <- x$infoDesign
  settings <- x$metadata$parameters
  # The field, its plots and its entries are on Field setup: the spacing alone here
  facts <- list(
    list(label = "Optimization", value = if (isFALSE(settings$spread_reps)) "Off" else "On"),
    list(label = "Distance",
         value = paste0(if (identical(settings$dist_method, "manhattan")) "Manhattan" else "Euclidean",
                        ", in grid steps")),
    list(label = "Distinct rows and columns",
         value = if (isTRUE(settings$rowcol_penalty > 0)) "Required" else "Not required"))
  if (!is.null(info$seed)) {
    facts <- c(facts, list(list(label = "Seed", value = format(info$seed, scientific = FALSE, trim = TRUE))))
  }
  facts
}

#' The spacing measures of one location of a p-rep design, formatted
#' @noRd
app_replicate_spacing_table <- function(pairs) {
  distance <- pairs$DIST
  shared <- pairs$rA == pairs$rB | pairs$cA == pairs$cB
  none <- !length(distance)
  data.frame(
    Measure = c("Minimum same-entry distance", "Mean same-entry distance",
                "Pairs of copies", "Pairs sharing a row or column"),
    Value = c(if (none) "–" else sprintf("%.2f", min(distance)),
              if (none) "–" else sprintf("%.2f", mean(distance)),
              format(length(distance), big.mark = ",", trim = TRUE),
              if (none) "–" else sprintf("%.1f%%", 100 * mean(shared))),
    stringsAsFactors = FALSE)
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
