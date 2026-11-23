#' A design's summary as a page, not as printed text
#'
#' @description What \code{print()} shows for a design, built from the
#' design itself: its parameters (\code{infoDesign}), the seed that
#' reproduces it, the efficiency of its blocks (\code{blocksModel}, when
#' the design has one) and the first rows of its field book. Explanations
#' are tooltips on the dotted terms. Parameters without a label here are
#' shown under their own name, so a design other than the lattices can use
#' it as it is.
#'
#' @param x A design created by FielDHub.
#' @param block_levels Names for the rows of \code{blocksModel}, outermost
#'   blocks first, or \code{NULL} to number them.
#' @param rows How many field book rows to preview.
#' @param book The field book to preview: the design's, or the one of the
#'   layout in view where the design's has no rows and columns.
#' @param field Whether to show the plots and the rows and columns of
#'   \code{book} among the parameters.
#' @param tabset The id of the page's result tabs, for the link to the
#'   field book (design-summary.js), or \code{NULL} for no link.
#' @return A \code{shiny::div()}.
#' @noRd
app_design_summary <- function(x, block_levels = NULL, rows = 6L, tabset = NULL,
                               book = x$fieldBook, field = FALSE) {
  info <- x$infoDesign
  seed <- if (!is.null(info$seed)) format(info$seed, scientific = FALSE, trim = TRUE)
  preview <- utils::head(book, rows)
  # A design keeps one efficiency table, or one for each location (alike)
  efficiency <- x$blocksModel
  if (!is.data.frame(efficiency) && is.list(efficiency) && length(efficiency)) efficiency <- efficiency[[1L]]
  if (!is.data.frame(efficiency)) efficiency <- NULL
  shiny::div(class = "fh-summary",
    shiny::tags$section(
      shiny::h3("Design"),
      shiny::tags$dl(class = "fh-stats", lapply(app_summary_items(info, book, field), function(item) {
        shiny::div(shiny::tags$dt(app_summary_term(item$label, item$tip)), shiny::tags$dd(item$value),
                   if (!is.null(item$names)) shiny::tags$small(app_summary_names(item$names)))
      })),
      if (!is.null(seed)) shiny::p(class = "fh-seed-line",
        app_summary_term("Seed", "Enter this seed to get the same randomization again."),
        shiny::tags$code(seed), app_summary_copy(seed))),
    if (!is.null(efficiency)) shiny::tags$section(
      shiny::h3("Efficiency"),
      shiny::div(class = "fh-table-wrap",
                 app_summary_table(app_summary_efficiency(efficiency, block_levels)))),
    shiny::tags$section(
      shiny::div(class = "fh-summary-head",
        shiny::h3("Field book"),
        shiny::span(paste("First", nrow(preview), "of", nrow(book), "plots")),
        if (!is.null(tabset)) shiny::tags$button(type = "button", class = "fh-summary-link",
          `data-fh-tabset` = tabset, `data-fh-tab` = "Field Book", "See all")),
      shiny::div(class = "fh-table-wrap",
                 app_summary_table(preview[setdiff(names(preview), app_summary_repeated(info))])))
  )
}

#' The parameters of a page before its design exists, as the summary shows them
#'
#' @param items Lists with a \code{label}, a \code{value} and, where the
#'   term needs it, a \code{tip}.
#' @param title The heading of the block.
#' @return A \code{shiny::div()}, the "Design" section of \code{app_design_summary()}.
#' @noRd
app_design_facts <- function(items, title = "Design") {
  shiny::div(class = "fh-summary fh-summary-facts",
    shiny::tags$section(
      shiny::h3(title),
      shiny::tags$dl(class = "fh-stats", lapply(items, function(item) {
        shiny::div(shiny::tags$dt(app_summary_term(item$label, item$tip)), shiny::tags$dd(item$value))
      }))))
}

#' Field book columns the preview leaves out: the row number, and a column
#' that repeats one of the design's parameters on every plot
#' @noRd
app_summary_repeated <- function(info) {
  c("ID", if (!is.null(info$check_direction)) "CHECK_DIRECTION")
}

#' A few names, and how many more there are
#' @noRd
app_summary_names <- function(names, show = 3L) {
  more <- length(names) - show
  paste0(paste(utils::head(names, show), collapse = ", "), if (more > 0) paste0(" and ", more, " more"))
}

#' What the efficiency columns mean
#' @noRd
app_summary_tips <- function() {
  c(`D-efficiency` = "Precision kept for all treatment comparisons taken together. 1 is the best possible.",
    `A-efficiency` = "Average precision kept for comparisons between pairs of treatments. 1 is the best possible.",
    `A-bound` = "The highest A-efficiency a design of this size can reach.")
}

#' One line under "Design ready": the size of the experiment
#' @noRd
app_summary_headline <- function(info, book) {
  locations <- if (is.null(info$NumberLocations)) 1 else info$NumberLocations
  parts <- c(
    paste(nrow(book), "plots"),
    if (!is.null(info$NumberTreatments) && !is.null(info$Reps)) {
      paste(info$NumberTreatments, "treatments in", info$Reps, "replicates")
    },
    paste(locations, if (locations == 1) "location" else "locations"))
  paste(parts, collapse = ", ")
}

#' A label, with its explanation as a tooltip that the keyboard reaches too
#' @noRd
app_summary_term <- function(label, tip = NULL) {
  if (is.null(tip)) return(label)
  shiny::span(class = "fh-tip", tabindex = "0", `data-fh-tip` = NA, title = tip, label)
}

#' A small button that copies a value (design-summary.js)
#' @noRd
app_summary_copy <- function(value) {
  shiny::tags$button(type = "button", class = "fh-copy", `data-fh-copy` = value,
    `aria-label` = paste("Copy", value), title = "Copy", shiny::icon("copy", verify_fa = FALSE))
}

#' The parameters of a design, with the labels a person would use
#' @noRd
app_summary_items <- function(info, book, field = FALSE) {
  # The designs name the same things differently: one label for each
  labels <- c(NumberTreatments = "Treatments", numberofTreatments = "Treatments", number.of.treatments = "Treatments", treatments = "Treatments",
    Reps = "Replicates", reps = "Replicates", blocks = "Blocks",
    iBlocks = "Blocks per replicate", IBlocks = "Blocks per replicate",
    rows = "Rows", columns = "Columns", nrows = "Rows", ncols = "Columns", entries = "Entries",
    nTrt = "Treatments", nRows = "Rows", checks = "Checks", fillers = "Filler plots",
    NumberLocations = "Locations", locations = "Locations", l = "Locations", lambda = "Lambda",
    planter = "Plot order", percent_checks = "Plots with checks", min_distance = "Minimum distance",
    optimization = "Optimization", locationName = "Location")
  tips <- c(lambda = "How often two treatments share an incomplete block, on average.",
    min_distance = "The smallest distance between two plots of the same entry.",
    fillers = "Plots left without an entry so the field is a full rectangle.",
    iBlocks = "Incomplete blocks in each replicate.", IBlocks = "Incomplete blocks in each replicate.")
  # Shown on its own (the seed) or not for people (the design's internal id)
  skip <- c("seed", "id_design", "Locations", "locationNames", "location_names", "plotNumber")
  # The Augmented Row-Column also keeps its mathematical sizes (v, s, k):
  # the rows, the columns and the number of checks again, each shown once
  if (identical(info$id_design, "augmented_row_column")) skip <- c(skip, "v", "s", "k")
  # Checks given by name are counted, and named under the count
  check_names <- if (is.character(info$checks) && length(info$checks) > 1L) info$checks
  if (!is.null(check_names)) info$checks <- length(check_names)
  shown <- Filter(function(name) is.atomic(info[[name]]) && length(info[[name]]) == 1L,
                  setdiff(names(info), skip))
  shown <- c(intersect(names(labels), shown), setdiff(shown, names(labels)))
  value <- function(v) if (is.numeric(v)) format(round(v, 3), scientific = FALSE, trim = TRUE) else as.character(v)
  locations <- c("NumberLocations", "locations", "l")
  # A parameter without a label keeps its name, written as words
  words <- function(name) {
    name <- gsub("[._]+", " ", name)
    paste0(toupper(substr(name, 1L, 1L)), substr(name, 2L, nchar(name)))
  }
  items <- lapply(shown, function(name) {
    list(label = if (name %in% names(labels)) labels[[name]] else words(name),
         tip = if (name %in% names(tips)) tips[[name]], value = value(info[[name]]),
         names = if (name %in% locations && "LOCATION" %in% names(book)) {
           unique(as.character(book$LOCATION))
         } else if (name == "checks") check_names)
  })
  blocks <- c(info$iBlocks, info$IBlocks)
  if (length(blocks) == 1L && !is.null(info$NumberTreatments)) {
    items <- append(items, list(list(label = "Plots per block",
      value = value(info$NumberTreatments / blocks))), after = which(shown %in% c("iBlocks", "IBlocks")))
  }
  if (isTRUE(field) && all(c("ROW", "COLUMN") %in% names(book))) {
    extent <- function(v) max(as.numeric(as.character(v)))
    # after the treatments and the replicates, before where the field is
    after <- max(0L, which(vapply(items, `[[`, "", "label") %in% c("Treatments", "Replicates")))
    items <- append(items, list(
      list(label = "Plots", value = value(nrow(book))),
      list(label = "Field", value = paste(extent(book$ROW), "\u00d7", extent(book$COLUMN)),
           tip = "Rows by columns of the layout in view.")), after = after)
  }
  items
}

#' The efficiency table with named block levels
#' @noRd
app_summary_efficiency <- function(model, block_levels = NULL) {
  names(model) <- sub("^Level$", "Block level", sub("A-Bound", "A-bound",
    sub("-Efficiency", "-efficiency", names(model))))
  # Two numbered levels are the replicates and the incomplete blocks in them
  if (is.null(block_levels) && is.numeric(model[[1L]]) && nrow(model) == 2L) {
    block_levels <- c("Replicates", "Incomplete blocks")
  }
  if (length(block_levels) == nrow(model)) model[[1L]] <- block_levels
  numbers <- vapply(model, is.double, TRUE) & names(model) != "Blocks"
  model[numbers] <- lapply(model[numbers], function(v) {
    ifelse(is.na(v), "\u2013", formatC(v, digits = 4, format = "f"))
  })
  model[] <- lapply(model, function(v) ifelse(is.na(v), "\u2013", as.character(v)))
  model
}

#' A data frame as a plain table, its column names explained where they need it
#' @noRd
app_summary_table <- function(data) {
  tips <- app_summary_tips()
  numeric <- vapply(data, function(v) is.numeric(v) || all(grepl("^[0-9.%\u2013]+$", v)), TRUE)
  class <- function(i) if (numeric[[i]]) "fh-num-cell"
  shiny::tags$table(class = "fh-table",
    shiny::tags$thead(shiny::tags$tr(lapply(seq_along(data), function(i) {
      name <- names(data)[[i]]
      shiny::tags$th(class = class(i), scope = "col",
                     app_summary_term(name, if (name %in% names(tips)) tips[[name]]))
    }))),
    shiny::tags$tbody(lapply(seq_len(nrow(data)), function(r) {
      shiny::tags$tr(lapply(seq_along(data), function(i) {
        shiny::tags$td(class = class(i), as.character(data[[i]][[r]]))
      }))
    })))
}
