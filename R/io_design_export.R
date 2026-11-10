#' Design-specific replay notes, keyed by the design name
#'
#' The reproduction code and the app's Help show these lines for the designs
#' that have them; other designs show nothing extra.
#' @noRd
design_replay_notes <- function() {
  list(augmented_row_column = c(
    "Replay is exact for evaluation-budget runs that did not hit `time_limit`, within the same software and numerical environment (R, BLAS/LAPACK, platform).",
    "A `time_limit` cutoff makes replay timing-dependent.",
    "In a different environment, replay gives a valid design with comparable statistics, which may be a different design.",
    "Standalone code is available for the standard entry template at any size and for any upload up to 200 rows; otherwise use the saved result (RDS)."
  ))
}

#' Replay notes of one design as comment lines, or nothing
#' @noRd
design_replay_comments <- function(x) {
  notes <- design_replay_notes()[[as.character(x$metadata$design)[1L]]]
  if (is.null(notes)) return(character())
  paste("# Replay:", notes)
}

#' Reproducible design download callbacks, independent of Shiny
#'
#' The result provider is evaluated when an export is requested, not when the
#' callbacks are registered. Only the core design is exported: app-selected
#' layout coordinates and simulated responses are separate outputs.
#' @noRd
design_export_handlers <- function(design) {
  result <- function() {
    x <- design()
    reproduction_engine(x)
    x
  }
  filename <- function(x) {
    paste0("FielDHub_", x$metadata$design, "_seed_",
           format(x$metadata$seed, scientific = FALSE, trim = TRUE), ".rds")
  }
  list(
    filename = function() filename(result()),
    content = function(file) {
      x <- result()
      tryCatch(
        write_design(x, file = file, overwrite = TRUE),
        error = function(e) fieldhub_abort(
          "Could not save the design: ", conditionMessage(e),
          class = "fieldhub_export_error", data = list(parent = e)
        )
      )
      invisible(NULL)
    },
    code = function() {
      x <- result()
      paste(c(
        paste0("# FielDHub version: ", encodeString(x$metadata$package_version)),
        paste0("# Design: ", x$metadata$design, "; seed: ", x$metadata$seed),
        design_replay_comments(x),
        design_call_section(x),
        "# Save the RDS download in your R working directory, then run:",
        paste0("saved_design <- readRDS(", encodeString(filename(x), quote = '"'), ")"),
        "design <- FielDHub::reproduce_design(saved_design)",
        "",
        "# saved_design is the exact saved result, including parameters and RNG settings.",
        "# Reconstruction uses the installed software; different versions may differ.",
        "# This does not replay app layout choices or simulated responses."
      ), collapse = "\n")
    }
  )
}
