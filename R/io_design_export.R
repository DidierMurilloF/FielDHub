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
        saveRDS(x, file = file, version = 2),
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
