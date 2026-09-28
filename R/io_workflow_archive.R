#' Validate an exported table without changing its values or column types
#' @noRd
validate_archive_table <- function(data) {
  if (!is.data.frame(data) || nrow(data) == 0L || ncol(data) == 0L ||
      anyNA(names(data)) || any(!nzchar(names(data))) || anyDuplicated(names(data)) > 0L ||
      any(!vapply(data, function(column) is.atomic(column) && is.null(dim(column)), logical(1)))) {
    fieldhub_abort("An export needs a non-empty data frame with unique column names and atomic columns.")
  }
  invisible(data)
}

#' Validate recorded layout settings without evaluating or drawing a layout
#' @noRd
validate_archive_layout <- function(layout) {
  if (is.null(layout)) return(invisible(NULL))
  if (!is.list(layout) || !identical(names(layout), c("parameters", "selected")) ||
      !is.list(layout$parameters) ||
      !identical(names(layout$parameters), c("layout", "planter", "stacked"))) {
    fieldhub_abort("The archive has invalid layout parameters.")
  }
  for (value in list(layout$selected, layout$parameters$layout)) {
    if (!is.numeric(value) || is.complex(value) || length(value) != 1L ||
        !is.finite(value) || value < 1 || value != trunc(value)) {
      fieldhub_abort("Archive layout and location selections must be positive whole numbers.")
    }
  }
  check_layout_arguments(layout$parameters$planter, layout$parameters$stacked)
  invisible(layout)
}

#' Save the exact exported data alongside the independently reproducible records
#' @noRd
new_workflow_archive <- function(design, field_book, data, simulation = NULL,
                                  layout = NULL, kind = "field_book") {
  reproduction_engine(design)
  validate_archive_table(field_book)
  validate_archive_table(data)
  if (!is.character(kind) || length(kind) != 1L || is.na(kind) ||
      !kind %in% c("field_book", "layout")) {
    fieldhub_abort("The export kind must be field_book or layout.")
  }
  if (!is.null(simulation)) simulation_reproduction_engine(simulation)
  validate_archive_layout(layout)
  imports <- utils::packageDescription("FielDHub", fields = "Imports")
  packages <- c("FielDHub", trimws(sub("[[:space:]]*\\(.*$", "", strsplit(imports, ",")[[1L]])))
  versions <- setNames(vapply(packages, function(package) {
    as.character(utils::packageVersion(package))
  }, character(1)), packages)
  list(
    schema_version = 1L, design = design, field_book = field_book,
    simulation = simulation, layout = layout, export = list(kind = kind, data = data),
    software = list(R = as.character(getRversion()), platform = R.version$platform,
                    packages = versions)
  )
}

#' Keep archive names portable and separate from filesystem paths
#' @noRd
csv_archive_filename <- function(filename) {
  if (!is.character(filename) || length(filename) != 1L || is.na(filename) ||
      !nzchar(filename) || grepl("[/\\\\[:cntrl:]]", filename) ||
      !grepl("[.]csv$", filename, ignore.case = TRUE)) {
    fieldhub_abort("Use one CSV filename without directories or control characters.")
  }
  sub("[.]csv$", ".zip", filename, ignore.case = TRUE)
}

#' Standalone design call followed by the complete saved-workflow reconstruction
#' @noRd
workflow_reproduction_code <- function(design = NULL) {
  c(
    if (!is.null(design)) design_call_section(design),
    "# Run from the directory containing workflow.rds, after extracting the ZIP.",
    "saved <- readRDS(\"workflow.rds\")",
    "design <- FielDHub::reproduce_design(saved$design)",
    "simulation <- if (!is.null(saved$simulation)) {",
    "  FielDHub::reproduce_simulation(saved$simulation)",
    "} else NULL",
    "layout <- if (!is.null(saved$layout)) {",
    "  do.call(FielDHub::field_layout,",
    "          c(list(x = design), saved$layout$parameters), quote = TRUE)",
    "} else NULL",
    "",
    "# Exact saved outputs, including app display-only types and plot IDs:",
    "field_book <- saved$field_book",
    "exported_data <- saved$export$data",
    "# Reconstructed results may differ across software versions or platforms.",
    "# Inspect saved$software for the recorded versions and platform."
  )
}

#' Write a CSV and its exact workflow context into one portable archive
#' @noRd
write_workflow_archive <- function(archive, file) {
  if (!requireNamespace("zip", quietly = TRUE)) {
    fieldhub_abort("CSV archives require the optional zip package. Install it with install.packages(\"zip\").",
                   class = "fieldhub_dependency_error", data = list(packages = "zip"))
  }
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    fieldhub_abort("The archive output must be one file path.")
  }
  directory <- tempfile("fieldhub-archive-")
  if (!dir.create(directory)) fieldhub_abort("Could not create a temporary export directory.",
                                             class = "fieldhub_export_error")
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  tryCatch({
    files <- file.path(directory, c("data.csv", "workflow.rds", "reproduce.R", "README.txt"))
    utils::write.csv(as.data.frame(archive$export$data), files[1L], row.names = FALSE)
    saveRDS(archive, files[2L], version = 2)
    writeLines(workflow_reproduction_code(archive$design), files[3L], useBytes = TRUE)
    writeLines(c(
      "FielDHub reproducible export (archive schema 1)",
      paste("Design:", archive$design$metadata$design),
      paste("Design seed:", archive$design$metadata$seed),
      paste("FielDHub version:", archive$design$metadata$package_version),
      "",
      "data.csv retains the existing CSV table format.",
      "workflow.rds stores the exact exported table and displayed field book,",
      "the core design with its parameters and seed, any simulation with its",
      "input field book and model parameters, layout settings, and software versions.",
      "reproduce.R reconstructs the design, simulation, and core layout where present.",
      "It also loads the exact saved app outputs; display-only types or plot IDs",
      "can differ from the reconstructed core layout or simulation record.",
      "Keep workflow.rds even after upgrading R or FielDHub. Only source scripts",
      "and read RDS files from trusted archives. No files are executed automatically."
    ), files[4L], useBytes = TRUE)
    zip::zipr(file, files, recurse = FALSE, include_directories = FALSE)
  }, error = function(e) {
    fieldhub_abort("Could not save the workflow archive: ", conditionMessage(e),
                   class = "fieldhub_export_error", data = list(parent = e))
  })
  invisible(NULL)
}

#' Lazy download callbacks for CSV archives, independent of Shiny
#' @noRd
csv_archive_handlers <- function(filename, data, design, field_book,
                                   simulation = function() NULL, layout = function() NULL,
                                   kind = "field_book") {
  list(
    filename = function() csv_archive_filename(filename()),
    content = function(file) {
      archive <- new_workflow_archive(design(), field_book(), data(), simulation(), layout(), kind)
      write_workflow_archive(archive, file)
    }
  )
}
