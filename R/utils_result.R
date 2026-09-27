#' Version of the structure of the design results
#'
#' @description Raised when the structure of the results changes, so that
#' code reading saved results can tell which structure it has.
#' @noRd
fieldhub_schema_version <- 1L

#' Reproducibility metadata shared by field designs and allocation plans
#' @noRd
fieldhub_metadata <- function(design, seed) {
  list(
    design = design,
    schema_version = fieldhub_schema_version,
    seed = seed,
    rng_kind = RNGkind(),
    package_version = as.character(utils::packageVersion("FielDHub"))
  )
}

#' Build the result of a design function
#'
#' @description Field designs and family splits return their result through
#' this constructor; allocation plans use \code{new_fieldhub_allocation()}.
#' It adds a \code{metadata} element and gives the result the
#' class \code{c("fieldhub_<design>", "FielDHub")}, so that \code{print()},
#' \code{summary()} and \code{plot()} dispatch on the design.
#'
#' @param x List with the elements of the result, including
#'   \code{infoDesign}.
#' @param design Name of the design, such as \code{"rcbd"}.
#'
#' @return The result, with the element \code{metadata}: a list with
#'   \code{design}, \code{schema_version}, \code{seed}, \code{rng_kind} (the
#'   \code{RNGkind()} used) and \code{package_version}.
#' @noRd
new_fieldhub_design <- function(x, design) {
  if (!is.list(x) || !is.list(x$infoDesign)) {
    fieldhub_abort("Internal error: the design result must be a list with infoDesign.",
                   class = "fieldhub_internal_error", call. = FALSE)
  }
  x$metadata <- fieldhub_metadata(design, x$infoDesign$seed)
  class(x) <- c(paste0("fieldhub_", design), "FielDHub")
  validate_fieldhub_design(x)
}

#' Designs of the id_design values of results saved by FielDHub 1.5 or
#' earlier, which have the class "FielDHub" only
#' @noRd
legacy_designs <- c(
  "1" = "crd", "2" = "rcbd", "3" = "latin_square", "4" = "full_factorial",
  "5" = "split_plot", "6" = "split_split_plot", "7" = "strip_plot",
  "8" = "incomplete_blocks", "9" = "row_column", "10" = "square_lattice",
  "11" = "rectangular_lattice", "12" = "alpha_lattice",
  "13" = "partially_replicated", "14" = "rcbd_augmented",
  "15" = "diagonal_arrangement", "16" = "optimized_arrangement",
  "17" = "split_families", "Sparse" = "sparse_allocation",
  "MultiPrep" = "multi_location_prep"
)

#' Give a result saved by FielDHub 1.5 or earlier the class of its design
#'
#' @param x A design result.
#' @return \code{x}, with the class \code{c("fieldhub_<design>", "FielDHub")}
#'   when it had the class "FielDHub" only and its id_design is known.
#' @noRd
with_design_class <- function(x) {
  if (!identical(class(x), "FielDHub")) return(x)
  design <- legacy_designs[as.character(x$infoDesign$id_design)]
  if (length(design) != 1 || is.na(design)) return(x)
  class(x) <- c(paste0("fieldhub_", design), "FielDHub")
  x
}

#' Check the structure of a design result
#'
#' Field books require ID and PLOT as finite numeric vectors and LOCATION
#' as a nonmissing atomic vector. Integer and double storage are both kept,
#' preserving the established types of each design family. Extra columns
#' remain permitted.
#'
#' @param x A design result.
#' @return \code{x}, invisibly, or an error when its structure is not valid.
#' @noRd
validate_fieldhub_design <- function(x) {
  if (!is.list(x)) {
    fieldhub_abort("Internal error: the design result is not a FielDHub list.",
                   class = "fieldhub_internal_error", call. = FALSE)
  }
  problems <- character(0)
  if (!inherits(x, "FielDHub")) problems <- c(problems, "is not a FielDHub list")
  if (!is.list(x$infoDesign) || is.null(x$infoDesign$id_design)) {
    problems <- c(problems, "has no infoDesign with id_design")
  }
  meta <- x$metadata
  if (!is.list(meta) || !is.character(meta$design) || length(meta$design) != 1L ||
      is.na(meta$design) || !nzchar(trimws(meta$design))) {
    problems <- c(problems, "has no metadata naming the design")
  } else {
    if (!identical(class(x)[1], paste0("fieldhub_", meta$design))) {
      problems <- c(problems, "has a class that does not match its design")
    }
    if (!identical(meta$schema_version, fieldhub_schema_version)) {
      problems <- c(problems, "has an unknown schema version")
    }
    if (meta$design != "split_families") {
      problems <- c(problems, field_book_problems(x$fieldBook))
    }
  }
  if (length(problems) > 0) {
    fieldhub_abort("Internal error: the design result ", paste(problems, collapse = ", "), ".",
                   class = "fieldhub_internal_error", call. = FALSE)
  }
  invisible(x)
}

#' Check the common field-book keys without coercing or dropping columns
#' @noRd
field_book_problems <- function(book) {
  if (!is.data.frame(book) || nrow(book) == 0L) return("has no field book")
  problems <- character()
  if (anyDuplicated(names(book)) > 0L) {
    problems <- c(problems, "has duplicate column names in its field book")
  }
  missing <- setdiff(c("ID", "LOCATION", "PLOT"), names(book))
  if (length(missing) > 0L) {
    problems <- c(problems, paste0("has no field-book columns: ", paste(missing, collapse = ", ")))
  }
  for (name in intersect(c("ID", "PLOT"), names(book))) {
    values <- book[[name]]
    if (!is.numeric(values) || is.complex(values) || !is.null(dim(values)) ||
        length(values) != nrow(book) || any(!is.finite(values))) {
      problems <- c(problems, paste0("has a field-book ", name,
                                    " column that is not a finite numeric vector"))
    }
  }
  if ("LOCATION" %in% names(book)) {
    locations <- book[["LOCATION"]]
    if (!is.atomic(locations) || !is.null(dim(locations)) ||
        length(locations) != nrow(book) || anyNA(locations)) {
      problems <- c(problems, "has a field-book LOCATION column that is not a nonmissing atomic vector")
    }
  }
  problems
}
