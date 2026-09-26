#' Version of the structure of the design results
#'
#' @description Raised when the structure of the results changes, so that
#' code reading saved results can tell which structure it has.
#' @noRd
fieldhub_schema_version <- 1L

#' Build the result of a design function
#'
#' @description Every design function returns its result through this
#' constructor. It adds a \code{metadata} element and gives the result the
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
  x$metadata <- list(
    design = design,
    schema_version = fieldhub_schema_version,
    seed = x$infoDesign$seed,
    rng_kind = RNGkind(),
    package_version = as.character(utils::packageVersion("FielDHub"))
  )
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
#' @param x A design result.
#' @return \code{x}, invisibly, or an error when its structure is not valid.
#' @noRd
validate_fieldhub_design <- function(x) {
  problems <- character(0)
  if (!is.list(x) || !inherits(x, "FielDHub")) problems <- c(problems, "is not a FielDHub list")
  if (!is.list(x$infoDesign) || is.null(x$infoDesign$id_design)) {
    problems <- c(problems, "has no infoDesign with id_design")
  }
  meta <- x$metadata
  if (!is.list(meta) || !is.character(meta$design) || length(meta$design) != 1) {
    problems <- c(problems, "has no metadata naming the design")
  } else {
    if (!identical(class(x)[1], paste0("fieldhub_", meta$design))) {
      problems <- c(problems, "has a class that does not match its design")
    }
    if (!identical(meta$schema_version, fieldhub_schema_version)) {
      problems <- c(problems, "has an unknown schema version")
    }
    if (meta$design != "split_families" &&
        (!is.data.frame(x$fieldBook) || nrow(x$fieldBook) == 0)) {
      problems <- c(problems, "has no field book")
    }
  }
  if (length(problems) > 0) {
    fieldhub_abort("Internal error: the design result ", paste(problems, collapse = ", "), ".",
                   class = "fieldhub_internal_error", call. = FALSE)
  }
  invisible(x)
}
