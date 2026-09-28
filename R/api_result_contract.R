#' Design results and reproducibility metadata
#'
#' @description FielDHub uses a versioned result contract. Schema 1 retains
#' the established elements and storage types of each design family; validation
#' checks those elements without coercing valid field books or removing user
#' columns. Field, family, allocation and optimization results share the same construction
#' boundary, with validation appropriate to each result type.
#'
#' @section Field designs:
#' Field designs inherit from \code{FielDHub}, with a first class such as
#' \code{fieldhub_rcbd} identifying the engine. Test inheritance with
#' \code{inherits(x, "FielDHub")}, not equality with \code{class(x)}.
#' Their \code{fieldBook} contains finite numeric \code{ID} and \code{PLOT}
#' vectors and nonmissing atomic \code{LOCATION} identifiers. Integer and
#' double storage, factors, and the existing character or numeric location
#' conventions are preserved rather than normalized.
#'
#' Additional required columns depend on the design:
#' \itemize{
#'   \item CRD and RCBD: \code{REP}, \code{TREATMENT}. RCBD with repeated
#'   checks also requires \code{ENTRY} and \code{CHECKS}.
#'   \item Latin square: \code{SQUARE}, \code{ROW}, \code{COLUMN},
#'   \code{TREATMENT}.
#'   \item Factorial: \code{REP}, \code{TRT_COMB}, and one
#'   \code{FACTOR_<name>} column per recorded factor.
#'   \item Split plot: \code{REP}, \code{WHOLE_PLOT}, \code{SUB_PLOT},
#'   \code{TRT_COMB}; split-split plot also requires \code{SUB_SUB_PLOT}.
#'   \item Strip plot: \code{REP}, \code{HSTRIP}, \code{VSTRIP},
#'   \code{TRT_COMB}.
#'   \item Incomplete blocks and lattices: \code{REP}, \code{IBLOCK},
#'   \code{UNIT}, \code{ENTRY}, \code{TREATMENT}.
#'   \item Row-column: \code{REP}, \code{ROW}, \code{COLUMN},
#'   \code{ENTRY}, \code{TREATMENT}.
#'   \item Diagonal, optimized, sparse, augmented and partially replicated:
#'   \code{EXPT}, \code{YEAR}, \code{ROW}, \code{COLUMN}, \code{CHECKS},
#'   \code{ENTRY}, \code{TREATMENT}. Augmented RCBD adds \code{BLOCK};
#'   partially replicated designs add \code{REP}.
#' }
#' Required extension columns are atomic vectors, not matrix or list columns.
#' Numeric values must be finite. Missing \code{CHECKS} values remain supported,
#' as do missing \code{REP} values on partially replicated filler plots with
#' \code{ENTRY = 0}. Other required extension values cannot be missing.
#' Extra columns are permitted. Use \code{field_layout()} to obtain the final
#' coordinates and plot numbers for a chosen planting path and layout.
#'
#' @section Family splits and allocation plans:
#' \code{split_families()} returns \code{rowsEachlist} with location counts
#' and \code{data_locations} with \code{ENTRY}, \code{NAME}, \code{FAMILY}
#' and \code{LOCATION}. Counts must agree with the entry table, including
#' locations with zero entries.
#'
#' \code{do_optim()} retains the \code{Sparse} or \code{MultiPrep} class.
#' Its allocation counts, location sizes and entry lists are validated; these
#' plans are not field books and do not yet specify plot coordinates.
#'
#' @section Standalone optimization:
#' \code{swap_pairs()} retains its matrices, distances and stopping diagnostics,
#' with the classes \code{fieldhub_pair_swap} and \code{fieldhub_optimization}.
#' Its shared metadata includes the input matrix and every optimization control.
#' The validator checks field geometry, entry counts and retained search steps.
#' These results can be saved and replayed with \code{reproduce_design()}.
#'
#' @section Reproducibility:
#' \code{metadata} records \code{design}, \code{schema_version}, \code{seed},
#' \code{rng_kind}, \code{package_version} and, in newly generated results,
#' \code{parameters}. Parameters use effective values after validation,
#' including defaults, the resolved year and seed, and uploaded data.
#' Automatic seeds are integers; explicit real-valued seeds retain the legacy
#' behavior of R's \code{set.seed()}, which truncates to an integer.
#'
#' Use \code{saveRDS()} and \code{readRDS()} to retain the exact result and
#' its metadata, then \code{reproduce_design()} to reconstruct it. Exact replay
#' of optimization may require the same dependency versions and platform.
#' Layout selections and simulated responses added later by the app are not
#' part of the core design parameters.
#'
#' Older schema-1 metadata without parameters still validates, but cannot be
#' replayed automatically. Objects saved by FielDHub 1.5.0 without metadata
#' remain supported by the print, summary and plotting compatibility methods;
#' they are not silently rewritten to the new schema.
#'
#' @seealso \code{\link{reproduce_design}}, \code{\link{field_layout}},
#'   \code{\link{design_arguments}}
#' @name design_results
NULL
