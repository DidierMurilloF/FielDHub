#' Augmented row-column design
#'
#' @description Arrange unreplicated test entries and \code{k} replicated
#' checks in a complete rectangular field, so that every check appears exactly
#' once in every field line along \code{check_direction}, and search the check
#' positions for an efficient design under fixed row and column effects.
#'
#' @param nrows Number of field rows, a whole number.
#' @param ncols Number of field columns, a whole number.
#' @param checks The checks: a whole-number count \code{k >= 2}, which gives
#'   the labels \code{CH1}, ..., \code{CHk}, or a character vector of at least
#'   two distinct, nonmissing, nonblank labels, used in that order. With
#'   \code{data}, it must be the count of checks.
#' @param check_direction \code{"columns"} or \code{"rows"}: the physical
#'   direction along which every check appears once per line. Required, with
#'   no default; it is never inferred from the field shape. With
#'   \code{"columns"}, each check appears once in every field column, so
#'   \code{v = nrows} and \code{s = ncols}; with \code{"rows"}, once in every
#'   field row, so \code{v = ncols} and \code{s = nrows}. The design needs
#'   \code{s <= v}.
#' @param entries The test entries, excluding checks. \code{NULL} (default)
#'   fills every test plot, \code{nrows * ncols - k * s} of them, with the
#'   labels \code{G(k+1)}, ..., \code{G(k+n)}. A whole-number count must equal
#'   that number of test plots and gives the same labels. A character vector
#'   of distinct, nonmissing, nonblank labels must have that length. Must be
#'   \code{NULL} when \code{data} is given.
#' @param objective The criterion the search optimizes: \code{"V_tt"}
#'   (minimize the mean variance of test-versus-test comparisons),
#'   \code{"V_tc"} (minimize the mean variance of test-versus-check
#'   comparisons) or \code{"E_aug"} (maximize the harmonic-mean efficiency of
#'   all treatment comparisons). The default is \code{"V_tt"}.
#'   Pass \code{objective} explicitly when a result must be reproducible
#'   across package versions; reproduction code always states it.
#' @param method Search method. Only \code{"staged"} (staged annealing with
#'   recolouring restarts) is available.
#' @param budget Objective-function calls allowed per location, a positive
#'   whole number. \code{NULL} (default) uses 60,000 calls when
#'   \code{v * s <= 600} and 100,000 above. Pass \code{budget}
#'   explicitly when a result must be reproducible across package versions.
#'   The recorded parameters keep the effective value.
#' @param time_limit Optional per-location search cutoff, in seconds of
#'   wall-clock time, or \code{NULL} (default) for none. See "Search, budget
#'   and time limit" below.
#' @param l Number of locations, each an independent run of the same search.
#' @param plotNumber First plot number: one positive whole number, used at
#'   every location, or one per location.
#' @param planter Plot-number path, \code{"serpentine"} or
#'   \code{"cartesian"}.
#' @param locationNames Distinct nonblank location labels, one per location;
#'   \code{NULL} gives \code{1}, ..., \code{l}.
#' @param exptName Experiment name recorded in the \code{EXPT} column;
#'   \code{NULL} gives \code{"Expt1"}.
#' @param year Year recorded in the \code{YEAR} column; \code{NULL} gives the
#'   current calendar year. The effective year is recorded, so a later replay
#'   reproduces it.
#' @param seed Randomization seed; \code{NULL} draws and records one
#'   automatic seed.
#' @param data Optional entry table with exactly the columns \code{ENTRY} and
#'   \code{NAME}, as in \code{diagonal_arrangement()}: the first \code{checks}
#'   rows are the checks and the remaining rows the tests. \code{ENTRY} holds
#'   distinct positive whole numbers and \code{NAME} distinct, nonblank text
#'   labels. It must have \code{k} rows plus one row per test plot.
#'
#' @details
#' \strong{Construction.} The design is a complete \code{nrows x ncols}
#' field. Every test entry occupies one plot and every check occupies
#' \code{s} plots: exactly one in each of the \code{s} field lines along
#' \code{check_direction}. Along the \code{v} cross-lines, no check is
#' repeated and every cross-line holds between \code{floor(k * s / v)} and
#' \code{ceiling(k * s / v)} check plots, at least one. This is the
#' restricted contraction construction of Piepho and Williams (2026); square
#' fields (\code{v = s}) include the square-array designs of Bailey and Haines
#' (2025). Field coordinates and plot numbers are fixed at construction.
#'
#' \strong{Model and objectives.} Under the additive model with fixed row,
#' column and entry effects and independent errors, the residual degrees of
#' freedom are \code{k * s - k - v - s + 2}. Fields with fewer than 1 are
#' rejected. Fields with fewer than 10 raise a warning: only adding a check
#' or changing the number of rows or columns raises them. \code{E_aug},
#' \code{V_tt} and \code{V_tc} are reported for every location, whichever is
#' optimized.
#'
#' \strong{Search, budget and time limit.} FielDHub searches the check
#' positions with staged annealing; the search is FielDHub's own method, not
#' part of the published construction. Each location runs one search with
#' its own seed (the recorded seed plus the location index minus one) and at
#' most \code{budget} objective calls. \code{time_limit}, when given, bounds
#' each location's search phase only: final verification, randomization and
#' field-book assembly always run to completion, and with \code{l} locations
#' the call can take about \code{l * time_limit} seconds plus that work. A
#' location stopped by the time limit records \code{time_limit_hit = TRUE} in
#' its diagnostics. A cutoff reached before the search has produced any
#' design raises a \code{fieldhub_arc_search_error}, as does a search that
#' finds no connected start; there is no silent fallback to another method.
#'
#' \strong{Randomization.} After the search, the field lines and cross-lines
#' are permuted at random, the check labels receive one random permutation,
#' and the tests are assigned to the test plots at random. None of these
#' steps changes an objective. The returned \code{contraction} and
#' \code{statistics} describe this final, randomized placement.
#'
#' @return A schema-1 \code{FielDHub} result with class
#'   \code{fieldhub_augmented_row_column} and the elements:
#' \itemize{
#'   \item \code{infoDesign}: \code{nrows}, \code{ncols},
#'   \code{check_direction}, the mathematical \code{v}, \code{s} and
#'   \code{k}, the check labels \code{checks}, the test count
#'   \code{entries}, \code{locations}, \code{locationNames},
#'   \code{residual_df}, \code{objective}, \code{method}, the effective
#'   \code{budget}, \code{seed} and \code{id_design}.
#'   \item \code{fieldBook}: \code{ID}, \code{EXPT}, \code{LOCATION},
#'   \code{YEAR}, \code{PLOT}, \code{ROW}, \code{COLUMN}, \code{CHECKS},
#'   \code{ENTRY}, \code{TREATMENT} and \code{CHECK_DIRECTION}. \code{ROW}
#'   and \code{COLUMN} are physical field coordinates; \code{CHECKS} is the
#'   check's \code{ENTRY} on check plots and 0 on test plots;
#'   \code{TREATMENT} is the label, unchanged.
#'   \item \code{statistics}: one row per location with \code{LOCATION},
#'   \code{E_aug}, \code{V_tt}, \code{V_tc}, \code{V_cc}, \code{max_tt},
#'   \code{max_tc}, \code{residual_df}, \code{authority} and
#'   \code{lambda_min_rel}, from the final scoring of the returned placement.
#'   \item \code{diagnostics}: per location, the location and its seed, the
#'   search status and phase statuses, objective and proxy calls, proposals,
#'   failed proposals, disconnected candidates and \code{time_limit_hit}.
#'   \item \code{contraction}: per location, the \code{k x s} matrix whose
#'   entry \code{(h, j)} is the cross-line holding check \code{h} in field
#'   line \code{j} (field column \code{j} for \code{"columns"}, field row
#'   \code{j} for \code{"rows"}), with rows in check-label order.
#'   \item \code{timing}: per location, search, verification and total
#'   elapsed seconds, kept apart from the deterministic content.
#'   \item \code{metadata}: the seed, RNG settings, package version and the
#'   complete effective \code{parameters}.
#' }
#'
#' @section Reproducibility:
#' \code{reproduce_design(x)} and
#' \code{do.call(augmented_row_column, x$metadata$parameters)} replay the
#' design from its recorded parameters. Replay is exact (identical field book,
#' contraction, statistics, diagnostics and parameters; only \code{timing}
#' and the recorded package version are excluded) for evaluation-budget runs
#' that did not hit \code{time_limit}, within the same software and numerical
#' environment: the same FielDHub and R versions, BLAS/LAPACK library and
#' thread count, and platform. A \code{time_limit} cutoff makes replay
#' timing-dependent. In a different environment, replay gives a valid design
#' with comparable statistics, which may be a different design.
#'
#' Standalone reproduction code is available for the standard entry template
#' at any size, and for any upload up to 200 rows. An upload whose
#' \code{ENTRY} values are exactly \code{1, ..., k + n} in row order is
#' recorded as its check and test labels, with \code{data = NULL}; other
#' uploads are recorded as uploaded, and above 200 rows the reproduction code
#' falls back to the saved result (RDS).
#'
#' @references
#' Piepho, H.-P., and Williams, E. R. (2026). Rectangular augmented
#' row-column designs generated from contractions. \emph{Biometrical
#' Journal}, 68(5), e70182. \doi{10.1002/bimj.70182}
#'
#' Bailey, R. A., and Haines, L. M. (2025). Square array designs for
#' unreplicated test treatments with replicated controls. \emph{Journal of
#' Agricultural, Biological and Environmental Statistics}.
#' \doi{10.1007/s13253-025-00699-1}
#'
#' @seealso \code{\link{diagonal_arrangement}}, \code{\link{reproduce_design}},
#'   \code{\link{design_arguments}}
#'
#' @examples
#' # 4 checks and 48 tests in a 10 x 8 field; every check appears once in
#' # each field column. A small budget keeps the example fast.
#' x <- augmented_row_column(nrows = 10, ncols = 8, checks = 4,
#'                           check_direction = "columns", budget = 2000,
#'                           seed = 2026)
#' x$infoDesign$residual_df
#' x$statistics
#' head(x$fieldBook)
#' # Check plots per field column: each check once in every column
#' checks <- x$fieldBook[x$fieldBook$CHECKS > 0, ]
#' table(checks$TREATMENT, checks$COLUMN)
#'
#' # The same field turned through 90 degrees, with labelled checks
#' y <- augmented_row_column(nrows = 8, ncols = 10,
#'                           checks = c("Check A", "Check B", "Check C",
#'                                      "Check D"),
#'                           check_direction = "rows", objective = "E_aug",
#'                           budget = 2000, seed = 7)
#' y$contraction
#'
#' # Replay from the recorded parameters
#' identical(reproduce_design(x)$fieldBook, x$fieldBook)
#' @export
augmented_row_column <- function(nrows, ncols, checks, check_direction,
                                 entries = NULL,
                                 objective = arc_design_defaults()$objective,
                                 method = "staged", budget = NULL,
                                 time_limit = NULL, l = 1, plotNumber = 101,
                                 planter = "serpentine", locationNames = NULL,
                                 exptName = NULL, year = NULL, seed = NULL,
                                 data = NULL) {
  call <- sys.call()
  # Input rules of sections 3.2 and 3.3, all before resolve_seed(): no
  # rejection draws a random number
  if (missing(nrows)) {
    arc_api_abort("`nrows` is required.", argument = "nrows", call = call)
  }
  if (missing(ncols)) {
    arc_api_abort("`ncols` is required.", argument = "ncols", call = call)
  }
  if (missing(checks)) {
    arc_api_abort("`checks` is required.", argument = "checks", call = call)
  }
  validate_iteration_budget(nrows, "nrows")
  validate_iteration_budget(ncols, "ncols")
  validate_locations(l)
  validate_design_size(c(nrows, ncols, l))
  orientation <- arc_orient(nrows, ncols, check_direction)
  v <- orientation$v
  s <- orientation$s
  arc_api_choice(method, "method", arc_design_defaults()$methods, call)
  arc_api_choice(objective, "objective", arc_search_objectives, call)
  labels <- arc_api_labels(checks, entries, data, v, s, objective, call)
  k <- length(labels$checks)
  n_tests <- length(labels$tests)
  df <- arc_df_formula(v, s, k)

  if (is.null(budget)) {
    budget <- arc_default_budget(v, s)
  } else {
    validate_iteration_budget(budget, "budget")
  }
  if (!is.null(time_limit) &&
      (!is.numeric(time_limit) || length(time_limit) != 1L ||
       !is.null(dim(time_limit)) || !is.finite(time_limit) ||
       time_limit <= 0)) {
    arc_api_abort("`time_limit` must be NULL or one positive, finite number ",
                  "of seconds.", argument = "time_limit", call = call)
  }
  validate_planter(planter)
  validate_plot_starts(plotNumber)
  if (!length(plotNumber) %in% c(1L, l) || any(plotNumber < 1) ||
      any(plotNumber + nrows * ncols - 1 > .Machine$integer.max)) {
    arc_api_abort("`plotNumber` must give one positive start, or one per ",
                  "location, with the final plot in range.",
                  argument = "plotNumber", call = call)
  }
  if (length(plotNumber) == 1L) plotNumber <- rep(plotNumber, l)
  if (is.null(locationNames)) locationNames <- seq_len(l)
  if (length(locationNames) != l) {
    arc_api_abort("`locationNames` must contain one label per location.",
                  argument = "locationNames", call = call)
  }
  validate_location_labels(locationNames, l)
  if (is.null(exptName)) exptName <- "Expt1"
  if (length(exptName) != 1L) {
    arc_api_abort("`exptName` must be one experiment name.",
                  argument = "exptName", call = call)
  }
  validate_entry_labels(exptName, "exptName")
  year <- resolve_year(year)
  if (df < 10L) {
    arc_warn_low_df(df, list(df_formula = df, lambda_min_rel = NA_real_,
                             v = v, s = s, k = k), call = call)
  }

  seed <- resolve_seed(seed)
  local_design_seed(seed)
  setup <- list(orientation = orientation, v = v, s = s, k = k,
                n_tests = n_tests, labels = labels, objective = objective,
                budget = budget,
                seconds = if (is.null(time_limit)) Inf else time_limit,
                planter = planter, exptName = exptName, year = year)
  runs <- lapply(seq_len(l), function(i) {
    arc_api_location(setup, location = locationNames[i],
                     plot_start = plotNumber[i],
                     seed = offset_design_seed(seed, i - 1L))
  })
  sites <- as.character(locationNames)
  pick <- function(name) stats::setNames(lapply(runs, `[[`, name), sites)
  field_book <- do.call(rbind, lapply(runs, `[[`, "field_book"))
  rownames(field_book) <- NULL
  output <- list(
    infoDesign = list(
      nrows = orientation$nrows, ncols = orientation$ncols,
      check_direction = orientation$check_direction, v = v, s = s, k = k,
      checks = labels$checks, entries = n_tests, locations = l,
      locationNames = locationNames, residual_df = df, objective = objective,
      method = method, budget = budget, seed = seed,
      id_design = "augmented_row_column"
    ),
    fieldBook = field_book,
    statistics = arc_api_table(locationNames, runs, "statistics"),
    diagnostics = pick("diagnostics"),
    contraction = pick("contraction"),
    timing = arc_api_table(locationNames, runs, "timing")
  )
  parameters <- list(
    nrows = nrows, ncols = ncols, checks = labels$record$checks,
    check_direction = check_direction, entries = labels$record$entries,
    objective = objective, method = method, budget = budget,
    time_limit = time_limit, l = l, plotNumber = plotNumber,
    planter = planter, locationNames = locationNames, exptName = exptName,
    year = year, seed = seed, data = labels$record$data
  )
  new_fieldhub_design(output, "augmented_row_column", parameters)
}

#' Defaults of augmented_row_column()
#'
#' Frozen with method freeze m2-method-r1-20261103: the
#' default objective, the accepted search methods and the evaluation budget
#' by field size. The code reads them only from here: the `objective`
#' default of the signature, the `method` check and `arc_default_budget()`.
#' The help page states the values in words (`@param objective`, `budget`
#' and `method`); update it with them.
#' @return A list with `objective`, `methods`, and the budget rule:
#'   `budget_small` calls when `v * s <= budget_cells`, `budget_large` above.
#' @noRd
arc_design_defaults <- function() {
  # frozen with m2-method-r1-20261103
  list(objective = "V_tt", methods = "staged", budget_cells = 600,
       budget_small = 60000, budget_large = 100000)
}

#' Default evaluation budget of one location
#' @param v,s Mathematical rows and columns.
#' @noRd
arc_default_budget <- function(v, s) {
  defaults <- arc_design_defaults()
  if (as.double(v) * s <= defaults$budget_cells) {
    defaults$budget_small
  } else {
    defaults$budget_large
  }
}

#' Classed input error of augmented_row_column()
#' @param ... Message pieces.
#' @param argument The argument at fault, kept in the condition data.
#' @param class Condition classes.
#' @param data Further condition data.
#' @param call The public call.
#' @noRd
arc_api_abort <- function(..., argument = NULL, call = NULL,
                          class = c("fieldhub_arc_input_error",
                                    "fieldhub_input_error"),
                          data = list()) {
  fieldhub_abort(..., class = class,
                 data = c(list(argument = argument), data), call = call)
}

#' Check a single exact choice
#' @param value Supplied value.
#' @param argument Argument name.
#' @param choices Accepted values.
#' @param call The public call.
#' @noRd
arc_api_choice <- function(value, argument, choices, call) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !value %in% choices) {
    arc_api_abort("`", argument, "` must be ",
                  paste0("\"", choices, "\"", collapse = ", "),
                  if (length(choices) == 1L) ", the only one available",
                  ".", argument = argument, call = call,
                  data = list(options = choices))
  }
  invisible(value)
}

#' Check distinct, nonblank labels
#' @noRd
arc_api_distinct <- function(labels, argument, call) {
  validate_entry_labels(labels, argument)
  if (anyDuplicated(labels) > 0L) {
    arc_api_abort("`", argument, "` must contain distinct labels; repeated: ",
                  paste0("\"", unique(labels[duplicated(labels)]), "\"",
                         collapse = ", "), ".",
                  argument = argument, call = call)
  }
  invisible(labels)
}

#' Resolve the checks, tests and entry identifiers (section 3.2)
#'
#' Also applies the dimension and degrees-of-freedom rules of section 3.3,
#' which need the number of checks, and the upload normalization of section
#' 5: an upload whose ENTRY values are exactly 1..(k + n) in row order is
#' recorded as its check and test labels with `data = NULL`.
#' @param checks,entries,data The public arguments.
#' @param v,s Mathematical rows and columns.
#' @param objective A validated objective name.
#' @param call The public call.
#' @return A list with character `checks` and `tests`, `entry_ids` (NULL or
#'   the uploaded ENTRY values), `check_ids` (NULL or the check identifiers
#'   in input order) and `record`, the recorded `checks`, `entries` and
#'   `data`.
#' @noRd
arc_api_labels <- function(checks, entries, data, v, s, objective, call) {
  if (is.numeric(checks) && length(checks) > 1L) {
    arc_api_abort("`checks` must be a count or character labels; a numeric ",
                  "vector of entry identifiers is not accepted.",
                  argument = "checks", call = call)
  }
  if (is.numeric(checks)) {
    validate_iteration_budget(checks, "checks", minimum = 2)
    k <- as.integer(checks)
    check_labels <- paste0("CH", seq_len(k))
  } else if (is.character(checks) && is.null(dim(checks))) {
    if (!is.null(data)) {
      arc_api_abort("With `data`, `checks` must be the number of checks: the ",
                    "first `checks` rows of `data` are the checks, and their ",
                    "labels come from data$NAME.", argument = "checks",
                    call = call)
    }
    if (length(checks) < 2L) {
      arc_api_abort("`checks` must name at least two checks.",
                    argument = "checks", call = call)
    }
    arc_api_distinct(checks, "checks", call)
    k <- length(checks)
    check_labels <- checks
  } else {
    arc_api_abort("`checks` must be a whole-number count of at least 2 or a ",
                  "character vector of at least two labels.",
                  argument = "checks", call = call)
  }

  status <- arc_search_entry(v, s, k, objective)
  if (identical(status, "invalid_dimensions")) {
    arc_api_abort("Every field line along check_direction holds all ", k,
                  " checks and at least one test, so the number of checks ",
                  "must be less than v = ", v, " (the plots in each line). ",
                  "Use fewer checks, or change the numbers of rows and ",
                  "columns.", argument = "checks", call = call,
                  class = c("fieldhub_arc_dimension_error",
                            "fieldhub_dimension_error"),
                  data = list(v = v, s = s, k = k))
  }
  df <- arc_df_formula(v, s, k)
  if (df < 1L) {
    fix <- paste0("Add a check, or change the numbers of rows and columns.")
    if (identical(status, "infeasible_df")) {
      arc_api_abort("No connected design exists for v = ", v, ", s = ", s,
                    " and k = ", k, ": it would have ks - k - v - s + 2 = ",
                    df, " residual degrees of freedom. ", fix,
                    call = call,
                    class = c("fieldhub_arc_infeasible_error",
                              "fieldhub_dimension_error"),
                    data = list(df_formula = df, v = v, s = s, k = k))
    }
    arc_api_abort("A design with v = ", v, ", s = ", s, " and k = ", k,
                  " has 0 residual degrees of freedom, so the residual ",
                  "variance cannot be estimated; at least 1 is required. ",
                  fix, call = call,
                  class = c("fieldhub_arc_df_error",
                            "fieldhub_dimension_error"),
                  data = list(df_formula = df, v = v, s = s, k = k))
  }

  n_cells <- (v - k) * s
  count_error <- function(argument, what, supplied) {
    arc_api_abort("`", argument, "` ", what, " ", supplied, ", but the field ",
                  "has ", n_cells, " test plots (", v * s, " plots minus ",
                  k * s, " check plots).", argument = argument, call = call,
                  data = list(supplied = supplied, required = n_cells))
  }
  if (!is.null(data)) {
    if (!is.null(entries)) {
      arc_api_abort("Supply the test entries either in `entries` or in ",
                    "`data`, not both.", argument = "entries", call = call)
    }
    if (!is.data.frame(data) || ncol(data) != 2L ||
        !setequal(names(data), c("ENTRY", "NAME"))) {
      arc_api_abort("`data` must be a data frame with exactly the columns ",
                    "ENTRY and NAME.", argument = "data", call = call)
    }
    ids <- data[["ENTRY"]]
    if (!is.numeric(ids) || is.complex(ids) || anyNA(ids) ||
        any(!is.finite(ids)) || any(ids != round(ids)) || any(ids < 1) ||
        any(ids > .Machine$integer.max) || anyDuplicated(ids) > 0L) {
      arc_api_abort("data$ENTRY must hold distinct positive whole numbers.",
                    argument = "data", call = call)
    }
    names_column <- data[["NAME"]]
    if (is.factor(names_column)) names_column <- as.character(names_column)
    if (!is.character(names_column)) {
      arc_api_abort("data$NAME must hold text labels; convert numbers with ",
                    "as.character().", argument = "data", call = call)
    }
    arc_api_distinct(names_column, "data$NAME", call)
    if (nrow(data) != k + n_cells) {
      count_error("data", "has", paste(nrow(data) - k, "test rows"))
    }
    check_labels <- names_column[seq_len(k)]
    test_labels <- names_column[-seq_len(k)]
    standard <- isTRUE(all(ids == seq_len(k + n_cells)))
    if (standard) {
      return(list(checks = check_labels, tests = test_labels,
                  entry_ids = NULL, check_ids = NULL,
                  record = list(checks = check_labels, entries = test_labels,
                                data = NULL)))
    }
    return(list(checks = check_labels, tests = test_labels, entry_ids = ids,
                check_ids = ids[seq_len(k)],
                record = list(checks = checks, entries = NULL, data = data)))
  }

  if (is.null(entries)) {
    test_labels <- paste0("G", k + seq_len(n_cells))
  } else if (is.character(entries) && is.null(dim(entries))) {
    arc_api_distinct(entries, "entries", call)
    if (length(entries) != n_cells) {
      count_error("entries", "has", paste(length(entries), "labels"))
    }
    test_labels <- entries
  } else if (is.numeric(entries) && length(entries) == 1L) {
    validate_iteration_budget(entries, "entries")
    if (entries != n_cells) count_error("entries", "is", entries)
    test_labels <- paste0("G", k + seq_len(n_cells))
  } else {
    arc_api_abort("`entries` must be NULL, a whole-number count or a ",
                  "character vector of labels.", argument = "entries",
                  call = call)
  }
  shared <- intersect(check_labels, test_labels)
  if (length(shared) > 0L) {
    arc_api_abort("Check and test labels must differ, so that TREATMENT ",
                  "identifies one entry; used for both: ",
                  paste0("\"", shared, "\"", collapse = ", "), ".",
                  argument = "entries", call = call,
                  data = list(labels = shared))
  }
  list(checks = check_labels, tests = test_labels, entry_ids = NULL,
       check_ids = NULL,
       record = list(checks = checks, entries = entries, data = NULL))
}

#' Search, randomize and assemble one location (sections 3.4-3.6)
#'
#' The search seeds itself with `seed`; as a nested design call it leaves
#' the stream where it ends, so the randomization continues the location's
#' own stream (section 3.5).
#' @param setup Validated design inputs.
#' @param location Location label.
#' @param plot_start First plot number.
#' @param seed Location seed.
#' @return A list with `field_book`, `contraction`, `statistics`,
#'   `diagnostics` and `timing`.
#' @noRd
arc_api_location <- function(setup, location, plot_start, seed) {
  started <- arc_clock()
  v <- setup$v
  s <- setup$s
  k <- setup$k
  run <- arc_search_staged(v, s, k, setup$objective,
                           max_evals = setup$budget, seconds = setup$seconds,
                           seed = seed)
  limit_hit <- is.finite(setup$seconds) &&
    (isTRUE(run$search_secs >= setup$seconds) ||
       "time_limit" %in% unlist(run$phases))
  if (!identical(run$status, "ok")) {
    arc_api_search_failure(run, location, seed, limit_hit, setup)
  }
  placement <- arc_randomize(run$design, v, s, k, setup$n_tests)
  field_book <- arc_field_book(placement, setup$orientation,
                               setup$labels$checks, setup$labels$tests,
                               entry_ids = setup$labels$entry_ids,
                               location = location, expt = setup$exptName,
                               year = setup$year, plot_start = plot_start,
                               planter = setup$planter)
  contraction <- arc_contraction_from_field_book(
    field_book, k, setup$orientation$check_direction,
    check_ids = setup$labels$check_ids
  )
  scoring <- arc_clock()
  final <- arc_final_score(contraction, v, "restricted")
  arc_api_check_invariance(final, run$final, location)
  finished <- arc_clock()
  list(
    field_book = field_book,
    contraction = contraction,
    statistics = list(E_aug = final$E_aug, V_tt = final$V_tt,
                      V_tc = final$V_tc, V_cc = final$V_cc,
                      max_tt = final$max_tt, max_tc = final$max_tc,
                      residual_df = as.integer(final$df_formula),
                      authority = final$authority,
                      lambda_min_rel = final$lambda_min_rel),
    diagnostics = list(location = location, seed = seed, status = run$status,
                       phases = run$phases,
                       objective_calls = run$objective_calls,
                       proxy_calls = run$proxy_calls,
                       proposals = run$proposals,
                       failed_proposals = run$failed_proposals,
                       disconnected = run$disconnected,
                       time_limit_hit = limit_hit),
    timing = list(search = run$search_secs,
                  verification = run$verify_secs + (finished - scoring),
                  total = finished - started)
  )
}

#' Raise the classed error of a search that produced no design
#' @noRd
arc_api_search_failure <- function(run, location, seed, limit_hit, setup) {
  reason <- if (identical(run$status, "start_search_exhausted")) {
    paste0("no connected starting design was found within the bounded ",
           "number of start tries. Try another seed, or change the number ",
           "of checks or the field dimensions.")
  } else if (limit_hit) {
    paste0("the time limit of ", setup$seconds, " seconds stopped the ",
           "search before it produced a design. Increase time_limit, or ",
           "leave it NULL to stop by the evaluation budget only.")
  } else if (identical(run$status, "no_design")) {
    paste0("the budget of ", setup$budget, " objective calls was used ",
           "before the search produced a design. Increase budget.")
  } else {
    paste0("the search ended with status \"", run$status, "\".")
  }
  fieldhub_abort("No augmented row-column design was found for location ",
                 location, ": ", reason,
                 class = c("fieldhub_arc_search_error",
                           "fieldhub_search_error"),
                 data = list(location = location, seed = seed,
                             status = run$status, phases = run$phases,
                             time_limit_hit = limit_hit),
                 call = NULL)
}

#' Check that randomization left the objectives unchanged
#'
#' M1 derivation 7: permuting lines, cross-lines and labels changes no
#' objective, so the final placement must score as the search output did.
#' @param final,searched `arc_final_score()` results.
#' @param location Location label.
#' @noRd
arc_api_check_invariance <- function(final, searched, location) {
  metrics <- c("E_aug", "V_tt", "V_tc")
  same <- isTRUE(final$connected) && isTRUE(searched$connected) &&
    all(vapply(metrics, function(metric) {
      a <- final[[metric]]
      b <- searched[[metric]]
      is.finite(a) && is.finite(b) && abs(a - b) <= 1e-8 * max(1, abs(b))
    }, logical(1)))
  if (!same) {
    fieldhub_abort("Internal error: the randomized design at location ",
                   location, " does not score as the search output did.",
                   class = c("fieldhub_arc_internal_error",
                             "fieldhub_internal_error"),
                   data = list(final = final, searched = searched),
                   call = NULL)
  }
  invisible(TRUE)
}

#' One data frame row per location from a named list element of each run
#' @param locationNames Location labels, in location order.
#' @param runs Per-location results of `arc_api_location()`.
#' @param name The element: a named list of single values.
#' @noRd
arc_api_table <- function(locationNames, runs, name) {
  rows <- lapply(runs, function(run) {
    as.data.frame(run[[name]], stringsAsFactors = FALSE)
  })
  table <- data.frame(LOCATION = locationNames, do.call(rbind, rows),
                      stringsAsFactors = FALSE, check.names = FALSE)
  rownames(table) <- NULL
  table
}
