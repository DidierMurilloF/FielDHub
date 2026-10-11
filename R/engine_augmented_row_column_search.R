# Search engine for augmented row-column designs: entry validation and the
# result object (research M2 specification, revision 3, sections 2, 8.1 and
# 8.3; implementation plan task 3). The search algorithms (task 8) call
# `arc_search_entry()` first and return at once, with zero objective calls,
# when it reports a status.

#' Objectives the search supports
#'
#' Specification section 2: E_aug (maximize), mean test-vs-test variance V_tt
#' (minimize), mean test-vs-check variance V_tc (minimize).
#' @noRd
arc_search_objectives <- c("E_aug", "V_tt", "V_tc")

#' Status vocabulary of the search
#'
#' Specification section 8.1. Entry statuses, exhaustion statuses, phase
#' stops and the run statuses `ok` and `no_design`.
#' @noRd
arc_search_statuses <- c(
  "ok", "no_design",
  "unsupported_objective", "invalid_dimensions", "infeasible_df",
  "below_min_df",
  "start_search_exhausted", "sampling_exhausted", "empty_neighbourhood",
  "budget", "phase_budget", "time_limit", "proposal_cap", "local_optimum",
  "interrupted", "skipped"
)

#' Entry check shared by every search algorithm
#'
#' Specification section 2. Order: objective name, integer dimensions
#' (`k >= 2`, `k < v`, `s <= v`), then the integer degrees of freedom. It is
#' total: any input yields a status or `NA_character_`, never an error or a
#' warning. Dimensions are screened before `arc_df_formula()` so that its
#' integer arithmetic cannot overflow.
#'
#' @param v,s,k Field rows, columns and checks per column.
#' @param objective One of `arc_search_objectives`.
#' @return `NA_character_` when the entry is valid, else one of
#'   `unsupported_objective`, `invalid_dimensions`, `infeasible_df` (df < 0)
#'   or `below_min_df` (df = 0).
#' @noRd
arc_search_entry <- function(v, s, k, objective) {
  if (!is.character(objective) || length(objective) != 1L ||
      is.na(objective) || !objective %in% arc_search_objectives) {
    return("unsupported_objective")
  }
  if (!arc_is_whole_number(v) || !arc_is_whole_number(s) ||
      !arc_is_whole_number(k)) {
    return("invalid_dimensions")
  }
  v <- as.double(v)
  s <- as.double(s)
  k <- as.double(k)
  if (k < 2 || s < 1 || k >= v || s > v) {
    return("invalid_dimensions")
  }
  if (k * s > .Machine$integer.max) {
    return("invalid_dimensions")
  }
  df <- k * s - k - v - s + 2
  # Far below zero: no connected design, and integer arithmetic could wrap.
  if (df < -1e9) {
    return("infeasible_df")
  }
  df <- arc_df_formula(v, s, k)
  if (df < 0L) {
    return("infeasible_df")
  }
  if (df == 0L) {
    return("below_min_df")
  }
  NA_character_
}

#' Build the search result object
#'
#' Specification sections 8.1 and 8.3. A plain list with stable fields:
#' `design` (matrix or NULL), `score`, `status` (a member of
#' `arc_search_statuses`), `phases` (per-phase statuses), `trace`,
#' `objective_calls`, `proxy_calls`, `proposals`, `failed_proposals`,
#' `disconnected`, `search_secs`, `seed`, plus fields filled later by the
#' verification step: `verify_secs`, `total_secs`, `final`, `valid`. Process
#' memory, BLAS and CPU details belong to the offline benchmark harness.
#'
#' @param design Design matrix or `NULL`.
#' @param score Objective value of the design.
#' @param status Single string from `arc_search_statuses`.
#' @param phases Per-phase stopping statuses (character vector or list).
#' @param trace Search trace.
#' @param calls Named numeric `c(objective =, proxy =)`.
#' @param proposals,failed,disconnected Candidate counters.
#' @param search_secs Search time in seconds.
#' @param seed Seed used for the run.
#' @param verify_secs,total_secs,final,valid Filled by task 8.
#' @noRd
arc_search_result <- function(design = NULL, score = NA_real_, status,
                              phases = NULL, trace = NULL,
                              calls = c(objective = 0, proxy = 0),
                              proposals = 0, failed = 0, disconnected = 0,
                              search_secs = NA_real_, seed = NA,
                              verify_secs = NA_real_, total_secs = NA_real_,
                              final = NULL, valid = NA) {
  if (missing(status) || !is.character(status) || length(status) != 1L ||
      is.na(status) || !status %in% arc_search_statuses) {
    fieldhub_abort("The search status must be one of: ",
                   paste(arc_search_statuses, collapse = ", "), ".",
                   class = c("fieldhub_arc_input_error",
                             "fieldhub_input_error"))
  }
  list(
    design = design, score = score, status = status, phases = phases,
    trace = trace,
    objective_calls = unname(calls[["objective"]]),
    proxy_calls = unname(calls[["proxy"]]),
    proposals = proposals, failed_proposals = failed,
    disconnected = disconnected, search_secs = search_secs,
    verify_secs = verify_secs, total_secs = total_secs, seed = seed,
    final = final, valid = valid
  )
}
