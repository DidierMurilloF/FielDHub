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

# ---- Global search budget (specification sections 5.4 and 7.1) ----

#' Default internal clock
#'
#' @return Current time in seconds as a double.
#' @noRd
arc_clock <- function() as.numeric(Sys.time())

#' Create a global search budget
#'
#' Specification sections 5.4 and 7.1. An environment shared by every phase
#' and restart. It holds the global evaluation cap, the wall-clock deadline
#' and all call accounting. The clock is injectable through `now` so tests
#' can drive deadlines deterministically.
#'
#' @param max_evals Global cap on objective plus proxy calls (`Inf` allowed).
#' @param seconds Seconds until the deadline (`Inf` allowed; zero or negative
#'   means already expired).
#' @param max_consec_fail Whole number >= 1, consecutive failed proposals
#'   tolerated by later phases.
#' @param proposal_factor Finite number > 0, proposal cap factor used by
#'   later phases.
#' @param now Function returning the current time in seconds.
#' @return An environment with the budget fields.
#' @noRd
arc_budget_new <- function(max_evals = Inf, seconds = Inf,
                           max_consec_fail = 200L, proposal_factor = 20,
                           now = arc_clock) {
  input_class <- c("fieldhub_arc_input_error", "fieldhub_input_error")
  single_number <- function(x) {
    is.numeric(x) && length(x) == 1L && !is.na(x)
  }
  if (!single_number(max_evals) || max_evals < 0) {
    fieldhub_abort("max_evals must be a single number >= 0 (Inf allowed).",
                   class = input_class)
  }
  if (!single_number(seconds)) {
    fieldhub_abort("seconds must be a single non-missing number.",
                   class = input_class)
  }
  if (!single_number(max_consec_fail) || !is.finite(max_consec_fail) ||
      max_consec_fail < 1 || max_consec_fail != round(max_consec_fail)) {
    fieldhub_abort("max_consec_fail must be a single whole number >= 1.",
                   class = input_class)
  }
  if (!single_number(proposal_factor) || !is.finite(proposal_factor) ||
      proposal_factor <= 0) {
    fieldhub_abort("proposal_factor must be a single finite number > 0.",
                   class = input_class)
  }
  if (!is.function(now)) {
    fieldhub_abort("now must be a function.", class = input_class)
  }
  b <- new.env(parent = emptyenv())
  b$max_evals <- as.double(max_evals)
  b$now <- now
  b$t_start <- now()
  b$deadline <- b$t_start + as.double(seconds)
  b$evals <- c(objective = 0, proxy = 0)
  b$proposals <- 0
  b$failed <- 0
  b$disconnected <- 0
  b$events <- character(0)
  b$trace <- list()
  b$max_consec_fail <- as.integer(max_consec_fail)
  b$proposal_factor <- as.double(proposal_factor)
  b
}

#' Total objective plus proxy calls spent
#' @param b Budget environment.
#' @noRd
arc_budget_used <- function(b) sum(b$evals)

#' Calls left under the global cap (`Inf` when uncapped)
#' @param b Budget environment.
#' @noRd
arc_budget_left <- function(b) b$max_evals - arc_budget_used(b)

#' Why the budget is exhausted
#'
#' @param b Budget environment.
#' @return `NA_character_`, `"budget"` (checked first) or `"time_limit"`.
#' @noRd
arc_budget_out <- function(b) {
  if (arc_budget_used(b) >= b$max_evals) {
    return("budget")
  }
  if (b$now() >= b$deadline) {
    return("time_limit")
  }
  NA_character_
}

#' Evaluate a function under the budget
#'
#' Refuses (returns `NULL`, `f` not called) once the budget or deadline is
#' exhausted. Otherwise the call is counted BEFORE `f` runs, so an `f` that
#' errors still counts. `NA` results are counted as disconnected.
#'
#' @param b Budget environment.
#' @param f Function of one argument.
#' @param x Argument passed to `f`.
#' @param kind `"objective"` or `"proxy"`.
#' @return `f(x)`, or `NULL` when refused.
#' @noRd
arc_budget_eval <- function(b, f, x, kind = "objective") {
  if (!is.character(kind) || length(kind) != 1L || is.na(kind) ||
      !kind %in% c("objective", "proxy")) {
    fieldhub_abort("kind must be \"objective\" or \"proxy\".",
                   class = c("fieldhub_arc_input_error",
                             "fieldhub_input_error"))
  }
  if (!is.na(arc_budget_out(b))) {
    return(NULL)
  }
  b$evals[[kind]] <- b$evals[[kind]] + 1
  y <- f(x)
  if (is.na(y)) {
    b$disconnected <- b$disconnected + 1
  }
  y
}

#' Seconds until the deadline (may be negative or `Inf`)
#' @param b Budget environment.
#' @noRd
arc_budget_secs_left <- function(b) b$deadline - b$now()

#' Seconds since the budget was created
#' @param b Budget environment.
#' @noRd
arc_budget_elapsed <- function(b) b$now() - b$t_start

#' Append an event message
#' @param b Budget environment.
#' @param ... Pieces pasted together with `paste0()`.
#' @noRd
arc_budget_log <- function(b, ...) {
  b$events <- c(b$events, paste0(...))
  invisible(b)
}

#' Append a one-row trace data frame
#' @param b Budget environment.
#' @param row One-row data frame.
#' @noRd
arc_budget_add_trace <- function(b, row) {
  b$trace[[length(b$trace) + 1L]] <- row
  invisible(b)
}

#' Combined trace, or `NULL` when empty
#' @param b Budget environment.
#' @noRd
arc_budget_trace <- function(b) {
  if (length(b$trace) == 0L) {
    return(NULL)
  }
  do.call(rbind, b$trace)
}

#' Record a proposal
#' @param b Budget environment.
#' @param failed `TRUE` when the proposal failed.
#' @noRd
arc_budget_note_proposal <- function(b, failed) {
  b$proposals <- b$proposals + 1
  if (isTRUE(failed)) {
    b$failed <- b$failed + 1
  }
  invisible(b)
}

#' Named call counts for `arc_search_result(calls = )`
#' @param b Budget environment.
#' @noRd
arc_budget_calls <- function(b) {
  c(objective = b$evals[["objective"]], proxy = b$evals[["proxy"]])
}

# ---- Fresh search evaluator (specification section 6.1) ----

#' Precompute the pieces shared by every search evaluation
#'
#' Specification section 6.1. Built once per run for fixed (v, s, k). Assumes
#' the dimensions passed `arc_search_entry()`; nothing is re-validated here.
#'
#' @param v,s,k Field rows, columns and checks per column (whole numbers).
#' @return A list with integer `v`, `s`, `k`, `n_tests` = (v - k) s,
#'   `v_star` = n_tests + k and `df` (from `arc_df_formula()`); `q0q0` = Q0
#'   Q0' ((v + s) square); `col_block` = k (I_s - J_s / s); `col_index` and
#'   `row_index`, the column and row of each entry of `as.integer()` of a
#'   k x s contraction.
#' @noRd
arc_search_prep <- function(v, s, k) {
  v <- as.integer(v)
  s <- as.integer(s)
  k <- as.integer(k)
  n_tests <- (v - k) * s
  q0 <- cbind(c(rep(1, v), rep(0, s)) / sqrt(v),
              c(rep(0, v), rep(1, s)) / sqrt(s))
  list(v = v, s = s, k = k, n_tests = n_tests, v_star = n_tests + k,
       df = arc_df_formula(v, s, k), q0q0 = tcrossprod(q0),
       col_block = k * (diag(s) - matrix(1 / s, s, s)),
       col_index = rep(seq_len(s), each = k),
       row_index = rep(seq_len(k), times = s))
}

#' Fresh evaluation of a restricted contraction inside the search
#'
#' Specification section 6.1, the lean counterpart of
#' `arc_evaluate_objectives()` (same formulas, M1 section 7.3) without its
#' structural checks. Steps: integer df pre-check; M_tilde = M + Q0 Q0' with
#' N_R assembled by counting (an error unless every row is restricted);
#' symmetric eigenvalues of M_tilde classified by `arc_classify_spectrum()`,
#' M1's own connectivity predicate; when connected, Omega =
#' chol2inv(chol(M_tilde)) - Q0 Q0' and the four objectives. Column labels
#' are assumed to lie in 1..v without repeats within a column; column-only
#' intermediate states must be recoloured or scored by `arc_column_proxy()`.
#'
#' @param contraction Integer k x s contraction with restricted rows.
#' @param prep Output of `arc_search_prep()`.
#' @return `list(connected, lambda_min_rel)` when df < 0 (`lambda_min_rel`
#'   NA) or disconnected; otherwise `list(connected = TRUE, lambda_min_rel,
#'   E_aug, V_tt, V_tc, V_cc)`.
#' @noRd
arc_search_evaluate <- function(contraction, prep) {
  if (prep$df < 0L) {
    return(list(connected = FALSE, lambda_min_rel = NA_real_))
  }
  v <- prep$v
  s <- prep$s
  k <- prep$k
  labels <- as.integer(contraction)
  nr <- tabulate(labels + v * (prep$row_index - 1L), nbins = v * k)
  if (any(nr > 1L)) {
    row <- (which(nr > 1L)[1L] - 1L) %/% v + 1L
    fieldhub_abort("Contraction row ", row, " repeats a pseudo-treatment; ",
                   "the search evaluator needs restricted rows (recolour ",
                   "first or use the column proxy).",
                   class = c("fieldhub_arc_row_repeat_error",
                             "fieldhub_arc_input_error",
                             "fieldhub_input_error"),
                   data = list(row = row))
  }
  nr <- matrix(as.double(nr), nrow = v, ncol = k)
  nc <- matrix(0, nrow = v, ncol = s)
  nc[cbind(labels, prep$col_index)] <- 1
  r <- rowSums(nc)
  m12 <- nc - r / s
  m_tilde <- rbind(cbind(diag(r, nrow = v) - tcrossprod(nr) / s, m12),
                   cbind(t(m12), prep$col_block)) + prep$q0q0
  lambda <- eigen(m_tilde, symmetric = TRUE, only.values = TRUE)$values
  spectrum <- arc_classify_spectrum(lambda)
  if (!spectrum$connected) {
    return(list(connected = FALSE, lambda_min_rel = spectrum$lambda_min_rel))
  }

  omega <- chol2inv(chol(m_tilde)) - prep$q0q0
  omega <- (omega + t(omega)) / 2
  rows <- seq_len(v)
  cols <- v + seq_len(s)
  n_tests <- prep$n_tests
  diag_omega <- diag(omega)
  e_aug <- (prep$v_star - 1) /
    (prep$v_star - v - s + 1 + s * sum(diag_omega[rows]) +
       v * sum(diag_omega[cols]))

  # Test aggregates S_T and h_T, check aggregates H_C (M1 section 7.3)
  ti <- 1 - nc
  ti_rows <- rowSums(ti)
  ti_cols <- colSums(ti)
  s_t <- rbind(cbind(diag(ti_rows, nrow = v), ti),
               cbind(t(ti), diag(ti_cols, nrow = s)))
  h_t <- c(ti_rows, ti_cols)
  omega_h_t <- drop(omega %*% h_t)
  trace_tt <- n_tests + sum(omega * s_t)
  sum_tt <- n_tests + sum(h_t * omega_h_t)
  h_c <- cbind(t(nr), matrix(1, k, s)) / s
  omega_h_c <- omega %*% t(h_c)
  g_cc <- diag(1 / s, nrow = k) + h_c %*% omega_h_c
  trace_cc <- sum(diag(g_cc))
  sum_cc <- sum(g_cc)
  sum_tc <- sum(omega_h_t * colSums(h_c))

  list(connected = TRUE, lambda_min_rel = spectrum$lambda_min_rel,
       E_aug = e_aug,
       V_tt = (n_tests * trace_tt - sum_tt) / choose(n_tests, 2),
       V_tc = (k * trace_tt + n_tests * trace_cc - 2 * sum_tc) /
         (n_tests * k),
       V_cc = (k * trace_cc - sum_cc) / choose(k, 2))
}

#' Search score of a restricted contraction (larger is better)
#'
#' Specification section 6.1: E_aug, or minus V_tt or V_tc; `NA_real_` when
#' the design is disconnected (or df < 0).
#'
#' @inheritParams arc_search_evaluate
#' @param objective One of `arc_search_objectives`.
#' @return A single number or `NA_real_`.
#' @noRd
arc_search_score <- function(contraction, prep, objective) {
  if (!is.character(objective) || length(objective) != 1L ||
      is.na(objective) || !objective %in% arc_search_objectives) {
    fieldhub_abort("The search objective must be one of: ",
                   paste(arc_search_objectives, collapse = ", "), ".",
                   class = c("fieldhub_arc_input_error",
                             "fieldhub_input_error"))
  }
  e <- arc_search_evaluate(contraction, prep)
  if (!e$connected) {
    return(NA_real_)
  }
  if (identical(objective, "E_aug")) e$E_aug else -e[[objective]]
}

#' Column-design proxy score
#'
#' Specification section 6.1 rules (phase 1): the A-efficiency of the column
#' design, columns read as blocks of size k, for column-binary contractions
#' whose rows need not be restricted. With C = diag(r) - N_C N_C' / k and
#' lambda its v - 1 largest eigenvalues, the score is
#' (v - 1) / sum(1 / lambda) / mean(r), and `NA_real_` when
#' min(lambda) < 1e-8 max(lambda) (disconnected column design).
#'
#' @param contraction Integer k x s column-binary contraction.
#' @param v Number of pseudo-treatments.
#' @return A single number in (0, 1] or `NA_real_`.
#' @noRd
arc_column_proxy <- function(contraction, v) {
  v <- as.integer(v)
  k <- nrow(contraction)
  s <- ncol(contraction)
  labels <- as.integer(contraction)
  nc <- matrix(as.double(tabulate(labels + v * (rep(seq_len(s), each = k) -
                                                   1L), nbins = v * s)),
               nrow = v, ncol = s)
  r <- tabulate(labels, nbins = v)
  info <- diag(as.double(r), nrow = v) - tcrossprod(nc) / k
  lambda <- eigen(info, symmetric = TRUE, only.values = TRUE)$values
  lambda <- lambda[seq_len(v - 1L)]
  if (min(lambda) < 1e-8 * max(lambda)) {
    return(NA_real_)
  }
  (v - 1) / sum(1 / lambda) / mean(r)
}
