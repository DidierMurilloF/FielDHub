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

# ---- Moves, lazy scans and exhaustion (specification sections 4, 5.1, 5.4) ----

#' Whether a two-cell exchange is feasible
#'
#' Specification section 5.1, moves A, B and C as one general check: the
#' labels at cells (i, j) and (ii, jj) trade places. Equal labels (and the
#' same cell) are never a move. Column binarity: when j != jj, a must not
#' already be in column jj nor b in column j. Row binarity (restricted mode
#' only): when i != ii, b must not be elsewhere in row i nor a elsewhere in
#' row ii. Cost O(k + s).
#'
#' @param contraction Integer k x s column-binary contraction.
#' @param i,j,ii,jj Row and column of the two cells.
#' @param restricted Also require restricted (row-binary) rows.
#' @return A single logical.
#' @noRd
arc_swap_feasible <- function(contraction, i, j, ii, jj, restricted = TRUE) {
  a <- contraction[i, j]
  b <- contraction[ii, jj]
  if (a == b) {
    return(FALSE)
  }
  if (j != jj && (any(contraction[, jj] == a) ||
                  any(contraction[, j] == b))) {
    return(FALSE)
  }
  if (restricted && i != ii && (any(contraction[i, -j] == b) ||
                                any(contraction[ii, -jj] == a))) {
    return(FALSE)
  }
  TRUE
}

#' Apply a two-cell exchange (no feasibility check)
#'
#' @inheritParams arc_swap_feasible
#' @return The contraction with cells (i, j) and (ii, jj) exchanged.
#' @noRd
arc_swap_apply <- function(contraction, i, j, ii, jj) {
  a <- contraction[i, j]
  contraction[i, j] <- contraction[ii, jj]
  contraction[ii, jj] <- a
  contraction
}

#' Kempe components of a pair of contraction rows
#'
#' Specification section 5.1, move K. Columns c and d are linked when the
#' label in row i of column c sits in row ii of column d. Swapping rows i and
#' ii on all columns of one component keeps restricted rows and leaves N_C
#' fixed; a singleton component is an A move. Assumes restricted rows.
#'
#' @inheritParams arc_swap_feasible
#' @param i,ii The two contraction rows.
#' @return A list of integer column sets partitioning `seq_len(s)`.
#' @noRd
arc_kempe_components <- function(contraction, i, ii) {
  s <- ncol(contraction)
  partner <- match(contraction[i, ], contraction[ii, ])
  root <- seq_len(s)
  find <- function(x) {
    while (root[x] != x) x <- root[x]
    x
  }
  for (column in which(!is.na(partner))) {
    a <- find(column)
    b <- find(partner[column])
    if (a != b) root[a] <- b
  }
  roots <- vapply(seq_len(s), find, integer(1))
  unname(split(seq_len(s), roots))
}

#' Swap two contraction rows on a set of columns
#'
#' @inheritParams arc_kempe_components
#' @param columns Integer column set, normally one Kempe component.
#' @return The contraction after the swap.
#' @noRd
arc_kempe_apply <- function(contraction, i, ii, columns) {
  upper <- contraction[i, columns]
  contraction[i, columns] <- contraction[ii, columns]
  contraction[ii, columns] <- upper
  contraction
}

#' Randomized recolouring with the same column incidence
#'
#' Specification sections 4 (item 3) and 5.1, move R: permute the labels
#' within each column at random, then edge-colour the rows with
#' `arc_recolor_rows()`. Needs max(r) <= k.
#'
#' @param contraction Integer k x s column-binary contraction.
#' @param v Number of pseudo-treatments.
#' @return An integer k x s restricted contraction with the same N_C.
#' @noRd
arc_random_recolor <- function(contraction, v) {
  k <- nrow(contraction)
  s <- ncol(contraction)
  shuffled <- vapply(seq_len(s), function(j) {
    as.integer(contraction[sample.int(k), j])
  }, integer(k))
  arc_recolor_rows(matrix(shuffled, nrow = k, ncol = s), v)
}

#' Balanced replication vector
#'
#' Every pseudo-treatment occurs floor(ks / v) times; the ks - v floor(ks /
#' v) extra units go to labels drawn at random.
#'
#' @param v,s,k Pseudo-treatments, columns and checks per column.
#' @return Integer vector of length v summing to ks.
#' @noRd
arc_balanced_replication <- function(v, s, k) {
  v <- as.integer(v)
  total <- as.integer(k) * as.integer(s)
  low <- total %/% v
  r <- rep(low, v)
  extra <- total - low * v
  r[sample.int(v, extra)] <- low + 1L
  r
}

#' Random restricted start with bounded repair
#'
#' Specification section 4 (item 1) and 5.4. Each of at most `tries` fills
#' places the labels at random, then runs at most `repair` random repairs of
#' a repeated label within a column. A column-binary fill is edge-coloured
#' by `arc_recolor_rows()`. `should_stop()` is checked before every fill and
#' every 256 repair steps; once it returns TRUE the result is `NULL`. The
#' connectivity check and scoring belong to the caller.
#'
#' @param v,s,k Pseudo-treatments, columns and checks per column.
#' @param replication Integer vector of length v summing to ks.
#' @param tries,repair Fill and repair-step bounds.
#' @param should_stop Function returning TRUE to abandon the search.
#' @return An integer k x s restricted contraction, or `NULL` when stopped,
#'   when no fill succeeded, or when `replication` admits no restricted
#'   contraction (length, total, or some r_h > min(k, s)).
#' @noRd
arc_random_start <- function(v, s, k,
                             replication = arc_balanced_replication(v, s, k),
                             tries = 50L, repair = 5000L,
                             should_stop = function() FALSE) {
  v <- as.integer(v)
  s <- as.integer(s)
  k <- as.integer(k)
  replication <- as.integer(replication)
  if (length(replication) != v || anyNA(replication) ||
      any(replication < 0L) || sum(replication) != k * s ||
      max(replication) > min(k, s)) {
    return(NULL)
  }
  labels <- rep(seq_len(v), replication)
  repeats <- function(m, j) anyDuplicated(m[, j]) > 0L
  for (fill in seq_len(tries)) {
    if (should_stop()) {
      return(NULL)
    }
    m <- matrix(labels[sample.int(k * s)], nrow = k, ncol = s)
    bad <- vapply(seq_len(s), function(j) repeats(m, j), logical(1))
    for (step in seq_len(repair)) {
      if (!any(bad)) break
      if (step %% 256L == 0L && should_stop()) {
        return(NULL)
      }
      candidates <- which(bad)
      j <- candidates[sample.int(length(candidates), 1L)]
      i <- anyDuplicated(m[, j])
      ii <- sample.int(k, 1L)
      jj <- sample.int(s, 1L)
      if (!any(m[, j] == m[ii, jj]) && !any(m[-ii, jj] == m[i, j])) {
        m <- arc_swap_apply(m, i, j, ii, jj)
        bad[j] <- repeats(m, j)
        bad[jj] <- repeats(m, jj)
      }
    }
    if (!any(bad)) {
      return(arc_recolor_rows(m, v))
    }
  }
  NULL
}

#' One Kempe draw: a random row pair and one of its components
#'
#' @inheritParams arc_swap_feasible
#' @return A contraction, or `NULL` when the row pair has one component.
#' @noRd
arc_propose_kempe <- function(contraction) {
  rows <- sample.int(nrow(contraction), 2L)
  i <- min(rows)
  ii <- max(rows)
  components <- arc_kempe_components(contraction, i, ii)
  if (length(components) < 2L) {
    return(NULL)
  }
  arc_kempe_apply(contraction, i, ii,
                  components[[sample.int(length(components), 1L)]])
}

#' Joint-phase sampler: moves A/B/C and K
#'
#' Specification section 5.4: with probability `kempe_share` one Kempe draw
#' (a single component means no Kempe move), then at most `tries` random
#' two-cell draws with restricted feasibility.
#'
#' @inheritParams arc_swap_feasible
#' @param kempe_share Probability of trying a Kempe move first.
#' @param tries Bound on two-cell draws.
#' @return A neighbouring restricted contraction, or `NULL`.
#' @noRd
arc_propose_joint <- function(contraction, kempe_share = 0.3, tries = 30L) {
  k <- nrow(contraction)
  s <- ncol(contraction)
  if (k >= 2L && stats::runif(1L) < kempe_share) {
    m <- arc_propose_kempe(contraction)
    if (!is.null(m)) {
      return(m)
    }
  }
  for (draw in seq_len(tries)) {
    i <- sample.int(k, 1L)
    j <- sample.int(s, 1L)
    ii <- sample.int(k, 1L)
    jj <- sample.int(s, 1L)
    if (arc_swap_feasible(contraction, i, j, ii, jj)) {
      return(arc_swap_apply(contraction, i, j, ii, jj))
    }
  }
  NULL
}

#' Column-phase sampler: column-incidence exchanges (move X)
#'
#' Two cells in different columns trade labels under column binarity only;
#' rows may repeat labels until the caller recolours. At most `tries` draws.
#'
#' @inheritParams arc_propose_joint
#' @return A column-binary contraction with two changed columns, or `NULL`.
#' @noRd
arc_propose_column <- function(contraction, tries = 30L) {
  k <- nrow(contraction)
  s <- ncol(contraction)
  if (s < 2L) {
    return(NULL)
  }
  for (draw in seq_len(tries)) {
    i <- sample.int(k, 1L)
    j <- sample.int(s, 1L)
    ii <- sample.int(k, 1L)
    jj <- sample.int(s - 1L, 1L)
    if (jj >= j) jj <- jj + 1L
    if (arc_swap_feasible(contraction, i, j, ii, jj, restricted = FALSE)) {
      return(arc_swap_apply(contraction, i, j, ii, jj))
    }
  }
  NULL
}

#' Row-phase sampler: moves A and K, N_C fixed
#'
#' @inheritParams arc_propose_joint
#' @return A restricted contraction with the same N_C, or `NULL`.
#' @noRd
arc_propose_row <- function(contraction, kempe_share = 0.6, tries = 30L) {
  k <- nrow(contraction)
  s <- ncol(contraction)
  if (k < 2L) {
    return(NULL)
  }
  if (stats::runif(1L) < kempe_share) {
    m <- arc_propose_kempe(contraction)
    if (!is.null(m)) {
      return(m)
    }
  }
  for (draw in seq_len(tries)) {
    j <- sample.int(s, 1L)
    rows <- sample.int(k, 2L)
    i <- min(rows)
    ii <- max(rows)
    if (arc_swap_feasible(contraction, i, j, ii, j)) {
      return(arc_swap_apply(contraction, i, j, ii, j))
    }
  }
  NULL
}

#' Lazy scan of a whole neighbourhood in random order
#'
#' Specification section 5.4. Descriptors are cheap: cell pairs (two-cell
#' exchanges) and row pairs (Kempe components, expanded on visit); a move is
#' built only when its descriptor is visited. Kinds: "joint", all two-cell
#' exchanges with restricted checks plus Kempe moves; "column", exchanges
#' between different columns under column binarity only; "row", same-column
#' exchanges (A) plus Kempe moves. `stop_fn()` is checked before every
#' descriptor and every Kempe component, so at most one move construction
#' follows a deadline.
#'
#' @inheritParams arc_swap_feasible
#' @param kind "joint", "column" or "row".
#' @param visit Function of a candidate contraction; TRUE stops the scan.
#' @param stop_fn Function returning TRUE to interrupt the scan.
#' @return `list(status, n_feasible)`: status "complete", "stopped" (visit
#'   asked) or "interrupted" (stop_fn), and the feasible moves visited.
#' @noRd
arc_scan_neighbourhood <- function(contraction, kind = c("joint", "column",
                                                         "row"),
                                   visit, stop_fn = function() FALSE) {
  kind <- match.arg(kind)
  k <- nrow(contraction)
  s <- ncol(contraction)
  n <- k * s
  cell_row <- rep(seq_len(k), times = s)
  cell_col <- rep(seq_len(s), each = k)
  upper <- upper.tri(matrix(FALSE, n, n))
  first <- row(upper)[upper]
  second <- col(upper)[upper]
  same_column <- cell_col[first] == cell_col[second]
  keep <- switch(kind, joint = rep(TRUE, length(first)),
                 column = !same_column, row = same_column)
  first <- first[keep]
  second <- second[keep]
  restricted <- kind != "column"
  if (kind != "column" && k >= 2L) {
    pairs <- upper.tri(matrix(FALSE, k, k))
    row_i <- row(pairs)[pairs]
    row_ii <- col(pairs)[pairs]
  } else {
    row_i <- integer(0)
    row_ii <- integer(0)
  }
  n_cells <- length(first)
  n_feasible <- 0L
  for (o in sample.int(n_cells + length(row_i))) {
    if (stop_fn()) {
      return(list(status = "interrupted", n_feasible = n_feasible))
    }
    if (o <= n_cells) {
      p <- first[o]
      q <- second[o]
      if (!arc_swap_feasible(contraction, cell_row[p], cell_col[p],
                             cell_row[q], cell_col[q], restricted)) {
        next
      }
      n_feasible <- n_feasible + 1L
      candidate <- arc_swap_apply(contraction, cell_row[p], cell_col[p],
                                  cell_row[q], cell_col[q])
      if (isTRUE(visit(candidate))) {
        return(list(status = "stopped", n_feasible = n_feasible))
      }
    } else {
      i <- row_i[o - n_cells]
      ii <- row_ii[o - n_cells]
      components <- arc_kempe_components(contraction, i, ii)
      if (length(components) < 2L) next
      for (columns in components[sample.int(length(components))]) {
        if (stop_fn()) {
          return(list(status = "interrupted", n_feasible = n_feasible))
        }
        n_feasible <- n_feasible + 1L
        if (isTRUE(visit(arc_kempe_apply(contraction, i, ii, columns)))) {
          return(list(status = "stopped", n_feasible = n_feasible))
        }
      }
    }
  }
  list(status = "complete", n_feasible = n_feasible)
}

#' Classify a sampling failure
#'
#' Specification section 5.4. `empty_neighbourhood` is certified only by a
#' complete scan of the phase's move set that finds no feasible move, and
#' the scan runs only when the contraction has at most `cap_cells` cells.
#' An interrupted scan certifies nothing.
#'
#' @inheritParams arc_scan_neighbourhood
#' @param cap_cells Largest contraction (k s cells) that is scanned.
#' @return "empty_neighbourhood" or "sampling_exhausted".
#' @noRd
arc_classify_exhaustion <- function(contraction, kind,
                                    stop_fn = function() FALSE,
                                    cap_cells = 400L) {
  if (length(contraction) > cap_cells) {
    return("sampling_exhausted")
  }
  scan <- arc_scan_neighbourhood(contraction, kind, function(x) TRUE,
                                 stop_fn = stop_fn)
  if (identical(scan$status, "complete") && scan$n_feasible == 0L) {
    "empty_neighbourhood"
  } else {
    "sampling_exhausted"
  }
}

# ---- Annealing phase and local search (specification sections 5.4, 7.4) ----

#' Check an argument and raise the classed search input error
#'
#' @param ok Single logical; the error is raised when it is not TRUE.
#' @param message Error message.
#' @noRd
arc_search_check <- function(ok, message) {
  if (!isTRUE(ok)) {
    fieldhub_abort(message, class = c("fieldhub_arc_input_error",
                                      "fieldhub_input_error"),
                   call = sys.call(-1))
  }
  invisible(TRUE)
}

#' Is `x` a single non-missing number?
#' @param x Any object.
#' @noRd
arc_is_number <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x)
}

#' Is `x` one of the allowed strings?
#' @param x Any object.
#' @param allowed Character vector.
#' @noRd
arc_is_choice <- function(x, allowed) {
  is.character(x) && length(x) == 1L && !is.na(x) && x %in% allowed
}

#' One bounded simulated-annealing phase (maximization)
#'
#' Specification sections 5.4 and 7.4. Spends at most `phase_evals` counted
#' calls (and never more than the global budget). Phases with fewer than 10
#' evaluations run greedy (accept non-worsening only, `t0 = 0`). Otherwise
#' `min(n_calibration, max(1, floor(phase_evals / 5)))` counted calibration
#' calls on proposals from the start give `t0`, the mean finite non-zero
#' |delta| from `score_start` (fallback `1e-6 max(|score_start|, 1)`, note
#' "calibration_fallback"), multiplied by `t0_multiplier`. The end
#' temperature is `t0 t_ratio`. A finite allowance cools geometrically after
#' every counted call; an infinite allowance with a finite deadline cools by
#' the elapsed fraction of the time from the phase start (after calibration)
#' to the deadline. With both infinite, a finite global cap is used as the
#' allowance for cooling, else the temperature stays at `t0`.
#'
#' Stop order before every proposal: global budget or deadline (`budget`,
#' `time_limit`), `phase_budget`, the proposal cap (`proposal_factor` times
#' the allowance; when every proposal of the phase failed the exhaustion
#' check decides `empty_neighbourhood` or `sampling_exhausted`, else
#' `proposal_cap`), then `max_consec_fail` consecutive failures (exhaustion
#' check). `NA` scores (disconnected) are never accepted but still cool.
#' Calibration candidates that beat the best become the best. One trace row
#' is appended on exit.
#'
#' @param budget Budget environment from `arc_budget_new()`.
#' @param start Start contraction.
#' @param score_start Score of `start` (`NA` or `-Inf` allowed: any finite
#'   score then improves).
#' @param score_fn Function of a contraction returning a number or `NA`.
#' @param propose Function of a contraction returning a candidate or `NULL`.
#' @param phase_evals Call allowance, a number >= 0 (`Inf`: time-bounded).
#' @param exhaustion_kind Move set for the exhaustion check: "joint",
#'   "column" or "row".
#' @param t_ratio End-to-start temperature ratio in (0, 1].
#' @param n_calibration Largest number of calibration calls, >= 1.
#' @param kind Call kind for the budget: "objective" or "proxy".
#' @param label Phase label for the trace.
#' @param t0_multiplier Finite number > 0 multiplying the start temperature.
#' @param on_step Internal, for tests: `NULL` or a function
#'   `(temp, y, current)` called before every acceptance decision.
#' @return `list(contraction, score, status, t0, note, proposals)` with the
#'   best state seen.
#' @noRd
arc_anneal_phase <- function(budget, start, score_start, score_fn, propose,
                             phase_evals, exhaustion_kind, t_ratio = 1e-3,
                             n_calibration = 50L, kind = "objective",
                             label = "", t0_multiplier = 1, on_step = NULL) {
  arc_search_check(arc_is_number(phase_evals) && phase_evals >= 0,
                   "phase_evals must be a single number >= 0 (Inf allowed).")
  arc_search_check(arc_is_number(t_ratio) && t_ratio > 0 && t_ratio <= 1,
                   "t_ratio must be a single number in (0, 1].")
  arc_search_check(arc_is_number(n_calibration) && n_calibration >= 1,
                   "n_calibration must be a single number >= 1.")
  arc_search_check(arc_is_number(t0_multiplier) && is.finite(t0_multiplier) &&
                     t0_multiplier > 0,
                   "t0_multiplier must be a single finite number > 0.")
  arc_search_check(arc_is_choice(kind, c("objective", "proxy")),
                   "kind must be \"objective\" or \"proxy\".")
  arc_search_check(arc_is_choice(exhaustion_kind, c("joint", "column", "row")),
                   "exhaustion_kind must be \"joint\", \"column\" or \"row\".")
  arc_search_check(is.function(score_fn) && is.function(propose),
                   "score_fn and propose must be functions.")
  arc_search_check(is.null(on_step) || is.function(on_step),
                   "on_step must be NULL or a function.")

  as_level <- function(y) if (is.na(y)) -Inf else y
  current <- start
  f_current <- as_level(score_start)
  best <- start
  f_best <- f_current
  score_best <- score_start
  n_accepted <- 0L
  n_worse <- 0L
  used_at_entry <- arc_budget_used(budget)
  stop_at <- used_at_entry + phase_evals
  max_proposals <- budget$proposal_factor * max(phase_evals, 1)
  proposals <- 0
  failed <- 0
  consec <- 0L
  budget_stop <- function() !is.na(arc_budget_out(budget))
  exhausted <- function() {
    arc_classify_exhaustion(current, exhaustion_kind, stop_fn = budget_stop)
  }
  local_out <- function() {
    out <- arc_budget_out(budget)
    if (!is.na(out)) return(out)
    if (arc_budget_used(budget) >= stop_at) return("phase_budget")
    if (proposals >= max_proposals) {
      return(if (failed == proposals) exhausted() else "proposal_cap")
    }
    NA_character_
  }
  next_candidate <- function() {
    proposals <<- proposals + 1
    candidate <- propose(current)
    arc_budget_note_proposal(budget, failed = is.null(candidate))
    if (is.null(candidate)) {
      failed <<- failed + 1
      consec <<- consec + 1L
    } else {
      consec <<- 0L
    }
    candidate
  }
  keep_best <- function(candidate, y) {
    if (!is.na(y) && y > f_best) {
      best <<- candidate
      f_best <<- y
      score_best <<- y
    }
  }

  greedy <- phase_evals < 10
  t0 <- 0
  note <- ""
  status <- NA_character_
  if (!greedy) {
    deltas <- numeric(0)
    n_cal <- min(n_calibration, max(1, floor(phase_evals / 5)))
    for (q in seq_len(n_cal)) {
      if (!is.na(local_out()) || consec >= budget$max_consec_fail) break
      candidate <- next_candidate()
      if (is.null(candidate)) next
      y <- arc_budget_eval(budget, score_fn, candidate, kind)
      if (is.null(y)) break
      if (is.finite(y) && is.finite(f_current)) {
        deltas <- c(deltas, abs(y - f_current))
      }
      keep_best(candidate, y)
    }
    deltas <- deltas[is.finite(deltas) & deltas > 0]
    if (length(deltas) > 0L) {
      t0 <- mean(deltas)
    } else {
      note <- "calibration_fallback"
      t0 <- 1e-6 * max(if (is.finite(f_current)) abs(f_current) else 0, 1)
    }
    t0 <- t0 * t0_multiplier
  }

  timed <- !is.finite(phase_evals) && is.finite(budget$deadline)
  t_phase <- budget$now()
  span <- max(1e-3, budget$deadline - t_phase)
  remaining <- min(stop_at, budget$max_evals) - arc_budget_used(budget)
  alpha <- if (is.finite(remaining)) t_ratio^(1 / max(1, remaining)) else 1
  temp <- t0
  repeat {
    status <- local_out()
    if (!is.na(status)) break
    if (consec >= budget$max_consec_fail) {
      status <- exhausted()
      break
    }
    candidate <- next_candidate()
    if (is.null(candidate)) next
    y <- arc_budget_eval(budget, score_fn, candidate, kind)
    if (is.null(y)) {
      status <- arc_budget_out(budget)
      break
    }
    temp <- if (timed) {
      t0 * t_ratio^min(1, max(0, budget$now() - t_phase) / span)
    } else {
      temp * alpha
    }
    if (is.na(y)) next
    if (!is.null(on_step)) on_step(temp, y, f_current)
    if (y >= f_current ||
        (!greedy && stats::runif(1) < exp((y - f_current) / temp))) {
      n_accepted <- n_accepted + 1L
      if (y < f_current) n_worse <- n_worse + 1L
      current <- candidate
      f_current <- y
      keep_best(candidate, y)
    }
  }
  arc_budget_add_trace(budget, data.frame(
    phase = label, f_start = score_start, f_best = score_best, t0 = t0,
    evals = arc_budget_used(budget) - used_at_entry, accepted = n_accepted,
    worse_accepted = n_worse, status = status, stringsAsFactors = FALSE
  ))
  list(contraction = best, score = score_best, status = status, t0 = t0,
       note = note, proposals = proposals)
}

#' First-improvement local search over the lazy neighbourhood scan
#'
#' Specification sections 5.4 and 7.4. Stop conditions (global budget or
#' deadline, then the phase allowance) are checked before every scan and,
#' through the scan's `stop_fn`, before every move. The first candidate that
#' improves by more than `tol` is taken and the scan restarts from it.
#' `local_optimum` (some feasible move, none improving) and
#' `empty_neighbourhood` (no feasible move) are reported only after a
#' complete scan; an incomplete scan reports its stop reason (`budget`,
#' `time_limit`, `phase_budget`) or `interrupted`.
#'
#' @inheritParams arc_anneal_phase
#' @param kind Move set: "joint", "column" or "row".
#' @param tol Improvement threshold, a number >= 0.
#' @param max_evals Call allowance, a number >= 0 (`Inf` allowed).
#' @return `list(contraction, score, status)`.
#' @noRd
arc_local_search <- function(budget, start, score_start, score_fn,
                             kind = "joint", tol = 1e-12, max_evals = Inf) {
  arc_search_check(arc_is_choice(kind, c("joint", "column", "row")),
                   "kind must be \"joint\", \"column\" or \"row\".")
  arc_search_check(arc_is_number(tol) && tol >= 0,
                   "tol must be a single number >= 0.")
  arc_search_check(arc_is_number(max_evals) && max_evals >= 0,
                   "max_evals must be a single number >= 0 (Inf allowed).")
  arc_search_check(is.function(score_fn), "score_fn must be a function.")
  current <- start
  score_current <- score_start
  f_current <- if (is.na(score_start)) -Inf else score_start
  stop_at <- arc_budget_used(budget) + max_evals
  why <- function() {
    out <- arc_budget_out(budget)
    if (!is.na(out)) return(out)
    if (arc_budget_used(budget) >= stop_at) return("phase_budget")
    NA_character_
  }
  done <- function(status) {
    list(contraction = current, score = score_current, status = status)
  }
  repeat {
    reason <- why()
    if (!is.na(reason)) return(done(reason))
    found <- NULL
    scan <- arc_scan_neighbourhood(current, kind, function(candidate) {
      y <- arc_budget_eval(budget, score_fn, candidate, "objective")
      if (is.null(y)) return(TRUE)
      if (!is.na(y) && y > f_current + tol) {
        found <<- list(contraction = candidate, score = y)
        return(TRUE)
      }
      !is.na(why())
    }, stop_fn = function() !is.na(why()))
    if (!is.null(found)) {
      current <- found$contraction
      score_current <- found$score
      f_current <- found$score
      next
    }
    if (!identical(scan$status, "complete")) {
      reason <- why()
      return(done(if (is.na(reason)) "interrupted" else reason))
    }
    return(done(if (scan$n_feasible == 0L) "empty_neighbourhood"
                else "local_optimum"))
  }
}

# ---- Search algorithms (specification sections 4, 7.1, 7.4, 8.1, 8.2) ----

#' Validate the arguments shared by every search algorithm
#'
#' Checks `max_evals`, `seconds`, `max_start_tries`, `now` and the shape of
#' `starts` (NULL or a list). A run with neither a finite evaluation budget
#' nor a finite deadline is refused: nothing would stop it. The contents of
#' `starts` are checked after the entry check by `arc_search_check_starts()`.
#'
#' @inheritParams arc_search_multistart
#' @noRd
arc_search_check_common <- function(max_evals, seconds, starts,
                                    max_start_tries, now) {
  arc_search_check(arc_is_number(max_evals) && max_evals >= 0,
                   "max_evals must be a single number >= 0 (Inf allowed).")
  arc_search_check(arc_is_number(seconds),
                   "seconds must be a single non-missing number.")
  arc_search_check(is.finite(max_evals) || is.finite(seconds),
                   "max_evals or seconds must be finite.")
  arc_search_check(arc_is_whole_number(max_start_tries) &&
                     max_start_tries >= 1,
                   "max_start_tries must be a single whole number >= 1.")
  arc_search_check(is.function(now), "now must be a function.")
  arc_search_check(is.null(starts) || is.list(starts),
                   "starts must be NULL or a list of contractions.")
  invisible(TRUE)
}

#' Validate supplied start contractions
#'
#' Each element must be a k x s restricted contraction accepted by
#' `arc_check_contraction(..., v, "restricted")`; any failure is raised as
#' the classed search input error.
#'
#' @inheritParams arc_search_multistart
#' @return The starts as integer matrices.
#' @noRd
arc_search_check_starts <- function(starts, v, s, k) {
  if (is.null(starts)) {
    return(NULL)
  }
  lapply(seq_along(starts), function(i) {
    x <- starts[[i]]
    arc_search_check(is.matrix(x) && identical(dim(x), as.integer(c(k, s))),
                     paste0("starts[[", i, "]] must be a ", k, " x ", s,
                            " matrix."))
    tryCatch(arc_check_contraction(x, v, "restricted"),
             fieldhub_error = function(e) {
               fieldhub_abort("starts[[", i, "]] is not a valid restricted ",
                              "contraction: ", conditionMessage(e),
                              class = c("fieldhub_arc_input_error",
                                        "fieldhub_input_error"),
                              call = NULL)
             })
    storage.mode(x) <- "integer"
    dimnames(x) <- NULL
    x
  })
}

#' Bounded start search for one restart
#'
#' Specification section 4: at most `max_tries` scored starts, each scored
#' through the budget (every scored start counts). A supplied start is the
#' first candidate; later candidates come from `arc_random_start()`, whose
#' repair stops with the budget.
#'
#' @param budget Budget environment.
#' @param v,s,k Dimensions.
#' @param score_fn Score function (larger is better, `NA` disconnected).
#' @param kind Call kind, "objective" or "proxy".
#' @param max_tries Largest number of candidate starts.
#' @param supplied A start contraction or `NULL`.
#' @return `list(contraction, score, status)`; `contraction` is `NULL` with
#'   status `budget`, `time_limit` or `start_search_exhausted` on failure.
#' @noRd
arc_search_start <- function(budget, v, s, k, score_fn, kind, max_tries,
                             supplied = NULL) {
  should_stop <- function() !is.na(arc_budget_out(budget))
  fail <- function(status) list(contraction = NULL, score = NA_real_,
                                status = status)
  for (try in seq_len(max_tries)) {
    out <- arc_budget_out(budget)
    if (!is.na(out)) {
      return(fail(out))
    }
    candidate <- if (try == 1L && !is.null(supplied)) {
      supplied
    } else {
      arc_random_start(v, s, k, should_stop = should_stop)
    }
    if (is.null(candidate)) next
    y <- arc_budget_eval(budget, score_fn, candidate, kind)
    if (is.null(y)) {
      return(fail(arc_budget_out(budget)))
    }
    if (!is.na(y)) {
      return(list(contraction = candidate, score = y, status = "ok"))
    }
  }
  fail("start_search_exhausted")
}

#' Supplied start of restart `i`, or `NULL`
#' @param starts `NULL` or a list of contractions.
#' @param i Restart index.
#' @noRd
arc_search_supplied <- function(starts, i) {
  if (i <= length(starts)) starts[[i]] else NULL
}

#' Run code under a temporary, earlier deadline
#'
#' The budget deadline becomes `min(current, deadline)` while `code` runs and
#' is restored on exit, also after an error.
#'
#' @param budget Budget environment.
#' @param deadline Temporary deadline (`Inf` leaves it unchanged).
#' @param code Code to evaluate (lazily).
#' @noRd
arc_search_with_deadline <- function(budget, deadline, code) {
  saved <- budget$deadline
  on.exit(budget$deadline <- saved, add = TRUE)
  budget$deadline <- min(saved, deadline)
  force(code)
}

#' Run a local search and append its trace row
#'
#' @inheritParams arc_local_search
#' @param label Phase label for the trace.
#' @noRd
arc_search_polish <- function(budget, start, score_start, score_fn, label,
                              max_evals = Inf) {
  used <- arc_budget_used(budget)
  res <- arc_local_search(budget, start, score_start, score_fn, "joint",
                          max_evals = max_evals)
  arc_budget_add_trace(budget, data.frame(
    phase = label, f_start = score_start, f_best = res$score, t0 = NA_real_,
    evals = arc_budget_used(budget) - used, accepted = NA_integer_,
    worse_accepted = NA_integer_, status = res$status,
    stringsAsFactors = FALSE
  ))
  res
}

#' Is `x` a better state than `best`? (finite score, larger wins)
#' @param x,best Lists with `contraction` and `score` (`best` may be NULL).
#' @noRd
arc_search_better <- function(x, best) {
  !is.null(x$contraction) && !is.na(x$score) && is.finite(x$score) &&
    (is.null(best) || x$score > best$score)
}

#' Final verification and the result of a search run
#'
#' Specification sections 7.1 and 8.1. Outside the search budget and timed
#' separately (with the budget clock): the best design is checked by
#' `arc_validate_contraction(..., "restricted")` (its low-df warning is
#' muffled, other warnings pass through) and re-scored by
#' `arc_final_score()`. The reported score is the final authority's value of
#' the objective in natural units (E_aug, V_tt or V_tc); the inner-loop score
#' is kept as `search_score`. A failure is a classed internal error.
#'
#' @param budget Budget environment.
#' @param best `NULL` or `list(contraction, score)`.
#' @param v Pseudo-treatments.
#' @param objective Objective name.
#' @param phases Named per-phase statuses.
#' @param seed Seed of the run.
#' @param algorithm Algorithm name.
#' @param fail_status Status when no design was found.
#' @return The result of `arc_search_result()` with `objective`, `algorithm`
#'   and `search_score` appended.
#' @noRd
arc_search_finish <- function(budget, best, v, objective, phases, seed,
                              algorithm, fail_status = "no_design") {
  search_secs <- arc_budget_elapsed(budget)
  internal <- c("fieldhub_arc_internal_error", "fieldhub_internal_error")
  design <- NULL
  score <- NA_real_
  final <- NULL
  valid <- NA
  verify_secs <- 0
  if (!is.null(best)) {
    design <- best$contraction
    t_verify <- budget$now()
    checked <- withCallingHandlers(
      tryCatch(arc_validate_contraction(design, v, "restricted"),
               fieldhub_error = function(e) {
                 fieldhub_abort("Internal error: the search returned a ",
                                "design that fails validation: ",
                                conditionMessage(e), class = internal,
                                data = list(design = design, cause = e),
                                call = NULL)
               }),
      fieldhub_arc_low_df_warning = function(w) {
        invokeRestart("muffleWarning")
      })
    final <- arc_final_score(design, v, "restricted")
    if (!isTRUE(checked$connected) || !isTRUE(final$connected)) {
      fieldhub_abort("Internal error: the search returned a disconnected ",
                     "design.", class = internal,
                     data = list(design = design), call = NULL)
    }
    score <- final[[objective]]
    valid <- TRUE
    verify_secs <- budget$now() - t_verify
  }
  result <- arc_search_result(
    design = design, score = score,
    status = if (is.null(design)) fail_status else "ok",
    phases = phases, trace = arc_budget_trace(budget),
    calls = arc_budget_calls(budget), proposals = budget$proposals,
    failed = budget$failed, disconnected = budget$disconnected,
    search_secs = search_secs, seed = seed, verify_secs = verify_secs,
    total_secs = search_secs + verify_secs, final = final, valid = valid
  )
  c(result, list(objective = objective, algorithm = algorithm,
                 search_score = if (is.null(best)) NA_real_ else best$score))
}

#' Status of a run that found no design
#' @param last_start Status of the last failed start search, or NA.
#' @noRd
arc_search_fail_status <- function(last_start) {
  if (identical(last_start, "start_search_exhausted")) last_start
  else "no_design"
}

#' Multistart first-improvement local search
#'
#' Specification section 7.1: first-improvement local search over the full
#' A/B/C/K neighbourhood from fresh random starts until the single global
#' budget (evaluation cap, deadline, or both) is spent. Restart i uses
#' `starts[[i]]` as its first candidate start when supplied. Final
#' verification by `arc_final_score()` is outside the budget.
#'
#' @param v,s,k Field rows, columns and checks per column.
#' @param objective One of `arc_search_objectives`.
#' @param max_evals Global cap on objective plus proxy calls (`Inf` for
#'   equal-time runs).
#' @param seconds Seconds until the global deadline (`Inf` allowed when
#'   `max_evals` is finite).
#' @param seed `NULL` or a single number (FielDHub seed contract).
#' @param starts `NULL` or a list of k x s restricted contractions for
#'   matched-start experiments.
#' @param max_start_tries Scored starts allowed per restart.
#' @param now Clock function (seconds).
#' @return A search result (`arc_search_result()` fields plus `objective`,
#'   `algorithm` and `search_score`).
#' @noRd
arc_search_multistart <- function(v, s, k, objective = "E_aug",
                                  max_evals = 60000, seconds = Inf,
                                  seed = NULL, starts = NULL,
                                  max_start_tries = 20L, now = arc_clock) {
  arc_search_check_common(max_evals, seconds, starts, max_start_tries, now)
  seed <- resolve_seed(seed)
  local_design_seed(seed)
  status <- arc_search_entry(v, s, k, objective)
  if (!is.na(status)) {
    return(c(arc_search_result(status = status, seed = seed),
             list(objective = objective, algorithm = "multistart",
                  search_score = NA_real_)))
  }
  starts <- arc_search_check_starts(starts, v, s, k)
  budget <- arc_budget_new(max_evals, seconds, now = now)
  prep <- arc_search_prep(v, s, k)
  score_fn <- function(x) arc_search_score(x, prep, objective)
  best <- NULL
  phases <- character(0)
  last_start <- NA_character_
  restart <- 0L
  while (is.na(arc_budget_out(budget))) {
    restart <- restart + 1L
    tag <- paste0("r", restart, ":")
    start <- arc_search_start(budget, v, s, k, score_fn, "objective",
                              max_start_tries,
                              arc_search_supplied(starts, restart))
    phases[[paste0(tag, "start")]] <- start$status
    if (is.null(start$contraction)) {
      last_start <- start$status
      break
    }
    if (arc_search_better(start, best)) best <- start
    res <- arc_search_polish(budget, start$contraction, start$score,
                             score_fn, paste0(tag, "local"))
    phases[[paste0(tag, "local")]] <- res$status
    if (arc_search_better(res, best)) best <- res
  }
  arc_search_finish(budget, best, v, objective, phases, seed, "multistart",
                    arc_search_fail_status(last_start))
}

#' Plain annealing on A/B/C/K moves, then polish
#'
#' Specification sections 7.1 and 7.4: one annealing run with
#' `1 - polish_share` of the budget (Kempe share `kempe_share`, end-to-start
#' temperature ratio `t_ratio`), then first-improvement polish with the
#' rest. In equal-time mode (`max_evals = Inf`) the polish share of the
#' remaining time is reserved through a temporary deadline.
#'
#' @inheritParams arc_search_multistart
#' @param polish_share Share of the budget for polish, in [0, 1].
#' @param kempe_share Kempe-move probability of the joint sampler, in [0, 1].
#' @param t_ratio End-to-start temperature ratio, in (0, 1].
#' @noRd
arc_search_anneal <- function(v, s, k, objective = "E_aug",
                              max_evals = 60000, seconds = Inf, seed = NULL,
                              starts = NULL, max_start_tries = 20L,
                              now = arc_clock, polish_share = 0.1,
                              kempe_share = 0.3, t_ratio = 1e-3) {
  arc_search_check_common(max_evals, seconds, starts, max_start_tries, now)
  arc_search_check(arc_is_number(polish_share) && polish_share >= 0 &&
                     polish_share <= 1,
                   "polish_share must be a single number in [0, 1].")
  arc_search_check(arc_is_number(kempe_share) && kempe_share >= 0 &&
                     kempe_share <= 1,
                   "kempe_share must be a single number in [0, 1].")
  arc_search_check(arc_is_number(t_ratio) && t_ratio > 0 && t_ratio <= 1,
                   "t_ratio must be a single number in (0, 1].")
  seed <- resolve_seed(seed)
  local_design_seed(seed)
  status <- arc_search_entry(v, s, k, objective)
  if (!is.na(status)) {
    return(c(arc_search_result(status = status, seed = seed),
             list(objective = objective, algorithm = "anneal",
                  search_score = NA_real_)))
  }
  starts <- arc_search_check_starts(starts, v, s, k)
  budget <- arc_budget_new(max_evals, seconds, now = now)
  prep <- arc_search_prep(v, s, k)
  score_fn <- function(x) arc_search_score(x, prep, objective)
  phases <- character(0)
  start <- arc_search_start(budget, v, s, k, score_fn, "objective",
                            max_start_tries, arc_search_supplied(starts, 1L))
  phases[["r1:start"]] <- start$status
  if (is.null(start$contraction)) {
    return(arc_search_finish(budget, NULL, v, objective, phases, seed,
                             "anneal", arc_search_fail_status(start$status)))
  }
  left <- arc_budget_left(budget)
  if (is.finite(left)) {
    phase_evals <- floor((1 - polish_share) * left)
    deadline <- Inf
  } else {
    phase_evals <- Inf
    deadline <- budget$now() + (1 - polish_share) * arc_budget_secs_left(budget)
  }
  annealed <- arc_search_with_deadline(budget, deadline, arc_anneal_phase(
    budget, start$contraction, start$score, score_fn,
    function(x) arc_propose_joint(x, kempe_share = kempe_share),
    phase_evals, "joint", t_ratio = t_ratio, label = "r1:joint"
  ))
  phases[["r1:joint"]] <- annealed$status
  best <- if (arc_search_better(annealed, start)) annealed else start
  polished <- arc_search_polish(budget, best$contraction, best$score,
                                score_fn, "r1:polish")
  phases[["r1:polish"]] <- polished$status
  if (arc_search_better(polished, best)) best <- polished
  arc_search_finish(budget, best, v, objective, phases, seed, "anneal")
}

#' Staged annealing with recolouring restarts
#'
#' Specification sections 7.1 and 7.4. `restarts` restarts share the single
#' global budget; each restart's allowance (an equal split of what is left
#' over the remaining restarts) goes by `shares` to column annealing on the
#' column-efficiency proxy, row annealing (A/K) from `recolourings`
#' recolourings, joint annealing (A/B/C/K, start temperature multiplied by
#' `joint_t0_multiplier`) and first-improvement polish. In square designs
#' (v == s) the row share goes to the column phase and the column state is
#' recoloured once. When the column share is positive the start is scored by
#' the proxy (a proxy call); otherwise by the objective. In equal-time mode
#' (`max_evals = Inf`) allowances are times enforced by temporary phase
#' deadlines and the annealing phases cool by elapsed fraction.
#'
#' @inheritParams arc_search_anneal
#' @param restarts Whole number >= 1 (R).
#' @param recolourings Whole number >= 1 (m), recolourings per restart.
#' @param shares Named non-negative shares `column`, `row`, `joint`,
#'   `polish` summing to 1.
#' @param kempe_share Named Kempe probabilities `joint` and `row` in [0, 1].
#' @param joint_t0_multiplier Finite number > 0 multiplying the joint
#'   phase's calibrated start temperature (default 1).
#' @noRd
arc_search_staged <- function(v, s, k, objective = "E_aug",
                              max_evals = 60000, seconds = Inf, seed = NULL,
                              starts = NULL, max_start_tries = 20L,
                              now = arc_clock, restarts = 4L,
                              recolourings = 6L,
                              shares = c(column = 0.4, row = 0.3,
                                         joint = 0.2, polish = 0.1),
                              kempe_share = c(joint = 0.3, row = 0.6),
                              t_ratio = 1e-3, joint_t0_multiplier = 1) {
  arc_search_check_common(max_evals, seconds, starts, max_start_tries, now)
  arc_search_check(arc_is_whole_number(restarts) && restarts >= 1,
                   "restarts must be a single whole number >= 1.")
  arc_search_check(arc_is_whole_number(recolourings) && recolourings >= 1,
                   "recolourings must be a single whole number >= 1.")
  share_names <- c("column", "row", "joint", "polish")
  arc_search_check(is.numeric(shares) && length(shares) == 4L &&
                     setequal(names(shares), share_names) &&
                     !anyNA(shares) && all(shares >= 0) &&
                     abs(sum(shares) - 1) < 1e-9,
                   paste("shares must be named column, row, joint, polish,",
                         "non-negative and sum to 1."))
  arc_search_check(is.numeric(kempe_share) && length(kempe_share) == 2L &&
                     setequal(names(kempe_share), c("joint", "row")) &&
                     !anyNA(kempe_share) && all(kempe_share >= 0) &&
                     all(kempe_share <= 1),
                   "kempe_share must be named joint, row, with values in [0, 1].")
  arc_search_check(arc_is_number(t_ratio) && t_ratio > 0 && t_ratio <= 1,
                   "t_ratio must be a single number in (0, 1].")
  arc_search_check(arc_is_number(joint_t0_multiplier) &&
                     is.finite(joint_t0_multiplier) &&
                     joint_t0_multiplier > 0,
                   "joint_t0_multiplier must be a single finite number > 0.")
  seed <- resolve_seed(seed)
  local_design_seed(seed)
  status <- arc_search_entry(v, s, k, objective)
  if (!is.na(status)) {
    return(c(arc_search_result(status = status, seed = seed),
             list(objective = objective, algorithm = "staged",
                  search_score = NA_real_)))
  }
  starts <- arc_search_check_starts(starts, v, s, k)
  budget <- arc_budget_new(max_evals, seconds, now = now)
  prep <- arc_search_prep(v, s, k)
  score_fn <- function(x) arc_search_score(x, prep, objective)
  proxy_fn <- function(x) arc_column_proxy(x, v)
  square <- v == s
  shares <- shares[share_names]
  if (square) {
    shares[["column"]] <- shares[["column"]] + shares[["row"]]
    shares[["row"]] <- 0
  }
  timed <- !is.finite(budget$max_evals)
  propose_column <- function(x) arc_propose_column(x)
  propose_row <- function(x) {
    arc_propose_row(x, kempe_share = kempe_share[["row"]])
  }
  propose_joint <- function(x) {
    arc_propose_joint(x, kempe_share = kempe_share[["joint"]])
  }
  best <- NULL
  phases <- character(0)
  last_start <- NA_character_

  for (restart in seq_len(restarts)) {
    if (!is.na(arc_budget_out(budget))) break
    tag <- paste0("r", restart, ":")
    remaining <- restarts - restart + 1L
    # Allowance of one phase: calls (evaluation mode) or a deadline
    # computed when the phase starts (equal-time mode).
    if (timed) {
      per <- arc_budget_secs_left(budget) / remaining
      allowance <- function(share) {
        list(evals = Inf, deadline = budget$now() + share * per)
      }
    } else {
      per <- floor(arc_budget_left(budget) / remaining)
      allowance <- function(share) {
        list(evals = floor(share * per), deadline = Inf)
      }
    }
    anneal <- function(start, score, fn, propose, share, exhaustion, label,
                       kind = "objective", t0_multiplier = 1) {
      a <- allowance(share)
      arc_search_with_deadline(budget, a$deadline, arc_anneal_phase(
        budget, start, score, fn, propose, a$evals, exhaustion,
        t_ratio = t_ratio, kind = kind, label = label,
        t0_multiplier = t0_multiplier
      ))
    }

    use_proxy <- shares[["column"]] > 0
    start <- arc_search_start(
      budget, v, s, k, if (use_proxy) proxy_fn else score_fn,
      if (use_proxy) "proxy" else "objective", max_start_tries,
      arc_search_supplied(starts, restart)
    )
    phases[[paste0(tag, "start")]] <- start$status
    if (is.null(start$contraction)) {
      last_start <- start$status
      if (identical(start$status, "start_search_exhausted")) next
      break
    }
    if (use_proxy) {
      column <- anneal(start$contraction, start$score, proxy_fn,
                       propose_column, shares[["column"]], "column",
                       paste0(tag, "column"), kind = "proxy")
      phases[[paste0(tag, "column")]] <- column$status
      column_state <- column$contraction
    } else {
      phases[[paste0(tag, "column")]] <- "skipped"
      column_state <- start$contraction
    }

    current <- NULL
    if (!square && shares[["row"]] > 0) {
      for (q in seq_len(recolourings)) {
        if (!is.na(arc_budget_out(budget))) break
        label <- paste0(tag, "row", q)
        x0 <- if (q == 1L) {
          arc_recolor_rows(column_state, v)
        } else {
          arc_random_recolor(column_state, v)
        }
        y <- arc_budget_eval(budget, score_fn, x0, "objective")
        if (is.null(y)) break
        if (is.na(y)) {
          phases[[label]] <- "skipped"
          next
        }
        rows <- anneal(x0, y, score_fn, propose_row,
                       shares[["row"]] / recolourings, "row", label)
        phases[[label]] <- rows$status
        if (arc_search_better(rows, current)) current <- rows
      }
    } else {
      x0 <- arc_recolor_rows(column_state, v)
      y <- arc_budget_eval(budget, score_fn, x0, "objective")
      if (!is.null(y) && !is.na(y)) {
        current <- list(contraction = x0, score = y)
      }
      phases[[paste0(tag, "recolour")]] <- if (is.null(y)) {
        arc_budget_out(budget)
      } else if (is.na(y)) "skipped" else "ok"
    }
    if (is.null(current)) next
    if (arc_search_better(current, best)) best <- current

    joint <- anneal(current$contraction, current$score, score_fn,
                    propose_joint, shares[["joint"]], "joint",
                    paste0(tag, "joint"),
                    t0_multiplier = joint_t0_multiplier)
    phases[[paste0(tag, "joint")]] <- joint$status
    if (arc_search_better(joint, current)) current <- joint

    a <- allowance(shares[["polish"]])
    polished <- arc_search_with_deadline(budget, a$deadline,
      arc_search_polish(budget, current$contraction, current$score,
                        score_fn, paste0(tag, "polish"), max_evals = a$evals))
    phases[[paste0(tag, "polish")]] <- polished$status
    if (arc_search_better(polished, current)) current <- polished
    if (arc_search_better(current, best)) best <- current
  }
  arc_search_finish(budget, best, v, objective, phases, seed, "staged",
                    arc_search_fail_status(last_start))
}
