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
