#' Treatment and block incidence without unused factor levels
#' @noRd
block_incidence <- function(treatment, block) {
  if (!is.atomic(treatment) || !is.atomic(block) || !is.null(dim(treatment)) ||
      !is.null(dim(block)) || length(treatment) != length(block) ||
      length(treatment) == 0L || anyNA(treatment) || anyNA(block)) {
    fieldhub_abort("Treatment and block labels must be complete vectors of equal length.")
  }
  incidence <- table(factor(treatment), factor(block))
  if (nrow(incidence) < 2L) {
    fieldhub_abort("At least two treatments are needed for an efficiency comparison.")
  }
  incidence
}

#' Connectivity of the treatment/block incidence graph
#'
#' Each nonterminal expansion reaches another treatment, so at most the number
#' of treatments is required. No numerical rank tolerance decides connectivity.
#' @noRd
block_model_connected <- function(incidence) {
  reached <- seq_len(nrow(incidence)) == 1L
  for (step in seq_len(nrow(incidence))) {
    blocks <- colSums(incidence[reached, , drop = FALSE]) > 0
    expanded <- unname(rowSums(incidence[, blocks, drop = FALSE]) > 0)
    if (all(expanded)) return(TRUE)
    if (identical(expanded, reached)) return(FALSE)
    reached <- expanded
  }
  FALSE
}

#' Treatments or blocks (the smaller count) above which the efficiency worker
#' reads the canonical efficiencies from the Gram matrix of the smaller side
#' (`block_incidence_gram_values()`). Up to this size the singular value
#' decomposition is kept, so every report of a design with at most 500
#' treatments or at most 500 blocks is computed as before.
#' @noRd
block_gram_limit <- 500

#' Squared singular values of the association matrix of an incidence table,
#' largest first, from the Gram matrix of its smaller side
#'
#' With X = D_r^(-1/2) N D_k^(-1/2), rho = sqrt(r / n) and kappa =
#' sqrt(k / n), the association matrix is A = X - rho kappa'. X kappa = rho
#' and X' rho = kappa, so A A' = X X' - rho rho' and A' A = X' X - kappa
#' kappa'. The product of the smaller order is summed over the nonzero
#' counts that share a block (or a treatment): a block of q distinct
#' treatments gives q^2 terms, so no dense v x b product is formed. The
#' eigenvalues of that symmetric matrix are the squared singular values of
#' A. Returns NULL when the terms are more than the v b cells of the
#' incidence table (large blocks: summing them is then slower than the
#' decomposition, 1.34 s against 0.19 s at 600 treatments in 601 blocks of
#' 150); the caller then uses the singular value decomposition.
#'
#' Measured 2026-11-22 (Task 8 Step 5, ruling R10.4): the Row report of a
#' nested 2000/1000/4 onestage design (2000 treatments, 4000 blocks of two)
#' took 14.2 s by the decomposition of the 2000 x 4000 matrix, of a
#' `row_column()` call of 33 s.
#' @noRd
block_incidence_gram_values <- function(incidence, replication, block_size, total) {
  by_treatment <- nrow(incidence) <= ncol(incidence)
  cells <- which(incidence > 0)
  treatment <- row(incidence)[cells]
  block <- col(incidence)[cells]
  own <- if (by_treatment) treatment else block
  group <- if (by_treatment) block else treatment
  value <- as.vector(incidence)[cells] / sqrt(unname(replication)[treatment] * unname(block_size)[block])
  in_order <- order(group)
  own <- own[in_order]
  value <- value[in_order]
  group <- group[in_order]
  size <- tabulate(group)
  if (sum(as.double(size)^2) > as.double(nrow(incidence)) * ncol(incidence)) return(NULL)
  first <- cumsum(c(1L, size))[group]
  left <- rep(seq_along(own), size[group])
  right <- sequence(size[group], from = first)
  order_of <- min(dim(incidence))
  terms <- rowsum(value[left] * value[right], (own[left] - 1) * order_of + own[right])
  gram <- numeric(order_of * order_of)
  gram[as.numeric(rownames(terms))] <- as.vector(terms)
  mean_direction <- sqrt((if (by_treatment) replication else block_size) / total)
  gram <- matrix(gram, order_of, order_of) - tcrossprod(mean_direction)
  sort(eigen(gram, symmetric = TRUE, only.values = TRUE)$values, decreasing = TRUE)
}

#' Canonical block efficiencies from an incidence table
#'
#' Removing the mean from D_r^(-1/2) N D_k^(-1/2) leaves treatment/block
#' association. Its nontrivial squared singular values are losses relative to
#' complete randomization. Use the smaller singular system, not plot-by-plot
#' projection matrices. Disconnected comparisons have zero overall efficiency.
#'
#' `digits = NULL` returns the unrounded values. `gram` chooses how the
#' squared singular values are computed: TRUE from the Gram matrix of the
#' smaller side (the decomposition where that function declines), FALSE
#' from the singular value decomposition, NULL (the default) the Gram matrix
#' above `block_gram_limit` treatments and blocks.
#' Both give the same values (checked to 1e-10 unrounded by
#' `tools/check-onestage-engine.R report`).
#' @noRd
block_incidence_efficiency <- function(incidence, digits = 7, gram = NULL) {
  if (!block_model_connected(incidence)) return(list(Deffic = 0, Aeffic = 0))
  replication <- rowSums(incidence)
  block_size <- colSums(incidence)
  total <- sum(replication)
  if (is.null(gram)) gram <- min(dim(incidence)) > block_gram_limit
  squared <- if (gram) block_incidence_gram_values(incidence, replication, block_size, total)
  if (is.null(squared)) {
    association <- incidence / sqrt(outer(replication, block_size)) -
      outer(sqrt(replication / total), sqrt(block_size / total))
    squared <- svd(association, nu = 0L, nv = 0L)$d^2
  }
  # Drop the zero singular direction for the mean. Additional treatment
  # directions outside the smaller system have efficiency one.
  efficiencies <- c(1 - squared[seq_len(length(squared) - 1L)],
                     rep(1, nrow(incidence) - min(dim(incidence))))
  if (any(!is.finite(efficiencies)) || any(efficiencies <= 0)) {
    fieldhub_abort("The connected block model is numerically singular.",
                   class = "fieldhub_numerical_error")
  }
  D <- exp(mean(log(efficiencies)))
  A <- length(efficiencies) / sum(1 / efficiencies)
  if (is.null(digits)) list(Deffic = D, Aeffic = A) else list(Deffic = round(D, digits), Aeffic = round(A, digits))
}

#' Convexity upper bound for the A-efficiency of an equireplicate one-way design
#'
#' With efficiencies of C/r on the contrast space, at least max(0, v - b) equal 1;
#' the remaining m = min(v, b) - 1 sum to m - T with T at its minimum when every
#' block's counts are as even as possible; the harmonic mean is then at most
#' (v - 1) / (max(0, v - b) + m^2 / (m - T)). `rank` replaces b in m when the
#' block incidence has a smaller known rank (resolvable designs,
#' `resolvable_a_bound()`); the default keeps the one-way bound unchanged.
#' Edge cases (spec 5.2b, tolerance 1e-12): m <= 0 or T <= 0 gives 1, and
#' m - T <= 0 gives 0 (every design of the shape is disconnected). This is the
#' `convexity` component of `fieldhub_a_bound()`, so one formula exists.
#' @noRd
block_a_bound <- function(v, r, k, rank = length(k)) {
  lo <- k %/% v
  hi <- k %% v
  t_min <- sum((hi * (lo + 1)^2 + (v - hi) * lo^2) / (r * k)) - 1
  m <- min(v, rank) - 1
  if (m <= 0 || t_min <= 1e-12) return(1)
  if (m - t_min <= 1e-12) return(0)
  (v - 1) / (max(0, v - rank) + m^2 / (m - t_min))
}

#' A-efficiency bound of a resolvable design (Level 2, ruling R18)
#'
#' The r replicates' block indicators each sum to the all-ones vector, so the
#' incidence N of the B = r b blocks has rank at most B - r + 1 and at most
#' m = min(v, B - r + 1) - 1 contrast efficiencies differ from 1. With the
#' same sum argument as `block_a_bound()` (T = b - 1 for binary blocks of
#' v / b plots) the harmonic mean is at most (v - 1 - m + m^2 / (m - T)). A
#' square lattice with r <= q + 1 orthogonal replicates attains it. Own
#' derivation (controller ruling R18, checked on 3000 random resolvable
#' designs). It is the `convexity` component of `fieldhub_a_bound()` in the
#' resolvable class; the engine's early exits keep comparing against it so
#' that no seeded design changes (spec 6.2). The reported bound is the
#' helper's `bound`.
#' @noRd
resolvable_a_bound <- function(v, r, blocks) {
  fieldhub_a_bound(v, r, v %/% blocks, r * blocks, "resolvable", R = r)[["convexity"]]
}

#' Least sum of squares of `n` non-negative integers that sum to `a`
#'
#' The most even split: q = a div n in every entry and one more in a mod n of
#' them (spec 5.2b, g(a, n)).
#' @noRd
a_bound_g <- function(a, n) {
  q <- a %/% n
  n * q^2 + (2 * q + 1) * (a %% n)
}

#' Least squared norm of one side of N N' or N' N (spec 5.2b, Step 2)
#'
#' A side with `n` rows whose row sums are `total` and whose diagonal entries
#' are at least `d_min`: n times the minimum over integers d from `d_min` to
#' `total` of d^2 + g(total - d, n - 1). With k <= v the minimum is at d_min
#' (`closed`); otherwise it is found by direct evaluation. With one row the
#' side has no off-diagonal entries and its norm is total^2.
#' @noRd
a_bound_side <- function(n, d_min, total, closed) {
  if (n < 2) return(n * total^2)
  if (closed) return(n * (d_min^2 + a_bound_g(total - d_min, n - 1)))
  d <- seq.int(min(d_min, total), total)
  n * min(d^2 + a_bound_g(total - d, n - 1))
}

#' FielDHub's own A-efficiency bound for one block factor (spec 5.2, 5.2b)
#'
#' An upper bound on the A-efficiency of every design with `v` treatments,
#' each replicated `r` times, in `b` blocks of `k` plots, valid for the block
#' factor's class:
#' - `general`: equal replication and equal block size only (rank <= b);
#' - `nested`: blocks within `R` complete replicates that each hold every
#'   treatment c > 1 times (R = r / c; rank B - R + 1, ruling R18);
#' - `resolvable`: as `nested` with c = 1, so `R` must equal `r`.
#'
#' `R` is the number of replicate levels. It is required for `nested` and
#' `resolvable`, ignored for `general`, and never derived from `r`.
#'
#' @return A named double: `convexity` (spec 5.2 with the class's rank, the
#'   vector form `block_a_bound()`), `second_moment` (from L_gen, and for
#'   `resolvable` the smaller of the bounds from L_gen and L_res), `pair`
#'   (from L_pair for `resolvable` with r = 2, otherwise NA) and `bound`, the
#'   smallest of the components that are not NA.
#' @noRd
fieldhub_a_bound <- function(v, r, k, b, class = c("general", "nested", "resolvable"),
                             R = NULL) {
  class <- match.arg(class)
  tol <- 1e-12
  v <- as.double(v)
  r <- as.double(r)
  k <- as.double(k)
  b <- as.double(b)
  if (class == "general") {
    R <- NULL
  } else if (is.null(R)) {
    fieldhub_abort("The number of replicate levels R is required for a ", class, " block factor.")
  }
  if (class == "resolvable" && R != r) {
    fieldhub_abort("A resolvable block factor needs R equal to the replication r.")
  }
  rank <- if (class == "general") b else b - R + 1
  out <- c(bound = 1, convexity = 1, second_moment = 1,
           pair = if (class == "resolvable" && r == 2) 1 else NA_real_)
  m <- min(v, rank) - 1
  rk <- r * k
  t_min <- b * a_bound_g(k, v) / rk - 1
  if (v == 1 || m <= 0 || t_min <= tol) return(out)
  out[["convexity"]] <- block_a_bound(v, r, rep(k, b), rank = rank)
  # Step 2: S >= S_gen for every design of the shape.
  closed <- k <= v
  s_gen <- max(a_bound_side(v, a_bound_g(r, b), rk, closed),
               a_bound_side(b, a_bound_g(k, v), rk, closed)) / rk^2 - 1
  if (s_gen > tol) {
    # Step 3 (a): (*) at y = min(S_gen / T_min, 1/2), where it grows with T and S.
    y <- min(s_gen / t_min, 0.5)
    l_gen <- (v - 1) + (1 - 2 * y) / (1 - y)^2 * t_min + s_gen / (1 - y)^2
    out[["second_moment"]] <- (v - 1) / l_gen
  }
  if (class == "resolvable") {
    # Step 3 (b): T = s - 1 exactly; S >= S* = max(S_res, S_gen, T^2 / m).
    s <- v %/% k
    t_res <- s - 1
    g_ks <- a_bound_g(k, s)
    s_res <- (s + (r - 1) * s * g_ks / k^2) / r - 1
    s_star <- max(s_res, s_gen, t_res^2 / m)
    a_res <- if (s_star >= t_res - tol) 0 else (v - 1) / ((v - 1) + t_res^2 / (t_res - s_star))
    out[["second_moment"]] <- min(out[["second_moment"]], a_res)
    if (r == 2) {
      # Step 3 (c): the eigenvalues pair as (1 +- c_i) / 2.
      p <- s - 1
      q_min <- s * g_ks / k^2 - 1
      out[["pair"]] <- if (q_min >= p - tol || 2 * p > v - 1) 0 else
        (v - 1) / ((v - 1) - 2 * p + 4 * p^2 / (p - q_min))
    }
  }
  out[["bound"]] <- min(out[c("convexity", "second_moment", "pair")], na.rm = TRUE)
  out
}

#' Verified class of a block factor claimed nested in a replicate factor
#'
#' The check of spec 6.2, exactly: every block lies in one level of
#' `replicate`, and one c >= 1 is shared by every treatment and every
#' replicate level that holds plots (each such level holds every treatment c
#' times). Then R is the number of those levels and the class is
#' `resolvable` (c = 1) or `nested` (c > 1). Otherwise the class is `general`
#' with no R. This verifies a stated claim; it does not guess.
#' @noRd
block_factor_class <- function(treatments, block, replicate) {
  general <- list(class = "general", R = NULL)
  replicate <- factor(replicate)
  levels_per_block <- tapply(as.integer(replicate), factor(block),
                             function(x) length(unique(x)))
  if (any(levels_per_block != 1L)) return(general)
  counts <- table(factor(treatments), droplevels(replicate))
  c_value <- unique(as.vector(counts))
  if (length(c_value) != 1L || c_value < 1L) return(general)
  list(class = if (c_value == 1L) "resolvable" else "nested", R = ncol(counts))
}

#' Efficiency report for named treatment, plot, and block-factor columns
#'
#' The `A-Bound` is FielDHub's own `fieldhub_a_bound()` for equal replication
#' and equal block sizes, and 1 otherwise. `nested_in_replicates` (recycled
#' over the block columns) claims that a block factor is nested in the
#' replicate factor named by `replicates`; the claim is verified by
#' `block_factor_class()`, and an unclaimed factor is `general`.
#' Retain the established report column names, order, and seven-digit rounding.
#' `known`, if given, holds the D- and A-efficiencies of each block factor in
#' order (columns `D-Efficiency` and `A-Efficiency`, as the block engine
#' reports them): they are reported, rounded, instead of being recomputed with
#' the singular value decomposition.
#' @noRd
BlockEfficiencies <- function(Design, known = NULL, nested_in_replicates = FALSE,
                              replicates = NULL) {
  if (!is.data.frame(Design) || nrow(Design) == 0L || anyNA(names(Design)) ||
      anyDuplicated(names(Design)) || !all(c("plots", "treatments") %in% names(Design))) {
    fieldhub_abort("A block model needs named plots, treatments, and block-factor columns.")
  }
  block_columns <- setdiff(names(Design), c("plots", "treatments"))
  if (!length(block_columns)) fieldhub_abort("A block model needs at least one block factor.")
  if (!is.logical(nested_in_replicates) || !length(nested_in_replicates) ||
      anyNA(nested_in_replicates)) {
    fieldhub_abort("`nested_in_replicates` must be TRUE or FALSE.")
  }
  nested_in_replicates <- rep_len(nested_in_replicates, length(block_columns))
  if (any(nested_in_replicates) &&
      (!is.character(replicates) || length(replicates) != 1L ||
       !replicates %in% names(Design))) {
    fieldhub_abort("A block factor claimed nested in replicates needs the replicate column.")
  }
  reports <- lapply(seq_along(block_columns), function(level) {
    block <- Design[[block_columns[level]]]
    incidence <- block_incidence(Design$treatments, block)
    replication <- unique(unname(rowSums(incidence)))
    block_size <- unique(unname(colSums(incidence)))
    bound <- if (length(replication) == 1L && length(block_size) == 1L) {
      class <- if (nested_in_replicates[level]) {
        block_factor_class(Design$treatments, block, Design[[replicates]])
      } else list(class = "general", R = NULL)
      fieldhub_a_bound(nrow(incidence), replication, block_size, ncol(incidence),
                       class$class, R = class$R)[["bound"]]
    } else 1
    efficiency <- if (is.null(known)) {
      block_incidence_efficiency(incidence)
    } else {
      list(Deffic = round(known[["D-Efficiency"]][level], 7),
           Aeffic = round(known[["A-Efficiency"]][level], 7))
    }
    data.frame(Level = level, Blocks = ncol(incidence),
               `D-Efficiency` = efficiency$Deffic, `A-Efficiency` = efficiency$Aeffic,
               `A-Bound` = round(bound, 7), check.names = FALSE)
  })
  do.call(rbind, reports)
}
