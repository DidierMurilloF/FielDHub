# R prototype of the staged onestage search of phase 2 spec 6.1 / 6.2, used
# only to MEASURE the stage rule before the C++ engine is built (plan Task 4).
# It is a measuring tool, not package code, and it calls FielDHub's public
# row_column() only (for the nested stage 1).
#
#   Rscript tools/prototype-onestage.R <out.csv> [library] [seeds=8] [tmax=100]
#           [tset=200,300] [tseeds=2] [shard=i/k]
#
# Shapes: the `os` shapes of nested_golden_grid("os") (k is nrows) with
# t <= tmax, plus those with t in tset (run with tseeds seeds instead of
# seeds); tmax=0 runs the tset shapes only. Both codings: "nested"
# (latinize = FALSE) and "latinized" (latinize = TRUE, rows and columns shared
# across replicates). Seeds are 1:seeds. [library] defaults to
# ~/fieldhub-libs/dev.
#
# One unit = (shape, coding, seed). It runs 10 searches (spec 6.2); each has
# its own stage 1 design, and each swap set (floor, row_preserving) runs its
# own repair and joint stage from that stage 1 design. The unit writes 8 rows:
# swap set x S in {1, 2, 5, 10}, where S applies the return rule of spec 6.2
# to searches 1..S. Rows are appended as each unit finishes; rerunning with
# the same path skips units already present. <out>.meta.rds records versions.
#
# Stages (spec 6.1):
# - stage 1, rows first. Nested: the installed
#   row_column(t, nrows = ncols, reps, method = "twostage", seed), transposed
#   (its columns, of size ncols, are our rows). Latinized: a first-improvement
#   descent on Row log det from a random complete start, rows spanning
#   replicates, swaps within a replicate; then a descent on Column log det with
#   swaps in the same row of the same replicate.
# - repair, when stage 1 is not full rank: first improvement on
#   log det(C/r + eps P), eps = 1e-6, same swap set and Row floors as the
#   joint stage, stopping as soon as the full-rank test passes (smallest
#   canonical efficiency of C/r >= 1e-7).
# - joint stage: first improvement on the joint log det, either under the Row
#   D and Row A floors (running values against the fixed stage 1 values) or
#   restricted to row-preserving pairs (same row of the same replicate).
# Every descent visits the pairs in one random order, cyclically, and stops
# after a full cycle without an accepted swap or after max_pass cycles. No
# escapes, no restarts within a search.
#
# Work counters (summed over the S searches): accepted swaps, passes
# (ceiling(evaluations / pairs) per descent) and evaluations (pairs evaluated
# up to and including each accepted one, i.e. sequential first-improvement
# semantics), per stage. Stage 1 nested is the installed twostage, whose work
# is not visible: its counters are NA.
#
# Implementation: the joint information is C = rI - sum_f N_f N_f'/k_f + (r/v)J
# (spec 5.1, block-space form); one-way factors use C = rI - N N'/k. A swap of
# the treatments tp, tq of plots p, q changes C by a w' + w a' + c w w' with
# w = e_tq - e_tp, so every candidate is scored by a 2x2 determinant from the
# inverse of M = C/r + J/v (+ eps P), recomputed from scratch after every
# accepted swap (no drift).

arguments <- commandArgs(trailingOnly = TRUE)
named <- grepl("^[a-z]+=", arguments)
opts <- arguments[named]
positional <- arguments[!named]
usage <- "Usage: Rscript tools/prototype-onestage.R <out.csv> [library] [seeds=8] [tmax=100] [tset=200,300] [tseeds=2] [shard=i/k]"
if (length(positional) < 1L || length(positional) > 2L) stop(usage)
opt <- function(name, default) {
  x <- grep(paste0("^", name, "="), opts, value = TRUE)
  if (length(x) > 1L) stop(usage)
  if (length(x) == 0L) default else sub(paste0("^", name, "="), "", x)
}
unknown <- setdiff(sub("=.*$", "", opts), c("seeds", "tmax", "tset", "tseeds", "shard"))
if (length(unknown)) stop("Unknown argument(s): ", paste(unknown, collapse = ", "), "\n", usage)
path <- positional[1]
library_path <- if (length(positional) == 2L) positional[2] else "~/fieldhub-libs/dev"
n_seeds <- as.integer(opt("seeds", "8"))
tmax <- as.numeric(opt("tmax", "100"))
tset <- as.numeric(strsplit(opt("tset", ""), ",", fixed = TRUE)[[1]])
tseeds <- as.integer(opt("tseeds", "2"))
shard <- opt("shard", NA_character_)
if (is.na(n_seeds) || n_seeds < 1L || is.na(tmax) || anyNA(tset) || is.na(tseeds) || tseeds < 1L) stop(usage)
.libPaths(c(normalizePath(library_path, mustWork = TRUE), .libPaths()))
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
suppressMessages(library(FielDHub))
source(file.path(script_dir, "nested-goldens-helpers.R"))
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

EPS <- 1e-6          # repair regulariser, relative to C/r (spec 6.1)
FULL_RANK <- 1e-7    # full-rank test of spec 5.1b
TIE <- 1.5e-7        # return-rule ties (spec 6.2)
IMPROVE <- 1e-10     # a swap improves when the log det rises by more than this
FLOOR_TOL <- 1e-9    # Row floor tolerance on the running log det / trace
MAX_PASS <- 60L      # cycles through the pair list per descent
SEARCHES <- 10L
S_VALUES <- c(1L, 2L, 5L, 10L)

# ---- designs ---------------------------------------------------------------
# d: v, r, nrows, ncols, latin, rep, row, col (within-replicate indices),
# trt, lab (block labels per factor), k (block sizes), N (incidence matrices).
incidence <- function(trt, lab, v, b) matrix(tabulate((lab - 1L) * v + trt, v * b), v, b)
finish_design <- function(d) {
  if (d$latin) {
    d$lab <- list(row = d$row, col = d$col)
    d$k <- list(row = d$r * d$ncols, col = d$r * d$nrows)
    b <- c(row = d$nrows, col = d$ncols)
  } else {
    d$lab <- list(row = (d$rep - 1L) * d$nrows + d$row, col = (d$rep - 1L) * d$ncols + d$col)
    d$k <- list(row = d$ncols, col = d$nrows)
    b <- c(row = d$r * d$nrows, col = d$r * d$ncols)
  }
  d$N <- list(row = incidence(d$trt, d$lab$row, d$v, b[["row"]]),
              col = incidence(d$trt, d$lab$col, d$v, b[["col"]]))
  d
}
swap_plots <- function(d, p, q) {
  a <- d$trt[p]; b <- d$trt[q]
  for (f in c("row", "col")) {
    lp <- d$lab[[f]][p]; lq <- d$lab[[f]][q]
    if (lp != lq) {
      d$N[[f]][a, lp] <- d$N[[f]][a, lp] - 1L; d$N[[f]][a, lq] <- d$N[[f]][a, lq] + 1L
      d$N[[f]][b, lq] <- d$N[[f]][b, lq] - 1L; d$N[[f]][b, lp] <- d$N[[f]][b, lp] + 1L
    }
  }
  d$trt[c(p, q)] <- c(b, a)
  d
}
# Nested stage 1: installed twostage on the transposed shape. Its grid has
# ncols rows and nrows columns; its columns (size ncols) are our rows.
stage1_nested <- function(t, nrows, r, seed) {
  x <- suppressWarnings(suppressMessages(
    row_column(t = t, nrows = t / nrows, reps = r, seed = seed, method = "twostage")))
  fb <- x$fieldBook
  finish_design(list(v = t, r = r, nrows = nrows, ncols = t / nrows, latin = FALSE,
                     rep = as.integer(fb$REP), row = as.integer(fb$COLUMN),
                     col = as.integer(fb$ROW), trt = as.integer(fb$ENTRY)))
}
random_latin_start <- function(t, nrows, r) {
  ncols <- t / nrows
  finish_design(list(v = t, r = r, nrows = nrows, ncols = ncols, latin = TRUE,
                     rep = rep(seq_len(r), each = t),
                     row = rep(rep(seq_len(nrows), each = ncols), r),
                     col = rep(seq_len(ncols), nrows * r),
                     trt = as.vector(replicate(r, sample.int(t)))))
}

# ---- information, efficiencies, full rank -----------------------------------
info <- function(d, facs) {
  C <- d$r * diag(d$v)
  for (f in facs) C <- C - tcrossprod(d$N[[f]]) / d$k[[f]]
  if (length(facs) == 2L) C <- C + d$r / d$v
  C
}
effs <- function(d, facs) {
  ev <- eigen(info(d, facs) / d$r, symmetric = TRUE, only.values = TRUE)$values
  sort(ev, decreasing = TRUE)[seq_len(d$v - 1L)]
}
DA <- function(e) {
  if (min(e) < FULL_RANK) return(c(D = NA_real_, A = NA_real_))
  c(D = exp(mean(log(e))), A = length(e) / sum(1 / e))
}
full_rank <- function(d) min(effs(d, c("row", "col"))) >= FULL_RANK
report <- function(d) {
  x <- c(DA(effs(d, "row")), DA(effs(d, "col")), DA(effs(d, c("row", "col"))))
  names(x) <- c("row_D", "row_A", "col_D", "col_A", "joint_D", "joint_A")
  x
}

# ---- criterion state and rank-2 scoring -------------------------------------
# cstate: inverse of M = C/r + J/v + eps (I - J/v), its log det and
# tr = trace(M^-1) - 1 (= sum of 1/(e_i + eps)); MN[[f]] = M^-1 N_f.
cstate <- function(d, facs, eps) {
  v <- d$v
  M <- info(d, facs) / d$r + 1 / v
  if (eps > 0) M <- M + eps * (diag(v) - 1 / v)
  R <- tryCatch(chol(M), error = function(e) NULL)
  if (is.null(R) || min(diag(R)) < 1e-6) return(list(ok = FALSE))  # as singular
  Minv <- chol2inv(R)
  list(ok = TRUE, facs = facs, joint = length(facs) == 2L, eps = eps,
       ld = 2 * sum(log(diag(R))), tr = sum(diag(Minv)) - 1, Minv = Minv,
       MN = sapply(facs, function(f) Minv %*% d$N[[f]], simplify = FALSE))
}
# log det ratio for every pair (P[i], Q[i]) of a batch
batch_delta <- function(d, cs, P, Q) {
  m <- length(P); v <- d$v; r <- d$r; idx <- seq_len(m)
  tp <- d$trt[P]; tq <- d$trt[Q]
  A <- matrix(0, v, m); MA <- matrix(0, v, m); cc <- numeric(m)
  if (cs$joint) {                       # a = -w - ..., c = 2 - ...
    A[cbind(tq, idx)] <- -1; A[cbind(tp, idx)] <- 1
    MA <- cs$Minv[, tp, drop = FALSE] - cs$Minv[, tq, drop = FALSE]
    cc <- cc + 2
  }
  for (f in cs$facs) {
    lp <- d$lab[[f]][P]; lq <- d$lab[[f]][Q]; k <- d$k[[f]]
    A <- A - (d$N[[f]][, lp, drop = FALSE] - d$N[[f]][, lq, drop = FALSE]) / k
    MA <- MA - (cs$MN[[f]][, lp, drop = FALSE] - cs$MN[[f]][, lq, drop = FALSE]) / k
    cc <- cc - 2 * (lp != lq) / k
  }
  G11 <- colSums(A * MA)
  G12 <- MA[cbind(tq, idx)] - MA[cbind(tp, idx)]
  G22 <- cs$Minv[cbind(tq, tq)] + cs$Minv[cbind(tp, tp)] - 2 * cs$Minv[cbind(tp, tq)]
  det <- (1 + G12 / r) * (1 + (G12 + cc * G22) / r) - (G22 / r) * (G11 + cc * G12) / r
  out <- rep(-Inf, m)
  out[det > 0] <- log(det[det > 0])
  out
}
# Row log det and trace after one swap (rank 2, Woodbury), from the exact row state
row_after <- function(d, rs, p, q) {
  f <- "row"; lp <- d$lab[[f]][p]; lq <- d$lab[[f]][q]
  if (lp == lq) return(c(ld = rs$ld, tr = rs$tr))
  r <- d$r; k <- d$k[[f]]; tp <- d$trt[p]; tq <- d$trt[q]
  a <- -(d$N[[f]][, lp] - d$N[[f]][, lq]) / k
  w <- numeric(d$v); w[tq] <- 1; w[tp] <- -1
  MU <- cbind(-(rs$MN[[f]][, lp] - rs$MN[[f]][, lq]) / k, rs$Minv[, tq] - rs$Minv[, tp])
  G <- crossprod(cbind(a, w), MU)
  cc <- -2 / k
  S <- matrix(c(0, 1, 1, cc), 2) / r
  dt <- det(diag(2) + S %*% G)
  if (!(dt > 0)) return(c(ld = -Inf, tr = Inf))
  Sinv <- r * matrix(c(-cc, 1, 1, 0), 2)
  tr_new <- tryCatch(rs$tr - sum(diag(solve(Sinv + G, crossprod(MU)))),
                     error = function(e) NA_real_)
  if (is.na(tr_new)) {                  # degenerate 2x2: recompute exactly
    ex <- cstate(swap_plots(d, p, q), "row", 0)
    return(if (ex$ok) c(ld = ex$ld, tr = ex$tr) else c(ld = -Inf, tr = Inf))
  }
  c(ld = rs$ld + log(dt), tr = tr_new)
}

# ---- first-improvement descent ----------------------------------------------
# Maximises the log det of the factors `facs` (+ eps) over `pairs`, visiting
# them in one random order cyclically. `floors` (ld, tr of the Row factor)
# rejects swaps that take Row log det below or Row trace above them.
# `stop_fn(d)` is checked at the start and after each accepted swap.
within_pairs <- function(d, same_row = FALSE) {
  out <- lapply(seq_len(d$r), function(g) {
    idx <- which(d$rep == g)
    if (same_row) {
      do.call(rbind, lapply(split(idx, d$row[idx]), function(x) t(utils::combn(x, 2))))
    } else t(utils::combn(idx, 2))
  })
  do.call(rbind, out)
}
descend <- function(d, facs, pairs, eps = 0, floors = NULL, stop_fn = NULL) {
  work <- c(accepted = 0, passes = 0, evaluations = 0)
  if (!is.null(stop_fn) && stop_fn(d)) return(list(d = d, work = work, stopped = TRUE))
  cs <- cstate(d, facs, eps)
  if (!cs$ok) stop("criterion matrix is not positive definite")
  rs <- if (is.null(floors)) NULL else cstate(d, "row", 0)
  pairs <- pairs[sample.int(nrow(pairs)), , drop = FALSE]
  np <- nrow(pairs); cap <- MAX_PASS * np
  i <- 0; since <- 0; evals <- 0; acc <- 0; size <- 16L; stopped <- FALSE
  while (since < np && evals < cap) {
    m <- min(size, np - since, cap - evals)
    idx <- (i + seq_len(m) - 1L) %% np + 1L
    dl <- batch_delta(d, cs, pairs[idx, 1], pairs[idx, 2])
    hit <- 0L
    for (j in which(dl > IMPROVE)) {
      if (!is.null(floors)) {
        ra <- row_after(d, rs, pairs[idx[j], 1], pairs[idx[j], 2])
        if (ra[["ld"]] < floors[["ld"]] - FLOOR_TOL ||
            ra[["tr"]] > floors[["tr"]] + FLOOR_TOL * max(1, floors[["tr"]])) next
      }
      # The rank-2 score loses digits when M is ill-conditioned (eps
      # criterion on a singular design) or the gain is tiny: confirm exactly.
      cs_new <- NULL
      if (eps > 0 || dl[j] < 1e-6) {
        cs_new <- cstate(swap_plots(d, pairs[idx[j], 1], pairs[idx[j], 2]), facs, eps)
        if (!cs_new$ok || cs_new$ld <= cs$ld + IMPROVE) next
      }
      hit <- j
      break
    }
    if (hit > 0L) {
      evals <- evals + hit; i <- (i + hit) %% np; since <- 0; acc <- acc + 1
      d <- swap_plots(d, pairs[idx[hit], 1], pairs[idx[hit], 2])
      if (!is.null(stop_fn) && stop_fn(d)) { stopped <- TRUE; break }
      cs <- if (is.null(cs_new)) cstate(d, facs, eps) else cs_new
      if (!cs$ok) stop("criterion matrix lost positive definiteness")
      if (!is.null(floors)) rs <- cstate(d, "row", 0)
      size <- 16L
    } else {
      evals <- evals + m; since <- since + m; i <- (i + m) %% np
      size <- min(512L, size * 2L)
    }
  }
  work[] <- c(acc, ceiling(evals / np), evals)
  list(d = d, work = work, stopped = stopped)
}
one_way_full <- function(f) function(d) min(effs(d, f)) >= FULL_RANK
# One-way descent; a singular start is first driven to full rank on the eps
# criterion (as the repair does), then optimised on the plain log det.
one_way_stage <- function(d, f, pairs) {
  w1 <- descend(d, f, pairs, eps = EPS, stop_fn = one_way_full(f))
  if (!w1$stopped) return(list(d = w1$d, work = w1$work))
  w2 <- descend(w1$d, f, pairs)
  list(d = w2$d, work = w1$work + w2$work)
}

# ---- one search: stage 1 (shared by the swap sets), repair, joint -----------
na_work <- c(accepted = NA_real_, passes = NA_real_, evaluations = NA_real_)
stage1 <- function(t, nrows, r, latin, seed) {
  t0 <- proc.time()[["elapsed"]]
  if (!latin) {
    d <- stage1_nested(t, nrows, r, seed)
    w_rows <- na_work; w_cols <- na_work
  } else {
    set.seed(seed)
    d <- random_latin_start(t, nrows, r)
    a <- one_way_stage(d, "row", within_pairs(d))
    b <- one_way_stage(a$d, "col", within_pairs(d, same_row = TRUE))
    d <- b$d; w_rows <- a$work; w_cols <- b$work
  }
  list(d = d, w_rows = w_rows, w_cols = w_cols, seconds = proc.time()[["elapsed"]] - t0)
}
search <- function(s1, set, seed) {
  d <- s1$d
  set.seed(seed)
  pairs <- within_pairs(d, same_row = set == "row_preserving")
  rs0 <- cstate(d, "row", 0)
  floors <- if (set == "floor") c(ld = rs0$ld, tr = rs0$tr) else NULL
  start_full <- full_rank(d)
  t0 <- proc.time()[["elapsed"]]
  w_rep <- c(accepted = 0, passes = 0, evaluations = 0)
  if (!start_full) {
    rp <- descend(d, c("row", "col"), pairs, eps = EPS, floors = floors, stop_fn = full_rank)
    d <- rp$d; w_rep <- rp$work
  }
  after_full <- start_full || full_rank(d)
  t1 <- proc.time()[["elapsed"]]
  w_joint <- c(accepted = 0, passes = 0, evaluations = 0)
  if (after_full) {
    jt <- descend(d, c("row", "col"), pairs, floors = floors)
    d <- jt$d; w_joint <- jt$work
  }
  t2 <- proc.time()[["elapsed"]]
  list(final = report(d), full = after_full && full_rank(d), start_full = start_full,
       after_full = after_full, w_rep = w_rep, w_joint = w_joint,
       sec_rep = t1 - t0, sec_joint = t2 - t1)
}
# Return rule of spec 6.2 over searches 1..S
pick <- function(res) {
  keep <- which(vapply(res, `[[`, logical(1), "full"))
  if (!length(keep)) return(NA_integer_)
  val <- function(i, x) res[[i]]$final[[x]]
  for (x in c("row_D", "row_A")) {
    v <- vapply(keep, val, numeric(1), x = x)
    keep <- keep[v >= max(v) - TIE]
  }
  v <- vapply(keep, val, numeric(1), x = "joint_D")
  keep[which(v == max(v))[1]]
}

run_unit <- function(t, nrows, r, latin, seed) {
  set.seed(seed)
  sub <- sample.int(2^30, 3L * SEARCHES)
  s1_seed <- c(seed, sub[2:SEARCHES])
  set_seed <- list(floor = c(seed, sub[SEARCHES + 2:SEARCHES]),
                   row_preserving = c(seed, sub[2L * SEARCHES + 2:SEARCHES]))
  stages <- lapply(seq_len(SEARCHES), function(j) stage1(t, nrows, r, latin, s1_seed[j]))
  s1rep <- report(stages[[1]]$d)
  names(s1rep) <- paste0("stage1_", names(s1rep))
  rows <- list()
  for (set in c("floor", "row_preserving")) {
    res <- lapply(seq_len(SEARCHES), function(j) search(stages[[j]], set, set_seed[[set]][j]))
    for (S in S_VALUES) {
      sel <- pick(res[seq_len(S)])
      sum_work <- function(get) {
        w <- Reduce(`+`, lapply(seq_len(S), get))
        names(w) <- names(na_work)
        w
      }
      w1r <- sum_work(function(j) stages[[j]]$w_rows)
      w1c <- sum_work(function(j) stages[[j]]$w_cols)
      wr <- sum_work(function(j) res[[j]]$w_rep)
      wj <- sum_work(function(j) res[[j]]$w_joint)
      fin <- if (is.na(sel)) setNames(rep(NA_real_, 6), names(res[[1]]$final)) else res[[sel]]$final
      rows[[length(rows) + 1L]] <- data.frame(
        t = t, k = nrows, ncols = t / nrows, r = r,
        coding = if (latin) "latinized" else "nested", seed = seed,
        swap_set = set, S = S, rule_holds = onestage_rule_holds(t, nrows, r, latin),
        as.list(s1rep),
        full_rank_start = res[[1]]$start_full, full_rank_after_repair = res[[1]]$after_full,
        repair_swaps = res[[1]]$w_rep[["accepted"]],
        searches_full_rank_start = sum(vapply(res[seq_len(S)], `[[`, logical(1), "start_full")),
        searches_full_rank_after_repair = sum(vapply(res[seq_len(S)], `[[`, logical(1), "after_full")),
        searches_full_rank_final = sum(vapply(res[seq_len(S)], `[[`, logical(1), "full")),
        selected = sel, full_rank = !is.na(sel), as.list(fin),
        stage1_rows_accepted = w1r[["accepted"]], stage1_rows_passes = w1r[["passes"]],
        stage1_rows_evaluations = w1r[["evaluations"]],
        stage1_cols_accepted = w1c[["accepted"]], stage1_cols_passes = w1c[["passes"]],
        stage1_cols_evaluations = w1c[["evaluations"]],
        repair_accepted = wr[["accepted"]], repair_passes = wr[["passes"]],
        repair_evaluations = wr[["evaluations"]],
        joint_accepted = wj[["accepted"]], joint_passes = wj[["passes"]],
        joint_evaluations = wj[["evaluations"]],
        seconds_stage1 = sum(vapply(stages[seq_len(S)], `[[`, numeric(1), "seconds")),
        seconds_repair = sum(vapply(res[seq_len(S)], `[[`, numeric(1), "sec_rep")),
        seconds_joint = sum(vapply(res[seq_len(S)], `[[`, numeric(1), "sec_joint")),
        stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

# ---- grid, shards, resume ---------------------------------------------------
grid <- unique(nested_golden_grid("os")[c("t", "k", "r")])
grid <- grid[grid$t <= tmax | grid$t %in% tset, , drop = FALSE]
grid$seeds <- ifelse(grid$t %in% tset, tseeds, n_seeds)
grid <- do.call(rbind, lapply(c(FALSE, TRUE), function(l) cbind(grid, latin = l)))
grid <- grid[order(grid$t, grid$k, grid$r, grid$latin), , drop = FALSE]
rownames(grid) <- NULL
if (!is.na(shard)) {
  ik <- as.integer(strsplit(shard, "/", fixed = TRUE)[[1]])
  if (length(ik) != 2L || anyNA(ik) || ik[2] < 1L || ik[1] < 1L || ik[1] > ik[2]) {
    stop("shard= must be i/k with 1 <= i <= k.")
  }
  # longest processing time on a rough cost: pairs per pass x v, plus overhead
  cost <- grid$seeds * (grid$r * grid$t^2 * (grid$t + 50) + 2e5)
  load <- numeric(ik[2]); owner <- integer(nrow(grid))
  for (i in order(-cost, seq_along(cost))) {
    owner[i] <- which.min(load); load[owner[i]] <- load[owner[i]] + cost[i]
  }
  grid <- grid[owner == ik[1], , drop = FALSE]
}
head_sha <- tryCatch(system2("git", c("-C", file.path(script_dir, ".."), "rev-parse", "HEAD"),
                             stdout = TRUE, stderr = FALSE), error = function(e) NA_character_)
saveRDS(list(FielDHub = as.character(utils::packageVersion("FielDHub")),
             FielDHub_path = find.package("FielDHub"), git_head = head_sha,
             R = R.version.string, platform = R.version$platform,
             seeds = n_seeds, tmax = tmax, tset = tset, tseeds = tseeds, shard = shard,
             eps = EPS, full_rank = FULL_RANK, tie = TIE, improve = IMPROVE,
             floor_tol = FLOOR_TOL, max_pass = MAX_PASS, searches = SEARCHES),
        paste0(path, ".meta.rds"))
done <- if (file.exists(path)) utils::read.csv(path) else NULL
for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  coding <- if (g$latin) "latinized" else "nested"
  for (seed in seq_len(g$seeds)) {
    if (!is.null(done) && any(done$t == g$t & done$k == g$k & done$r == g$r &
                              done$coding == coding & done$seed == seed)) next
    t0 <- proc.time()[["elapsed"]]
    out <- run_unit(g$t, g$k, g$r, g$latin, seed)
    utils::write.table(out, path, sep = ",", row.names = FALSE,
                       col.names = !file.exists(path), append = file.exists(path))
    message(sprintf("%d/%d/%d %s seed %d: %.1f s", g$t, g$k, g$r, coding, seed,
                    proc.time()[["elapsed"]] - t0))
  }
}
cat(sprintf("%s: %d rows\n", path, if (file.exists(path)) nrow(utils::read.csv(path)) else 0L))
