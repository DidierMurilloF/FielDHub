# Validity of FielDHub's own A-bound (phase 2 spec 5.2, 5.2b, 6.2 and 7):
# no design may have an A-efficiency above the bound reported for its block
# factor's class.
#   Rscript tools/check-a-bound-validity.R [golden.csv.gz ...]
# Run it against the installed build (R CMD INSTALL . --preclean). The bound
# and the class check are the installed package's own fieldhub_a_bound() and
# block_factor_class() (R/engine_block_efficiency.R), read from
# asNamespace("FielDHub"). The A-efficiencies are computed here, from the
# eigenvalues of the incidence, independently of the package.
#
# Checks, one printed line each (designs, violations, largest A - bound):
# - random designs of every class of 5.2b: general (binary and non-binary,
#   k < v and k >= v, one block), resolvable, nested with 2-3 copies of each
#   treatment per replicate, and the rows and columns of row-column designs
#   with nested and with shared labels. About NREP (default 2000) designs per
#   class: 40% as drawn, 20% made binary, 40% hill-climbed on A. The class
#   comes from block_factor_class() on a stated replicate factor;
# - the edge shapes of 5.2b (k = 1, k = v, one block, r = 1, the replicate
#   factor), with their bound of 0 or 1;
# - v = 12, r = 2, k = 3 in the general and in the resolvable class;
# - the two nested counterexamples of 5.2b (d);
# - every design in each golden file given: recorded A <= the bound of the
#   shape's class + 1.5e-7, for every factor with a recorded A. Lattices,
#   alpha, incomplete-block, twostage row-column and onestage nested factors
#   are resolvable (R = r); onestage latinized rows and columns are general
#   (shared labels, 5.2b (e)), except on fallback seeds, which report the
#   nested twostage design. A golden check never passes on nothing: a file
#   fails when it has no rows, lacks a column its callers carry (caller, t,
#   r, k; A for block callers; col_A and row_A for row-column callers), has a
#   missing caller or status, has a factor with no recorded A at all, or has
#   a row whose bound is missing (the bound of its shape, or the recorded
#   `bound` of a row with a recorded A). Rows without a recorded A are left
#   out of the comparison and counted on the line, "error" rows apart.
# A violation is A > bound + 1.5e-7. The tool exits with status 1 on any
# violation, on a value that cannot be compared (NA), on a golden failure
# above, or on a known value that does not match.

arguments <- commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages(library(parallel))
fieldhub <- asNamespace("FielDHub")
fieldhub_a_bound <- get("fieldhub_a_bound", envir = fieldhub)
block_factor_class <- get("block_factor_class", envir = fieldhub)
tol <- 1.5e-7
nrep <- as.integer(Sys.getenv("NREP", "2000"))
cores <- as.integer(Sys.getenv("CORES", "4"))
failures <- character(0)

# An NA excess cannot be compared with the bound: it fails the check
line <- function(check, excess) {
  missing <- sum(is.na(excess))
  excess <- excess[!is.na(excess)]
  bad <- sum(excess > tol)
  cat(sprintf("%-52s designs %7d  violations %d  largest A - bound %.3g\n",
              check, length(excess), bad, if (length(excess)) max(excess) else NA_real_))
  if (missing > 0L) cat(sprintf("  FAIL %s: %d designs with an NA A or bound\n", check, missing))
  if (bad > 0L || missing > 0L) failures <<- c(failures, check)
}
fail <- function(check, why) {
  cat(sprintf("  FAIL %s: %s\n", check, why))
  failures <<- c(failures, check)
}
# One factor of a golden file: `A` the recorded values, `bound` the bound of
# each row's shape, `error` the rows with status "error", which carry no
# design. Rows without a recorded A are left out and counted; no recorded A
# at all, or a missing bound, fails the check.
golden_line <- function(check, A, bound, error) {
  A <- suppressWarnings(as.numeric(A))
  have <- !is.na(A)
  line(check, A[have] - bound[have])
  dropped <- sum(!have)
  if (dropped > 0L) {
    cat(sprintf("  %s: %d rows without a recorded A left out (%d with status error, %d other)\n",
                check, dropped, sum(!have & error), sum(!have & !error)))
  }
  if (!any(have)) fail(check, "no recorded A to check")
  # the have rows with an NA bound are already failed by line()
  if (any(is.na(bound) & !have)) {
    fail(check, sprintf("%d rows without a recorded A have an NA bound", sum(is.na(bound) & !have)))
  }
}
expect <- function(what, value, target, digits = 7) {
  ok <- isTRUE(abs(value - target) <= 0.5 * 10^-digits + 1e-12)
  cat(sprintf("  %-50s %.7f (expected %.7f) %s\n", what, value, target, if (ok) "ok" else "MISMATCH"))
  if (!ok) failures <<- c(failures, what)
}

# A-efficiency of a one-way block design: the efficiency factors are 1 minus
# the eigenvalues of R^(-1/2) N K^(-1) N' R^(-1/2) other than the trivial 1;
# a disconnected design has A = 0.
a_eff <- function(trt, blk, v) {
  blk <- as.integer(factor(blk))
  b <- max(blk)
  N <- matrix(tabulate((blk - 1L) * v + trt, v * b), v, b)
  r <- rowSums(N)
  k <- colSums(N)
  M <- (N %*% (t(N) / k)) / sqrt(outer(r, r))
  e <- 1 - eigen(M, symmetric = TRUE, only.values = TRUE)$values[-1]
  if (min(e) < 1e-9) 0 else (v - 1) / sum(1 / e)
}

# Reported bound of a design: the class from the stated replicate factor
# (NULL states no claim, as an unclaimed BlockEfficiencies() factor) and the
# shape read from the incidence.
design_bound <- function(trt, blk, replicate = NULL, v) {
  cls <- if (is.null(replicate)) list(class = "general", R = NULL) else
    block_factor_class(trt, blk, replicate)
  r <- tabulate(trt, v)
  k <- tabulate(as.integer(factor(blk)))
  stopifnot(length(unique(r)) == 1L, length(unique(k)) == 1L)
  list(class = cls$class, R = cls$R,
       bound = fieldhub_a_bound(v, r[1], k[1], length(k), cls$class, R = cls$R))
}

# Hill climb on A by swaps of plots in the same segment and different blocks
climb <- function(trt, blk, seg, v, steps) {
  a <- a_eff(trt, blk, v)
  n <- length(trt)
  for (i in seq_len(steps)) {
    p <- sample.int(n, 1L)
    cand <- which(seg == seg[p] & blk != blk[p] & trt != trt[p])
    if (!length(cand)) next
    q <- cand[sample.int(length(cand), 1L)]
    t2 <- trt
    t2[c(p, q)] <- trt[c(q, p)]
    a2 <- a_eff(t2, blk, v)
    if (a2 >= a) {
      trt <- t2
      a <- a2
    }
  }
  trt
}

# Removes within-block duplicates by swaps inside a segment (k <= v)
make_binary <- function(trt, blk, seg) {
  for (it in seq_len(5000L)) {
    dup <- which(duplicated(cbind(trt, blk)))
    if (!length(dup)) break
    p <- dup[sample.int(length(dup), 1L)]
    pool <- which(seg == seg[p] & blk != blk[p])
    if (!length(pool)) break
    q <- pool[sample.int(length(pool), 1L)]
    if (any(trt[blk == blk[q]] == trt[p]) || any(trt[blk == blk[p]] == trt[q])) next
    trt[c(p, q)] <- trt[c(q, p)]
  }
  trt
}

is_binary <- function(trt, blk) !anyDuplicated(cbind(trt, blk))
pick <- function(x) x[sample.int(length(x), 1L)]
divisors <- function(n) which(n %% seq_len(n) == 0L)

# One random design of a class; returns one row per factor checked
random_design <- function(i, class) {
  set.seed(20261120L + 100000L * match(class, random_classes) + i)
  mode <- sample(c("drawn", "binary", "climbed"), 1L, prob = c(0.4, 0.2, 0.4))
  rows <- list()
  add <- function(factor, trt, blk, replicate, v, expected = NULL) {
    b <- design_bound(trt, blk, replicate, v)
    if (!is.null(expected) && !identical(b$class, expected)) {
      stop(sprintf("%s design %d: class %s, expected %s", class, i, b$class, expected))
    }
    k <- length(trt) / length(unique(blk))
    rows[[length(rows) + 1L]] <<- data.frame(
      class = class, factor = factor, mode = mode, verified = b$class, v = v,
      r = length(trt) / v, k = k, b = length(unique(blk)),
      A = a_eff(trt, blk, v), bound = b$bound[["bound"]],
      binary = is_binary(trt, blk), stringsAsFactors = FALSE)
  }
  if (class == "general") {
    repeat {
      v <- sample(2:14, 1L); r <- sample(1:6, 1L); k <- pick(divisors(v * r))
      if (v * r <= 72L) break
    }
    b <- v * r / k
    trt <- sample(rep(seq_len(v), r))
    blk <- rep(seq_len(b), each = k)
    seg <- rep(1L, v * r)
    if (mode == "binary" && k <= v) trt <- make_binary(trt, blk, seg)
    if (mode == "climbed") trt <- climb(trt, blk, seg, v, 250L)
    # a stated replicate factor: consecutive blocks in g groups; the check
    # decides whether the claim holds
    g <- pick(divisors(b))
    replicate <- rep(seq_len(g), each = v * r / g)
    add("blocks", trt, blk, if (runif(1) < 0.5) replicate else NULL, v)
  } else if (class == "resolvable") {
    repeat {
      s <- sample(1:7, 1L); k <- sample(1:8, 1L); v <- s * k; r <- sample(1:5, 1L)
      if (v >= 2L && v <= 30L) break
    }
    trt <- unlist(lapply(seq_len(r), function(g) sample.int(v)))
    blk <- rep(seq_len(r * s), each = k)
    replicate <- rep(seq_len(r), each = v)
    if (mode == "climbed") trt <- climb(trt, blk, replicate, v, 250L)
    add("blocks", trt, blk, replicate, v, "resolvable")
  } else if (class == "nested") {
    repeat {
      v <- sample(2:10, 1L); cc <- sample(2:3, 1L); R <- sample(1:3, 1L)
      k <- pick(divisors(v * cc))
      if (v * cc * R <= 72L) break
    }
    r <- cc * R
    b <- v * r / k
    trt <- unlist(lapply(seq_len(R), function(g) sample(rep(seq_len(v), cc))))
    blk <- rep(seq_len(b), each = k)
    replicate <- rep(seq_len(R), each = v * cc)
    if (mode == "binary" && k <= v) trt <- make_binary(trt, blk, replicate)
    if (mode == "climbed") trt <- climb(trt, blk, replicate, v, 250L)
    add("blocks", trt, blk, replicate, v, "nested")
  } else {
    repeat {
      nr <- sample(1:6, 1L); nc <- sample(1:6, 1L); v <- nr * nc; r <- sample(1:4, 1L)
      if (v >= 2L && v <= 30L) break
    }
    trt <- unlist(lapply(seq_len(r), function(g) sample.int(v)))
    replicate <- rep(seq_len(r), each = v)
    row <- rep(rep(seq_len(nr), each = nc), r)
    col <- rep(rep(seq_len(nc), nr), r)
    shared <- class == "rc_shared"
    rb <- if (shared) row else (replicate - 1L) * nr + row
    cb <- if (shared) col else (replicate - 1L) * nc + col
    if (mode == "climbed") trt <- climb(trt, if (runif(1) < 0.5) rb else cb, replicate, v, 250L)
    # rows and columns are claimed nested in the replicates: the check
    # accepts the nested labels and turns down shared ones (r > 1)
    expected <- if (shared && r > 1L) "general" else "resolvable"
    add("rows", trt, rb, replicate, v, expected)
    add("columns", trt, cb, replicate, v, expected)
  }
  do.call(rbind, rows)
}
random_classes <- c("general", "resolvable", "nested", "rc_nested", "rc_shared")

cat("Random designs (", nrep, " per class)\n", sep = "")
for (class in random_classes) {
  res <- do.call(rbind, mclapply(seq_len(nrep), random_design, class = class,
                                 mc.cores = cores, mc.preschedule = TRUE))
  stopifnot(is.data.frame(res), length(unique(res$class)) == 1L)
  line(sprintf("random %s", class), res$A - res$bound)
  cat(sprintf("  modes %s; non-binary %d, k >= v %d, one block %d, verified classes %s\n",
              paste(names(table(res$mode)), table(res$mode), collapse = " "),
              sum(!res$binary), sum(res$k >= res$v), sum(res$b == 1),
              paste(names(table(res$verified)), table(res$verified), collapse = " ")))
}

cat("Edge shapes (spec 5.2b)\n")
edge <- function(name, v, r, k, replicate_of, target, draws = 50L) {
  excess <- numeric(0)
  for (j in seq_len(draws)) {
    set.seed(7000L + j)
    if (identical(replicate_of, "replicates")) {
      trt <- unlist(lapply(seq_len(r), function(g) sample.int(v)))
      blk <- rep(seq_len(r * v / k), each = k)
      replicate <- rep(seq_len(r), each = v)
    } else {
      trt <- sample(rep(seq_len(v), r))
      blk <- rep(seq_len(v * r / k), each = k)
      replicate <- NULL
    }
    b <- design_bound(trt, blk, replicate, v)
    if (b$bound[["bound"]] != target) {
      failures <<- c(failures, name)
      cat(sprintf("  %s: bound %.7f, expected %g (class %s)\n", name, b$bound[["bound"]], target, b$class))
    }
    excess <- c(excess, a_eff(trt, blk, v) - b$bound[["bound"]])
  }
  line(sprintf("edge %s (bound %g)", name, target), excess)
}
edge("k = 1, general", 5, 2, 1, NULL, 0)
edge("k = 1, resolvable", 5, 2, 1, "replicates", 0)
edge("k = v, general", 5, 3, 5, NULL, 1)
edge("k = 2v, general", 4, 4, 8, NULL, 1)
edge("one block", 4, 3, 12, NULL, 1)
edge("r = 1, general", 6, 1, 3, NULL, 0)
edge("r = 1, resolvable (R = 1)", 6, 1, 3, "replicates", 0)
edge("replicate factor (k = v, resolvable)", 6, 3, 6, "replicates", 1)

cat("v = 12, r = 2, k = 3\n")
general_bound <- fieldhub_a_bound(12, 2, 3, 8, "general")[["bound"]]
expect("general-class bound", general_bound, 0.6226415)
set.seed(12023L)
best <- 0
for (start in seq_len(20L)) {
  trt <- climb(sample(rep(1:12, 2)), rep(1:8, each = 3), rep(1L, 24), 12, 1500L)
  best <- max(best, a_eff(trt, rep(1:8, each = 3), 12))
}
cat(sprintf("  best climbed general design A %.7f (the spec's climbed design: 0.5992218)\n", best))
line("v = 12, r = 2, k = 3 general, climbed", best - general_bound)
resolvable_bound <- fieldhub_a_bound(12, 2, 3, 8, "resolvable", R = 2)[["bound"]]
expect("resolvable-class bound", resolvable_bound, 0.5945946)
# The rectangular lattice: treatments are the off-diagonal cells (i, j) of a
# 4 x 4 square; replicate 1 blocks them by i, replicate 2 by j.
cells <- which(row(diag(4)) != col(diag(4)), arr.ind = TRUE)
lattice_trt <- c(order(cells[, 1]), order(cells[, 2]))
lattice_blk <- rep(1:8, each = 3)
lattice_rep <- rep(1:2, each = 12)
lattice <- design_bound(lattice_trt, lattice_blk, lattice_rep, 12)
stopifnot(identical(lattice$class, "resolvable"), lattice$R == 2L)
expect("rectangular lattice 12/3/2 A (attains the bound)", a_eff(lattice_trt, lattice_blk, 12), 0.5945946)
set.seed(12024L)
excess <- a_eff(lattice_trt, lattice_blk, 12) - lattice$bound[["bound"]]
for (start in seq_len(20L)) {
  trt <- climb(c(sample.int(12), sample.int(12)), lattice_blk, lattice_rep, 12, 1500L)
  excess <- c(excess, a_eff(trt, lattice_blk, 12) - lattice$bound[["bound"]])
}
line("v = 12, r = 2, k = 3 resolvable, lattice + climbed", excess)

cat("Nested counterexamples (spec 5.2b (d)), blocks by replicate\n")
nested_case <- function(name, blocks, v, A_target, bound_target, convexity_target = NULL) {
  trt <- unlist(blocks)
  k <- length(blocks[[1]])
  blk <- rep(seq_along(blocks), each = k)
  replicate <- rep(1:2, each = length(trt) / 2)
  b <- design_bound(trt, blk, replicate, v)
  cat(sprintf("  %s: class %s, R = %s\n", name, b$class, format(b$R)))
  if (!identical(b$class, "nested") || !identical(as.integer(b$R), 2L)) failures <<- c(failures, name)
  A <- a_eff(trt, blk, v)
  expect(paste(name, "A"), A, A_target)
  expect(paste(name, "reported bound"), b$bound[["bound"]], bound_target)
  if (!is.null(convexity_target)) expect(paste(name, "convexity component"), b$bound[["convexity"]], convexity_target)
  # with the replication r in place of R the rank is too small and A beats it
  r <- length(trt) / v
  wrong <- fieldhub_a_bound(v, r, k, length(blocks), "nested", R = r)[["bound"]]
  cat(sprintf("  %s: bound with r for R %.7f (beaten: %s)\n", name, wrong, A > wrong + tol))
  line(name, A - b$bound[["bound"]])
}
nested_case("v = 6, k = 4",
            list(c(2, 4, 5, 6), c(1, 3, 4, 6), c(1, 2, 3, 5), c(2, 3, 5, 6), c(1, 3, 4, 5), c(1, 2, 4, 6)),
            6, 0.8936755, 0.8940397, 0.8974359)
nested_case("v = 4, k = 3",
            rep(list(c(2, 3, 4), c(1, 3, 4), c(1, 2, 4), c(1, 2, 3)), 2),
            4, 0.8888889, 0.8888889)

# Golden files: one check per file and factor. The recorded A holds seven
# decimals, so it may exceed an attained bound by up to 5e-8.
resolvable_shape <- function(t, r, k) fieldhub_a_bound(t, r, k, r * t / k, "resolvable", R = r)[["bound"]]
general_shape <- function(t, r, k) fieldhub_a_bound(t, r, k, t * r / k, "general")[["bound"]]
memo <- new.env()
# NA when the shape is incomplete or the helper turns it down
shape_bound <- function(kind, t, r, k) {
  if (anyNA(c(t, r, k))) return(NA_real_)
  key <- paste(kind, t, r, k)
  if (is.null(memo[[key]])) {
    memo[[key]] <- tryCatch(
      if (kind == "resolvable") resolvable_shape(t, r, k) else general_shape(t, r, k),
      error = function(e) NA_real_)
  }
  memo[[key]]
}
for (path in arguments) {
  g <- utils::read.csv(path, stringsAsFactors = FALSE)
  name <- basename(path)
  cat("Golden file", name, "\n")
  file_check <- sprintf("golden %s", name)
  absent <- setdiff(c("caller", "t", "r", "k"), names(g))
  if (length(absent)) {
    fail(file_check, sprintf("no column %s", paste(absent, collapse = ", ")))
    next
  }
  if (!nrow(g)) {
    fail(file_check, "no rows")
    next
  }
  if (!"status" %in% names(g)) g$status <- "ok"
  unknown <- is.na(g$caller) | is.na(g$status) | !nzchar(g$caller) | !nzchar(g$status)
  if (any(unknown)) fail(file_check, sprintf("%d rows with a missing caller or status", sum(unknown)))
  error <- g$status %in% "error"
  rowcol <- g$caller %in% c("row_column", "onestage_nested", "onestage_latinized")
  # shared labels only where a latinized onestage design was returned; a
  # fallback seed reports the nested twostage design
  shared <- g$caller %in% "onestage_latinized" & g$status %in% "ok"
  # blocks of k plots (columns of a row-column design: k = nrows plots, or
  # r * nrows when shared, in ncols blocks)
  col_bound <- mapply(function(t, r, k, sh) {
    if (sh) shape_bound("general", t, r, r * k) else shape_bound("resolvable", t, r, k)
  }, g$t, g$r, g$k, shared)
  # the recorded bound is the gate of the optimum rule (spec 6.4): a row
  # with a recorded A carries one
  if ("bound" %in% names(g)) {
    recorded_A <- if ("A" %in% names(g)) g$A else g$col_A
    unbounded <- is.na(suppressWarnings(as.numeric(g$bound))) &
      !is.na(suppressWarnings(as.numeric(recorded_A)))
    if (length(unbounded) && any(unbounded)) {
      fail(file_check, sprintf("%d rows with a recorded A and an NA recorded bound", sum(unbounded)))
    }
  }
  golden_factor <- function(label, column, rows, bound) {
    check <- sprintf("golden %s %s", name, label)
    if (!column %in% names(g)) {
      fail(check, sprintf("no column %s (%d rows should carry it)", column, sum(rows)))
      return(invisible())
    }
    golden_line(check, g[[column]][rows], bound, error[rows])
  }
  if (any(!rowcol)) golden_factor("blocks A", "A", !rowcol, col_bound[!rowcol])
  if (any(rowcol)) {
    golden_factor("column A", "col_A", rowcol, col_bound[rowcol])
    # rows: ncols = t / nrows plots each (r * ncols when shared, in nrows blocks)
    row_bound <- mapply(function(t, r, k, sh) {
      if (sh) shape_bound("general", t, r, r * t / k) else shape_bound("resolvable", t, r, t / k)
    }, g$t[rowcol], g$r[rowcol], g$k[rowcol], shared[rowcol])
    golden_factor("row A", "row_A", rowcol, row_bound)
  }
}

if (length(failures)) {
  cat("\nFAILED:", paste(unique(failures), collapse = "; "), "\n")
  quit(status = 1L)
}
cat("\nAll A-bound validity checks passed.\n")
