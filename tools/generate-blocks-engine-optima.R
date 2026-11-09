# Writes tests/testthat/fixtures/blocks-engine-optima.rds: designs with a proven
# optimum for the block engine's known-optima test (spec section 11, gate M1-6).
#   Rscript --vanilla tools/generate-blocks-engine-optima.R .
# Optimality:
# - BIBDs: every nonzero eigenvalue of C is equal, so D and A attain the bound.
# - 0/1 profile designs with equal block sizes and equal concurrences: D- and
#   A-optimal (plan review gate, claim P4).
# - Any design whose A-efficiency attains the convexity A-bound is A-optimal
#   (claim P3); used for the p-rep case, where only the A claim applies.
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) != 1L) stop("Usage: Rscript tools/generate-blocks-engine-optima.R <repo>")
root <- normalizePath(arguments[1], mustWork = TRUE)
efficiency <- function(N, r) {
  k <- colSums(N)
  C <- diag(rowSums(N), nrow(N)) - N %*% diag(1 / k, length(k)) %*% t(N)
  ev <- eigen(C, symmetric = TRUE, only.values = TRUE)$values
  ev <- ev[ev > 1e-9]
  c(D = exp(mean(log(ev))) / r, A = 1 / mean(1 / ev) / r)
}
profiles <- function(v, L, size) {
  P <- utils::combn(L, size)
  Z <- matrix(0, v, L)
  for (i in seq_len(v)) Z[i, P[, (i - 1) %% ncol(P) + 1]] <- 1
  Z
}
bibd7 <- matrix(0, 7, 7)
for (j in 1:7) bibd7[c(j, j %% 7 + 1, (j + 2) %% 7 + 1), j] <- 1
bibd13 <- matrix(0, 13, 13)
for (j in 0:12) bibd13[(c(0, 1, 3, 9) + j) %% 13 + 1, j + 1] <- 1
cases <- list(
  list(name = "BIBD (7,3,1)", v = 7, r = 3, b = 7, N = bibd7, criterion = "D"),
  list(name = "BIBD (13,4,1)", v = 13, r = 4, b = 13, N = bibd13, criterion = "D"),
  list(name = "sparse profiles v=30 L=4 r=2", v = 30, r = 2, b = 4, N = profiles(30, 4, 2), criterion = "D"),
  list(name = "sparse profiles v=20 L=5 r=2", v = 20, r = 2, b = 5, N = profiles(20, 5, 2), criterion = "D"),
  list(name = "p-rep profiles v=10 L=5 s=6", v = 10, r = 6, b = 5, N = 1 + profiles(10, 5, 1), criterion = "A")
)
optima <- do.call(rbind, lapply(cases, function(x) {
  stopifnot(all(rowSums(x$N) == x$r))
  e <- efficiency(x$N, x$r)
  data.frame(name = x$name, v = x$v, r = x$r, b = x$b, criterion = x$criterion,
             target = unname(e[x$criterion]))
}))
stopifnot(abs(optima$target[1] - 7 / 9) < 1e-12, abs(optima$target[2] - 13 / 16) < 1e-12)
saveRDS(optima, file.path(root, "tests", "testthat", "fixtures", "blocks-engine-optima.rds"), version = 2)
print(optima)
