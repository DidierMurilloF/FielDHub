# First-principles references for the block engine: eigenvalues of
# C = R - N K^{-1} N' (no block-space algebra), used to check the C++ updates.
engine_ref_counts <- function(design) {
  N <- table(design$treatments, design$Level_1)
  matrix(as.integer(N), nrow(N))
}
engine_ref_info <- function(N) {
  k <- colSums(N)
  diag(rowSums(N), nrow(N)) - N %*% diag(1 / k, length(k)) %*% t(N)
}
engine_ref_efficiency <- function(N, r) {
  ev <- eigen(engine_ref_info(N), symmetric = TRUE, only.values = TRUE)$values
  ev <- ev[ev > 1e-9]
  c(D = exp(mean(log(ev))) / r, A = 1 / mean(1 / ev) / r, rank = length(ev))
}
engine_ref_swapped <- function(N, a, p, c, q) {
  N[a, p] <- N[a, p] - 1L; N[a, q] <- N[a, q] + 1L
  N[c, q] <- N[c, q] - 1L; N[c, p] <- N[c, p] + 1L
  N
}
# det(C' + J/v) / det(C + J/v); 0 when the swapped design is disconnected
engine_ref_ratio <- function(N, a, p, c, q) {
  N2 <- engine_ref_swapped(N, a, p, c, q)
  if (engine_ref_efficiency(N2, 1)[["rank"]] < nrow(N) - 1) return(0)
  v <- nrow(N)
  exp(determinant(engine_ref_info(N2) + 1 / v)$modulus -
        determinant(engine_ref_info(N) + 1 / v)$modulus)[[1]]
}
engine_ref_pick <- function(x) x[sample.int(length(x), 1L)]
engine_ref_random_counts <- function(v, r, b) {
  n <- v * r
  k <- n %/% b + (seq_len(b) <= n %% b)
  plots <- sample(rep(seq_len(v), r))
  blk <- rep(seq_len(b), k)
  N <- matrix(0L, v, b)
  for (i in seq_len(n)) N[plots[i], blk[i]] <- N[plots[i], blk[i]] + 1L
  N
}
engine_ref_random_swap <- function(N) {
  repeat {
    p <- engine_ref_pick(seq_len(ncol(N)))
    q <- engine_ref_pick(setdiff(seq_len(ncol(N)), p))
    a <- engine_ref_pick(which(N[, p] > 0L))
    c <- engine_ref_pick(which(N[, q] > 0L))
    if (a != c) return(c(a, p, c, q))
  }
}
