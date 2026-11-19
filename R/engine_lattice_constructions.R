#' Algebraic lattice starts for the resolvable block engine (internal)
#'
#' Nested-engine spec section 6.5. A square lattice of order q with r
#' replicates is a resolvable design for v = q^2 treatments in blocks of q:
#' treatment i q + j (i, j in 0..q-1) goes to group i in the first replicate,
#' to group j in the second and to group L(i, j) in each further replicate,
#' where the L are mutually orthogonal Latin squares of order q (Yates 1936;
#' Raghavarao 1971, section 1.3). With r - 2 such squares any two treatments
#' meet in at most one block, every pair of replicates is orthogonal, and the
#' design reaches the A-efficiency bound. The squares come from the finite
#' field GF(q) when q is a prime power: L_a(x, y) = a x + y for the q - 1
#' nonzero a (Bose 1938, Sankhya 3:323-338), so r <= q + 1. At other orders
#' they are direct products of the prime-power factors' squares (MacNeish
#' 1922, Ann. Math. 23:221-227).
#'
#' Clean room: written from the cited mathematics and the spec only.
#' @noRd
NULL

#' Prime power decomposition: list(p, e) when q = p^e, otherwise NULL
#' @noRd
prime_power <- function(q) {
  q <- as.integer(q)
  if (is.na(q) || q < 2L) return(NULL)
  p <- 2L
  while (q %% p != 0L) p <- p + 1L
  e <- 0L
  x <- q
  while (x %% p == 0L) {
    x <- x %/% p
    e <- e + 1L
  }
  if (x != 1L) return(NULL)
  list(p = p, e = e)
}

#' Remainder of polynomial `a` modulo the monic `d` over Z_p (coefficient
#' vectors, lowest degree first)
#' @noRd
poly_mod <- function(a, d, p) {
  n <- length(d) - 1L
  while (length(a) > n) {
    lead <- a[length(a)]
    if (lead != 0L) {
      idx <- length(a) - n + seq_len(n + 1L) - 1L
      a[idx] <- (a[idx] - lead * d) %% p
    }
    a <- a[-length(a)]
  }
  a
}

#' Whether the monic polynomial `f` (degree e, lowest degree first) is
#' irreducible over Z_p: no monic divisor of degree 1..floor(e / 2)
#' @noRd
poly_irreducible <- function(f, p) {
  e <- length(f) - 1L
  if (e < 2L) return(TRUE)
  for (d in seq_len(e %/% 2L)) {
    for (g in 0:(p^d - 1L)) {
      divisor <- c((g %/% p^(0:(d - 1L))) %% p, 1L)
      if (all(poly_mod(f, divisor, p) == 0L)) return(FALSE)
    }
  }
  TRUE
}

#' Addition and multiplication tables of GF(q)
#'
#' q = p^e. Elements are the integers 0..q-1, read as base-p digit vectors
#' (polynomials in x of degree < e, lowest digit = constant term). The field
#' is Z_p[x] modulo the first monic irreducible polynomial of degree e, found
#' by brute force in the order of its lower coefficients read as a base-p
#' number. Returns `list(add, mul)`, q x q, 0-based entries (entry [a + 1,
#' b + 1] is a + b or a b), or NULL when q is not a prime power.
#' @noRd
gf_tables <- function(q) {
  pe <- prime_power(q)
  if (is.null(pe)) return(NULL)
  p <- pe$p
  e <- pe$e
  q <- as.integer(q)
  elements <- 0:(q - 1L)
  if (e == 1L) {
    return(list(add = outer(elements, elements, function(a, b) (a + b) %% p),
                mul = outer(elements, elements, function(a, b) (a * b) %% p)))
  }
  weights <- as.integer(p^(0:(e - 1L)))
  digits <- vapply(elements, function(a) (a %/% weights) %% p, integer(e))  # e x q
  add <- matrix(0L, q, q)
  for (b in elements) {
    add[, b + 1L] <- as.integer(colSums(((digits + digits[, b + 1L]) %% p) * weights))
  }
  f <- NULL
  for (g in 0:(q - 1L)) {
    candidate <- c(digits[, g + 1L], 1L)
    if (poly_irreducible(candidate, p)) {
      f <- candidate
      break
    }
  }
  # Multiplication by x modulo f on digit vectors: shift up, then reduce the
  # overflowing x^e by x^e = -(f_0 + f_1 x + ... + f_(e-1) x^(e-1)).
  times_x <- function(d) {
    top <- d[e, ]
    shifted <- rbind(0L, d[-e, , drop = FALSE])
    (shifted - outer(f[seq_len(e)], top)) %% p
  }
  # powers[[i + 1]] holds the digits of a x^i for every element a.
  powers <- list(digits)
  for (i in seq_len(e - 1L)) powers[[i + 1L]] <- times_x(powers[[i]])
  mul <- matrix(0L, q, q)
  for (b in elements) {
    product <- matrix(0L, e, q)
    for (i in seq_len(e)) {
      coefficient <- digits[i, b + 1L]
      if (coefficient != 0L) product <- product + coefficient * powers[[i]]
    }
    mul[, b + 1L] <- as.integer(colSums((product %% p) * weights))
  }
  list(add = add, mul = mul)
}

#' Prime-power factors p^e of q (increasing p); integer(0) for q < 2
#' @noRd
prime_power_factors <- function(q) {
  q <- as.integer(q)
  factors <- integer(0)
  p <- 2L
  while (q > 1L) {
    if (q %% p == 0L) {
      power <- 1L
      while (q %% p == 0L) {
        q <- q %/% p
        power <- power * p
      }
      factors <- c(factors, power)
    }
    p <- p + 1L
  }
  factors
}

#' Number of mutually orthogonal Latin squares of order q that `mols()` builds:
#' q - 1 when q is a prime power, otherwise the MacNeish count, the least
#' p^e - 1 over the prime-power factors of q.
#' @noRd
mols_max <- function(q) {
  q <- as.integer(q)
  if (is.na(q) || q < 2L) return(0L)
  if (!is.null(prime_power(q))) return(q - 1L)
  as.integer(min(prime_power_factors(q)) - 1L)
}

#' `count` mutually orthogonal Latin squares of order q (q x q, 0-based), or
#' NULL when there are not that many.
#'
#' Prime power q: the Bose squares L_a(x, y) = a x + y over GF(q), a =
#' 1..count. Otherwise, with q = q_1 q_2 ... the prime-power factors, square a
#' is the direct product of the factors' a-th squares (MacNeish 1922): cell
#' ((x1, x2), (y1, y2)), x = x1 q2 + x2, holds L1[x1, y1] q2 + L2[x2, y2],
#' folded over the factors in turn. Direct products of orthogonal squares are
#' orthogonal, so the count is the least p^e - 1.
#' @noRd
mols <- function(q, count) {
  if (count == 0L) return(list())
  if (count < 0L || count > mols_max(q)) return(NULL)
  q <- as.integer(q)
  if (is.null(prime_power(q))) return(macneish_mols(q, count))
  field <- gf_tables(q)
  x <- rep(0:(q - 1L), times = q)  # row index of each cell, column-major
  y <- rep(0:(q - 1L), each = q)
  lapply(seq_len(count), function(a) {
    matrix(field$add[cbind(field$mul[a + 1L, x + 1L] + 1L, y + 1L)], q, q)
  })
}

#' MacNeish (1922) direct product of the prime-power factors' squares;
#' `count` must not exceed the least p^e - 1
#' @noRd
macneish_mols <- function(q, count) {
  factors <- prime_power_factors(q)
  squares <- mols(factors[1L], count)
  for (q2 in factors[-1L]) {
    second <- mols(q2, count)
    q1 <- nrow(squares[[1L]])
    squares <- lapply(seq_len(count), function(a) {
      kronecker(squares[[a]] * q2, matrix(1L, q2, q2)) +
        kronecker(matrix(1L, q1, q1), second[[a]])
    })
  }
  lapply(squares, function(square) {
    storage.mode(square) <- "integer"
    square
  })
}

#' Groups of a square lattice with `classes` replicates: a q^2 x classes
#' matrix of 0-based groups. Treatment i q + j (row i q + j + 1) is in group i
#' of class 1, group j of class 2 and group L[[m - 2]][i + 1, j + 1] of class
#' m >= 3. NULL when there are not enough orthogonal squares.
#' @noRd
lattice_classes <- function(q, classes) {
  if (classes < 1L || classes > 2L + mols_max(q)) return(NULL)
  q <- as.integer(q)
  squares <- mols(q, max(0L, classes - 2L))
  if (is.null(squares)) return(NULL)
  i <- rep(0:(q - 1L), each = q)
  j <- rep(0:(q - 1L), times = q)
  groups <- cbind(i, j)[, seq_len(min(classes, 2L)), drop = FALSE]
  for (m in seq_along(squares)) groups <- cbind(groups, squares[[m]][cbind(i + 1L, j + 1L)])
  unname(groups)
}

#' Algebraic start for a resolvable shape (v treatments, r replicates, b
#' blocks per replicate), or NULL
#'
#' The shape is recognised from (v, b) alone, whatever function asked for the
#' design: a square lattice when v = b^2 and 2 <= r <= 2 + `mols_max(b)`.
#' The start holds v r 1-based treatments in engine plot order (replicate m
#' in plots (m - 1) v + 1 .. m v, block j of it in the j-th run of v / b
#' plots). Treatment labels and the block order within each replicate are
#' permuted with `sample.int()`; returning NULL makes no random draws, so
#' shapes without a construction keep their seeded results.
#' @noRd
resolvable_lattice_start <- function(v, r, b) {
  if (b < 2 || r < 2 || v != b * b) return(NULL)
  if (r > 2L + mols_max(b)) return(NULL)
  groups <- lattice_classes(b, r)
  if (is.null(groups)) return(NULL)
  v <- as.integer(v)
  b <- as.integer(b)
  groups <- groups[sample.int(v), , drop = FALSE]
  for (m in seq_len(r)) groups[, m] <- (sample.int(b) - 1L)[groups[, m] + 1L]
  as.integer(unlist(lapply(seq_len(r), function(m) order(groups[, m])), use.names = FALSE))
}

#' Efficiencies of a square lattice of order q with r <= q + 1 orthogonal
#' replicates (Level 2, blocks within replicates)
#'
#' With C = r I - N N' / q and the replicates' group spaces mutually
#' orthogonal, the efficiency factors are (r - 1) / r on the r (q - 1)
#' contrasts between groups of one replicate and 1 on the other
#' (q - 1)^2 - (r - 2)(q - 1) contrasts. D and A are their geometric and
#' harmonic means.
#' @noRd
square_lattice_efficiencies <- function(q, r) {
  contrasts <- q * q - 1
  low <- r * (q - 1)
  factor <- (r - 1) / r
  c(D = factor^(low / contrasts), A = contrasts / (low / factor + contrasts - low))
}
