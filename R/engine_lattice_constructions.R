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
#' 1922, Ann. Math. 23:221-227), and at q = 3m + 1 (10, 22, 34, 46, ...) a
#' pair from a quasi-difference matrix (Bose, Shrikhande & Parker 1960). The
#' remaining orders below 50 use pairs found by FielDHub's own search
#' (`stored_mols`, R/engine_mols_data.R) or products of the above.
#' Rectangular lattices (v = s (s - 1), blocks of s - 1) come from a square
#' lattice with one block deleted (Shrikhande's method, Cochran & Cox 1957,
#' p. 417) or, at r = 3, from a Latin square with a transversal deleted.
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
#' p^e - 1 over the prime-power factors of q. When that is below 2 (q = 2 mod
#' 4) it is 2 if q = 3m + 1 with m not 2 or 6 and two orthogonal squares of
#' order m (`three_m_plus_one()`), if a pair of order q is stored
#' (`stored_mols`), or if q = m n with two squares of each order
#' (`product_split()`).
#' @noRd
mols_max <- function(q) {
  q <- as.integer(q)
  if (is.na(q) || q < 2L) return(0L)
  if (!is.null(prime_power(q))) return(q - 1L)
  count <- as.integer(min(prime_power_factors(q)) - 1L)
  if (count < 2L && (three_m_plus_one_order(q) || has_stored_mols(q) || !is.null(product_split(q)))) {
    count <- 2L
  }
  count
}

#' Whether an orthogonal pair of order q is stored in `stored_mols`
#' @noRd
has_stored_mols <- function(q) !is.null(stored_mols[[as.character(q)]])

#' A factorisation q = m n (the smallest m > 1) with at least two orthogonal
#' squares of each order, as c(m, n), or NULL
#'
#' The direct product of orthogonal squares of orders m and n is orthogonal
#' whatever the factors (MacNeish 1922), so N(m n) >= min(N(m), N(n)) also
#' holds when the factors' squares come from `three_m_plus_one()` or
#' `stored_mols`: order 30 = 3 x 10.
#' @noRd
product_split <- function(q) {
  m <- 2L
  while (m * m <= q) {
    if (q %% m == 0L && mols_max(m) >= 2L && mols_max(q %/% m) >= 2L) return(c(m, q %/% m))
    m <- m + 1L
  }
  NULL
}

#' Whether q = 3m + 1 with m not in {2, 6} and `mols_max(m) >= 2`
#' @noRd
three_m_plus_one_order <- function(q) {
  if (q %% 3L != 1L) return(FALSE)
  m <- (q - 1L) %/% 3L
  !m %in% c(2L, 6L) && mols_max(m) >= 2L
}

#' Two orthogonal Latin squares of order q = 3m + 1 (Bose, Shrikhande &
#' Parker 1960, Canad. J. Math. 12:189-203; van Lint & Wilson, ch. 22)
#'
#' Nested-engine spec section 6.5. Symbols are 0..2m (the group Z_(2m+1))
#' and the m infinite points inf_i = 2m + i. The columns of an OA(4, q) are
#' (1) for i = 1..m the base columns (inf_i, 0, i, 2i), (0, inf_i, 2i, i),
#' (i, 2i, inf_i, 0), (2i, i, 0, inf_i), each developed over Z_(2m+1) (g =
#' 0..2m added to the finite entries, inf_i fixed); (2) the 2m + 1 constant
#' columns (g, g, g, g); (3) the m^2 columns (2m + 1 + x, 2m + 1 + y,
#' 2m + 1 + H1[x, y], 2m + 1 + H2[x, y]) from two orthogonal squares H1, H2
#' of order m, filling the infinite hole. That is 4m (2m + 1) + 2m + 1 + m^2 =
#' q^2 columns; rows 1-2 index the cell and rows 3-4 give the two squares.
#' @noRd
three_m_plus_one <- function(q) {
  q <- as.integer(q)
  m <- (q - 1L) %/% 3L
  hole <- mols(m, 2L)
  z <- 2L * m + 1L
  g <- 0:(z - 1L)
  develop <- function(base, inf) {
    # One developed column per g: finite entries shifted by g mod z.
    vapply(base, function(entry) if (entry == inf) rep(inf, z) else (entry + g) %% z,
           integer(z))  # z x 4
  }
  pieces <- list()
  for (i in seq_len(m)) {
    inf <- 2L * m + i
    for (base in list(c(inf, 0L, i, 2L * i), c(0L, inf, 2L * i, i),
                      c(i, 2L * i, inf, 0L), c(2L * i, i, 0L, inf))) {
      pieces[[length(pieces) + 1L]] <- develop(base, inf)
    }
  }
  pieces[[length(pieces) + 1L]] <- cbind(g, g, g, g)
  x <- rep(0:(m - 1L), times = m)
  y <- rep(0:(m - 1L), each = m)
  hole_symbols <- vapply(hole, function(h) h[cbind(x + 1L, y + 1L)], integer(m * m))
  pieces[[length(pieces) + 1L]] <- z + cbind(x, y, matrix(hole_symbols, m * m, 2L))
  oa <- do.call(rbind, pieces)  # q^2 x 4, one OA column per row
  colnames(oa) <- c("row", "column", "first", "second")
  cells <- cbind(oa[, "row"] + 1L, oa[, "column"] + 1L)
  lapply(c("first", "second"), function(symbol) {
    square <- matrix(NA_integer_, q, q)
    square[cells] <- as.integer(oa[, symbol])
    square
  })
}

#' `count` mutually orthogonal Latin squares of order q (q x q, 0-based), or
#' NULL when there are not that many.
#'
#' Prime power q: the Bose squares L_a(x, y) = a x + y over GF(q), a =
#' 1..count. Otherwise, with q = q_1 q_2 ... the prime-power factors, square a
#' is the direct product of the factors' a-th squares (MacNeish 1922): cell
#' ((x1, x2), (y1, y2)), x = x1 q2 + x2, holds L1[x1, y1] q2 + L2[x2, y2],
#' folded over the factors in turn. Direct products of orthogonal squares are
#' orthogonal, so the count is the least p^e - 1. When that is below 2, two
#' squares come from `three_m_plus_one()` when q = 3m + 1, else from
#' `stored_mols`, else from the direct product over `product_split()`.
#' @noRd
mols <- function(q, count) {
  if (count == 0L) return(list())
  if (count < 0L || count > mols_max(q)) return(NULL)
  q <- as.integer(q)
  if (is.null(prime_power(q))) {
    factors <- prime_power_factors(q)
    if (count <= min(factors) - 1L) return(direct_product_mols(factors, count))
    if (three_m_plus_one_order(q)) return(three_m_plus_one(q)[seq_len(count)])
    if (has_stored_mols(q)) return(unname(stored_mols[[as.character(q)]][seq_len(count)]))
    return(direct_product_mols(product_split(q), count))
  }
  field <- gf_tables(q)
  x <- rep(0:(q - 1L), times = q)  # row index of each cell, column-major
  y <- rep(0:(q - 1L), each = q)
  lapply(seq_len(count), function(a) {
    matrix(field$add[cbind(field$mul[a + 1L, x + 1L] + 1L, y + 1L)], q, q)
  })
}

#' MacNeish (1922) direct product of `count` orthogonal squares of each order
#' in `factors`; every order must have that many
#' @noRd
direct_product_mols <- function(factors, count) {
  q1 <- factors[1L]
  squares <- mols(q1, count)
  for (q2 in factors[-1L]) {
    second <- mols(q2, count)
    squares <- lapply(seq_len(count), function(a) {
      kronecker(squares[[a]] * q2, matrix(1L, q2, q2)) +
        kronecker(matrix(1L, q1, q1), second[[a]])
    })
    q1 <- q1 * q2
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

#' Groups of a rectangular lattice: v = s (s - 1) treatments in s blocks of
#' s - 1 per replicate, as a v x r matrix of 0-based groups, or NULL
#'
#' Shrikhande's method (Cochran & Cox 1957, p. 417) when r + 1 <= 2 +
#' `mols_max(s)`: in a square lattice of order s with r + 1 orthogonal
#' classes, group 0 of class r + 1 meets every block of the other classes in
#' exactly one treatment, so deleting its s treatments and dropping class r + 1
#' leaves r resolvable classes of s blocks of s - 1. Otherwise, at r = 3, the
#' rows, columns and symbols of the cells of a Latin square of order s outside
#' one transversal (`transversal_square()`): each row, column and symbol loses
#' exactly one cell.
#' @noRd
rectangular_lattice_classes <- function(s, r) {
  s <- as.integer(s)
  r <- as.integer(r)
  if (s < 3L || r < 2L) return(NULL)
  if (r + 1L <= 2L + mols_max(s)) {
    groups <- lattice_classes(s, r + 1L)
    if (is.null(groups)) return(NULL)
    return(groups[groups[, r + 1L] != 0L, seq_len(r), drop = FALSE])
  }
  if (r != 3L) return(NULL)
  square <- transversal_square(s)
  keep <- matrix(TRUE, s, s)
  keep[square$transversal] <- FALSE
  rows <- row(keep)[keep]
  columns <- col(keep)[keep]
  unname(cbind(rows - 1L, columns - 1L, square$square[keep]))
}

#' A Latin square of order s >= 3 (0-based symbols) and one of its
#' transversals, `list(square, transversal)`, the transversal as an s x 2
#' matrix of 1-based (row, column) cells
#'
#' Odd s: the cyclic square (i + j) mod s, whose main diagonal holds the
#' distinct symbols 2i mod s. Even s: the cyclic square of the odd order
#' u = s - 1 prolonged along its main diagonal (Denes & Keedwell 1974, section
#' 1.5): each diagonal cell's symbol moves to the new last column of its row
#' and the new last row of its column, the diagonal cells and the corner take
#' the new symbol u. Its transversal is found by depth-first search over the
#' rows, deterministic and with no random draws: row i (0-based) tries the
#' columns i + 1, i + 2, ... cyclically. The search then takes no step back,
#' because the first choices already form a transversal: cells (i, i + 1 mod
#' u) for i < u hold the distinct symbols 2 i + 1 mod u (u odd) and miss the
#' diagonal, and the corner (u, u) adds the symbol u (own derivation). Plain
#' increasing column order needed 50 s at s = 38. NULL if the search finds
#' none.
#' @noRd
transversal_square <- function(s) {
  s <- as.integer(s)
  if (s %% 2L == 1L) {
    square <- outer(0:(s - 1L), 0:(s - 1L), function(i, j) (i + j) %% s)
    return(list(square = square, transversal = cbind(seq_len(s), seq_len(s))))
  }
  u <- s - 1L
  square <- matrix(u, s, s)
  square[seq_len(u), seq_len(u)] <- outer(0:(u - 1L), 0:(u - 1L), function(i, j) (i + j) %% u)
  diagonal <- cbind(seq_len(u), seq_len(u))
  square[cbind(seq_len(u), s)] <- square[diagonal]
  square[cbind(s, seq_len(u))] <- square[diagonal]
  square[diagonal] <- u
  storage.mode(square) <- "integer"
  column_used <- logical(s)
  symbol_used <- logical(s)
  pick <- integer(s)  # column chosen in each row, 0 while unset
  row <- 1L
  while (row >= 1L && row <= s) {
    if (pick[row] > 0L) {
      # Undo this row's choice before trying its next column.
      column_used[pick[row]] <- FALSE
      symbol_used[square[row, pick[row]] + 1L] <- FALSE
    }
    # Columns in cyclic order from the one after the diagonal; `tried` is the
    # number of them this row has used up.
    order_of_row <- (row + seq_len(s) - 1L) %% s + 1L
    tried <- if (pick[row] == 0L) 0L else match(pick[row], order_of_row)
    next_column <- 0L
    for (column in order_of_row[seq_len(s) > tried]) {
      if (!column_used[column] && !symbol_used[square[row, column] + 1L]) {
        next_column <- column
        break
      }
    }
    if (next_column == 0L) {
      pick[row] <- 0L
      row <- row - 1L
    } else {
      pick[row] <- next_column
      column_used[next_column] <- TRUE
      symbol_used[square[row, next_column] + 1L] <- TRUE
      row <- row + 1L
    }
  }
  if (row < 1L) return(NULL)
  list(square = square, transversal = cbind(seq_len(s), pick))
}

#' Groups of a lattice for a resolvable shape (v, r, b): the square lattice
#' when v = b^2 (`lattice_classes()`), the rectangular lattice when
#' v = b (b - 1) (`rectangular_lattice_classes()`), otherwise NULL
#' @noRd
lattice_groups <- function(v, r, b) {
  if (b < 2 || r < 2) return(NULL)
  if (v == b * b) {
    if (r > 2L + mols_max(b)) return(NULL)
    return(lattice_classes(b, r))
  }
  if (b >= 3 && v == b * (b - 1)) return(rectangular_lattice_classes(b, r))
  NULL
}

#' Algebraic start for a resolvable shape (v treatments, r replicates, b
#' blocks per replicate), or NULL
#'
#' The shape is recognised from (v, b) alone, whatever function asked for the
#' design: a square lattice when v = b^2 and 2 <= r <= 2 + `mols_max(b)`, a
#' rectangular lattice when v = b (b - 1) and either r + 1 <= 2 +
#' `mols_max(b)` or r = 3 (`lattice_groups()`). The start holds v r 1-based
#' treatments in engine plot order (replicate m in plots (m - 1) v + 1 .. m v,
#' block j of it in the j-th run of v / b plots). Treatment labels and the
#' block order within each replicate are permuted with `sample.int()`;
#' returning NULL makes no random draws, so shapes without a construction keep
#' their seeded results.
#' @noRd
resolvable_lattice_start <- function(v, r, b) {
  groups <- lattice_groups(v, r, b)
  if (is.null(groups)) return(NULL)
  v <- as.integer(v)
  b <- as.integer(b)
  groups <- groups[sample.int(v), , drop = FALSE]
  for (m in seq_len(r)) groups[, m] <- (sample.int(b) - 1L)[groups[, m] + 1L]
  as.integer(unlist(lapply(seq_len(r), function(m) order(groups[, m])), use.names = FALSE))
}

#' Level 2 efficiencies of a resolvable design from its block concurrences
#'
#' `start` in engine plot order (see `resolvable_lattice_start()`), blocks of
#' k = v / b. With N the v x B incidence, the efficiency factors are the
#' eigenvalues of I - N N' / (r k) on the treatment contrasts. The nonzero
#' eigenvalues of N N' are those of the B x B matrix N' N, whose (i, j) entry
#' counts the treatments blocks i and j share; one of them, r k, belongs to the
#' grand mean. So the rank(N) - 1 other nonzero eigenvalues mu give factors
#' 1 - mu / (r k), and the remaining v - rank(N) contrasts have factor 1. D and
#' A are their geometric and harmonic means. Costs O(B^3) and B^2 memory,
#' not v B. Own derivation.
#' @noRd
resolvable_dual_efficiencies <- function(start, v, r, b) {
  v <- as.integer(v)
  k <- v %/% as.integer(b)
  total <- as.integer(r) * as.integer(b)
  block <- matrix(0L, v, r)  # 1-based block of each treatment in each replicate
  for (m in seq_len(r)) {
    block[start[(m - 1L) * v + seq_len(v)], m] <- (m - 1L) * b + (seq_len(v) - 1L) %/% k + 1L
  }
  concurrence <- matrix(0, total, total)
  for (m in seq_len(r)) {
    for (l in seq_len(r)) {
      counts <- tabulate((block[, m] - 1L) * total + block[, l], total * total)
      concurrence <- concurrence + matrix(counts, total, total, byrow = TRUE)
    }
  }
  mu <- eigen(concurrence / (r * k), symmetric = TRUE, only.values = TRUE)$values
  nonzero <- mu[mu > 1e-9]
  factors <- c(1 - nonzero[-1L], rep(1, v - length(nonzero)))
  c(D = exp(mean(log(factors))), A = (v - 1) / sum(1 / factors))
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
