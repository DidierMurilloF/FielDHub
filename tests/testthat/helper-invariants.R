# Independent structural checks: no design-engine or result-validator calls.
# Explicit expected levels retain missing rows, replicates, and locations.
has_invariant_counts <- function(book, levels, expected = 1L) {
  columns <- names(levels)
  if (!is.data.frame(book) || !all(columns %in% names(book)) ||
      !length(columns) || any(lengths(levels) == 0L)) return(FALSE)
  factors <- Map(function(column, values) factor(column, levels = values), book[columns], levels)
  if (any(vapply(factors, anyNA, logical(1)))) return(FALSE)
  counts <- do.call(table, factors)
  if (!length(expected) || !length(counts) || length(counts) %% length(expected) != 0L) return(FALSE)
  all(as.numeric(counts) == rep_len(expected, length(counts)))
}

has_unique_units <- function(book, columns) {
  is.data.frame(book) && nrow(book) > 0L && all(columns %in% names(book)) &&
    !anyNA(book[columns]) && !anyDuplicated(book[columns])
}

has_same_units <- function(first, second, columns) {
  ordered <- function(book) {
    book <- as.data.frame(lapply(book[columns], as.character), stringsAsFactors = FALSE)
    book <- book[do.call(order, book), , drop = FALSE]
    rownames(book) <- NULL
    book
  }
  identical(ordered(first), ordered(second))
}

# Independently form C = diag(r) - N diag(1/k) N' from incidence counts.
# This oracle is for equally replicated treatments in nested complete blocks.
block_efficiency_oracle <- function(book) {
  block <- interaction(book$REP, book$IBLOCK, drop = TRUE)
  incidence <- table(factor(book$ENTRY), block)
  replication <- rowSums(incidence)
  stopifnot(length(replication) > 1L, length(unique(replication)) == 1L)
  scaled <- sweep(incidence, 2, sqrt(colSums(incidence)), `/`)
  information <- diag(as.numeric(replication)) - tcrossprod(scaled)
  eigenvalues <- eigen(information, symmetric = TRUE, only.values = TRUE)$values
  nonzero <- eigenvalues[eigenvalues > max(1, max(eigenvalues)) * 1e-10]
  if (length(nonzero) != length(replication) - 1L) return(0)
  length(nonzero) / sum(replication[1L] / nonzero)
}
