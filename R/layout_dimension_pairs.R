#' Rectangular factor pairs in the established dimension-option order
#'
#' @description Find feasible rectangles without enumerating every subset of
#' repeated prime factors. Filter precedence and prime-number behavior are
#' retained for compatibility with existing layout choices.
#'
#' @author Matthew Seelfedt [aut]
#' @return A list of named row/column pairs, labels and an optional matrix,
#' or NULL when the requested filters admit no rectangle.
#' @noRd
factor_subsets <- function(n, diagonal = FALSE, augmented = FALSE, all_factors = FALSE) {
    factors <- prime_factors(n)
    if (length(factors) == 1L) {
        if (all_factors == TRUE) {
            return(list(comb_factors = matrix(c(1, factors, factors, 1),
                                               nrow = 2, ncol = 2, byrow = TRUE)))
        }
        return(NULL)
    }
    pairs <- ordered_factor_pairs(n)
    rows <- 3
    cols <- 3
    if (diagonal) {
        rows <- 4
        cols <- 9
    } else if (augmented) {
        rows <- 0
        cols <- 3
    } else if (all_factors) {
        rows <- 1
        cols <- 1
    }
    pairs <- pairs[pairs[, "row"] > rows & pairs[, "col"] > cols, , drop = FALSE]
    if (nrow(pairs) == 0L) return(NULL)
    combos <- lapply(seq_len(nrow(pairs)), function(i) {
        if (is.null(names(factors))) return(pairs[i, ])
        # Named scalar inputs historically propagate prime-factor names to
        # individual pair elements. Retain those attributes as well as values.
        remaining <- pairs[i, "row"]
        selected <- logical(length(factors))
        for (j in rev(seq_along(factors))) {
            if (remaining %% factors[j] == 0) {
                selected[j] <- TRUE
                remaining <- remaining / factors[j]
            }
        }
        left <- right <- 1
        for (j in seq_along(factors)) {
            if (selected[j]) left <- left * factors[j] else right <- right * factors[j]
        }
        c(row = left, col = right)
    })
    labels <- lapply(seq_len(nrow(pairs)), function(i) paste(pairs[i, 1], "x", pairs[i, 2]))
    list(combos = combos, labels = labels,
         comb_factors = if (all_factors) unname(pairs) else NULL)
}

#' Proper divisor pairs in legacy binary-subset first-appearance order
#'
#' A divisor determines how many copies of each prime go on the left. Its
#' first appearance in binary-subset enumeration selects the rightmost copies
#' of each repeated prime. The corresponding mask orders the distinct pairs
#' without constructing any of the 2^length(factors) subsets.
#'
#' Work is bounded by the square-root divisor search and at most 30 prime
#' factors per divisor in the supported signed-integer range.
#' @noRd
ordered_factor_pairs <- function(n) {
    factors <- prime_factors(n)
    size <- as.double(n)
    left <- as.double(integer_divisors(size))
    left <- left[left > 1 & left < size]
    groups <- rle(unname(factors))
    shifts <- length(factors) - cumsum(groups$lengths)
    remaining <- left
    masks <- numeric(length(left))
    for (i in seq_along(groups$values)) {
        exponents <- integer(length(left))
        for (power in seq_len(groups$lengths[i])) {
            divisible <- remaining %% groups$values[i] == 0
            exponents[divisible] <- exponents[divisible] + 1L
            remaining[divisible] <- remaining[divisible] / groups$values[i]
        }
        masks <- masks + (2^exponents - 1) * 2^shifts[i]
    }
    left <- left[order(masks)]
    cbind(row = left, col = size / left)
}

#' Valid field dimensions as a data frame
#'
#' @param choices Character vector of dimensions such as "10 x 20".
#' @return A data frame with the columns rows and cols, sorted by rows, or NULL
#'   when there is no choice.
#' @noRd
dimension_options <- function(choices) {
  if (is.null(choices) || length(choices) == 0) return(NULL)
  dims <- do.call(rbind, lapply(choices, function(x) {
    parts <- as.integer(trimws(strsplit(x, "x")[[1]]))
    data.frame(rows = parts[1], cols = parts[2])
  }))
  dims <- unique(dims[order(dims$rows), ])
  rownames(dims) <- NULL
  dims
}
