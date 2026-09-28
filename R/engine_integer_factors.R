#' Validate input to the integer factor helpers
#'
#' @param x Value to validate.
#' @param scalar Whether exactly one value is required.
#' @param call Call to report when validation fails.
#' @return `x`, invisibly.
#' @noRd
validate_factor_input <- function(x, scalar = FALSE, call = sys.call(-1)) {
  invalid <- !is.numeric(x) || length(x) == 0 || anyNA(x) ||
    any(!is.finite(x)) || any(x < 1) || any(x %% 1 != 0) ||
    any(x > .Machine$integer.max)
  if (scalar) invalid <- invalid || length(x) != 1
  if (invalid) {
    requirement <- if (scalar) "one positive whole number" else "positive whole numbers"
    fieldhub_abort(paste0("`x` must contain ", requirement, "."), call = call)
  }
  invisible(x)
}

#' Test whether positive whole numbers are prime
#'
#' @param x Numeric vector of positive whole numbers.
#' @return A logical vector with the same length as `x`.
#' @noRd
is_prime <- function(x) {
  validate_factor_input(x, call = sys.call())
  vapply(x, function(value) {
    if (value < 2) return(FALSE)
    if (value == 2) return(TRUE)
    if (value %% 2 == 0) return(FALSE)
    limit <- floor(sqrt(value))
    if (limit < 3) return(TRUE)
    !any(value %% seq.int(3, limit, by = 2) == 0)
  }, logical(1))
}

#' Find every positive divisor of a whole number
#'
#' @param x One positive whole number.
#' @return A sorted integer vector.
#' @noRd
integer_divisors <- function(x) {
  validate_factor_input(x, scalar = TRUE, call = sys.call())
  lower <- seq_len(floor(sqrt(x)))
  lower <- lower[x %% lower == 0]
  as.integer(sort(unique(c(lower, x %/% lower))))
}

#' Factor a positive whole number into primes
#'
#' @param x One positive whole number.
#' @return A numeric vector of prime factors, including multiplicity. One is
#'   represented by `1`, matching the previous dependency's contract.
#' @noRd
prime_factors <- function(x) {
  validate_factor_input(x, scalar = TRUE, call = sys.call())
  if (x == 1) return(1)

  factors <- numeric(0)
  remaining <- x
  divisor <- 2
  while (divisor * divisor <= remaining) {
    while (remaining %% divisor == 0) {
      factors <- c(factors, divisor)
      remaining <- remaining / divisor
    }
    divisor <- if (divisor == 2) 3 else divisor + 2
  }
  if (remaining > 1) factors <- c(factors, remaining)
  factors
}
