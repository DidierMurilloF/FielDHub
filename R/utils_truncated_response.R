#' Validate a classic field book and describe its response distributions
#'
#' Treatment means span the middle two thirds of the requested range. The
#' standard deviation retains the established spacing/replication rule. The
#' model requires at least two treatments and one unambiguous named treatment
#' column; unrelated user columns do not enter the response calculation.
#' @noRd
truncated_response_spec <- function(a, b, data) {
  finite_scalar <- function(x) {
    is.numeric(x) && !is.complex(x) && length(x) == 1L && is.finite(x)
  }
  if (!finite_scalar(a) || !finite_scalar(b) || a >= b ||
      !is.finite(b - a) || !is.finite(a + b)) {
    fieldhub_abort("The simulated-response range must have finite lower < upper bounds and a finite width and midpoint.")
  }
  if (!is.data.frame(data) || nrow(data) == 0L || anyNA(names(data)) ||
      anyDuplicated(names(data)) > 0L || !all(c("LOCATION", "PLOT") %in% names(data))) {
    fieldhub_abort("The simulation field book must have rows and unique LOCATION and PLOT columns.")
  }
  column <- intersect(c("TREATMENT", "TRT_COMB"), names(data))
  if (length(column) != 1L) {
    fieldhub_abort("The simulation field book must contain exactly one treatment column: TREATMENT or TRT_COMB.")
  }
  if ("RESP" %in% names(data)) {
    fieldhub_abort("The field book already contains the simulated-response column RESP.")
  }
  for (name in c("LOCATION", "PLOT", column)) {
    value <- data[[name]]
    if (!is.atomic(value) || !is.null(dim(value)) || is.complex(value) || anyNA(value)) {
      fieldhub_abort("`", name, "` must be a nonmissing atomic vector.")
    }
  }
  treatments <- factor(data[[column]])
  nt <- nlevels(treatments)
  if (nt < 2L) fieldhub_abort("The response model requires at least two distinct treatments.")
  counts <- as.vector(table(treatments))
  reps <- max(counts)
  midpoint <- (a + b) / 2
  spread <- diff(c(a, b)) / 3
  means <- seq(midpoint - spread, midpoint + spread, length.out = nt)
  deviation <- diff(means)[1] / reps
  deviation <- deviation + if (reps > 3) 0.13 * reps else 0.3
  list(treatment_column = column, treatments = treatments, means = means,
       sd = deviation, counts = counts)
}

#' Simulate treatment-specific truncated-normal responses
#'
#' An explicit seed preserves the caller's random-number state. With no seed,
#' this internal simulator consumes its caller's already scoped stream. Values
#' are rounded to two decimals, which can move them by at most 0.005 beyond a
#' bound specified at finer precision. Existing row and location order is kept
#' through the established location-then-plot sorting.
#' @importFrom stats pnorm qnorm runif
#' @noRd
norm_trunc <- function(a = NULL, b = NULL, data = NULL, seed = NULL) {
  model <- truncated_response_spec(a, b, data)
  if (!is.null(seed)) {
    local_rng_state()
    set.seed(resolve_seed(seed))
  }
  trt.sample <- sample(levels(model$treatments))
  counts <- model$counts[match(trt.sample, levels(model$treatments))]
  resp <- vector("list", length(model$means))
  z <- 1
  for (mean in model$means) {
    uniforms <- runif(counts[z], pnorm(a, mean, model$sd), pnorm(b, mean, model$sd))
    resp[[z]] <- round(qnorm(uniforms, mean, model$sd), 2)
    z <- z + 1
  }
  names(resp) <- trt.sample
  result <- data
  result$RESP <- NA_real_
  labels <- as.character(data[[model$treatment_column]])
  for (treatment in trt.sample) {
    result$RESP[labels == treatment] <- resp[[treatment]]
  }
  result$LOCATION <- factor(result$LOCATION, unique(as.character(result$LOCATION)))
  result[order(result$LOCATION, result$PLOT), ]
}
