#' Treatment and block incidence without unused factor levels
#' @noRd
block_incidence <- function(treatment, block) {
  if (!is.atomic(treatment) || !is.atomic(block) || !is.null(dim(treatment)) ||
      !is.null(dim(block)) || length(treatment) != length(block) ||
      length(treatment) == 0L || anyNA(treatment) || anyNA(block)) {
    fieldhub_abort("Treatment and block labels must be complete vectors of equal length.")
  }
  incidence <- table(factor(treatment), factor(block))
  if (nrow(incidence) < 2L) {
    fieldhub_abort("At least two treatments are needed for an efficiency comparison.")
  }
  incidence
}

#' Connectivity of the treatment/block incidence graph
#'
#' Each nonterminal expansion reaches another treatment, so at most the number
#' of treatments is required. No numerical rank tolerance decides connectivity.
#' @noRd
block_model_connected <- function(incidence) {
  reached <- seq_len(nrow(incidence)) == 1L
  for (step in seq_len(nrow(incidence))) {
    blocks <- colSums(incidence[reached, , drop = FALSE]) > 0
    expanded <- unname(rowSums(incidence[, blocks, drop = FALSE]) > 0)
    if (all(expanded)) return(TRUE)
    if (identical(expanded, reached)) return(FALSE)
    reached <- expanded
  }
  FALSE
}

#' Canonical block efficiencies from normalized incidence counts
#'
#' Removing the mean from D_r^(-1/2) N D_k^(-1/2) leaves treatment/block
#' association. Its nontrivial squared singular values are losses relative to
#' complete randomization. Use the smaller singular system, not plot-by-plot
#' projection matrices. Disconnected comparisons have zero overall efficiency.
#' @noRd
blockEstEffics <- function(TF, BF) {
  block_incidence_efficiency(block_incidence(TF, BF))
}

#' Efficiency worker reusing the validated report incidence table
#' @noRd
block_incidence_efficiency <- function(incidence) {
  if (!block_model_connected(incidence)) return(list(Deffic = 0, Aeffic = 0))
  replication <- rowSums(incidence)
  block_size <- colSums(incidence)
  total <- sum(replication)
  association <- incidence / sqrt(outer(replication, block_size)) -
    outer(sqrt(replication / total), sqrt(block_size / total))
  singular <- svd(association, nu = 0L, nv = 0L)$d
  # Drop the zero singular direction for the mean. Additional treatment
  # directions outside the smaller system have efficiency one.
  efficiencies <- c(1 - singular[seq_len(length(singular) - 1L)]^2,
                     rep(1, nrow(incidence) - min(dim(incidence))))
  if (any(!is.finite(efficiencies)) || any(efficiencies <= 0)) {
    fieldhub_abort("The connected block model is numerically singular.",
                   class = "fieldhub_numerical_error")
  }
  list(Deffic = round(exp(mean(log(efficiencies))), 7),
       Aeffic = round(length(efficiencies) / sum(1 / efficiencies), 7))
}

#' Efficiency report for named treatment, plot, and block-factor columns
#'
#' A-efficiency bounds use the documented blocksdesign::A_bound() API.
#' Retain the established report column names, order, and seven-digit rounding.
#' @noRd
BlockEfficiencies <- function(Design) {
  if (!is.data.frame(Design) || nrow(Design) == 0L || anyNA(names(Design)) ||
      anyDuplicated(names(Design)) || !all(c("plots", "treatments") %in% names(Design))) {
    fieldhub_abort("A block model needs named plots, treatments, and block-factor columns.")
  }
  block_columns <- setdiff(names(Design), c("plots", "treatments"))
  if (!length(block_columns)) fieldhub_abort("A block model needs at least one block factor.")
  reports <- lapply(seq_along(block_columns), function(level) {
    block <- Design[[block_columns[level]]]
    incidence <- block_incidence(Design$treatments, block)
    replication <- rowSums(incidence)
    block_size <- colSums(incidence)
    bound <- if (length(unique(replication)) == 1L && length(unique(block_size)) == 1L) {
      blocksdesign::A_bound(nrow(Design), nrow(incidence), ncol(incidence))
    } else 1
    efficiency <- block_incidence_efficiency(incidence)
    data.frame(Level = level, Blocks = ncol(incidence),
               `D-Efficiency` = efficiency$Deffic, `A-Efficiency` = efficiency$Aeffic,
               `A-Bound` = round(bound, 7), check.names = FALSE)
  })
  do.call(rbind, reports)
}
