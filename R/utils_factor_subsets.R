#' factor_subsets 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
factor_subsets <- function(n, 
                           diagonal = FALSE, 
                           augmented = FALSE, 
                           all_factors = FALSE) {
  factors <- numbers::primeFactors(n)
  left <- 1
  right <- 1
  combos <- list()
  labels <- list()
  both <- list()
  if(length(sq(length(factors))) == 2) {
    if (all_factors == TRUE) {
      comb_factors <- matrix(data = c(1,factors, factors, 1), 
                             nrow = 2, 
                             ncol = 2, 
                             byrow = TRUE)
      return(list(comb_factors = comb_factors))
    } else return(NULL)
  } else {
    list <- sq(length(factors))[-1,][-(nrow(sq(length(factors)))-1),]
  }
  for (i in 1:nrow(list)) {
    for (n in 1:length(factors)) {
      if (list[i,][n]==1) {
        left <- left*factors[n]
      } else {
        right <- right*factors[n]
      }
    }
    cols <- 3
    rows <- 3
    if (diagonal) {
      cols <- 9
      rows <- 4
    } else if (augmented) {
      cols <- 3
      rows <- 0
    } else if (all_factors) {
      cols <- 1
      rows <- 1
    }
    if(left > rows & right > cols) {
      combos[[i]] <- c(row = left, col = right)
      labels[[i]] <- paste(left,"x",right,sep = " ")
    }
    left <- 1
    right <- 1
  }
  combos <- unique(combos[!sapply(combos,is.null)])
  labels <- unique(labels[!sapply(labels,is.null)])
  
  if (all_factors) {
    c_factors <- labels
    n_labels <- length(labels)
    comb_factors <- matrix(data = NA, nrow = n_labels, ncol = 2)
    for (i in 1:n_labels) {
      comb_factors[i,] <- as.numeric(unlist(strsplit(c_factors[[i]],  " x ")))
    }
  } else comb_factors <- NULL
  
  if (length(combos) == 0) {
    return(NULL)
  } else return(list(combos = combos, 
                     labels = labels, 
                     comb_factors = comb_factors))
}

#' sq
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
sq <- function (J, s = NULL) 
{
  M = NULL
  if (J > 0) {
    if (!is.null(s)) {
      if (s == J) 
        M = matrix(1, 1, J)
      if (s > 1 & s < J) 
        for (i in 1:(J - s + 1)) {
          S = sq(J - i, s - 1)
          if (is.null(S)) 
            r = 0
          else r = dim(S)[1]
          M1 = cbind(matrix(0, r, i - 1), matrix(1, r, 
                                                 1), S)
          M = rbind(M1, M)
        }
      if (s == 1) {
        M = matrix(0, J, J)
        for (j in 1:J) M[j, J - j + 1] = 1
      }
      if (s == 0) 
        M = matrix(0, 1, J)
    }
    else {
      if (J == 1) {
        M = matrix(c(0, 1), 2, 1)
      }
      else {
        M1 = sq(J - 1)
        M = rbind(cbind(0, M1), cbind(1, M1))
      }
    }
  }
  return(M)
}