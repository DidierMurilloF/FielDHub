#' @importFrom stats runif
AR1xAR1_simulation <- function(nrows = NULL, ncols = NULL, ROX = NULL, 
                               ROY = NULL, minValue = NULL, 
                               maxValue = NULL, fieldbook = NULL, 
                               trail = NULL, seed = NULL) {
  if (!is.null(seed)) {
    seed <- resolve_seed(seed)
    local_rng_state()
    set.seed(seed)
  }
  rag <- diff(c(minValue, maxValue))
  sigma <- rag*0.15
  Beta <- sum(minValue, maxValue)/2
  s20 <- 0.1;H2 <- 0.5
  info <- as.data.frame(cbind(ROX, ROY, s20))
  info <- info[abs(info$ROY-info$ROX) < 0.85, ]
  ar1 <- info[1,]
  #trt <- length(levels(as.factor(fieldbook$ENTRY[fieldbook$ENTRY > 0])))
  Treatments <- as.numeric(fieldbook$ENTRY[fieldbook$ENTRY > 0])
  unique_trt <- sort(unique(Treatments), decreasing = FALSE)
  trt <- length(unique_trt)
  g.random <- matrix(0,trt, 1) 
  g.random[,1] <- rnorm(trt, mean = 0, sd = sigma)
  # genet <- data.frame(Treatment = 1:trt, g.random)
  genet <- data.frame(Treatment = unique_trt, g.random)
  matdf <- fieldbook
  plan <- ZST(n = nrows, m = ncols, 
              RHOX = ar1$ROX, RHOY = ar1$ROY, 
              s20 = ar1$s20)
  newPlan <- merge(matdf, plan, by = c("ROW","COLUMN"))
  newPlan <- newPlan[order(newPlan$ID),]
  newPlan <- newPlan[, c("ID", "ROW", "COLUMN", "ENTRY", "ZST")]
  gen <- cbind(genet[,1], genet[,2])
  colnames(gen) <- c("ENTRY","genot")
  if (0 %in% newPlan$ENTRY) {
    z <- data.frame(list(ENTRY = 0, genot = NA))
    gen <- rbind(gen, z)
  }
  merged <- merge(newPlan, gen, by = "ENTRY")
  merged$genot <- sqrt(H2) * merged$genot
  merged$resp <- Beta + merged$genot + sqrt(1 - H2) * merged$ZST
  merged <- merged[, c("ID", "ENTRY", "ROW", "COLUMN", "ZST", "genot", "resp")]
  outOrder <- merged[order(merged$ID),]
  # "resp" is the simulated response column merged[, ...] built above; rename
  # it by that name rather than by its (currently 7th) position.
  names(outOrder)[names(outOrder) == "resp"] <- trail
  outOrder$ROW <- as.factor(outOrder$ROW)
  outOrder$COLUMN <- as.factor(outOrder$COLUMN)
  label_trail <- paste(trail, ": ")
  new_outOrder <- outOrder |>
    dplyr::mutate(text = paste0("Row: ", outOrder$ROW, "\n", 
                                "Col: ", outOrder$COLUMN, "\n", 
                                "Entry: ", outOrder$ENTRY, "\n", 
                                label_trail, round(outOrder[,7],2)))
  return(list(outOrder = new_outOrder))
}

#' Append one simulated response to a field book
#'
#' @param field_book Field book whose row order was used for the simulation.
#' @param simulation Data frame returned in `outOrder` by
#'   `AR1xAR1_simulation()`.
#' @param response_name Name of the simulated-response column.
#' @param digits Number of decimal places retained.
#'
#' @return `field_book` with the response appended as its last column.
#' @noRd
append_simulated_response <- function(field_book, simulation, response_name,
                                      digits = 2) {
  if (!is.data.frame(field_book) || !is.data.frame(simulation)) {
    fieldhub_abort("The field book and simulation must be data frames.")
  }
  if (!is.character(response_name) || length(response_name) != 1L ||
      is.na(response_name) || !nzchar(response_name)) {
    fieldhub_abort("The simulated response must have one non-empty name.")
  }
  if (response_name %in% names(field_book)) {
    fieldhub_abort(sprintf("The field book already has a '%s' column.",
                           response_name))
  }
  if (!response_name %in% names(simulation)) {
    fieldhub_abort(sprintf("The simulation has no '%s' response column.",
                           response_name))
  }
  if (nrow(field_book) != nrow(simulation)) {
    fieldhub_abort("The field book and simulation must have the same number of rows.")
  }
  field_book[[response_name]] <- round(simulation[[response_name]], digits)
  field_book
}

#' @importFrom stats rnorm sd
ZST <- function(n,m,RHOX,RHOY,s20) {
  N <- check_spatial_grid(n, m, RHOX, RHOY)
  if (N < 2L) fieldhub_abort("Spatial standardization needs at least two plots.")
  if (!is.numeric(s20) || is.complex(s20) || length(s20) != 1L ||
      !is.finite(s20) || s20 < 0 || s20 > 1) {
    fieldhub_abort("The spatial nugget fraction must be a finite number between zero and one.")
  }
  e1 <- rnorm(N)
  e2 <- rnorm(N)
  Y <- rep(as.numeric(seq_len(n)), each = m)
  X <- rep(as.numeric(seq_len(m)), times = n)
  PAT <- separable_ar1_patch(e1, n, m, RHOX, RHOY)
  PAT <- (PAT-mean(PAT))/sd(PAT); #Standarization of Patchess
  
  Z <- PAT
  
  ZST <- Z*sqrt(1-s20) + e2*sqrt(s20)
  out <- data.frame(list(ROW = Y, COLUMN = X, ZST = ZST))
  return(out)
}
