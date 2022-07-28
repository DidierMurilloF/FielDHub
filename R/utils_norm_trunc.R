#' @importFrom stats pnorm qnorm
norm_trunc <- function(a = NULL, b = NULL, data = NULL) {
  if (a == b) validate('Error: Truncation range values (a, b) is empty.')
  min <- a;max <- b
  Nc <- ncol(data)
  NameCol <- colnames(data)[Nc]
  trt.f <- factor(data[,Nc]) 
  nt <- length(levels(trt.f))
  reps <- max(table(trt.f))
  xbar <- (min+max)/2
  sigma <- diff(c(min,max))/3
  xbar_by_T <- seq(xbar - sigma, xbar + sigma, l = nt)
  sigma_by_T <- diff(xbar_by_T)[1]/reps
  if (reps > 3) {
    eps <- 0.13 * reps
  }else eps <- 0.3
  sigma_by_T <- sigma_by_T + eps
  N <- as.vector(table(trt.f))
  resp <- vector(mode = "list", length = nt)
  z <- 1
  for (i in xbar_by_T) {
    U <- runif(N[z], pnorm(min, i, sigma_by_T), pnorm(max, i, sigma_by_T))
    X <- round(qnorm(U, i, sigma_by_T),2)
    resp[[z]] <- X
    z <- z + 1
  }
  resp_v <- unlist(resp)
  trt.sample <- sample(levels(trt.f))
  if (NameCol == "TREATMENT") {
    TREATMENT <- rep(trt.sample, times = N)
    df <- data.frame(list(TREATMENT = TREATMENT, RESP = resp_v))
    df <- df[order(df$TREATMENT), ]
    NEW_EXPT <- data
    NEW_EXPT <- NEW_EXPT[order(NEW_EXPT$TREATMENT), ]
  }else if (NameCol == "ENTRY") {
    ENTRY <- rep(trt.sample, times = N)
    df <- data.frame(list(ENTRY = ENTRY, RESP = resp_v))
    df <- df[order(df$ENTRY), ]
    NEW_EXPT <- data
    NEW_EXPT <- NEW_EXPT[order(NEW_EXPT$ENTRY), ]
  }else if (NameCol == "TRT_COMB") {
    TRT_COMB <- rep(trt.sample, times = N)
    df <- data.frame(list(TRT_COMB = TRT_COMB, RESP = resp_v))
    df <- df[order(df$TRT_COMB), ]
    NEW_EXPT <- data
    NEW_EXPT <- NEW_EXPT[order(NEW_EXPT$TRT_COMB), ]
  }
  NEW_EXPT$RESP <- df$RESP
  NEW_EXPT$LOCATION <- factor(NEW_EXPT$LOCATION, unique(as.character(NEW_EXPT$LOCATION)))
  NEW_EXPT <- NEW_EXPT[order(NEW_EXPT$LOCATION, NEW_EXPT$PLOT),]
  return(NEW_EXPT)
}
