#' @importFrom stats runif
AR1xAR1_simulation <- function(nrows = NULL, ncols = NULL, ROX = NULL, 
                               ROY = NULL, minValue = NULL, 
                               maxValue = NULL, fieldbook = NULL, 
                               trail = NULL, seed = NULL) {
  if (!is.null(seed)) set.seed(seed) else set.seed(runif(1))
  rag <- diff(c(minValue, maxValue))
  sigma <- rag*0.15
  Beta <- sum(minValue, maxValue)/2
  s20 <- 0.1;H2 <- 0.5
  iter <- 20
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
  newPlan <- newPlan[, c(3,1,2,4,5)]
  gen <- cbind(genet[,1], genet[,2])
  colnames(gen) <- c("ENTRY","genot")
  if (0 %in% newPlan$ENTRY) {
    z <- data.frame(list(ENTRY = 0, genot = NA))
    gen <- rbind(gen, z)
  }
  merged <- merge(newPlan, gen, by = "ENTRY")
  merged$genot <- sqrt(H2) * merged$genot
  merged$resp <- Beta + merged$genot + sqrt(1 - H2) * merged$ZST
  merged <- merged[, c(2,1,3:7)]
  outOrder <- merged[order(merged$ID),]
  colnames(outOrder)[7] <- trail
  outOrder$ROW <- as.factor(outOrder$ROW)
  outOrder$COLUMN <- as.factor(outOrder$COLUMN)
  label_trail <- paste(trail, ": ")
  new_outOrder <- outOrder %>%
    dplyr::mutate(text = paste0("Row: ", outOrder$ROW, "\n", 
                                "Col: ", outOrder$COLUMN, "\n", 
                                "Entry: ", outOrder$ENTRY, "\n", 
                                label_trail, round(outOrder[,7],2)))
  return(list(outOrder = new_outOrder))
}
#' @importFrom stats rnorm sd
ZST <- function(n,m,RHOX,RHOY,s20) {
  
  s2 <- 1
  N <- n*m
  V <- diag(N)
  
  e1 <- rnorm(N)
  e2 <- rnorm(N)
  
  Y <- matrix(0,N,1)
  X <- matrix(0,N,1)
  for (K in 1:(N)) {
    
    Y1 <- floor((K-1)/m)+1     
    X1 <- K-(Y1-1)*m         
    Y[K,1] <- Y1                
    X[K,1] <- X1
    
    if (K!=N) {
      for (L in (K+1):N) {
        Y2 <- floor((L-1)/m)+1         
        X2 <- L-(Y2-1)*m              
        V[K,L] <- s2*(RHOX^abs(X2-X1))*(RHOY^abs(Y2-Y1))
        V[L,K] <- V[K,L]              
      }
    }
  }
  L <- chol(V)
  PAT <- t(L)%*%e1
  PAT <- (PAT-mean(PAT))/sd(PAT); #Standarization of Patchess
  
  Z <- PAT
  
  ZST <- Z*sqrt(1-s20) + e2*sqrt(s20)
  out <- data.frame(list(ROW = Y, COLUMN = X, ZST = ZST))
  return(out)
}

#' @importFrom stats pnorm qnorm
norm_trunc <- function(a = NULL, b = NULL, data = NULL, seed = NULL) {
  set.seed(seed)
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
