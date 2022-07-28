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