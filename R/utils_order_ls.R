order_ls <- function(S = NULL) {
  cindex <- ncol(S)
  rindex <- nrow(S)
  r <- paste("Row", 1:rindex, sep = " ")
  c <- paste("Column", 1:cindex, sep = " ")
  rnames <- rownames(S)
  cnames <- colnames(S)
  rOrder <- vector(mode = "numeric")
  cOrder <- vector(mode = "numeric")
  for (i in r) {rOrder[i] <- which(rnames == i)}
  for (j in c) {cOrder[j] <- which(cnames == j)}
  rOrder <- as.numeric(rOrder)
  cOrder <- as.numeric(cOrder)
  new_s <- S[,cOrder]
  new_s <- new_s[rOrder,]
  return(new_s)
}