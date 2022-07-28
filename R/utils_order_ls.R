order_ls <- function(S = NULL, data = NULL) {
  cindex <- ncol(S)
  rindex <- nrow(S)
  if (is.null(data)) {
    r <- paste("Row", 1:rindex, sep = " ")
    c <- paste("Column", 1:cindex, sep = " ")
  }else {
    r <- factor(data[,1], levels = as.character(unique(data[,1])))
    c <- factor(data[,2], levels = as.character(unique(data[,2])))
  }
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