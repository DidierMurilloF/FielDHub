concurrence_matrix <- function(df=NULL, trt=NULL, target=NULL) {
  if (is.null(df)) {
    stop('No input dataset provided.')
  }
  if (is.null(trt)) {
    stop('No input treatment factor provided.')
  }
  if (is.null(target)) {
    stop('No input target design factor provided.')
  }
  df[,target]<-as.factor(df[,target])
  df[,trt]<-as.factor(df[,trt])
  s <- length(levels(df[,target]))
  if (s==0) { stop('No levels found for design factor provided.') }
  inc <- as.matrix(table(df[,target],df[,trt]))
  for (i in 1:s) {
    inc[inc[,i]>0,i] <- 1
  }
  conc.matrix <- t(inc) %*% inc
  summ.rep <- diag(conc.matrix)
  diag(conc.matrix) <- rep(99999999,nrow(conc.matrix))
  summ<-as.data.frame(table(as.vector(conc.matrix)))
  summ$Freq <- summ$Freq/2  # Added
  colnames(summ) <- c('Concurrence',target)
  summ <- summ[-nrow(summ),]
  
  return(summ = summ)
}