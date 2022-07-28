lsq <- function(len, reps = 1, seed = NA) {
  
  if (!is.na(seed)) {
    if (exists(".Random.seed"))  { saved.seed <- .Random.seed }
    else                         { saved.seed <- NA }
    set.seed(seed)
  }
  allsq <- matrix(nrow = reps*len, ncol = len)
  #if (returnstrings) { squareid <- vector(mode = "character", length = reps) }
  sample1 <- function(x) {
    if (length(x)==1) { return(x) }
    else              { return(sample(x,1)) }
  }
  for (n in 1:reps) {
    sq <- matrix(nrow=len, ncol=len) 
    while (any(is.na(sq))) {
      k <- sample1(which(is.na(sq)))
      i <- (k-1) %% len + 1       
      j <- floor((k-1) / len) + 1 
      sqrow <- sq[i,]
      sqcol <- sq[,j]
      openCell <- rbind(cbind(which(is.na(sqcol)), j),
                        cbind(i, which(is.na(sqrow))))
      openCell <- openCell[sample(nrow(openCell)),]
      openCell <- rbind(c(i,j), openCell)
      openCell <- matrix(openCell[!duplicated(openCell),], ncol=2)
      for (c in 1:nrow(openCell)) {
        ci <- openCell[c,1]
        cj <- openCell[c,2]
        freeNum <- which(!(1:len %in% c(sq[ci,], sq[,cj])))
        if (length(freeNum)>0) { sq[ci,cj] <- sample1(freeNum) }
        else {
          sq <- matrix(nrow=len, ncol=len)
          break;
        }
      }
    }
    allsqrows <- ((n-1)*len) + 1:len
    allsq[allsqrows,] <- sq
  }
  if (!is.na(seed) && !is.na(saved.seed)) { .Random.seed <- saved.seed }
  #put LETTERS
  ls4.random <- allsq
  z <- 1
  for (w in 1:len){
    ls4.random[ls4.random == w] <- LETTERS[z]
    z <- z + 1
  }
  colnames(ls4.random) <- paste(rep("Column", len), 1:len)
  rownames(ls4.random) <- paste(rep("Row", len), 1:len)
  
  return(ls4.random)
}