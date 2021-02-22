lsq <- function(len, reps = 1, seed = NA, returnstrings = FALSE) {
  
  if (!is.na(seed)) {
    if (exists(".Random.seed"))  { saved.seed <- .Random.seed }
    else                         { saved.seed <- NA }
    set.seed(seed)
  }
  
  allsq <- matrix(nrow = reps*len, ncol = len)
  
  if (returnstrings) { squareid <- vector(mode = "character", length = reps) }
  
  sample1 <- function(x) {
    if (length(x)==1) { return(x) }
    else              { return(sample(x,1)) }
  }
  
  for (n in 1:reps) {
    
    sq <- matrix(nrow=len, ncol=len) 
    
    while (any(is.na(sq))) {
      
      k <- sample1(which(is.na(sq)))
      
      i <- (k-1) %% len + 1       # Get the row num
      j <- floor((k-1) / len) + 1 # Get the col num
      
      sqrow <- sq[i,]
      sqcol <- sq[,j]
      
      openCell <- rbind(cbind(which(is.na(sqcol)), j),
                        cbind(i, which(is.na(sqrow))))
      # Randomize fill order
      openCell <- openCell[sample(nrow(openCell)),]
      
      # Put center cell at top of list, so that it gets filled first
      openCell <- rbind(c(i,j), openCell)
      
      openCell <- matrix(openCell[!duplicated(openCell),], ncol=2)
      
      for (c in 1:nrow(openCell)) {
        # The current cell to fill
        ci <- openCell[c,1]
        cj <- openCell[c,2]
        # Get the numbers that are unused in the "cross" centered on i,j
        freeNum <- which(!(1:len %in% c(sq[ci,], sq[,cj])))
        
        # Fill in this location on the square
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
  
  # Restore the old random seed, if present
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
  
  if (returnstrings) { return(squareid) }
  else               { return(ls4.random) }
  
}