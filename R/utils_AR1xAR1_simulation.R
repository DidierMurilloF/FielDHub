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