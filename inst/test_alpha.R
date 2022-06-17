alpha_test <- FielDHub::alpha_lattice(t = 12, k = 3, r = 3, l = 1, 
                                      plotNumber = 101, seed = 16)
data <- alpha_test$fieldBook
n_TrtGen <- dplyr::n_distinct(data$ENTRY)
n_Reps <- dplyr::n_distinct(data$REP)
sizeIblocks <- dplyr::n_distinct(data$UNIT)
iBlocks <- n_TrtGen/sizeIblocks


w_r <- 1:(iBlocks*n_Reps)
u_r <- seq(1, length(w_r), by = sizeIblocks)
v_r <- seq(sizeIblocks, length(w_r), by = sizeIblocks)

z_rows <- vector(mode = "list", length = n_Reps)
for (j in 1:iBlocks) {
  z_rows[[j]] <- rep(c(rep(u_r[j]:v_r[j], each = sizeIblocks)), times = 1)
}
z_rows <- unlist(z_rows)
z_rows

bookROWCol <- data %>% 
  dplyr::mutate(ROW = z_rows,
                COLUMN = rep(rep(1:sizeIblocks, times = iBlocks), n_Reps))

df0 <- alpha_test
df0 <- df0[order(df0$ROW, decreasing = FALSE), ]
nCols <- max(df0$COLUMN)
newPlots <- planter_transform(plots = plots, 
                              planter = planter, 
                              reps = n_Reps, 
                              cols = nCols, 
                              units = NULL)
df0$PLOT <- newPlots
books0[[1]] <- df0