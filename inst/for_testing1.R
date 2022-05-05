lines <- 725
t1 <- floor(lines + lines * 0.090)
t2 <- ceiling(lines + lines * 0.20)
t <- t1:t2
n <- t[-numbers::isPrime(t)]
choices_list <- list()
i <- 1
for (n in t) {
  choices_list[[i]] <- FielDHub:::factor_subsets(n, diagonal = TRUE)$labels
  i <- i + 1
}
choices <- unlist(choices_list[!sapply(choices_list, is.null)])
if(is.null(choices)) {
  choices <- "No options available"
} 

length(choices)

field_dims <- function(n, opt) {
  total_entries <- as.numeric(getData()$dim_data_entry)
  lines <- total_entries - checks
  t1 <- floor(lines + lines * 0.090)
  t2 <- ceiling(lines + lines * 0.20)
  t <- t1:t2
  n <- t[-numbers::isPrime(t)]
  choices_list <- list()
  i <- 1
  for (n in t) {
    choices_list[[i]] <- FielDHub:::factor_subsets(n, diagonal = TRUE)$labels
    i <- i + 1
  }
  # dimensions.d <- FielDHub:::factor_subsets(n, diagonal = TRUE)$labels
  choices_list <- unlist(choices_list[!sapply(choices_list, is.null)])
  dims <- unlist(strsplit(unlist(choices_list[[opt]]), " x "))
  d_row <- as.numeric(dims[1])
  d_col <- as.numeric(dims[2])
  l = length(choices_list)
  return(list(d_row = d_row, d_col = d_col, l = l, choices_list = choices_list))
}

Option_NCD <- TRUE
way <- "By Column"
kindExpt <- "SUDC"
planter_mov <- "serpentine"
planter_mov1 <- "serpentine"
checksEntries <- 1:4
if(kindExpt == "DBUDC" && way == "By Column") {
  Option_NCD <- FALSE
}

if (kindExpt != "SUDC") {
  planter_mov <- planter_mov
}else planter_mov <- planter_mov1

n <- 725
owndataDIAGONALS <- "No"
checks <- 4
lines <- n
kindExpt <- "SUDC"
lines.d <- lines
sameEntries <- FALSE
opt = 1
obj <- field_dims(n = n, opt = opt)
l <- obj$l
l
choices_list <- obj$choices_list
choices_list
for (j in 1:l) {
  n_rows <- field_dims(n = n, opt = j)$d_row
  n_cols <- field_dims(n = n, opt = j)$d_col
  print(c(n_rows, n_cols))
  print(j)
  
  # getData()
  
  dt_info <- available_percent(n_rows = n_rows, n_cols = n_cols, checks = checksEntries, 
                               Option_NCD = Option_NCD, Visual_ch = NULL, visualCheck = FALSE, 
                               kindExpt = kindExpt, myWay = way, planter_mov1 = planter_mov, 
                               data = getData()$data_entry, dim_data = getData()$dim_data_entry,
                               dim_data_1 = getData()$dim_data_1, Block_Fillers = blocks_length())
  print(dt_info$dt)
}


dt_info$dt





# req(n_rows, n_cols)
n_rows <- 79; n_cols <- 10
if(n_rows < 5 || n_cols < 5) return(NULL)
n_rows <- n_rows; n_cols = n_cols
checks <- checks
dim_expt <- n_rows * n_cols

P <- matrix(c(rep(NA,10),c(1:10), rep(c(14:8,7,6,5),1),
              c(3,3,7,7,3,2,5,2,7,2)), ncol = 4, byrow = F)

W <- matrix(c(rep(0,50),c(1:50), rep(c(14:8,7,6,5),5),
              rep(c(3,3,7,7,3,2,5,2,7,2),5), rep(c(0,1,2,3,4),each = 10), rep(NA,50)),
            ncol = 6, byrow = F)

w_map_list <- list()
for (l in 1:nrow(W)) {
  if (n_cols < W[l,3]) next
  p_start <-  W[l,5]
  w_map_checks <- diagonals_checks(n_rows = n_rows, n_cols = n_cols, jump_by_cols = W[l,3],
                                   jump_by_rows = W[l,4], checks = c(1,1,1,1), p_start = p_start)
  w_map <- w_map_checks[[2]]
  W[l,6] <- sum(w_map != 0)
  w_map_list[[l]] <- w_map
}
# print("we are here")
w_test <- w_map_list[[50]]
sum(w_test == 1)/868







