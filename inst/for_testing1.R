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


## Single test for diagonal
Option_NCD <- TRUE
n <- 80
until <- 200
N <- until
kindExpt <- "SUDC"
planter_mov <- "serpentine"
planter_mov1 <- "serpentine"
checksEntries <- 1:4
if (kindExpt != "SUDC") {
  planter_mov <- planter_mov
}else planter_mov <- planter_mov1
owndataDIAGONALS <- "No"
checks <- 4
kindExpt <- "SUDC"
sameEntries <- FALSE
lines.d <- n
getData()
R <- n:N
for (lines.d in R) {
  lines.d <- lines.d
  opt = 1
  obj <- field_dims(n = lines.d, opt = opt)
  l <- obj$l
  getData()
  for (j in 1:l) {
    n_rows <- field_dims(n = lines.d, opt = j)$d_row
    n_cols <- field_dims(n = lines.d, opt = j)$d_col
    dt_info <- FielDHub:::available_percent(n_rows = n_rows, 
                                            n_cols = n_cols, 
                                            checks = checksEntries, 
                                            Option_NCD = Option_NCD, 
                                            kindExpt = kindExpt,
                                            planter_mov1 = planter_mov, 
                                            data = getData()$data_entry,
                                            dim_data = getData()$dim_data_entry,
                                            dim_data_1 = getData()$dim_data_1, 
                                            Block_Fillers = blocks_length())
    if (!is.null(dt_info$dt)) {
      percentege <- as.vector(dt_info$dt[,2])
      for (percent in percentege) {
        random_checks_opt <- FielDHub:::random_checks(
          dt = dt_info$dt, 
          d_checks = dt_info$d_checks, 
          p = dt_info$P, 
          percent = percent, 
          kindExpt = kindExpt, 
          planter_mov = planter_mov, 
          Checks = checksEntries,
          stacked = stacked, 
          data = getData()$data_entry, 
          data_dim_each_block = dt_info$data_dim_each_block,
          seed = NULL)$map_checks
        data_random <- FielDHub:::get_single_random(
          n_rows = n_rows, 
          n_cols = n_cols, 
          matrix_checks = random_checks_opt, 
          checks = checksEntries, 
          data = getData()$data_entry
          )
      }
    }
  }
}






######################################################
#####################################################
break_number <- function(n) {
  count <- 0
  options_up <- list()
  v <- 1
  for (i in 1:n) {
    for (j in 1:n) {
      for (k in 1:n) {
        if (i + j + k == n) {
          if (all(c(i, j, k) >= ceiling(n*0.30))) {
            options_up[[v]] <- c(i, j, k)
            v <- v + 1
          }
          count <- count + 1
        }
      }
    }
  }
  one_sample <- sample(options_up, size = 1)
  return(list(possible_combinations = count, 
              options_up = options_up, 
              one_sample = one_sample))
}


## Test for multiple expts in diagonals
n <- 80
until <- 1000
N <- until
stacked <- "By Row"
kindExpt <- "DBUDC"
planter_mov <- "serpentine"
planter_mov1 <- "serpentine"
checksEntries <- 1:4
if (kindExpt != "SUDC") {
  planter_mov <- planter_mov
}else planter_mov <- planter_mov1
owndataDIAGONALS <- "No"
checks <- 4
sameEntries <- FALSE
blocks.db <- break_number(n = n)$one_sample
blocks.db <- unlist(blocks.db)
print(blocks.db)
lines.db <- n
getData()
R <- n:N
for (lines.d in R) {
  blocks.db <- break_number(n = lines.d)$one_sample
  blocks.db <- unlist(blocks.db)
  print(blocks.db)
  lines.db <- lines.d
  opt = 1
  obj <- field_dims(n = lines.d, opt = opt)
  l <- obj$l
  getData()
  for (j in 1:l) {
    Option_NCD <- TRUE
    n_rows <- field_dims(n = lines.d, opt = j)$d_row
    n_cols <- field_dims(n = lines.d, opt = j)$d_col
    dt_info <- FielDHub:::available_percent(n_rows = n_rows, 
                                            n_cols = n_cols, 
                                            checks = checksEntries, 
                                            Option_NCD = Option_NCD, 
                                            kindExpt = kindExpt,
                                            stacked = stacked,
                                            planter_mov1 = planter_mov, 
                                            data = getData()$data_entry,
                                            dim_data = getData()$dim_data_entry,
                                            dim_data_1 = getData()$dim_data_1, 
                                            Block_Fillers = blocks_length())
    if (!is.null(dt_info$dt)) {
      percentege <- as.vector(dt_info$dt[,2])
      for (percent in percentege) {
        random_checks_opt <- FielDHub:::random_checks(
          dt = dt_info$dt, 
          d_checks = dt_info$d_checks, 
          p = dt_info$P, 
          percent = percent, 
          kindExpt = kindExpt, 
          planter_mov = planter_mov, 
          Checks = checksEntries,
          stacked = stacked, 
          data = getData()$data_entry, 
          data_dim_each_block = dt_info$data_dim_each_block,
          seed = NULL)$map_checks

        data_dim_each_block <- dt_info$data_dim_each_block
        my_row_sets <- FielDHub:::automatically_cuts(
          data = random_checks_opt,
          planter_mov = planter_mov,
          way = "By Row",
          dim_data = data_dim_each_block)[[1]]

        if(is.null(my_row_sets)) return(NULL)
        n_blocks <- length(my_row_sets)

        if ("Filler" %in% random_checks_opt) Option_NCD <- TRUE else Option_NCD <- FALSE
        data_entry <- getData()$data_entry
        if(kindExpt == "DBUDC" && Option_NCD == FALSE) {
          data_entry1 <- data_entry[(checks + 1):nrow(data_entry), ]
          data_random <- FielDHub:::get_DBrandom(
            binaryMap = random_checks_opt,
            data_dim_each_block = dt_info$data_dim_each_block,
            data_entries = data_entry1,
            planter = planter_mov
            )
        }else if(kindExpt == "DBUDC" && Option_NCD == TRUE) {
          Block_Fillers <- as.numeric(blocks_length())
          data_random <- FielDHub:::get_random(
            n_rows = n_rows,
            n_cols = n_cols,
            d_checks = random_checks_opt,
            Fillers = FALSE,
            row_sets = my_row_sets,
            checks = checksEntries,
            data = data_entry,
            planter_mov  = planter_mov,
            Multi.Fillers = TRUE,
            which.blocks = Block_Fillers
            )
        }
       }
    }
  }
}




###################################################
###################################################
###################################################
## Test for multiple expts in diagonals
n <- 80
until <- 200
N <- until
stacked <- "By Column"
kindExpt <- "DBUDC"
planter_mov <- "serpentine"
planter_mov1 <- "serpentine"
checksEntries <- 1:4
if (kindExpt != "SUDC") {
  planter_mov <- planter_mov
}else planter_mov <- planter_mov1
owndataDIAGONALS <- "No"
checks <- 4
sameEntries <- FALSE
blocks.db <- break_number(n = n)$one_sample
blocks.db <- unlist(blocks.db)
lines.db <- n
getData()
R <- n:N
for (lines.d in R) {
  blocks.db <- break_number(n = lines.d)$one_sample
  blocks.db <- unlist(blocks.db)
  print(blocks.db)
  lines.db <- lines.d
  opt = 1
  obj <- field_dims(n = lines.d, opt = opt)
  l <- obj$l
  getData()
  for (j in 1:l) {
    Option_NCD <- TRUE
    n_rows <- field_dims(n = lines.d, opt = j)$d_row
    n_cols <- field_dims(n = lines.d, opt = j)$d_col
    dt_info <- FielDHub:::available_percent(n_rows = n_rows, 
                                            n_cols = n_cols, 
                                            checks = checksEntries, 
                                            Option_NCD = Option_NCD, 
                                            kindExpt = kindExpt,
                                            stacked = stacked,
                                            planter_mov1 = planter_mov, 
                                            data = getData()$data_entry,
                                            dim_data = getData()$dim_data_entry,
                                            dim_data_1 = getData()$dim_data_1, 
                                            Block_Fillers = blocks_length())
    if (!is.null(dt_info$dt)) {
      percentege <- as.vector(dt_info$dt[,2])
      for (percent in percentege) {
        random_checks_opt <- FielDHub:::random_checks(
          dt = dt_info$dt, 
          d_checks = dt_info$d_checks, 
          p = dt_info$P, 
          percent = percent, 
          kindExpt = kindExpt, 
          planter_mov = planter_mov, 
          Checks = checksEntries,
          stacked = stacked, 
          data = getData()$data_entry, 
          data_dim_each_block = dt_info$data_dim_each_block,
          seed = NULL)$map_checks
        
          data_dim_each_block <- dt_info$data_dim_each_block
          data_random <- FielDHub:::get_random_stacked(
            stacked = "By Column", 
            n_rows = n_rows,
            n_cols = n_cols,
            matrix_checks = random_checks_opt,
            Fillers = FALSE,
            checks = checksEntries,
            data = getData()$data_entry,
            data_dim_each_block = data_dim_each_block
          )
      }
    }
  }
}


