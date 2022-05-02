#' @importFrom utils capture.output
diagonals_checks <- function(n_rows = NULL, n_cols = NULL, jump_by_cols = 5, jump_by_rows = 4,
                             name_file = "Car2020", checks = c(1,2,3,4), p_start = NULL){
  
  di <- jump_by_cols - n_rows
  
  if (n_rows < jump_by_cols){
    
    n_rows <- n_rows + di
    
  }
  w_map <- matrix(data = 0, ncol = n_cols, nrow = n_rows, byrow = F)
  
  breaks_points <- seq(1, (jump_by_cols + p_start)*jump_by_rows, jump_by_rows)
  breaks_points <- rev(breaks_points)
  min_breaks_points <- numeric(length = jump_by_cols)
  
  for (i in 1:jump_by_cols){
    min_breaks_points[i] <- min(seq(breaks_points[i], 1, by = -jump_by_cols))
  }
  
  star_break_points <- list()
  for (l in 1:length(min_breaks_points)) {
    star_break_points[[l]] <- seq(min_breaks_points[l], n_rows, by = jump_by_cols)
  }
  
  ################ some test ####################
  rem <- (n_cols) %% jump_by_cols
  divs <- (n_cols - rem) / jump_by_cols
  ###############################################
  
  for(i in 1:length(star_break_points)) {
    w_map[star_break_points[[i]], i] <- 1
  }
  
  if ( divs > 1){
    new_divs <- 1:divs
    for (k in 2:length(new_divs)) {
      w_map[, (jump_by_cols + 1):(new_divs[k]*jump_by_cols)] <- w_map[, 1:jump_by_cols]
    }
  }
  ######## Final cheking ###################################### 
  if (rem > 0){
    w_map[, (divs*jump_by_cols + 1):n_cols] <- w_map[, 1:rem]
  }
  ############################################################
  
  if (di > 0){
    w_map <- w_map[-((n_rows-di+1):n_rows),]
  }
  n_Checks <- length(which(w_map == 1))
  pots <- dim(w_map)[1] * dim(w_map)[2]
  a <- capture.output(print(paste("You have",  n_Checks,"(",round((n_Checks/pots)*100,2),
                                  "%) pots available for Check of the", pots)))
  b <- capture.output(print(c(jump_by_cols, jump_by_rows)))
  
  ## Get random the checks into the whole experiment.
  res <- n_Checks %% length(checks)
  divv <- (n_Checks - res) / length(checks)
  if (res == 0){
    s <- rep(checks, n_Checks/length(checks))
    random_checks <- sample(s)
  }else{
    v <- c(rep(checks,divv), sample(checks,res))
    random_checks <- sample(v)
  }
  
  w_map[w_map == 1] <- random_checks
  output_list <- list(a, w_map, table(random_checks), b)
  
  return(output_list)
}
