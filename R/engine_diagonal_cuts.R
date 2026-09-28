automatically_cuts <- function(
    data = NULL, 
    planter_mov = "serpentine",
    stacked = "By Row", 
    dim_data = NULL) {
    if (is.null(data)) {
        fieldhub_abort("Internal error: the checks map is missing.",
                       class = "fieldhub_internal_error")
    }
    w_map <- data
    auto_cuts_by_r <- numeric()
    data_dim_each_block <- dim_data
    max_v <- length(data_dim_each_block)
    v <- 1;k <- 0
    if (stacked == "By Row") {
        # Walk the planting path and cut a block where its entries are complete
        path <- field_path(nrow(w_map), ncol(w_map), planter_mov)
        for (p in seq_len(nrow(path))) {
            if (w_map[path[p, , drop = FALSE]] == 0) k <- k + 1
            if (data_dim_each_block[v] == k) {
                auto_cuts_by_r[v] <- path[p, "row"];k <- 0
                if (v < max_v) {
                    v <- v + 1
                } else v <- 1
            }
        }
        
        x <- nrow(w_map):1
        y <- auto_cuts_by_r
        cuts <- x[y]
        bks  <- list()
        s <- 1
        for (h in 1:length(cuts)){
        bks[[h]] <- s:(cuts[h])
        s <- (cuts[h] + 1)
        }
        B <- sort(as.vector(unlist(bks)))
        if(nrow(w_map) != B[length(B)]) return(NULL)
        
        return(list(bks = bks, cuts = cuts))
        
    }else {
        auto_cuts_by_c <- numeric()
        v <- 1;k <- 0 
        for (j in 1:ncol(w_map)) {
        for (i in 1:nrow(w_map)) {
            if (w_map[i,j] == 0) k <- k + 1
            if (data_dim_each_block[v] == k) {
            auto_cuts_by_c[v] <- j;k <- 0
            if (v < max_v) {
                v <- v + 1
            }else v <- 1
            }
        }
        }
    }
    
    return(auto_cuts_by_c)
}
