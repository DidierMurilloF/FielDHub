#' @title Partially Replicated Engine Function
#' 
#' @description
#' This function generates and optimizes a partially replicated (p-rep) experimental design 
#' for a given set of treatments and replication levels. The design is represented by 
#' a matrix and optimized using a pairwise distance metric. The function outputs 
#' various information about the optimized design including the field layout, replicated 
#' and unreplicated treatments, and pairwise distances between treatments.
#' 
#' @param nrows Number of rows field.
#' @param ncols Number of columns field.
#' @param Fillers An integer
#' @param seed An optional seed value to set the random number generator.
#' @param optim A logical value indicating whether or not to optimize the design. Default is TRUE.
#' @param niter The number of iterations to use in the first step optimization algorithm. 
#' Default is 1000.
#' @param data  Data frame with 3 columns: \code{ENTRY | NAME | REPS}.
#' 
#' @importFrom stats dist
#' 
#' @return A list containing the following elements:
#' \describe{
#'   \item{field.map}{The optimized matrix representing the experimental design layout.}
#'   \item{rows_incidence}{A vector of row indices.}
#'   \item{min_distance}{The minimum distance achieved during the optimization algorithm.}
#'   \item{pairwise_distance}{A data frame of pairwise distances between rep treatments.}
#'   \item{replicated_treatments}{A vector of the replicated treatments in the optimized design.}
#'   \item{unreplicated_treatments}{A vector of the unreplicated treatments in the optimized design.}
#'   \item{gen.entries}{A list of the entry treatments.}
#'   \item{gen.list}{The original input list of treatments and replication levels.}
#'   \item{reps.checks}{A vector of the number of times each replicated treatment appears in the design.}
#'   \item{entryChecks}{A vector of the rep entry treatments in the design.}
#'   \item{binary.field}{The binary matrix representing the experimental design before optimization.}
#' }
#' 
#' 
#' @noRd
pREP <- function(
    nrows = NULL, 
    ncols = NULL,
    Fillers = NULL, 
    seed = NULL, 
    optim = TRUE, 
    niter = 10000, 
    data = NULL) {
  
    niter <- 1000
    prep <- TRUE
    if (!is.null(data)) {
        gen_list <- data
        gen_list <- gen_list[, 1:3]
        gen_list <- na.omit(gen_list)
        colnames(gen_list) <- c("ENTRY", "NAME", "REPS")
        if (length(gen_list$ENTRY) != length(unique(gen_list$ENTRY))) {
            stop("Please ensure all ENTRIES in data are distinct.")
        }
        if (length(gen_list$NAME) != length(unique(gen_list$NAME))) {
            stop("Please ensure all NAMES in data are distinct.")
        }
        reps_one_time <- subset(gen_list, REPS == 1)
        if (nrow(reps_one_time) == 0) {
            prep <- FALSE
            optim <- FALSE
            gen_list_order <- gen_list[order(gen_list$REPS, decreasing = TRUE), ]
            reps_treatments <- as.vector(gen_list_order[,3])
            entry_treatments <- as.vector(gen_list_order[,1])
            total_plot_reps <- sum(gen_list_order$REPS)
            reps_checks <- reps_treatments
            treatments <- rep(entry_treatments, times = reps_treatments)
            freq_reps <- table(reps_treatments)
            nREPS <- as.vector(as.numeric((names(freq_reps))))
            total_checks <- sum(freq_reps * nREPS)
        } else {
            gen_list_order <- gen_list[order(gen_list$REPS, decreasing = TRUE), ]
            data_rep_treatments <- subset(gen_list_order, REPS > 1)
            total_plot_reps <- sum(data_rep_treatments$REPS)
            data_rep_treatments <- data_rep_treatments[order(data_rep_treatments$REPS, decreasing = TRUE), ]
            data_unrep_treatments <- subset(gen_list_order, REPS == 1)
            reps_checks <- as.vector(data_rep_treatments[, 3])
        } 
    }
    ###################### Some review on the data entry ##########################
    if (prep == TRUE) {
        freq_reps <- table(data_rep_treatments[,3])
        nREPS <- as.vector(as.numeric((names(freq_reps))))
        total_checks <- sum(freq_reps * nREPS)
        total_plots <- sum(gen_list$REPS)
        if (sum(total_plots) != (nrows * ncols)) {
            choices <- factor_subsets(n = total_plots)$labels
            if (!is.null(choices)) {
                message(cat("\n", "Error in partially_replicated(): ", "\n", "\n",
                            "Field dimensions do not fit with the data entered!", "\n",
                            "Try one of the following options: ", "\n"))
                return(for (i in 1:length(choices)) {print(choices[[i]])})
            } else {
                stop("Field dimensions do not fit with the data entered. Try another amount of treatments!", call. = FALSE)
            }
        }
    }
    ########## Init the p-rep data  ##############################################
    datos <- sample(c(rep(0, nrows * ncols - total_checks),
                      rep(1, total_checks)))
    ######### Building the binary Matrix #########################################
    field0 <- matrix(
      data = sample(datos), 
      nrow = nrows, 
      ncol = ncols, 
      byrow = FALSE
    )
    ################## Get optimized the design using a metric distance ##########
    if (optim) {
        m1 <- as.vector(field0)
        dist_field0 <- sum(dist(field0))
        designs <- vector(mode = "list", length = niter)
        dists <- vector(mode = "numeric", length = niter)
        designs[[1]] <- field0
        dists[1] <- dist_field0
        for(i in 2:niter) {
            m <- as.vector(designs[[i-1]])
            k1 <- which(m == 1);k2 <- which(m == 0)
            D <- vector(length = 2)
            D[1] <- sample(k1, 1, replace = FALSE)
            D[2] <- sample(k2, 1, replace = FALSE)
            m1 <- replace(m, D, m[rev(D)])
            iter_designs <- matrix(m1, nrow = nrows, ncol = ncols, byrow = FALSE)
            iter_dist <- sum(dist(iter_designs))
          if (iter_dist > dists[i - 1]) {
              designs[[i]] <- iter_designs
              dists[i] <- iter_dist 
          } else {
              designs[[i]] <- designs[[i - 1]]
              dists[i] <- dists[i-1]
          }
        }
        # Selecting the 'best' design
        field <- designs[[niter]]          
    } else field <- field0
    
    if (prep == TRUE) {
        entry_gens <- as.vector(data_unrep_treatments[,1])
        entry_checks <- as.vector(data_rep_treatments[,1])
        layout <- field
        ch <- nrow(data_rep_treatments)
        trt_reps <- paste(rep("CH", ch), 1:ch, sep = "")
        target_checks <- rep(trt_reps, times = reps_checks)
        layout[layout == 1] <- sample(target_checks)
        target_check_levels <- levels(factor(target_checks, unique(as.character(target_checks))))
        ########## Randomize checks to the letters ############################
        trt <- entry_checks
        trts_to_random <- trt  
        l <- 1
        layout1 <- layout
        for (i in target_check_levels) {
            layout1[layout1 == i] <- trts_to_random[l]
            l <- l + 1
        }
        binary_field <- layout1
        entries <- list(entry_checks = entry_checks, entry_gens = entry_gens)
        if (length(entry_gens) == 1) {
            layout1[layout1 == 0] <- as.vector(entry_gens)
        } else {
            layout1[layout1 == 0] <- sample(entry_gens)
        }
    } else {
        reps_treatments <- as.vector(gen_list_order[, 3])
        entry_treatments <- as.vector(gen_list_order[, 1])
        treatments <- rep(entry_treatments, times = reps_treatments)
        entry_checks <- entry_treatments
        binary_field <- field0
        entries <- list(entry_checks = entry_treatments, entry_gens = 0)
        layout1 <- field
        layout1[layout1 == 1] <- sample(treatments)
    }
    # Make numeric each element in the matrix layout1
    field_layout <- apply(layout1, c(1,2), as.numeric)
    ################### Optimization ##########################################
    # Perform an optimization by using the function swap_pairs()
    if (max(table(field_layout)) == 2) {
        swap <- swap_pairs(X = field_layout, starting_dist = 3, stop_iter = 50)
    } else {
        swap <- swap_pairs(X = field_layout, starting_dist = 2, stop_iter = 50)
    }
    optim_layout <- swap$optim_design
    dups <- table(as.vector(optim_layout))
    replicated_treatments <- as.numeric(rownames(dups)[dups > 1])
    treatments <- as.vector(optim_layout)
    rep_trts <- treatments[which(treatments %in% replicated_treatments)]
    # Check if the frequency of rep treatments is the same as the input data
    if (total_plot_reps != length(rep_trts)) {
      stop("In the final design, rep treatments does not match with imput data")
    }
    unreplicated_treatments <- as.numeric(rownames(dups)[dups == 1])
    min_distance <- swap$min_distance
    pairwise_distance <- swap$pairwise_distance
    rows_incidence <- swap$rows_incidence
    binary_field <- optim_layout
    binary_field[!binary_field %in% replicated_treatments] <- 0
    return(
        list(
            field.map = optim_layout,
            rows_incidence = rows_incidence, 
            min_distance = min_distance,
            pairwise_distance = pairwise_distance,
            replicated_treatments = replicated_treatments,
            unreplicated_treatments = unreplicated_treatments,
            gen.entries = entries, 
            gen.list = gen_list,
            reps.checks = reps_checks,
            entryChecks = entry_checks, 
            binary.field = binary_field
        )
    )
}
