validateTreatments <- function(data) {
  # Group the data by LOCATION, REP, and then TREATMENT, and count the occurrences
  treatment_counts <- aggregate(ID ~ LOCATION + REP + ENTRY, data=data, FUN=length)
  
  # Identify any treatment counts greater than 1, indicating duplicates within a LOCATION and REP
  duplicates <- treatment_counts[treatment_counts$ID > 1, ]
  
  if (nrow(duplicates) > 0) {
    stop("There are duplicates within REP for some LOCATIONs:\n")
  } else {
    # Check if all treatments are present exactly once within each REP for each LOCATION
    unique_treatments_per_location_rep <- aggregate(ENTRY ~ LOCATION + REP, data=data, function(x) length(unique(x)))
    expected_treatments_count <- unique(unique_treatments_per_location_rep$ENTRY)
    
    # if (length(expected_treatments_count) != 1) {
    #   stop("Not all treatments are present exactly once within each REP for each LOCATION. Here are the details:\n")
    # } 
  }
}
# 
# library(blocksdesign)
# # When producing a randomization for a resolvable row-column design
# # with 36 entries, 6 reps, 2 rows, and a random seed of 20243, each entry
# # does not appear in every replicate.
# # set.seed(20243)
# nt <- 36
# r <- 6
# b <- 2
# l <- 1
# k <- 2 #nrows
# nincblock <- nt*r/k
# N <- nt * r
# locationNames <- "FARGO"
# square <- FALSE
# if (sqrt(nt) == round(sqrt(nt))) square <- TRUE
# outIBD_loc <- vector(mode = "list", length = l)
# for (i in 1:l) {
#   if (square) {
#     mydes <- blocksdesign::blocks(treatments = nt, replicates = r + 1, blocks = list(r + 1, b), seed = NULL)
#     ##### Dropping the cyclical REP ######
#     rep_to_drop <- mydes$Design |>
#       dplyr::group_by(Level_1, Level_2) |>
#       dplyr::mutate(treatments = as.numeric(treatments)) |>
#       dplyr::summarise(dif = sum(diff(sort(treatments)))/(dplyr::n()-1)) |>
#       dplyr::filter(dif == 1) |>
#       dplyr::pull(Level_1) |>
#       unique()
#     print(rep_to_drop)
#     if (length(rep_to_drop) > 0) {
#       mydes$Design <- mydes$Design |>
#         dplyr::filter(Level_1 != rep_to_drop) |>
#         dplyr::mutate(Level_1 = rep(paste0("B", 1:r), each = nt))
#     } else {
#       mydes$Design <- mydes$Design |>
#         dplyr::filter(Level_1 != paste0("B", r + 1))
#     }
#   } else {
#     mydes <- blocksdesign::blocks(treatments = nt, replicates = r, blocks = list(r, b), seed = NULL)
#   }
#   # mydes <- blocksdesign::blocks(treatments = nt, replicates = r, blocks = list(r, b), seed = NULL)
#   ibd_plots <- list(1:216)
#   matdf <- base::data.frame(list(LOCATION = rep(locationNames[i], each = N)))
#   matdf$PLOT <- as.numeric(unlist(ibd_plots[[i]]))
#   matdf$BLOCK <- rep(c(1:r), each = nt)
#   matdf$iBLOCK <- rep(c(1:b), each = k)
#   matdf$UNIT <- rep(c(1:k), nincblock)
#   matdf$TREATMENT <- mydes$Design[,4]
#   colnames(matdf) <- c("LOCATION","PLOT", "REP", "IBLOCK", "UNIT", "ENTRY")
#   outIBD_loc[[i]] <- matdf
# }
# OutIBD <- dplyr::bind_rows(outIBD_loc)
# OutIBD <- as.data.frame(OutIBD)
# OutIBD$ENTRY <- as.numeric(OutIBD$ENTRY)
# OutIBD_test <- OutIBD
# OutIBD_test$ID <- 1:nrow(OutIBD_test)
# lookup <- FALSE
# if(lookup) {
#   OutIBD <- dplyr::inner_join(OutIBD, dataLookUp, by = "ENTRY")
#   OutIBD <- OutIBD[,-6]
#   colnames(OutIBD) <- c("LOCATION","PLOT", "REP", "IBLOCK", "UNIT", "TREATMENT")
#   OutIBD <- dplyr::inner_join(OutIBD, data_up, by = "TREATMENT")
#   OutIBD <- OutIBD[, c(1:5,7,6)]
#   colnames(OutIBD) <- c("LOCATION","PLOT", "REP", "IBLOCK", "UNIT", "ENTRY", "TREATMENT")
# }
# ID <- 1:nrow(OutIBD)
# OutIBD_new <- cbind(ID, OutIBD)
# 
# 
# # Load the data frame (replace this with your actual data loading code)
# data1 <- read.csv("OutIBD_before_merging.csv")
# View(data1)
# data <- OutIBD_test
# 
# # Example usage:
# validateTreatments(data)
# 
# 
# data1 <- read.csv("IBD_row_example.csv")
# View(data1)
# 
# data1 <- read.csv("IBD_row_example_before_drop_rep.csv")
# View(data1)




