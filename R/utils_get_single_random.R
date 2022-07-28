get_single_random <- function(n_rows = NULL, 
                              n_cols = NULL, 
                              matrix_checks = NULL, 
                              checks = NULL,
                              data = NULL) {
  n_rows <- as.numeric(n_rows)
  n_cols <- as.numeric(n_cols)
  data_entries <- as.vector(data[,1])
  data_entries_no_checks <- data_entries[!(data_entries %in% checks)]
  len_entries_to_random <- length(data_entries_no_checks)
  len_spots_to_fill <- sum(matrix_checks == 0)
  if (len_entries_to_random != len_spots_to_fill) {
    stop("data entries do not fit to the plot availables!!")
  }
  rand_entries <- sample(data_entries_no_checks)
  matrix_checks[matrix_checks == 0] <- rand_entries
  treatments_random <- sum(data_entries_no_checks %in% matrix_checks)
  if (treatments_random == len_entries_to_random) {
    matrix_checks_random_entries <- matrix_checks
    # print("Randomization was successful. It passed all tests!")
    # print(c(treatments_random, len_entries_to_random))
  } else stop("Some entries are missing in the randomization!!")
  return(list(rand = matrix_checks_random_entries, 
              Entries = rand_entries, 
              Lines = len_entries_to_random))
}