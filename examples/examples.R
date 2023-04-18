# Examples
library(FielDHub)
## Multi location Prep Example 180 treatments ################################
entry_list <- read.csv("data/entry_list_180_trts.csv", header = TRUE)
optim_multi_prep <- multi_location_prep(
  lines = 180,  
  l = 4, 
  copies_per_entry = 6, 
  locationNames = c("LOC1", "LOC2", "LOC3", "LOC4"), 
  nrows = c(15, 10, 18, 15), # rows at each location
  ncols = c(18, 27, 15, 18), # cols at each location
  seed = 1234,
  data = entry_list
)
print(optim_multi_prep)
optim_multi_prep$size_locations
plot(optim_multi_prep)
plot(optim_multi_prep, l = 2)
plot(optim_multi_prep, l = 3)
plot(optim_multi_prep, l = 4)

## Multi location Prep Example 180 treatments With Checks #####################
entry_list_checks <- read.csv("data/entry_list_180_trts.csv", header = TRUE)
optim_multi_prep <- multi_location_prep(
  lines = 176,  
  l = 4,
  checks = 4,
  rep_checks = c(6,5,5,5),
  copies_per_entry = 6, 
  locationNames = c("LOC1", "LOC2", "LOC3", "LOC4"), 
  nrows = c(15, 19, 19, 19), # rows at each location
  ncols = c(19, 15, 15, 15), # cols at each location
  seed = 1234,
  data = entry_list_checks
)
print(optim_multi_prep)
optim_multi_prep$size_locations
plot(optim_multi_prep)
plot(optim_multi_prep, l = 2)
plot(optim_multi_prep, l = 3)
plot(optim_multi_prep, l = 4)

## Multi location Prep Example 180 treatments With Checks #####################
entry_list_checks2 <- read.csv("data/entry_list_176_trts_2.csv", header = TRUE)
optim_multi_prep <- multi_location_prep(
  lines = 176,  
  l = 4,
  checks = 4,
  rep_checks = c(6,5,5,5),
  copies_per_entry = 6, 
  locationNames = c("LOC1", "LOC2", "LOC3", "LOC4"), 
  nrows = c(15, 19, 19, 19), # rows at each location
  ncols = c(19, 15, 15, 15), # cols at each location
  seed = 1234,
  data = entry_list_checks2
)
print(optim_multi_prep)
optim_multi_prep$size_locations
plot(optim_multi_prep)
plot(optim_multi_prep, l = 2)
plot(optim_multi_prep, l = 3)
plot(optim_multi_prep, l = 4)

## Sparse Allocation Example 180 treatments ################################
library(FielDHub)
entry_list_sparse_176_1 <- read.csv("data/entry_list_176_trts_1.csv", header = TRUE)
sparse_example1 <- sparse_allocation(
  lines = 176,  
  l = 4,
  checks = 4,
  copies_per_entry = 3, 
  plotNumber = c(1001, 5001, 10001, 15001), 
  locationNames = c("LOC1", "LOC2", "LOC3", "LOC4"), 
  nrows = 11,
  ncols = 14,
  seed = 1234,
  data = entry_list_sparse_176_1
)
print(sparse_example1)

plot(sparse_example1)
plot(sparse_example1, l = 2)
plot(sparse_example1, l = 3)
plot(sparse_example1, l = 4)

## Sparse Allocation Example 180 treatments ################################
entry_list_sparse_176_2 <- read.csv("data/entry_list_176_trts_2.csv", header = TRUE)
sparse_example2 <- sparse_allocation(
  lines = 176,  
  l = 4,
  checks = 4,
  copies_per_entry = 3, 
  plotNumber = c(1001, 5001, 10001, 15001), 
  locationNames = c("LOC1", "LOC2", "LOC3", "LOC4"), 
  nrows = 11,
  ncols = 14,
  seed = 1234,
  data = entry_list_sparse_176_2
)
print(sparse_example2)

plot(sparse_example2)
plot(sparse_example2, l = 2)
plot(sparse_example2, l = 3)
plot(sparse_example2, l = 4)




############## Multi-Location p-rep Example (User Input Data) ################
# Entry list with 680 treatments
# Four environments
# Five copies of each treatment
# Passing user input data
# It takes a few minutes
entry_list_680 <- read.csv("data/entry_list_680.csv", header = TRUE)
multi_prep1 <- multi_location_prep(
  lines = 680,  
  l = 4, 
  copies_per_entry = 5, 
  plotNumber = c(1001, 5001, 10001, 15001), 
  locationNames = c("LOC1", "LOC2", "LOC3", "LOC4", "LOC5"), 
  seed = 78945,
  nrows = c(34,17,34,17),
  ncols = c(25,50,25,50),
  data = entry_list_680
)
# Print design info
print(multi_prep1)
# Plot design field layouts
plot(multi_prep1)
plot(multi_prep1, l = 2)
plot(multi_prep1, l = 3)
plot(multi_prep1, l = 4)

###################### Multi-Location p-rep Example #####################
# lines = 680 (680 treatments, breeding lines, etc)
# Four environments
# Five copies of each treatment
# The function generates the entry list internally
# Treatments are tagged from 1 to 680.
multi_prep2 <- multi_location_prep(
  lines = 680,  
  l = 4, 
  copies_per_entry = 5, 
  plotNumber = c(1001, 5001, 10001, 15001), 
  locationNames = c("LOC1", "LOC2", "LOC3", "LOC4", "LOC5"), 
  seed = 78945,
  nrows = c(34,17,34,17),
  ncols = c(25,50,25,50)
)
# Print design info
print(multi_prep2)
# Plot design field layouts
plot(multi_prep2)
plot(multi_prep2, l = 2)
plot(multi_prep2, l = 3)
plot(multi_prep2, l = 4)

################################# FIN #####################################
###########################################################################
## The following example shows how the merge works for sparse user input data
entry_list_sparse_176_2 <- read.csv("data/entry_list_176_trts_1.csv", header = TRUE)
entry_list_sparse_176_2 <- read.csv("data/entry_list_176_trts_2.csv", header = TRUE)
optim_out <- do_optim(
  design = "sparse", 
  lines = 176, 
  l = 4, 
  copies_per_entry = 3, 
  add_checks = TRUE, 
  checks = 4, 
  seed = 1, 
  data = entry_list_sparse_176_2
)
optim_out$list_locs[[1]]
optim_out$allocation
optim_out$size_locations
lines <- 176
input_checks = 4
add_checks <- TRUE
df_data_lines <- entry_list_sparse_176_2[(input_checks + 1):nrow(entry_list_sparse_176_2), ]
if (add_checks) {
  max_entry <- lines
  vlookUp_entry <- c((max_entry + 1):((max_entry + input_checks)), 1:lines)
} else vlookUp_entry <- 1:lines
vlookUp_entry
user_data_input <- entry_list_sparse_176_2
locs <- length(optim_out$list_locs)
size_location <- vector(mode = "numeric", length = locs)
merged_list_locs <- setNames(
  vector("list", length = locs), 
  nm = paste0("LOC", 1:locs)
)
locs_range <- 1:locs
LOC <- 1
for (LOC in locs_range) {
  iter_loc <- optim_out$list_locs[[LOC]]
  data_input_mutated <- user_data_input %>%
    dplyr::mutate(
      USER_ENTRY = ENTRY,
      ENTRY = vlookUp_entry
    ) %>%
    dplyr::select(USER_ENTRY, ENTRY, NAME) %>%
    dplyr::left_join(y = iter_loc, by = "ENTRY") %>%
    dplyr::filter(!is.na(NAME.y)) %>% 
    dplyr::select(USER_ENTRY, NAME.x) %>%
    dplyr::rename(ENTRY = USER_ENTRY, NAME = NAME.x)
 # Store the number of plots (It does not include checks)
    df_to_check <- data_input_mutated[(input_checks + 1):nrow(data_input_mutated), ]
    if (inherits(optim_out, "MultiPrep")) {
        size_location[LOC] <- sum(df_to_check$REPS)
    } else {
        size_location[LOC] <- nrow(df_to_check)
    }
}

if (!all(size_location == as.numeric(optim_out$size_locations))) {
  stop("After data merge, size of locations does not match!")
}
optim_out$list_locs <- merged_list_locs

## The following example shows how the merge works for multi prep user input data
# No checks 
entry_list_prep <- read.csv("~/Desktop/entry_list_180_trts.csv", header = TRUE)
optim_out <- do_optim(
  design = "prep", 
  lines = 180, 
  l = 4, 
  copies_per_entry = 5, 
  add_checks = FALSE, 
  seed = 1, 
  data = entry_list_prep
)
optim_out$list_locs[[1]]
optim_out$allocation
lines <- 180
input_checks = 0
add_checks <- FALSE
df_data_lines <- entry_list_prep[(input_checks + 1):nrow(entry_list_prep), ]
if (add_checks) {
  max_entry <- max(df_data_lines$ENTRY)
  vlookUp_entry <- c((max_entry + 1):((max_entry + input_checks)), 1:lines)
} else vlookUp_entry <- 1:lines

user_data_input <- entry_list_prep
locs <- length(optim_out$list_locs)
size_location <- vector(mode = "numeric", length = locs)
merged_list_locs <- setNames(
  vector("list", length = locs), 
  nm = paste0("LOC", 1:locs)
)
locs_range <- 1:locs
for (LOC in locs_range) {
  iter_loc <- optim_out$list_locs[[LOC]]
  data_input_mutated <- user_data_input %>%
    dplyr::mutate(
      ENTRY_list = ENTRY,
      ENTRY = vlookUp_entry
    ) %>%
    dplyr::select(ENTRY_list, ENTRY, NAME) %>%
    dplyr::left_join(y = iter_loc, by = "ENTRY") %>%
    dplyr::select(.data = ., ENTRY_list, NAME.x, REPS) %>%
    dplyr::arrange(dplyr::desc(REPS)) %>%
    dplyr::rename(ENTRY = ENTRY_list, NAME = NAME.x)
  size_location[LOC] <- nrow(data_input_mutated) - input_checks
  merged_list_locs[[LOC]] <- data_input_mutated
}
# if (!all(size_location == as.numeric(optim_out$size_locations))) {
#   stop("After data merge, size of locations does not match!")
# }
optim_out$list_locs <- merged_list_locs



