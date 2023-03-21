library(blocksdesign)
library(dplyr)
library(FielDHub)
################################# Sparse examples #################################
# Create a list of 6 locations with their respective entries and names
# for 450 treatments. We add four checks, these checks are mandatory for
# unreplicated designs.
sparse_list <- do_optim(
    design = "sparse",
    lines = 450, 
    l = 6, 
    plant_reps = 5,
    add_checks = TRUE, 
    checks = 4, 
    seed = 12
)
sparse_list$allocation
sparse_list$size_locations
sparse_list$list_locs

################################ P-rep examples #################################
# Create a list of 6 locations with their respective entries and names
# for 360 treatments. We add three checks 8 times each.
optim_list_with_checks <- do_optim(
    design = "prep",
    lines = 360, 
    l = 6, 
    plant_reps = 9,
    add_checks = TRUE, 
    checks = 3, 
    rep_checks = c(8,8,8),
    seed = 87
)
optim_list_with_checks$allocation
optim_list_with_checks$size_locations
optim_list_with_checks$list_locs
# Create a list of 6 locations with their respective entries and names
# for 360 treatments. NO Checks
optim_list_no_checks <- do_optim(
    design = "prep",
    lines = 360, 
    l = 6, 
    plant_reps = 9,
    add_checks = FALSE, 
    seed = 87
)
optim_list_no_checks$allocation
optim_list_no_checks$size_locations
optim_list_no_checks$list_locs
