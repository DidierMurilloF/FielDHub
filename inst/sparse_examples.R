#######################
library(blocksdesign)
library(dplyr)
library(FielDHub)

optim_list <- do_optim(
    design = "prep",
    lines = 363, 
    l = 6, 
    plant_reps = 9, 
    checks = 3, 
    rep_checks = c(8,8,8),
    seed = 87
)

environments <- optim_list$list_locs
environments$LOC1
environments$LOC2
environments$LOC3
environments$LOC4
environments$LOC5
environments$LOC6
optim_list$allocation
optim_list$size_locations

sparse <- sparse_allocation(
   checks_allocation = "diagonal",
   lines = 450, 
   l = 6, 
   plant_reps = 4, 
   checks = 4, 
   locationNames = c("LOC1", "LOC2", "LOC3", "LOC4", "LOC5"), 
   seed = 1234
)

sparse$designs$LOC1
plot(sparse$designs$LOC1)


optim_multi_prep <- multi_location_prep(
    lines = 240,  
    l = 6, 
    plant_reps = 9, 
    checks = 3, 
    rep_checks = c(6,6,6),
    locationNames = c("LOC1", "LOC2", "LOC3", "LOC4", "LOC5", "LOC6"), 
    seed = 1234
)

designs <- optim_multi_prep$designs
plot(designs$LOC1)
field_book_loc_1 <- designs$LOC1$fieldBook
head(field_book_loc_1, 10)


