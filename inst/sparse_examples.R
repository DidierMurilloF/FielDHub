library(FielDHub)
################################# Sparse examples #################################
sparse <- sparse_allocation(
   checks_allocation = "diagonal",
   lines = 450, 
   l = 6, 
   plant_reps = 4, 
   checks = 4, 
   locationNames = c("LOC1", "LOC2", "LOC3", "LOC4", "LOC5"), 
   seed = 123
)

sparse_designs <- sparse$designs
head(sparse_designs$LOC1$fieldBook, 10)
plot(sparse_designs$LOC1)

################################# P-rep example #################################
optim_multi_prep <- multi_location_prep(
    lines = 240,  
    l = 6, 
    plant_reps = 9, 
    checks = 3, 
    rep_checks = c(6,6,6),
    locationNames = c("LOC1", "LOC2", "LOC3", "LOC4", "LOC5", "LOC6"), 
    seed = 2458
)

prep_designs <- optim_multi_prep$designs
head(prep_designs$LOC1$fieldBook, 10)
plot(prep_designs$LOC1)



