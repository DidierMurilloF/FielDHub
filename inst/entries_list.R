library(magrittr)
library(FielDHub)
data_input <- read.csv("data/sparse_entries_list.csv")
data_input <- read.csv("data/sparse_entries_list_two.csv")

checks <- 4
lines <- 380
input_lines <- 380 + checks

optim_list <- do_optim(
    design = "sparse",
    lines = 380, 
    l = 5, 
    plant_reps = 4,
    add_checks = TRUE, 
    checks = 4, 
    seed = 87
)

max_entry <- lines
new_list_locs <- setNames(vector("list", length = 5), nm = paste0("LOC", 1:5))
for (LOC in 1:5) {
    iter_loc <- optim_list$list_locs[[LOC]]
    data_input_mutated <- data_input %>%
        dplyr::mutate(
            ENTRY_list = ENTRY,
            ENTRY = c((max_entry + 1):((max_entry + checks)), 1:lines)
        ) %>%
        dplyr::select(ENTRY_list, ENTRY, NAME) %>%
        dplyr::left_join(y = iter_loc, by = "ENTRY") %>%
        stats::na.omit() %>%
        dplyr::select(ENTRY_list, NAME.x) %>%
        dplyr::rename(ENTRY = ENTRY_list, NAME = NAME.x)
    new_list_locs[[LOC]] <- data_input_mutated
}
new_list_locs
    
nrow(new_list_locs[[1]])




