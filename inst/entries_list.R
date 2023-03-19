library(magrittr)
library(FielDHub)
data_input <- read.csv("data/sparse_entries_list.csv")
data_input <- read.csv("data/sparse_entries_list_two.csv")

checks <- 4
input_lines <- 380 + checks

optim_list <- do_optim(
    design = "sparse",
    lines = 380, 
    l = 6, 
    plant_reps = 5,
    add_checks = TRUE, 
    checks = 4, 
    seed = 87
)

Env1 <- optim_list$list_locs$LOC6

data_input_mutated <- data_input %>%
    dplyr::mutate(
        ENTRY_list = ENTRY,
        ENTRY = 1:input_lines
        ) %>%
    dplyr::select(ENTRY_list, ENTRY, NAME) %>%
    dplyr::left_join(y = Env1, by = "ENTRY") %>%
    stats::na.omit() %>%
    dplyr::select(ENTRY_list, NAME.x) %>%
    dplyr::rename(ENTRY = ENTRY_list, NAME = NAME.x)
    
data_input_mutated




