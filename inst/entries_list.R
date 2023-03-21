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

############### P-rep example with entries list ############################
library(FielDHub)
prep_data_input <- read.csv("data/sparse_entries_list_two.csv")

is.numeric(prep_data_input$ENTRY)

checks <- 4
lines <- 380
locs <- 6
input_lines <- 380 + checks

optim_list <- do_optim(
    design = "prep",
    lines = 380, 
    l = locs, 
    plant_reps = 9,
    add_checks = TRUE, 
    checks = checks,
    rep_checks = c(8,8,8,8),
    seed = 87,
    data = prep_data_input
)

optim_list$list_locs[[1]]

max_entry <- max(prep_data_input$ENTRY)
new_optim_list_locs <- setNames(vector("list", length = locs), nm = paste0("LOC", 1:locs))
for (LOC in 1:locs) {
    iter_loc <- optim_list$list_locs[[LOC]]

    data_input_mutated <- prep_data_input %>%
        dplyr::mutate(
            ENTRY_list = ENTRY,
            ENTRY = c((max_entry + 1):((max_entry + checks)), 1:lines)
        ) %>%
        dplyr::select(ENTRY_list, ENTRY, NAME) %>%
        dplyr::left_join(y = iter_loc, by = "ENTRY") %>%
        dplyr::select(ENTRY_list, NAME.x, REPS) %>%
        dplyr::arrange(dplyr::desc(REPS)) %>%
        dplyr::rename(ENTRY = ENTRY_list, NAME = NAME.x)
    new_optim_list_locs[[LOC]] <- data_input_mutated
}
new_optim_list_locs

#########P-rep example with entries list NO CHECKS############################
prep_data_input <- read.csv("data/prep_entries_list_NO_checks.csv")
prep_data_input
is.numeric(prep_data_input$ENTRY)

lines <- 380
checks <- 0
locs <- 6

optim_list <- do_optim(
    design = "prep",
    lines = lines, 
    l = locs, 
    plant_reps = 9,
    add_checks = FALSE,
    seed = 87,
    data = prep_data_input
)

optim_list$list_locs[[1]]

include_checks <- FALSE
vlookUp_entry <- 1:lines
if (include_checks) {
    vlookUp_entry <- c((max_entry + 1):((max_entry + checks)), 1:lines)
}
max_entry <- max(prep_data_input$ENTRY)
new_optim_list_locs <- setNames(vector("list", length = locs), nm = paste0("LOC", 1:locs))
for (LOC in 1:locs) {
    iter_loc <- optim_list$list_locs[[LOC]]

    data_input_mutated <- prep_data_input %>%
        dplyr::mutate(
            ENTRY_list = ENTRY,
            ENTRY = vlookUp_entry
        ) %>%
        dplyr::select(ENTRY_list, ENTRY, NAME) %>%
        dplyr::left_join(y = iter_loc, by = "ENTRY") %>%
        dplyr::select(ENTRY_list, NAME.x, REPS) %>%
        dplyr::arrange(dplyr::desc(REPS)) %>%
        dplyr::rename(ENTRY = ENTRY_list, NAME = NAME.x)
    new_optim_list_locs[[LOC]] <- data_input_mutated
}
new_optim_list_locs







