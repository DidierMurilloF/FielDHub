#' Merge user data with optimization output
#'
#' This function merges user data with optimization output to prepare input
#' data for randomization. It accepts the output from the optimization function
#' `do_optim()` and user data with entries and corresponding line names.
#' It returns a modified `optim_out` object containing the merged data.
#'
#' @param optim_out Output object from the optimization function `do_optim()`.
#' @param data A data frame containing entries and corresponding names.
#' @param lines Number of entries.
#' @param add_checks A boolean indicating whether to add checks to the input data.
#' @param checks An integer containing the number of checks.
#' @param rep_checks A numeric vector containing replicates for each checks.
#'
#' @return The modified `optim_out` object containing merged data.
#' @noRd
merge_user_data <- function(
    optim_out,
    data,
    lines,
    add_checks = FALSE,
    checks,
    rep_checks = NULL) {
    if (!is.null(data)) {
        data_entry <- data[, 1:2]
        data_entry <- na.omit(data_entry)
        colnames(data_entry) <- c("ENTRY", "NAME")
        if (length(data_entry$ENTRY) != length(unique(data_entry$ENTRY))) {
            fieldhub_abort("Please ensure all ENTRIES in data are distinct.")
        }
        if (length(data_entry$NAME) != length(unique(data_entry$NAME))) {
                fieldhub_abort("Please ensure all NAMES in data are distinct.")
        }
        if (add_checks) input_checks <- checks else input_checks <- 0
        if (!is.null(rep_checks)) {
            if (length(rep_checks) != input_checks) {
                fieldhub_abort("Length of checks does not match replications!")
            }
        }
        df_data_lines <- data_entry[(input_checks + 1):nrow(data_entry), ]
        entries_in_file <- nrow(df_data_lines)
        if (entries_in_file != lines) {
            fieldhub_abort("Input lines does not match number of lines in input data!")
        }
        if (add_checks) {
            max_entry <- lines
            vlookup_entry <- c((max_entry + 1):((max_entry + input_checks)), 1:lines)
        } else vlookup_entry <- 1:lines
        user_data_input <- data_entry
        locs <- length(optim_out$list_locs)
        size_location <- vector(mode = "numeric", length = locs)
        merged_list_locs <- setNames(
            vector("list", length = locs),
            nm = paste0("LOC", 1:locs)
        )
        locs_range <- 1:locs
        # Merge each optimized location into the user data input
        for (LOC in locs_range) {
            iter_loc <- optim_out$list_locs[[LOC]]
            data_input_mutated <- user_data_input |>
              dplyr::mutate(
                USER_ENTRY = ENTRY,
                ENTRY = vlookup_entry
              ) |>
              dplyr::select(USER_ENTRY, ENTRY, NAME) |>
              dplyr::left_join(y = iter_loc, by = "ENTRY")

            if (inherits(optim_out, "MultiPrep")) {
              data_input_mutated <- data_input_mutated |>
                dplyr::select(USER_ENTRY, NAME.x, REPS) |> # Just specify columns directly
                dplyr::arrange(dplyr::desc(REPS)) |> # Arrange rows
                dplyr::rename(ENTRY = USER_ENTRY, NAME = NAME.x) # Rename columns
            } else if (inherits(optim_out, "Sparse")) {
              data_input_mutated <- data_input_mutated |>
                dplyr::filter(!is.na(NAME.y)) |> # Filter rows
                dplyr::select(USER_ENTRY, NAME.x) |> # Select columns
                dplyr::rename(ENTRY = USER_ENTRY, NAME = NAME.x) # Rename columns
            }

            # Store the number of plots (It does not include checks)
            df_to_check <- data_input_mutated[(input_checks + 1):nrow(data_input_mutated), ]
            if (inherits(optim_out, "MultiPrep")) {
                size_location[LOC] <- sum(df_to_check$REPS)
            } else {
                size_location[LOC] <- nrow(df_to_check)
            }
            # Store the merged data
            merged_list_locs[[LOC]] <- data_input_mutated
        }
        # Check if the number of plots are the same after the data merge
        if (!all(size_location == as.numeric(optim_out$size_locations))) {
            fieldhub_abort("After data merge, size of locations does not match!")
        }
        optim_out$list_locs <- merged_list_locs
        return(optim_out)
    }
}

#' Give each smaller location one more copy of an entry
#'
#' @description Walks up the rows of the allocation table, from the last
#' entry, and gives each location that is smaller than the largest one an
#' extra copy of the first entry it holds \code{key_value} copies of.
#'
#' @param allocation Table or matrix with the copies of each entry (rows)
#'   in each location (columns).
#' @param key_value Number of copies an entry must have in a location to
#'   receive one more (0 for sparse, 1 for p-rep designs).
#' @param add_value Number of copies the entry gets instead.
#'
#' @return The allocation, with one more copy in each smaller location when
#'   possible.
#' @noRd
balance_allocation <- function(allocation, key_value, add_value) {
    size_locs <- as.vector(base::colSums(allocation))
    unbalanced_locs <- which(size_locs != max(size_locs))
    max_swaps <- length(unbalanced_locs)
    k <- nrow(allocation)
    init <- 1
    while (init <= max_swaps && k >= 1) {
        # Add an additional gen copy to the unbalanced locations
        add_gen <- as.vector(allocation[k, unbalanced_locs])
        if (length(which(add_gen == key_value)) > 0) {
            one_index <- which(add_gen == key_value)[1]
            add_gen[one_index] <- add_value
            allocation[k, unbalanced_locs] <- add_gen
            unbalanced_locs <- unbalanced_locs[-one_index]
            init <- init + 1
        }
        k <- k - 1
    }
    if (init <= max_swaps) {
        warning("The locations could not be balanced: no entry could be added to ",
                length(unbalanced_locs), " of them.", call. = FALSE)
    }
    allocation
}
