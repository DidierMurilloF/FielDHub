#' @title  Generate the sparse or p-rep allocation to multiple locations.
#' @param design Type of experimental design. It can be \code{prep} or \code{sparse}
#' @param lines Number of genotypes, experimental lines or treatments.
#' @param l Number of locations or sites. By default  \code{l = 1}.
#' @param copies_per_entry Number of copies per plant.
#' When design is \code{sparse} then \code{copies_per_entry} should be less than \code{l}
#' @param add_checks Option to add checks. Optional if \code{design = "prep"}
#' @param checks Number of genotypes checks.
#' @param rep_checks Replication for each check.
#' @param force_balance Get balanced unbalanced locations. By default \code{force_balance = TRUE}.
#' @param data (optional) Data frame with 2 columns: \code{ENTRY | NAME }. ENTRY must be numeric.
#' @param seed (optional) Real number that specifies the starting seed to obtain reproducible designs.
#'
#' @author Didier Murillo [aut],
#'         Salvador Gezan [aut],
#'         Ana Heilman [ctb]
#'
#' @return A list with five elements, retaining class \code{Sparse} or
#'   \code{MultiPrep} for compatibility.
#' \itemize{
#'   \item \code{multi_location_data} is a data frame with the entries of every
#'     location: \code{LOCATION | ENTRY | NAME}, with a \code{REPS} column
#'     for p-rep allocations.
#'   \item \code{list_locs} is a list with each location list of entries.
#'   \item \code{allocation} is a data frame of test-entry copy counts, with
#'     one column per location.
#'   \item \code{size_locations} is a named vector of test-entry copies per
#'     location, excluding checks.
#'   \item \code{metadata} records the allocation type, schema version, seed,
#'     random-number settings, package version and evaluated input parameters.
#'     With the same package versions and RNG settings, rebuild the allocation
#'     with \code{do.call(do_optim, x$metadata$parameters)}.
#' }
#'
#' @references
#' Edmondson, R.N. Multi-level Block Designs for Comparative Experiments. JABES 25,
#' 500–522 (2020). https://doi.org/10.1007/s13253-020-00416-0
#'
#' @examples
#' sparse_example <- do_optim(
#'    design = "sparse",
#'    lines = 120,
#'    l = 4,
#'    copies_per_entry = 3,
#'    add_checks = TRUE,
#'    checks = 4,
#'    seed = 15
#' )
#' @export
do_optim <- function(
    design = "sparse",
    lines,
    l,
    copies_per_entry,
    add_checks = FALSE,
    checks = NULL,
    rep_checks = NULL,
    force_balance = TRUE,
    seed,
    data = NULL) {
    validate_locations(l)
    validate_flag(add_checks, "add_checks")
    validate_flag(force_balance, "force_balance")
    # set a random seed if it is missing
    if (missing(seed)) seed <- NULL
    seed <- resolve_seed(seed, default = function() sample.int(10000, size = 1))
    local_design_seed(seed)
    if (missing(lines)) fieldhub_abort("Please, define the number of lines/treatments for this design.")
    if (missing(l)) fieldhub_abort("Please, define the number of locations for this design.")
    if (missing(design) || is.null(design)) fieldhub_abort("Paramenter design is missing.")
    if (all(c("prep", "sparse") != design)) {
        fieldhub_abort("Input design is unknown. Please, choose one: 'sparse' or 'prep'.")
    }
    if (design == "prep" && copies_per_entry <= l) {
        fieldhub_abort("p-reps option requires that copies_per_entry be greater than the number of locations")
    }
    if (design == "sparse") {
        if (is.null(checks)) fieldhub_abort("Please, specify the number of checks!")
    }
    if (design == "prep") {
        if (add_checks == TRUE & is.null(checks) & is.null(rep_checks)) {
            fieldhub_abort("Please, specify the number of checks!")
        }
    }
    max_entry <- lines
    if (!is.null(data)) {
        if (design == "sparse") {
            data_input <- data[, 1:2]
            data_input <- stats::na.omit(data_input)
            colnames(data_input) <- c("ENTRY", "NAME")
            if (length(data_input$ENTRY) != length(unique(data_input$ENTRY))) {
              fieldhub_abort("Please ensure all ENTRIES in data are distinct.")
            }
            if (length(data_input$NAME) != length(unique(data_input$NAME))) {
              fieldhub_abort("Please ensure all NAMES in data are distinct.")
            }
            df_data_checks <- data_input[1:checks, ]
            df_data_lines <- data_input[(checks + 1):nrow(data_input), ]
            ENTRY <- as.vector(df_data_lines$ENTRY)
            if (!is.numeric(ENTRY)) fieldhub_abort("ENTRY column should have integer numbers!")
            # max_entry <- max(ENTRY)
            max_entry <- lines
            if (nrow(df_data_lines) != lines) {
                fieldhub_abort("The number of treatments/lines in the data does not match the input value")
            }
        } else {
            if (add_checks == TRUE) {
                data_input <- data[, 1:2]
                data_input <- stats::na.omit(data_input)
                colnames(data_input) <- c("ENTRY", "NAME")
                if (length(data_input$ENTRY) != length(unique(data_input$ENTRY))) {
                  fieldhub_abort("Please ensure all ENTRIES in data are distinct.")
                }
                if (length(data_input$NAME) != length(unique(data_input$NAME))) {
                  fieldhub_abort("Please ensure all NAMES in data are distinct.")
                }
                df_data_checks <- data_input[1:checks, ]
                df_data_lines <- data_input[(checks + 1):nrow(data_input), ]
                ENTRY <- as.vector(df_data_lines$ENTRY)
                if (!is.numeric(ENTRY)) fieldhub_abort("ENTRY column should have integer numbers!")
                # max_entry <- max(ENTRY)
                max_entry <- lines
                if (nrow(df_data_lines) != lines) {
                  fieldhub_abort("The number of treatments/lines in the data does not match the input value")
                }
            } else {
                data_input <- data[, 1:2]
                data_input <- stats::na.omit(data_input)
                df_data_lines <- data_input
                colnames(df_data_lines) <- c("ENTRY", "NAME")
                if (length(df_data_lines$ENTRY) != length(unique(df_data_lines$ENTRY))) {
                    fieldhub_abort("Please ensure all ENTRIES in data are distinct.")
                }
                if (length(df_data_lines$NAME) != length(unique(df_data_lines$NAME))) {
                    fieldhub_abort("Please ensure all NAMES in data are distinct.")
                }
                ENTRY <- as.vector(df_data_lines$ENTRY)
                if (!is.numeric(ENTRY)) fieldhub_abort("ENTRY column should have integer numbers!")
                #max_entry <- max(ENTRY)
                max_entry <- lines
                if (nrow(df_data_lines) != lines) {
                    fieldhub_abort("The number of treatments/lines in the data does not match the input value")
                }
            }
        }
    }
    # Generate the optim IBs
    local_optimizer_options()
    optim_blocks <- blocksdesign::blocks(
        treatments = lines,
        replicates = copies_per_entry,
        blocks = l,
        searches = 20,
        seed = seed
    )
    # Create allocation table
    allocation <- table(optim_blocks$Design$treatments, optim_blocks$Design$Level_1)
    key_value <- 0
    add_value <- 1
    if (design == "prep") {
        key_value <- 1
        add_value <- 2
    }
    # Check if there are unbalanced locations and force them to be balanced
    size_locs <- as.vector(base::colSums(allocation))
    max_size_locs <- max(size_locs)
    if (!all(size_locs == max_size_locs) & force_balance == TRUE) {
        allocation <- balance_allocation(allocation, key_value, add_value)
    }
    allocation_df <- as.data.frame.matrix(allocation)
    colnames(allocation_df) <- paste0("LOC", 1:l)
    # Create a wide data frame with number of copies and average per plant
    col_sum <- base::colSums(allocation_df)
    wide_allocation <- allocation_df |>
        dplyr::mutate(
            copies = rowSums(dplyr::across(dplyr::everything())),
            avg = copies / l
        )
    # Create a long data frame with the allocations per location
    long_allocation <- as.data.frame(allocation) |>
        dplyr::rename_with(~c("ENTRY", "LOCATION", "REPS"), dplyr::everything()) |>  # rename columns
        dplyr::mutate(
            LOCATION = gsub("B", "LOC", LOCATION),
            NAME = paste0("G-", ENTRY)
        ) |>
        dplyr::select(LOCATION, ENTRY, NAME, REPS)
    # Create a data frame for the checks
    if (design != "prep") {
        if (!add_checks) fieldhub_abort("Un-replicated designs need checks")
        if (!is.null(checks) & checks > 0) {
            df_checks <- data.frame(
                ENTRY = (max_entry + 1):((max_entry + checks)),
                NAME = paste0("CH-", (max_entry + 1):((max_entry + checks)))
            )
        }
    } else {
        if (add_checks == TRUE & !is.null(checks) & !is.null(rep_checks)) {
            if (length(rep_checks) != checks) {
                fieldhub_abort("Length of rep_checks does not match with number of checks")
            }
            df_checks <- data.frame(
                ENTRY = (max_entry + 1):((max_entry + checks)),
                NAME = paste0("CH-", (max_entry + 1):((max_entry + checks))),
                REPS = rep_checks
            )
        } else df_checks <- NULL
    }
    # Create a space in memory for the locations data entry list
    list_locs <- setNames(
        object = vector(mode = "list", length = l),
        nm = unique(long_allocation$LOCATION)
    )
    # Generate the lists of entries for each location
    for (site in unique(long_allocation$LOCATION)) {
        df_loc <- long_allocation |>
            dplyr::filter(LOCATION == site, REPS > 0) |>
            dplyr::mutate(ENTRY = as.numeric(ENTRY)) |>
            dplyr::select(ENTRY, NAME, REPS) |>
            dplyr::bind_rows(df_checks) |>
            dplyr::arrange(dplyr::desc(ENTRY))

        if (design == "prep") {
            df_loc <- df_loc |>
                dplyr::arrange(dplyr::desc(REPS))
        }

        if (design != "prep") {
            df_loc <- df_loc |>
                dplyr::select(ENTRY, NAME)
        }

        list_locs[[site]] <- df_loc
    }
    if (design == "prep") {
        # Combine the data frames into a single data frame with a new column for the list element name
        multi_location_data <- dplyr::bind_rows(lapply(names(list_locs), function(name) {
            dplyr::mutate(list_locs[[name]], LOCATION = name)
        })) |>
            dplyr::select(LOCATION, ENTRY, NAME, REPS)
    } else {
         # Combine the data frames into a single data frame with a new column for the list element name
        multi_location_data <- dplyr::bind_rows(lapply(names(list_locs), function(name) {
            dplyr::mutate(list_locs[[name]], LOCATION = name)
        })) |>
            dplyr::select(LOCATION, ENTRY, NAME)
    }
    # out object with the allocation and the list of entries per location
    out <- list(
        multi_location_data = multi_location_data,
        list_locs = list_locs,
        allocation = allocation_df,
        size_locations = col_sum
    )
    out <- new_fieldhub_allocation(out, parameters = list(
        design = design, lines = lines, l = l, copies_per_entry = copies_per_entry,
        add_checks = add_checks, checks = checks, rep_checks = rep_checks,
        force_balance = force_balance, seed = seed, data = data
    ))
    return(out)
}
