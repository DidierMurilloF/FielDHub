test_that("result schemas require each design's field-book extensions", {
  for (name in names(catalogue)) {
    x <- catalogue_design(name)
    if (!is.list(x) || !is.data.frame(x$fieldBook)) next
    for (column in setdiff(names(x$fieldBook), c("ID", "LOCATION", "PLOT"))) {
      bad <- x
      bad$fieldBook[[column]] <- NULL
      expect_error(validate_fieldhub_design(bad), column,
                   class = "fieldhub_internal_error", info = paste(name, column))
    }
  }
})

test_that("required extension columns are ordinary vectors without invalid values", {
  for (name in c("RCBD_two_locations", "latin_square", "full_factorial_rcbd",
                 "diagonal_single", "partially_replicated")) {
    x <- catalogue_design(name)
    columns <- setdiff(names(x$fieldBook), c("ID", "LOCATION", "PLOT", "CHECKS"))
    for (column in columns) {
      for (value in list(rep(list(1), nrow(x$fieldBook)),
                         matrix(1, nrow(x$fieldBook), 1L),
                         rep(NA_character_, nrow(x$fieldBook)))) {
        bad <- x
        bad$fieldBook[[column]] <- value
        expect_error(validate_fieldhub_design(bad), column,
                     class = "fieldhub_internal_error", info = paste(name, column))
      }
    }
  }
})

test_that("family-split results validate their entry tables and location totals", {
  x <- split_families(3, data.frame(ENTRY = 1:9, NAME = paste0("G", 1:9), FAMILY = "A"), seed = 38)
  for (name in c("rowsEachlist", "data_locations")) {
    bad <- x
    bad[[name]] <- NULL
    expect_error(validate_fieldhub_design(bad), class = "fieldhub_internal_error")
  }
  for (column in names(x$data_locations)) {
    bad <- x
    bad$data_locations[[column]] <- NULL
    expect_error(validate_fieldhub_design(bad), class = "fieldhub_internal_error")
  }
  bad <- x
  bad$rowsEachlist$n[1] <- bad$rowsEachlist$n[1] + 1
  expect_error(validate_fieldhub_design(bad), "location totals", class = "fieldhub_internal_error")
  bad <- x
  bad$data_locations$LOCATION[1] <- "unknown"
  expect_error(validate_fieldhub_design(bad), "location", class = "fieldhub_internal_error")
  bad <- x
  bad$rowsEachlist$Location[1] <- bad$rowsEachlist$Location[2]
  expect_error(validate_fieldhub_design(bad), "location", class = "fieldhub_internal_error")
  x <- suppressWarnings(split_families(5, data.frame(ENTRY = 1:3, NAME = LETTERS[1:3], FAMILY = "A"), seed = 38))
  expect_identical(validate_fieldhub_design(x), x)
})

test_that("field designs and allocations share a single construction boundary", {
  seen <- NULL
  validate <- function(x) {seen <<- x; invisible(x)}
  value <- list(payload = "unchanged")
  parameters <- list(seed = 38)
  result <- new_fieldhub_result(value, "example", 38, parameters,
                               c("fieldhub_example", "FielDHub"), validate)
  expect_identical(result, seen)
  expect_identical(result$payload, value$payload)
  expect_identical(result$metadata$parameters, parameters)
  expect_identical(class(result), c("fieldhub_example", "FielDHub"))
  for (builder in list(new_fieldhub_design, new_fieldhub_allocation)) {
    expect_true("new_fieldhub_result" %in% all.names(body(builder)))
  }
})

test_that("core code selects field-book columns by name, not position", {
  # Static/structural check per constraints.md ruling R2: inspect namespace
  # function bodies (core_functions(), from helper-source.R) instead of
  # parsing R/ source files, so this also works under R CMD check, where no
  # R/ directory exists.
  pattern <- paste0(
    "\\[\\s*,\\s*(-?c\\(\\s*[0-9]|-?[0-9]+\\s*(:|\\]))", # [, c(6,7,9,...)], [, 1:3], [, 2], [, -1], [, -c(1,2)]
    "|\\[\\[\\s*[0-9]+\\s*\\]\\]"                          # [[3]]
  )
  functions <- core_functions()
  hits <- names(Filter(function(f) any(grepl(pattern, deparse(body(f)))), functions))

  # Everything below indexes by position for a reason other than selecting
  # or reordering a *field book's* final columns: a fixed-position raw
  # upload contract (the app's entry-list CSV/data.frame always has ENTRY,
  # NAME, and sometimes CHECK/REPS in columns 1-3, regardless of the header
  # text a user supplies), internal matrix/vector algebra on a helper
  # structure the function builds and consumes itself (never returned as a
  # field book), a third-party package's own output matrix, or `[[n]]`
  # list-element access. `[[n]]` is kept in the pattern (it could catch a
  # genuine `df[[3]]` column pull), but every current hit was checked by
  # hand and is list-element access, not a data-frame column -- if that
  # changes, fix the site instead of adding it here.
  exceptions <- c(
    "alpha_lattice",               # upload contract: data_up <- data[, c(1, 2)]
    "AR1xAR1_simulation",          # internal simulation matrices (g.random/genet), response column
    "ARCBD_plot_number",           # list-element access
    "available_percent",           # internal percent lookup table + list access
    "check_same_entries",          # list-element access
    "CRD",                         # upload contract: data[, 1:2]
    "diagonal_arrangement",        # internal dimension-search table (infoP/percent_table) + list access
    "diagonal_dimension_choices",  # candidate-dimension matrix column comparison
    "dimension_options",           # list-element access (strsplit result)
    "do_optim",                    # upload contract: data[, 1:2]
    "export_design",                # G is a list of matrices, not a data frame; list-element access
    "factorial_levels_unique",     # upload contract: data[, 1:2]
    "field_layout",                # list-element access
    "full_factorial",              # dynamic placeholder factor columns (not yet named) + upload contract
    "get_random",                  # upload contract: data[, 1] + list access
    "get_random_stacked",          # upload contract: data[, 1]
    "get_single_random",           # upload contract: data[, 1]
    "incomplete_blocks",           # upload contract + blocksdesign package's own output matrix + list access
    "infer_arcbd_grid_dims",       # list-element access
    "latin_square",                # upload contract: data[, 1:3]
    "merge_user_data",             # upload contract: data[, 1:2]
    "optimized_arrangement",       # internal REPS matrix columns + upload contract
    "optimized_dimension_choices", # candidate-dimension matrix column comparison
    "order_ls",                    # internal factor-level columns
    "parse_whole_numbers",         # list-element access
    "partially_replicated",        # internal REPS matrix columns + upload contract + list access
    "paste_by_row",                # list-element access
    "planter_transform",           # list-element access
    "print.fieldhub_row_column",   # list-element access
    "pREP",                        # internal REPS/entry matrix columns + upload contract
    "random_checks",               # internal percent lookup table + list access
    "RCBD",                        # upload contract + internal layout matrix + list access
    "RCBD_augmented",              # upload contract + internal lookup table + list access
    "rcbd_resolve_entries",        # list-element access
    "rectangular_lattice",         # upload contract: data_up <- data[, c(1, 2)]
    "row_column",                  # upload contract: data_up <- data[, c(1, 2)]
    "set_augmented_blocks",        # list-element access
    "split_families",              # upload contract: data[, 1:3]
    "split_plot",                  # upload contract + internal layout matrix construction + list access
    "split_split_plot",            # upload contract + internal layout matrix construction + list access
    "square_lattice",              # upload contract: data_up <- data[, c(1, 2)]
    "stack_reps",                  # list-element access
    "strip_plot",                  # upload contract + internal layout vectors
    "unrep_data_parameters"        # upload contract: data_entry[, 1:2] / gen_list[, 1:3]
  )

  offenders <- setdiff(hits, exceptions)
  expect_identical(offenders, character(0))

  # Guard the exception list itself: every listed name must still be a real
  # hit, so a future fix that removes one doesn't quietly leave a stale,
  # unverifiable entry behind.
  expect_identical(sort(intersect(exceptions, hits)), sort(exceptions))
})
