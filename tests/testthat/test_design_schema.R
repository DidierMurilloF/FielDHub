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
  #
  # Exceptions are keyed to the exact offending sub-expression (function
  # name + the deparsed `[`/`[[` call), not to the whole function: a
  # function that legitimately indexes a helper matrix by position
  # elsewhere must still fail this test if a *different* expression in it
  # selects or reorders field-book columns positionally -- including a
  # reverted fix. fielddhub_positional_index_calls() (helper-source.R)
  # walks the parsed call tree directly, so it also catches an expression
  # wrapped across lines, unlike matching on deparse(body(f)) text.
  pos <- function(fn, expr, reason) list(fn = fn, expr = expr, reason = reason)

  # Everything below indexes by position for a reason other than selecting
  # or reordering a *field book's* final columns: a fixed-position raw
  # upload contract (the app's entry-list CSV/data.frame always has ENTRY,
  # NAME, and sometimes CHECK/REPS in columns 1-3, regardless of the header
  # text a user supplies), internal matrix/vector algebra on a helper
  # structure the function builds and consumes itself (never returned as a
  # field book), a third-party package's own output matrix, or `[[n]]`
  # list-element access.
  allowed <- list(
    pos("alpha_lattice", "data[, c(1, 2)]",
        "upload contract: ENTRY/NAME are columns 1-2 regardless of header text"),
    pos("AR1xAR1_simulation", "g.random[, 1]",
        "internal simulation-effects matrix column"),
    pos("AR1xAR1_simulation", "genet[, 1]",
        "internal simulation-effects matrix column"),
    pos("AR1xAR1_simulation", "genet[, 2]",
        "internal simulation-effects matrix column"),
    pos("AR1xAR1_simulation", "outOrder[, 7]",
        "fixed response-column position before colnames(outOrder)[7] <- trail names it"),
    pos("ARCBD_plot_number", "plot_number(planter = planter, plot_number_start = i, layout_names = datos_name, expe_names = Name_expt, fillers = 0)[[1]]",
        "list-element access, not a data-frame column"),
    pos("available_percent", "dt[, 1]", "internal percent lookup table"),
    pos("available_percent", "dt[, 3]", "internal percent lookup table"),
    pos("available_percent", "M[, 4]", "internal percent lookup table"),
    pos("available_percent", "M[, 5]", "internal percent lookup table"),
    pos("available_percent", "w_map_checks[[2]]", "list-element access, not a data-frame column"),
    pos("available_percent", "W[, 2]", "internal percent lookup table"),
    pos("check_same_entries", "block_entries[[1]]", "list-element access, not a data-frame column"),
    pos("CRD", "data[, 1:2]", "upload contract: data[, 1:2]"),
    pos("diagonal_arrangement",
        "automatically_cuts(data = map_checks, planter_mov = planter, stacked = \"By Row\", dim_data = data_dim_each_block)[[1]]",
        "list-element access, not a data-frame column"),
    pos("diagonal_arrangement", "getData$checksEntries[[1]]",
        "list-element access, not a data-frame column"),
    pos("diagonal_arrangement", "infoP[, 6]", "internal dimension-search table"),
    pos("diagonal_arrangement", "infoP[, 7]", "internal dimension-search table"),
    pos("diagonal_arrangement", "percent_table[, 2]", "internal dimension-search table"),
    pos("diagonal_dimension_choices", "dims[, 1]", "candidate-dimension matrix column comparison"),
    pos("diagonal_dimension_choices", "dims[, 2]", "candidate-dimension matrix column comparison"),
    pos("diagonals_checks", "w_map[, 1:jump_by_cols]",
        "tiling an internal checks-placement matrix, not a field book"),
    pos("diagonals_checks", "w_map[, 1:rem]",
        "tiling an internal checks-placement matrix, not a field book"),
    pos("dimension_options", "strsplit(x, \"x\")[[1]]", "list-element access, not a data-frame column"),
    pos("do_optim", "data[, 1:2]", "upload contract: data[, 1:2]"),
    pos("export_design", "G[[1]]", "G is a list of matrices, not a data frame"),
    pos("export_design", "G[[2]]", "G is a list of matrices, not a data frame"),
    pos("export_design", "G[[3]]", "G is a list of matrices, not a data frame"),
    pos("export_design", "G[[4]]", "G is a list of matrices, not a data frame"),
    pos("export_design", "G[[5]]", "G is a list of matrices, not a data frame"),
    pos("factorial_levels_unique", "data[, 1:2, drop = FALSE]", "upload contract: data[, 1:2]"),
    pos("field_layout", "options[[1]]", "list-element access, not a data-frame column"),
    pos("full_factorial", "data.by.factor[[v]][, 2]",
        "upload contract: level column of a per-factor level CSV"),
    pos("full_factorial", "data[, 1:2]", "upload contract: data[, 1:2]"),
    pos("full_factorial", "m1[, 4:(4 + nt - 1), drop = FALSE]",
        "dynamic placeholder factor columns bound by cbind(), not yet named"),
    pos("full_factorial", "m1[, 4:(4 + nt - 1)]",
        "dynamic placeholder factor columns bound by cbind(), not yet named"),
    pos("get_random", "data[, 1]", "upload contract: data[, 1]"),
    pos("get_random", "W_SPLIT[[1]]", "list-element access, not a data-frame column"),
    pos("get_random", "W_SPLIT[[2]]", "list-element access, not a data-frame column"),
    pos("get_random_stacked", "data[, 1]", "upload contract: data[, 1]"),
    pos("get_single_random", "data[, 1]", "upload contract: data[, 1]"),
    pos("build_incomplete_blocks", "blocks_model[[1]]", "list-element access, not a data-frame column"),
    pos("build_incomplete_blocks", "data[, c(1, 2)]", "upload contract: data[, c(1, 2)]"),
    pos("build_incomplete_blocks", "mydes$Design_new[, 4]",
        "the blocksdesign package's own output matrix"),
    pos("infer_arcbd_grid_dims", "strsplit(s, \"x\", fixed = TRUE)[[1]]",
        "list-element access, not a data-frame column"),
    pos("latin_square", "data[, 1:3]", "upload contract: data[, 1:3]"),
    pos("merge_user_data", "data[, 1:2]", "upload contract: data[, 1:2]"),
    pos("new_workflow_archive", "strsplit(imports, \",\")[[1L]]",
        "list-element access, not a data-frame column"),
    pos("optimized_arrangement", "gen_list[, 1:3]", "upload contract: gen_list[, 1:3]"),
    pos("optimized_arrangement", "my_REPS[, 1]",
        "internal REPS-ordered entry-list column, not a field book"),
    pos("optimized_arrangement", "my_REPS[, 3]",
        "internal REPS-ordered entry-list column, not a field book"),
    pos("optimized_dimension_choices", "dims[, 1]", "candidate-dimension matrix column comparison"),
    pos("optimized_dimension_choices", "dims[, 2]", "candidate-dimension matrix column comparison"),
    pos("order_ls", "data[, 1]",
        "row/column label column of a small internal layout-description table"),
    pos("order_ls", "data[, 2]",
        "row/column label column of a small internal layout-description table"),
    pos("parse_whole_numbers", "strsplit(text, \",\", fixed = TRUE)[[1]]",
        "list-element access, not a data-frame column"),
    pos("partially_replicated", "data[, 1:3]", "upload contract: data[, 1:3]"),
    pos("partially_replicated", "gen_list[, 1:4]", "upload contract: gen_list[, 1:4]"),
    pos("partially_replicated", "genEntries[[2]]", "list-element access, not a data-frame column"),
    pos("partially_replicated", "reps_data[, 1]",
        "internal REPS-ordered entry-list column, not a field book"),
    pos("partially_replicated", "reps_data[, 3]",
        "internal REPS-ordered entry-list column, not a field book"),
    pos("paste_by_row", "files_list[[1]]", "list-element access, not a data-frame column"),
    pos("pREP", "data_rep_treatments[, 1]",
        "internal REPS-ordered entry-list column, not a field book"),
    pos("pREP", "data_rep_treatments[, 3]",
        "internal REPS-ordered entry-list column, not a field book"),
    pos("pREP", "data_unrep_treatments[, 1]",
        "internal REPS-ordered entry-list column, not a field book"),
    pos("pREP", "gen_list_order[, 1]",
        "internal REPS-ordered entry-list column, not a field book"),
    pos("pREP", "gen_list_order[, 3]",
        "internal REPS-ordered entry-list column, not a field book"),
    pos("pREP", "gen_list[, 1:3]", "upload contract: gen_list[, 1:3]"),
    pos("print.fieldhub_row_column", "x$blocksModel[[1]]", "list-element access, not a data-frame column"),
    pos("random_checks",
        "automatically_cuts(data = w_map, planter_mov = planter_mov, stacked = \"By Row\", dim_data = data_dim_each_block)[[1]]",
        "list-element access, not a data-frame column"),
    pos("random_checks", "my_P[, 1]", "internal percent lookup table"),
    pos("RCBD", "data[, 1]", "upload contract: data[, 1]"),
    pos("RCBD", "p.number.loc[[1]]", "list-element access, not a data-frame column"),
    pos("RCBD", "RCBD.layout[, 1]", "internal layout matrix column"),
    pos("RCBD_augmented", "data[, 1:2]", "upload contract: data[, 1:2]"),
    pos("RCBD_augmented", "feedback[, 1]", "internal grid-dimension options lookup table"),
    pos("RCBD_augmented", "feedback[, 2]", "internal grid-dimension options lookup table"),
    pos("RCBD_augmented", "layout1_loc1[[1]]", "list-element access, not a data-frame column"),
    pos("RCBD_augmented", "lines_blocks[[1]]", "list-element access, not a data-frame column"),
    pos("RCBD_augmented", "plot_loc1[[1]]", "list-element access, not a data-frame column"),
    pos("rcbd_resolve_entries", "data[[1]]",
        "upload contract: the treatment column is data[[1]] regardless of header text"),
    pos("rectangular_lattice", "data[, c(1, 2)]", "upload contract: data[, c(1, 2)]"),
    pos("row_column", "data[, c(1, 2)]", "upload contract: data[, c(1, 2)]"),
    pos("set_augmented_blocks", "strsplit(s, \"x\", fixed = TRUE)[[1]]",
        "list-element access, not a data-frame column"),
    pos("split_families", "data[, 1:3]", "upload contract: data[, 1:3]"),
    pos("split_plot", "args1[[1]]", "list-element access, not a data-frame column"),
    pos("split_plot", "args1[[2]]", "list-element access, not a data-frame column"),
    pos("split_plot", "data[, 1:2]", "upload contract: data[, 1:2]"),
    pos("split_plot", "spd.layout[, 1]", "internal layout matrix column"),
    pos("split_plot", "spd.layout[, 2]", "internal layout matrix column"),
    pos("split_plot", "spd.layout[, 3]", "internal layout matrix column"),
    pos("split_split_plot", "args1[[1]]", "list-element access, not a data-frame column"),
    pos("split_split_plot", "args1[[2]]", "list-element access, not a data-frame column"),
    pos("split_split_plot", "args1[[3]]", "list-element access, not a data-frame column"),
    pos("split_split_plot", "data[, 1:3]", "upload contract: data[, 1:3]"),
    pos("split_split_plot", "sspd.layout[, 1]", "internal layout matrix column"),
    pos("split_split_plot", "sspd.layout[, 2]", "internal layout matrix column"),
    pos("split_split_plot", "sspd.layout[, 3]", "internal layout matrix column"),
    pos("square_lattice", "data[, c(1, 2)]", "upload contract: data[, c(1, 2)]"),
    pos("stack_reps", "x_list[[1]]", "list-element access, not a data-frame column"),
    pos("strip_plot", "data[, 1:2]", "upload contract: data[, 1:2]"),
    pos("strip_plot", "Hplots.random[, 1]", "internal layout matrix column"),
    pos("strip_plot", "Vplots.random[, 1]", "internal layout matrix column"),
    pos("swap_pairs_core", "active_pos[, 1L]", "internal plot-coordinate matrix column"),
    pos("swap_pairs_core", "active_pos[, 2L]", "internal plot-coordinate matrix column"),
    pos("swap_pairs_core", "designs[[1L]]", "list-element access, not a data-frame column"),
    pos("swap_pairs_core", "distances[[1L]]", "list-element access, not a data-frame column"),
    pos("unrep_data_parameters", "data_entry[, 1:2]", "upload contract: data_entry[, 1:2]"),
    pos("unrep_data_parameters", "gen_list[, 1:3]", "upload contract: gen_list[, 1:3]"),
    pos("validate_fieldhub_optimization", "x$designs[[1L]]",
        "list-element access, not a data-frame column")
  )

  key <- function(fn, expr) paste(fn, expr, sep = " ||| ")
  allowed_keys <- vapply(allowed, function(a) key(a$fn, a$expr), character(1))
  expect_identical(anyDuplicated(allowed_keys), 0L)

  functions <- core_functions()
  observed <- unlist(lapply(names(functions), function(fn) {
    calls <- fielddhub_positional_index_calls(body(functions[[fn]]))
    if (length(calls) == 0L) return(character(0))
    exprs <- unique(vapply(calls, deparse1, character(1)))
    key(fn, exprs)
  }))
  observed <- unique(observed)

  offenders <- setdiff(observed, allowed_keys)
  expect_identical(offenders, character(0))

  # Guard the allow-list itself: every entry must still be a real, observed
  # hit, so a fix that removes one doesn't quietly leave a stale,
  # unverifiable entry behind.
  expect_identical(sort(intersect(allowed_keys, observed)), sort(allowed_keys))
})
