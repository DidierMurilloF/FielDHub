library(FielDHub)

# Plain helpers behind the spatial design pages (design_app_spec()): the
# choices their steps offer, the checks of Run! and the tables their result
# tabs show. Every one of them either returns what the page offers or raises
# a classed FielDHub error, so no input can end a session.

blank_inputs <- list(NULL, NA, NA_real_, numeric(), "", "a", -3, 0, 2.5, c(10, 20), Inf)

expect_classed <- function(expr, info = NULL) {
  expect_error(expr, class = "fieldhub_error", info = info)
}

test_that("field sizes are offered with the first selected, or explained when none fits", {
  expect_identical(field_size_choices(c("2 x 3", "3 x 2"), "none"),
                   list(choices = c("2 x 3", "3 x 2"), selected = "2 x 3"))
  expect_error(field_size_choices(character(), "Nothing fits."), "Nothing fits.",
               class = "fieldhub_input_error")
})

test_that("optimized arrangements offer the field sizes of their plots", {
  offered <- optim_field_choices(312)
  expect_identical(offered$choices, optimized_dimension_choices(312))
  expect_identical(offered$selected, "13 x 24")
  # a prime number of plots has no field of at least 4 x 4
  expect_error(optim_field_choices(307), "Please try a different number of treatments or checks.",
               class = "fieldhub_input_error")
  for (value in blank_inputs) expect_classed(optim_field_choices(value), info = deparse(value))
})

test_that("an optimized arrangement typed as counts needs more entries than check plots", {
  expect_identical(optim_total_plots(280, c(8, 8, 8, 8)), 312)
  expect_error(optim_total_plots(32, c(8, 8, 8, 8)), "Number of lines should be greater",
               class = "fieldhub_input_error")
})

test_that("an uploaded REPS column must hold whole numbers", {
  entries <- data.frame(ENTRY = 1:3, NAME = c("A", "B", "C"), REPS = c(2L, 1L, 1L))
  expect_identical(check_reps_upload(entries), entries)
  for (reps in list(c(2, 1, 1), c("2", "1", "1"), factor(c(2, 1, 1)))) {
    entries$REPS <- reps
    expect_error(check_reps_upload(entries), "'REPS' must be numeric.", class = "fieldhub_input_error")
  }
})

test_that("field grids read as the field, numbered from its bottom row", {
  grid <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE)
  view <- field_grid_view(grid, highlight = c(1, 6), colours = spatial_highlight_colours("checks", 2))
  expect_identical(colnames(view$data), c("V1", "V2", "V3"))
  expect_identical(rownames(view$data), c("2", "1"))
  expect_identical(view$data$V1, c(1, 4))
  expect_identical(view$colours, c("royalblue", "salmon"))
  fillers <- matrix(c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE), nrow = 2, byrow = TRUE)
  expect_identical(field_grid_view(grid, fillers = fillers)$data$V3, c("3", "Filler"))
  for (bad in list(NULL, 1:3, "x")) {
    expect_error(field_grid_view(bad), "no field layout", class = "fieldhub_input_error")
  }
})

test_that("highlight colours are one per value, or one colour for replicated entries", {
  expect_length(spatial_highlight_colours("checks", 3), 3L)
  expect_identical(spatial_highlight_colours("replicated", 2), c("green", "green"))
  expect_identical(spatial_highlight_colours("experiments", 2), c("snow", "cadetblue"))
  expect_true(is.na(spatial_highlight_colours("checks", 13)[13]))
  expect_error(spatial_highlight_colours("other", 1), class = "fieldhub_internal_error")
})

test_that("entry lists show their names and entries as filterable factors", {
  entries <- entry_list_view(data.frame(ENTRY = 1:2, NAME = c("A", "B"), REPS = c(2L, 1L)),
                             c("ENTRY", "NAME", "REPS"))
  expect_true(all(vapply(entries, is.factor, logical(1))))
  expect_identical(levels(entry_list_view(list(ENTRY = 2:1, NAME = c("b", "a")))$NAME), c("a", "b"))
})

test_that("p-rep pages offer field sizes, labelled with their filler plots where allowed", {
  offered <- prep_field_choices(300, FALSE)
  expect_identical(offered$selected, "15 x 20")
  expect_identical(unname(offered$choices), prep_dimension_options(300)$value)
  # 301 plots: 7 x 43 without fillers, squarer fields with them
  expect_identical(unname(prep_field_choices(301, FALSE)$choices), c("43 x 7", "7 x 43"))
  with_fillers <- prep_field_choices(301, TRUE)
  expect_true("16 x 19 (+3 fillers)" %in% names(with_fillers$choices))
  expect_identical(with_fillers$selected, "43 x 7")
  expect_error(prep_field_choices(307, FALSE), "Select 'Allow filler plots'", class = "fieldhub_input_error")
  expect_error(prep_field_choices(307, NA), "Select 'Allow filler plots'", class = "fieldhub_input_error")
  for (value in blank_inputs) expect_classed(prep_field_choices(value, TRUE), info = deparse(value))
})

test_that("augmented RCBD pages offer the blocks and fields their entries fit", {
  offered <- augmented_block_choices(180, 4)
  expect_identical(offered$choices, unique(set_augmented_blocks(180, 4, start = 3)$b))
  expect_identical(offered$selected, 3)
  fields <- augmented_field_choices(180, 4, 3)
  expect_identical(fields$selected, "3 x 64")
  expect_error(augmented_field_choices(180, 4, 999), "No field size holds 999 blocks",
               class = "fieldhub_input_error")
  for (value in blank_inputs) {
    expect_classed(augmented_block_choices(value, 4), info = deparse(value))
    expect_classed(augmented_block_choices(180, value), info = deparse(value))
    expect_classed(augmented_field_choices(value, 4, 3), info = deparse(value))
    expect_classed(augmented_field_choices(180, 4, value), info = deparse(value))
    expect_classed(augmented_lines(value, 4), info = deparse(value))
  }
  expect_identical(augmented_lines(180, 4), 180)
  expect_identical(augmented_lines(NULL, 4, data.frame(ENTRY = 1:20)), 16)
  expect_error(augmented_lines(NULL, 4, data.frame(ENTRY = 1:11)), "At least ten treatments",
               class = "fieldhub_input_error")
})

test_that("diagonal pages read their entries and checks, typed or uploaded", {
  expect_identical(diagonal_entries(287, 4),
                   list(checks_entries = 1:4, entries = 291, field_entries = 287, layout = NULL))
  entries <- data.frame(ENTRY = 11:30, NAME = paste0("E", 1:20))
  read <- diagonal_entries(NULL, 3, entries)
  expect_identical(read$checks_entries, c(11, 12, 13))
  expect_identical(read$entries, 20L)
  expect_identical(read$field_entries, 20L)
  expect_identical(upload_check_entries(entries[c(3, 1, 2, 4:20), ], 3), c(11, 12, 13))
  for (bad in list(entries[c(1, 5, 2:4, 6:20), ], transform(entries, ENTRY = c("a", ENTRY[-1])),
                   entries[1:3, ])) {
    expect_error(upload_check_entries(bad, 3), "consecutive ENTRY numbers", class = "fieldhub_input_error")
  }
})

test_that("diagonal pages offer fields, then percentages of checks with the API default selected", {
  fields <- diagonal_field_choices(287, 287, 1:4)
  expect_identical(fields$choices, diagonal_dimension_choices(287, 1:4))
  expect_identical(fields$selected, "18 x 18")
  expect_error(diagonal_field_choices(2, 2, 1:4), "Insufficient number of entries provided!",
               class = "fieldhub_input_error")
  percents <- diagonal_percent_choices(18, 18, 1:4, 291)
  expect_identical(percents$selected, utils::tail(percents$choices, 1L))
  expect_identical(percents$choices, as.numeric(percents$table[["Percentage of Checks"]]))
  expect_error(diagonal_percent_choices(5, 5, 1:4, 291), "does not fit", class = "fieldhub_input_error")
  for (value in blank_inputs) {
    expect_classed(diagonal_field_choices(value, 287, 1:4), info = deparse(value))
    expect_classed(diagonal_field_choices(287, value, 1:4), info = deparse(value))
    expect_classed(diagonal_percent_choices(value, 18, 1:4, 291), info = deparse(value))
    expect_classed(diagonal_percent_choices(18, 18, 1:4, value), info = deparse(value))
  }
})

test_that("the checks table of a diagonal location lists each check and its plots", {
  design <- diagonal_arrangement(nrows = 18, ncols = 18, lines = 287, checks = 4, seed = 1)
  view <- diagonal_checks_view(design, 1)
  expect_identical(names(view), c("ENTRY", "NAME", "TIMES"))
  expect_identical(view$ENTRY, design$infoDesign$entry_checks[[1]])
  expect_equal(sum(view$TIMES), sum(design$fieldBook$CHECKS != 0))
})

test_that("multiple diagonal pages lay out their entries by experiment, checks first", {
  read <- multiple_diagonal_entries(60, c(30, 30), 2, FALSE)
  expect_identical(read$checks_entries, 1:2)
  expect_identical(read$entries, 62)
  expect_identical(read$field_entries, 60)
  expect_identical(read$layout$BLOCK, c("ALL", "ALL", rep(c("1", "2"), each = 30)))
  upload <- data.frame(ENTRY = 1:62, NAME = paste0("E", 1:62))
  read <- multiple_diagonal_entries(NULL, c(30, 30), 2, TRUE, upload)
  expect_identical(read$field_entries, 62L)
  expect_identical(names(read$layout), c("ENTRY", "NAME", "BLOCK"))
  expect_error(multiple_diagonal_entries(61, c(30, 30), 2, FALSE), "must add up",
               class = "fieldhub_input_error")
  expect_error(multiple_diagonal_entries(NULL, c(30, 31), 2, FALSE, upload), "does not match",
               class = "fieldhub_input_error")
  expect_error(multiple_diagonal_entries(40, c(20, 20), 2, FALSE), "Larger field size",
               class = "fieldhub_input_error")
  expect_error(multiple_diagonal_entries(60, c(20, 40), 2, TRUE), "same size",
               class = "fieldhub_input_error")
})

test_that("multiple diagonal pages show the entries per experiment and each experiment's plots", {
  entries <- data.frame(ENTRY = 1:7, BLOCK = c("ALL", "1", "1", "2", "2", "2", "2"))
  expect_identical(block_frequency_view(entries),
                   data.frame(`SUB-BLOCKS` = factor(c("1", "2", "ALL")), FREQUENCY = c(2L, 4L, 1L),
                              check.names = FALSE))
  design <- diagonal_arrangement(nrows = 19, ncols = 18, lines = 300, checks = 4, kindExpt = "DBUDC",
                                 blocks = c(100, 120, 80), exptName = c("E1", "E2", "E3"), l = 2,
                                 seed = 3)
  view <- experiment_grid_view(design, 2)
  expect_identical(view$highlight, c("E1", "E2", "E3"))
  expect_identical(view$colours, c("snow", "cadetblue", "lightgreen"))
  expect_identical(dim(view$data), c(19L, 18L))
  expect_error(experiment_grid_view(design, 3), "no field layout", class = "fieldhub_input_error")
})

test_that("sparse allocation pages check their locations, entries and uploaded list", {
  expect_identical(sparse_entries(100, 3, 4), list(checks_entries = c(101, 102, 103), names = NULL))
  upload <- data.frame(ENTRY = 1:63, NAME = c("C1", "C2", "C3", paste0("L", 4:63)))
  expect_identical(sparse_entries(60, 3, 3, upload),
                   list(checks_entries = c(1, 2, 3), names = paste0("L", 4:63)))
  expect_error(sparse_entries(100, 3, 2), "at least 3 locations", class = "fieldhub_input_error")
  expect_error(sparse_entries(59, 3, 3), "at least 60 entries", class = "fieldhub_input_error")
  expect_error(sparse_entries(61, 3, 3, upload), "does not match", class = "fieldhub_input_error")
})

test_that("allocation tables add the copies of each entry and the totals", {
  allocation <- list(allocation = data.frame(LOC1 = c(1, 0, 1), LOC2 = c(1, 1, 0)))
  view <- allocation_view(allocation, c("a", "b", "c"))
  expect_identical(rownames(view), c("a", "b", "c", "Total"))
  expect_identical(view$Copies, c(2, 1, 1, 4))
  expect_identical(view$LOC1, c(1, 0, 1, 2))
  averaged <- allocation_view(allocation, c("a", "b", "c"), average = TRUE)
  expect_identical(averaged$Avg, c(1, 0.5, 0.5, NA))
})

test_that("location entry lists show each location's entries, LOCATION first", {
  view <- location_entries_view(list(LOC1 = data.frame(ENTRY = 1:2, NAME = c("a", "b"), X = 0),
                                     LOC2 = data.frame(ENTRY = 3L, NAME = "c", X = 0)))
  expect_identical(names(view), c("LOCATION", "ENTRY", "NAME"))
  expect_identical(as.character(view$LOCATION), c("LOC1", "LOC1", "LOC2"))
  expect_true(all(vapply(view, is.factor, logical(1))))
})
