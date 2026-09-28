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
