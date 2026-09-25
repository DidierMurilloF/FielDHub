library(testthat)
library(dplyr)
library(FielDHub)


# Prepare the example field book.
prep_example <- partially_replicated(
  nrows = 8, 
  ncols = 5, 
  repGens = c(10, 20), 
  repUnits = c(2, 1),
  seed = 65784
)

# Save the original field book (without REP)
field_book1 <- prep_example$fieldBook |>
  dplyr::select(-REP)

### TESTS for the first example

# Test 1: Check that the REP column is added and is in the 8th column position after reordering.
test_that("REP column is added and is in the 8th column position after reordering", {
  df_new <- add_rep_column(field_book1)
  # In the function, we always select exactly 11 columns in the specified order.
  expect_equal(names(df_new)[8], "REP")
})

# Test 2: Check that dimensions are as expected.
test_that("Dimensions of the data frame are as expected", {
  df_new <- add_rep_column(field_book1)
  # The original field_book1 has 10 columns, so after adding REP there should be 11.
  expect_equal(nrow(df_new), nrow(field_book1))
  expect_equal(ncol(df_new), ncol(field_book1) + 1)
})

# For tests 3 and 4, add an 'entry_num' column after adding REP.
field_book_with_num <- field_book1 |> 
  dplyr::mutate(entry_num = ENTRY)

# Test 3: For groups with ENTRY values between 1 and 10.
test_that("For groups with ENTRY between 1 and 10, REP is 1 for single occurrences and 1,2 when replicated", {
  df_new <- add_rep_column(field_book_with_num) |>
    dplyr::mutate(entry_num = ENTRY)
  
  groups <- df_new |>
    dplyr::group_by(LOCATION, ENTRY, entry_num) |>
    dplyr::summarise(
      group_size = dplyr::n(),
      max_rep = max(REP),
      .groups = "drop"
    )
  
  groups_small <- groups |> dplyr::filter(entry_num >= 1, entry_num <= 10)
  
  expect_true(
    all(ifelse(groups_small$group_size > 1, groups_small$max_rep == 2, groups_small$max_rep == 1)),
    info = "For ENTRY values between 1 and 10, groups with multiple rows should have max REP = 2; otherwise, REP is 1."
  )
})

# Test 4: For groups with ENTRY values between 11 and 30.
test_that("For groups with ENTRY between 11 and 30, REP is always 1", {
  df_new <- add_rep_column(field_book_with_num) |>
    dplyr::mutate(entry_num = ENTRY)
  
  groups <- df_new |>
    dplyr::group_by(LOCATION, ENTRY, entry_num) |>
    dplyr::summarise(
      group_size = dplyr::n(),
      max_rep = max(REP),
      .groups = "drop"
    )
  
  groups_medium <- groups |> dplyr::filter(entry_num >= 11, entry_num <= 30)
  
  expect_true(
    all(groups_medium$max_rep == 1),
    info = "For ENTRY values between 11 and 30, REP should always be 1 (no replication)."
  )
})

# Test 5: Check that the relationship among ROW, COLUMN, and ENTRY remains unchanged.
test_that("The relationship among ROW, COLUMN, and ENTRY remains unchanged", {
  df_new <- add_rep_column(field_book1)
  df_compare <- df_new |> dplyr::select(-REP)
  expect_equal(df_compare, field_book1)
})


### Additional Tests for a Second Example

prep_example2 <- partially_replicated(
  nrows = 11,
  ncols = 5,
  repGens = c(10, 20, 3),
  repUnits = c(2, 1, 5),
  seed = 65784
)

# Use the original field book from prep_example2 (without REP)
field_book2 <- prep_example2$fieldBook |>
  dplyr::select(-REP)

test_that("REP column is added and is in the 8th column position after reordering (example 2)", {
  df_new <- add_rep_column(field_book2)
  expect_equal(names(df_new)[8], "REP")
})

test_that("Dimensions of the data frame are as expected (example 2)", {
  df_new <- add_rep_column(field_book2)
  # field_book2 has 10 columns, so we expect 11 columns after adding REP.
  expect_equal(nrow(df_new), nrow(field_book2))
  expect_equal(ncol(df_new), ncol(field_book2) + 1)
})

# For groups with ENTRY between 1 and 10 (example 2)
field_book2_with_num <- field_book2 |> 
  dplyr::mutate(entry_num = ENTRY)

test_that("For groups with ENTRY between 1 and 10, REP is correct (example 2)", {
  df_new <- add_rep_column(field_book2_with_num) |>
    dplyr::mutate(entry_num = ENTRY)
  
  groups <- df_new |>
    dplyr::group_by(LOCATION, ENTRY, entry_num) |>
    dplyr::summarise(
      group_size = dplyr::n(),
      max_rep = max(REP),
      .groups = "drop"
    )
  
  groups_small <- groups |> dplyr::filter(entry_num >= 1, entry_num <= 10)
  
  expect_true(
    all(ifelse(groups_small$group_size > 1, groups_small$max_rep == 2, groups_small$max_rep == 1)),
    info = "For ENTRY values between 1 and 10, groups with multiple rows should have max REP = 2; otherwise, REP is 1 (example 2)."
  )
})

test_that("For groups with ENTRY between 11 and 30, REP is always 1 (example 2)", {
  df_new <- add_rep_column(field_book2_with_num) |>
    dplyr::mutate(entry_num = ENTRY)
  
  groups <- df_new |>
    dplyr::group_by(LOCATION, ENTRY, entry_num) |>
    dplyr::summarise(
      group_size = dplyr::n(),
      max_rep = max(REP),
      .groups = "drop"
    )
  
  groups_medium <- groups |> dplyr::filter(entry_num >= 11, entry_num <= 30)
  
  expect_true(
    all(groups_medium$max_rep == 1),
    info = "For ENTRY values between 11 and 30, REP should always be 1 (example 2)."
  )
})

test_that("For groups with ENTRY numeric value between 31 and 33, REP is always 5", {
  df_new <- add_rep_column(field_book2) |>
    dplyr::mutate(entry_num = as.numeric(ENTRY))
  
  groups <- df_new |>
    dplyr::group_by(LOCATION, ENTRY, entry_num) |>
    dplyr::summarise(
      group_size = n(),
      max_rep = max(REP),
      .groups = "drop"
    )
  
  groups_target <- groups |>
    dplyr::filter(entry_num >= 31, entry_num <= 33)
  
  expect_true(
    all(groups_target$max_rep == 5),
    info = "For ENTRY values between 31 and 33, REP should be 5 for each group."
  )
})

test_that("prime-sized p-rep designs support opt-in filler plots", {
  expect_error(
    partially_replicated(
      nrows = 6,
      ncols = 7,
      repGens = c(5, 31),
      repUnits = c(2, 1),
      seed = 123
    ),
    "prime number"
  )

  design <- partially_replicated(
    nrows = 6,
    ncols = 7,
    repGens = c(5, 31),
    repUnits = c(2, 1),
    seed = 123,
    allow_fillers = TRUE
  )

  expect_equal(design$infoDesign$experimental_plots, 41)
  expect_equal(design$infoDesign$field_capacity, 42)
  expect_equal(design$infoDesign$fillers, 1)
  expect_equal(sum(design$fillerField[[1]]), 1)
  expect_equal(sum(design$layoutRandom[[1]] == 0), 1)
  expect_equal(sum(design$plotNumber[[1]] == 0), 1)
  expect_false(any(design$pairsDistance[[1]]$geno == 0))

  filler_row <- design$fieldBook$TREATMENT == "Filler"
  expect_equal(sum(filler_row), 1)
  expect_equal(design$fieldBook$ENTRY[filler_row], 0)
  expect_equal(design$fieldBook$PLOT[filler_row], 0)
  expect_equal(design$fieldBook$CHECKS[filler_row], 0)
  expect_true(is.na(design$fieldBook$REP[filler_row]))

  observed_reps <- table(design$fieldBook$ENTRY[!filler_row])
  expect_equal(as.numeric(observed_reps[as.character(1:5)]), rep(2, 5))
  expect_equal(as.numeric(observed_reps[as.character(6:36)]), rep(1, 31))
})

test_that("p-rep dimension options keep fillers opt-in", {
  expect_null(FielDHub:::prep_dimension_options(41, allow_fillers = FALSE))

  options <- FielDHub:::prep_dimension_options(41, allow_fillers = TRUE)
  expect_gt(nrow(options), 0)
  expect_equal(min(options$fillers), 1)
  expect_true(all(options$capacity - options$fillers == 41))
  expect_true(all(grepl("filler", options$label)))
})

test_that("p-rep dimension options cap offered fillers by default", {
  options <- FielDHub:::prep_dimension_options(293, allow_fillers = TRUE)
  expanded <- FielDHub:::prep_dimension_options(
    293,
    allow_fillers = TRUE,
    max_fillers = 12
  )

  expect_lte(max(options$fillers), 10)
  expect_true(any(expanded$fillers > 10))
})

test_that("fillers follow planter direction and row parity", {
  cartesian_even <- FielDHub:::prep_field_mask(
    nrows = 6,
    ncols = 7,
    fillers = 1,
    planter = "cartesian"
  )
  serpentine_even <- FielDHub:::prep_field_mask(
    nrows = 6,
    ncols = 7,
    fillers = 1,
    planter = "serpentine"
  )
  serpentine_odd <- FielDHub:::prep_field_mask(
    nrows = 5,
    ncols = 7,
    fillers = 1,
    planter = "serpentine"
  )

  expect_false(cartesian_even[1, 7])
  expect_false(serpentine_even[1, 1])
  expect_false(serpentine_odd[1, 7])
})



