library(testthat)
library(dplyr)
library(stringr)
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

test_that("For groups with ENTRY numeric value between 11 and 30, REP is always 1", {
  df_new <- add_rep_column(field_book2) |>
    dplyr::mutate(entry_num = as.numeric(ENTRY))

  groups <- df_new |>
    dplyr::group_by(LOCATION, ENTRY, entry_num) |>
    dplyr::summarise(
      group_size = n(),
      max_rep = max(REP),
      .groups = "drop"
    )

  groups_medium <- groups |>
    dplyr::filter(entry_num >= 31, entry_num <= 33)

  expect_true(
    all(groups_medium$max_rep == 5),
    info = "For ENTRY values between 31 and 33, REP should be max 5."
  )
})

