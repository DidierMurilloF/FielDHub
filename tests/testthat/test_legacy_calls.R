# Compatibility evidence for 1.5.x scripts: replays every call recorded in
# the v1.5.0 fixture (tools/generate-legacy-fixtures.R) against the current
# API. test_historical_objects.R already checks that the *saved* 1.5.0
# result objects still print/summary/plot/field_layout; this file focuses on
# replaying the recorded calls themselves.

library(FielDHub)

historical_bundle <- readRDS(test_path("fixtures", "v1.5.0-designs.rds"))

# Warning classes a 1.5.0 script may legitimately see when replayed on the
# current API: deprecated-argument notices and defaulted wrong-length
# plotNumber/locationNames fallbacks (DEF-12). Anything else is unexpected.
allowed_warning_classes <- c("fieldhub_deprecated_warning", "fieldhub_default_warning")

# Calls whose recorded 1.5.0 output is intentionally not reproduced by
# replaying the call exactly as recorded, with the NEWS entry explaining why
# and how to still obtain the 1.5.x design (verified in dedicated tests
# below). Every other recorded call's field book is expected unchanged.
legacy_result_changes <- c(
  row_column = paste(
    "row_column()'s new default, method = \"onestage\", jointly optimizes",
    "rows and columns instead of the 1.5.x two-step optimization (NEWS,",
    "'Changes to results for a given seed'); method = \"twostage\"",
    "reproduces the 1.5.x field book."
  ),
  split_families = paste(
    "split_families() now accepts seed; called without one (as the 1.5.0",
    "fixture script did, relying on a set.seed() made just before the",
    "call) it draws its own seed instead of consuming the caller's stream",
    "directly, so the default allocation changes (NEWS, Breaking changes).",
    "seed = 38 reproduces the 1.5.0 set.seed(38); split_families(l, data)",
    "allocation."
  )
)

# The main data-frame output of a FielDHub design/allocation result: the
# field book for every design function, `data_locations` for
# split_families() (which has no field book).
main_output <- function(x) {
  if (!is.null(x$fieldBook)) return(x$fieldBook)
  x$data_locations
}

# Aligns two field books' column types before comparing values. 1.5.0 stored
# some columns (e.g. RCBD_augmented()'s ENTRY) as character where the
# current version stores them as numeric, and used factors where the
# current version uses plain character; neither reflects a difference in
# the generated design, so columns whose type disagrees are coerced to
# character on both sides before comparing.
align_column_types <- function(old, new) {
  common <- intersect(names(old), names(new))
  old <- old[common]
  new <- new[common]
  for (column in common) {
    if (is.factor(old[[column]]) || is.factor(new[[column]]) ||
          is.numeric(old[[column]]) != is.numeric(new[[column]])) {
      old[[column]] <- as.character(old[[column]])
      new[[column]] <- as.character(new[[column]])
    }
  }
  list(old = old, new = new)
}

# Runs `expr`, asserting every warning it raises is of an allowed class, and
# returns its value (warnings are muffled so `expr` can complete).
expect_only_classed_warnings <- function(expr, engine) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      expect_true(
        any(class(w) %in% allowed_warning_classes),
        info = sprintf(
          "%s raised a warning of unexpected class <%s>: %s",
          engine, paste(class(w), collapse = ", "), conditionMessage(w)
        )
      )
      invokeRestart("muffleWarning")
    }
  )
}

for (name in names(historical_bundle$calls)) local({
  engine <- name
  args <- historical_bundle$calls[[engine]]
  saved <- historical_bundle$designs[[engine]]

  test_that(paste("1.5.0", engine, "call replays against the current API"), {
    replayed <- expect_only_classed_warnings(do.call(engine, args), engine)

    new_book <- main_output(replayed)
    old_book <- main_output(saved)
    expect_s3_class(new_book, "data.frame")
    expect_identical(dim(new_book), dim(old_book))
    expect_identical(names(new_book), names(old_book))

    if (!engine %in% names(legacy_result_changes)) {
      aligned <- align_column_types(old_book, new_book)
      expect_equal(aligned$new, aligned$old, ignore_attr = TRUE)
    }
  })
})

test_that("row_column()'s method = \"twostage\" reproduces the 1.5.0 field book", {
  args <- historical_bundle$calls$row_column
  saved <- historical_bundle$designs$row_column

  twostage <- expect_only_classed_warnings(
    do.call(row_column, c(args, list(method = "twostage"))), "row_column(method = twostage)"
  )

  aligned <- align_column_types(saved$fieldBook, twostage$fieldBook)
  expect_equal(aligned$new, aligned$old, ignore_attr = TRUE)
})

test_that("split_families() with seed = 38 reproduces the 1.5.0 allocation", {
  args <- historical_bundle$calls$split_families
  saved <- historical_bundle$designs$split_families

  reseeded <- expect_only_classed_warnings(
    do.call(split_families, c(args, list(seed = historical_bundle$initial_seed))),
    "split_families(seed = 38)"
  )

  aligned <- align_column_types(saved$data_locations, reseeded$data_locations)
  expect_equal(aligned$new, aligned$old, ignore_attr = TRUE)
  expect_equal(
    as.data.frame(saved$rowsEachlist), as.data.frame(reseeded$rowsEachlist),
    ignore_attr = TRUE
  )
})
