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
