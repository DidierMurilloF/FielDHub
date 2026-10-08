library(FielDHub)

test_that("location counts use a shared scalar whole-number validator", {
  expect_identical(FielDHub:::validate_locations(2), 2)
  expect_identical(FielDHub:::validate_locations(2L), 2L)
  for (value in list(NULL, NA_real_, Inf, 0, -1, 1.5, "2", 1 + 1i, c(1, 2),
                     .Machine$integer.max + 1)) {
    expect_error(FielDHub:::validate_locations(value),
                 "number of locations", class = "fieldhub_input_error")
  }
})

test_that("every public location-aware engine rejects malformed counts consistently", {
  entries <- catalogue[!duplicated(vapply(catalogue, `[[`, character(1), "fun"))]
  for (entry in entries) {
    fun <- getExportedValue("FielDHub", entry$fun)
    if (!"l" %in% names(formals(fun))) next
    call <- if (entry$fun == "split_families") {
      quote(split_families(data = data.frame(ENTRY = 1:4, NAME = letters[1:4], FAMILY = 1)))
    } else {
      body(entry$build)[[2]]
    }
    args <- as.list(call)[-1]
    for (value in list(NA_real_, Inf, "2", c(1, 2))) {
      args["l"] <- list(value)
      invalid_call <- as.call(c(list(as.name(entry$fun)), args))
      expect_error(eval(invalid_call), "number of locations",
                   class = "fieldhub_input_error", info = entry$fun)
    }
  }
})
