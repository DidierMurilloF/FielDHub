test_that("one core registry declares every replay engine and field-book extension", {
  registry <- fieldhub_design_registry()
  expect_identical(fieldhub_engine_registry(), vapply(registry, `[[`, character(1), "engine"))
  expect_false(anyDuplicated(names(registry)) > 0L)
  for (entry in registry) {
    expect_true(is.function(getExportedValue("FielDHub", entry$engine)))
    expect_true(is.character(entry$columns) || is.function(entry$columns))
  }
  for (name in names(catalogue)) {
    x <- catalogue_design(name)
    if (inherits(x, "FielDHub") && x$metadata$design != "split_families") {
      expect_true(all(field_book_extension_columns(x) %in% names(x$fieldBook)), info = name)
    }
  }
})

test_that("a declared fixed-coordinate design needs no new presentation dispatch", {
  registry <- fieldhub_design_registry()
  registry$fixture <- list(engine = "CRD", columns = c("ROW", "COLUMN", "TREATMENT"),
    title = "Fixture Design", layout = "field_book_layouts", render = "draw_registered_layout")
  local_mocked_bindings(fieldhub_design_registry = function() registry)
  x <- new_fieldhub_design(list(infoDesign = list(id_design = "fixture", seed = 1),
    fieldBook = data.frame(ID = 1:4, LOCATION = "A", PLOT = 101:104,
      ROW = c(1L, 1L, 2L, 2L), COLUMN = c(1L, 2L, 1L, 2L), TREATMENT = c("A", "B", "B", "A"))),
    "fixture", parameters = list(t = 2, reps = 2, seed = 1))
  expect_identical(field_layout(x), x$fieldBook)
  expect_output(print(x), "Fixture Design")
  expect_output(print(summary(x)), "Fixture Design")
  plots <- draw_layout(x, x$fieldBook)
  expect_s3_class(plots$p1, "ggplot")
  expect_s3_class(plots$p2, "ggplot")
  expect_identical(plots$data, x$fieldBook)
  expect_identical(reproduction_engine(x), CRD)
  bad <- x
  bad$fieldBook$TREATMENT <- NULL
  expect_error(validate_fieldhub_design(bad), "TREATMENT", class = "fieldhub_internal_error")
  registry$fixture$layout <- NULL
  expect_error(field_layout(x), "no field layout", class = "fieldhub_input_error")
})
