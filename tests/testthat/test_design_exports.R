test_that("design downloads preserve complete results and reproducibility metadata", {
  directory <- tempfile("fieldhub-design-export-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  for (name in names(catalogue)) {
    x <- catalogue_design(name)
    handlers <- design_export_handlers(function() x)
    expect_match(handlers$filename(), "^FielDHub_[a-z_]+_seed_-?[0-9.]+[.]rds$", info = name)
    file <- file.path(directory, handlers$filename())
    handlers$content(file)
    expect_identical(readRDS(file), x, info = name)
    expect_identical(readRDS(file)$metadata$parameters, x$metadata$parameters, info = name)
  }
})

test_that("displayed R code reconstructs the saved design", {
  directory <- tempfile("fieldhub-design-code-")
  dir.create(directory)
  previous <- setwd(directory)
  on.exit({setwd(previous); unlink(directory, recursive = TRUE)}, add = TRUE)
  x <- RCBD(t = 5, reps = 3, seed = 38)
  handlers <- design_export_handlers(function() x)
  handlers$content(handlers$filename())
  set.seed(91)
  before <- .Random.seed
  result <- new.env(parent = globalenv())
  eval(parse(text = handlers$code()), envir = result)
  expect_identical(result$saved_design, x)
  expect_identical(result$design, x)
  expect_identical(.Random.seed, before)
  expect_match(handlers$code(), "simulated responses", fixed = TRUE)
  expect_match(handlers$code(), x$metadata$package_version, fixed = TRUE)
})

test_that("download handlers use the current completed result without modifying it", {
  x <- RCBD(t = 4, reps = 2, seed = 38)
  handlers <- design_export_handlers(function() x)
  first <- handlers$filename()
  x <- CRD(t = 4, reps = 2, seed = 57)
  before <- serialize(x, NULL)
  expect_false(identical(handlers$filename(), first))
  expect_match(handlers$filename(), "crd_seed_57", fixed = TRUE)
  expect_identical(serialize(x, NULL), before)
  x$metadata <- NULL
  expect_error(handlers$filename(), "recorded parameters", class = "fieldhub_input_error")
  expect_error(handlers$code(), "recorded parameters", class = "fieldhub_input_error")
})

test_that("exports reject unavailable designs before writing a file", {
  file <- tempfile(fileext = ".rds")
  handlers <- design_export_handlers(function() NULL)
  expect_error(handlers$content(file), class = "fieldhub_input_error")
  expect_false(file.exists(file))
})
