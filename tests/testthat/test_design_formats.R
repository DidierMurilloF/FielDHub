test_that("registered formats have explicit read and write capabilities", {
  formats <- design_formats()
  expect_identical(formats$format, c("rds", "r"))
  expect_identical(formats$readable, c(TRUE, FALSE))
  expect_true(all(formats$writable))
  expect_identical(names(fieldhub_importer_registry()), "rds")
  expect_identical(names(fieldhub_exporter_registry()), c("rds", "r"))
})

test_that("the RDS registry round-trips every result without replaying it", {
  directory <- tempfile("fieldhub-formats-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  set.seed(391)
  before <- .Random.seed
  for (name in names(catalogue)) {
    x <- catalogue_design(name)
    file <- file.path(directory, paste0(name, ".rds"))
    expect_identical(write_design(x, file), file)
    expect_identical(read_design(file), x, info = name)
    expect_identical(.Random.seed, before)
  }
})

test_that("export permissions are explicit and failures leave existing files intact", {
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  x <- latin_rectangle(5, 3, seed = 1)
  saveRDS("existing", file)
  expect_error(write_design(x, file), "exists", class = "fieldhub_input_error")
  expect_identical(readRDS(file), "existing")
  expect_error(write_design(NULL, file, overwrite = TRUE), class = "fieldhub_input_error")
  expect_identical(readRDS(file), "existing")
  expect_error(write_design(x, file, format = "unknown", overwrite = TRUE), class = "fieldhub_input_error")
  expect_error(write_design(x, file, overwrite = NA), class = "fieldhub_input_error")
  expect_identical(readRDS(file), "existing")
  write_design(x, file, overwrite = TRUE)
  expect_identical(read_design(file), x)
  expect_error(write_design(x, dirname(file), overwrite = TRUE), class = "fieldhub_input_error")
})

test_that("imports reject malformed and unknown-schema files with one classed condition", {
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  expect_error(read_design(file), class = "fieldhub_input_error")
  for (bad in list(NULL, "wrong", list(metadata = list(schema_version = 99)))) {
    saveRDS(bad, file)
    expect_error(read_design(file), class = "fieldhub_import_error")
  }
  x <- latin_rectangle(5, 3, seed = 1)
  x$metadata$schema_version <- 99L
  saveRDS(x, file)
  expect_error(read_design(file), "unknown schema", class = "fieldhub_import_error")
  writeLines("not serialized R data", file)
  expect_error(read_design(file), class = "fieldhub_import_error")
  expect_error(read_design(file, "r"), "not supported", class = "fieldhub_input_error")
  expect_error(read_design(c(file, file)), class = "fieldhub_input_error")
})

test_that("copy failures use the common error contract and clean temporary output", {
  directory <- tempfile("fieldhub-copy-failure-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  local_mocked_bindings(file.copy = function(...) FALSE, .package = "base")
  expect_error(write_design(latin_rectangle(5, 3, seed = 1), file.path(directory, "result.rds")),
    "Could not copy", class = "fieldhub_export_error")
  expect_length(list.files(directory, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("standalone script exports replay exactly but are never imported as code", {
  file <- tempfile(fileext = ".R")
  on.exit(unlink(file), add = TRUE)
  x <- latin_rectangle(c("No nitrogen", "A*B", "C", "D", "E"), 3, seed = 27)
  write_design(x, file, "r")
  environment <- new.env(parent = globalenv())
  sys.source(file, envir = environment)
  expect_identical(environment$design, x)
  expect_error(read_design(file, "r"), class = "fieldhub_input_error")
  bad <- x
  bad$metadata$parameters$t <- quote(stop("must not execute"))
  expect_error(write_design(bad, file, "r", overwrite = TRUE), "not portable", class = "fieldhub_export_error")
  sys.source(file, envir = environment)
  expect_identical(environment$design, x)
})
