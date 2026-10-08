library(testthat)

test_that("footer and team come from maintained metadata", {
  footer <- FielDHub:::fieldhub_footer("2042")
  expect_match(footer, "2042", fixed = TRUE)
  expect_match(footer, "https://sites.google.com/ndsu.edu/plsc-bpdm/home", fixed = TRUE)
  for (year in list(NULL, NA, "20xx", "<script>", c("2025", "2026"))) {
    expect_error(FielDHub:::fieldhub_footer(year), class = "fieldhub_input_error")
  }
  desc <- utils::packageDescription("FielDHub")
  persons <- eval(parse(text = desc[["Authors@R"]]))
  team <- FielDHub:::fieldhub_team(desc)
  expect_identical(nrow(team), length(persons))
  expect_identical(team$name, vapply(persons, function(p) paste(c(p$given, p$family), collapse = " "), ""))
  expect_identical(team$roles, vapply(persons, function(p) paste(p$role, collapse = ", "), ""))
  expect_identical(team$email, vapply(persons, function(p) paste(p$email, collapse = ", "), ""))
})

test_that("missing upload columns have guidance without a UI argument", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("ENTRY", "1", "2"), path)
  error <- tryCatch(FielDHub:::read_design_upload(path, ",", "alpha"), error = identity)
  expect_s3_class(error, "fieldhub_input_error")
  expect_match(conditionMessage(error), "ENTRY and NAME", fixed = TRUE)
})

test_that("BOM uploads work in the C locale too", {
  previous <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", previous), add = TRUE)
  invisible(Sys.setlocale("LC_CTYPE", "C"))
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeBin(c(as.raw(c(239, 187, 191)), charToRaw("ENTRY,NAME\n1,G1\n2,G2\n")), path)
  expect_identical(names(FielDHub:::read_design_upload(path, ",", "alpha")$data), c("ENTRY", "NAME"))
})

test_that("styles do not reset every child or impose a heatmap font", {
  css <- paste(readLines(system.file("app/www/style.css", package = "FielDHub")), collapse = "\n")
  expect_false(grepl("#fieldhub-app * {", css, fixed = TRUE))
  expect_false(grepl("Calibri", paste(deparse(body(FielDHub:::app_tile_heatmap)), collapse = " "), fixed = TRUE))
  expect_false(any(vapply(FielDHub:::fieldhub_design_specs(), function(spec) is.null(spec), logical(1))))
  expect_false(exists("default_entries", envir = asNamespace("FielDHub"), inherits = FALSE))
  expect_true(exists("app_check_dependencies", envir = asNamespace("FielDHub"), inherits = FALSE))
  expect_true(exists("app_design_menus", envir = asNamespace("FielDHub"), inherits = FALSE))
})

test_that("empty dimensions do not suggest an already-enabled filler option", {
  step <- list(label = "Select dimension for location", type = "location_selects")
  error <- tryCatch(FielDHub:::check_step_choices(step, list(character()), allow_fillers = TRUE), error = identity)
  expect_s3_class(error, "fieldhub_input_error")
  expect_match(conditionMessage(error), "Change the entries or checks", fixed = TRUE)
  expect_false(grepl("Allow filler plots", conditionMessage(error), fixed = TRUE))
})
