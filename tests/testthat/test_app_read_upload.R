library(FielDHub)

write_upload <- function(lines) {
  path <- tempfile(fileext = ".csv")
  writeLines(lines, path)
  path
}

fake_input <- function(design, path, sep = ",", name = "entries.csv") {
  spec <- FielDHub:::app_upload_spec(design)
  input <- list()
  input[[spec$file]] <- list(datapath = path, name = name)
  input[[spec$sep]] <- sep
  input
}

test_that("app_read_upload() returns list(data =) for a well-formed file", {
  path <- write_upload(c("ENTRY,NAME", "1,G1", "2,G2"))
  out <- app_read_upload(fake_input("alpha", path), "alpha")
  expect_named(out, "data")
  expect_equal(nrow(out$data), 2)
})

test_that("app_read_upload() reports a bad upload and returns NULL, never an error", {
  path <- write_upload(c("TREATMENT", "T1", "T1", "T2"))
  reported <- character()
  withCallingHandlers(
    out <- app_read_upload(fake_input("crd", path), "crd"),
    message = function(m) {
      reported[[length(reported) + 1L]] <<- conditionMessage(m)
      invokeRestart("muffleMessage")
    }
  )
  expect_null(out)
  expect_match(paste(reported, collapse = ""), "duplicate values", fixed = TRUE)
})

test_that("app_read_upload() returns NULL when no file has been chosen yet", {
  input <- list()
  spec <- FielDHub:::app_upload_spec("crd")
  input[[spec$sep]] <- ","
  err <- tryCatch(app_read_upload(input, "crd"), error = function(e) e)
  expect_s3_class(err, "shiny.silent.error")
})

test_that("app_read_upload() passes check = FALSE through to skip the uniqueness rule", {
  path <- write_upload(c("ENTRY,NAME", "1,G1", "1,G1"))
  out <- app_read_upload(fake_input("mdiag", path), "mdiag", check = FALSE)
  expect_named(out, "data")
  expect_equal(nrow(out$data), 2)
})
