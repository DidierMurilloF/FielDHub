# Regression guard: R's `$` allows partial matching, so `inFile$data` would
# silently resolve to `inFile$datapath` (or worse, nothing, if there were a
# `datapath_backup` field) instead of raising an error. The path used to
# come from each module's own `inFile$datapath`; it now comes from one
# place, app_read_upload() (R/app_upload.R), so the guard runs on that body
# once instead of on all 19 modules' own copies.
test_that("app_read_upload() reads the uploaded file's exact path, not a partial match", {
  previous <- options(warnPartialMatchDollar = TRUE)
  on.exit(options(previous), add = TRUE)

  code <- body(FielDHub:::app_read_upload)
  calls <- function(x) {
    if (!is.call(x) && !is.pairlist(x)) return(list())
    if (is.call(x) && identical(x[[1]], as.name("[[")) &&
        identical(x[[3]], "datapath")) {
      return(list(x))
    }
    unlist(lapply(as.list(x), calls), recursive = FALSE)
  }
  accesses <- calls(code)
  expect_length(accesses, 1L)

  file <- data.frame(name = "entries.csv", datapath = "exact-upload-path")
  expect_warning(path <- eval(accesses[[1]], list(file = file)), NA)
  expect_identical(path, file$datapath)

  absent <- data.frame(name = "entries.csv", datapath_backup = "unrelated-file")
  expect_warning(missing_path <- eval(accesses[[1]], list(file = absent)), NA)
  expect_true(is.null(missing_path))
})

test_that("every registered module reads its upload through app_read_upload() exactly once", {
  namespace <- asNamespace("FielDHub")
  calls <- function(code) {
    if (missing(code)) return(list())
    if (!is.call(code) && !is.pairlist(code)) return(list())
    if (is.call(code) && identical(code[[1]], as.name("app_read_upload"))) return(list(code))
    unlist(lapply(as.list(code), calls), recursive = FALSE)
  }
  for (entry in fieldhub_app_registry()) {
    uploads <- calls(body(get(entry$server, namespace)))
    expect_identical(length(uploads), 1L, info = entry$server)
  }
})
