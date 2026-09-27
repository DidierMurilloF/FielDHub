test_that("upload modules use exact file paths without partial-match warnings", {
  previous <- options(warnPartialMatchDollar = TRUE)
  on.exit(options(previous), add = TRUE)
  namespace <- asNamespace("FielDHub")
  calls <- function(code) {
    if (missing(code)) return(list())
    if (!is.call(code) && !is.pairlist(code)) return(list())
    if (is.call(code) && identical(code[[1]], as.name("load_file"))) return(list(code))
    unlist(lapply(as.list(code), calls), recursive = FALSE)
  }
  inFile <- data.frame(name = "entries.csv", datapath = "exact-upload-path")
  for (entry in fieldhub_app_registry()) {
    uploads <- calls(body(get(entry$server, namespace)))
    expect_identical(length(uploads), 1L, info = entry$server)
    for (upload in uploads) {
      expect_warning(path <- eval(upload$path), NA, info = entry$server)
      expect_identical(path, inFile$datapath, info = entry$server)
      absent <- data.frame(datapath_backup = "unrelated-file")
      expect_warning(missing_path <- eval(upload$path, list(inFile = absent)), NA,
                     info = entry$server)
      expect_true(is.null(missing_path), info = entry$server)
    }
  }
})
