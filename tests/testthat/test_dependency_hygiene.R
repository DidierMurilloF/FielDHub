library(FielDHub)

test_that("trivial dependencies stay out of Imports", {
  imports <- utils::packageDescription("FielDHub", fields = "Imports")
  packages <- trimws(sub("\\s*\\(.*$", "", strsplit(imports, ",")[[1L]]))

  expect_false("viridis" %in% packages)
  expect_false("numbers" %in% packages)
  expect_false("config" %in% packages)
  expect_false("shinythemes" %in% packages)
  expect_true("bslib" %in% packages)
  expect_true("ggplot2" %in% packages)
  expect_true("viridisLite" %in% packages)
})

test_that("the lightweight viridis scale preserves the existing heatmap palette", {
  points <- seq(0, 1, length.out = 11)
  expected <- c(
    "#440154", "#482575", "#414487", "#35608D", "#2A788E", "#21908D",
    "#22A884", "#43BE71", "#7AD151", "#BCDF27", "#FDE725"
  )

  expect_identical(fieldhub_viridis_scale()$palette(points), expected)
})

test_that("FielDHub has no dependency on blocksdesign", {
  root <- testthat::test_path("..", "..")
  r_dir <- file.path(root, "R")
  skip_if_not(file.exists(file.path(r_dir, "fct_CRD.R")), "The R/ sources are not available.")

  description <- read.dcf(file.path(root, "DESCRIPTION"))
  for (field in c("Depends", "Imports", "Suggests", "LinkingTo", "Enhances")) {
    if (!field %in% colnames(description)) next
    declared <- trimws(sub("\\s*\\(.*$", "", strsplit(description[1L, field], ",")[[1L]]))
    expect_false("blocksdesign" %in% declared, info = field)
  }

  namespace <- readLines(file.path(root, "NAMESPACE"))
  expect_false(any(grepl("^\\s*import(From)?\\(\\s*blocksdesign\\b", namespace)))

  src <- list.files(file.path(root, "src"), full.names = TRUE)
  src <- src[!grepl("[.](o|so|dll|dylib)$", src) & !dir.exists(src)]
  src_hits <- character()
  for (path in src) {
    code <- sub("//.*$", "", readLines(path, warn = FALSE))
    hit <- grep("blocksdesign", code)
    if (length(hit) > 0L) src_hits <- c(src_hits, paste0(basename(path), ":", hit))
  }
  expect_identical(src_hits, character())

  quotes <- c("\"blocksdesign\"", "'blocksdesign'")
  offenders <- character()
  for (path in list.files(r_dir, pattern = "[.][Rr]$", full.names = TRUE)) {
    data <- utils::getParseData(parse(path, keep.source = TRUE), includeText = TRUE)
    hits <- data[(data$token == "SYMBOL_PACKAGE" & data$text == "blocksdesign") |
                   (data$token == "STR_CONST" & data$text %in% quotes), ]
    if (nrow(hits) > 0L) {
      offenders <- c(offenders, paste0(basename(path), ":", hits$line1))
    }
  }
  expect_identical(offenders, character(), info = paste(offenders, collapse = "\n"))

  loaders <- c("requireNamespace", "loadNamespace", "getFromNamespace",
               "skip_if_not_installed", "library", "require")
  allowed <- c("test_dependency_hygiene.R", "test_app_dependencies.R",
               "test_design_args_parity.R")
  test_offenders <- character()
  tests <- list.files(testthat::test_path("."), pattern = "[.][Rr]$", full.names = TRUE)
  for (path in tests[!basename(tests) %in% allowed]) {
    data <- utils::getParseData(parse(path, keep.source = TRUE), includeText = TRUE)
    if (is.null(data)) next
    if (any(data$token == "SYMBOL_PACKAGE" & data$text == "blocksdesign")) {
      test_offenders <- c(test_offenders, paste0(basename(path), ": blocksdesign::"))
    }
    for (h in which(data$token == "STR_CONST" & data$text %in% quotes)) {
      # The call that holds the string: the function name sits in a child
      # expression of the call expression, next to the argument.
      parent <- data$parent[h]
      call_name <- ""
      while (parent != 0L) {
        kids <- data$id[data$parent == parent & data$token == "expr"]
        fn <- data$text[data$parent %in% kids & data$token == "SYMBOL_FUNCTION_CALL"]
        if (length(fn) > 0L) {
          call_name <- fn[1L]
          break
        }
        parent <- data$parent[data$id == parent]
      }
      # The value of a `.package =` argument: the string follows `.package`, `=`.
      before <- data[data$line1 <= data$line1[h] & data$terminal, ]
      before <- before[order(before$line1, before$col1), ]
      pos <- which(before$id == data$id[h])
      is_package_arg <- length(pos) == 1L && pos > 2L &&
        before$token[pos - 1L] == "EQ_SUB" && before$text[pos - 2L] == ".package"
      if (call_name %in% loaders || is_package_arg) {
        test_offenders <- c(test_offenders, paste0(basename(path), ":", data$line1[h]))
      }
    }
  }
  expect_identical(test_offenders, character(), info = paste(test_offenders, collapse = "\n"))

  ignore <- readLines(file.path(root, ".Rbuildignore"))
  expect_true("^tools$" %in% ignore)
})
