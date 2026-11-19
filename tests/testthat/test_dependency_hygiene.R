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

test_that("blocksdesign is used only from the allow-listed functions", {
  root <- testthat::test_path("..", "..")
  r_dir <- file.path(root, "R")
  skip_if_not(file.exists(file.path(r_dir, "fct_CRD.R")), "The R/ sources are not available.")
  allowed_functions <- c("build_row_column_onestage", "BlockEfficiencies")
  allowed_files <- "engine_optimizer_options.R"

  offenders <- character()
  for (path in list.files(r_dir, pattern = "[.][Rr]$", full.names = TRUE)) {
    if (basename(path) %in% allowed_files) next
    parsed <- parse(path, keep.source = TRUE)
    data <- utils::getParseData(parsed, includeText = TRUE)
    hits <- data[(data$token == "SYMBOL_PACKAGE" & data$text == "blocksdesign") |
                   (data$token == "STR_CONST" &
                      data$text %in% c("\"blocksdesign\"", "'blocksdesign'")), ]
    if (nrow(hits) == 0L) next
    # The enclosing top-level expression of each hit, named by its assignment.
    top <- data[data$parent == 0L, ]
    for (h in seq_len(nrow(hits))) {
      expr <- top[top$line1 <= hits$line1[h] & top$line2 >= hits$line1[h], ]
      expr_id <- expr$id[1L]
      first <- utils::head(data[data$parent == expr_id, ], 1L)
      name <- utils::getParseText(data, first$id)
      name <- sub("^`(.*)`$", "\\1", name)
      if (!name %in% allowed_functions) {
        offenders <- c(offenders, paste0(basename(path), ":", hits$line1[h], " in ", name))
      }
    }
  }
  expect_identical(offenders, character(), info = paste(offenders, collapse = "\n"))

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
})
