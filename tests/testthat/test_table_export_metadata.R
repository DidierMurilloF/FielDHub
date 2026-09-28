test_that("table export records retain full parameters and selected location", {
  design <- RCBD(t = 4, reps = 2, l = 2, plotNumber = c(1, 101),
                  locationNames = c("West", "East"), seed = 42)
  before <- serialize(design, NULL)
  record <- table_export_record(design, "Plot numbers", location = 2L)
  expect_identical(record$schema_version, 1L)
  expect_identical(record$metadata, design$metadata)
  expect_identical(record$view, list(table = "Plot numbers", location = 2L))
  expect_identical(serialize(design, NULL), before)
  text <- table_export_text(record)
  expect_match(text, "FielDHub table export", fixed = TRUE)
  expect_match(text, design$metadata$package_version, fixed = TRUE)
  rebuilt <- new.env(parent = baseenv())
  eval(parse(text = text), rebuilt)
  expect_identical(rebuilt$record, record)
  expect_error(table_export_record(design, "Layout", location = 0), class = "fieldhub_input_error")
  expect_error(table_export_record(design, NA_character_), class = "fieldhub_input_error")
  expect_error(table_export_record(NULL, "Layout"), class = "fieldhub_input_error")
})

test_that("metadata chunks retain every character without reaching Excel's cell limit", {
  text <- paste0(strrep("0123456789", 7000), "\n", strrep("\U0001f33e", 10000))
  chunks <- table_export_chunks(text)
  expect_gt(length(chunks), 1L)
  expect_true(all(nchar(chunks, type = "chars") <= 16000L))
  expect_identical(paste0(chunks, collapse = ""), text)
})

test_that("all recorded design and allocation families round-trip table metadata", {
  for (name in names(catalogue)) {
    design <- catalogue_design(name)
    record <- table_export_record(design, "Recorded design")
    rebuilt <- new.env(parent = baseenv())
    eval(parse(text = table_export_text(record)), rebuilt)
    expect_identical(rebuilt$record, record, info = name)
  }
})

test_that("all client export buttons include metadata without evaluating supplied labels", {
  skip_if_not_installed("DT")
  skip_if_not_installed("htmltools")
  label <- "</pre><script>stop('not code')</script>"
  design <- CRD(t = c(label, "Other"), reps = 2, seed = 13)
  buttons <- app_table_export_buttons(design, "Entry layout", print = TRUE)
  expect_identical(vapply(buttons, `[[`, character(1), "extend"), c("copy", "excel", "print"))
  record <- new.env(parent = baseenv())
  eval(parse(text = buttons[[1L]]$messageTop), record)
  expect_identical(record$record$metadata, design$metadata)
  expect_identical(paste0(unlist(buttons[[2L]]$fieldhubMetadata), collapse = ""), buttons[[1L]]$messageTop)
  expect_s3_class(buttons[[2L]]$customize, "JS_EVAL")
  expect_match(buttons[[3L]]$messageTop, "&lt;script&gt;", fixed = TRUE)
  expect_false(grepl("<script>", buttons[[3L]]$messageTop, fixed = TRUE))
})

test_that("every client-side table export uses the shared metadata configuration", {
  # The augmented RCBD page draws its layouts as plots: its one table
  # export belonged to a grid output its page never showed
  expected <- c(Diagonal = 2L, diagonal_multiple = 3L, Optim = 2L,
                 RCBD_augmented = 0L, pREPS = 2L, multi_loc_preps = 3L,
                 sparse_allocation = 3L)
  specs <- names(fieldhub_design_specs())
  for (module in names(expected)) {
    if (module %in% specs) {
      # A page of the generic module exports its field grids and its
      # allocation table through app_spatial_grid()/app_spatial_table()
      spec <- design_app_spec(module)
      grids <- Filter(function(panel) identical(panel$type, "grid"), spec$panels)
      exports <- c(vapply(grids, `[[`, character(1), "export", USE.NAMES = FALSE), spec$setup$export)
      expect_length(exports, expected[[module]])
      next
    }
    code <- body(get(paste0("mod_", module, "_server"), asNamespace("FielDHub")))
    expect_identical(sum(all.names(code) == "app_table_export_buttons"), unname(expected[[module]]))
  }
  for (name in c("app_spatial_grid", "app_spatial_table")) {
    expect_identical(sum(all.names(body(get(name, asNamespace("FielDHub")))) == "app_table_export_buttons"), 1L)
  }
})
