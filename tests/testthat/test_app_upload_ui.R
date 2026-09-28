library(FielDHub)

all_upload_keys <- c("crd", "rcbd", "alpha", "rect", "rcd", "square", "mdiag",
                     "sdiag", "sparse_allocation", "arcbd", "factorial", "ibd",
                     "lsd", "multi_loc_prep", "optim", "prep", "spd", "sspd",
                     "strip")

test_that("app_upload_ui() namespaces each module's own input ids", {
  for (design in all_upload_keys) {
    ns <- shiny::NS("mod")
    spec <- FielDHub:::app_upload_spec(design)
    html <- as.character(app_upload_ui(ns, design))
    expect_match(html, paste0('id="mod-', spec$toggle, '"'), fixed = TRUE, info = design)
    expect_match(html, paste0('id="mod-', spec$file, '"'), fixed = TRUE, info = design)
    expect_match(html, paste0('id="mod-', spec$sep, '"'), fixed = TRUE, info = design)
  }
})

test_that("app_upload_ui() uses the canonical labels for every module", {
  for (design in all_upload_keys) {
    html <- as.character(app_upload_ui(shiny::NS("mod"), design))
    expect_match(html, "Import entries' list?", fixed = TRUE, info = design)
    expect_match(html, "Upload a CSV File:", fixed = TRUE, info = design)
    expect_match(html, "Separator", fixed = TRUE, info = design)
    expect_match(html, "Comma", fixed = TRUE, info = design)
    expect_match(html, "Semicolon", fixed = TRUE, info = design)
    expect_match(html, "Tab", fixed = TRUE, info = design)
    # Regression: several modules said "Upload a csv File:" (lowercase csv).
    expect_no_match(html, "Upload a csv File", fixed = TRUE, info = design)
  }
})

test_that("app_upload_ui() drops the inline column spacing styles modules had", {
  for (design in all_upload_keys) {
    html <- as.character(app_upload_ui(shiny::NS("mod"), design))
    expect_no_match(html, "padding-right", fixed = TRUE, info = design)
    expect_no_match(html, "padding-left", fixed = TRUE, info = design)
  }
})

test_that("app_upload_dialog() shows the design's own example table and note", {
  html <- as.character(app_upload_dialog("crd"))
  expect_match(html, "Important message", fixed = TRUE)
  expect_match(html,
    "Please, follow the format shown in the following example. Make sure to upload a CSV file!",
    fixed = TRUE)
  expect_match(html, "Note that only the TREATMENT column is required.", fixed = TRUE)

  no_note <- as.character(app_upload_dialog("lsd"))
  expect_false(grepl("<h4></h4>", no_note, fixed = TRUE))
})

test_that("app_upload_dialog() renders for every module upload key", {
  for (design in all_upload_keys) {
    expect_no_error(as.character(app_upload_dialog(design)))
  }
})

test_that("app_upload_spec() validation_design falls back to the module key", {
  expect_identical(FielDHub:::app_upload_spec("crd")$validation_design, "crd")
  expect_identical(FielDHub:::app_upload_spec("multi_loc_prep")$validation_design, "sdiag")
  expect_identical(FielDHub:::app_upload_spec("sparse_allocation")$validation_design, "sdiag")
})

test_that("app_upload_spec() ids match the check_input() rule for the module's design", {
  for (design in all_upload_keys) {
    spec <- FielDHub:::app_upload_spec(design)
    rule <- upload_validation_rule(spec$validation_design)
    expect_false(is.null(rule), info = design)
  }
})
