# Expected rows transcribed from the original module UIs, not derived from
# app_sidebar_layout(). Each slash denotes the original 6/6 Bootstrap columns.
original_sidebar_rows <- list(
  CRD = c("t", "reps", "planter", "plot_start/location_names", "seed"),
  RCBD = c("t", "reps", "use_checks", "checks/rep_checks", "spread_checks", "checks_note",
    "l", "planter", "plot_start/continuous", "location_names", "seed"),
  LSD = c("t", "reps", "planter", "plot_start/location_names", "seed"),
  Latin_Rectangle = c("t", "nrows", "l", "planter", "plot_start/location_names", "seed"),
  FD = c("type", "setfactors", "reps/l", "plot_start/location_names", "planter", "seed"),
  SPD = c("type", "wp", "sp", "reps/l", "planter", "plot_start/location_names", "seed"),
  SSPD = c("type", "wp", "sp", "ssp", "reps/l", "planter", "plot_start/location_names", "seed"),
  STRIPD = c("Hplots/Vplots", "reps", "l", "planter", "plot_start/location_names",
    "randomizeH", "randomizeV", "seed"),
  IBD = c("t", "reps", "k", "l", "planter", "plot_start/location_names", "seed"),
  RowCol = c("t", "nrows/reps", "l", "planter", "plot_start/location_names", "seed"),
  Alpha_Lattice = c("t", "reps", "k", "l", "planter", "plot_start/location_names", "seed"),
  Square_Lattice = c("t", "reps", "k", "l", "planter", "plot_start/location_names", "seed"),
  Rectangular_Lattice = c("t", "reps", "k", "l", "planter", "plot_start/location_names", "seed"),
  Optim = c("checks", "rep_checks", "lines", "planter", "l/location_view",
    "plot_start/expt_name", "seed/location_names"),
  pREPS = c("repGens", "repUnits", "allow_fillers", "l/location_view", "planter",
    "plot_start/expt_name", "seed/location_names"),
  RCBD_augmented = c("repsExpt/random", "random_note", "repsStack", "lines", "checks/b",
    "l/location_view", "planter", "plot_start/expt_name", "seed/location_names"),
  Diagonal = c("lines", "checks", "l/location_view", "planter", "plot_start/expt_name", "seed/location_names"),
  diagonal_multiple = c("sameEntries", "lines", "blocks", "checks", "l/location_view", "stacked/planter",
    "plot_start/expt_name", "seed/location_names"),
  sparse_allocation = c("lines", "checks", "l/location_view", "copies_per_entry", "planter",
    "plot_start/expt_name", "seed/location_names"),
  multi_loc_preps = c("lines", "use_checks", "checks/rep_checks", "l/location_view", "copies_per_entry",
    "planter", "allow_fillers", "plot_start/expt_name", "seed/location_names")
)

test_that("each original input panel retains its own rows and every control exactly once", {
  for (spec in fieldhub_design_specs()) {
    before <- serialize(spec$controls, NULL)
    layout <- app_sidebar_layout(spec)
    rows <- Filter(function(row) !identical(row, "upload_file"), layout$rows)
    expect_identical(vapply(rows, paste, "", collapse = "/"), original_sidebar_rows[[spec$module]])
    expect_setequal(unlist(rows), vapply(spec$controls, `[[`, "", "id"))
    expect_false(anyDuplicated(unlist(rows)) > 0L)
    ui <- app_sidebar_ui(spec, shiny::NS("page"), toggle = "upload")
    expect_identical(serialize(spec$controls, NULL), before)
    query <- htmltools::tagQuery(ui)
    for (control in spec$controls) expect_equal(query$find(paste0("#page-", control$id))$length(), 1L)
    expect_equal(query$find(".fieldhub-input-row")$length(), sum(lengths(rows) == 2L))
    expect_equal(query$find(".fieldhub-input-row > .col-sm-6")$length(), 2L * sum(lengths(rows) == 2L))
    expect_equal(query$find(".fieldhub-control-row")$length(), 0L)
    upload <- app_upload_spec(spec$upload)
    upload_inputs <- query$find(paste0(".col-sm-", layout$upload_width, " input"))$selectedTags()
    expect_equal(sum(vapply(upload_inputs, function(tag) {
      identical(tag$attribs$id, paste0("page-", upload$file))
    }, TRUE)), 1L)
    seed <- query$find("#page-seed")$selectedTags()[[1]]
    expect_identical(seed$attribs$placeholder, "Automatic")
    expect_null(seed$attribs$value)
  }
})

test_that("restored labels and widget types do not alter parsers or automatic seeds", {
  for (module in c("Diagonal", "diagonal_multiple", "sparse_allocation")) {
    spec <- design_app_spec(module)
    checks <- Filter(function(x) x$id == "checks", spec$controls)[[1]]
    restored <- app_sidebar_control(checks, module)
    expect_identical(restored$type, "select")
    expect_identical(restored$parse, checks$parse)
    expect_equal(restored$choices, seq_len(if (module == "diagonal_multiple") 20 else 10))
    expect_identical(restored$parse("4", list()), restored$parse(4, list()))
    expect_error(restored$parse("garbage", list()), class = "fieldhub_input_error")
    expect_error(restored$parse("0", list()), class = "fieldhub_input_error")
    raw <- design_control_defaults(spec)
    raw$checks <- "4"
    expect_equal(read_design_controls(spec, raw)$checks, 4)
  }
  seed <- app_sidebar_control(ctl_seed(), "pREPS")
  expect_identical(seed$label, "Random Seed:")
  expect_null(seed$parse("", list()))
  expect_identical(app_sidebar_control(ctl_reps(1), "LSD")$label, "Input # of Full Reps (Squares):")
  expect_identical(app_sidebar_control(ctl_checks(4), "RCBD_augmented")$label, "Checks per Block:")
})

test_that("multi-location Yes/No controls still pass logical flags to the engine", {
  spec <- design_app_spec("multi_loc_preps")
  flag <- Filter(function(x) x$id == "use_checks", spec$controls)[[1]]
  expect_identical(flag$choices, c(Yes = TRUE, No = FALSE))
  for (value in list(TRUE, "TRUE")) expect_true(flag$parse(value, list()))
  for (value in list(FALSE, "FALSE")) expect_false(flag$parse(value, list()))
  expect_error(flag$parse("garbage", list()), class = "fieldhub_input_error")
  raw <- design_control_defaults(spec)
  raw$use_checks <- "FALSE"
  off <- read_design_controls(spec, raw)
  expect_false(off$use_checks)
  expect_null(off$checks)
  raw$use_checks <- "TRUE"
  on <- read_design_controls(spec, raw)
  expect_true(on$use_checks)
  expect_equal(on$checks, 3)
  expect_equal(on$rep_checks, c(8, 8, 8))
})

test_that("original gutters are scoped and native plot sizing is retained", {
  css <- paste(readLines(system.file("app/www/style.css", package = "FielDHub")), collapse = "\n")
  for (rule in c("#fieldhub-app .fieldhub-sidebar-controls .fieldhub-input-left",
    "padding-right: 28px;", "padding-left: 5px;", "#fieldhub-app .fieldhub-layout-image",
    "justify-content: flex-end;")) expect_match(css, rule, fixed = TRUE)
  expect_false(grepl("fieldhub-control-row|@container", css))
})
