library(testthat)
library(FielDHub)

# Plain tests of the generic design page (R/app_design_module.R) and the
# specs it renders (R/app_design_specs.R): what a page sends its engine,
# the HTML it builds, and the consistency of its controls. No Shiny server
# is started.

classic_modules <- c("CRD", "RCBD", "LSD", "FD", "SPD", "SSPD", "STRIPD", "IBD", "RowCol",
                     "Alpha_Lattice", "Square_Lattice", "Rectangular_Lattice")

#' Muffle only the onestage->twostage fallback row_column() may report
quiet_design <- function(expr) {
  withCallingHandlers(expr, fieldhub_design_warning = function(w) invokeRestart("muffleWarning"))
}

#' The design a page builds from raw control values (and an uploaded file),
#' as mod_design_server() does on Run!
page_design <- function(spec, raw, file = NULL) {
  shaped <- if (!is.null(file)) spec$upload_shape(file)
  controls <- read_design_controls(spec, shiny_shaped(raw), uploaded = !is.null(shaped))
  data <- spec$data(shaped, controls)
  quiet_design(do.call(spec$engine, spec$args(spec$values(controls, data), data)))
}

expect_same_design <- function(via_page, direct, info = NULL) {
  expect_identical(via_page$fieldBook, direct$fieldBook, info = info)
  expect_identical(via_page$metadata$parameters, direct$metadata$parameters, info = info)
}

page_defaults <- function(module, seed = 7) {
  raw <- design_control_defaults(design_app_spec(module))
  raw$seed <- seed
  raw
}

test_that("there is one page spec per classic design, in the registry's workflow order", {
  specs <- fieldhub_design_specs()
  expect_identical(names(specs), classic_modules)
  expect_identical(names(specs), names(fieldhub_classic_workflows()))
  registry <- fieldhub_app_registry()
  for (entry in registry) {
    if (!identical(entry$workflow_family, "classic")) next
    spec <- design_app_spec(entry$workflow)
    expect_identical(spec$module, entry$workflow)
    expect_identical(spec$engine, getExportedValue("FielDHub", entry$engine), info = entry$id)
    expect_identical(spec$args, get(paste0("design_args_", spec$module), asNamespace("FielDHub")),
                     info = entry$id)
    expect_identical(spec$workflow, classic_workflow_spec(spec$module))
    expect_identical(spec$layout, spec$workflow$layout)
    expect_false(spec$long_running)
    expect_true(is.function(spec$values) && is.function(spec$data) && is.function(spec$upload_shape))
    expect_no_error(app_upload_spec(spec$upload))
  }
  for (bad in list(NULL, NA_character_, "Diagonal", 1, c("CRD", "RCBD"))) {
    expect_error(design_app_spec(bad), class = "fieldhub_input_error")
  }
})

test_that("each page's defaults build the design a direct API call with the documented defaults builds", {
  direct <- list(
    CRD = CRD(t = 15, reps = 4, plotNumber = 101, locationNames = "FARGO", seed = 7),
    RCBD = RCBD(t = 18, reps = 3, l = 1, plotNumber = 101, continuous = TRUE,
                planter = "serpentine", locationNames = "FARGO", seed = 7),
    LSD = latin_square(t = 5, reps = 1, plotNumber = 101, planter = "serpentine",
                       locationNames = "FARGO", seed = 7),
    FD = full_factorial(setfactors = c(2, 2, 3), reps = 3, l = 1, type = 2, plotNumber = 101,
                        planter = "serpentine", locationNames = "FARGO", seed = 7),
    SPD = split_plot(wp = 4, sp = 3, reps = 3, l = 1, type = 2, plotNumber = 101,
                     locationNames = "FARGO", seed = 7),
    SSPD = split_split_plot(wp = 2, sp = 2, ssp = 5, reps = 3, l = 1, type = 2, plotNumber = 101,
                            locationNames = "FARGO", seed = 7),
    STRIPD = strip_plot(Hplots = 5, Vplots = 5, reps = 3, l = 1, planter = "serpentine",
                        plotNumber = 101, locationNames = "FARGO", seed = 7,
                        randomizeH = TRUE, randomizeV = TRUE),
    IBD = incomplete_blocks(t = 15, k = 3, reps = 4, l = 1, plotNumber = 101,
                            locationNames = "FARGO", seed = 7),
    RowCol = quiet_design(row_column(t = 42, nrows = 6, reps = 2, l = 1, plotNumber = 101,
                                     locationNames = "FARGO", seed = 7)),
    Alpha_Lattice = alpha_lattice(t = 36, k = 6, reps = 3, l = 1, plotNumber = 101,
                                  locationNames = "FARGO", seed = 7),
    Square_Lattice = square_lattice(t = 49, k = 7, reps = 3, l = 1, plotNumber = 101,
                                    locationNames = "FARGO", seed = 7),
    Rectangular_Lattice = rectangular_lattice(t = 30, k = 5, reps = 3, l = 1, plotNumber = 101,
                                              locationNames = "FARGO", seed = 7)
  )
  expect_identical(names(direct), classic_modules)
  for (module in classic_modules) {
    expect_same_design(page_design(design_app_spec(module), page_defaults(module)),
                       direct[[module]], info = module)
  }
})

test_that("pages with changed controls still build the API's design", {
  raw <- utils::modifyList(page_defaults("RCBD", seed = 4), list(
    t = 6, l = 2, planter = "cartesian", plot_start = "101,1001", location_names = "A,B",
    continuous = FALSE, use_checks = TRUE, checks = 2, rep_checks = "2", spread_checks = TRUE))
  expect_same_design(page_design(design_app_spec("RCBD"), raw),
    RCBD(t = 6, reps = 3, l = 2, planter = "cartesian", plotNumber = c(101, 1001),
         locationNames = c("A", "B"), continuous = FALSE, seed = 4,
         checks = 2, rep_checks = c(2, 2), spread_checks = TRUE))
  raw <- utils::modifyList(page_defaults("SPD", seed = 3), list(type = "1", planter = "cartesian", l = 2,
                                                                    plot_start = "101,1001",
                                                                    location_names = "A,B"))
  expect_same_design(page_design(design_app_spec("SPD"), raw),
    split_plot(wp = 4, sp = 3, reps = 3, l = 2, type = 1, plotNumber = c(101, 1001),
               locationNames = c("A", "B"), seed = 3))
  raw <- utils::modifyList(page_defaults("STRIPD"), list(randomizeV = FALSE, reps = 1))
  expect_same_design(page_design(design_app_spec("STRIPD"), raw),
    strip_plot(Hplots = 5, Vplots = 5, reps = 1, l = 1, plotNumber = 101, locationNames = "FARGO",
               seed = 7, randomizeH = TRUE, randomizeV = FALSE))
})

test_that("pages with an uploaded file build the design the module always sent", {
  entries <- data.frame(ENTRY = 1:12, NAME = paste0("G", 1:12), NOTE = "x")
  raw <- utils::modifyList(page_defaults("IBD"), list(t = NA, k = "3"))
  expect_same_design(page_design(design_app_spec("IBD"), raw, entries),
    incomplete_blocks(t = 12, k = 3, reps = 4, l = 1, plotNumber = 101, locationNames = "FARGO",
                      seed = 7, data = data.frame(ENTRY = 1:12, NAME = paste0("G", 1:12))))
  treatments <- data.frame(TREATMENT = c(paste0("T", 1:5), NA))
  raw <- utils::modifyList(page_defaults("CRD"), list(t = NA))
  expect_same_design(page_design(design_app_spec("CRD"), raw, treatments),
    CRD(reps = 4, plotNumber = 101, locationNames = "FARGO", seed = 7,
        data = data.frame(TREATMENT = paste0("T", 1:5), REP = 4L)))
  strips <- data.frame(Hplot = c("H1", "H2", "H3"), Vplot = c("V1", "V2", NA))
  raw <- utils::modifyList(page_defaults("STRIPD"), list(Hplots = NA, Vplots = NA))
  expect_same_design(page_design(design_app_spec("STRIPD"), raw, strips),
    strip_plot(Hplots = 3, Vplots = 2, reps = 3, l = 1, plotNumber = 101, locationNames = "FARGO",
               seed = 7, randomizeH = TRUE, randomizeV = TRUE, data = strips))
  levels <- data.frame(WHOLEPLOT = c("W1", "W2", "W3"), SUBPLOT = c("S1", "S2", NA))
  raw <- utils::modifyList(page_defaults("SPD"), list(wp = NA, sp = NA))
  expect_same_design(page_design(design_app_spec("SPD"), raw, levels),
    split_plot(reps = 3, l = 1, type = 2, plotNumber = 101, locationNames = "FARGO", seed = 7,
               data = levels))
  factors <- data.frame(FACTOR = c("A", "A", "B"), LEVEL = c("a0", "a1", "b0"))
  expect_error(page_design(design_app_spec("FD"), page_defaults("FD"), factors[1:2, ]),
               "More than one factor", class = "fieldhub_input_error")
})

test_that("choices of computed selects follow the entries, typed or uploaded", {
  spec <- design_app_spec("RowCol")
  nrows <- Filter(function(control) identical(control$id, "nrows"), spec$controls)[[1]]
  expect_identical(design_control_choices(spec, nrows, list(t = 42L))$selected, 6L)
  expect_identical(design_control_choices(spec, nrows, list(t = NA), data.frame(ENTRY = 1:12))$choices,
                   c(2L, 3L, 4L, 6L))
  spec <- design_app_spec("Square_Lattice")
  k <- Filter(function(control) identical(control$id, "k"), spec$controls)[[1]]
  expect_identical(design_control_choices(spec, k, list(t = 50L))$choices, "No Options Available")
  raw <- utils::modifyList(page_defaults("Square_Lattice"), list(t = 50, k = "No Options Available"))
  expect_error(page_design(spec, raw), "No options for this combination", class = "fieldhub_input_error")
})

test_that("the page HTML has every control, the shared buttons and no inline style", {
  # Styles the libraries write themselves: shiny's hidden file input and the
  # spinner's placeholder
  library_styles <- c('<input[^>]*class="shiny-input-file"[^>]*>', '<div style="height:400px" class="shiny-spinner-placeholder">')
  for (module in classic_modules) {
    spec <- design_app_spec(module)
    html <- as.character(mod_design_ui("x", spec))
    for (control in spec$controls) {
      expect_true(grepl(paste0('id="x-', control$id, '"'), html, fixed = TRUE), info = paste(module, control$id))
      if (!is.null(control$label)) {
        expect_true(grepl(htmltools::htmlEscape(control$label), html, fixed = TRUE),
                    info = paste(module, control$id))
      }
    }
    upload <- app_upload_spec(spec$upload)
    for (id in c("run", spec$workflow$ids[c("simulate", "field_book_download", "layout_download",
                                            "plot", "table")], spec$layout$output,
                 upload$toggle, upload$file, upload$sep)) {
      expect_true(grepl(paste0('id="x-', id, '"'), html, fixed = TRUE), info = paste(module, id))
    }
    for (text in c(spec$title, "Run!", "Simulate!", "Save experiment (ZIP)", "CSV + metadata (ZIP)",
                   "Import entries' list?", "Upload a CSV File:", "Field Layout", "Field Book")) {
      expect_true(grepl(text, html, fixed = TRUE), info = paste(module, text))
    }
    expect_identical(grepl("Summary Design", html, fixed = TRUE), spec$summary, info = module)
    expect_identical(grepl('id="x-summary"', html, fixed = TRUE), spec$summary, info = module)
    stripped <- html
    for (pattern in library_styles) stripped <- gsub(pattern, "", stripped, perl = TRUE)
    expect_false(grepl("style=", stripped, fixed = TRUE), info = module)
    # controls read only for generated entries hide while a file is used
    for (control in Filter(function(control) isTRUE(control$generated_only), spec$controls)) {
      expect_true(grepl(paste0("input.", upload$toggle, " != &#39;Yes&#39;"), html, fixed = TRUE), info = module)
    }
  }
})

test_that("the same concept has the same label, default and minimum on every page", {
  concepts <- fieldhub_control_concepts()
  # Where an engine accepts less than the concept's minimum, the page offers
  # the engine's minimum (checked against the engines below).
  engine_minimums <- c(CRD.t = 1, CRD.reps = 1, LSD.reps = 1, SPD.reps = 1, SSPD.reps = 1,
                       STRIPD.reps = 1, SPD.wp = 1, SSPD.wp = 1)
  seen <- character()
  for (module in classic_modules) {
    spec <- design_app_spec(module)
    ids <- vapply(spec$controls, `[[`, character(1), "id")
    expect_identical(anyDuplicated(ids), 0L, info = module)
    # every page has the shared controls, the seed last
    expect_true(all(c("planter", "plot_start", "location_names", "seed") %in% ids), info = module)
    expect_identical(ids[[length(ids)]], "seed", info = module)
    expect_identical("l" %in% ids, "l" %in% names(formals(spec$engine)), info = module)
    for (control in spec$controls) {
      concept <- concepts[[control$id]]
      info <- paste(module, control$id)
      expect_false(is.null(concept), info = info)
      expect_identical(control$label, concept$label, info = info)
      if ("value" %in% names(concept)) expect_identical(control$value, concept$value, info = info)
      if (!is.null(concept$choices)) expect_identical(unname(control$choices), concept$choices, info = info)
      key <- paste(module, control$id, sep = ".")
      if (key %in% names(engine_minimums)) {
        expect_identical(control$min, engine_minimums[[key]], info = info)
        seen <- c(seen, key)
      } else {
        expect_identical(control$min, concept$min, info = info)
      }
    }
  }
  expect_setequal(seen, names(engine_minimums))
})

test_that("every minimum a page offers is the smallest value its engine accepts", {
  for (module in classic_modules) {
    spec <- design_app_spec(module)
    computed <- any(vapply(spec$controls, function(control) !is.null(control$options), logical(1)))
    for (control in spec$controls) {
      if (!identical(control$type, "number") || is.null(control$min)) next
      info <- paste(module, control$id)
      raw <- page_defaults(module)
      if (!is.null(control$enabled_by)) raw[[control$enabled_by]] <- TRUE
      below <- replace(raw, control$id, list(control$min - 1))
      expect_error(page_design(spec, below), class = "fieldhub_input_error", info = info)
      # a treatment count at its minimum leaves no block size to choose
      if (computed && identical(control$id, "t")) next
      at <- replace(raw, control$id, list(control$min))
      built <- tryCatch(page_design(spec, at), error = conditionMessage)
      expect_true(is.data.frame(built$fieldBook), info = paste(info, built))
    }
    maximum <- Filter(function(control) !is.null(control$max), spec$controls)
    for (control in maximum) {
      above <- replace(page_defaults(module), control$id, list(control$max + 1))
      expect_error(page_design(spec, above), class = "fieldhub_input_error", info = module)
    }
  }
})

test_that("only a numeric locations control and whole-number defaults are offered", {
  for (module in classic_modules) {
    for (control in design_app_spec(module)$controls) {
      if (identical(control$type, "number")) {
        expect_true(is.numeric(control$value) && control$value >= control$min, info = paste(module, control$id))
      }
    }
  }
})
