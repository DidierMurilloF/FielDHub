# The spatial design pages of the app: the choices their steps offer and the
# checks of Run!, the tables and plots of their result tabs, their heatmaps,
# and the trait simulation dialogs and requests.

library(FielDHub)

# Inputs a cleared or malformed numeric control can deliver.
blank_inputs <- list(NULL, NA, NA_real_, numeric(), "", "a", -3, 0, 2.5, c(10, 20), Inf)

# One expectation that `f(value)` signals `class` for every value.
expect_all_classed <- function(f, values, label, class = "fieldhub_error") {
  ok <- vapply(values, function(value) inherits(tryCatch(f(value), error = identity), class),
               logical(1))
  expect(all(ok), sprintf("%s: %s does not raise %s", label, deparse(values[!ok][1L]), class))
}

# One expectation that every call in `calls` signals `class`.
expect_calls_classed <- function(calls, class = "fieldhub_input_error") {
  ok <- vapply(calls, function(f) inherits(tryCatch(f(), error = identity), class), logical(1))
  expect(all(ok), sprintf("case %s does not raise %s",
                          c(names(calls), seq_along(calls))[!ok][1L], class))
}

correlation_ids <- function(spec) {
  vapply(spatial_correlation_spec(spec$correlation_suffix), `[[`, character(1), "inputId")
}

# ---- field sizes offered -----------------------------------------------------

test_that("field sizes are offered with the first selected, or explained when none fits", {
  expect_identical(field_size_choices(c("2 x 3", "3 x 2"), "none"),
                   list(choices = c("2 \u00d7 3" = "2 x 3", "3 \u00d7 2" = "3 x 2"), selected = "2 x 3"))
  # Listed from the most square on, the one with fewer rows of two alike; the
  # size selected is still the first given
  expect_identical(field_size_choices(c("19 x 18", "18 x 19", "20 x 17", "17 x 20"), "none"),
                   list(choices = c("18 \u00d7 19" = "18 x 19", "19 \u00d7 18" = "19 x 18",
                                    "17 \u00d7 20" = "17 x 20", "20 \u00d7 17" = "20 x 17"),
                        selected = "19 x 18"))
  expect_error(field_size_choices(character(), "Nothing fits."), "Nothing fits.",
               class = "fieldhub_input_error")
})

test_that("optimized arrangements offer the field sizes of their plots", {
  offered <- optim_field_choices(312)
  expect_setequal(offered$choices, optimized_dimension_choices(312))
  expect_identical(offered$choices[1:4], c("13 \u00d7 24" = "13 x 24", "24 \u00d7 13" = "24 x 13",
                                           "12 \u00d7 26" = "12 x 26", "26 \u00d7 12" = "26 x 12"))
  expect_identical(offered$selected, "13 x 24")
  # a prime number of plots has no field of at least 4 x 4
  expect_error(optim_field_choices(307), "Please try a different number of treatments or checks.",
               class = "fieldhub_input_error")
  expect_all_classed(optim_field_choices, blank_inputs, "optim_field_choices")
  # typed as counts, it needs more entries than check plots
  expect_identical(optim_total_plots(280, c(8, 8, 8, 8)), 312)
  expect_error(optim_total_plots(32, c(8, 8, 8, 8)), "Number of lines should be greater",
               class = "fieldhub_input_error")
})

test_that("p-rep pages offer field sizes, labelled with their filler plots where allowed", {
  offered <- prep_field_choices(300, FALSE)
  expect_identical(offered$selected, "15 x 20")
  expect_setequal(offered$choices, prep_dimension_options(300)$value)
  # Listed from the most square on, the one with fewer rows of two alike
  expect_identical(offered$choices[1:4], c("15 \u00d7 20" = "15 x 20", "20 \u00d7 15" = "20 x 15",
                                           "12 \u00d7 25" = "12 x 25", "25 \u00d7 12" = "25 x 12"))
  # 301 plots: 7 x 43 without fillers, squarer fields with them. The size
  # selected is the one partially_replicated() chooses for these plots
  expect_identical(prep_field_choices(301, FALSE),
                   list(choices = c("7 \u00d7 43" = "7 x 43", "43 \u00d7 7" = "43 x 7"),
                        selected = prep_dimension_options(301)$value[1L]))
  with_fillers <- prep_field_choices(301, TRUE)
  expect_identical(with_fillers$choices[1:4],
                   c("7 \u00d7 43" = "7 x 43", "43 \u00d7 7" = "43 x 7",
                     "16 \u00d7 19 \u00b7 3 fillers" = "16 x 19", "19 \u00d7 16 \u00b7 3 fillers" = "19 x 16"))
  expect_identical(prep_field_choices(299, TRUE)$choices[["15 \u00d7 20 \u00b7 1 filler"]], "15 x 20")
  expect_identical(with_fillers$selected, "43 x 7")
  expect_error(prep_field_choices(307, FALSE), "Select 'Allow filler plots'", class = "fieldhub_input_error")
  expect_error(prep_field_choices(307, NA), "Select 'Allow filler plots'", class = "fieldhub_input_error")
  expect_all_classed(function(value) prep_field_choices(value, TRUE), blank_inputs, "prep_field_choices")
})

test_that("augmented RCBD pages offer the blocks and fields their entries fit", {
  offered <- augmented_block_choices(180, 4)
  expect_identical(offered, list(choices = unique(set_augmented_blocks(180, 4, start = 3)$b), selected = 3))
  fields <- augmented_field_choices(180, 4, 3)
  values <- c("12 x 16", "16 x 12", "8 x 24", "24 x 8", "6 x 32", "32 x 6",
              "4 x 48", "48 x 4", "3 x 64", "2 x 96", "96 x 2")
  expect_identical(fields, list(choices = stats::setNames(values, gsub("x", "\u00d7", values)),
                                selected = "12 x 16"))
  expect_identical(squarest_first(c("15 x 2", "3 x 10", "2 x 15", "6 x 5", "10 x 3", "5 x 6")),
                   c("5 x 6", "6 x 5", "3 x 10", "10 x 3", "2 x 15", "15 x 2"))
  expect_identical(squarest_first(character(0)), character(0))
  expect_error(augmented_field_choices(180, 4, 999), "No field size holds 999 blocks",
               class = "fieldhub_input_error")
  expect_all_classed(function(v) augmented_block_choices(v, 4), blank_inputs, "blocks: lines")
  expect_all_classed(function(v) augmented_block_choices(180, v), blank_inputs, "blocks: checks")
  expect_all_classed(function(v) augmented_field_choices(v, 4, 3), blank_inputs, "fields: lines")
  expect_all_classed(function(v) augmented_field_choices(180, 4, v), blank_inputs, "fields: blocks")
  expect_all_classed(function(v) augmented_lines(v, 4), blank_inputs, "augmented_lines")
  expect_identical(c(augmented_lines(180, 4), augmented_lines(NULL, 4, data.frame(ENTRY = 1:20))),
                   c(180, 16))
  expect_error(augmented_lines(NULL, 4, data.frame(ENTRY = 1:11)), "At least ten treatments",
               class = "fieldhub_input_error")
})

test_that("diagonal pages offer fields, then percentages of checks with the API default selected", {
  fields <- diagonal_field_choices(287, 287, 1:4)
  expect_setequal(fields$choices, diagonal_dimension_choices(287, 1:4))
  expect_identical(fields$choices[1:3], c("18 \u00d7 18" = "18 x 18", "18 \u00d7 19" = "18 x 19",
                                          "19 \u00d7 18" = "19 x 18"))
  expect_identical(fields$selected, "18 x 18")
  expect_error(diagonal_field_choices(2, 2, 1:4), "Insufficient number of entries provided!",
               class = "fieldhub_input_error")
  percents <- diagonal_percent_choices(18, 18, 1:4, 291)
  expect_identical(percents$selected, utils::tail(percents$choices, 1L))
  expect_identical(percents$choices, as.numeric(percents$table[["Percentage of Checks"]]))
  expect_error(diagonal_percent_choices(5, 5, 1:4, 291), "does not fit", class = "fieldhub_input_error")
  expect_all_classed(function(v) diagonal_field_choices(v, 287, 1:4), blank_inputs, "fields: lines")
  expect_all_classed(function(v) diagonal_field_choices(287, v, 1:4), blank_inputs, "fields: entries")
  expect_all_classed(function(v) diagonal_percent_choices(v, 18, 1:4, 291), blank_inputs, "percent: rows")
  expect_all_classed(function(v) diagonal_percent_choices(18, 18, 1:4, v), blank_inputs, "percent: entries")
})

test_that("multi-location p-rep pages offer copies per entry and a field size per location", {
  expect_identical(prep_copies_choices(6), list(choices = 7:11, selected = 8))
  expect_identical(prep_copies_choices(2), list(choices = 3L, selected = 3))
  expect_error(prep_copies_choices(1), "at least 2 locations", class = "fieldhub_input_error")
  expect_all_classed(prep_copies_choices, blank_inputs, "prep_copies_choices")
  sizes <- prep_location_field_choices(c(300, 307), FALSE)
  expect_identical(sizes$selected, list("15 x 20", character()))
  expect_length(sizes$choices[[2]], 0L)
  upload <- data.frame(ENTRY = 1:12, NAME = paste0("N", 1:12))
  expect_identical(list(multi_prep_entries(100, NULL, 3), multi_prep_entries(10, 2, 3, upload)$names,
                        multi_prep_entries(12, NULL, 3, upload)$names),
                   list(list(names = NULL), paste0("N", 3:12), paste0("N", 1:12)))
  expect_error(multi_prep_entries(11, 2, 3, upload), "does not match", class = "fieldhub_input_error")
  expect_error(multi_prep_entries(10, 2, 1), "at least 2 locations", class = "fieldhub_input_error")
  expect_identical(check_numeric_entries_upload(upload), upload)
  expect_error(check_numeric_entries_upload(transform(upload, ENTRY = as.character(ENTRY))),
               "should be numeric", class = "fieldhub_input_error")
})

# ---- entries, typed or uploaded ----------------------------------------------

test_that("an uploaded REPS column must hold whole numbers", {
  entries <- data.frame(ENTRY = 1:3, NAME = c("A", "B", "C"), REPS = c(2L, 1L, 1L))
  expect_identical(check_reps_upload(entries), entries)
  bad <- lapply(list(c(2, 1, 1), c("2", "1", "1"), factor(c(2, 1, 1))), function(reps) {
    entries$REPS <- reps
    function() check_reps_upload(entries)
  })
  expect_calls_classed(bad)
})

test_that("diagonal pages read their entries and checks, typed or uploaded", {
  expect_identical(diagonal_entries(287, 4),
                   list(checks_entries = 1:4, entries = 291, field_entries = 287, layout = NULL))
  entries <- data.frame(ENTRY = 11:30, NAME = paste0("E", 1:20))
  read <- diagonal_entries(NULL, 3, entries)
  expect_identical(read[c("checks_entries", "entries", "field_entries")],
                   list(checks_entries = c(11, 12, 13), entries = 20L, field_entries = 20L))
  expect_identical(upload_check_entries(entries[c(3, 1, 2, 4:20), ], 3), c(11, 12, 13))
  bad <- lapply(list(entries[c(1, 5, 2:4, 6:20), ], transform(entries, ENTRY = c("a", ENTRY[-1])),
                     entries[1:3, ]), function(b) function() upload_check_entries(b, 3))
  expect_calls_classed(bad)
})

test_that("multiple diagonal pages lay out their entries by experiment, checks first", {
  read <- multiple_diagonal_entries(60, c(30, 30), 2, FALSE)
  expect_identical(read[c("checks_entries", "entries", "field_entries")],
                   list(checks_entries = 1:2, entries = 62, field_entries = 60))
  expect_identical(read$layout$BLOCK, c("ALL", "ALL", rep(c("1", "2"), each = 30)))
  upload <- data.frame(ENTRY = 1:62, NAME = paste0("E", 1:62))
  read <- multiple_diagonal_entries(NULL, c(30, 30), 2, TRUE, upload)
  expect_identical(read$field_entries, 62L)
  expect_identical(names(read$layout), c("ENTRY", "NAME", "BLOCK"))
  expect_error(multiple_diagonal_entries(61, c(30, 30), 2, FALSE), "must add up",
               class = "fieldhub_input_error")
  expect_error(multiple_diagonal_entries(NULL, c(30, 31), 2, FALSE, upload), "does not match",
               class = "fieldhub_input_error")
  expect_error(multiple_diagonal_entries(40, c(20, 20), 2, FALSE), "Larger field size",
               class = "fieldhub_input_error")
  expect_error(multiple_diagonal_entries(60, c(20, 40), 2, TRUE), "same size",
               class = "fieldhub_input_error")
})

test_that("sparse allocation pages check their locations, entries and uploaded list", {
  expect_identical(sparse_entries(100, 3, 4), list(checks_entries = c(101, 102, 103), names = NULL))
  upload <- data.frame(ENTRY = 1:63, NAME = c("C1", "C2", "C3", paste0("L", 4:63)))
  expect_identical(sparse_entries(60, 3, 3, upload),
                   list(checks_entries = c(1, 2, 3), names = paste0("L", 4:63)))
  expect_error(sparse_entries(100, 3, 2), "at least 3 locations", class = "fieldhub_input_error")
  expect_error(sparse_entries(59, 3, 3), "at least 60 entries", class = "fieldhub_input_error")
  expect_error(sparse_entries(61, 3, 3, upload), "does not match", class = "fieldhub_input_error")
})

# ---- result tables -----------------------------------------------------------

test_that("field grids read as the field, numbered from its bottom row", {
  grid <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE)
  view <- field_grid_view(grid, highlight = c(1, 6), colours = spatial_highlight_colours("checks", 2))
  expect_identical(dimnames(view$data), list(c("2", "1"), c("V1", "V2", "V3")))
  expect_identical(view$data$V1, c(1, 4))
  fillers <- matrix(c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE), nrow = 2, byrow = TRUE)
  expect_identical(field_grid_view(grid, fillers = fillers)$data$V3, c("3", "Filler"))
  expect_calls_classed(lapply(list(NULL, 1:3, "x"), function(bad) function() field_grid_view(bad)))
})

test_that("entry lists show their names and entries as filterable factors", {
  entries <- entry_list_view(data.frame(ENTRY = 1:2, NAME = c("A", "B"), REPS = c(2L, 1L)),
                             c("ENTRY", "NAME", "REPS"))
  expect_true(all(vapply(entries, is.factor, logical(1))))
  expect_identical(levels(entry_list_view(list(ENTRY = 2:1, NAME = c("b", "a")))$NAME), c("a", "b"))
  view <- location_entries_view(list(LOC1 = data.frame(ENTRY = 1:2, NAME = c("a", "b"), X = 0),
                                     LOC2 = data.frame(ENTRY = 3L, NAME = "c", X = 0)))
  expect_identical(names(view), c("LOCATION", "ENTRY", "NAME"))
  expect_identical(as.character(view$LOCATION), c("LOC1", "LOC1", "LOC2"))
  expect_true(all(vapply(view, is.factor, logical(1))))
})

test_that("the checks table of a diagonal location lists each check and its plots", {
  design <- diagonal_arrangement(nrows = 18, ncols = 18, lines = 287, checks = 4, seed = 1)
  view <- diagonal_checks_view(design, 1)
  expect_identical(names(view), c("ENTRY", "NAME", "TIMES"))
  expect_identical(view$ENTRY, design$infoDesign$entry_checks[[1]])
  expect_equal(sum(view$TIMES), sum(design$fieldBook$CHECKS != 0))
})

test_that("the Design block shows the chosen field and spacing reports show each location's own measures", {
  # what a reader sees: the text of the page, without its markup
  text <- function(html) trimws(gsub("[[:space:]]+", " ", gsub("<[^>]+>", " ", as.character(html))))
  # smallest Euclidean distance between two plots of the same entry, from the field book alone
  min_same <- function(plots) {
    sprintf("%.2f", min(unlist(lapply(split(plots, plots$ENTRY), function(d) stats::dist(cbind(d$ROW, d$COLUMN))))))
  }
  spec <- design_app_spec("pREPS")
  raw <- utils::modifyList(design_control_defaults(spec),
                           list(l = 2, location_names = "North,South", plot_start = "1,1001", seed = 7))
  seen <- new.env()
  shiny::testServer(mod_design_server, args = list(spec = spec), {
    do.call(session$setInputs, raw)
    session$setInputs(run = 1, dimensions = "15 x 20")
    seen$facts <- text(output$design_facts$html)
    session$setInputs(randomize = 1)
    session$setInputs(location_view = "1")
    for (i in 1:5) { session$flushReact(); later::run_now(0.02) }
    seen$report <- text(output$design_report$html)
    seen$book <- session$returned$design()$fieldBook
  })
  sites <- split(seen$book, factor(seen$book$LOCATION, levels = unique(seen$book$LOCATION)))
  unoptimized <- diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4, seed = 8)
  checks <- unoptimized$fieldBook[unoptimized$fieldBook$CHECKS != 0, ]
  cases <- list(
    # before Randomize!, the field chosen in the select and the fillers it leaves
    "p-rep Design block" = list(seen$facts, "Field 15 \u00d7 20 Fillers 0 Locations 2"),
    # each location's minimum, in the order of the locations
    "p-rep replicate spacing" = list(seen$report, paste0("Replicate spacing .*",
      paste("Minimum same-entry distance", vapply(sites, min_same, ""), collapse = " .*"))),
    # without the check search there is one value per measure, not a before and an after
    "unoptimized check spacing" = list(text(app_design_report("check_spacing", unoptimized)),
                                       paste("Optimization Off", ".*Measure Value Minimum same-check distance",
                                             min_same(checks)))
  )
  found <- vapply(cases, function(case) grepl(case[[2]], case[[1]]), logical(1))
  bad <- c(which(!found), 1L)[1L]
  expect(all(found), sprintf("case %s: '%s' not shown in '%s'", names(cases)[bad], cases[[bad]][[2]],
                             cases[[bad]][[1]]))
})

test_that("multiple diagonal pages show the entries per experiment and each experiment's plots", {
  entries <- data.frame(ENTRY = 1:7, BLOCK = c("ALL", "1", "1", "2", "2", "2", "2"))
  expect_identical(block_frequency_view(entries),
                   data.frame(`SUB-BLOCKS` = factor(c("1", "2", "ALL")), FREQUENCY = c(2L, 4L, 1L),
                              check.names = FALSE))
  design <- diagonal_arrangement(nrows = 19, ncols = 18, lines = 300, checks = 4, kindExpt = "DBUDC",
                                 blocks = c(100, 120, 80), exptName = c("E1", "E2", "E3"), l = 2,
                                 seed = 3)
  view <- experiment_grid_view(design, 2)
  expect_identical(view$highlight, c("E1", "E2", "E3"))
  expect_identical(dim(view$data), c(19L, 18L))
  expect_error(experiment_grid_view(design, 3), "no field layout", class = "fieldhub_input_error")
})

test_that("allocation tables add the copies of each entry and the totals", {
  allocation <- list(allocation = data.frame(LOC1 = c(1, 0, 1), LOC2 = c(1, 1, 0)))
  view <- allocation_view(allocation, c("a", "b", "c"))
  expect_identical(rownames(view), c("a", "b", "c", "Total"))
  expect_identical(view[c("LOC1", "Copies")], data.frame(LOC1 = c(1, 0, 1, 2), Copies = c(2, 1, 1, 4),
                                                         row.names = c("a", "b", "c", "Total")))
  averaged <- allocation_view(allocation, c("a", "b", "c"), average = TRUE)
  expect_identical(averaged$Avg, c(1, 0.5, 0.5, NA))
})

test_that("a Randomize! is ready once its design is built, and explains a failure", {
  waiting <- list(ready = FALSE, problem = NULL)
  design <- diagonal_arrangement(nrows = 18, ncols = 18, lines = 287, checks = 4, seed = 1)
  failed <- tryCatch(fieldhub_abort("The field dimensions do not fit the entries."), error = function(e) e)
  expect_identical(
    list(spatial_randomize_state(FALSE, NULL), spatial_randomize_state(TRUE, NULL),
         spatial_randomize_state(TRUE, design), spatial_randomize_state(FALSE, design),
         spatial_randomize_state(TRUE, failed),
         spatial_randomize_state(TRUE, simpleError("subscript out of bounds"))),
    list(waiting, waiting, list(ready = TRUE, problem = NULL), waiting,
         list(ready = FALSE, problem = "The field dimensions do not fit the entries."),
         list(ready = FALSE, problem = "Unexpected problem: subscript out of bounds")))
  # the sparse page's last check of a design turns a missing field book into that problem
  rejected <- tryCatch(design_app_spec("sparse_allocation")$accept(list(fieldBook = NULL)),
                       error = function(e) e)
  expect_match(spatial_randomize_state(TRUE, rejected)$problem, "do not fit the entries")
})

# ---- layout images and their CSV ---------------------------------------------

test_that("plot toolbars have exactly three consistent namespaced downloads", {
  for (csv_id in c("layout_csv", "downloadCsv.rcbd")) {
    tools <- app_plot_tools(shiny::NS("page"), "layout", csv_id)
    buttons <- htmltools::tagQuery(tools)$find("a.shiny-download-link")$selectedTags()
    expect_identical(unname(vapply(buttons, function(x) x$attribs$id, "")),
                     paste0("page-", c("layout_png", "layout_pdf", csv_id)))
    html <- as.character(tools)
    labels <- c("PNG (300 dpi)", "PDF (vector)", "Layout CSV")
    expect(all(vapply(labels, grepl, logical(1), html, fixed = TRUE)),
           sprintf("toolbar %s lacks a download label", csv_id))
  }
})

test_that("augmented RCBD images and CSVs match the selected native plot", {
  design <- catalogue_design("RCBD_augmented_two_locations")
  for (location in 1:2) {
    view <- checked_layout_view(design, location = location)
    for (panel in design_app_spec("RCBD_augmented")$panels) {
      info <- paste("location", location, panel$label)
      plot <- panel$view(design, location, list())
      expected <- if (panel$label == "PLOT") view$out_layoutPlots else view$out_layout
      expect_equal(ggplot2::ggplot_build(plot)$data, ggplot2::ggplot_build(expected)$data, info = info)
      csv <- app_plot_grid_data(plot, panel$label)
      cells <- match(paste(rep(csv$ROW, ncol(csv) - 1L),
                           rep(seq_len(ncol(csv) - 1L), each = nrow(csv))),
                     paste(plot$data$ROW, plot$data$COLUMN))
      expect_identical(as.character(as.matrix(csv[, -1L])), as.character(plot$data[[panel$label]][cells]),
                       info = info)
    }
  }
})

test_that("all spatial images match plot() and CSV cells without mutating designs", {
  designs <- list(
    pREPS = partially_replicated(nrows = c(6, 7), ncols = c(7, 6), l = 2,
      repGens = c(5, 31), repUnits = c(2, 1), allow_fillers = TRUE,
      plotNumber = c(101, 1001), locationNames = c("West", "East"), seed = 16),
    multi_loc_preps = catalogue_design("multi_location_prep"),
    Optim = catalogue_design("optimized_arrangement"),
    Diagonal = diagonal_arrangement(nrows = 18, ncols = 18, lines = 287, checks = 4,
      l = 2, plotNumber = c(101, 1001), locationNames = c("West", "East"), seed = 1),
    diagonal_multiple = catalogue_design("diagonal_blocks_row"),
    sparse_allocation = catalogue_design("sparse_allocation")
  )
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (module in names(designs)) {
    design <- designs[[module]]
    before <- serialize(design, NULL)
    set.seed(93)
    rng <- .Random.seed
    panels <- design_app_spec(module)$panels
    for (location in seq_along(field_book_locations(design$fieldBook))) {
      book <- location_books(design)[[location]]
      for (panel in panels) {
        plot <- panel$view(design, location, list())
        label <- switch(panel$id, field_layout = "ENTRY", plot_numbers = "PLOT", expt_layout = "EXPT")
        expected <- book[[label]]
        if (label == "ENTRY") expected <- ifelse(book$TREATMENT == "Filler", "Filler", book$ENTRY)
        built <- ggplot2::ggplot_build(plot)$data
        text <- Filter(function(layer) "label" %in% names(layer) && any(layer$size > 0), built)[[1L]]
        public <- plot(design, l = location, text.string = label)$p
        csv <- app_spatial_plot_data(panel, design, location, list())
        old <- panel$grid(design, location, list())$data
        cells <- match(paste(rep(csv$ROW, ncol(csv) - 1L),
                             rep(seq_len(ncol(csv) - 1L), each = nrow(csv))),
                       paste(book$ROW, book$COLUMN))
        ok <- c(
          labels = identical(as.character(text$label), as.character(expected)),
          coordinates = isTRUE(all.equal(cbind(as.numeric(text$x), as.numeric(text$y)),
                                         cbind(as.numeric(book$COLUMN), as.numeric(book$ROW)))),
          same_as_plot = isTRUE(all.equal(built, ggplot2::ggplot_build(public)$data)),
          csv_rows = identical(csv$ROW, as.integer(rownames(old))),
          csv_grid = isTRUE(all.equal(as.matrix(csv[, -1, drop = FALSE]), unname(as.matrix(old)),
                                      check.attributes = FALSE)),
          csv_cells = !anyNA(cells) &&
            identical(as.character(as.matrix(csv[, -1L, drop = FALSE])), as.character(expected[cells])))
        expect(all(ok), sprintf("%s location %d %s: %s differs", module, location, panel$id,
                                names(ok)[!ok][1L]))
      }
    }
    expect_true(identical(serialize(design, NULL), before) && identical(.Random.seed, rng),
                info = paste(module, "changed the design or the random stream"))
  }
})

test_that("dense preview labels fit without mutating native plots or download text", {
  design <- RCBD_augmented(lines = 180, checks = 4, b = 3, seed = 27)
  plot <- checked_layout_view(design)$out_layout
  original <- ggplot2::ggplot_build(plot)$data
  preview <- app_plot_preview(plot, list(width = 800, height = 500))
  fitted <- ggplot2::ggplot_build(preview)$data
  text <- which(vapply(original, function(layer) "label" %in% names(layer), TRUE))
  expect_length(text, 1L)
  expect_lt(max(fitted[[text]]$size), max(original[[text]]$size))
  expect_gte(min(fitted[[text]]$size), 1.1)
  columns <- c("label", "x", "y", "colour")
  expect_equal(fitted[[text]][columns], original[[text]][columns])
  expect_equal(ggplot2::ggplot_build(plot)$data, original)
  expect_lt(preview$theme$axis.text.x$size, 10)
})

test_that("preview dimensions are measured independently of the field's shape", {
  expect_identical(
    list(app_spatial_preview_size(list(width = 930, height = 540)),
         app_spatial_preview_size(list(width = 930.8, height = 540.2)),
         app_spatial_preview_size(list(width = 5000, height = 2500))),
    list(list(width = 930, height = 540), list(width = 930, height = 540),
         list(width = 1800, height = 900)))
  bad <- list(NULL, 0, -1, NA_real_, Inf, "800", c(1, 2), matrix(1), 1 + 1i)
  sizes <- c(list(NULL, c(width = 930, height = 540)),
             lapply(bad, function(value) list(width = value, height = 540)),
             lapply(bad, function(value) list(width = 930, height = value)))
  measured <- !vapply(lapply(sizes, app_spatial_preview_size), is.null, logical(1))
  expect(!any(measured), sprintf("size %s is not rejected", deparse(sizes[measured][1L])))
})

test_that("image sizes grow with the field but are bounded", {
  size <- function(rows, columns) app_spatial_plot_size(list(data = data.frame(ROW = rows, COLUMN = columns)))
  small <- size(1:5, 1:5)
  expect_identical(small, list(width = 800, height = 560))
  expect_gt(size(1:5, 36:40)$width, small$width)
  expect_gt(size(36:40, 1:5)$height, small$height)
  expect_identical(size(10000, 10000), list(width = 1800, height = 1800))
})

test_that("preview PNGs use 192 dpi and downloads retain quality", {
  skip_if_not(capabilities("png"))
  design <- catalogue_design("partially_replicated_fillers")
  plot <- design_app_spec("pREPS")$panels[[1]]$view(design, 1, list())
  file <- tempfile(fileext = ".png")
  pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(c(file, pdf)), add = TRUE)
  png_size <- function(file) {
    con <- file(file, "rb")
    on.exit(close(con))
    readBin(con, "raw", n = 16L)
    readBin(con, "integer", n = 2L, size = 4L, endian = "big")
  }
  size <- unlist(app_spatial_plot_file(plot, file, dpi = 192), use.names = FALSE)
  expect_equal(png_size(file), size * 2)
  preview <- app_spatial_preview_size(list(width = 930, height = 540))
  app_spatial_plot_file(plot, file, dpi = 192, size = preview)
  expect_equal(png_size(file), c(1860, 1080))
  app_spatial_plot_file(plot, file)
  expect_equal(png_size(file), floor(size * 300 / 96))
  app_spatial_plot_file(plot, pdf, "pdf")
  expect_identical(readChar(pdf, 4L, useBytes = TRUE), "%PDF")
})

# ---- heatmaps ----------------------------------------------------------------

heatmap_test_book <- function() {
  data.frame(LOCATION = rep(c("B", "A"), each = 4),
             ROW = rep(c(1L, 1L, 2L, 2L), 2), COLUMN = rep(c(1L, 2L, 1L, 2L), 2),
             TREATMENT = paste0("T", 1:8), ENTRY = 1:8,
             CHECKS = rep(c(1L, 0L, 0L, 0L), 2), YIELD = (1:8) / 3)
}

test_that("heatmap data selects locations and values by name, with its tooltip wording", {
  book <- heatmap_test_book()
  actual <- field_book_heatmap_data(book, "YIELD", selected = 2, include_checks = TRUE)
  expect_identical(as.character(actual$LOCATION), rep("A", 4))
  expect_identical(actual$YIELD, book$YIELD[5:8])
  expect_identical(actual$text[1], "Site: A\nRow: 1\nCol: 1\nTreatment: T5\nCheck: yes\nYIELD : 1.67")
  expect_true(is.factor(actual$ROW) && is.factor(actual$COLUMN))
  altered <- book[rev(names(book))]
  altered$NOTES <- "keep"
  reordered <- field_book_heatmap_data(altered, "YIELD", selected = 2, include_checks = TRUE)
  expect_identical(reordered[c(names(actual), "NOTES")], cbind(actual, NOTES = rep("keep", 4)))
  out <- field_book_heatmap_data(book, "YIELD", label_column = "ENTRY", label_title = "Entry",
                                 include_site = FALSE)
  expect_identical(out$text[1], "Row: 1\nCol: 1\nEntry: 1\nYIELD : 0.33")
  book$YIELD[2] <- NA_real_
  expect_true(is.na(field_book_heatmap_data(book, "YIELD")$YIELD[2]))
})

test_that("invalid heatmap selections and schemas give classed errors", {
  book <- heatmap_test_book()
  expect_all_classed(function(v) field_book_heatmap_data(book, "YIELD", v),
                     list(NULL, NA_real_, 0, 3, 1.5, "A", c(1, 2)), "selected", "fieldhub_input_error")
  expect_all_classed(function(v) field_book_heatmap_data(book, v),
                     list(NULL, NA_character_, "", "missing", c("YIELD", "ENTRY")), "response",
                     "fieldhub_input_error")
  expect_all_classed(function(v) field_book_heatmap_data(v, "YIELD"),
                     list(book[FALSE, ], book[names(book) != "ROW"],
                          transform(book, LOCATION = NA_character_), transform(book, ROW = NA_real_),
                          transform(book, YIELD = "bad"), transform(book, YIELD = Inf)),
                     "book", "fieldhub_input_error")
})

test_that("heatmap images and CSVs keep the response at its field coordinates", {
  book <- data.frame(LOCATION = rep(c("West", "East"), each = 4),
    ROW = rep(c(1, 1, 2, 2), 2), COLUMN = rep(c(1, 2, 1, 2), 2),
    TREATMENT = letters[1:8], YIELD = c(1:7, NA_real_))
  plot <- app_field_heatmap(book, "YIELD", selected = 2)
  expect_s3_class(plot, "ggplot")
  expect_equal(plot$data$YIELD, book$YIELD[5:8])
  csv <- app_plot_grid_data(plot, "YIELD")
  expect_identical(csv$ROW, 2:1)
  expect_equal(unname(as.matrix(csv[, -1L])), matrix(c(7, 5, NA, 6), nrow = 2))
  expect_identical(app_spatial_plot_size(plot), list(width = 800, height = 560))
  spatial <- app_spatial_heatmap(list(plot$data), "YIELD")
  expect_s3_class(spatial, "ggplot")
  expect_equal(app_plot_grid_data(spatial, "YIELD"), csv)
  expect_warning(ggplot2::ggplot_build(spatial), NA)
})

test_that("spatial heatmaps select simulation locations and columns by name", {
  data <- data.frame(ID = 1:4, ENTRY = c(1, 2, 3, 0),
                     ROW = factor(c(1, 1, 2, 2)), COLUMN = factor(c(1, 2, 1, 2)),
                     YIELD = c(1.234, 4.567, 2.111, NA), text = letters[1:4])
  second <- transform(data, YIELD = YIELD + 10)
  expect_identical(spatial_heatmap_data(list(data, second), "YIELD", 2), second)
  changed <- second[rev(names(second))]
  changed$NOTES <- "preserve"
  out <- spatial_heatmap_data(list(data, changed), "YIELD", 2)
  expect_identical(out[c(names(second), "NOTES")], cbind(second, NOTES = rep("preserve", 4)))
})

test_that("spatial heatmaps reject invalid selections and schema before plotting", {
  data <- data.frame(ROW = 1:2, COLUMN = 1L, YIELD = c(1, NA), text = c("a", "b"))
  expect_all_classed(function(v) spatial_heatmap_data(list(data), "YIELD", v),
                     list(NULL, 0, 2, 1.5, NA_real_, c(1, 1), "1"), "selected", "fieldhub_input_error")
  expect_all_classed(function(v) spatial_heatmap_data(list(data), v),
                     list(NULL, "", "missing", NA_character_, c("YIELD", "text")), "response",
                     "fieldhub_input_error")
  expect_all_classed(function(v) spatial_heatmap_data(v, "YIELD"),
                     list(NULL, list(), data, list(NULL), list(data[FALSE, ]),
                          list(data[c("ROW", "YIELD", "text")]), list(transform(data, ROW = NA_real_)),
                          list(transform(data, YIELD = Inf)), list(transform(data, YIELD = "invalid")),
                          list(transform(data, text = 1:2))),
                     "data", "fieldhub_input_error")
})

# ---- trait simulation --------------------------------------------------------

test_that("spatial controls name the axis of each correlation", {
  for (suffix in c(".O", ".DIAG", ".PREP")) {
    fields <- spatial_correlation_spec(suffix)
    expect_identical(vapply(fields, `[[`, "", "label"),
                     c(correlation_x = "Adjacent columns (within a row)",
                       correlation_y = "Adjacent rows (within a column)"), info = suffix)
  }
  expect_all_classed(spatial_correlation_spec, list(NULL, NA_character_, "", 1, c(".O", ".PREP")),
                     "suffix", "fieldhub_input_error")
})

test_that("simulation dialogs show their namespaced controls and use trait terminology", {
  skip_if_not_installed("shiny")
  ids <- c(trait = "trait", other = "other", mean = "mean", sd = "sd", submit = "ok")
  html <- as.character(app_simulation_modal(shiny::NS("example"), ids))
  wanted <- c(paste0('id="example-', ids, '"'), 'data-ns-prefix="example-"', "Trait name")
  expect_identical(wanted[!vapply(wanted, grepl, logical(1), html, fixed = TRUE)], character())
  for (module in names(fieldhub_spatial_workflows())) {
    spec <- spatial_workflow_spec(module)
    for (spatial in c(FALSE, TRUE)) {
      html <- as.character(app_spatial_simulation_modal(shiny::NS("example"), spec, spatial_available = spatial))
      ids <- c(spec$simulation_ids, if (spatial) correlation_ids(spec), spec$heatmap_checkbox)
      wanted <- c(paste0('id="example-', ids, '"'), "Trait name", "Trait mean", "Plot-to-plot SD")
      shown <- c(vapply(wanted, grepl, logical(1), html, fixed = TRUE),
                 old_label = !grepl("Input Trial Name:", html, fixed = TRUE),
                 correlation = grepl("Add spatial correlation", html, fixed = TRUE) == spatial)
      expect(all(shown), sprintf("%s (spatial %s): %s", module, spatial, names(shown)[!shown][1L]))
    }
  }
})

test_that("Shiny validates the trait mean and residual SD atomically", {
  expected <- list(mean = 100, residual_sd = 8, trait = "YIELD", spatial = FALSE)
  expect_identical(simulation_request(100, 8, "YIELD"), expected)
  expect_identical(simulation_request(100L, 8L, "Other", "plant height"),
                   modifyList(expected, list(trait = "plant height")))
  expect_identical(simulation_request(-3, 0, "HEIGHT")$mean, -3)
  set.seed(917)
  before <- .Random.seed
  bad <- list(NULL, numeric(), NA_real_, NaN, Inf, -Inf, c(1, 2), "1", TRUE, 1 + 1i, matrix(1))
  expect_all_classed(function(v) simulation_request(v, 8, "YIELD"), bad, "mean", "fieldhub_input_error")
  expect_all_classed(function(v) simulation_request(100, v, "YIELD"), c(bad, -1), "residual SD",
                     "fieldhub_input_error")
  expect_identical(.Random.seed, before)
})

test_that("trait names are checked against choices and the field book", {
  book <- data.frame(ID = 1L, PLOT = 101L, LOCATION = "A", YIELD = 6)
  expect_all_classed(function(v) simulation_request(100, 8, v),
                     list(NULL, character(), NA_character_, "", " ", "unknown", c("YIELD", "HEIGHT")),
                     "trait", "fieldhub_input_error")
  expect_all_classed(function(v) simulation_request(100, 8, "Other", v, field_book = book),
                     list(NULL, NA_character_, "", " ", 1, c("a", "b"), matrix("a"),
                          "ID", "PLOT", "LOCATION", "text"),
                     "other trait", "fieldhub_input_error")
  expect_error(simulation_request(100, 8, "YIELD", field_book = book),
               class = "fieldhub_input_error")
})

test_that("spatial requests validate coordinates and correlations before submission", {
  book <- data.frame(LOCATION = c("A", "A"), ROW = 1:2, COLUMN = 1L)
  request <- simulation_request(100, 8, "HEIGHT", field_book = book,
                                spatial = list(rho_col = -.9, rho_row = .9))
  expect_identical(request$spatial[c("rho_col", "rho_row")], list(rho_col = -.9, rho_row = .9))
  expect_calls_classed(list(
    no_coordinates = function() simulation_request(100, 8, "YIELD", spatial = TRUE),
    correlation_one = function() simulation_request(100, 8, "YIELD", field_book = book,
                                                    spatial = list(rho_col = 1)),
    repeated_plot = function() simulation_request(100, 8, "YIELD", field_book = book[c(1, 1), ],
                                                  spatial = TRUE)))
})

test_that("spatial workflow books preserve simulation records separately from display IDs", {
  design <- partially_replicated(nrows = 4, ncols = 4, repGens = c(8, 4), repUnits = c(1, 2),
                                 seed = 18, year = 2026, spread_reps = FALSE)
  design$fieldBook$ID <- seq_len(nrow(design$fieldBook)) + 100L
  book <- design$fieldBook
  settings <- list(mean = 6, residual_sd = .4, trait = "YIELD", spatial = TRUE)
  set.seed(816)
  before <- .Random.seed
  expect_identical(app_trait_simulation(design, book, seed = stop("unused")),
                   list(df = book, simulation = NULL))
  expected <- stats::simulate(design, seed = 14, mean = 6, residual_sd = .4, spatial = TRUE)
  for (renumber in c(FALSE, TRUE)) {
    actual <- app_trait_simulation(design, book, settings, 14, renumber_display = renumber)
    display <- expected$field_book
    if (renumber) display$ID <- seq_len(nrow(display))
    expect_identical(actual, list(df = display, simulation = expected), info = paste("renumber", renumber))
  }
  plain_display <- book
  plain_display$ID <- seq_len(nrow(book))
  expect_identical(app_trait_simulation(design, book, renumber_display = TRUE),
                   list(df = plain_display, simulation = NULL))
  expect_identical(.Random.seed, before)
  expect_error(app_trait_simulation(design, book, renumber_display = NA),
               class = "fieldhub_input_error")
})

test_that("classic and spatial Shiny workflows export the package simulation", {
  classic <- classic_workflow_spec("RCBD")
  spatial <- spatial_workflow_spec("Diagonal")
  classic_design <- RCBD(6, 3, nrows = 3, seed = 12)
  spatial_design <- diagonal_arrangement(15, 20, 270, 4, seed = 12)
  view <- checked_layout_view(classic_design, layout = 1, location = 1)
  expected <- function(design) {
    # The dialog's default correlations; spatial = TRUE would draw them
    stats::simulate(design, seed = 12, mean = 6, residual_sd = 0.4,
                    spatial = list(rho_row = 0.5, rho_col = 0.5))$field_book
  }
  simulation_inputs <- function(spec) {
    ids <- spec$simulation_ids
    stats::setNames(list("YIELD", 6, 0.4, TRUE, 1L),
      c(ids["trait"], ids["mean"], ids["sd"], simulation_spatial_id(ids), ids["submit"]))
  }
  shiny::testServer(function(input, output, session) {
    state <- app_classic_workflow(input, output, session,
      design = function() classic_design, layout = function() view,
      seed = function() 12, selected = function() 1L, spec = classic,
      simulation_ready = function() TRUE)
  }, {
    session$flushReact()
    do.call(session$setInputs, c(simulation_inputs(classic),
      stats::setNames(list("3"), classic$ids[["plot_type"]])))
    reference <- expected(classic_design)
    book <- state$book()$df
    expect_identical(book$YIELD, reference$YIELD[match(book$ID, reference$ID)])
    expect_true(is.data.frame(state$layout_data()$file))
    expect_true(file.exists(output[[classic$ids[["field_book_download"]]]]))
  })
  shiny::testServer(function(input, output, session) {
    state <- app_spatial_workflow(input, output, session,
      design = function() spatial_design, seed = function() 12,
      selected = function() 1L, visible = function() TRUE,
      simulation_ready = function() TRUE, book_ready = function() TRUE, spec = spatial)
  }, {
    session$flushReact()
    do.call(session$setInputs, simulation_inputs(spatial))
    expect_identical(state$book()$df$YIELD, expected(spatial_design)$YIELD)
    expect_true(file.exists(output[[spatial$ids[["download"]]]]) &&
                  file.exists(output[[paste0(spatial$ids[["heatmap"]], "_csv")]]))
  })
})

test_that("row-column simulation uses the physical layout and rejects experimental axis names", {
  spec <- classic_workflow_spec("RowCol")
  ids <- spec$simulation_ids
  x <- row_column(12, nrows = 3, reps = 3, seed = 1)
  view <- checked_layout_view(x, layout = 1, location = 1)
  physical <- x
  physical$fieldBook <- field_layout(x)
  reference <- stats::simulate(physical, seed = 1, mean = 6, residual_sd = 0.4,
                               spatial = list(rho_row = 0.5, rho_col = 0.5))$field_book
  shiny::testServer(function(input, output, session) {
    state <- app_classic_workflow(input, output, session,
      design = function() x, layout = function() view,
      seed = function() 1, selected = function() 1L, spec = spec,
      simulation_ready = function() TRUE)
  }, {
    session$flushReact()
    do.call(session$setInputs, setNames(list("YIELD", 6, 0.4, TRUE, 1L),
      c(ids[c("trait", "mean", "sd")], simulation_spatial_id(ids), ids["submit"])))
    accepted <- state$book()
    book <- accepted$df
    expect_identical(book$YIELD, reference$YIELD[match(book$ID, reference$ID)])
    do.call(session$setInputs, setNames(list("Other", "ROW_REP", 2L),
      ids[c("trait", "other", "submit")]))
    expect_identical(list(state$settings()$trait, state$book()), list("YIELD", accepted))
  })
})

# ---- the p-rep "distinct rows and columns" box -------------------------------

test_that("the pREPS box gives the design of the strict API rule", {
  spec <- design_app_spec("pREPS")
  controls <- list(repGens = c(40, 120), repUnits = c(2, 1), allow_fillers = FALSE,
                   rowcol_distinct = TRUE, planter = "serpentine", l = 1, plot_start = 101,
                   seed = 31, expt_name = "Expt1", location_names = "A")
  build <- function(controls) {
    values <- c(spec$values(controls, NULL), list(nrows = 10, ncols = 20))
    do.call(partially_replicated, design_args_pREPS(shiny_shaped(values)))
  }
  direct <- function(penalty) {
    partially_replicated(nrows = 10, ncols = 20, repGens = c(40, 120), repUnits = c(2, 1),
                         plotNumber = 101, seed = 31, exptName = "Expt1",
                         locationNames = "A", rowcol_penalty = penalty)
  }
  checked <- build(controls)
  expect_identical(checked$fieldBook, direct(Inf)$fieldBook)
  expect_identical(checked$infoDesign$shared_pairs, 0L)
  # unchecked, or a page without the box, gives the default rule
  default <- direct(0)$fieldBook
  expect_identical(build(utils::modifyList(controls, list(rowcol_distinct = FALSE)))$fieldBook, default)
  expect_identical(build(controls[setdiff(names(controls), "rowcol_distinct")])$fieldBook, default)
})

test_that("the multi-location p-rep box gives the design of the strict API rule", {
  values <- list(lines = 80, l = 4, copies_per_entry = 5, checks = 2, rep_checks = c(4, 4),
                 allow_fillers = TRUE, seed = 34, plot_start = 1)
  build <- function(box) {
    do.call(multi_location_prep, design_args_multi_loc_preps(c(values, list(rowcol_distinct = box))))
  }
  direct <- function(penalty) {
    multi_location_prep(lines = 80, l = 4, copies_per_entry = 5, checks = 2, rep_checks = c(4, 4),
                        allow_fillers = TRUE, seed = 34, plotNumber = c(1, 1001, 2001, 3001),
                        rowcol_penalty = penalty)$fieldBook
  }
  expect_identical(build(TRUE)$fieldBook, direct(Inf))
  expect_identical(build(FALSE)$fieldBook, direct(0))
})

test_that("both p-rep modules offer the boxes once, checked by default", {
  specs <- fieldhub_design_specs()
  for (module in c("pREPS", "multi_loc_preps")) {
    ids <- vapply(specs[[module]]$controls, `[[`, "", "id")
    values <- vapply(specs[[module]]$controls[ids %in% c("rowcol_distinct", "allow_fillers")],
                     function(control) isTRUE(control$value), logical(1))
    expect_identical(c(sum(ids == "rowcol_distinct"), sum(ids == "allow_fillers"), sum(values)),
                     c(1L, 1L, 2L), info = module)
  }
})
