# The design result object: its class, recorded metadata and element names,
# the structure reproduce_design() requires before it replays a saved result,
# print(), summary() and plot(), and the YEAR column.

# TRUE when `expr` signals an error of class `class`.
fails_with <- function(expr, class) {
  tryCatch({
    force(expr)
    FALSE
  }, error = function(e) inherits(e, class))
}

# Design identifier recorded by each design function
result_designs <- c(
  CRD = "crd", RCBD = "rcbd", latin_square = "latin_square", latin_rectangle = "latin_rectangle",
  full_factorial = "full_factorial", split_plot = "split_plot",
  split_split_plot = "split_split_plot", strip_plot = "strip_plot",
  incomplete_blocks = "incomplete_blocks", row_column = "row_column",
  square_lattice = "square_lattice", rectangular_lattice = "rectangular_lattice",
  alpha_lattice = "alpha_lattice", partially_replicated = "partially_replicated",
  multi_location_prep = "multi_location_prep", RCBD_augmented = "rcbd_augmented",
  diagonal_arrangement = "diagonal_arrangement", sparse_allocation = "sparse_allocation",
  optimized_arrangement = "optimized_arrangement", split_families = "split_families",
  augmented_row_column = "augmented_row_column"
)
design_entries <- Filter(function(name) catalogue[[name]]$fun %in% names(result_designs),
                         names(catalogue))

# Class, metadata and elements --------------------------------------------------

test_that("every design result has the class of its design", {
  wrong <- Filter(function(name) {
    !identical(class(catalogue_design(name)),
               c(paste0("fieldhub_", result_designs[[catalogue[[name]]$fun]]), "FielDHub"))
  }, design_entries)
  expect_identical(wrong, character(0))
})

test_that("every design result records how it was built", {
  version <- as.character(utils::packageVersion("FielDHub"))
  wrong <- Filter(function(name) {
    design <- catalogue_design(name)
    !identical(
      design$metadata[c("design", "schema_version", "seed", "rng_kind", "package_version")],
      list(design = result_designs[[catalogue[[name]]$fun]], schema_version = 1L,
           seed = design$infoDesign$seed, rng_kind = RNGkind(), package_version = version)
    )
  }, design_entries)
  expect_identical(wrong, character(0))
})

test_that("the seed in the metadata rebuilds the design", {
  design <- RCBD(t = 5, reps = 3)
  expect_identical(RCBD(t = 5, reps = 3, seed = design$metadata$seed)$fieldBook, design$fieldBook)
})

test_that("the metadata leaves the elements of the previous release's results in place", {
  design <- RCBD(t = 4, reps = 3, seed = 89076)
  expect_identical(names(design),
                   c("infoDesign", "layoutRandom", "plotNumber", "fieldBook", "metadata"))
})

# Structure required before a saved result is replayed -------------------------

test_that("replay requires each design's field-book columns", {
  missing_ok <- character(0)
  for (name in design_entries) {
    x <- catalogue_design(name)
    if (!is.data.frame(x$fieldBook)) next
    for (column in setdiff(names(x$fieldBook), c("ID", "LOCATION", "PLOT"))) {
      bad <- x
      bad$fieldBook[[column]] <- NULL
      if (!fails_with(reproduce_design(bad), "fieldhub_internal_error")) {
        missing_ok <- c(missing_ok, paste(name, column))
      }
    }
  }
  expect_identical(missing_ok, character(0))
})

test_that("replay requires field-book columns to be ordinary vectors without missing values", {
  accepted <- character(0)
  for (name in c("RCBD_two_locations", "latin_square", "full_factorial_rcbd",
                 "diagonal_single", "partially_replicated")) {
    x <- catalogue_design(name)
    n <- nrow(x$fieldBook)
    values <- list(list = rep(list(1), n), matrix = matrix(1, n, 1L),
                   missing = rep(NA_character_, n))
    for (column in setdiff(names(x$fieldBook), c("ID", "LOCATION", "PLOT", "CHECKS"))) {
      for (kind in names(values)) {
        bad <- x
        bad$fieldBook[[column]] <- values[[kind]]
        if (!fails_with(reproduce_design(bad), "fieldhub_internal_error")) {
          accepted <- c(accepted, paste(name, column, kind))
        }
      }
    }
  }
  expect_identical(accepted, character(0))
})

test_that("replay rejects results whose shared field-book keys or metadata are broken", {
  design <- RCBD(t = 4, reps = 2, seed = 1)
  allocation <- do_optim(design = "sparse", lines = 12, l = 3, copies_per_entry = 2,
                         checks = 2, add_checks = TRUE, seed = 38)
  edit <- function(x, f) f(x)
  cases <- list()
  # ID, LOCATION and PLOT are required, once each
  for (column in c("ID", "LOCATION", "PLOT")) local({
    key <- column
    cases[[paste("no", key)]] <<- edit(design, function(b) {b$fieldBook[[key]] <- NULL; b})
  })
  cases[["duplicate column names"]] <- edit(design, function(b) {
    b$fieldBook <- cbind(b$fieldBook, ID = b$fieldBook$ID); b})
  # ID and PLOT are finite numeric vectors
  numeric_bad <- list(character = rep("1", 8), missing = rep(NA_real_, 8), infinite = rep(Inf, 8),
                      complex = rep(1 + 1i, 8), matrix = matrix(1:8, ncol = 1))
  for (column in c("ID", "PLOT")) for (kind in names(numeric_bad)) local({
    key <- column
    value <- numeric_bad[[kind]]
    cases[[paste(key, kind)]] <<- edit(design, function(b) {b$fieldBook[[key]] <- value; b})
  })
  # LOCATION is a nonmissing atomic vector
  location_bad <- list(missing = rep(NA_character_, 8), list = rep(list("A"), 8),
                       matrix = matrix(1:8, ncol = 1))
  for (kind in names(location_bad)) local({
    value <- location_bad[[kind]]
    cases[[paste("LOCATION", kind)]] <<- edit(design, function(b) {b$fieldBook$LOCATION <- value; b})
  })
  # Designs and allocations need complete random-number and version records
  results <- list(design = design, allocation = allocation)
  rng_bad <- list(NULL, "Mersenne-Twister", c("", "Inversion", "Rejection"),
                  c(NA_character_, "Inversion", "Rejection"), c(" ", "Inversion", "Rejection"))
  version_bad <- list(NULL, "", " ", c("1.5.0", "1.5.1"), 1.5)
  seed_bad <- list(NULL, NA_real_, Inf, "38", numeric(), c(38, 39), 1 + 1i, .Machine$integer.max + 1)
  for (kind in names(results)) {
    for (i in seq_along(rng_bad)) local({
      value <- rng_bad[[i]]
      cases[[paste(kind, "rng_kind", i)]] <<- edit(results[[kind]], function(b) {
        b$metadata["rng_kind"] <- list(value); b})
    })
    for (i in seq_along(version_bad)) local({
      value <- version_bad[[i]]
      cases[[paste(kind, "package_version", i)]] <<- edit(results[[kind]], function(b) {
        b$metadata["package_version"] <- list(value); b})
    })
    # Unusable seeds are rejected even when every copy of the seed agrees
    for (i in seq_along(seed_bad)) local({
      value <- seed_bad[[i]]
      is_design <- kind == "design"
      cases[[paste(kind, "seed", i)]] <<- edit(results[[kind]], function(b) {
        b$metadata["seed"] <- list(value)
        b$metadata$parameters["seed"] <- list(value)
        if (is_design) b$infoDesign["seed"] <- list(value)
        b
      })
    })
    cases[[paste(kind, "duplicate metadata field")]] <- edit(results[[kind]], function(b) {
      b$metadata <- c(b$metadata, list(seed = 38)); b})
  }
  cases[["design seed disagrees with infoDesign"]] <- edit(design, function(b) {
    b$infoDesign$seed <- 39; b})
  accepted <- names(cases)[!vapply(cases, function(bad) {
    fails_with(reproduce_design(bad), "fieldhub_internal_error")
  }, logical(1))]
  expect_identical(accepted, character(0))
})

test_that("replay accepts extra user columns and established location identifiers", {
  design <- RCBD(t = 4, reps = 2, seed = 1)
  design$fieldBook$LOCATION <- rep("A", 8)
  design$fieldBook$NOTES <- NA_character_
  expect_s3_class(reproduce_design(design), "fieldhub_rcbd")
})

test_that("replay checks family-split entry tables and location totals", {
  x <- split_families(3, data.frame(ENTRY = 1:9, NAME = paste0("G", 1:9), FAMILY = "A"), seed = 38)
  cases <- list()
  for (name in c("rowsEachlist", "data_locations")) local({
    element <- name
    bad <- x
    bad[[element]] <- NULL
    cases[[paste("no", element)]] <<- bad
  })
  for (column in names(x$data_locations)) local({
    key <- column
    bad <- x
    bad$data_locations[[key]] <- NULL
    cases[[paste("no data_locations column", key)]] <<- bad
  })
  bad <- x
  bad$rowsEachlist$n[1] <- bad$rowsEachlist$n[1] + 1
  cases[["location totals disagree"]] <- bad
  bad <- x
  bad$data_locations$LOCATION[1] <- "unknown"
  cases[["unknown location"]] <- bad
  bad <- x
  bad$rowsEachlist$Location[1] <- bad$rowsEachlist$Location[2]
  cases[["duplicate location"]] <- bad
  accepted <- names(cases)[!vapply(cases, function(b) {
    fails_with(reproduce_design(b), "fieldhub_internal_error")
  }, logical(1))]
  expect_identical(accepted, character(0))
  # A split with more locations than entries is still valid
  small <- suppressWarnings(split_families(5, data.frame(ENTRY = 1:3, NAME = LETTERS[1:3], FAMILY = "A"),
                                           seed = 38))
  expect_identical(suppressWarnings(reproduce_design(small)), small)
})

# print(), summary() and plot() -------------------------------------------------

# Lines printed by print(summary(x)) between two numbered section headings.
summary_section <- function(x, from, to) {
  out <- utils::capture.output(print(summary(x)))
  start <- grep(from, out, fixed = TRUE)[1]
  end <- grep(to, out, fixed = TRUE)[1]
  out[(start + 1):(end - 1)]
}

# The structure written by the previous release: one FielDHub class and no
# metadata, after a serialization round trip.
legacy_roundtrip <- function(design) {
  legacy <- structure(unclass(design)[setdiff(names(design), "metadata")], class = "FielDHub")
  unserialize(serialize(legacy, NULL, version = 2))
}

test_that("print() and print(summary()) return the design invisibly", {
  # Regression: print.FielDHub() returned head() of the field book
  crd <- CRD(t = 4, reps = 3, seed = 1)
  utils::capture.output(printed <- withVisible(print(crd)))
  utils::capture.output(summarised <- withVisible(print(summary(crd))))
  expect_identical(list(printed$visible, printed$value, summarised$visible),
                   list(FALSE, crd, FALSE))
})

test_that("print() lists the design parameters without the design identifier", {
  base <- RCBD(t = 4, reps = 2, seed = 38)
  cases <- list(
    # id_design between other parameters is dropped by name, not position
    `identifier in the middle` = list(blocks = 3, id_design = base$infoDesign$id_design,
                                      seed = 38, custom = "keep"),
    `identifier only` = list(id_design = base$infoDesign$id_design)
  )
  for (name in names(cases)) {
    x <- base
    x$infoDesign <- cases[[name]]
    x$metadata$seed <- x$infoDesign$seed
    out <- utils::capture.output(print(x))
    start <- grep("Information on the design parameters:", out, fixed = TRUE)
    end <- grep("First observations", out, fixed = TRUE)
    shown <- out[(start + 1):(end - 1)]
    want <- utils::capture.output(str(cases[[name]][names(cases[[name]]) != "id_design"]))
    expect_identical(shown[nzchar(trimws(shown))], want, info = name)
  }
})

test_that("print(summary()) shows the data input of p-rep and optimized designs", {
  # Regression: summary() read x$data_entry, but these designs store x$dataEntry
  prep <- partially_replicated(nrows = 8, ncols = 8, repGens = c(50, 7),
                               repUnits = c(1, 2), planter = "cartesian", seed = 1)
  optim <- optimized_arrangement(nrows = 12, ncols = 10, lines = 100,
                                 rep_checks = 20, checks = 1:5, seed = 1)
  shows_data <- vapply(list(prep = prep, optim = optim), function(x) {
    any(grepl("data.frame", summary_section(x, "4. Structure", "5. Structure"), fixed = TRUE))
  }, logical(1))
  expect(all(shows_data), paste("no data input in the summary of", names(shows_data)[!shows_data]))
})

test_that("print(summary()) describes sparse and multi-location p-rep allocations", {
  # Regression: print.summary.FielDHub() had no branch for these designs
  sparse <- sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4, seed = 1234)
  sparse_text <- utils::capture.output(print(summary(sparse)))
  multi_prep <- multi_location_prep(lines = 80, l = 4, copies_per_entry = 5, checks = 2,
                                    rep_checks = c(4, 4), allow_fillers = TRUE, seed = 1)
  multi_text <- utils::capture.output(print(summary(multi_prep)))
  ok <- c(sparse_title = any(grepl("Sparse Allocation", sparse_text)),
          sparse_field_book = any(grepl("field book", sparse_text)),
          multi_title = any(grepl("Multi-Location Partially Replicated", multi_text)),
          multi_structure = any(grepl("data.frame",
                                      summary_section(multi_prep, "3. Structure", "4. Structure"))))
  expect(all(ok), paste("missing from the summary:", paste(names(ok)[!ok], collapse = ", ")))
})

test_that("plot() explains that split_families() results have no layout", {
  # Regression: plot() failed with "object of type 'closure' is not subsettable"
  set.seed(77)
  gen_list <- data.frame(ENTRY = 1:200, NAME = paste0("SB-", 1:200),
                         FAMILY = sample(1:20, size = 200, replace = TRUE))
  families <- split_families(l = 2, data = gen_list, seed = 5)
  expect_error(plot(families), class = "fieldhub_input_error")
})

test_that("results in the previous release's format print and summarise as their design", {
  # They have the class "FielDHub" only and no metadata; print() and
  # summary() find their design from id_design.
  differ <- character(0)
  for (name in names(catalogue)) {
    design <- catalogue_design(name)
    if (!inherits(design, "FielDHub") ||
        !as.character(design$infoDesign$id_design) %in% names(legacy_designs)) next
    saved <- legacy_roundtrip(design)
    if (!identical(utils::capture.output(print(saved)), utils::capture.output(print(design))) ||
        !identical(utils::capture.output(print(summary(saved))),
                   utils::capture.output(print(summary(design))))) {
      differ <- c(differ, name)
    }
  }
  expect_identical(differ, character(0))
})

test_that("results in the previous release's format still plot", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  differ <- character(0)
  for (name in names(catalogue)) {
    design <- catalogue_design(name)
    if (!inherits(design, "FielDHub") || inherits(design, "fieldhub_split_families") ||
        !as.character(design$infoDesign$id_design) %in% names(legacy_designs)) next
    plotted <- suppressWarnings(suppressMessages(plot(legacy_roundtrip(design))))
    if (!identical(plotted$field_book, field_layout(design))) differ <- c(differ, name)
  }
  expect_identical(differ, character(0))
})

test_that("print() shows the elements of a result whose design it does not know", {
  # print() used to print nothing for such results
  unknown <- structure(list(infoDesign = list(id_design = 99), fieldBook = data.frame(PLOT = 1:2)),
                       class = "FielDHub")
  expect_output(print(unknown), "fieldBook")
  expect_output(print(summary(unknown)), "fieldBook")
})

# The YEAR column -----------------------------------------------------------------

# Regression: YEAR was always taken from the system clock (export_design()
# even ignored the year given), so a call could not be reproduced across
# calendar years.
year_designs <- function(year) {
  list(
    diagonal_arrangement = diagonal_arrangement(nrows = 15, ncols = 20, lines = 270,
                                                checks = 4, seed = 1, year = year),
    RCBD_augmented = RCBD_augmented(lines = 50, checks = 3, b = 5, seed = 1, year = year),
    optimized_arrangement = optimized_arrangement(nrows = 12, ncols = 10, lines = 100,
                                                  rep_checks = 20, checks = 1:5, seed = 1, year = year),
    partially_replicated = partially_replicated(nrows = 8, ncols = 8, repGens = c(50, 7),
                                                repUnits = c(1, 2), seed = 1, year = year),
    sparse_allocation = sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4,
                                          seed = 1234, year = year),
    multi_location_prep = multi_location_prep(lines = 80, l = 4, copies_per_entry = 5, checks = 2,
                                              rep_checks = c(4, 4), allow_fillers = TRUE,
                                              seed = 1, year = year)
  )
}

test_that("the YEAR column can be set with `year` and defaults to the current year", {
  for (year in list(2020, NULL)) {
    want <- if (is.null(year)) format(Sys.Date(), "%Y") else as.character(year)
    designs <- year_designs(year)
    wrong <- names(designs)[!vapply(designs, function(x) all(x$fieldBook$YEAR == want), logical(1))]
    expect_identical(wrong, character(0), info = paste("year =", want))
  }
})

test_that("`year` must be a single value", {
  expect_error(RCBD_augmented(lines = 50, checks = 3, b = 5, seed = 1, year = c(2020, 2021)),
               class = "fieldhub_input_error")
})
