library(FielDHub)

# Saving and exporting designs: write_design()/read_design(), the standalone
# R script format, the app's downloads, table exports and their metadata.

# One expectation for a case that checks several properties: it names the
# case and every property that fails.
expect_case <- function(ok, case) {
  bad <- names(ok)[!vapply(ok, isTRUE, logical(1))]
  expect(length(bad) == 0L, sprintf("case %s: %s", case, paste(bad, collapse = ", ")))
}

# Whether `expr` signals an error of `class` whose message matches `regexp`
signals <- function(expr, class = "fieldhub_input_error", regexp = NULL) {
  tryCatch({
    force(expr)
    FALSE
  }, error = function(e) {
    inherits(e, class) && (is.null(regexp) || grepl(regexp, conditionMessage(e)))
  })
}

# The design a standalone R script written by write_design(format = "r")
# builds, and the script's text
replay_script <- function(x) {
  file <- tempfile(fileext = ".R")
  on.exit(unlink(file), add = TRUE)
  write_design(x, file, "r")
  env <- new.env(parent = baseenv())
  suppressWarnings(sys.source(file, envir = env))
  list(design = env$design, code = paste(readLines(file), collapse = "\n"))
}

# Wall-clock timings of the augmented row-column search are not
# deterministic content
comparable <- function(x) {
  if (inherits(x, "fieldhub_augmented_row_column")) arc_deterministic_view(x) else x
}

# Design formats --------------------------------------------------------------

test_that("design formats declare which can be read and written", {
  expect_identical(design_formats()[c("format", "readable", "writable")],
                   data.frame(format = c("rds", "r"), readable = c(TRUE, FALSE),
                              writable = c(TRUE, TRUE)))
})

test_that("an RDS file round-trips every result without replaying it", {
  directory <- tempfile("fieldhub-formats-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  set.seed(391)
  before <- .Random.seed
  for (name in names(catalogue)) {
    x <- catalogue_design(name)
    file <- file.path(directory, paste0(name, ".rds"))
    expect_case(list(
      "returns the file" = identical(write_design(x, file), file),
      "reads back the result" = identical(read_design(file), x),
      "draws no random number" = identical(.Random.seed, before)
    ), name)
  }
})

test_that("export permissions are explicit and failures leave existing files intact", {
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  x <- latin_rectangle(5, 3, seed = 1)
  saveRDS("existing", file)
  expect_error(write_design(x, file), "exists", class = "fieldhub_input_error")
  expect_error(write_design(NULL, file, overwrite = TRUE), class = "fieldhub_input_error")
  expect_error(write_design(x, file, format = "unknown", overwrite = TRUE), class = "fieldhub_input_error")
  expect_error(write_design(x, file, overwrite = NA), class = "fieldhub_input_error")
  expect_identical(readRDS(file), "existing")
  write_design(x, file, overwrite = TRUE)
  expect_identical(read_design(file), x)
  expect_error(write_design(x, dirname(file), overwrite = TRUE), class = "fieldhub_input_error")
})

test_that("imports reject malformed and unknown-schema files with one classed condition", {
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  expect_error(read_design(file), class = "fieldhub_input_error")
  bad <- list(null = NULL, character = "wrong", unknown_schema = list(metadata = list(schema_version = 99)))
  for (name in names(bad)) {
    saveRDS(bad[[name]], file)
    expect_error(read_design(file), class = "fieldhub_import_error", info = name)
  }
  x <- latin_rectangle(5, 3, seed = 1)
  x$metadata$schema_version <- 99L
  saveRDS(x, file)
  expect_error(read_design(file), "unknown schema", class = "fieldhub_import_error")
  writeLines("not serialized R data", file)
  expect_error(read_design(file), class = "fieldhub_import_error")
  expect_error(read_design(file, "r"), "not supported", class = "fieldhub_input_error")
  expect_error(read_design(c(file, file)), class = "fieldhub_input_error")
})

test_that("copy failures use the common error contract and clean temporary output", {
  directory <- tempfile("fieldhub-copy-failure-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  local_mocked_bindings(file.copy = function(...) FALSE, .package = "base")
  expect_error(write_design(latin_rectangle(5, 3, seed = 1), file.path(directory, "result.rds")),
    "Could not copy", class = "fieldhub_export_error")
  expect_length(list.files(directory, all.files = TRUE, no.. = TRUE), 0L)
})

# Standalone R scripts --------------------------------------------------------

test_that("standalone script exports replay exactly but are never imported as code", {
  file <- tempfile(fileext = ".R")
  on.exit(unlink(file), add = TRUE)
  x <- latin_rectangle(c("No nitrogen", "A*B", "C", "D", "E"), 3, seed = 27)
  write_design(x, file, "r")
  environment <- new.env(parent = globalenv())
  sys.source(file, envir = environment)
  expect_identical(environment$design, x)
  expect_error(read_design(file, "r"), class = "fieldhub_input_error")
  bad <- x
  bad$metadata$parameters$t <- quote(stop("must not execute"))
  expect_error(write_design(bad, file, "r", overwrite = TRUE), "not portable", class = "fieldhub_export_error")
  # The failed export left the script in place
  sys.source(file, envir = environment)
  expect_identical(environment$design, x)
})

test_that("standalone scripts rebuild every catalogue result", {
  for (name in names(catalogue)) {
    x <- catalogue_design(name)
    expect_identical(comparable(replay_script(x)$design), comparable(x), info = name)
  }
})

test_that("standalone scripts omit constant defaults and always retain seed and year", {
  code <- replay_script(RCBD(t = 5, reps = 3, seed = 11))$code
  expect_case(list(
    "calls the design" = grepl("FielDHub::RCBD(", code, fixed = TRUE),
    "keeps the seed" = grepl("seed = 11", code, fixed = TRUE),
    "omits defaults" = !grepl("planter =", code, fixed = TRUE) && !grepl("continuous =", code, fixed = TRUE)
  ), "RCBD")
  expect_match(replay_script(catalogue_design("diagonal_single"))$code, 'year = "2026"', fixed = TRUE)
})

test_that("standalone scripts quote labels and serialize small uploaded tables as data", {
  labels <- c("No nitrogen", 'A "; stop("not code"); #', "line\nfeed", "back\\slash", "café")
  cases <- list(
    crd = CRD(data = data.frame(TREATMENT = labels, REPS = rep(2, length(labels))), seed = 21),
    full_factorial = full_factorial(
      data = data.frame(FACTOR = c("Water supply", "Water supply", "Dose", "Dose"),
                        LEVEL = c("Rain fed", "Irrigated", "No nitrogen", "High N")),
      reps = 2, seed = 21)
  )
  for (name in names(cases)) {
    expect_identical(replay_script(cases[[name]])$design, cases[[name]], info = name)
  }
})

test_that("standalone scripts retain presence-sensitive explicit defaults", {
  x <- sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4,
                         exptName = NULL, seed = 35, year = 2026)
  replay <- replay_script(x)
  expect_true(grepl("exptName = NULL", replay$code, fixed = TRUE) && identical(replay$design, x))
})

test_that("standalone scripts retain RNG settings without leaking state", {
  local_rng_state()
  old_kind <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old_kind)), add = TRUE, after = FALSE)
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  x <- RCBD(t = 5, reps = 3, seed = 11)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(82)
  before <- .Random.seed
  kind <- RNGkind()
  file <- tempfile(fileext = ".R")
  on.exit(unlink(file), add = TRUE)
  write_design(x, file, "r")
  replay <- function() {
    env <- new.env(parent = baseenv())
    sys.source(file, envir = env)
    env$design
  }
  expect_case(list(
    "writing draws no random number" = identical(.Random.seed, before),
    "replays under the recorded RNG" = identical(replay(), x),
    "restores the caller's RNG" = identical(.Random.seed, before) && identical(RNGkind(), kind)
  ), "with a seed")
  rm(".Random.seed", envir = globalenv())
  expect_case(list(
    "replays" = identical(replay(), x),
    "leaves no seed" = !exists(".Random.seed", globalenv(), inherits = FALSE),
    "restores the RNG kind" = identical(RNGkind(), kind)
  ), "without a seed")
})

test_that("allocation inputs produce a two-step standalone script", {
  for (design in c("sparse", "prep")) {
    allocation <- catalogue_design(paste0("do_optim_", design))
    x <- if (design == "sparse") {
      sparse_allocation(lines = 120, l = 4, copies_per_entry = 3,
                        sparse_list = allocation, checks = 4, seed = 31, year = 2026)
    } else {
      multi_location_prep(lines = 80, l = 4, copies_per_entry = 5,
                          optim_list = allocation, checks = 2, rep_checks = c(4, 4),
                          allow_fillers = TRUE, seed = 31, year = 2026)
    }
    replay <- replay_script(x)
    # Without the allocation's own record there is no script, only the RDS path
    parameter <- if (design == "sparse") "sparse_list" else "optim_list"
    unrecorded <- x
    unrecorded$metadata$parameters[[parameter]]$metadata <- NULL
    expect_case(list(
      "builds the allocation first" = grepl("alloc <-", replay$code, fixed = TRUE) &&
        grepl("FielDHub::do_optim(", replay$code, fixed = TRUE),
      "replays" = identical(replay$design, x),
      "refuses an unrecorded allocation" = signals(write_design(unrecorded, tempfile(fileext = ".R"), "r"),
                                                    "fieldhub_export_error", "[Aa]llocation")
    ), design)
  }
})

test_that("unsupported records and large uploads explain the RDS fallback", {
  file <- tempfile(fileext = ".R")
  large <- CRD(data = data.frame(TREATMENT = paste0("T", seq_len(201)), REPS = 2), seed = 12)
  expect_error(write_design(large, file, "r"), "200", class = "fieldhub_export_error")
  code_data <- RCBD(t = 5, reps = 3, seed = 11)
  code_data$metadata$parameters$data <- quote(stop("must not execute"))
  expect_error(write_design(code_data, file, "r"), "data", class = "fieldhub_export_error")
  expect_false(file.exists(file))
})

test_that("standalone scripts use canonical argument names for older recordings", {
  x <- catalogue_design("optimized_arrangement")
  old <- x
  old$metadata$parameters$amountChecks <- old$metadata$parameters$rep_checks
  old$metadata$parameters$rep_checks <- NULL
  replay <- replay_script(old)
  expect_true(!grepl("amountChecks", replay$code, fixed = TRUE) && identical(replay$design, x))
  # The previous release's saved objects recorded no parameters: they are
  # refused by class, with the reason
  legacy <- readRDS(test_path("fixtures", "v1.5.0-designs.rds"))$designs
  refused <- lapply(legacy, function(saved) {
    signals(write_design(with_design_class(saved), tempfile(fileext = ".R"), "r"),
            regexp = "recorded parameters")
  })
  expect_case(refused, "previous release's saved objects")
})

# App downloads ---------------------------------------------------------------

test_that("design downloads preserve complete results and reproducibility metadata", {
  directory <- tempfile("fieldhub-design-export-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  for (name in names(catalogue)) {
    x <- catalogue_design(name)
    handlers <- design_export_handlers(function() x)
    file <- file.path(directory, handlers$filename())
    handlers$content(file)
    expect_case(list(
      "file name" = grepl("^FielDHub_[a-z_]+_seed_-?[0-9.]+[.]rds$", handlers$filename()),
      "complete result" = identical(readRDS(file), x)
    ), name)
  }
})

test_that("displayed R code reconstructs the saved design", {
  directory <- tempfile("fieldhub-design-code-")
  dir.create(directory)
  previous <- setwd(directory)
  on.exit({setwd(previous); unlink(directory, recursive = TRUE)}, add = TRUE)
  x <- RCBD(t = 5, reps = 3, seed = 38)
  handlers <- design_export_handlers(function() x)
  handlers$content(handlers$filename())
  set.seed(91)
  before <- .Random.seed
  result <- new.env(parent = globalenv())
  code <- handlers$code()
  eval(parse(text = code), envir = result)
  expect_case(list(
    "reads the saved design" = identical(result$saved_design, x),
    "rebuilds it" = identical(result$design, x),
    "draws no random number" = identical(.Random.seed, before),
    "notes simulated responses" = grepl("simulated responses", code, fixed = TRUE),
    "records the version" = grepl(x$metadata$package_version, code, fixed = TRUE)
  ), "RCBD")
})

test_that("reproduction code shows the standalone call before the RDS path", {
  code <- design_export_handlers(function() RCBD(t = 5, reps = 3, seed = 11))$code()
  call <- regexpr("FielDHub::RCBD(", code, fixed = TRUE)[[1]]
  expect_true(call > 0 && call < regexpr("readRDS(", code, fixed = TRUE)[[1]])
  large <- CRD(data = data.frame(TREATMENT = paste0("T", seq_len(201)), REPS = 2), seed = 12)
  expect_match(design_export_handlers(function() large)$code(), "200")
})

test_that("download handlers use the current completed result without modifying it", {
  x <- RCBD(t = 4, reps = 2, seed = 38)
  handlers <- design_export_handlers(function() x)
  first <- handlers$filename()
  x <- CRD(t = 4, reps = 2, seed = 57)
  before <- serialize(x, NULL)
  expect_true(!identical(handlers$filename(), first) && grepl("crd_seed_57", handlers$filename(), fixed = TRUE))
  expect_identical(serialize(x, NULL), before)
  x$metadata <- NULL
  expect_error(handlers$filename(), "recorded parameters", class = "fieldhub_input_error")
  expect_error(handlers$code(), "recorded parameters", class = "fieldhub_input_error")
})

test_that("exports reject unavailable designs before writing a file", {
  file <- tempfile(fileext = ".rds")
  handlers <- design_export_handlers(function() NULL)
  expect_error(handlers$content(file), class = "fieldhub_input_error")
  expect_false(file.exists(file))
})

# Field books and tables ------------------------------------------------------

test_that("field books join entry names by ENTRY, not by row position", {
  # Uploaded entries in shuffled order: each plot's name is its entry's
  set.seed(5)
  names <- c(paste0("CK", 1:4), paste0("L", sample(1000, 120)))
  entries <- data.frame(ENTRY = 1:124, NAME = names)[c(1:4, sample(5:124)), ]
  book <- diagonal_arrangement(nrows = 15, ncols = 10, lines = 120, checks = 4, seed = 1,
                               data = entries)$fieldBook
  expect_identical(book$TREATMENT, entries$NAME[match(book$ENTRY, entries$ENTRY)])
})

test_that("CSV downloads keep the table and reject malformed exports", {
  book <- field_layout(RCBD(t = 4, reps = 2, seed = 17))
  directory <- tempfile("fieldhub-csv-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  download <- function(table, name) {
    file <- file.path(directory, paste0(name, ".csv"))
    csv_download_handlers(function() "book.csv", function() table)$content(file)
    file
  }
  # The CSV holds every column and value of the table, as text
  written <- utils::read.csv(download(book, "book"), colClasses = "character")
  expect_identical(written, as.data.frame(lapply(book, as.character)))
  duplicate <- book
  names(duplicate)[2] <- names(duplicate)[1]
  unnamed <- book
  names(unnamed)[1] <- ""
  missing <- book
  names(missing)[1] <- NA_character_
  nested <- book
  nested$BAD <- rep(list(1), nrow(book))
  matrix_column <- book
  matrix_column$BAD <- matrix(1, nrow(book), 2)
  bad <- list(null = NULL, list = list(), no_rows = book[FALSE, ], no_columns = book[, FALSE],
              duplicate = duplicate, unnamed = unnamed, missing = missing, nested = nested,
              matrix_column = matrix_column)
  refused <- lapply(names(bad), function(name) signals(download(bad[[name]], name)))
  expect_case(setNames(refused, names(bad)), "malformed tables")
  expect_identical(list.files(directory), "book.csv")
})

test_that("export grids place values by named coordinates without reordering labels", {
  book <- data.frame(ROW = c(2L, 1L, 2L, 1L), COLUMN = c(2L, 1L, 1L, 2L),
                     TREATMENT = c("D", "A", "C", "B"))
  expected <- matrix(c("A", "C", "B", "D"), 2, 2)
  expect_identical(field_book_export_grid(book, "TREATMENT"), expected)
  book <- book[4:1, c("TREATMENT", "COLUMN", "ROW")]
  book$ROW <- factor(book$ROW, levels = 2:1)
  book$COLUMN <- as.character(book$COLUMN)
  expect_identical(field_book_export_grid(book, "TREATMENT"), expected)
  book$ENTRY <- c(0L, 3L, 1L, 4L)
  expect_identical(field_book_export_grid(book, "ENTRY"), matrix(c(1L, 3L, 0L, 4L), 2, 2))
})

test_that("export grids explain invalid coordinate maps", {
  book <- data.frame(ROW = c(1, 1, 2, 2), COLUMN = c(1, 2, 1, 2), ENTRY = 1:4)
  matrix_entry <- book
  matrix_entry$ENTRY <- I(matrix(1:4, ncol = 1))
  maps <- list(missing_plot = book[-1, ], repeated_plot = book[c(1, 1, 3, 4), ], empty = book[FALSE, ],
               missing_row = transform(book, ROW = c(1, NA, 2, 2)),
               gap = transform(book, ROW = c(1, 1, 3, 3)),
               zero = transform(book, COLUMN = c(0, 1, 0, 1)),
               fraction = transform(book, COLUMN = c(1.5, 2, 1.5, 2)),
               no_row = book[c("ENTRY", "COLUMN")], list = list(ROW = 1, COLUMN = 1, ENTRY = 1),
               matrix_values = matrix_entry)
  expect_case(lapply(maps, function(bad) signals(field_book_export_grid(bad, "ENTRY"))), "maps")
  types <- list(null = NULL, na = NA_character_, missing = "MISSING", two = c("ROW", "COLUMN"))
  expect_case(lapply(types, function(type) signals(field_book_export_grid(book, type))), "types")
})

test_that("layout exports reject invalid locations with classed errors", {
  book <- data.frame(LOCATION = "A", ROW = c(1, 1, 2, 2), COLUMN = c(1, 2, 1, 2), ENTRY = 1:4)
  selected <- list(null = NULL, zero = 0, two = 2, na = NA_real_, fraction = 1.5, repeated = c(1, 1),
                   name = "A")
  refused <- lapply(selected, function(s) signals(export_layout(book, s)))
  refused$no_location <- signals(export_layout(book[names(book) != "LOCATION"], 1))
  refused$missing_location <- signals(export_layout(transform(book, LOCATION = NA_character_), 1))
  expect_case(refused, "export_layout()")
})

# Table export metadata -------------------------------------------------------

test_that("table export records retain full parameters and selected location", {
  design <- RCBD(t = 4, reps = 2, l = 2, plotNumber = c(1, 101),
                 locationNames = c("West", "East"), seed = 42)
  before <- serialize(design, NULL)
  record <- table_export_record(design, "Plot numbers", location = 2L)
  text <- table_export_text(record)
  rebuilt <- new.env(parent = baseenv())
  eval(parse(text = text), rebuilt)
  expect_case(list(
    "record" = identical(record$schema_version, 1L) && identical(record$metadata, design$metadata) &&
      identical(record$view, list(table = "Plot numbers", location = 2L)),
    "design unchanged" = identical(serialize(design, NULL), before),
    "text names its source" = grepl("FielDHub table export", text, fixed = TRUE) &&
      grepl(design$metadata$package_version, text, fixed = TRUE),
    "text round-trips" = identical(rebuilt$record, record)
  ), "RCBD")
  expect_error(table_export_record(design, "Layout", location = 0), class = "fieldhub_input_error")
  expect_error(table_export_record(design, NA_character_), class = "fieldhub_input_error")
  expect_error(table_export_record(NULL, "Layout"), class = "fieldhub_input_error")
})

test_that("metadata chunks retain every character without reaching Excel's cell limit", {
  # Excel keeps at most 32767 characters per cell; chunks stay at 16000
  text <- paste0(strrep("0123456789", 7000), "\n", strrep("\U0001f33e", 10000))
  chunks <- table_export_chunks(text)
  expect_true(length(chunks) > 1L && all(nchar(chunks, type = "chars") <= 16000L) &&
                identical(paste0(chunks, collapse = ""), text))
})

test_that("all recorded design and allocation families round-trip table metadata", {
  for (name in names(catalogue)) {
    record <- table_export_record(catalogue_design(name), "Recorded design")
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
  record <- new.env(parent = baseenv())
  eval(parse(text = buttons[[1L]]$messageTop), record)
  expect_case(list(
    "copy, excel and print" = identical(vapply(buttons, `[[`, character(1), "extend"),
                                        c("copy", "excel", "print")),
    "copy carries the record" = identical(record$record$metadata, design$metadata),
    "excel carries it in chunks" = identical(paste0(unlist(buttons[[2L]]$fieldhubMetadata), collapse = ""),
                                             buttons[[1L]]$messageTop) &&
      inherits(buttons[[2L]]$customize, "JS_EVAL"),
    "print escapes labels" = grepl("&lt;script&gt;", buttons[[3L]]$messageTop, fixed = TRUE) &&
      !grepl("<script>", buttons[[3L]]$messageTop, fixed = TRUE)
  ), "CRD with a script label")
})
