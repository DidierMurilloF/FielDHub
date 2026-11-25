# Entry-list uploads in the app: reading the CSV (load_file(),
# read_design_upload()), each design's column and uniqueness rule, the
# module adapter that reports a bad file instead of failing
# (app_read_upload()), the upload controls and example dialog, and the
# upload size limit the app sets and restores.

all_upload_keys <- c("crd", "rcbd", "alpha", "rect", "rcd", "square", "mdiag",
                     "sdiag", "sparse_allocation", "arcbd", "factorial", "ibd",
                     "lsd", "multi_loc_prep", "optim", "prep", "spd", "sspd",
                     "strip")

write_upload <- function(lines, fileext = ".csv") {
  path <- tempfile(fileext = fileext)
  writeLines(lines, path, useBytes = TRUE)
  path
}

write_upload_bytes <- function(text) {
  path <- tempfile(fileext = ".csv")
  writeBin(charToRaw(text), path)
  path
}

write_upload_table <- function(data) {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(data, path, row.names = FALSE)
  path
}

# list(value =) or list(error =); a warning becomes an unclassed error, so a
# case also fails when the reader leaks a parser warning
upload_outcome <- function(fun) {
  tryCatch(
    withCallingHandlers(list(value = fun()), warning = function(w) {
      stop("leaked warning: ", conditionMessage(w), call. = FALSE)
    }),
    error = function(e) list(error = e)
  )
}

alpha_entries <- data.frame(ENTRY = 1:2, NAME = c("G1", "G2"))
bad_file <- "Invalid file; Please upload a .csv file."

# Reading uploads --------------------------------------------------------------

# `read` is a function of no arguments; `value` the expected result, or
# `message` the exact text of the fieldhub_input_error the user sees.
upload_read_cases <- c(
  lapply(c(comma = ",", semicolon = ";", tab = "\t"), function(sep) list(
    read = function() read_design_upload(
      write_upload(gsub(",", sep, c("ENTRY,NAME", "1,G1", "2,G2"), fixed = TRUE)), sep, "alpha"),
    value = list(data = alpha_entries))),
  list(
    bom = list(read = function() read_design_upload(
      write_upload_bytes("\xef\xbb\xbfENTRY,NAME\n1,G1\n2,G2\n"), ",", "alpha"),
      value = list(data = alpha_entries)),
    extra_columns = list(read = function() read_design_upload(
      write_upload(c("ENTRY,NAME,EXTRA", "1,G1,x", "2,G2,y")), ",", "alpha"),
      value = list(data = data.frame(alpha_entries, EXTRA = c("x", "y")))),
    trailing_blank_lines = list(read = function() read_design_upload(
      write_upload(c("ENTRY,NAME", "1,G1", "2,G2", "", "")), ",", "alpha"),
      value = list(data = alpha_entries)),
    single_row = list(read = function() read_design_upload(
      write_upload(c("ENTRY,NAME", "1,G1")), ",", "alpha"),
      value = list(data = alpha_entries[1, ])),
    one_column_rule = list(read = function() read_design_upload(
      write_upload(c("TREATMENT", "T1", "T2")), ",", "crd"),
      value = list(data = data.frame(TREATMENT = c("T1", "T2")))),
    three_column_rule = list(read = function() read_design_upload(
      write_upload(c("ROW,COLUMN,TREATMENT", "1,1,A", "2,2,B")), ",", "lsd"),
      value = list(data = data.frame(ROW = 1:2, COLUMN = 1:2, TREATMENT = c("A", "B")))),
    paired_rule = list(read = function() read_design_upload(
      write_upload(c("FACTOR,LEVEL", "A,a0", "A,a1", "B,b0")), ",", "factorial"),
      value = list(data = data.frame(FACTOR = c("A", "A", "B"), LEVEL = c("a0", "a1", "b0")))),
    duplicates_allowed_without_check = list(read = function() read_design_upload(
      write_upload(c("ENTRY,NAME", "1,G1", "1,G1")), ",", "mdiag", check = FALSE),
      value = list(data = data.frame(ENTRY = c(1L, 1L), NAME = c("G1", "G1")))),

    empty_file = list(read = function() read_design_upload(
      write_upload(character(0)), ",", "alpha", missing_columns = "Use ENTRY and NAME"),
      message = bad_file),
    header_only = list(read = function() read_design_upload(
      write_upload("ENTRY,NAME"), ",", "alpha"), message = bad_file),
    ragged_rows = list(read = function() read_design_upload(
      write_upload(c("ENTRY,NAME", "1,G1", "2")), ",", "alpha"), message = bad_file),
    not_csv = list(read = function() read_design_upload(
      write_upload(c("ENTRY,NAME", "1,G1"), fileext = ".txt"), ",", "alpha", name = "entries.txt"),
      message = bad_file),
    unterminated_quote = list(read = function() read_design_upload(
      write_upload(c("ENTRY,NAME", '1,"unterminated')), ",", "alpha"), message = bad_file),
    unterminated_quote_inside = list(read = function() read_design_upload(
      write_upload(c("ENTRY,NAME", "1,G1", '2,"unterminated', "next line")), ",", "alpha"),
      message = bad_file),
    duplicate_values = list(read = function() read_design_upload(
      write_upload(c("TREATMENT", "T1", "T1", "T2")), ",", "crd", missing_columns = "Use TREATMENT"),
      message = "Check input file for duplicate values."),
    missing_columns_override = list(read = function() read_design_upload(
      write_upload(c("ENTRY", "1", "2")), ",", "alpha",
      missing_columns = "Data input needs at least two columns: ENTRY and NAME"),
      message = "Data input needs at least two columns: ENTRY and NAME"),
    # each design keeps its own missing-columns guidance
    missing_columns_alpha = list(read = function() read_design_upload(
      write_upload(c("ENTRY", "1", "2")), ",", "alpha"),
      message = "Data input needs at least two columns: ENTRY and NAME"),
    missing_columns_lsd = list(read = function() read_design_upload(
      write_upload(c("ROW,COLUMN", "1,1", "2,2")), ",", "lsd"),
      message = "Data input needs at least three columns: ROW, COLUMN and TREATMENT"),

    # load_file() status flags: a malformed upload used to raise inside the
    # module's reactive and end the user's session
    load_well_formed = list(read = function() load_file(
      "entries.csv", write_upload(c("ENTRY,NAME", "1,G1", "2,G2")), ","),
      value = list(dataUp = alpha_entries)),
    load_ragged = list(read = function() load_file(
      "ragged.csv", write_upload(c("ENTRY,NAME", "1,G1,extra,fields")), ","),
      value = list(bad_format = TRUE)),
    load_empty = list(read = function() load_file("empty.csv", write_upload(character(0)), ","),
      value = list(bad_format = TRUE)),
    load_not_csv = list(read = function() load_file("entries.xlsx", "entries.xlsx", ","),
      value = list(bad_format = TRUE)),
    # empty cells in optional (omit_na) columns are kept as NA
    load_padded_split_plot = list(read = function() load_file(
      "entries.csv", write_upload(c("WHOLEPLOT,SUBPLOT", "W1,S1", "W2,S2", ",S3")), ",",
      check = TRUE, design = "spd"),
      value = list(dataUp = data.frame(WHOLEPLOT = c("W1", "W2", NA), SUBPLOT = c("S1", "S2", "S3"))))
  ),
  # extra fields are rejected, not taken as row names
  lapply(c(extra_comma = ",", extra_semicolon = ";", extra_tab = "\t"), function(sep) list(
    read = function() load_file("entries.csv", write_upload(
      gsub(",", sep, c("ENTRY,NAME", "1,G1,extra", "2,G2,more"), fixed = TRUE)), sep),
    value = list(bad_format = TRUE))),
  # short records are rejected, not padded
  lapply(c(short_first = 2L, short_last = 8L), function(row) list(
    read = function() {
      lines <- c("ENTRY,NAME", paste0(seq_len(8L), ",G", seq_len(8L)))
      lines[row] <- "99"
      load_file("entries.csv", write_upload(lines), ",")
    },
    value = list(bad_format = TRUE))),
  # a complete last record needs no final newline
  lapply(c(no_newline = "", newline = "\n", crlf = "\r\n"), function(ending) list(
    read = function() read_design_upload(
      write_upload_bytes(paste0('ENTRY,NAME\n1,"G,1"\n2,"G\ntwo"', ending)), ",", "alpha"),
    value = list(data = data.frame(ENTRY = 1:2, NAME = c("G,1", "G\ntwo")))))
)

test_that("uploads are read into the entries table or rejected with the message users see", {
  for (name in names(upload_read_cases)) {
    case <- upload_read_cases[[name]]
    got <- upload_outcome(case$read)
    if (!is.null(case$value)) {
      expect_equal(got$value, case$value, info = name)
    } else {
      expect(inherits(got$error, "fieldhub_input_error") &&
               identical(conditionMessage(got$error), case$message),
             sprintf("case %s: got %s", name, if (is.null(got$error)) "no error" else
               paste0(class(got$error)[1], ": ", conditionMessage(got$error))))
    }
  }
})

test_that("quoted records and explicit empty cells are read as read.csv() reads them", {
  # independent reference: base R's own CSV reader on the same file
  path <- write_upload(c("ENTRY,NAME", '1,"G,1"', '2,"G""2"', '3,"G', 'three"', "4,", "", "5,G5"))
  expect_equal(load_file("entries.csv", path, ","),
               list(dataUp = utils::read.csv(path, na.strings = c("", " ", "NA"))))
})

# Column and uniqueness rules ------------------------------------------------

# For each design key: how many leading columns it requires, whether empty
# cells are allowed in them (omit_na), and whether uniqueness applies to
# FACTOR/LEVEL pairs (paired) instead of each column.
upload_rules <- data.frame(
  design = c("crd", "rcbd", "sdiag", "mdiag", "optim", "arcbd", "prep", "square", "rect",
             "alpha", "ibd", "rcd", "factorial", "spd", "strip", "lsd", "sspd"),
  required = c(1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3),
  omit_na = c(rep(FALSE, 13), TRUE, TRUE, FALSE, TRUE),
  paired = c(rep(FALSE, 12), TRUE, rep(FALSE, 4))
)

test_that("each design's upload rule requires its columns and unique entries", {
  status <- function(data, design) {
    names(load_file("entries.csv", write_upload_table(data), ",", check = TRUE, design = design))
  }
  for (i in seq_len(nrow(upload_rules))) {
    rule <- upload_rules[i, ]
    n <- rule$required
    unique_rows <- as.data.frame(lapply(stats::setNames(seq_len(n), paste0("C", seq_len(n))),
                                        function(j) paste0(letters[j], 1:4)))
    duplicate_last <- unique_rows
    duplicate_last[2, n] <- duplicate_last[1, n]
    empty_last <- unique_rows
    empty_last[2:3, n] <- NA
    got <- c(
      unique = status(unique_rows, rule$design),
      short = if (n > 1) status(unique_rows[, -n, drop = FALSE], rule$design) else "missing_cols",
      # a repeated LEVEL is fine when its FACTOR differs
      duplicate_last = status(duplicate_last, rule$design),
      # two empty cells are two equal values, unless empty cells are allowed
      empty_last = status(empty_last, rule$design)
    )
    want <- c(unique = "dataUp", short = "missing_cols",
              duplicate_last = if (rule$paired) "dataUp" else "duplicated_vals",
              empty_last = if (rule$omit_na || rule$paired) "dataUp" else "duplicated_vals")
    expect_identical(got, want, info = rule$design)
  }
})

test_that("an incomplete upload validation request reports missing columns, not a result", {
  data <- data.frame(ENTRY = 1:2, NAME = c("A", "B"))
  requests <- list(
    design_NULL = list(NULL, data), design_NA = list(NA_character_, data),
    design_empty = list(character(), data), two_designs = list(c("crd", "rcbd"), data),
    design_number = list(1, data), design_unknown = list("unknown", data),
    data_NULL = list("crd", NULL), data_vector = list("crd", 1:3)
  )
  answered <- names(Filter(function(r) !is.null(check_input(r[[1]], r[[2]])), requests))
  expect_identical(answered, character())
})

# The module adapter -------------------------------------------------------------

fake_input <- function(design, file, sep = ",") {
  spec <- app_upload_spec(design)
  input <- list()
  input[[spec$file]] <- file
  input[[spec$sep]] <- sep
  input
}

# app_read_upload() on a file, with the messages it reported
read_module_upload <- function(design, path, check = TRUE, file = list(datapath = path, name = "entries.csv")) {
  reported <- character()
  out <- withCallingHandlers(
    app_read_upload(fake_input(design, file), design, check = check),
    message = function(m) {
      reported[[length(reported) + 1L]] <<- conditionMessage(m)
      invokeRestart("muffleMessage")
    }
  )
  list(out = out, reported = paste(reported, collapse = ""))
}

test_that("a module reads a good upload and reports a bad one without failing", {
  skip_if_not_installed("shiny")
  good <- read_module_upload("alpha", write_upload(c("ENTRY,NAME", "1,G1", "2,G2")))
  expect_equal(good$out, list(data = alpha_entries))

  # a module that lets entries repeat skips the uniqueness rule
  repeated <- read_module_upload("mdiag", write_upload(c("ENTRY,NAME", "1,G1", "1,G1")), check = FALSE)
  expect_equal(repeated$out, list(data = data.frame(ENTRY = c(1L, 1L), NAME = c("G1", "G1"))))

  # before a file is chosen the module waits silently
  input <- list()
  input[[app_upload_spec("crd")$sep]] <- ","
  expect_s3_class(tryCatch(app_read_upload(input, "crd"), error = identity), "shiny.silent.error")

  # Shiny's file record is a data frame: the path is read from its exact
  # `datapath` field, never from a partially matching one
  entries <- write_upload(c("ENTRY,NAME", "1,G1", "2,G2"))
  exact <- read_module_upload("alpha", file = data.frame(name = "entries.csv", datapath = entries))
  expect_equal(exact$out, list(data = alpha_entries))
  partial <- read_module_upload("alpha", file = data.frame(name = "entries.csv", datapath_backup = entries))
  expect(is.null(partial$out) && grepl(bad_file, partial$reported, fixed = TRUE),
         "a file record without `datapath` was read from another field")
})

test_that("every module rejects an upload with duplicate entries", {
  skip_if_not_installed("shiny")
  for (design in all_upload_keys) {
    example <- upload_format_example(design)
    got <- read_module_upload(design, write_upload_table(example[c(1, seq_len(nrow(example))), ]))
    expect(is.null(got$out) && grepl("duplicate values", got$reported, fixed = TRUE),
           sprintf("design %s: a repeated first row was %s", design,
                   if (is.null(got$out)) paste("reported as:", got$reported) else "accepted"))
  }
  # multi-location p-rep and sparse allocation use the single-diagonal rule:
  # entry numbers and names must both be unique
  for (design in c("multi_loc_prep", "sparse_allocation")) {
    got <- read_module_upload(design, write_upload(c("ENTRY,NAME", "1,G1", "2,G1", "3,G3")))
    expect(is.null(got$out) && grepl("duplicate values", got$reported, fixed = TRUE),
           sprintf("design %s accepted a repeated NAME", design))
  }
})

# Upload controls and the format dialog -----------------------------------------

test_that("each module's upload controls use its own namespaced input ids", {
  skip_if_not_installed("shiny")
  for (design in all_upload_keys) {
    spec <- app_upload_spec(design)
    html <- as.character(app_upload_ui(shiny::NS("mod"), design))
    ids <- paste0('id="mod-', unlist(spec[c("toggle", "file", "sep")]), '"')
    missing <- ids[!vapply(ids, grepl, logical(1), x = html, fixed = TRUE)]
    expect(length(missing) == 0L, sprintf("design %s: no %s", design, paste(missing, collapse = ", ")))
  }
})

test_that("every module's format dialog shows a non-empty example table", {
  skip_if_not_installed("shiny")
  for (design in all_upload_keys) {
    example <- upload_format_example(design)
    dialog <- tryCatch(as.character(app_upload_dialog(design)), error = identity)
    expect(is.data.frame(example) && nrow(example) > 0L && is.character(dialog),
           sprintf("design %s: no example table or the dialog failed to render", design))
  }
  expect_error(upload_format_example("not-a-design"), class = "fieldhub_input_error")
})

test_that("upload examples show each design's own published columns and contents", {
  columns <- list(crd = "TREATMENT", rcbd = "TREATMENT", alpha = c("ENTRY", "NAME"),
                  lsd = c("ROW", "COLUMN", "TREATMENT"), factorial = c("FACTOR", "LEVEL"),
                  prep = c("ENTRY", "NAME", "REPS"), strip = c("HPLOTS", "VPLOTS"))
  for (design in names(columns)) {
    expect_identical(names(upload_format_example(design)), columns[[design]], info = design)
  }
  literal <- list(
    optim = data.frame(
      ENTRY = 1:9, NAME = c("CHECK1", "CHECK2", "CHECK3", "GenotypeA", "GenotypeB", "GenotypeC",
                            "GenotypeD", "GenotypeE", "GenotypeF"),
      REPS = factor(c(10, 10, 10, 1, 1, 1, 1, 1, 1))),
    spd = data.frame(
      WHOLEPLOT = c("NFung", "Fung1", "Fung2", "Fung3", "Fung4", "", "", "", "", ""),
      SUBPLOT = paste0("Beans", 1:10)),
    sspd = data.frame(
      WHOLPLOT = c("IRR_NO", "IRR_Yes", "", "", "", "", "", "", "", ""),
      SUBPLOT = c("NFung", "Fung1", "Fung2", "Fung3", "Fung4", "", "", "", "", ""),
      SUB_SUBPLOT = paste0("Beans", 1:10)),
    multi_loc_prep = data.frame(ENTRY = 1:10, NAME = paste0("Genotype-", LETTERS[1:10])))
  for (design in names(literal)) {
    expect_identical(upload_format_example(design), literal[[design]], info = design)
  }
  # modules sharing the single-diagonal rule keep their own example
  expect_identical(upload_format_example("sdiag"), upload_format_example("sparse_allocation"))
  expect_false(identical(upload_format_example("multi_loc_prep"), upload_format_example("sdiag")))
})

# Upload size limit (no global option left changed) -----------------------------

test_that("app startup raises the upload limit and shutdown restores the host's value once", {
  previous <- options(shiny.maxRequestSize = 7 * 1024^2)
  on.exit(options(previous), add = TRUE)
  callbacks <- list()
  app_upload_limit(register_stop = function(callback) callbacks[[length(callbacks) + 1L]] <<- callback)
  expect_equal(getOption("shiny.maxRequestSize"), 100 * 1024^2)
  expect_length(callbacks, 1L)
  callbacks[[1]]()
  expect_equal(getOption("shiny.maxRequestSize"), 7 * 1024^2)
  # a repeated stop callback must not undo a later host setting
  options(shiny.maxRequestSize = 9 * 1024^2)
  callbacks[[1]]()
  expect_equal(getOption("shiny.maxRequestSize"), 9 * 1024^2)
})

test_that("app shutdown removes an upload limit the host had not set", {
  previous <- options(shiny.maxRequestSize = NULL)
  on.exit(options(previous), add = TRUE)
  restore <- NULL
  app_upload_limit(register_stop = function(callback) restore <<- callback)
  restore()
  expect_false("shiny.maxRequestSize" %in% names(options()))
})

test_that("a failed stop registration leaves the upload limit unchanged", {
  previous <- options(shiny.maxRequestSize = 11 * 1024^2)
  on.exit(options(previous), add = TRUE)
  # the error is planted by the test, so its own text identifies it
  expect_error(app_upload_limit(register_stop = function(callback) stop("registration failed")),
               "registration failed")
  expect_equal(getOption("shiny.maxRequestSize"), 11 * 1024^2)
})

test_that("each app restart restores the host upload limit current at that start", {
  previous <- options(shiny.maxRequestSize = 13 * 1024^2)
  on.exit(options(previous), add = TRUE)
  restored <- vapply(c(13, 17) * 1024^2, function(limit) {
    options(shiny.maxRequestSize = limit)
    restore <- NULL
    app_upload_limit(register_stop = function(callback) restore <<- callback)
    restore()
    getOption("shiny.maxRequestSize")
  }, numeric(1))
  expect_equal(restored, c(13, 17) * 1024^2)
})

# Factorial uploads with spaces in factor and level names (issue #43) -------------

factorial_upload <- data.frame(
  FACTOR = c("Nitrogen rate", "Nitrogen rate", "Irrigation type", "Irrigation type"),
  LEVEL = c("No nitrogen", "High nitrogen", "Dry land", "Full water")
)

# The factorial page's own path: upload reader, page spec, builder, engine
factorial_page_design <- function(data, type) {
  spec <- design_app_spec("FD")
  upload <- app_upload_spec(spec$upload)
  file <- read_design_upload(write_upload_table(data), ",", design = upload$validation_design,
                             missing_columns = upload$missing_columns, name = "entries.csv")
  shaped <- design_upload_data(spec$upload_shape(file$data))
  raw <- stats::setNames(lapply(spec$controls, `[[`, "value"),
                         vapply(spec$controls, `[[`, character(1), "id"))
  raw$type <- as.character(type)
  raw$l <- 2L
  raw$location_names <- "FARGO,CASSELTON"
  raw$plot_start <- "101,1001"
  raw$seed <- 7L
  parsed <- read_design_controls(spec, raw, uploaded = TRUE)
  shaped <- spec$data(shaped, parsed)
  values <- spec$values(parsed, shaped)
  do.call(spec$engine, spec$args(values, shaped))
}

test_that("factor and level names with spaces build the factorial page's design", {
  for (type in c(1, 2)) {
    x <- factorial_page_design(factorial_upload, type)
    direct <- full_factorial(reps = 3, l = 2, type = type, plotNumber = c(101, 1001),
                             locationNames = c("FARGO", "CASSELTON"), seed = 7,
                             data = factorial_upload)
    expect_identical(x$fieldBook, direct$fieldBook, info = paste("type", type))
  }
  expect_setequal(unique(x$fieldBook$TRT_COMB),
                  c("No nitrogen*Dry land", "No nitrogen*Full water",
                    "High nitrogen*Dry land", "High nitrogen*Full water"))
  expect_true(all(c("FACTOR_Nitrogen rate", "FACTOR_Irrigation type") %in% names(x$fieldBook)))
})

test_that("the factorial page's results work with names containing spaces", {
  spec <- classic_workflow_spec("FD")
  x <- factorial_page_design(factorial_upload, 2)
  view <- checked_layout_view(x, layout = 1, planter = "serpentine", location = 2,
                              stacked = "vertical")
  book <- app_trait_simulation(x, view[[spec$book_component]],
                               list(mean = 6, residual_sd = .4, trait = "YIELD"),
                               7, spec$order_by_id)
  expect_true("YIELD" %in% names(book$df))
  expect_silent(field_book_table_data(book$df, factor_columns = spec$table_columns))
  exported <- vapply(1:3, function(plot_type) {
    file <- classic_workflow_layout(book$df, 1L, plot_type, spec)$file
    is.data.frame(file) && nrow(file) > 0L
  }, logical(1))
  expect_true(all(exported), info = paste("plot types without an export:", toString(which(!exported))))
  expect_identical(reproduce_design(x)$fieldBook, x$fieldBook)
})
