# Input validation of the exported design functions: malformed counts,
# labels, entry tables, choices and flags raise a classed error (by default
# fieldhub_input_error) before any random numbers are drawn; values replaced
# by defaults raise a classed fieldhub_default_warning; deprecated spellings
# raise fieldhub_deprecated_warning.

# Each element of `calls` is a zero-argument function expected to fail with
# `class`. Returns one line for each call that did not, or that drew random
# numbers before failing when `rng = TRUE`.
input_error_problems <- function(calls, class = "fieldhub_input_error", rng = FALSE) {
  problems <- character()
  for (name in names(calls)) {
    set.seed(718)
    before <- .Random.seed
    error <- tryCatch({
      suppressWarnings(calls[[name]]())
      NULL
    }, error = identity)
    problem <- if (is.null(error)) "no error" else if (!inherits(error, class)) {
      paste0(class(error)[1], ": ", conditionMessage(error))
    } else if (rng && !identical(.Random.seed, before)) "random numbers drawn before the error"
    if (!is.null(problem)) problems <- c(problems, paste0("  ", name, ": ", problem))
  }
  problems
}

expect_input_errors <- function(calls, case, ...) {
  problems <- input_error_problems(calls, ...)
  expect(length(problems) == 0L, paste0("case ", case, ":\n", paste(problems, collapse = "\n")))
}

# One call of `fun` per bad value of `parameter`, named after the value
with_bad_values <- function(fun, args, parameter, values) {
  force(fun)
  calls <- lapply(values, function(value) {
    args[parameter] <- list(value)
    function() do.call(fun, args)
  })
  names(calls) <- paste(parameter, "=", vapply(values, function(v) paste(deparse(v), collapse = " "), ""))
  calls
}

# The first catalogue entry of each exported design function
first_catalogue_entries <- function() {
  names(catalogue)[!duplicated(vapply(catalogue, `[[`, character(1), "fun"))]
}

# Counts and scalar controls ----------------------------------------------------

bad_counts <- list(NULL, numeric(), NA_real_, NaN, Inf, -Inf, 0, -1, 2.5, "2", TRUE,
                   c(2, 3), matrix(2))

# name, function, arguments, parameter, bad values; all must fail before any
# random numbers are drawn
count_cases <- local({
  cases <- list()
  add <- function(name, fun, args, parameter, values = bad_counts) {
    cases[[name]] <<- list(fun = fun, args = args, parameter = parameter, values = values)
  }
  labels <- list(c("A", NA), c("A", " "))
  add("RCBD t", "RCBD", list(reps = 2, seed = 9), "t",
      c(bad_counts[-10], list(list(1, 2)), labels))
  add("RCBD t with checks", "RCBD", list(checks = "CK", rep_checks = 1, reps = 2, seed = 9), "t",
      list(0, NA_real_, NaN, Inf, -Inf, 2.5, matrix(3)))
  add("latin_square t", "latin_square", list(seed = 9), "t", c(bad_counts, list(list(2))))

  blocks <- list(incomplete_blocks = c(t = 12, k = 3), alpha_lattice = c(t = 12, k = 3),
                 square_lattice = c(t = 16, k = 4), rectangular_lattice = c(t = 12, k = 3))
  for (fun in names(blocks)) for (supplied in c(FALSE, TRUE)) for (parameter in c("k", "reps")) {
    args <- c(as.list(blocks[[fun]]), list(reps = 2, seed = 19))
    if (supplied) args$data <- data.frame(ENTRY = seq_len(args$t), TREATMENT = paste0("T", seq_len(args$t)))
    add(paste(fun, parameter, if (supplied) "with data" else "generated"), fun, args, parameter)
  }
  for (fun in names(blocks)) {
    add(paste(fun, "t"), fun, list(k = 2, reps = 2, seed = 19), "t",
        list(NULL, numeric(), NA_real_, NaN, Inf, -Inf, 0, -1, 2.5, TRUE, list(1, 2), matrix(1:4),
             c("A", NA, "C", "D"), c("A", " ", "C", "D")))
  }

  classic <- list(
    RCBD = list(list(t = 4), list(data = data.frame(TREATMENT = LETTERS[1:4]))),
    latin_square = list(list(t = 4), list(data = data.frame(ROW = 1:4, COLUMN = 1:4, TREATMENT = LETTERS[1:4]))),
    full_factorial = list(list(setfactors = c(2, 3)),
                          list(data = data.frame(FACTOR = c("A", "A", "B", "B"), LEVEL = c(1, 2, 1, 2)))),
    split_plot = list(list(wp = 3, sp = 2), list(data = data.frame(WP = c("A", "B"), SP = c("a", "b")))),
    split_split_plot = list(list(wp = 3, sp = 2, ssp = 2),
                            list(data = data.frame(WP = c("A", "B"), SP = c("a", "b"), SSP = c("X", "Y")))),
    strip_plot = list(list(Hplots = 3, Vplots = 2), list(data = data.frame(H = c("A", "B"), V = c("a", "b"))))
  )
  for (fun in names(classic)) for (spec in classic[[fun]]) {
    add(paste(fun, "reps", if (is.null(spec$data)) "generated" else "with data"), fun,
        c(spec, list(seed = 41, plotNumber = 101)), "reps")
  }

  for (supplied in c(FALSE, TRUE)) for (parameter in c("t", "nrows", "reps")) {
    args <- list(t = 12, nrows = 3, reps = 2, seed = 41, iterations = 1)
    if (supplied) args$data <- data.frame(ENTRY = 1:12, TREATMENT = paste0("T", 1:12))
    values <- if (parameter == "t") bad_counts[-c(10, 12)] else bad_counts
    add(paste("row_column", parameter, if (supplied) "with data" else "generated"),
        "row_column", args, parameter, values)
  }

  spatial <- list(
    diagonal_arrangement = list(nrows = 15, ncols = 20, lines = 270, checks = 4),
    optimized_arrangement = list(nrows = 12, ncols = 10, lines = 100, rep_checks = 20, checks = 1:5),
    RCBD_augmented = list(lines = 122, checks = 4, b = 5, nrows = 5, ncols = 29, random = FALSE),
    partially_replicated = list(nrows = 8, ncols = 8, repGens = c(50, 7), repUnits = c(1, 2),
                                spread_reps = FALSE))
  for (fun in names(spatial)) for (parameter in c("nrows", "ncols")) {
    add(paste(fun, parameter), fun, c(spatial[[fun]], list(seed = 19, year = 2026)), parameter,
        c(bad_counts[-c(1, 12)], list(list(2))))
  }
  cases
})

test_that("malformed counts fail with an input error before random numbers are drawn", {
  for (name in names(count_cases)) {
    case <- count_cases[[name]]
    expect_input_errors(with_bad_values(case$fun, case$args, case$parameter, case$values),
                        name, rng = TRUE)
  }
})

test_that("other counts, sizes and choices are validated as classed input errors", {
  unreplicated <- list(
    diagonal_arrangement = list(nrows = 15, ncols = 20, lines = 270, checks = 4),
    optimized_arrangement = list(nrows = 12, ncols = 10, lines = 100, rep_checks = 20, checks = 1:5),
    RCBD_augmented = list(lines = 50, checks = 3, b = 5, repsExpt = 1))
  values <- list(numeric(), NA_real_, NaN, Inf, -Inf, 0, -1, 2.5, "2", TRUE, matrix(2))
  cases <- list()
  for (fun in names(unreplicated)) {
    parameters <- if (fun == "RCBD_augmented") c("lines", "checks", "b", "repsExpt") else "lines"
    for (parameter in parameters) {
      cases[[paste(fun, parameter)]] <- with_bad_values(
        fun, c(unreplicated[[fun]], list(seed = 19, year = 2026)), parameter, values)
    }
  }
  for (fun in c("full_factorial", "split_plot", "split_split_plot")) {
    args <- c(list(full_factorial = list(setfactors = c(2, 3)), split_plot = list(wp = 3, sp = 2),
                   split_split_plot = list(wp = 3, sp = 2, ssp = 2))[[fun]], list(reps = 2, seed = 41))
    cases[[paste(fun, "type")]] <- with_bad_values(fun, args, "type", list(
      NULL, numeric(), NA_real_, NaN, Inf, -Inf, 0, 3, 1.5, "2", TRUE, c(1, 2), matrix(2)))
  }
  cases[["partially_replicated per-location dimensions"]] <- unlist(lapply(c("nrows", "ncols"), function(parameter) {
    with_bad_values("partially_replicated",
                    list(nrows = c(8, 8), ncols = c(8, 8), l = 2, repGens = c(50, 7), repUnits = c(1, 2),
                         spread_reps = FALSE, plotNumber = c(101, 1001), seed = 19, year = 2026),
                    parameter, list(c(8, NA), c(8, Inf), c(8, 0), c(8, 8.5)))
  }), recursive = FALSE)
  # field books whose size overflows the integer range are refused before allocation
  cases[["field-book size bounds"]] <- list(
    RCBD = function() RCBD(t = .Machine$integer.max, reps = 2),
    incomplete_blocks_k1 = function() incomplete_blocks(t = 12, k = 1, reps = 2, seed = 19),
    incomplete_blocks_reps = function() incomplete_blocks(t = 12, k = 3, reps = .Machine$integer.max, seed = 19),
    incomplete_blocks_l = function() incomplete_blocks(t = 12, k = 3, reps = 2, l = .Machine$integer.max, seed = 19))
  cases[["complete-block checks and labels"]] <- c(
    with_bad_values("RCBD", list(t = 3, reps = 2, rep_checks = 1, seed = 9), "checks",
                    list(c("CK", NA_character_), c("CK", " "), matrix(1))),
    duplicate_labels = function() RCBD(data = data.frame(T = c("A", "A", "B")), reps = 2, seed = 9))
  bad_tables <- list(data.frame(), data.frame(T = character()), data.frame(T = c(NA, NA)),
                     data.frame(T = c("CK", " ")))
  for (checks in list(NULL, "CK")) {
    cases[[paste("RCBD entry table, checks", toString(checks))]] <- with_bad_values(
      "RCBD", list(reps = 2, checks = checks, rep_checks = 1, seed = 9), "data", bad_tables)
  }
  cases[["latin_square entry table"]] <- with_bad_values("latin_square", list(seed = 9), "data", list(
    data.frame(), data.frame(R = 1:3), data.frame(R = 1:3, C = 1:3), data.frame(R = 1, C = 1, T = "A"),
    data.frame(R = numeric(), C = numeric(), T = character()),
    data.frame(R = c(NA, NA), C = 1:2, T = c("a", "b")), data.frame(R = 1:3, C = 1:3, T = c("a", " ", "b"))))
  cases[["lattice one-column entry table"]] <- list(
    square_lattice = function() square_lattice(t = 16, k = 4, reps = 2, seed = 19, data = data.frame(ENTRY = 1:16)),
    rectangular_lattice = function() rectangular_lattice(t = 12, k = 3, reps = 2, seed = 19,
                                                         data = data.frame(ENTRY = 1:12)))
  for (name in names(cases)) expect_input_errors(cases[[name]], name)
})

test_that("every public Boolean control rejects a missing value before design work", {
  # every logical scalar formal of every engine is a Boolean control
  engines <- unique(unname(fieldhub_engine_registry()))
  for (engine in engines) {
    defaults <- formals(get(engine))
    flags <- names(defaults)[vapply(defaults, function(v) is.logical(v) && length(v) == 1L, logical(1))]
    if (!length(flags)) next
    entry <- names(catalogue)[vapply(catalogue, function(x) x$fun == engine, logical(1))][1L]
    problems <- if (is.na(entry)) "no catalogue entry to build it from" else {
      arguments <- catalogue_design(entry)$metadata$parameters
      unlist(lapply(flags, function(flag) {
        arguments[flag] <- list(NA)
        set.seed(817)
        before <- .Random.seed
        error <- tryCatch(do.call(get(engine), arguments), error = identity)
        ok <- inherits(error, "fieldhub_input_error") && identical(error$argument, flag) &&
          identical(error$value, NA) && identical(error$options, c(FALSE, TRUE)) &&
          identical(.Random.seed, before)
        if (!ok) flag
      }))
    }
    expect(length(problems) == 0L, sprintf("%s: %s = NA not rejected with argument, value and options",
                                           engine, toString(problems)))
  }
})

test_that("design option errors carry the argument, the choices and the value", {
  check <- function(call, argument, choices) {
    failed <- character()
    for (value in list("invalid", NA_character_, "", 1, c("one", "two"), character())) {
      err <- tryCatch(call(value), error = identity)
      if (!(inherits(err, "fieldhub_input_error") && identical(err$argument, argument) &&
            identical(err$choices, choices) && identical(err$value, value))) {
        failed <- c(failed, paste(deparse(value), collapse = " "))
      }
    }
    expect(length(failed) == 0L, sprintf("%s: %s", argument, toString(failed)))
  }
  check(function(v) row_column(t = 12, nrows = 3, reps = 2, method = v, seed = 1),
        "method", c("onestage", "twostage"))
  check(function(v) RCBD_augmented(lines = 20, checks = 2, b = 2, repsStack = v, seed = 1),
        "repsStack", c("vertical", "horizontal"))
  # an unambiguous abbreviation selects the choice
  abbreviated <- RCBD_augmented(lines = 20, checks = 2, b = 4, repsExpt = 2, repsStack = "hor", seed = 1)
  full <- RCBD_augmented(lines = 20, checks = 2, b = 4, repsExpt = 2, repsStack = "horizontal", seed = 1)
  expect_identical(abbreviated$fieldBook, full$fieldBook)
})

# Locations -----------------------------------------------------------------------

test_that("every location-aware engine rejects a malformed number of locations", {
  for (entry in catalogue[first_catalogue_entries()]) {
    if (!"l" %in% names(formals(getExportedValue("FielDHub", entry$fun)))) next
    call <- if (entry$fun == "split_families") {
      quote(split_families(data = data.frame(ENTRY = 1:4, NAME = letters[1:4], FAMILY = 1)))
    } else {
      body(entry$build)[[2]]
    }
    args <- as.list(call)[-1]
    values <- list(NULL, NA_real_, Inf, 0, -1, 1.5, "2", 1 + 1i, c(1, 2), matrix(2),
                   .Machine$integer.max + 1)
    calls <- lapply(values, function(value) {
      args["l"] <- list(value)
      invalid <- as.call(c(list(as.name(entry$fun)), args))
      function() {
        e <- tryCatch(eval(invalid), error = identity)
        if (inherits(e, "error") && !grepl("number of locations", conditionMessage(e))) {
          stop("the error does not name the number of locations: ", conditionMessage(e))
        }
        if (inherits(e, "error")) stop(e)
      }
    })
    names(calls) <- vapply(values, function(v) paste(deparse(v), collapse = " "), "")
    expect_input_errors(calls, entry$fun)
  }
})

test_that("location names are kept as given and recorded in the field book", {
  values <- list(character = c("A", "B"), named = c(a = "A", b = "B"), integer = 1:2,
                 double = c(1, 2), factor = factor(c("A", "B"), levels = c("unused", "B", "A")))
  for (name in names(values)) {
    x <- RCBD(t = 4, reps = 2, l = 2, locationNames = values[[name]], plotNumber = c(101, 1001), seed = 19)
    expect_identical(as.character(unique(x$fieldBook$LOCATION)), as.character(unname(values[[name]])),
                     info = name)
  }
  # names of the wrong length fall back to defaults with a warning, even repeated ones
  expect_warning(RCBD(t = 4, reps = 2, l = 1, locationNames = c("A", "A"), seed = 19),
                 class = "fieldhub_default_warning")
})

test_that("location names that are malformed or collapse locations are rejected by every engine", {
  case_collision <- list(
    RCBD = function() RCBD(t = 4, reps = 2, l = 2, locationNames = c("a", "A"),
                           plotNumber = c(101, 1001), seed = 19),
    RCBD_augmented = function() RCBD_augmented(lines = 40, checks = 4, b = 4, l = 2, locationNames = c("a", "A"),
                                               plotNumber = c(101, 1001), seed = 19))
  expect_input_errors(case_collision, "names equal up to case")
  for (name in first_catalogue_entries()) {
    engine <- catalogue[[name]]$fun
    formal_names <- names(formals(get(engine)))
    if (!"locationNames" %in% formal_names) next
    args <- catalogue_design(name)$metadata$parameters
    locations <- if (is.null(args[["l"]])) 1 else args[["l"]]
    calls <- with_bad_values(engine, args, "locationNames", list(
      rep(NA_character_, locations), rep("", locations), rep("  ", locations),
      as.list(rep("A", locations)), matrix(rep("A", locations))))
    if ("l" %in% formal_names) {
      # one name for several fields
      args$l <- max(2, args$l)
      args$plotNumber <- 101 + seq_len(args$l) * 1000
      calls[["duplicate names"]] <- with_bad_values(engine, args, "locationNames", list(rep("A", args$l)))[[1]]
    }
    expect_input_errors(calls, engine)
  }
})

# Replication -------------------------------------------------------------------

replication_alias_cases <- list(
  incomplete_blocks = list(t = 12, k = 3),
  alpha_lattice = list(t = 12, k = 3),
  square_lattice = list(t = 16, k = 4),
  rectangular_lattice = list(t = 12, k = 3),
  row_column = list(t = 24, nrows = 6, iterations = 10),
  strip_plot = list(Hplots = 3, Vplots = 2)
)

test_that("legacy replication spellings warn as deprecated and build the reps design", {
  for (name in names(replication_alias_cases)) {
    fun <- get(name)
    args <- c(replication_alias_cases[[name]], list(plotNumber = 101, seed = 29))
    old <- if (name == "strip_plot") "b" else "r"
    expect_silent(canonical <- do.call(fun, c(args, list(reps = 2))))
    expect_warning(legacy <- do.call(fun, c(args, stats::setNames(list(2), old))),
                   "reps", class = "fieldhub_deprecated_warning")
    expect_warning(positional <- do.call(fun, c(unname(args[1:2]), list(2), args[-(1:2)])),
                   "reps", class = "fieldhub_deprecated_warning")
    expect_identical(list(legacy, positional), list(canonical, canonical), info = name)
  }
})

test_that("reps together with a legacy replication spelling is rejected", {
  for (name in names(replication_alias_cases)) {
    old <- if (name == "strip_plot") "b" else "r"
    args <- c(replication_alias_cases[[name]], list(reps = 2), stats::setNames(list(3), old))
    expect_error(do.call(name, args), "Supply only", class = "fieldhub_input_error", info = name)
  }
})

test_that("one replicate is rejected as not enough replication by every resolvable design", {
  calls <- list(
    incomplete_blocks = list(t = 20, k = 4, reps = 1),
    alpha_lattice = list(t = 20, k = 4, reps = 1),
    square_lattice = list(t = 16, k = 4, reps = 1),
    rectangular_lattice = list(t = 20, k = 4, reps = 1),
    row_column = list(t = 20, nrows = 4, reps = 1, method = "twostage")
  )
  for (caller in names(calls)) {
    expect_error(do.call(caller, c(calls[[caller]], seed = 1)),
                 paste0("^", caller, "\\(\\) cannot build a resolvable design.*not enough replication"),
                 class = "fieldhub_input_error", info = caller)
  }
})

test_that("optimized_arrangement() takes rep_checks and deprecates amountChecks", {
  args <- list(nrows = 10, ncols = 20, lines = 160, checks = 1:4, seed = 1)
  a <- do.call(optimized_arrangement, c(args, list(rep_checks = c(10, 10, 10, 10))))
  expect_warning(b <- do.call(optimized_arrangement, c(args, list(amountChecks = c(10, 10, 10, 10)))),
                 class = "fieldhub_deprecated_warning")
  expect_identical(a$fieldBook, b$fieldBook)
  # the replay parameters use the current spelling
  expect(identical(a$metadata$parameters$rep_checks, c(10, 10, 10, 10)) &&
           !"amountChecks" %in% names(a$metadata$parameters),
         "the replay parameters do not record rep_checks alone")
  expect_error(do.call(optimized_arrangement, c(args, list(rep_checks = c(10, 10, 10, 10),
                                                           amountChecks = c(10, 10, 10, 10)))),
               class = "fieldhub_input_error")
  # an invalid value reports `rep_checks` under either spelling
  e1 <- tryCatch(do.call(optimized_arrangement, c(args, list(rep_checks = "20"))), error = identity)
  expect_warning(e2 <- tryCatch(do.call(optimized_arrangement, c(args, list(amountChecks = "20"))),
                                error = identity),
                 class = "fieldhub_deprecated_warning")
  for (e in list(rep_checks = e1, amountChecks = e2)) {
    expect(inherits(e, "fieldhub_input_error") && identical(e$argument, "rep_checks") &&
             grepl("`rep_checks`", conditionMessage(e), fixed = TRUE) &&
             !grepl("amountChecks", conditionMessage(e), fixed = TRUE),
           paste("the error does not report `rep_checks`:", conditionMessage(e)))
  }
})

# Errors any public function gives ---------------------------------------------

test_that("public design functions use the shared input-error contract", {
  calls <- list(
    CRD = function() CRD(reps = 2, data = "not a data frame"),
    RCBD = function() RCBD(t = 4, reps = 2, planter = "diagonal"),
    latin_square = function() latin_square(t = 3, planter = "diagonal"),
    latin_square_one_treatment = function() latin_square(t = 1, seed = 1),
    full_factorial = function() full_factorial(setfactors = c(2, 2), planter = "diagonal"),
    split_plot = function() split_plot(wp = 2, sp = 2, reps = 2, type = 3),
    split_split_plot = function() split_split_plot(wp = 2, sp = 2, ssp = 2, reps = 2, type = 3),
    strip_plot = function() strip_plot(Hplots = 2, Vplots = 2, reps = 2, data = "not a data frame"),
    split_families = function() split_families(data = data.frame(ENTRY = 1:4, NAME = letters[1:4],
                                                                 FAMILY = rep(1:2, 2))),
    split_families_no_locations = function() split_families(data = data.frame(
      ENTRY = 1:20, NAME = paste0("G", 1:20), FAMILY = rep(1:4, 5))),
    incomplete_blocks = function() incomplete_blocks(t = 12, k = 3, reps = 2, data = "not a data frame"),
    row_column = function() row_column(t = 12, nrows = 3, reps = 2, data = "not a data frame"),
    square_lattice = function() square_lattice(t = 16, k = 4, reps = 2, data = "not a data frame"),
    square_lattice_not_square = function() square_lattice(t = 15, k = 4, reps = 2),
    rectangular_lattice = function() rectangular_lattice(t = 12, k = 3, reps = 2, data = "not a data frame"),
    alpha_lattice = function() alpha_lattice(t = 12, k = 3, reps = 2, data = "not a data frame"),
    RCBD_augmented = function() RCBD_augmented(planter = "diagonal"),
    diagonal_arrangement = function() diagonal_arrangement(planter = "diagonal"),
    optimized_arrangement = function() optimized_arrangement(planter = "diagonal"),
    partially_replicated = function() partially_replicated(planter = "diagonal"),
    sparse_allocation_no_dimensions = function() sparse_allocation(lines = 60, l = 3, copies_per_entry = 2,
                                                                   checks = 3, seed = 1),
    do_optim = function() do_optim(lines = 20, l = 2, design = "unknown")
  )
  for (name in names(calls)) expect_input_errors(calls[name], name)
})

test_that("every exported planter check gives the same classed error with the options", {
  calls <- list(
    RCBD = function() RCBD(t = 4, reps = 2, planter = "zigzag", seed = 1),
    full_factorial = function() full_factorial(setfactors = c(2, 2), planter = "zigzag", seed = 1),
    partially_replicated = function() partially_replicated(nrows = 8, ncols = 5, repGens = c(10, 20),
                                                           repUnits = c(2, 1), planter = "zigzag", seed = 1),
    diagonal_arrangement = function() diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4,
                                                           planter = "zigzag", seed = 1),
    latin_square = function() latin_square(t = 4, planter = "zigzag", seed = 1),
    RCBD_augmented = function() RCBD_augmented(lines = 50, checks = 3, b = 6, planter = "zigzag", seed = 1),
    optimized_arrangement = function() optimized_arrangement(nrows = 10, ncols = 20, lines = 160, checks = 1:4,
                                                             rep_checks = c(10, 10, 10, 10), planter = "zigzag",
                                                             seed = 1),
    strip_plot = function() strip_plot(Hplots = 3, Vplots = 2, reps = 2, planter = "zigzag", seed = 1),
    multi_location_prep = function() multi_location_prep(lines = 80, l = 4, copies_per_entry = 5, checks = 2,
                                                         rep_checks = c(4, 4), planter = "zigzag", seed = 1),
    sparse_allocation = function() sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4,
                                                     planter = "zigzag", seed = 1)
  )
  for (name in names(calls)) {
    e <- tryCatch(calls[[name]](), error = identity)
    expect(inherits(e, "fieldhub_input_error") && identical(e$options, c("serpentine", "cartesian")),
           sprintf("%s: no planter input error with the options", name))
  }
})

test_that("degenerate factors and replication give classed errors that name the called function", {
  # These used to leak unclassed errors: base R's "incorrect number of
  # dimensions" for a one-level sub- or strip-plot factor, and "Too many
  # parameters for the available plots" for under-replicated block designs.
  expect_input_errors(list(
    split_plot = function() split_plot(wp = 2, sp = 1, reps = 2, seed = 1),
    split_split_plot = function() split_split_plot(wp = 2, sp = 2, ssp = 1, reps = 2, seed = 1),
    strip_plot = function() strip_plot(Hplots = 2, Vplots = 1, reps = 2, seed = 1),
    # row_column() first falls back from "onestage" to "twostage" with a warning
    row_column = function() row_column(t = 4, nrows = 2, reps = 1, seed = 1)
  ), "degenerate factors")
  # The lattices build through incomplete_blocks() and used to report its
  # name; full_factorial() of a single run reported RCBD().
  named <- list(
    alpha_lattice = quote(alpha_lattice(t = 16, k = 4, reps = 1, seed = 1)),
    alpha_lattice = quote(alpha_lattice(t = 16, k = 20, reps = 2, seed = 1)),
    square_lattice = quote(square_lattice(t = 16, k = 4, reps = 1, seed = 1)),
    rectangular_lattice = quote(rectangular_lattice(t = 12, k = 3, reps = 1, seed = 1)),
    incomplete_blocks = quote(incomplete_blocks(t = 4, k = 2, reps = 1, seed = 1)),
    incomplete_blocks = quote(incomplete_blocks(t = 4, k = 20, reps = 2, seed = 1)),
    incomplete_blocks = quote(incomplete_blocks(t = 12, k = 5, reps = 2, seed = 1)),
    row_column = quote(row_column(t = 12, nrows = 5, reps = 2, seed = 1)),
    full_factorial = quote(full_factorial(setfactors = c(1, 1), reps = 2, seed = 1))
  )
  others <- c("incomplete_blocks", "RCBD")
  for (i in seq_along(named)) {
    fn <- names(named)[i]
    e <- tryCatch(suppressWarnings(eval(named[[i]])), error = identity)
    message <- if (inherits(e, "error")) conditionMessage(e) else "no error"
    wrong <- setdiff(others, fn)
    expect(inherits(e, "fieldhub_input_error") && grepl(fn, message, fixed = TRUE) &&
             !any(vapply(wrong, grepl, logical(1), x = message, fixed = TRUE)),
           sprintf("%s: %s", deparse(named[[i]]), message))
  }
  # the called function's name is not recorded as a replay parameter
  ibd <- incomplete_blocks(t = 12, k = 4, reps = 2, seed = 1)
  expect_false("caller" %in% names(ibd$metadata$parameters))
})

test_that("field dimensions that do not fit raise a dimension error with the valid options", {
  # These printed a banner and returned NULL, or for RCBD_augmented() a data
  # frame of options instead of a design.
  calls <- list(
    diagonal_arrangement = quote(diagonal_arrangement(nrows = 7, ncols = 7, lines = 100, checks = 4)),
    optimized_arrangement = quote(optimized_arrangement(nrows = 7, ncols = 7, lines = 100,
                                                        rep_checks = 20, checks = 1:5)),
    partially_replicated = quote(partially_replicated(nrows = 7, ncols = 7, repGens = c(50, 7),
                                                      repUnits = c(1, 2))),
    RCBD_augmented = quote(RCBD_augmented(lines = 20, checks = 3, b = 4, nrows = 7, ncols = 7))
  )
  for (fn in names(calls)) {
    error <- NULL
    printed <- utils::capture.output(error <- tryCatch(suppressWarnings(eval(calls[[fn]])), error = identity))
    expect(inherits(error, "fieldhub_dimension_error") && grepl(fn, conditionMessage(error), fixed = TRUE) &&
             is.data.frame(error$options) && nrow(error$options) > 0L && length(printed) == 0L,
           sprintf("%s: no dimension error with options, or output printed", fn))
  }
})

test_that("augmented RCBD's dimension error lists exactly the block counts it accepts", {
  accepted <- Filter(function(b) {
    !inherits(try(RCBD_augmented(lines = 20, checks = 4, b = b, seed = 1), silent = TRUE), "try-error")
  }, 1:10)
  # the explicit nrows/ncols path used to list a narrower range from 5 blocks
  calls <- list(too_few_blocks = quote(RCBD_augmented(lines = 20, checks = 4, b = 2, seed = 1)),
                explicit_dimensions = quote(RCBD_augmented(lines = 20, checks = 4, b = 3, nrows = 2,
                                                           ncols = 2, seed = 1)))
  for (name in names(calls)) {
    e <- tryCatch(eval(calls[[name]]), error = identity)
    # each option line of the message reads "<b> blocks: <rows> x <cols>"
    listed <- vapply(accepted, function(b) grepl(paste0("\n\\s*", b, " blocks: "), conditionMessage(e)),
                     logical(1))
    expect(inherits(e, "fieldhub_dimension_error") && all(listed),
           sprintf("%s: accepted block counts not listed: %s", name, toString(accepted[!listed])))
  }
})

test_that("plot() of an unavailable view is a classed error with the options", {
  # plot_layout() used to warn and return NULL, and plot.FielDHub() then
  # raised a generic error without the valid options
  rcbd <- RCBD(t = 6, reps = 3, seed = 1)
  e <- expect_error(plot(rcbd, l = 3), class = "fieldhub_input_error")
  expect_true(length(e$options) >= 1L)
  expect_warning(expect_error(plot(rcbd, layout = 99), "available layout option",
                              class = "fieldhub_input_error"), NA)
})

test_that("the app shows FielDHub errors as validation messages", {
  skip_if_not_installed("shiny")
  err <- tryCatch(validate_design(CRD(t = 5, reps = 3, plotNumber = 0)), error = identity)
  expect(inherits(err, "shiny.silent.error") &&
           grepl("plotNumber must be an integer greater than 0", conditionMessage(err)),
         "the input error is not shown as a validation message")
})

# Valid edge inputs ----------------------------------------------------------------

test_that("edge inputs that are still meaningful designs build", {
  crd <- CRD(t = factor(c("A", "B", "C")), reps = 2, seed = 1)
  expect_identical(c(table(crd$fieldBook$TREATMENT)), c(A = 2L, B = 2L, C = 2L))
  # a count of whole plots with sub-plot labels
  spd <- split_plot(wp = 3, sp = c("a", "b"), reps = 2, seed = 1)
  sspd <- split_split_plot(wp = 3, sp = c("a", "b"), ssp = 2, reps = 2, seed = 1)
  for (case in list(split_plot = list(spd$fieldBook, 3 * 2 * 2),
                    split_split_plot = list(sspd$fieldBook, 3 * 2 * 2 * 2))) {
    book <- case[[1]]
    expect(setequal(book$WHOLE_PLOT, 1:3) && setequal(book$SUB_PLOT, c("a", "b")) && nrow(book) == case[[2]],
           "a whole-plot count with sub-plot labels gives the wrong field book")
  }
  # a single whole plot, or a single replicate
  for (call in list(quote(split_plot(wp = 1, sp = 2, reps = 2, seed = 1)),
                    quote(split_split_plot(wp = 1, sp = 2, ssp = 2, reps = 2, seed = 1)),
                    quote(split_plot(wp = 2, sp = 2, reps = 1, seed = 1)))) {
    expect_s3_class(eval(call), "FielDHub")
  }
})

# Plot numbers and location names replaced by defaults ---------------------------

test_that("each design's default plot numbers build and replay without a warning", {
  # each catalogue entry's own arguments, with l = 2 where the entry has no l,
  # and without plotNumber, so the function uses its own default
  for (entry in catalogue[first_catalogue_entries()]) {
    fun <- getExportedValue("FielDHub", entry$fun)
    if (!all(c("l", "plotNumber") %in% names(formals(fun)))) next
    args <- as.list(body(entry$build)[[2]])[-1]
    if (is.null(args$l)) args$l <- 2
    args$plotNumber <- NULL
    warnings <- character()
    collect <- function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
    x <- withCallingHandlers(eval(as.call(c(list(as.name(entry$fun)), args))), warning = collect)
    replay <- withCallingHandlers(reproduce_design(x), warning = collect)
    expect(length(warnings) == 0L && identical(replay$fieldBook, x$fieldBook),
           sprintf("%s: %s", entry$fun, if (length(warnings)) warnings[1] else "replay differs"))
  }
})

test_that("wrong-length plot numbers warn with a classed default", {
  calls <- list(
    RCBD = function() RCBD(t = 4, reps = 2, l = 2, plotNumber = 101, seed = 1),
    full_factorial = function() full_factorial(setfactors = c(2, 2), reps = 2, l = 2, plotNumber = 101, seed = 1),
    incomplete_blocks = function() incomplete_blocks(t = 12, k = 4, reps = 2, l = 2, plotNumber = 101, seed = 1),
    row_column = function() row_column(t = 12, nrows = 3, reps = 2, l = 2, plotNumber = 101, seed = 1,
                                       iterations = 20),
    row_column_twostage = function() row_column(t = 12, nrows = 3, reps = 2, l = 2, plotNumber = 101, seed = 1,
                                                method = "twostage", iterations = 50),
    strip_plot = function() strip_plot(Hplots = 3, Vplots = 2, reps = 2, l = 2, plotNumber = 101, seed = 1),
    RCBD_augmented = function() RCBD_augmented(lines = 40, checks = 4, b = 4, l = 2, plotNumber = c(1, 2, 3),
                                               seed = 1),
    diagonal_arrangement = function() diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4,
                                                           l = 2, plotNumber = 101, seed = 1),
    optimized_arrangement = function() optimized_arrangement(nrows = 12, ncols = 10, lines = 100, rep_checks = 20,
                                                             checks = 1:5, l = 2, plotNumber = 101, seed = 1),
    partially_replicated = function() partially_replicated(nrows = 8, ncols = 8, repGens = c(50, 7),
                                                           repUnits = c(1, 2), l = 2, plotNumber = 101, seed = 1),
    sparse_allocation = function() sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4,
                                                     plotNumber = c(1, 1001), seed = 1234)
  )
  arguments <- character()
  for (fn in names(calls)) {
    w <- expect_warning(utils::capture.output(calls[[fn]]()),
                        "'plotNumber' has [0-9]+ value\\(s\\) for [0-9]+ location\\(s\\)",
                        class = "fieldhub_default_warning", info = fn)
    arguments[fn] <- w$argument
  }
  expect_identical(arguments[arguments != "plotNumber"], stats::setNames(character(), character()))
  # the warning says what was supplied and what was used; RCBD() used to print
  # "Since plotNumber was missing" to stdout instead
  output <- utils::capture.output(w <- expect_warning(
    RCBD(t = 4, reps = 2, l = 2, plotNumber = 101, seed = 1), class = "fieldhub_default_warning"))
  expect(identical(w$supplied, 101) && length(w$used) == 2L && length(output) == 0L,
         "the plotNumber warning does not report the supplied and used values, or output was printed")
  for (fun in list(split_plot = function() split_plot(wp = 3, sp = 2, reps = 2, plotNumber = NULL, seed = 1),
                   split_split_plot = function() split_split_plot(wp = 2, sp = 2, ssp = 2, reps = 2,
                                                                  plotNumber = NULL, seed = 1))) {
    w <- expect_warning(fun(), class = "fieldhub_default_warning")
    expect_equal(w[c("argument", "supplied")], list(argument = "plotNumber", supplied = NULL))
  }
})

test_that("wrong-length location names warn with a classed default", {
  names3 <- c("A", "B", "C")
  calls <- list(
    RCBD = function() RCBD(t = 4, reps = 2, l = 2, plotNumber = c(1, 101), locationNames = names3, seed = 1),
    full_factorial = function() full_factorial(setfactors = c(2, 2), reps = 2, l = 2, plotNumber = c(1, 101),
                                               locationNames = names3, seed = 1),
    incomplete_blocks = function() incomplete_blocks(t = 12, k = 4, reps = 2, l = 2, plotNumber = c(1, 101),
                                                     locationNames = names3, seed = 1),
    alpha_lattice = function() alpha_lattice(t = 12, k = 4, reps = 2, l = 2, plotNumber = c(1, 101),
                                             locationNames = names3, seed = 1),
    square_lattice = function() square_lattice(t = 16, k = 4, reps = 2, l = 2, plotNumber = c(1, 101),
                                               locationNames = names3, seed = 1),
    rectangular_lattice = function() rectangular_lattice(t = 12, k = 3, reps = 2, l = 2, plotNumber = c(1, 101),
                                                         locationNames = names3, seed = 1),
    row_column = function() row_column(t = 12, nrows = 3, reps = 2, l = 2, plotNumber = c(1, 101),
                                       locationNames = names3, seed = 1),
    strip_plot = function() strip_plot(Hplots = 3, Vplots = 2, reps = 2, l = 2, plotNumber = c(1, 101),
                                       locationNames = "A", seed = 1),
    diagonal_arrangement = function() diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4, l = 2,
                                                           plotNumber = c(1, 1001), locationNames = names3,
                                                           seed = 1),
    RCBD_augmented = function() RCBD_augmented(lines = 40, checks = 4, b = 4, l = 2, plotNumber = c(1, 101),
                                               locationNames = names3, seed = 1),
    sparse_allocation = function() sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4,
                                                     locationNames = paste0("L", 1:5), seed = 1234)
  )
  arguments <- character()
  for (fn in names(calls)) {
    w <- expect_warning(utils::capture.output(calls[[fn]]()),
                        "'locationNames' has [0-9]+ value\\(s\\) for [0-9]+ location\\(s\\)",
                        class = "fieldhub_default_warning", info = fn)
    arguments[fn] <- w$argument
  }
  expect_identical(arguments[arguments != "locationNames"], stats::setNames(character(), character()))
})

# Slow tier ------------------------------------------------------------------------

test_that("incomplete_blocks() builds many blocks of two plots", {
  expect_no_warning(x <- incomplete_blocks(t = 200, k = 2, reps = 3, seed = 1))
  expect_equal(nrow(x$fieldBook), 600)
  # With two replicates of blocks of two, every connected design is one cycle
  # through all treatments, whose A-efficiency is 3 / (v + 1); the engine
  # reports it to seven decimals, hence the 1e-7 tolerance.
  y <- incomplete_blocks(t = 100, k = 2, reps = 2, seed = 1)
  level2 <- y$blocksModel[y$blocksModel$Level == 2, ]
  expect_lte(abs(level2[["A-Efficiency"]] - 3 / 101), 1e-7)
})

test_that("incomplete_blocks() returns an unoptimized resolvable design above the block limit", {
  skip_on_cran()
  # 1002 / 2 * 4 = 2004 blocks, beyond the block search (2000): the design is
  # still built, connected and resolvable, with a warning that it was not
  # optimized.
  expect_warning(x <- incomplete_blocks(t = 1002, k = 2, reps = 4, seed = 1),
                 "not optimized", class = "fieldhub_design_warning")
  book <- x$fieldBook
  level2 <- x$blocksModel[x$blocksModel$Level == 2, ]
  expect(all(table(book$ENTRY, book$REP) == 1) &&
           all(table(interaction(book$REP, book$IBLOCK, drop = TRUE)) == 2) &&
           level2[["A-Efficiency"]] > 0,
         "the unoptimized design is not resolvable in blocks of two, or not connected")
  # Within the block limit but too much work for the search (1000 blocks of
  # 12): not optimized either, and the same seed gives the same field book.
  expect_warning(y <- incomplete_blocks(t = 6000, k = 12, reps = 2, seed = 1),
                 "not optimized", class = "fieldhub_design_warning")
  z <- suppressWarnings(incomplete_blocks(t = 6000, k = 12, reps = 2, seed = 1))
  expect(all(table(y$fieldBook$ENTRY, y$fieldBook$REP) == 1) && identical(y$fieldBook, z$fieldBook),
         "the large design is not resolvable or does not reproduce from its seed")
})
