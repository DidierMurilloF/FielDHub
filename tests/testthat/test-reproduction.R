# Reproducing design results: reproduce_design(), replay of the recorded
# parameters with do.call(), the random-number state around a replay, and
# compatibility with results and scripts written for the previous release.

# Replay the recorded parameters of a result through its engine.
replay_recorded <- function(x, fun) do.call(fun, x$metadata$parameters)

# Run `expr`, muffling warnings, and return its value with the classes of
# any warning that is not one of `allowed`.
with_warning_classes <- function(expr, allowed) {
  unexpected <- character(0)
  value <- withCallingHandlers(expr, warning = function(w) {
    if (!any(class(w) %in% allowed)) {
      unexpected <<- c(unexpected, paste0(class(w)[1], ": ", conditionMessage(w)))
    }
    invokeRestart("muffleWarning")
  })
  list(value = value, unexpected = unexpected)
}

# reproduce_design() on the catalogue ---------------------------------------

test_that("reproduce_design rebuilds every recorded catalogue result", {
  differs <- Filter(function(name) {
    x <- catalogue_design(name)
    replay <- suppressWarnings(reproduce_design(x))
    # Wall-clock search timings are not deterministic content
    if (identical(catalogue[[name]]$fun, "augmented_row_column")) {
      replay <- arc_deterministic_view(replay)
      x <- arc_deterministic_view(x)
    }
    !identical(replay, x)
  }, names(catalogue))
  expect_identical(differs, character(0))
})

# Recorded parameters replay with do.call() ---------------------------------

# Each row builds a result, optionally names a recorded value and what it
# must be, and the result must equal a replay of its recorded parameters.
recorded_cases <- local({
  rows <- list()
  add <- function(name, fun, build, recorded = function(x) TRUE, want = TRUE,
                  replay = function(x) replay_recorded(x, fun)) {
    rows[[name]] <<- list(fun = fun, build = build, recorded = recorded,
                          want = want, replay = replay)
  }
  # Classic catalogue entries record the seed they used among their parameters
  for (name in c("CRD_count", "CRD_labels", "CRD_data", "RCBD_two_locations",
                 "RCBD_checks", "RCBD_cartesian")) local({
    entry <- name
    add(entry, catalogue[[entry]]$fun, function() catalogue_design(entry),
        function(x) identical(x$metadata$parameters$seed, x$metadata$seed))
  })
  add("RCBD records starting plots, not expanded block plots", "RCBD",
      function() RCBD(t = 5, reps = 3, l = 2, plotNumber = c(101, 1001), seed = 42),
      function(x) x$metadata$parameters$plotNumber, c(101, 1001))
  entries <- data.frame(TREATMENT = c("B", "A", "C"), REPS = c(2, 3, 2))
  add("CRD with caller data and an automatic seed", "CRD",
      function() CRD(data = entries),
      function(x) x$metadata$parameters$data[c("Treatment", "Reps")],
      data.frame(Treatment = entries$TREATMENT, Reps = entries$REPS))
  add("RCBD with an automatic seed records an integer seed", "RCBD",
      function() RCBD(t = 5, reps = 3),
      function(x) typeof(x$metadata$parameters$seed), "integer")
  incomplete <- list(
    incomplete_blocks = list(t = 12, k = 4, reps = 2, seed = 38),
    alpha_lattice = list(t = 12, k = 4, reps = 2, seed = 38),
    square_lattice = list(t = 16, k = 4, reps = 2, seed = 38),
    rectangular_lattice = list(t = 12, k = 3, reps = 2, seed = 38),
    row_column = list(t = 12, nrows = 3, reps = 2, seed = 38)
  )
  for (fun in names(incomplete)) for (supplied in c(FALSE, TRUE)) local({
    engine <- fun
    inputs <- incomplete[[engine]]
    if (supplied) {
      inputs$data <- data.frame(ENTRY = seq_len(inputs$t) + 100L,
                                NAME = paste0("Custom ", seq_len(inputs$t)))
    }
    add(paste(engine, if (supplied) "with supplied entries" else "with generated entries"),
        engine, function() do.call(engine, inputs))
  })
  for (fun in c("split_plot", "split_split_plot")) for (type in 1:2) local({
    engine <- fun
    args <- list(wp = c("Dry", "Wet"), sp = c("Low", "High"), type = type, reps = 2, seed = 38)
    if (engine == "split_split_plot") args$ssp <- c("Early", "Late")
    add(paste(engine, "type", type, "keeps its label vectors"), engine,
        function() do.call(engine, args),
        function(x) x$metadata$parameters[c("wp", "sp")], args[c("wp", "sp")])
  })
  add("full_factorial with generated levels records no data", "full_factorial",
      function() full_factorial(setfactors = c(2, 3), reps = 2, seed = 38),
      function(x) list(x$metadata$parameters$setfactors, x$metadata$parameters$data),
      list(c(2, 3), NULL))
  add("RCBD_augmented records the full field width", "RCBD_augmented",
      function() RCBD_augmented(lines = 50, checks = 3, b = 4, nrows = 2, ncols = 32,
                                seed = 38, year = 2026),
      function(x) x$metadata$parameters$ncols, 32)
  add("optimized_arrangement records custom check entries", "optimized_arrangement",
      function() optimized_arrangement(nrows = 12, ncols = 10, lines = 100, rep_checks = 20,
                                       checks = c(101, 102, 103, 104), seed = 38, year = 2026),
      function(x) x$metadata$parameters$checks, c(101, 102, 103, 104))
  supplied <- list(
    split_plot = list(reps = 2, type = 1, factorLabels = FALSE,
                      data = data.frame(W = c("Dry", "Wet", NA), S = c("A", "B", "C"))),
    split_split_plot = list(reps = 2, type = 1, factorLabels = FALSE,
                            data = data.frame(W = c("Dry", "Wet", NA),
                                              S = c("A", "B", "C"), SS = c("D", "E", "F"))),
    strip_plot = list(reps = 2, plotNumber = 101, factorLabels = FALSE,
                      data = data.frame(H = c("Dry", "Wet", NA), V = c("A", "B", "C"))),
    full_factorial = list(reps = 2, type = 1, factorLabels = FALSE,
                          data = data.frame(F = c("Water", "Water", "Dose", "Dose"),
                                            L = c("Low", "High", "Low", "High"))),
    RCBD_augmented = list(lines = 50, checks = 3, b = 5, year = 2026,
                          data = data.frame(ENTRY = 101:153, NAME = paste0("Custom ", 1:53))),
    optimized_arrangement = list(nrows = 12, ncols = 10, year = 2026,
                                 data = data.frame(ENTRY = 101:204, NAME = paste0("Custom ", 1:104),
                                                   REPS = c(rep(5, 4), rep(1, 100)))),
    partially_replicated = list(nrows = 8, ncols = 8, year = 2026,
                                data = data.frame(ENTRY = 101:157, NAME = paste0("Custom ", 1:57),
                                                  REPS = c(rep(2, 7), rep(1, 50)))),
    diagonal_arrangement = list(nrows = 15, ncols = 20, checks = 4, year = 2026,
                                data = data.frame(ENTRY = 101:374, NAME = paste0("Custom ", 1:274))),
    multi_location_prep = list(lines = 40, l = 3, copies_per_entry = 5, checks = 2,
                               rep_checks = c(4, 4), allow_fillers = TRUE, year = 2026,
                               data = data.frame(ENTRY = 101:142, NAME = paste0("Custom ", 1:42))),
    sparse_allocation = list(lines = 120, l = 4, copies_per_entry = 3, checks = 4, year = 2026,
                             data = data.frame(ENTRY = 101:224, NAME = paste0("Custom ", 1:124)))
  )
  for (fun in names(supplied)) local({
    engine <- fun
    inputs <- c(supplied[[engine]], list(seed = 38))
    add(paste(engine, "replays normalized supplied data"), engine,
        function() do.call(engine, inputs))
  })
  for (latinize in c(FALSE, TRUE)) local({
    L <- latinize
    add(paste("onestage row_column with latinize =", L), "row_column",
        function() row_column(t = 12, nrows = 3, reps = 2, seed = 11, iterations = 20, latinize = L),
        function(x) x$infoDesign$optimization, "onestage",
        replay = reproduce_design)
  })
  rows
})

test_that("recorded parameters hold the evaluated inputs and replay the result", {
  for (name in names(recorded_cases)) {
    row <- recorded_cases[[name]]
    x <- row$build()
    expect_identical(
      list(recorded = row$recorded(x), replays = identical(row$replay(x), x)),
      list(recorded = row$want, replays = TRUE),
      info = name
    )
  }
})

test_that("recording a replication alias does not supply both spellings", {
  expect_warning(x <- incomplete_blocks(t = 12, k = 4, r = 2, seed = 38),
                 class = "fieldhub_deprecated_warning")
  expect_identical(list(x$metadata$parameters$reps, "r" %in% names(x$metadata$parameters)),
                   list(2, FALSE))
  expect_silent(replay <- do.call(incomplete_blocks, x$metadata$parameters))
  expect_identical(replay, x)
})

test_that("incomplete-block designs reproduce from the seed and differ between locations", {
  first <- incomplete_blocks(t = 20, k = 4, reps = 2, l = 3, seed = 7)
  expect_identical(incomplete_blocks(t = 20, k = 4, reps = 2, l = 3, seed = 7), first)
  entries <- split(first$fieldBook$ENTRY, first$fieldBook$LOCATION)
  expect(length(entries) == 3L && length(unique(entries)) > 1L,
         sprintf("expected 3 locations with different randomizations, got %d location(s), %d distinct",
                 length(entries), length(unique(entries))))
  alpha <- alpha_lattice(t = 30, k = 5, reps = 3, seed = 11)
  expect_identical(reproduce_design(alpha), alpha)
})

test_that("reproduce_design replays an old result recorded with amountChecks", {
  # Results saved before rep_checks was the recorded name carry
  # amountChecks; replay must rebuild them without a deprecation warning
  # about an argument the caller never typed.
  x <- optimized_arrangement(nrows = 10, ncols = 20, lines = 160, checks = 1:4,
                             rep_checks = c(10, 10, 10, 10), seed = 40, year = 2026)
  old_shape <- x
  old_shape$metadata$parameters$amountChecks <- old_shape$metadata$parameters$rep_checks
  old_shape$metadata$parameters$rep_checks <- NULL
  replay <- expect_no_warning(reproduce_design(old_shape))
  expect_identical(replay[c("fieldBook", "infoDesign")], x[c("fieldBook", "infoDesign")])
})

# Random-number state around a replay ----------------------------------------

test_that("reconstruction uses recorded RNG settings and restores the caller", {
  local_rng_state()
  old_kind <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old_kind)), add = TRUE, after = FALSE)
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  x <- RCBD(t = 5, reps = 3, seed = 38)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(73)
  caller <- list(kind = RNGkind(), seed = .Random.seed)
  expect_identical(reproduce_design(x), x)
  expect_identical(list(kind = RNGkind(), seed = .Random.seed), caller)
  x$metadata$parameters$t <- NULL
  expect_error(reproduce_design(x), class = "fieldhub_input_error")
  expect_identical(list(kind = RNGkind(), seed = .Random.seed), caller)
})

test_that("reconstruction preserves an initially absent random seed", {
  local_rng_state()
  x <- RCBD(t = 4, reps = 2, seed = 38)
  if (exists(".Random.seed", globalenv(), inherits = FALSE)) {
    rm(".Random.seed", envir = globalenv())
  }
  kind <- RNGkind()
  expect_identical(reproduce_design(x), x)
  expect_identical(list(RNGkind(), exists(".Random.seed", globalenv(), inherits = FALSE)),
                   list(kind, FALSE))
})

test_that("reconstruction restores the caller after invalid RNG metadata", {
  x <- RCBD(t = 4, reps = 2, seed = 38)
  x$metadata$rng_kind <- c("invalid-generator", "Inversion", "Rejection")
  set.seed(73)
  caller <- list(kind = RNGkind(), seed = .Random.seed)
  expect_error(reproduce_design(x), class = "fieldhub_input_error")
  expect_identical(list(kind = RNGkind(), seed = .Random.seed), caller)
})

test_that("four-setting replay either restores all settings or fails without changing the caller", {
  local_rng_state()
  x <- CRD(t = 4, reps = 2, seed = 38)
  x$metadata$rng_kind <- c("Mersenne-Twister", "Inversion", "Rejection", "BTPE")
  set.seed(721)
  caller <- list(kind = RNGkind(), seed = .Random.seed)
  code <- design_call_code(x)
  if ("binom.kind" %in% names(formals(RNGkind))) {
    expect_identical(reproduce_design(x)$metadata$rng_kind, x$metadata$rng_kind)
    expect_identical(eval(parse(text = code))$metadata$rng_kind, x$metadata$rng_kind)
  } else {
    expect_error(reproduce_design(x), "binomial RNG", class = "fieldhub_input_error")
    # The generated script is plain R, so it stops with a base error
    expect_error(eval(parse(text = code)), "recorded binomial RNG requires R >= 4.7")
  }
  expect_identical(list(kind = RNGkind(), seed = .Random.seed), caller)
})

test_that("three-setting records use the historical binomial algorithm on newer R", {
  skip_if_not("binom.kind" %in% names(formals(RNGkind)))
  local_rng_state()
  previous <- RNGkind()
  on.exit(do.call(RNGkind, as.list(previous)), add = TRUE)
  x <- CRD(t = 4, reps = 2, seed = 38)
  x$metadata$rng_kind <- x$metadata$rng_kind[1:3]
  RNGkind(binom.kind = "BTPE")
  expect_identical(
    list(replay = tail(reproduce_design(x)$metadata$rng_kind, 1L),
         script = tail(eval(parse(text = design_call_code(x)))$metadata$rng_kind, 1L),
         caller = tail(RNGkind(), 1L)),
    list(replay = "Buggy BTPE", script = "Buggy BTPE", caller = "BTPE")
  )
})

# Errors and warnings --------------------------------------------------------

test_that("reconstruction refuses results it cannot replay safely", {
  x <- RCBD(t = 4, reps = 2, seed = 38)
  tamper <- function(f) {
    bad <- x
    f(bad)
  }
  crd <- CRD(t = 4, reps = 2, seed = 38)
  cases <- list(
    `not a result` = list(1, "fieldhub_input_error"),
    `no recorded parameters` = list(
      tamper(function(b) {b$metadata$parameters <- NULL; b}), "fieldhub_input_error"),
    `no metadata` = list(tamper(function(b) {b$metadata <- NULL; b}), "fieldhub_input_error"),
    # Only registered public design engines are resolved, never any function
    `unsupported engine` = list(tamper(function(b) {
      b$metadata$design <- "system"
      class(b) <- c("fieldhub_system", "FielDHub")
      b
    }), "fieldhub_input_error"),
    # Recorded arguments are values; a call stored there is not evaluated
    `recorded expression` = list(local({
      crd$metadata$parameters$data <- quote(stop("must not execute"))
      crd
    }), "fieldhub_input_error"),
    `unnamed parameters` = list(tamper(function(b) {b$metadata$parameters <- 1; b}),
                                "fieldhub_internal_error"),
    `unnamed seed parameter` = list(tamper(function(b) {b$metadata$parameters <- list(38); b}),
                                    "fieldhub_internal_error"),
    `parameter seed disagrees` = list(tamper(function(b) {b$metadata$parameters <- list(seed = 39); b}),
                                      "fieldhub_internal_error"),
    `duplicate parameter names` = list(
      tamper(function(b) {b$metadata$parameters <- list(seed = 38, seed = 38); b}),
      "fieldhub_internal_error")
  )
  for (name in names(cases)) {
    expect_error(reproduce_design(cases[[name]][[1]]), class = cases[[name]][[2]], info = name)
  }
})

test_that("reconstruction warns about a different package version", {
  x <- RCBD(t = 4, reps = 2, seed = 38)
  x$metadata$package_version <- "0.0.0"
  expect_warning(replay <- reproduce_design(x), class = "fieldhub_reproduction_warning")
  expect_identical(replay$fieldBook, x$fieldBook)
})

# Results and scripts from the previous release ------------------------------

# Real output of the previous release (tools/generate-legacy-fixtures.R): the
# calls its fixture script made and the results it saved.
historical_bundle <- readRDS(test_path("fixtures", "v1.5.0-designs.rds"))

# Numerical optimizations were recorded on macOS; other BLAS builds can pick
# different equivalent layouts, so literal positions are compared only there.
legacy_optimized_engines <- c("incomplete_blocks", "alpha_lattice", "square_lattice",
                              "rectangular_lattice", "sparse_allocation",
                              "multi_location_prep", "row_column")
legacy_classical_engines <- c("incomplete_blocks", "alpha_lattice", "square_lattice",
                              "rectangular_lattice", "row_column")
legacy_reference_platform <- identical(Sys.info()[["sysname"]], "Darwin") &&
  !tolower(Sys.getenv("FIELDHUB_GOLDEN", "true")) %in% c("false", "0", "no")

# Warnings a script for the previous release may see on the current API:
# deprecated-argument notices and defaulted wrong-length plot numbers.
allowed_warning_classes <- c("fieldhub_deprecated_warning", "fieldhub_default_warning")

# Calls whose saved output is intentionally not reproduced by the same call
# today, each documented in NEWS under "Changes to results for a given seed"
# or "Breaking changes":
# - row_column: new onestage default; twostage now uses FielDHub's own block
#   search.
# - incomplete_blocks, alpha_lattice, square_lattice, rectangular_lattice,
#   sparse_allocation, multi_location_prep: FielDHub's own block search
#   replaced blocksdesign::blocks().
# - split_families: draws its own seed when none is given; seed = 38
#   reproduces the old set.seed(38) allocation.
# - RCBD_augmented: ENTRY and CHECKS are numeric in every case.
legacy_result_changes <- c("row_column", "row_column(method = twostage)", "incomplete_blocks",
                           "alpha_lattice", "square_lattice", "rectangular_lattice",
                           "split_families", "RCBD_augmented", "sparse_allocation",
                           "multi_location_prep")

legacy_main_output <- function(x) if (!is.null(x$fieldBook)) x$fieldBook else x$data_locations

# Coerce to character only the columns whose type differs between the saved
# and the current field book (factor versus not, numeric versus not), so a
# representation change never hides a value change.
align_column_types <- function(old, new) {
  common <- intersect(names(old), names(new))
  old <- old[common]
  new <- new[common]
  for (column in common) {
    if (is.factor(old[[column]]) != is.factor(new[[column]]) ||
          is.numeric(old[[column]]) != is.numeric(new[[column]])) {
      old[[column]] <- as.character(old[[column]])
      new[[column]] <- as.character(new[[column]])
    }
  }
  list(old = old, new = new)
}

# The contract an optimized replay meets when its layout may differ from the
# saved one: the same entries, locations and replicates, reproducible from its
# own seed, and (for resolvable block designs whose layout changed) at least
# the saved Level 2 A-efficiency. Returns a named logical vector.
legacy_optimized_contract <- function(saved, replayed, engine) {
  old <- saved$fieldBook
  new <- replayed$fieldBook
  counts <- function(book, columns) {
    do.call(table, c(lapply(book[columns], as.character), list(useNA = "ifany")))
  }
  same <- function(columns) isTRUE(all.equal(counts(new, columns), counts(old, columns)))
  design <- replayed$metadata$design
  replay <- with_warning_classes(reproduce_design(replayed), allowed_warning_classes)
  ok <- c(entries = same(c("ENTRY", "TREATMENT")), locations = same("LOCATION"),
          reproducible = identical(replay$value, replayed) && !length(replay$unexpected))
  if (design %in% legacy_classical_engines) ok["replicates"] <- same(c("ENTRY", "REP", "LOCATION"))
  if (engine %in% legacy_result_changes && design %in% setdiff(legacy_classical_engines, "row_column")) {
    level_2_a <- function(x) x$blocksModel[as.character(x$blocksModel$Level) == "2", "A-Efficiency"]
    # 1.5e-7 is the engines' own acceptance tolerance on A-efficiency
    ok["efficiency"] <- length(level_2_a(saved)) == 1L &&
      level_2_a(replayed) >= level_2_a(saved) - 1.5e-7
  }
  if (legacy_reference_platform && !engine %in% legacy_result_changes) {
    aligned <- align_column_types(old, new)
    ok["reference layout"] <- isTRUE(all.equal(aligned$new, aligned$old, check.attributes = FALSE))
  }
  ok
}

test_that("scripts written for the previous release still run with the same output shape", {
  for (engine in names(historical_bundle$calls)) {
    saved <- historical_bundle$designs[[engine]]
    run <- with_warning_classes(do.call(engine, historical_bundle$calls[[engine]]),
                                allowed_warning_classes)
    new_book <- legacy_main_output(run$value)
    old_book <- legacy_main_output(saved)
    expect_identical(list(warnings = run$unexpected, dim = dim(new_book), names = names(new_book)),
                     list(warnings = character(0), dim = dim(old_book), names = names(old_book)),
                     info = engine)
    if (engine %in% setdiff(legacy_optimized_engines, "row_column")) {
      ok <- legacy_optimized_contract(saved, run$value, engine)
      expect(all(ok), sprintf("%s: replay fails the optimized-design contract (%s)",
                              engine, paste(names(ok)[!ok], collapse = ", ")))
    } else if (!engine %in% legacy_result_changes) {
      aligned <- align_column_types(old_book, new_book)
      expect_equal(aligned$new, aligned$old, ignore_attr = TRUE, info = engine)
    }
  }
})

test_that("row_column()'s twostage method meets the optimized contract on an old script", {
  args <- historical_bundle$calls$row_column
  run <- with_warning_classes(do.call(row_column, c(args, list(method = "twostage"))),
                              allowed_warning_classes)
  ok <- c(warnings = !length(run$unexpected),
          legacy_optimized_contract(historical_bundle$designs$row_column, run$value,
                                    "row_column(method = twostage)"))
  expect(all(ok), sprintf("twostage replay fails: %s", paste(names(ok)[!ok], collapse = ", ")))
})

test_that("split_families() with the old script's seed reproduces its saved allocation", {
  saved <- historical_bundle$designs$split_families
  run <- with_warning_classes(
    do.call(split_families, c(historical_bundle$calls$split_families,
                              list(seed = historical_bundle$initial_seed))),
    allowed_warning_classes)
  aligned <- align_column_types(saved$data_locations, run$value$data_locations)
  expect_equal(list(run$unexpected, aligned$new, as.data.frame(run$value$rowsEachlist)),
               list(character(0), aligned$old, as.data.frame(saved$rowsEachlist)),
               ignore_attr = TRUE)
})

test_that("RCBD_augmented()'s ENTRY and CHECKS are numeric versions of the saved columns", {
  saved <- historical_bundle$designs$RCBD_augmented$fieldBook
  run <- with_warning_classes(do.call(RCBD_augmented, historical_bundle$calls$RCBD_augmented),
                              allowed_warning_classes)
  new_book <- run$value$fieldBook[names(saved)]
  # The saved columns are character, with "NA" for filler CHECKS
  expect_equal(new_book[c("ENTRY", "CHECKS")],
               data.frame(ENTRY = as.numeric(as.character(saved$ENTRY)),
                          CHECKS = as.numeric(as.character(saved$CHECKS))),
               ignore_attr = TRUE)
  other <- setdiff(names(saved), c("ENTRY", "CHECKS"))
  expect_identical(list(run$unexpected, new_book[other]), list(character(0), saved[other]))
})

test_that("results saved by the previous release print and summarise without changing", {
  for (engine in names(historical_bundle$designs)) {
    saved <- historical_bundle$designs[[engine]]
    original <- serialize(saved, NULL, version = 2)
    text <- utils::capture.output(printed <- withVisible(print(saved)))
    summary_text <- utils::capture.output(print(summary(saved)))
    ok <- c(prints = any(nzchar(text)), invisible = !printed$visible,
            returns_input = identical(printed$value, saved),
            summarises = any(nzchar(summary_text)),
            unchanged = identical(serialize(saved, NULL, version = 2), original))
    expect(all(ok), sprintf("saved %s: %s", engine, paste(names(ok)[!ok], collapse = ", ")))
  }
})

test_that("results saved by the previous release still lay out and plot", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  # split_families() results have no field layout
  for (engine in setdiff(names(historical_bundle$designs), "split_families")) {
    saved <- historical_bundle$designs[[engine]]
    original <- serialize(saved, NULL, version = 2)
    book <- field_layout(saved)
    plotted <- suppressWarnings(suppressMessages(plot(saved)))
    ok <- c(layout = is.data.frame(book) && nrow(book) > 0 &&
              all(c("ID", "PLOT", "ROW", "COLUMN") %in% names(book)),
            plot_matches_layout = identical(plotted$field_book, book),
            unchanged = identical(serialize(saved, NULL, version = 2), original))
    expect(all(ok), sprintf("saved %s: %s", engine, paste(names(ok)[!ok], collapse = ", ")))
  }
})
