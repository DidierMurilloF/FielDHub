# Augmented row-column designs: augmented_row_column() inputs, results, field
# books, reported statistics, replay and presentation, and the independent
# references behind the statistics.
#
# Independent references, never the engine's own output:
# - arc_field_book_oracle() (helper-arc-design.R): a least-squares fit of the
#   returned field book alone;
# - published values of Piepho and Williams (2026), Biometrical Journal
#   68(5):e70182, and Bailey and Haines (2025), JABES (both CC-BY), with the
#   fixtures in fixtures/augmented-row-column/;
# - the counts of the exhaustive fixed-replication 6 x 4 enumeration of the
#   research verification (helper-arc-enumeration.R).
#
# Users cannot pass a contraction to augmented_row_column(), so no exported
# path reaches the published contractions: those checks call the internal
# scorers behind the reported statistics (arc_final_score() and the paper
# route arc_evaluate_contraction()) directly. The same holds for the 6 x 4
# enumeration and the work-bound gate on the staged search.
#
# Tiers: FIELDHUB_ARC_SEARCH_SLOW=true runs the slow published-E_aug search
# and a denser deadline sweep; FIELDHUB_ARC_EXHAUSTIVE=true runs the
# exhaustive enumeration agreement.

# ---- Local helpers ---------------------------------------------------------

# A 10 x 8 field with 4 checks along the columns: v = 10, s = 8, 48 tests,
# residual df 12 (no low-df warning)
arc_call <- list(nrows = 10, ncols = 8, checks = 4,
                 check_direction = "columns", budget = 600, seed = 2026)

arc_run <- function(...) {
  do.call(augmented_row_column, utils::modifyList(arc_call, list(...)))
}

# Physical dimensions of a v x s design laid out along `direction`
arc_dims <- function(v, s, direction) {
  if (identical(direction, "columns")) c(nrows = v, ncols = s)
  else c(nrows = s, ncols = v)
}

# The 10 x 8 design laid out along `direction`
arc_oriented <- function(direction, ...) {
  dims <- arc_dims(10L, 8L, direction)
  arc_run(nrows = dims[["nrows"]], ncols = dims[["ncols"]],
          check_direction = direction, ...)
}

arc_location_book <- function(x, location) {
  x$fieldBook[x$fieldBook$LOCATION == location, , drop = FALSE]
}

# The deterministic content of a result: everything but timing and the
# package version
arc_view <- function(x) {
  x[["timing"]] <- NULL
  x[["metadata"]][["package_version"]] <- NULL
  x
}

arc_rng <- function() get(".Random.seed", envir = globalenv())

arc_muffle_low_df <- function(expr) {
  withCallingHandlers(expr, fieldhub_arc_low_df_warning = function(w) {
    invokeRestart("muffleWarning")
  })
}

# Evaluate `expr` with `opts` set, restoring the previous options
arc_with_options <- function(opts, expr) {
  if (length(opts)) {
    old <- options(opts)
    on.exit(options(old), add = TRUE)
  }
  expr
}

arc_manifest <- function() {
  utils::read.csv(test_path("fixtures", "augmented-row-column", "manifest.csv"),
                  stringsAsFactors = FALSE)
}

# The coordinate checks on one field book, from arc_reconstruct(): one plot
# per cell; every check exactly once in every line along CHECK_DIRECTION;
# at most one check per cross-line plot set, and floor or ceiling of k s / v
# checks in each cross-line.
arc_placement_ok <- function(fb) {
  rec <- arc_reconstruct(fb)
  bounds <- c(floor(rec$k * rec$s / rec$v), ceiling(rec$k * rec$s / rec$v))
  all(rec$cells == 1L) && all(rec$by_line == 1L) &&
    all(rec$by_cross <= 1L) && all(rec$cross_checks %in% bounds) &&
    all(rec$cross_checks >= 1L) && sum(rec$cross_checks) == rec$k * rec$s
}

# Printed output as the app shows it, one location's details block, and the
# number printed after "<name> = "
arc_printed <- function(x) utils::capture.output(print(x, n = 6))
arc_summarised <- function(x) utils::capture.output(print(summary(x)))
arc_details_block <- function(lines, site) {
  start <- match(paste0("Location ", site, ":"), lines)
  if (is.na(start)) return(character())
  end <- start
  while (end < length(lines) && startsWith(lines[end + 1L], "  ")) end <- end + 1L
  lines[start:end]
}
arc_printed_value <- function(line, name) {
  as.numeric(sub(paste0(".*\\b", name, " = ([-+.0-9e]+).*"), "\\1", line))
}

arc_metrics <- c("E_aug", "V_tt", "V_tc", "V_cc", "max_tt", "max_tc")

# Brute-force move counts of a restricted contraction m (rows are checks,
# columns field lines, entries cross-lines). A row move exchanges two checks'
# cross-lines on a proper, non-empty set of field lines; a cross-line swap
# exchanges two cells in different field lines. A move is feasible when no
# field line and no check repeats a cross-line afterwards. Both sets are
# unchanged by the randomization of lines, cross-lines and check labels.
arc_restricted <- function(m) {
  all(apply(m, 1L, anyDuplicated) == 0L) &&
    all(apply(m, 2L, anyDuplicated) == 0L)
}
arc_row_moves <- function(m) {
  s <- ncol(m)
  n <- 0L
  for (i in seq_len(nrow(m) - 1L)) for (ii in (i + 1L):nrow(m)) {
    for (code in seq_len(2^s - 2)) {
      on <- bitwAnd(code, 2^(seq_len(s) - 1L)) > 0
      x <- m
      x[c(i, ii), on] <- m[c(ii, i), on]
      if (arc_restricted(x)) n <- n + 1L
    }
  }
  n
}
arc_cross_line_swaps <- function(m) {
  line <- col(m)
  n <- 0L
  for (p in seq_len(length(m) - 1L)) for (q in (p + 1L):length(m)) {
    if (line[p] == line[q]) next
    x <- m
    x[c(p, q)] <- m[c(q, p)]
    if (arc_restricted(x)) n <- n + 1L
  }
  n
}

# ---- Input validation ------------------------------------------------------

test_that("invalid inputs are rejected by class before a seed is drawn", {
  arc_local_seed(303) # restores the session's RNG when this test ends
  input <- "fieldhub_input_error"
  arc_input <- "fieldhub_arc_input_error"
  dimension <- "fieldhub_arc_dimension_error"
  labels4 <- c("A", "B", "C", "D")
  tests48 <- paste0("T", 1:48)
  upload <- data.frame(ENTRY = 1:52, NAME = c(labels4, tests48))
  # args override arc_call; drop removes arguments; class NULL = accepted
  cases <- list(
    list(drop = "check_direction", class = arc_input),
    list(drop = "nrows", class = arc_input),
    list(drop = "checks", class = arc_input),
    list(args = list(check_direction = "Columns"), class = arc_input),
    list(args = list(check_direction = NA_character_), class = arc_input),
    list(args = list(check_direction = c("columns", "rows")), class = arc_input),
    # s > v in the mapped orientation
    list(args = list(nrows = 8, ncols = 10), class = dimension),
    list(args = list(check_direction = "rows"), class = dimension),
    list(args = list(nrows = 0), class = input),
    list(args = list(nrows = 10.5), class = input),
    list(args = list(ncols = "8"), class = input),
    list(args = list(method = "anneal"), class = arc_input),
    list(args = list(method = c("staged", "staged")), class = arc_input),
    list(args = list(objective = "v_tt"), class = arc_input),
    list(args = list(objective = "A-opt"), class = arc_input),
    list(args = list(objective = c("V_tt", "E_aug")), class = arc_input),
    list(args = list(objective = NA_character_), class = arc_input),
    list(args = list(checks = 1), class = input),
    list(args = list(checks = 2.5), class = input),
    list(args = list(checks = c(1, 2, 3, 4)), class = arc_input),
    list(args = list(checks = "A"), class = arc_input),
    list(args = list(checks = c("A", "A", "B", "C")), class = arc_input),
    list(args = list(checks = c("A", NA, "B", "C")), class = input),
    list(args = list(checks = c("A", "", "B", "C")), class = input),
    list(args = list(checks = c("A", "  ", "B", "C")), class = input),
    list(args = list(checks = TRUE), class = arc_input),
    list(args = list(checks = factor(labels4)), class = arc_input),
    # k >= v: no test in a line
    list(args = list(checks = 10), class = dimension),
    # residual df below 1: -2 here, 0 for 6 x 6 with 2 checks
    list(args = list(checks = 2), class = "fieldhub_arc_infeasible_error"),
    list(args = list(nrows = 6, ncols = 6, checks = 2),
         class = "fieldhub_arc_df_error"),
    # entries: counts, labels and types
    list(args = list(entries = 47), class = arc_input),
    list(args = list(entries = 0), class = input),
    list(args = list(entries = tests48[-1]), class = arc_input),
    # a trailing space makes a distinct label: accepted
    list(args = list(entries = c(tests48[-1], "T1 "), budget = 100),
         class = NULL),
    list(args = list(entries = c(tests48[-48], "T1")), class = arc_input),
    list(args = list(entries = c(tests48[-48], NA)), class = input),
    list(args = list(entries = c(48, 1)), class = arc_input),
    list(args = list(entries = TRUE), class = arc_input),
    # check and test labels must be disjoint, also with generated labels
    list(args = list(checks = labels4, entries = c("A", tests48[-1])),
         class = arc_input),
    list(args = list(checks = c("G5", "X", "Y", "Z")), class = arc_input),
    # data: conflicts, columns, identifiers, labels and rows
    list(args = list(data = upload, entries = tests48), class = arc_input),
    list(args = list(data = upload, checks = labels4), class = arc_input),
    list(args = list(data = upload[, "NAME", drop = FALSE]), class = arc_input),
    list(args = list(data = cbind(upload, BLOCK = 1)), class = arc_input),
    list(args = list(data = "upload.csv"), class = arc_input),
    list(args = list(data = transform(upload, ENTRY = ENTRY + 0.5)),
         class = arc_input),
    list(args = list(data = transform(upload, ENTRY = c(1:51, 1L))),
         class = arc_input),
    list(args = list(data = transform(upload, ENTRY = 0:51)), class = arc_input),
    list(args = list(data = transform(upload, ENTRY = c(NA, 2:52))),
         class = arc_input),
    list(args = list(data = transform(upload, NAME = 1:52)), class = arc_input),
    list(args = list(data = transform(upload,
                                      NAME = c("A", "A", NAME[-(1:2)]))),
         class = arc_input),
    list(args = list(data = transform(upload, NAME = c(" ", NAME[-1]))),
         class = input),
    list(args = list(data = upload[-52, ]), class = arc_input),
    # search and layout controls
    list(args = list(budget = 0), class = input),
    list(args = list(budget = 10.5), class = input),
    list(args = list(budget = "600"), class = input),
    list(args = list(time_limit = 0), class = arc_input),
    list(args = list(time_limit = -1), class = arc_input),
    list(args = list(time_limit = Inf), class = arc_input),
    list(args = list(time_limit = NA_real_), class = arc_input),
    list(args = list(time_limit = c(1, 2)), class = arc_input),
    list(args = list(time_limit = "5"), class = arc_input),
    list(args = list(l = 0), class = input),
    list(args = list(l = 1.5), class = input),
    list(args = list(planter = "zigzag"), class = input),
    list(args = list(plotNumber = 0), class = arc_input),
    list(args = list(plotNumber = 10.5), class = input),
    list(args = list(plotNumber = c(1, 101)), class = arc_input),
    list(args = list(plotNumber = .Machine$integer.max - 10), class = arc_input),
    list(args = list(locationNames = c("A", "B")), class = arc_input),
    list(args = list(l = 2, locationNames = c("A", "A")), class = input),
    list(args = list(l = 2, locationNames = c("A", "")), class = input),
    list(args = list(exptName = c("E1", "E2")), class = arc_input),
    list(args = list(exptName = ""), class = input),
    list(args = list(year = c(2025, 2026)), class = input),
    list(args = list(seed = "2026"), class = input),
    list(args = list(seed = NA_real_), class = input)
  )
  rng_moved <- character()
  for (case in cases) {
    arguments <- utils::modifyList(arc_call, as.list(case$args))
    arguments[case$drop] <- NULL
    label <- paste(deparse(c(case$args, drop = case$drop), width.cutoff = 500L),
                   collapse = "")
    before <- arc_rng()
    if (is.null(case$class)) {
      expect_no_error(do.call(augmented_row_column, arguments))
      next
    }
    expect_error(do.call(augmented_row_column, arguments), class = case$class,
                 label = label)
    if (!identical(arc_rng(), before)) rng_moved <- c(rng_moved, label)
  }
  expect(length(rng_moved) == 0L,
         sprintf("a rejected call drew from the RNG: %s", rng_moved[1]))
})

test_that("rejections name the fix in their message and condition fields", {
  cases <- list(
    wide = list(args = list(nrows = 8, ncols = 10),
                class = "fieldhub_dimension_error", fields = list(),
                messages = "check_direction = \"rows\""),
    long = list(args = list(check_direction = "rows"),
                class = "fieldhub_dimension_error", fields = list(),
                messages = "check_direction = \"columns\""),
    infeasible = list(args = list(checks = 2),
                      class = "fieldhub_arc_infeasible_error",
                      fields = list(df_formula = -2L),
                      messages = c("Add a check", "rows and columns")),
    saturated = list(args = list(nrows = 6, ncols = 6, checks = 2),
                     class = "fieldhub_arc_df_error",
                     fields = list(df_formula = 0L), messages = "at least 1"),
    count = list(args = list(entries = 47), class = "fieldhub_arc_input_error",
                 fields = list(argument = "entries", required = 48L),
                 messages = "48 test plots"),
    method = list(args = list(method = "multistart"),
                  class = "fieldhub_arc_input_error",
                  fields = list(options = "staged"), messages = character()),
    objective = list(args = list(objective = "A"),
                     class = "fieldhub_arc_input_error",
                     fields = list(options = c("E_aug", "V_tt", "V_tc")),
                     messages = character()),
    overlap = list(args = list(checks = c("G5", "X", "Y", "Z")),
                   class = "fieldhub_arc_input_error",
                   fields = list(labels = "G5"), messages = character()),
    conflict = list(args = list(data = data.frame(ENTRY = 1:52,
                                                  NAME = paste0("N", 1:52)),
                                entries = 48),
                    class = "fieldhub_arc_input_error", fields = list(),
                    messages = "not both")
  )
  for (nm in names(cases)) {
    case <- cases[[nm]]
    e <- tryCatch(do.call(arc_run, case$args), error = identity)
    got <- list(class = inherits(e, case$class),
                fields = unname(e[names(case$fields)]),
                messages = all(vapply(case$messages, grepl, logical(1),
                                      x = conditionMessage(e), fixed = TRUE)))
    want <- list(class = TRUE, fields = unname(case$fields), messages = TRUE)
    expect_identical(got, want, info = nm)
  }
})

test_that("low residual df warns once, before the seed is drawn", {
  # 12 x 8 with 3 checks: residual df 3
  warnings <- list()
  x <- withCallingHandlers(
    arc_run(nrows = 12, ncols = 8, checks = 3, l = 2, budget = 300),
    fieldhub_arc_low_df_warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 1L)
  msg <- conditionMessage(warnings[[1L]])
  expect_identical(
    list(warnings[[1L]]$df_formula,
         vapply(c("only 3 residual degrees", "adding a check",
                  "rows or columns"), grepl, logical(1), x = msg, fixed = TRUE),
         grepl("optimi|search|longer|iteration|budget", msg, ignore.case = TRUE),
         x$infoDesign$residual_df, x$statistics$residual_df),
    list(3L, c("only 3 residual degrees" = TRUE, "adding a check" = TRUE,
               "rows or columns" = TRUE), FALSE, 3L, c(3L, 3L))
  )
  # A warning turned into an error leaves the caller's RNG alone
  arc_local_seed(5)
  before <- arc_rng()
  expect_error(withCallingHandlers(
    arc_run(nrows = 12, ncols = 8, checks = 3, seed = NULL),
    fieldhub_arc_low_df_warning = function(w) {
      rlang::abort("as error", class = "arc_warning_as_error")
    }
  ), class = "arc_warning_as_error")
  expect_identical(arc_rng(), before)
  expect_no_warning(arc_run(budget = 100))
})

# ---- Results and field books -----------------------------------------------

test_that("results have the documented class, parameters and field book", {
  x <- arc_run(l = 2, locationNames = c("North", "South"),
               plotNumber = c(1, 1001))
  expect_s3_class(x, c("fieldhub_augmented_row_column", "FielDHub"),
                  exact = TRUE)
  expect_identical(
    x$infoDesign,
    list(nrows = 10L, ncols = 8L, check_direction = "columns", v = 10L,
         s = 8L, k = 4L, checks = paste0("CH", 1:4), entries = 48L,
         locations = 2, locationNames = c("North", "South"),
         residual_df = 12L, objective = "V_tt", method = "staged",
         budget = 600, seed = 2026, id_design = "augmented_row_column")
  )
  # The parameters name every argument, with effective values
  expect_identical(
    x$metadata$parameters,
    list(nrows = 10, ncols = 8, checks = 4, check_direction = "columns",
         entries = NULL, objective = "V_tt", method = "staged", budget = 600,
         time_limit = NULL, l = 2, plotNumber = c(1, 1001),
         planter = "serpentine", locationNames = c("North", "South"),
         exptName = "Expt1", year = format(Sys.Date(), "%Y"), seed = 2026,
         data = NULL)
  )
  fb <- x$fieldBook
  expect_identical(
    list(names(fb), rownames(fb), fb$LOCATION, fb$PLOT, fb$ID,
         x$statistics$LOCATION),
    list(c("ID", "EXPT", "LOCATION", "YEAR", "PLOT", "ROW", "COLUMN",
           "CHECKS", "ENTRY", "TREATMENT", "CHECK_DIRECTION"),
         as.character(seq_len(160L)), rep(c("North", "South"), each = 80L),
         c(1 + 0:79, 1001 + 0:79), rep(seq_len(80L), 2L), c("North", "South"))
  )
})

test_that("field books have the column types and the planter's planting order", {
  cases <- expand.grid(direction = c("columns", "rows"),
                       planter = c("serpentine", "cartesian"),
                       stringsAsFactors = FALSE)
  for (i in seq_len(nrow(cases))) {
    direction <- cases$direction[i]
    planter <- cases$planter[i]
    nm <- paste(direction, planter)
    x <- arc_oriented(direction, planter = planter, plotNumber = 1001,
                      budget = 100, seed = 3)
    fb <- x$fieldBook
    n <- nrow(fb)
    types <- vapply(fb[c("ROW", "COLUMN", "CHECKS", "ENTRY", "TREATMENT")],
                    typeof, "")
    expect_identical(types, c(ROW = "integer", COLUMN = "integer",
                              CHECKS = "integer", ENTRY = "integer",
                              TREATMENT = "character"), info = nm)
    expect_identical(
      list(fb$ID, fb$PLOT, unique(fb$CHECK_DIRECTION), unique(fb$EXPT),
           unique(fb$YEAR)),
      list(seq_len(n), 1001 + (seq_len(n) - 1), direction, "Expt1",
           format(Sys.Date(), "%Y")),
      info = nm
    )
    # Planting order: row by row from row 1, column 1; a serpentine planter
    # goes back along every second row
    nrows <- max(fb$ROW)
    ncols <- max(fb$COLUMN)
    row <- rep(seq_len(nrows), each = ncols)
    column <- rep(seq_len(ncols), times = nrows)
    if (identical(planter, "serpentine")) {
      back <- row %% 2L == 0L
      column[back] <- ncols + 1L - column[back]
    }
    expect_identical(list(fb$ROW, fb$COLUMN), list(row, column), info = nm)
  }
})

test_that("returned field books pass the placement checks and match the oracle", {
  odd_checks <- c("Check A", "std-2 (local); \"B\"", "Cé #3", "D/4")
  odd_tests <- sprintf("Line %02d / x.%s, y", 1:48,
                       rep_len(c("a", " b", "c'"), 48))
  # v cross-lines and s lines along check_direction; options select the
  # scorer route above the plot cap and the worst-pair block size
  # (block b = floor(sqrt(mib 2^20 / 8)): 1 and 7, which does not divide the
  # 48 test plots)
  cases <- list(
    list(name = "labelled columns V_tt", v = 10, s = 8, k = 4,
         direction = "columns", objective = "V_tt", labels = TRUE, l = 2),
    list(name = "labelled rows V_tt", v = 10, s = 8, k = 4,
         direction = "rows", objective = "V_tt", labels = TRUE, l = 2),
    list(name = "labelled columns E_aug", v = 10, s = 8, k = 4,
         direction = "columns", objective = "E_aug", labels = TRUE, l = 2),
    list(name = "labelled rows E_aug", v = 10, s = 8, k = 4,
         direction = "rows", objective = "E_aug", labels = TRUE, l = 2),
    list(name = "14 x 9 V_tc", v = 14, s = 9, k = 4, direction = "columns",
         objective = "V_tc", seed = 7),
    list(name = "11 x 7 rows", v = 11, s = 7, k = 4, direction = "rows",
         objective = "V_tt", seed = 8),
    list(name = "12 x 8 with residual df 3", v = 12, s = 8, k = 3,
         direction = "columns", objective = "E_aug", seed = 9),
    list(name = "20 x 12 with 5 checks", v = 20, s = 12, k = 5,
         direction = "rows", objective = "V_tt", seed = 10),
    list(name = "single-matrix authority, block 1", v = 10, s = 8, k = 4,
         direction = "columns", objective = "V_tt", seed = 11,
         options = list(FielDHub.arc_oracle_max_plots = 50,
                        FielDHub.arc_worst_pair_block_mib = 1e-9),
         authority = "single_matrix"),
    list(name = "single-matrix authority, block 7", v = 10, s = 8, k = 4,
         direction = "rows", objective = "E_aug", seed = 12,
         options = list(FielDHub.arc_oracle_max_plots = 50,
                        FielDHub.arc_worst_pair_block_mib = 49 * 8 / 2^20),
         authority = "single_matrix"),
    # A 5 x 4 field with 3 checks admits no row move but some cross-line
    # swaps: the row phases must certify an empty neighbourhood and the
    # polish a local optimum
    list(name = "5 x 4 with no row move", v = 5, s = 4, k = 3,
         direction = "columns", objective = "V_tt", budget = 3000,
         stops = TRUE),
    # A 6 x 3 field with 4 checks and residual df 1, where the annealing
    # proposes disconnected candidates
    list(name = "6 x 3 with disconnected candidates", v = 6, s = 3, k = 4,
         direction = "columns", objective = "V_tt")
  )
  # Absolute agreement of well-conditioned (lambda_min_rel > 0.1) O(1-10)
  # quantities from two independent factorizations of matrices of size at
  # most 245: rounding is about 1e-13
  tol <- 1e-10
  for (case in cases) {
    labels <- isTRUE(case$labels)
    l <- if (is.null(case$l)) 1L else case$l
    dims <- arc_dims(case$v, case$s, case$direction)
    n_tests <- (case$v - case$k) * case$s
    x <- arc_with_options(case$options, arc_muffle_low_df(augmented_row_column(
      nrows = dims[["nrows"]], ncols = dims[["ncols"]],
      checks = if (labels) odd_checks else case$k,
      check_direction = case$direction,
      entries = if (labels) odd_tests else NULL,
      objective = case$objective, l = l,
      budget = if (is.null(case$budget)) 300 else case$budget,
      seed = if (is.null(case$seed)) 41 else case$seed
    )))
    checks <- if (labels) odd_checks else paste0("CH", seq_len(case$k))
    tests <- if (labels) odd_tests else paste0("G", case$k + seq_len(n_tests))
    df <- case$k * case$s - case$k - case$v - case$s + 2
    authority <- if (is.null(case$authority)) "full_layout" else case$authority
    for (site in seq_len(l)) {
      nm <- paste(case$name, "location", site)
      fb <- arc_location_book(x, site)
      rec <- arc_reconstruct(fb)
      # Placement from coordinates, and the returned contraction is that
      # placement
      expect(arc_placement_ok(fb) &&
               identical(c(rec$v, rec$s, rec$k),
                         as.integer(c(case$v, case$s, case$k))) &&
               identical(rec$contraction, x$contraction[[site]]),
             sprintf("case %s: placement checks fail", nm))
      # Every test once; each check s times; labels bound to identifiers
      on_test <- fb$CHECKS == 0L
      expect(identical(sort(fb$TREATMENT[on_test], method = "radix"),
                       sort(tests, method = "radix")) &&
               all(table(factor(fb$TREATMENT[!on_test], levels = checks)) ==
                     case$s) &&
               identical(fb$TREATMENT, c(checks, tests)[fb$ENTRY]) &&
               identical(fb$CHECKS[!on_test], fb$ENTRY[!on_test]),
             sprintf("case %s: labels are not bound to their entries", nm))
      oracle <- arc_field_book_oracle(fb)
      stats <- x$statistics[site, ]
      expect_identical(
        list(oracle$connected, stats$residual_df, oracle$residual_df,
             oracle$residual_df_svd, stats$authority),
        list(TRUE, as.integer(df), as.integer(df), as.integer(df), authority),
        info = nm
      )
      dev <- abs(unlist(stats[arc_metrics]) - unlist(oracle[arc_metrics]))
      expect(all(dev <= tol),
             sprintf("case %s: %s got %.12g, oracle %.12g", nm,
                     arc_metrics[which.max(dev)],
                     stats[[arc_metrics[which.max(dev)]]],
                     oracle[[arc_metrics[which.max(dev)]]]))
      # Weighted-variance identity on the reported statistics:
      # n (v* - 1) / E_aug equals the sum over treatment pairs of
      # u_i u_j Var(tau_i - tau_j), tests replicated once and checks s times.
      # Both sides are sums of O(1e4) positive terms: relative rounding is
      # far below 1e-10.
      lhs <- case$v * case$s * (n_tests + case$k - 1) / stats$E_aug
      rhs <- choose(n_tests, 2) * stats$V_tt +
        case$s * n_tests * case$k * stats$V_tc +
        case$s^2 * choose(case$k, 2) * stats$V_cc
      expect_lt(abs(lhs - rhs) / lhs, 1e-10, label = nm)
      # Stop claims checked against brute-force move counts of the result
      if (isTRUE(case$stops)) {
        m <- x$contraction[[site]]
        phases <- unlist(x$diagnostics[[site]]$phases)
        expect_identical(
          list("empty_neighbourhood" %in% phases, arc_row_moves(m) == 0L,
               "local_optimum" %in% phases, arc_cross_line_swaps(m) > 0L),
          list(TRUE, TRUE, TRUE, TRUE), info = nm
        )
      }
    }
  }
})

# ---- Seeds, replay and locations -------------------------------------------

test_that("reproduce_design() and the recorded call replay the deterministic content", {
  for (direction in c("columns", "rows")) {
    x <- arc_oriented(direction, checks = c("A 1", "B-2", "C.3", "D,4"),
                      l = 2, seed = 47, planter = "cartesian", budget = 300,
                      locationNames = c("Field 1", "Field 2"),
                      exptName = "Trial 26", year = 2025)
    direct <- do.call(augmented_row_column, x$metadata$parameters)
    expect_identical(list(arc_view(reproduce_design(x)), arc_view(direct)),
                     list(arc_view(x), arc_view(x)), info = direction)
  }
})

test_that("explicit seeds are recorded and leave the caller's RNG alone", {
  arc_local_seed(71) # restores the session's RNG when this test ends
  before <- arc_rng()
  x <- arc_run(seed = 5, budget = 200)
  other <- arc_run(seed = 6, budget = 200)
  expect_identical(arc_rng(), before)
  expect_identical(list(x$metadata$seed, x$infoDesign$seed,
                        x$diagnostics[[1]]$seed), list(5, 5, 5))
  # Different seeds give different placements
  expect_false(identical(other$fieldBook$TREATMENT, x$fieldBook$TREATMENT))
})

test_that("an automatic seed consumes exactly one recorded draw and replays", {
  # Each helper seeds and restores the RNG in its own frame
  automatic_run <- function() {
    arc_local_seed(72)
    x <- arc_run(seed = NULL, budget = 200)
    list(x = x, after = arc_rng())
  }
  one_draw <- function() {
    arc_local_seed(72)
    seed <- sample.int(.Machine$integer.max, 1L)
    list(seed = seed, after = arc_rng())
  }
  run <- automatic_run()
  expected <- one_draw()
  expect_identical(
    list(run$x$infoDesign$seed, run$x$metadata$parameters$seed, run$after),
    list(expected$seed, expected$seed, expected$after)
  )
  expect_identical(arc_view(reproduce_design(run$x)), arc_view(run$x))
})

test_that("locations are independent runs with offset seeds", {
  x <- arc_run(l = 3, seed = 100, locationNames = c("A", "B", "C"),
               budget = 300)
  expect_identical(vapply(x$diagnostics, `[[`, numeric(1), "seed"),
                   c(A = 100, B = 101, C = 102))
  layout <- c("ROW", "COLUMN", "CHECKS", "ENTRY", "TREATMENT")
  for (i in 1:3) {
    site <- c("A", "B", "C")[i]
    alone <- arc_run(seed = 99 + i, budget = 300)
    fb <- arc_location_book(x, site)
    rownames(fb) <- NULL
    expect_identical(
      list(fb[layout], x$contraction[[site]], x$diagnostics[[site]][-1]),
      list(alone$fieldBook[layout], alone$contraction[[1]],
           alone$diagnostics[[1]][-1]),
      info = site
    )
    expect_identical(x$statistics[i, -1], alone$statistics[1, -1],
                     ignore_attr = TRUE, info = site)
  }
  expect_false(identical(x$contraction$A, x$contraction$B))
})

# ---- Search controls -------------------------------------------------------

test_that("the evaluation budget is respected and NULL records the size rule's budget", {
  for (budget in c(150, 450)) {
    x <- arc_run(budget = budget, l = 2)
    calls <- vapply(x$diagnostics, function(d) {
      d$objective_calls + d$proxy_calls
    }, numeric(1))
    expect(all(calls <= budget) &&
             all(vapply(x$diagnostics, `[[`, numeric(1), "objective_calls") > 0),
           sprintf("budget %d: %g calls used", budget, max(calls)))
  }
  # NULL resolves by field size (v s <= budget_cells), recorded as the
  # effective budget; the rule's budgets are made small so the search is fast
  local_mocked_bindings(arc_design_defaults = function() {
    list(objective = "V_tt", methods = "staged", budget_cells = 600,
         budget_small = 120, budget_large = 160)
  })
  small <- arc_run(budget = NULL)
  large <- arc_run(nrows = 26, ncols = 24, checks = 5, budget = NULL)
  expect_identical(
    list(small$infoDesign$budget, small$metadata$parameters$budget,
         small$diagnostics[[1]]$objective_calls +
           small$diagnostics[[1]]$proxy_calls <= 120,
         large$infoDesign$budget),
    list(120, 120, TRUE, 160)
  )
  expect_identical(arc_view(reproduce_design(large)), arc_view(large))
})

test_that("the default objective is recorded and written into the call code", {
  x <- arc_run(budget = 100)
  code <- paste(design_call_code(x), collapse = "\n")
  expect_identical(
    list(x$infoDesign$objective, x$metadata$parameters$objective,
         grepl("objective = \"V_tt\"", code, fixed = TRUE),
         grepl("budget = 100", code, fixed = TRUE)),
    list("V_tt", "V_tt", TRUE, TRUE)
  )
})

test_that("time_limit cuts each location's search and verification still runs", {
  # A clock that advances 0.01 s on every reading makes the cutoff
  # deterministic
  clock <- arc_fake_clock()
  local_mocked_bindings(arc_clock = function() clock$advance(0.01))
  full <- arc_run(budget = 2000, seed = 3)
  expect_false(full$diagnostics[[1]]$time_limit_hit)
  limit <- full$timing$search / 2
  limited <- arc_run(budget = 2000, seed = 3, time_limit = limit, l = 2)
  expect_identical(limited$metadata$parameters$time_limit, limit)
  # Deadlines (fake seconds from a fresh clock) that fall between an
  # annealing phase's stop check and its next objective call, and inside the
  # neighbourhood scan of a 5 x 4 field with no row move, where the cut scan
  # must not certify an empty neighbourhood
  cases <- list(
    list(name = "halfway", x = limited, limit = limit, budget = 2000, l = 2),
    list(name = "before an objective call", limit = 22.2,
         args = list(budget = 2000, seed = 3)),
    list(name = "inside a scan", limit = 1.32, uncertified = TRUE,
         args = list(nrows = 5, ncols = 4, checks = 3, budget = 300,
                     seed = 41))
  )
  for (case in cases) {
    if (is.null(case$x)) {
      clock <- arc_fake_clock()
      case$x <- arc_muffle_low_df(do.call(arc_run, c(case$args,
                                                     time_limit = case$limit)))
      case$budget <- case$args$budget
      case$l <- 1L
    }
    for (site in seq_len(case$l)) {
      nm <- paste(case$name, "location", site)
      d <- case$x$diagnostics[[site]]
      fb <- arc_location_book(case$x, site)
      oracle <- arc_field_book_oracle(fb)
      # Stopped at the deadline (give or take one scored step of the fake
      # clock), then verified, randomized and assembled
      expect_identical(
        list(d$time_limit_hit, "time_limit" %in% d$phases, d$status,
             d$objective_calls + d$proxy_calls < case$budget,
             case$x$timing$search[site] >= case$limit,
             case$x$timing$search[site] < case$limit + 1,
             case$x$timing$verification[site] > 0,
             case$x$timing$total[site] > case$x$timing$search[site],
             arc_placement_ok(fb), oracle$connected,
             !isTRUE(case$uncertified) ||
               "sampling_exhausted" %in% d$phases),
        list(TRUE, TRUE, "ok", TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
             TRUE),
        info = nm
      )
      # Scale of V_tt is O(1); see the oracle-agreement test for the
      # tolerance
      expect_lt(abs(case$x$statistics$V_tt[site] - oracle$V_tt), 1e-10,
                label = paste("V_tt deviation at", nm))
    }
  }
  # A cutoff before any design exists is a classed search error
  arc_local_seed(9)
  before <- arc_rng()
  expect_error(arc_run(budget = 2000, time_limit = 0.001),
               class = "fieldhub_arc_search_error")
  early <- tryCatch(arc_run(budget = 2000, time_limit = 0.001),
                    error = identity)
  expect_identical(list(inherits(early, "fieldhub_search_error"),
                        early$time_limit_hit, arc_rng()),
                   list(TRUE, TRUE, before))
})

test_that("a search without a design raises a classed error, never a fallback", {
  arc_local_seed(11)
  before <- arc_rng()
  expect_error(arc_run(budget = 1), class = "fieldhub_arc_search_error")
  tiny <- tryCatch(arc_run(budget = 1), error = identity)
  expect_identical(list(tiny$status, tiny$time_limit_hit,
                        grepl("Increase budget", conditionMessage(tiny),
                              fixed = TRUE), arc_rng()),
                   list("no_design", FALSE, TRUE, before))
  local_mocked_bindings(arc_random_start = function(...) NULL)
  expect_error(arc_run(l = 2), class = "fieldhub_arc_search_error")
  exhausted <- tryCatch(arc_run(l = 2), error = identity)
  expect_identical(list(exhausted$status, exhausted$location, arc_rng()),
                   list("start_search_exhausted", 1L, before))
})

# ---- Recorded labels and uploads -------------------------------------------

test_that("exptName, year and locationNames are recorded as effective values", {
  x <- arc_run(l = 2, budget = 100)
  this_year <- format(Sys.Date(), "%Y")
  expect_identical(
    list(x$metadata$parameters$exptName, x$metadata$parameters$year,
         x$metadata$parameters$locationNames, x$infoDesign$locationNames,
         unique(x$fieldBook$EXPT), unique(x$fieldBook$YEAR),
         unique(x$fieldBook$LOCATION)),
    list("Expt1", this_year, 1:2, 1:2, "Expt1", this_year, 1:2)
  )
  named <- arc_run(budget = 100, exptName = "AYT 26", year = 2027,
                   locationNames = "Casselton")
  expect_identical(list(named$metadata$parameters$year,
                        unique(named$fieldBook$EXPT),
                        unique(named$fieldBook$LOCATION)),
                   list("2027", "AYT 26", "Casselton"))
  # Replay in a later year reproduces the recorded YEAR
  local_mocked_bindings(resolve_year = function(year) {
    if (is.null(year)) "2099" else as.character(year)
  })
  expect_identical(arc_run(budget = 100)$fieldBook$YEAR[1], "2099")
  expect_identical(arc_view(reproduce_design(x)), arc_view(x))
})

test_that("standard-template uploads are recorded as labels", {
  checks <- c("Check one", "Check two", "Check three", "Check four")
  tests <- paste("Line", 1:48)
  upload <- data.frame(ENTRY = 1:52, NAME = c(checks, tests))
  x <- arc_run(checks = 4, data = upload, budget = 200)
  expect_identical(
    list(x$metadata$parameters$data, x$metadata$parameters$checks,
         x$metadata$parameters$entries, x$infoDesign$checks,
         x$fieldBook$ENTRY),
    list(NULL, checks, tests, checks, match(x$fieldBook$TREATMENT, upload$NAME))
  )
  # The same call with labels, the replay, and double identifiers with factor
  # labels all give the same deterministic content
  as_factor <- data.frame(ENTRY = as.numeric(1:52),
                          NAME = factor(upload$NAME, levels = upload$NAME))
  expect_identical(
    list(arc_view(arc_run(checks = checks, entries = tests, budget = 200)),
         arc_view(reproduce_design(x)),
         arc_view(arc_run(checks = 4, data = as_factor, budget = 200))),
    rep(list(arc_view(x)), 3L)
  )
})

test_that("non-standard identifiers keep data and the checks' input order", {
  checks <- paste0("C", 1:4)
  tests <- paste0("T", 1:48)
  ids <- c(905L, 17L, 300L, 41L, 2000L - 1:48)
  upload <- data.frame(NAME = c(checks, tests), ENTRY = ids)
  for (direction in c("columns", "rows")) {
    x <- arc_oriented(direction, checks = 4, data = upload, budget = 300)
    fb <- x$fieldBook
    expect_identical(
      list(x$metadata$parameters$data, x$metadata$parameters$checks,
           x$metadata$parameters$entries, fb$ENTRY, fb$CHECKS),
      list(upload, 4, NULL, ids[match(fb$TREATMENT, upload$NAME)],
           ifelse(fb$TREATMENT %in% checks, fb$ENTRY, 0L)),
      info = direction
    )
    # Checks keep their input order, not their identifiers' order, so labels
    # give the same design
    labelled <- arc_oriented(direction, checks = checks, entries = tests,
                             budget = 300)
    expect_identical(
      list(x$contraction, x$statistics, fb[c("ROW", "COLUMN", "TREATMENT")],
           arc_view(reproduce_design(x))),
      list(labelled$contraction, labelled$statistics,
           labelled$fieldBook[c("ROW", "COLUMN", "TREATMENT")], arc_view(x)),
      info = direction
    )
  }
})

test_that("a field-scale standard upload gives standalone code that replays", {
  # 48 x 16 with 5 checks, 688 tests (693 entry rows); a reduced budget
  # checks the recording route, not search quality
  upload <- data.frame(ENTRY = 1:693,
                       NAME = c(paste("Check", LETTERS[1:5]),
                                sprintf("BRD-%04d", 1:688)))
  x <- augmented_row_column(nrows = 48, ncols = 16, checks = 5,
                            check_direction = "columns", data = upload,
                            budget = 300, seed = 112660)
  code <- design_call_code(x)
  text <- paste(code, collapse = "\n")
  expect_identical(
    list(nrow(x$fieldBook), x$infoDesign$residual_df,
         x$metadata$parameters$data, attr(code, "reason"),
         grepl("data =", text, fixed = TRUE),
         grepl("entries = c(", text, fixed = TRUE)),
    list(768L, 13L, NULL, NULL, FALSE, TRUE)
  )
  rebuilt <- eval(parse(text = code), envir = new.env(parent = baseenv()))
  expect_identical(arc_view(rebuilt), arc_view(x))
})

test_that("a non-standard upload over 200 rows falls back to the saved result", {
  # 24 x 12 with 5 checks: 228 tests, 233 entry rows
  upload <- data.frame(ENTRY = 5000L + 1:233,
                       NAME = c(paste0("CK", 1:5), paste0("L", 1:228)))
  x <- augmented_row_column(nrows = 24, ncols = 12, checks = 5,
                            check_direction = "columns", data = upload,
                            budget = 200, seed = 15)
  code <- design_call_code(x)
  section <- design_call_section(x)
  expect_identical(
    list(x$metadata$parameters$data, length(code),
         grepl("more than 200 rows", attr(code, "reason"), fixed = TRUE),
         grepl("Standalone call unavailable", section[1], fixed = TRUE),
         grepl("saved RDS", section[2], fixed = TRUE)),
    list(upload, 0L, TRUE, TRUE, TRUE)
  )
  # The saved result still replays
  expect_identical(arc_view(reproduce_design(x)), arc_view(x))
  # Up to 200 rows a non-standard upload keeps standalone code
  small <- arc_run(checks = 4, budget = 100,
                   data = data.frame(ENTRY = 100L + 1:52,
                                     NAME = paste0("E", 1:52)))
  small_code <- design_call_code(small)
  expect_identical(
    arc_view(eval(parse(text = small_code), envir = new.env(parent = baseenv()))),
    arc_view(small)
  )
})

# ---- Presentation ----------------------------------------------------------

test_that("print() and summary() show each location's statistics, as the app does", {
  df <- 4L * 8L - 4L - 10L - 8L + 2L
  sites <- c("Fargo", "Casselton")
  cases <- expand.grid(direction = c("columns", "rows"),
                       objective = c("V_tt", "E_aug"), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(cases))) {
    direction <- cases$direction[i]
    objective <- cases$objective[i]
    x <- arc_oriented(direction, objective = objective, l = 2, budget = 300,
                      locationNames = sites)
    printed <- arc_printed(x)
    summarised <- arc_summarised(x)
    sense <- if (objective == "E_aug") "maximized" else "minimized"
    # The statistics follow the title, before the design parameters; no
    # low-df notice at twelve residual df
    expect_identical(
      list(printed[1:3], summarised[1:3],
           match("Location Casselton:", printed) <
             match("Information on the design parameters: ", printed),
           match("Location Casselton:", summarised) <
             match("1. Information on the design parameters: ", summarised),
           any(grepl("Notice", c(printed, summarised), fixed = TRUE))),
      list(c("Augmented Row-Column Design ", "",
             "Statistics by location (variances in units of the residual variance):"),
           c("Augmented Row-Column Design ", "",
             "Statistics by location (variances in units of the residual variance):"),
           TRUE, TRUE, FALSE),
      info = paste(direction, objective)
    )
    for (site in sites) {
      nm <- paste(direction, objective, site)
      block <- arc_details_block(printed, site)
      expect_identical(
        list(length(block), arc_details_block(summarised, site), block[2],
             grepl(paste0("^  Optimized objective: ", objective,
                          " = [-+.0-9e]+ \\(", sense, "\\)$"), block[3]),
             block[6]),
        list(6L, block, paste0("  Residual df: ", df), TRUE,
             "  Search status: ok"),
        info = nm
      )
      # Printed to 4 significant digits: each shown value is within half a
      # unit of the fourth digit, at most 5e-4 relative, of the independent
      # oracle (the oracle itself agrees to 1e-10)
      oracle <- arc_field_book_oracle(arc_location_book(x, site))
      shown <- c(arc_printed_value(block[3], objective),
                 vapply(c("E_aug", "V_tt", "V_tc"), arc_printed_value, 0,
                        line = block[4]),
                 vapply(c("max_tt", "max_tc"), arc_printed_value, 0,
                        line = block[5]))
      want <- unlist(oracle[c(objective, "E_aug", "V_tt", "V_tc", "max_tt",
                              "max_tc")])
      rel <- abs(unname(shown) - unname(want)) / abs(unname(want))
      expect(length(rel) == 6L && !anyNA(rel) && all(rel <= 5e-4 + 1e-9),
             sprintf("case %s: printed %s = %s, oracle %.8g", nm,
                     names(want)[which.max(rel)], shown[which.max(rel)],
                     want[which.max(rel)]))
    }
  }
})

test_that("a low-df design shows the notice on every print and summary", {
  notice <- c(
    "  Notice: only 3 residual degrees of freedom (fewer than 10). Only adding a",
    "  check or changing the number of rows or columns raises them."
  )
  for (direction in c("columns", "rows")) {
    # A 12 x 8 field with 3 checks: k s - k - v - s + 2 = 3 residual df
    dims <- arc_dims(12L, 8L, direction)
    expect_warning(
      x <- arc_run(nrows = dims[["nrows"]], ncols = dims[["ncols"]],
                   checks = 3, check_direction = direction, l = 2,
                   budget = 300),
      class = "fieldhub_arc_low_df_warning"
    )
    # Persistent: shown each time, with no further warning
    for (time in 1:2) {
      expect_no_warning(printed <- arc_printed(x))
      expect_no_warning(summarised <- arc_summarised(x))
      blocks <- lapply(1:2, function(site) arc_details_block(printed, site))
      expect_identical(
        list(lapply(blocks, `[`, 2), lapply(blocks, utils::tail, 2L),
             lapply(1:2, function(site) arc_details_block(summarised, site))),
        list(rep(list("  Residual df: 3"), 2), rep(list(notice), 2), blocks),
        info = paste(direction, time)
      )
    }
  }
})

test_that("a location stopped by time_limit says so when printed", {
  clock <- arc_fake_clock()
  local_mocked_bindings(arc_clock = function() clock$advance(0.01))
  full <- arc_run(budget = 600, seed = 3)
  limited <- arc_run(budget = 600, seed = 3,
                     time_limit = full$timing$search / 2)
  expect_identical(
    list(arc_details_block(arc_printed(full), 1)[6],
         arc_details_block(arc_printed(limited), 1)[6]),
    list("  Search status: ok", "  Search status: ok (time limit reached)")
  )
})

test_that("field_layout(), plot() and draw_layout() work in both orientations", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  planters <- c("serpentine", "cartesian")
  cases <- expand.grid(direction = c("columns", "rows"), planter = planters,
                       stringsAsFactors = FALSE)
  for (i in seq_len(nrow(cases))) {
    direction <- cases$direction[i]
    nm <- paste(direction, cases$planter[i])
    dims <- arc_dims(10L, 8L, direction)
    title <- paste0("Augmented Row-Column Design ", dims[["nrows"]], " × ",
                    dims[["ncols"]])
    x <- arc_oriented(direction, planter = cases$planter[i], l = 2,
                      budget = 100, locationNames = c("A", "B"))
    # Plots are placed at construction: every planter returns the field book
    # as it is; there is only one layout option
    expect_identical(lapply(planters, function(p) field_layout(x, planter = p)),
                     list(x$fieldBook, x$fieldBook), info = nm)
    expect_error(field_layout(x, layout = 2), class = "fieldhub_input_error")
    plotted <- plot(x, l = 2)
    book <- arc_location_book(x, "B")
    rownames(book) <- NULL
    view <- plot_layout(x, l = 2)
    drawn <- draw_layout(x, book)
    maps <- list(drawn$p1, drawn$p2, view$out_layout, view$out_layoutPlots)
    expect_identical(
      list(inherits(plotted$p, "ggplot"), plotted$field_book,
           view$fieldBookXY, drawn$data,
           vapply(maps, function(m) inherits(m, "ggplot"), logical(1)),
           vapply(maps, function(m) m$labels$title, "")),
      list(TRUE, x$fieldBook, book, book, rep(TRUE, 4), rep(title, 4)),
      info = nm
    )
    # The plot-number map labels every plot with its number
    built <- ggplot2::ggplot_build(drawn$p2)$data
    text <- built[[length(built)]]
    expect_identical(text$label[match(paste(book$COLUMN, book$ROW),
                                      paste(text$x, text$y))],
                     sprintf("%d", as.integer(book$PLOT)), info = nm)
  }
})

test_that("the entry map labels entries and colours each check", {
  checks <- c("Check A", "B", "C", "D")
  for (direction in c("columns", "rows")) {
    x <- arc_oriented(direction, checks = checks, budget = 100, seed = 17)
    book <- x$fieldBook
    p1 <- draw_layout(x, book)$p1
    built <- ggplot2::ggplot_build(p1)$data
    geoms <- vapply(p1$layers, function(layer) class(layer$geom)[1L], "")
    tiles <- built[[match("GeomTile", geoms)]]
    text <- built[[match("GeomText", geoms)]]
    at <- paste(book$COLUMN, book$ROW)
    fill <- tiles$fill[match(at, paste(tiles$x, tiles$y))]
    test <- book$CHECKS == 0L
    # Each plot shows its entry number; tests on the shared neutral fill and
    # each check in its own colour
    expect_identical(
      list(text$label[match(at, paste(text$x, text$y))],
           unique(fill[test]), fill[!test], length(unique(fill[!test]))),
      list(as.character(book$ENTRY), fieldhub_layout_neutral(),
           expected_check_fills(book$CHECKS[!test]), length(checks)),
      info = direction
    )
  }
})

test_that("latin_rectangle print and summary output is unchanged by details", {
  # The output of print() and summary() rebuilt from base functions
  x <- latin_rectangle(c("No nitrogen", "A*B", "e 3", "4", "five"), 3, l = 2,
                       locationNames = c("Fargo", " CASSELTON"), seed = 7)
  for (n in c(10L, 6L)) {
    expected <- utils::capture.output({
      cat("Latin Rectangle Design", "\n\n")
      cat("Information on the design parameters:", "\n")
      str(x$infoDesign[names(x$infoDesign) != "id_design"])
      cat("\n", n, "First observations of the data frame with the latin_rectangle field book:",
          "\n")
      print(head(x$fieldBook, n = n))
    })
    expect_identical(utils::capture.output(print(x, n = n)), expected,
                     info = n)
  }
  expected <- utils::capture.output({
    cat("Latin Rectangle Design", "\n\n")
    cat("1. Information on the design parameters:", "\n")
    str(x$infoDesign)
    cat("\n")
    cat("2. Structure of the field book:", "\n\n")
    str(x$fieldBook)
  })
  expect_identical(utils::capture.output(print(summary(x))), expected)
})

# ---- Published values (internal scorer: no exported path takes a contraction)

# Contraction of a Bailey and Haines (2025) square layout: row of check i in
# each column, checks coded 109 to 111
arc_bh_contraction <- function(file) {
  codes <- arc_fixture(file)
  t(vapply(109:111, function(code) {
    apply(codes == code, 2L, which)
  }, integer(ncol(codes))))
}

# Half a unit in the last printed decimal, plus rounding slack
arc_published_tol <- function(decimals) 0.5 * 10^(-decimals) + 1e-12

test_that("the reported-statistics scorer reproduces published values", {
  # Piepho and Williams (2026) Examples 1 and 2: E_aug printed to 4 decimals
  # and the residual df k s - k - v - s + 2. Bailey and Haines (2025)
  # Table 1: C(3,4,5) has A_tt = 4.0341 and A_ct = 2.0910 (4 decimals) and
  # A_cc = 1/6; C(4,4,4) is disconnected and gets no objectives.
  cases <- list(
    example1 = list(contraction = arc_fixture("example1-contraction.csv"),
                    v = 12, want = list(connected = TRUE, df_formula = 3L),
                    values = c(E_aug = 0.3881), tol = arc_published_tol(4)),
    example2 = list(contraction = arc_fixture("example2-contraction.csv"),
                    v = 24, want = list(connected = TRUE, df_formula = 37L),
                    values = c(E_aug = 0.6031), tol = arc_published_tol(4)),
    bh_c345 = list(contraction = arc_bh_contraction("bailey-haines-c-3-4-5.csv"),
                   v = 12, want = list(connected = TRUE),
                   values = c(V_tt = 4.0341, V_tc = 2.0910),
                   tol = arc_published_tol(4)),
    bh_c345_cc = list(contraction = arc_bh_contraction("bailey-haines-c-3-4-5.csv"),
                      v = 12, want = list(connected = TRUE),
                      values = c(V_cc = 1 / 6), tol = 1e-10),
    bh_c444 = list(contraction = arc_bh_contraction("bailey-haines-c-4-4-4.csv"),
                   v = 12, want = list(connected = FALSE),
                   absent = c("E_aug", "V_tt", "V_tc", "V_cc", "max_tt",
                              "max_tc"),
                   values = numeric(), tol = 0)
  )
  for (nm in names(cases)) {
    case <- cases[[nm]]
    got <- arc_final_score(case$contraction, case$v, "restricted")
    reported <- unlist(got[names(case$values)])
    dev <- if (length(reported) == length(case$values)) {
      abs(reported - case$values)
    } else {
      Inf # a published quantity is not reported
    }
    expect(identical(got[names(case$want)], case$want) &&
             !anyNA(dev) && all(dev <= case$tol) &&
             !any(case$absent %in% names(got)),
           sprintf("case %s: fields %s, worst deviation %g", nm,
                   paste(names(case$want), collapse = "/"),
                   if (length(dev)) max(dev) else 0))
  }
})

test_that("the paper route reproduces every row of Table 1", {
  # Piepho and Williams (2026) Table 1 (supplement contractions): E_aug to 6
  # decimals, Cbar_v and Cbar_s to 4, and the full-precision reference
  # (independent least-squares oracle) to 1e-10
  manifest <- arc_manifest()
  table1 <- manifest[startsWith(manifest$file, "table1-"), ]
  expect_identical(nrow(table1), 21L)
  for (i in seq_len(nrow(table1))) {
    entry <- table1[i, ]
    result <- arc_evaluate_contraction(arc_fixture(entry$file), entry$v,
                                       entry$mode)
    dev <- c(E_aug = abs(result$E_aug - entry$pub_E_aug) -
               arc_published_tol(entry$pub_E_aug_dec),
             ref = abs(result$E_aug - entry$ref_E_aug) - 1e-10,
             Cbar_v = abs(result$Cbar_v - entry$pub_Cbar_v) -
               arc_published_tol(entry$pub_Cbar_v_dec),
             Cbar_s = abs(result$Cbar_s - entry$pub_Cbar_s) -
               arc_published_tol(entry$pub_Cbar_s_dec))
    # all four quantities must be reported: c() drops a missing one
    expect(isTRUE(result$connected) && length(dev) == 4L && !anyNA(dev) &&
             all(dev < 0),
           sprintf("%s: %s outside its published tolerance by %g", entry$file,
                   names(dev)[which.max(dev)], max(dev)))
  }
})

test_that("Cbar_s projects out the trivial eigenvalue instead of dropping the smallest", {
  # Example 1: dropping the smallest eigenvalue of the corrected eq. (4)
  # bracket (whose trivial eigenvalue is 1) gives 0.5600, not the published
  # Cbar_s = 0.4828
  contraction <- arc_fixture("example1-contraction.csv")
  v <- 12
  k <- nrow(contraction)
  s <- ncol(contraction)
  mats <- arc_contraction_matrices(contraction, v)
  m12 <- mats$nc - outer(mats$r, rep(1, s)) / s
  row_block <- diag(mats$r) - mats$w / s + matrix(1, v, v)
  bracket <- diag(s) - crossprod(m12, solve(row_block, m12)) / k
  lambda <- eigen((bracket + t(bracket)) / 2, symmetric = TRUE,
                  only.values = TRUE)$values
  naive <- (s - 1) / sum(1 / lambda[seq_len(s - 1)])
  expect_lt(abs(naive - 0.5600), arc_published_tol(4))
  result <- arc_evaluate_contraction(contraction, v, "restricted")
  expect_lt(abs(result$Cbar_s - 0.4828), arc_published_tol(4))
})

test_that("layouts confounded with rows or columns have zero information", {
  # C = 0 exactly: the rank decision must not be relative to the rounding
  # noise of an all-zero spectrum. Residual df = n - rank(Z), never -1.
  two <- matrix(c(1L, 2L, 1L, 2L), nrow = 2L)
  one <- matrix(c(1L, 1L, 2L, 2L, 3L, 4L), ncol = 1L)
  cases <- list(
    # 1 1 / 2 2: n = 4, rank(Z) = 3, residual df 1
    rows = list(layouts = list(two, two[2:1, ], two[, 2:1], t(two)), df = 1L),
    # six rows, one column: rank(Z) = 6, residual df 0
    column = list(layouts = list(one, one[c(6, 3, 1, 5, 2, 4), , drop = FALSE],
                                 t(one)), df = 0L)
  )
  absent <- c("cefs", "E_aug", "V_tt", "V_tc", "V_cc", "max_tt", "max_tc")
  for (nm in names(cases)) {
    ok <- vapply(cases[[nm]]$layouts, function(codes) {
      r <- arc_evaluate_layout(codes)
      !r$connected && identical(r$rank_C, 0L) &&
        identical(r$residual_df_rank, cases[[nm]]$df) &&
        !any(absent %in% names(r))
    }, logical(1))
    expect(all(ok), sprintf("case %s: layout %d reports information", nm,
                            which(!ok)[1]))
  }
})

# ---- Exhaustive 6 x 4 enumeration (research verification counts) ----------
# v = 6 pseudo treatments, each replicated twice, k = 3, s = 4

arc_enum <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      all <- arc_enumerate_6x4()
      cache <<- list(all = all,
                     restricted = all[vapply(all, arc_is_row_binary, logical(1))])
    }
    cache
  }
})
arc_enum_objectives <- function(reps, mode) {
  lapply(reps, arc_evaluate_objectives, v = 6L, mode = mode)
}
arc_enum_layouts <- function(reps, mode, evaluator) {
  lapply(reps, function(m) evaluator(arc_expand(m, 6L, mode)$codes))
}
arc_enum_connected <- function(results) {
  vapply(results, function(r) isTRUE(r$connected), logical(1))
}
# Counts and value classes: classes are the distinct (E_aug, V_tt) pairs of
# connected designs, rounded to 10 decimals; opposite counts class pairs
# that E_aug and V_tt rank in opposite order
arc_enum_summary <- function(results) {
  connected <- arc_enum_connected(results)
  values <- do.call(rbind, lapply(results[connected], function(r) {
    round(c(r$E_aug, r$V_tt), 10)
  }))
  classes <- unique(values)
  pairs <- utils::combn(nrow(classes), 2L)
  opposite <- sign(classes[pairs[1, ], 1] - classes[pairs[2, ], 1]) *
    sign(classes[pairs[2, ], 2] - classes[pairs[1, ], 2]) < 0
  list(n = length(results), disconnected = sum(!connected),
       classes = nrow(classes), best = round(max(classes[, 1]), 9),
       opposite = sum(opposite), pairs = ncol(pairs))
}

test_that("the 6 x 4 enumeration reproduces the research counts in both modes", {
  enum <- arc_enum()
  cases <- list(
    column_binary = list(reps = enum$all,
                         want = list(n = 18360L, disconnected = 5880L,
                                     classes = 25L, best = 0.450643777,
                                     opposite = 10L, pairs = 300L)),
    restricted = list(reps = enum$restricted,
                      want = list(n = 1920L, disconnected = 240L,
                                  classes = 6L, best = 0.450643777,
                                  opposite = 0L, pairs = 15L))
  )
  for (mode in names(cases)) {
    # best is rounded to the 9 printed decimals of the reference, so it must
    # equal it: relative 1e-9 is below one unit of the ninth decimal (2.2e-9
    # relative) and far above rounding; the counts are integers
    expect_equal(arc_enum_summary(arc_enum_objectives(cases[[mode]]$reps, mode)),
                 cases[[mode]]$want, tolerance = 1e-9, info = mode)
  }
})

test_that("contraction and full-layout connectivity agree on a stratified sample", {
  # every restricted disconnected design, plus every 36th column-binary one
  enum <- arc_enum()
  restricted_connected <- arc_enum_connected(
    arc_enum_objectives(enum$restricted, "restricted"))
  cases <- list(
    restricted = enum$restricted[!restricted_connected],
    column_binary = enum$all[seq(36L, length(enum$all), by = 36L)]
  )
  for (mode in names(cases)) {
    reps <- cases[[mode]]
    expect_identical(
      arc_enum_connected(arc_enum_layouts(reps, mode, arc_evaluate_layout)),
      arc_enum_connected(arc_enum_objectives(reps, mode)), info = mode)
  }
})

test_that("exhaustive: contraction, layout and oracle agree on every representative", {
  skip_if_not(identical(Sys.getenv("FIELDHUB_ARC_EXHAUSTIVE"), "true"),
              "exhaustive suite")
  enum <- arc_enum()
  cases <- list(column_binary = list(reps = enum$all, disconnected = 5880L),
                restricted = list(reps = enum$restricted, disconnected = 240L))
  for (mode in names(cases)) {
    reps <- cases[[mode]]$reps
    objectives <- arc_enum_objectives(reps, mode)
    layouts <- arc_enum_layouts(reps, mode, arc_evaluate_layout)
    oracles <- arc_enum_layouts(reps, mode, arc_oracle)
    verdict <- arc_enum_connected(objectives)
    expect_identical(list(arc_enum_connected(layouts),
                          arc_enum_connected(oracles), sum(!verdict)),
                     list(verdict, verdict, cases[[mode]]$disconnected),
                     info = mode)
    e_of <- function(results) {
      vapply(results[verdict], function(r) r$E_aug, numeric(1))
    }
    # E_aug of 6 x 4 designs from independent factorizations: 1e-10
    e <- e_of(objectives)
    dev <- max(abs(e_of(layouts) - e), abs(e_of(oracles) - e))
    expect(dev <= 1e-10 && abs(max(e) - 0.450643777) <= 1e-9,
           sprintf("%s: deviation %g, best %.10f", mode, dev, max(e)))
  }
})

test_that("every fixed-replication 6 x 4 column multiset recolours row-binary", {
  # The 85 sorted column multisets of the enumeration: recolouring rows keeps
  # each column's set and the replication and removes every row repeat
  v <- 6L
  s <- 4L
  subsets <- utils::combn(v, 3L, simplify = FALSE)
  multisets <- list()
  extend <- function(start, chosen, counts) {
    if (length(chosen) == s) {
      if (all(counts == 2L)) multisets[[length(multisets) + 1L]] <<- chosen
      return(invisible(NULL))
    }
    for (i in start:length(subsets)) {
      next_counts <- counts
      next_counts[subsets[[i]]] <- next_counts[subsets[[i]]] + 1L
      if (all(next_counts <= 2L)) extend(i, c(chosen, i), next_counts)
    }
  }
  extend(1L, integer(0), integer(v))
  ok <- vapply(multisets, function(chosen) {
    original <- do.call(cbind, subsets[chosen])
    recolored <- arc_recolor_rows(original, v)
    identical(dim(recolored), dim(original)) &&
      all(apply(recolored, 1L, anyDuplicated) == 0L) &&
      all(vapply(seq_len(s), function(j) {
        setequal(recolored[, j], original[, j]) &&
          !anyDuplicated(recolored[, j])
      }, logical(1))) &&
      identical(tabulate(recolored, v), tabulate(original, v))
  }, logical(1))
  expect(length(ok) == 85L && all(ok),
         sprintf("%d multisets, multiset %d fails", length(ok), which(!ok)[1]))
})

# ---- Search deadline: work bound (hard gate) -------------------------------
# After the deadline is observed passed, the staged search performs at most
# one further operation (an objective or proxy evaluation, a proposal, a scan
# move, a recolouring or a completed random start). Counted, never timed: the
# injected clock of arc_work_bound_run() (helper-arc-search.R) makes the
# deadline pass at a chosen read.

test_that("the staged search does at most one operation after the deadline", {
  skip_on_cran() # about 4 s of repeated searches
  stride <- if (identical(Sys.getenv("FIELDHUB_ARC_SEARCH_SLOW"), "true")) 1L else 6L
  cases <- list(rectangular = list(v = 12, s = 8),
                square = list(v = 12, s = 12))
  for (nm in names(cases)) {
    args <- cases[[nm]]
    # Switch reads: every stride-th read up to the natural end, plus every
    # read within 2 of each restart start and recolouring of an unswitched run
    full <- do.call(arc_work_bound_run, c(list("staged", Inf), args))
    last <- max(full$ops$read)
    marks <- full$ops$read[full$ops$kind %in% c("start", "recolour")]
    near <- unlist(lapply(marks, function(m) seq.int(m - 2L, m + 2L)))
    points <- sort(unique(c(seq.int(2L, last, by = stride), near, last)))
    points <- points[points >= 2L & points <= last]
    bad <- character()
    for (n in points) {
      r <- do.call(arc_work_bound_run, c(list("staged", n), args))
      if (nrow(r$after) > 1L || !r$result$status %in% c("ok", "no_design")) {
        bad <- c(bad, sprintf("read %d: %d operations (%s), status %s", n,
                              nrow(r$after), paste(r$after$kind, collapse = ","),
                              r$result$status))
      }
    }
    expect(length(points) > 20L && length(bad) == 0L,
           sprintf("case %s, %d switch reads: %s", nm, length(points),
                   paste(utils::head(bad, 3L), collapse = "; ")))
  }
})

# ---- Slow tier -------------------------------------------------------------

test_that("slow: the search reaches Example 1's published E_aug", {
  arc_skip_unless_search_slow()
  # Piepho and Williams (2026) Example 1: 12 x 8 with 3 checks, optimum
  # E_aug printed as 0.3881; at least 4 of 5 seeds reach it
  hits <- vapply(1:5, function(seed) {
    x <- arc_muffle_low_df(augmented_row_column(
      nrows = 12, ncols = 8, checks = 3, check_direction = "columns",
      objective = "E_aug", budget = 20000, seed = seed))
    abs(x$statistics$E_aug - 0.3881) <= arc_published_tol(4)
  }, logical(1))
  expect_gte(sum(hits), 4L)
})
