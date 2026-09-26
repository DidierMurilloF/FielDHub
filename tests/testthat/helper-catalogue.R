# Catalogue of fixed-seed calls to every exported design function.
#
# Each entry builds one small design. The golden tests snapshot the result of
# every entry, so any change to a design's output fails a test; a change that
# is intended is accepted with testthat::snapshot_accept(). Entries set `seed`,
# and `year` where the function has it, so the output does not depend on the
# clock. Functions without a `seed` argument are called after set.seed().

catalogue <- list(
  # Classic designs ----------------------------------------------------------
  CRD_count = list(fun = "CRD", family = "classic", build = function() {
    CRD(t = 5, reps = 3, plotNumber = 101, locationName = "FARGO", seed = 1)
  }),
  CRD_labels = list(fun = "CRD", family = "classic", build = function() {
    CRD(t = c("A", "B", "C", "D"), reps = 2, seed = 2)
  }),
  CRD_data = list(fun = "CRD", family = "classic", build = function() {
    CRD(data = data.frame(Treatment = paste0("T", 1:4), Reps = c(2, 3, 2, 3)), seed = 3)
  }),
  RCBD_two_locations = list(fun = "RCBD", family = "classic", build = function() {
    RCBD(t = 6, reps = 3, l = 2, plotNumber = c(101, 1001),
         locationNames = c("A", "B"), seed = 4)
  }),
  RCBD_checks = list(fun = "RCBD", family = "classic", build = function() {
    RCBD(t = 8, reps = 3, checks = c("CK1", "CK2"), rep_checks = c(2, 2),
         plotNumber = 101, seed = 5)
  }),
  RCBD_cartesian = list(fun = "RCBD", family = "classic", build = function() {
    RCBD(t = 5, reps = 3, planter = "cartesian", continuous = TRUE, seed = 6)
  }),
  latin_square = list(fun = "latin_square", family = "classic", build = function() {
    latin_square(t = 4, reps = 2, plotNumber = 101, seed = 7)
  }),
  latin_square_cartesian = list(fun = "latin_square", family = "classic", build = function() {
    latin_square(t = 5, reps = 1, plotNumber = 1, planter = "cartesian", seed = 8)
  }),
  full_factorial_rcbd = list(fun = "full_factorial", family = "classic", build = function() {
    full_factorial(setfactors = c(2, 3), reps = 2, l = 2, type = 2,
                   plotNumber = c(101, 1001), seed = 9)
  }),
  full_factorial_crd_data = list(fun = "full_factorial", family = "classic", build = function() {
    full_factorial(reps = 2, l = 1, type = 1, plotNumber = 101, seed = 10,
                   data = data.frame(factor = c("N", "N", "P", "P", "P"),
                                     level = c(0, 1, 0, 1, 2)))
  }),
  split_plot_rcbd = list(fun = "split_plot", family = "classic", build = function() {
    split_plot(wp = 3, sp = 2, reps = 2, l = 2, plotNumber = c(101, 1001), seed = 11)
  }),
  split_plot_crd = list(fun = "split_plot", family = "classic", build = function() {
    split_plot(wp = c("W1", "W2"), sp = c("a", "b", "c"), reps = 2, type = 1,
               plotNumber = 101, seed = 12)
  }),
  split_split_plot_rcbd = list(fun = "split_split_plot", family = "classic", build = function() {
    split_split_plot(wp = 2, sp = 2, ssp = 2, reps = 2, plotNumber = 101, seed = 13)
  }),
  split_split_plot_crd = list(fun = "split_split_plot", family = "classic", build = function() {
    split_split_plot(wp = 2, sp = 3, ssp = 2, reps = 2, type = 1, plotNumber = 101, seed = 14)
  }),
  strip_plot = list(fun = "strip_plot", family = "classic", build = function() {
    strip_plot(Hplots = 3, Vplots = 2, b = 2, l = 2, plotNumber = c(101, 1001), seed = 15)
  }),
  strip_plot_labels = list(fun = "strip_plot", family = "classic", build = function() {
    strip_plot(Hplots = c("H1", "H2"), Vplots = c("V1", "V2", "V3"), b = 3,
               plotNumber = 101, planter = "cartesian", seed = 16)
  }),

  # Incomplete-block designs -------------------------------------------------
  incomplete_blocks = list(fun = "incomplete_blocks", family = "incomplete", build = function() {
    incomplete_blocks(t = 12, k = 4, r = 2, plotNumber = 101, seed = 17)
  }),
  incomplete_blocks_labels = list(fun = "incomplete_blocks", family = "incomplete", build = function() {
    incomplete_blocks(t = paste0("V", 1:10), k = 5, r = 2, l = 2,
                      plotNumber = c(1, 101), seed = 18)
  }),
  alpha_lattice = list(fun = "alpha_lattice", family = "incomplete", build = function() {
    alpha_lattice(t = 12, k = 4, r = 2, plotNumber = 101, seed = 19)
  }),
  square_lattice = list(fun = "square_lattice", family = "incomplete", build = function() {
    square_lattice(t = 16, k = 4, r = 2, plotNumber = 101, seed = 20)
  }),
  rectangular_lattice = list(fun = "rectangular_lattice", family = "incomplete", build = function() {
    rectangular_lattice(t = 12, k = 3, r = 2, plotNumber = 101, seed = 21)
  }),
  row_column = list(fun = "row_column", family = "incomplete", build = function() {
    row_column(t = 12, nrows = 3, r = 2, plotNumber = 101, seed = 22)
  }),
  row_column_twostage = list(fun = "row_column", family = "incomplete", build = function() {
    row_column(t = 12, nrows = 3, r = 2, l = 2, plotNumber = c(101, 1001),
               method = "twostage", seed = 23)
  }),

  # Unreplicated designs -----------------------------------------------------
  diagonal_single = list(fun = "diagonal_arrangement", family = "unreplicated", build = function() {
    diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4,
                         plotNumber = 101, seed = 24, year = 2026)
  }),
  diagonal_single_cartesian = list(fun = "diagonal_arrangement", family = "unreplicated", build = function() {
    diagonal_arrangement(nrows = 10, ncols = 32, lines = 287, checks = 4,
                         planter = "cartesian", plotNumber = 101, seed = 25, year = 2026)
  }),
  diagonal_blocks_row = list(fun = "diagonal_arrangement", family = "unreplicated", build = function() {
    diagonal_arrangement(nrows = 30, ncols = 26, lines = 720, checks = 5,
                         kindExpt = "DBUDC", splitBy = "row",
                         blocks = c(150, 155, 95, 200, 120), plotNumber = 1,
                         seed = 26, year = 2026)
  }),
  diagonal_blocks_column_same = list(fun = "diagonal_arrangement", family = "unreplicated", build = function() {
    diagonal_arrangement(nrows = 20, ncols = 25, lines = 400, checks = 4,
                         kindExpt = "DBUDC", splitBy = "column", blocks = rep(40, 10),
                         sameEntries = TRUE, plotNumber = 1, seed = 27, year = 2026)
  }),
  optimized_arrangement = list(fun = "optimized_arrangement", family = "unreplicated", build = function() {
    optimized_arrangement(nrows = 12, ncols = 10, lines = 100, amountChecks = 20,
                          checks = 1:5, plotNumber = 101, seed = 28, year = 2026)
  }),
  RCBD_augmented = list(fun = "RCBD_augmented", family = "unreplicated", build = function() {
    RCBD_augmented(lines = 50, checks = 3, b = 5, plotNumber = 101, seed = 29, year = 2026)
  }),
  RCBD_augmented_fixed = list(fun = "RCBD_augmented", family = "unreplicated", build = function() {
    RCBD_augmented(lines = 122, checks = 4, b = 5, nrows = 5, ncols = 29, random = FALSE,
                   plotNumber = 101, seed = 30, year = 2026)
  }),
  RCBD_augmented_two_locations = list(fun = "RCBD_augmented", family = "unreplicated", build = function() {
    RCBD_augmented(lines = 40, checks = 4, b = 4, l = 2, plotNumber = c(1, 101),
                   locationNames = c("A", "B"), seed = 31, year = 2026)
  }),

  # Partially replicated designs and allocations -----------------------------
  partially_replicated = list(fun = "partially_replicated", family = "allocation", build = function() {
    partially_replicated(nrows = 8, ncols = 8, repGens = c(50, 7), repUnits = c(1, 2),
                         planter = "cartesian", plotNumber = 101, seed = 32, year = 2026)
  }),
  partially_replicated_fillers = list(fun = "partially_replicated", family = "allocation", build = function() {
    partially_replicated(nrows = 9, ncols = 8, repGens = c(50, 7), repUnits = c(1, 2),
                         allow_fillers = TRUE, plotNumber = 101, seed = 33, year = 2026)
  }),
  multi_location_prep = list(fun = "multi_location_prep", family = "allocation", build = function() {
    multi_location_prep(lines = 80, l = 4, copies_per_entry = 5, checks = 2,
                        rep_checks = c(4, 4), allow_fillers = TRUE, seed = 34, year = 2026)
  }),
  sparse_allocation = list(fun = "sparse_allocation", family = "allocation", build = function() {
    sparse_allocation(lines = 120, l = 4, copies_per_entry = 3, checks = 4,
                      seed = 35, year = 2026)
  }),
  do_optim_sparse = list(fun = "do_optim", family = "allocation", build = function() {
    do_optim(design = "sparse", lines = 120, l = 4, copies_per_entry = 3,
             add_checks = TRUE, checks = 4, seed = 36)
  }),
  do_optim_prep = list(fun = "do_optim", family = "allocation", build = function() {
    do_optim(design = "prep", lines = 80, l = 4, copies_per_entry = 5,
             add_checks = TRUE, checks = 2, rep_checks = c(4, 4), seed = 37)
  }),
  split_families = list(fun = "split_families", family = "allocation", build = function() {
    gen_list <- data.frame(ENTRY = 1:60, NAME = paste0("SB-", 1:60),
                           FAMILY = rep(1:6, times = c(14, 12, 10, 10, 8, 6)))
    set.seed(38)
    split_families(l = 3, data = gen_list)
  }),
  swap_pairs = list(fun = "swap_pairs", family = "allocation", build = function() {
    set.seed(39)
    X <- matrix(sample(c(rep(1:10, 2), 11:50)), ncol = 10)
    swap_pairs(X, starting_dist = 3, stop_iter = 10)
  })
)

# Builds each design once per test run and caches it.
catalogue_cache <- new.env(parent = emptyenv())

catalogue_design <- function(name) {
  if (!exists(name, envir = catalogue_cache, inherits = FALSE)) {
    entry <- catalogue[[name]]
    design <- suppressWarnings(suppressMessages({
      output <- NULL
      utils::capture.output(output <- entry$build())
      output
    }))
    assign(name, design, envir = catalogue_cache)
  }
  base::get(name, envir = catalogue_cache, inherits = FALSE)
}

# Catalogue entries of one family.
catalogue_family <- function(family) {
  names(catalogue)[vapply(catalogue, function(e) e$family == family, logical(1))]
}

# The golden snapshots run on the reference platform (macOS) unless
# FIELDHUB_GOLDEN is "false", and elsewhere only when it is "true", because
# floating-point optimizers can pick different designs on other platforms.
skip_unless_golden_platform <- function() {
  golden <- Sys.getenv("FIELDHUB_GOLDEN")
  if (identical(golden, "true")) return(invisible(TRUE))
  if (identical(golden, "false")) testthat::skip("FIELDHUB_GOLDEN is false")
  if (!identical(Sys.info()[["sysname"]], "Darwin")) {
    testthat::skip("golden snapshots are recorded on macOS")
  }
  invisible(TRUE)
}

# What a golden snapshot records: the full result and, for the designs whose
# field map is built by plot_layout() (id_design 1 to 12), the field book with
# the ROW and COLUMN coordinates that the app exports.
golden_view <- function(design) {
  view <- list(design = design)
  id <- if (inherits(design, "FielDHub")) design$infoDesign$id_design else NULL
  if (is.numeric(id) && id <= 12) {
    view$layout <- tryCatch(
      suppressWarnings(suppressMessages(plot_layout(design, layout = 1)$allSitesFieldbook)),
      error = function(e) "plot_layout() fails for this design"
    )
  }
  view
}

# One golden test per catalogue entry of a family
test_golden_family <- function(family) {
  for (name in catalogue_family(family)) {
    local({
      entry <- name
      testthat::test_that(paste(entry, "keeps its output"), {
        skip_unless_golden_platform()
        testthat::expect_snapshot_value(golden_view(catalogue_design(entry)), style = "json2")
      })
    })
  }
}
