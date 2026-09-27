# Run in a fresh R process against an installation of the unmodified v1.5.0 tag.
# Usage: Rscript tools/generate-legacy-fixtures.R <library> <output.rds>
args <- commandArgs(TRUE)
stopifnot(length(args) == 2L)
library(FielDHub, lib.loc = args[1L])
stopifnot(identical(as.character(utils::packageVersion("FielDHub")), "1.5.0"))
cases <- list(
  CRD = list(t = 5, reps = 3, locationName = "FARGO"),
  RCBD = list(t = 6, reps = 3),
  latin_square = list(t = 4, reps = 2),
  full_factorial = list(setfactors = c(2, 3), reps = 2),
  split_plot = list(wp = 3, sp = 2, reps = 2),
  split_split_plot = list(wp = 2, sp = 2, ssp = 2, reps = 2),
  strip_plot = list(Hplots = 3, Vplots = 2, b = 2, plotNumber = 101),
  incomplete_blocks = list(t = 12, k = 4, r = 2),
  alpha_lattice = list(t = 12, k = 4, r = 2),
  square_lattice = list(t = 16, k = 4, r = 2),
  rectangular_lattice = list(t = 12, k = 3, r = 2),
  row_column = list(t = 24, nrows = 6, r = 2, iterations = 100, seed = 21),
  diagonal_arrangement = list(nrows = 15, ncols = 20, lines = 270, checks = 4),
  optimized_arrangement = list(nrows = 12, ncols = 10, lines = 100,
                               amountChecks = 20, checks = 1:5),
  RCBD_augmented = list(lines = 50, checks = 3, b = 5),
  partially_replicated = list(nrows = 8, ncols = 8, repGens = c(50, 7), repUnits = c(1, 2)),
  sparse_allocation = list(lines = 120, l = 4, copies_per_entry = 3, checks = 4),
  multi_location_prep = list(lines = 80, l = 4, copies_per_entry = 5,
                             checks = 2, rep_checks = c(4, 4)),
  split_families = list(l = 3, data = data.frame(ENTRY = 1:60, NAME = paste0("SB-", 1:60),
                                               FAMILY = rep(1:6, each = 10)))
)
# The published 1.5.0 split_families() example defines this global. That
# version mistakenly reads it instead of assigning the supplied data locally.
gen.list <- cases$split_families$data
designs <- setNames(vector("list", length(cases)), names(cases))
for (name in names(cases)) {
  set.seed(38)
  inputs <- cases[[name]]
  if ("seed" %in% names(formals(get(name))) && is.null(inputs$seed)) inputs$seed <- 38
  cases[[name]] <- inputs
  designs[[name]] <- do.call(name, inputs)
  stopifnot(inherits(designs[[name]], "FielDHub"))
  cat(name, "OK\n")
}
bundle <- list(
  source_version = "1.5.0",
  source_commit = "13585d0a6ce39a76125dcdbf57315ff1c992eb11",
  r_version = R.version.string,
  rng_kind = RNGkind(),
  dependency_versions = vapply(c("blocksdesign", "dplyr", "numbers"),
                               function(x) as.character(utils::packageVersion(x)), character(1)),
  initial_seed = 38,
  globals = list(gen.list = gen.list),
  calls = cases,
  designs = designs
)
saveRDS(bundle, args[2L], version = 2, compress = "xz")
