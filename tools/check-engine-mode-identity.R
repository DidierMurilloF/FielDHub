# Bit-identity of the block engine's modes 0, 1 and 2 (phase 2 spec 6.1:
# "Modes 0, 1 and 2 must stay bit-identical after both modes are added").
#
#   Rscript tools/check-engine-mode-identity.R record  <hashes.rds> [library]
#   Rscript tools/check-engine-mode-identity.R compare <hashes.rds> [library]
#
# Each case is a seeded output of the installed FielDHub, hashed as the md5 of
# its serialize()d value (functions and environments removed). `record` runs
# every case twice, fails if the two runs differ (a case that does not
# reproduce cannot pin identity) and writes the hashes. `compare` reruns the
# cases and prints one PASS / FAIL line each; it exits 1 on any mismatch or
# missing case. [library] defaults to ~/fieldhub-libs/dev. Install first with
# `R CMD INSTALL . --preclean` (AGENTS.md).
#
# Cases:
# - blocks_engine() single-level (mode 0): 3 shapes x 3 seeds, one of them in
#   sampled mode (more than 2e5 plot pairs);
# - do_optim(): the two seeded rows of phase 1 Task 1 Step 7 (sparse, sampled
#   search; sparse, square-lattice start) and the p-rep row of the catalogue;
# - blocks_engine(resolvable = TRUE) (mode 1): 4 shapes x 3 seeds, including a
#   square-lattice start and r = 2, k = 2;
# - row_column(method = "twostage") (modes 1 and 2): 3 shapes x 3 seeds.

arguments <- commandArgs(trailingOnly = TRUE)
usage <- "Usage: Rscript tools/check-engine-mode-identity.R <record|compare> <hashes.rds> [library]"
if (length(arguments) < 2L || length(arguments) > 3L || !arguments[1] %in% c("record", "compare")) {
  message(usage)
  quit(status = 1)
}
action <- arguments[1]
path <- path.expand(arguments[2])
library_path <- if (length(arguments) == 3L) arguments[3] else "~/fieldhub-libs/dev"
.libPaths(c(normalizePath(library_path, mustWork = TRUE), .libPaths()))
suppressMessages(library(FielDHub))
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

# Functions and environments carry addresses, not results: drop them.
strip <- function(x) {
  if (is.function(x) || is.environment(x)) return(NULL)
  if (is.list(x) && !is.data.frame(x)) {
    keep <- !vapply(x, function(e) is.function(e) || is.environment(e), logical(1))
    a <- attributes(x)
    x <- lapply(x[keep], strip)
    if (!is.null(a$class)) class(x) <- a$class
  }
  x
}
hash <- function(x) {
  file <- tempfile()
  on.exit(unlink(file))
  writeBin(serialize(strip(x), NULL, version = 3L), file)
  unname(tools::md5sum(file))
}
quiet <- function(expr) suppressWarnings(suppressMessages({
  utils::capture.output(value <- expr)
  value
}))
engine <- function(...) {
  args <- list(...)
  function(seed) {
    set.seed(seed)
    quiet(do.call(FielDHub:::blocks_engine, args))
  }
}

cases <- list()
add <- function(name, run) cases[[name]] <<- run
seeds <- 1:3
single <- list(c(treatments = 12, replicates = 3, blocks = 6),
               c(treatments = 30, replicates = 2, blocks = 10),
               c(treatments = 160, replicates = 4, blocks = 40))  # 640 plots: sampled mode
for (s in single) {
  for (seed in seeds) {
    add(sprintf("mode0_v%d_r%d_b%d_seed%d", s[["treatments"]], s[["replicates"]], s[["blocks"]], seed),
        local({ f <- engine(treatments = s[["treatments"]], replicates = s[["replicates"]],
                            blocks = s[["blocks"]]); sd <- seed; function() f(sd) }))
  }
}
add("do_optim_sparse_sampled", function() quiet(do_optim(
  design = "sparse", lines = 400, l = 3, copies_per_entry = 2, add_checks = TRUE, checks = 2, seed = 1)))
add("do_optim_sparse_lattice_start", function() quiet(do_optim(
  design = "sparse", lines = 9, l = 6, copies_per_entry = 2, add_checks = TRUE, checks = 2, seed = 1)))
add("do_optim_prep", function() quiet(do_optim(
  design = "prep", lines = 80, l = 4, copies_per_entry = 5, add_checks = TRUE, checks = 2,
  rep_checks = c(4, 4), seed = 37)))
resolvable <- list(c(treatments = 20, replicates = 3, blocks = 5),
                   c(treatments = 16, replicates = 3, blocks = 4),   # square-lattice start
                   c(treatments = 12, replicates = 2, blocks = 6),   # r = 2, k = 2
                   c(treatments = 30, replicates = 2, blocks = 5))
for (s in resolvable) {
  for (seed in seeds) {
    add(sprintf("mode1_v%d_r%d_b%d_seed%d", s[["treatments"]], s[["replicates"]], s[["blocks"]], seed),
        local({ f <- engine(treatments = s[["treatments"]], replicates = s[["replicates"]],
                            blocks = s[["blocks"]], resolvable = TRUE); sd <- seed; function() f(sd) }))
  }
}
twostage <- list(c(t = 12, nrows = 3, reps = 2), c(t = 30, nrows = 5, reps = 3),
                 c(t = 48, nrows = 6, reps = 3))
for (s in twostage) {
  for (seed in seeds) {
    add(sprintf("twostage_t%d_nrows%d_r%d_seed%d", s[["t"]], s[["nrows"]], s[["reps"]], seed),
        local({ sh <- s; sd <- seed; function() {
          x <- quiet(row_column(t = sh[["t"]], nrows = sh[["nrows"]], reps = sh[["reps"]],
                                seed = sd, method = "twostage"))
          list(fieldBook = x$fieldBook, blocksModel = x$blocksModel)
        } }))
  }
}

hashes <- vapply(names(cases), function(name) hash(cases[[name]]()), character(1))
if (action == "record") {
  again <- vapply(names(cases), function(name) hash(cases[[name]]()), character(1))
  unstable <- names(cases)[hashes != again]
  if (length(unstable)) {
    cat("FAIL not reproducible:", paste(unstable, collapse = ", "), "\n")
    quit(status = 1)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(list(hashes = hashes, library = find.package("FielDHub"),
               version = as.character(utils::packageVersion("FielDHub")),
               r = R.version.string, recorded = Sys.time()), path)
  cat("Recorded", length(hashes), "hashes to", path, "\n")
  quit(status = 0)
}
recorded <- readRDS(path)$hashes
failed <- FALSE
for (name in union(names(recorded), names(hashes))) {
  ok <- !is.na(recorded[name]) && !is.na(hashes[name]) && identical(recorded[[name]], hashes[[name]])
  cat(if (ok) "PASS" else "FAIL", name, "\n")
  if (!ok) failed <- TRUE
}
cat(sum(names(hashes) %in% names(recorded) & hashes == recorded[names(hashes)], na.rm = TRUE),
    "of", length(recorded), "recorded hashes match\n")
quit(status = if (failed) 1 else 0)
