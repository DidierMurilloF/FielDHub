# Bit-identity of the block engine's modes 0, 1 and 2 (phase 2 spec 6.1:
# "Modes 0, 1 and 2 must stay bit-identical after both modes are added"), and
# seeded outputs of modes 3 and 4 (phase 2 plan, Task 8 Step 3b: the
# treatment-space form must not change a call with at most 500 block
# columns).
#
#   Rscript tools/check-engine-mode-identity.R record  <hashes.rds> [library] [set=012|34]
#   Rscript tools/check-engine-mode-identity.R compare <hashes.rds> [library]
#
# Each case is a seeded output of the installed FielDHub, hashed as the md5 of
# its serialize()d value (functions and environments removed). `record` runs
# every case of its set (default 012) twice, fails if the two runs differ (a
# case that does not reproduce cannot pin identity) and writes the hashes.
# `compare` reruns the cases of the set the file was recorded with and prints
# one PASS / FAIL line each; it exits 1 on any mismatch or missing case.
# [library] defaults to ~/fieldhub-libs/dev. Install first with
# `R CMD INSTALL . --preclean` (AGENTS.md).
#
# Cases of set 012:
# - blocks_engine() single-level (mode 0): 3 shapes x 3 seeds, one of them in
#   sampled mode (more than 2e5 plot pairs);
# - do_optim(): the two seeded rows of phase 1 Task 1 Step 7 (sparse, sampled
#   search; sparse, square-lattice start) and the p-rep row of the catalogue;
# - blocks_engine(resolvable = TRUE) (mode 1): 4 shapes x 3 seeds, including a
#   square-lattice start and r = 2, k = 2;
# - row_column(method = "twostage") (modes 1 and 2): 3 shapes x 3 seeds.
#
# Cases of set 34, on 24/6/2, 48/6/3, 100/5/4 and 9/3/2 (t/nrows/r), nested
# and shared rows and columns, seeds 1:3. Each case has two hashes: the whole
# output, and `<case>.design`, the treatments and efficiencies alone, so that
# a change of the reported work can be told from a change of the design.
# - mode4_rows: segmented_blocks() on the shared rows (blocks the nrows rows,
#   segments the replicates) from a random complete start;
# - mode4_columns: segmented_blocks() on the shared columns within the rows
#   (segments the replicate x row cells) from the mode4_rows design;
# - mode3_random: crossed_search() from a random complete start (singular on
#   some seeds of 9/3/2, which runs the repair);
# - mode3_staged: crossed_search() from a rows-first start built here from
#   the engine's own steps with their default arguments (nested:
#   blocks_engine(resolvable = TRUE), then rows_within_columns() on the
#   transposed grid; shared: the two segmented_blocks() calls above), so the
#   start does not depend on onestage_stage1().

arguments <- commandArgs(trailingOnly = TRUE)
usage <- "Usage: Rscript tools/check-engine-mode-identity.R <record|compare> <hashes.rds> [library] [set=012|34]"
set_argument <- grep("^set=", arguments, value = TRUE)
arguments <- arguments[!grepl("^set=", arguments)]
if (length(arguments) < 2L || length(arguments) > 3L || !arguments[1] %in% c("record", "compare") ||
    length(set_argument) > 1L || !all(set_argument %in% c("set=012", "set=34")) ||
    (length(set_argument) == 1L && arguments[1] == "compare")) {
  message(usage)
  quit(status = 1)
}
action <- arguments[1]
path <- path.expand(arguments[2])
library_path <- if (length(arguments) == 3L) arguments[3] else "~/fieldhub-libs/dev"
# `compare` takes the set from the file; a file without one holds set 012.
case_set <- if (action == "record") {
  if (length(set_argument)) sub("^set=", "", set_argument) else "012"
} else {
  recorded_set <- readRDS(path)$set
  if (is.null(recorded_set)) "012" else recorded_set
}
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
if (case_set == "012") {
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
}

if (case_set == "34") {
  ns <- asNamespace("FielDHub")
  segmented <- get("segmented_blocks", envir = ns)
  crossed <- get("crossed_search", envir = ns)
  layout <- function(v, nrows, r) {
    ncols <- v %/% nrows
    list(rep = rep(seq_len(r), each = v), row = rep(rep(seq_len(nrows), each = ncols), r),
         col = rep(seq_len(ncols), nrows * r), ncols = ncols)
  }
  random_start <- function(v, r) as.vector(vapply(seq_len(r), function(m) sample.int(v), integer(v)))
  shared_rows <- function(v, nrows, r) {
    lay <- layout(v, nrows, r)
    segmented(random_start(v, r), lay$row, lay$rep, v, r, nrows)
  }
  shared_columns <- function(v, nrows, r) {
    lay <- layout(v, nrows, r)
    rows <- shared_rows(v, nrows, r)
    segmented(rows$treatments, lay$col, (lay$rep - 1L) * nrows + lay$row, v, r, lay$ncols)
  }
  staged_start <- function(v, nrows, r, shared) {
    if (shared) return(as.integer(shared_columns(v, nrows, r)$treatments))
    lay <- layout(v, nrows, r)
    rows <- FielDHub:::blocks_engine(v, r, nrows, resolvable = TRUE)
    treat <- as.integer(rows$Design$treatments)
    transposed <- order(lay$rep, lay$col, lay$row)
    cols <- FielDHub:::rows_within_columns(treat[transposed], lay$row[transposed], v, r, lay$ncols,
                                           effort = 1)
    treat[transposed] <- cols$treatments
    treat
  }
  design_of <- function(x) {
    keep <- intersect(c("treatments", "d_efficiency", "a_efficiency", "full_rank", "row_d", "row_a",
                        "joint_d", "joint_a", "joint_log_det", "stage1_row_d", "stage1_row_a"), names(x))
    x[keep]
  }
  add2 <- function(name, run) {
    # One run, two hashes: the whole output and the design alone.
    value <- NULL
    add(name, function() { value <<- run(); value })
    add(paste0(name, ".design"), function() design_of(value))
  }
  shapes <- list(c(24, 6, 2), c(48, 6, 3), c(100, 5, 4), c(9, 3, 2))
  for (s in shapes) {
    for (seed in seeds) {
      local({
        v <- s[1]; nrows <- s[2]; r <- s[3]; sd <- seed
        tag <- sprintf("t%d_nrows%d_r%d_seed%d", v, nrows, r, sd)
        add2(paste0("mode4_rows_", tag), function() { set.seed(sd); quiet(shared_rows(v, nrows, r)) })
        add2(paste0("mode4_columns_", tag), function() { set.seed(sd); quiet(shared_columns(v, nrows, r)) })
        for (shared in c(FALSE, TRUE)) {
          local({
            sh <- shared
            coding <- if (sh) "shared" else "nested"
            add2(sprintf("mode3_random_%s_%s", coding, tag), function() {
              set.seed(sd)
              quiet(crossed(random_start(v, r), v, r, nrows, v %/% nrows, sh, work_cap = 2e11))
            })
            add2(sprintf("mode3_staged_%s_%s", coding, tag), function() {
              set.seed(sd)
              quiet(crossed(staged_start(v, nrows, r, sh), v, r, nrows, v %/% nrows, sh, work_cap = 2e11))
            })
          })
        }
      })
    }
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
  saveRDS(list(hashes = hashes, set = case_set, library = find.package("FielDHub"),
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
