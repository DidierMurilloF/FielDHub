# Pre-change goldens for the pair-swap optimizer (spec-m15 section 4.4): full
# results of swap_pairs(), partially_replicated(), optimized_arrangement() and
# multi_location_prep() from an untouched source tree, replayed with identical().
#   Rscript --vanilla tools/generate-swap-goldens.R <source-dir> <out.rds> <commit>
args <- commandArgs(TRUE)
if (length(args) != 3L) stop("Usage: generate-swap-goldens.R <source-dir> <out.rds> <commit>")
suppressMessages(pkgload::load_all(args[1], quiet = TRUE, export_all = FALSE))
default_kind <- c("Mersenne-Twister", "Inversion", "Rejection")
make_input <- function(nr, nc, frac_rep, seed, holes = 0) {
  set.seed(seed)
  plots <- nr * nc
  rep <- as.integer(plots * frac_rep / 2)
  entries <- c(rep(seq_len(rep), each = 2L), seq.int(rep + 1L, plots - rep))
  X <- matrix(sample(entries), nrow = nr)
  if (holes > 0) X[sample(length(X), holes)] <- NA
  X
}
# m entries with k copies each; the remaining active cells hold single entries
make_multi <- function(nr, nc, k, m, seed, holes = 0, m2 = 0) {
  set.seed(seed)
  active <- nr * nc - holes
  entries <- c(rep(seq_len(m), each = k), rep(m + seq_len(m2), each = 2L))
  entries <- c(entries, seq.int(m + m2 + 1L, length.out = active - length(entries)))
  X <- matrix(NA_real_, nr, nc)
  cells <- if (holes > 0) sort(sample(nr * nc, active)) else seq_len(nr * nc)
  X[cells] <- sample(entries)
  X
}
cases <- list()
add <- function(fun, args, rng_kind = default_kind) {
  cases[[length(cases) + 1L]] <<- list(fun = fun, args = args, rng_kind = rng_kind)
}
# The 79 cases captured on 2026-11-05 (research m15-goldens), now with an explicit year
for (sz in list(c(8, 10), c(12, 20), c(20, 30))) for (dm in c("euclidean", "manhattan"))
  for (lam in c(0, 0.5, 2)) for (seed in 1:2)
    add("swap_pairs", list(X = make_input(sz[1], sz[2], 0.3, seed), starting_dist = 3, stop_iter = 10,
                           lambda = lam, dist_method = dm, candidate_sample_size = 4, seed = 100 + seed))
for (css in c(1, 8)) add("swap_pairs", list(X = make_input(12, 20, 0.3, 7), lambda = 0.5,
                                            candidate_sample_size = css, seed = 77))
for (seed in 1:2) add("swap_pairs", list(X = make_input(12, 20, 0.3, seed, holes = 15), lambda = 0.5,
                                         seed = 200 + seed))
add("swap_pairs", list(X = make_input(10, 10, 0.6, 3), starting_dist = 2, stop_iter = 5, seed = 9))
for (cf in list(c(10, 20, 40, 120), c(16, 25, 80, 240), c(30, 60, 360, 1080)))
  for (dm in c("euclidean", "manhattan")) for (bp in c(0.5, 0, 2)) for (seed in 1:2)
    add("partially_replicated", list(nrows = cf[1], ncols = cf[2], repGens = cf[3:4], repUnits = c(2, 1),
                                     dist_method = dm, border_penalization = bp, seed = seed, year = 2026))
add("partially_replicated", list(nrows = 10, ncols = 20, repGens = c(40, 120), repUnits = c(2, 1), l = 3,
                                 seed = 5, year = 2026))
add("partially_replicated", list(nrows = 10, ncols = 20, repGens = c(40, 120), repUnits = c(2, 1),
                                 spread_reps = FALSE, seed = 6, year = 2026))
# New: three or more copies (the starting_dist = 2 branch of pREP() and combn() with k >= 3)
for (k in c(3, 5)) for (dm in c("euclidean", "manhattan")) for (holes in c(0, 12))
  add("swap_pairs", list(X = make_multi(if (k == 3) 12 else 16, if (k == 3) 20 else 25, k,
                                        if (k == 3) 20 else 12, seed = k, holes = holes),
                         starting_dist = 2, dist_method = dm, seed = 300 + k))
add("swap_pairs", list(X = make_multi(12, 20, 4, 6, seed = 8, m2 = 30), starting_dist = 2, seed = 308))
# New: p-rep with checks of 4-8 copies, fillers and the data= path
for (dm in c("euclidean", "manhattan")) {
  add("partially_replicated", list(nrows = 12, ncols = 20, repGens = c(6, 40, 120), repUnits = c(6, 2, 1),
                                   allow_fillers = TRUE, dist_method = dm, seed = 11, year = 2026))
  add("partially_replicated", list(nrows = 10, ncols = 25, repGens = c(4, 4, 50, 102),
                                   repUnits = c(8, 4, 2, 1), dist_method = dm, seed = 12, year = 2026))
}
prep_data <- data.frame(ENTRY = 1:100, NAME = paste0("G", 1:100), REPS = rep(c(2, 1), c(30, 70)))
add("partially_replicated", list(nrows = 10, ncols = 13, data = prep_data, seed = 13, year = 2026))
add("partially_replicated", list(nrows = 12, ncols = 12, data = prep_data, allow_fillers = TRUE,
                                 seed = 14, year = 2026))
# New: optimized_arrangement() shares pREP() with checks only (k >= 3)
for (seed in 1:2) add("optimized_arrangement", list(nrows = 10, ncols = 20, lines = 180, checks = 4,
                                                   rep_checks = c(5, 5, 5, 5), seed = seed, year = 2026))
# New: multi-location p-rep, and one case under sample.kind = "Rounding"
add("multi_location_prep", list(lines = 150, l = 3, copies_per_entry = 4, seed = 2, year = 2026))
add("swap_pairs", list(X = make_input(12, 20, 0.3, 21), seed = 21),
    rng_kind = c("Mersenne-Twister", "Inversion", "Rounding"))
results <- lapply(seq_along(cases), function(i) {
  cs <- cases[[i]]
  suppressWarnings(do.call(RNGkind, as.list(cs$rng_kind)))
  t <- system.time(value <- suppressWarnings(do.call(cs$fun, cs$args)))[["elapsed"]]
  cat(sprintf("%3d/%d %s %.2fs\n", i, length(cases), cs$fun, t))
  list(label = sprintf("%03d %s", i, cs$fun), fun = cs$fun, args = cs$args, rng_kind = cs$rng_kind,
       value = value, seconds = t)
})
saveRDS(list(commit = args[3], platform = R.version$platform, r_version = R.version.string,
             created = format(Sys.time()), results = results), args[2], version = 2)
cat("saved", length(results), "golden cases\n")
