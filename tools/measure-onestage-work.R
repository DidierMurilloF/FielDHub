# Work of the onestage build's stages, for the admission fit (phase 2 spec
# 6.1 "Work cap and admission", Task 8 Step 5):
#
#   Rscript tools/measure-onestage-work.R <out.csv> <library> [grid=os|universe]
#           [seeds=2] [tmin=0] [tmax=200] [shard=i/k]
#
# Per shape (t, nrows, r), coding (latinize FALSE / TRUE) and seed, in one
# process: set.seed(seed), then onestage_stage1() and one crossed_search()
# from its design, both with work_cap = Inf, so neither stops on the cap. One
# row per run is appended as it finishes (rerunning with the same path
# resumes):
#   t,nrows,ncols,r,latinize,seed,row_work,col_work,stage1_work,joint_work,
#   total_work,repair_evaluations,evaluations,budget,stop_reason,full_rank,
#   stage1_seconds,joint_seconds,status,message
# total_work = stage1_work + joint_work is what one search of a build costs;
# the joint work includes the repair (charged to the work cap only). Seconds
# are recorded but the fit uses work: a loaded machine inflates time, not work.
#
# grid=os: the shapes of nested_golden_grid("os") (deduplicated over
# iterations). grid=universe: every (t, nrows, r) with t in {20, 30, 50, 100,
# 200, 300, 500, 750, 1000, 1500, 2000}, thin(divisors(t, 2)) nrows and
# r in 2:4, plus every os shape. tmin / tmax select t. Shapes whose stage 1
# step limits refuse even an infinite cap (more than 2000 blocks or 1e8
# cells) are recorded with status "refused". Install with
# `R CMD INSTALL . --preclean` first; it stops if FielDHub is loaded from a
# source tree.
arguments <- commandArgs(trailingOnly = TRUE)
option <- function(name, default) {
  hit <- grep(paste0("^", name, "="), arguments, value = TRUE)
  if (length(hit) == 0L) default else sub(paste0("^", name, "="), "", hit[1])
}
positional <- arguments[!grepl("=", arguments, fixed = TRUE)]
if (length(positional) != 2L) {
  stop("Usage: Rscript tools/measure-onestage-work.R <out.csv> <library> [grid=os|universe] ",
       "[seeds=2] [tmin=0] [tmax=200] [shard=i/k]")
}
path <- positional[1]
.libPaths(c(normalizePath(positional[2], mustWork = TRUE), .libPaths()))
grid_name <- option("grid", "os")
seeds <- as.integer(option("seeds", "2"))
tmin <- as.numeric(option("tmin", "0"))
tmax <- as.numeric(option("tmax", "200"))
shard <- option("shard", "1/1")
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
suppressMessages(library(FielDHub))
if (dir.exists(file.path(find.package("FielDHub"), "src"))) {
  stop("FielDHub is loaded from a source tree; measure an installed build instead.")
}
source(file.path(script_dir, "..", "tests", "testthat", "helper-nested-goldens.R"))
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
ns <- asNamespace("FielDHub")

onestage_universe <- function() {
  thin <- function(x) if (length(x) <= 4L) x else x[unique(round(seq(1, length(x), length.out = 4)))]
  divisors <- function(t, from) {
    d <- seq.int(from, t %/% 2L)
    d[t %% d == 0L]
  }
  shapes <- do.call(rbind, lapply(c(20, 30, 50, 100, 200, 300, 500, 750, 1000, 1500, 2000),
                                  function(t) expand.grid(t = t, k = thin(divisors(t, 2L)), r = 2:4)))
  os <- nested_golden_grid("os")[c("t", "k", "r")]
  shapes <- rbind(shapes, os)
  shapes$t <- as.integer(shapes$t)
  shapes$k <- as.integer(shapes$k)
  shapes$r <- as.integer(shapes$r)
  shapes[!duplicated(shapes), ]
}

shapes <- if (grid_name == "universe") onestage_universe() else {
  os <- nested_golden_grid("os")[c("t", "k", "r")]
  os[!duplicated(os), ]
}
shapes <- shapes[shapes$t >= tmin & shapes$t <= tmax, , drop = FALSE]
runs <- merge(shapes, data.frame(latinize = c(FALSE, TRUE)))
runs <- runs[order(runs$t * runs$r, runs$k, runs$latinize), , drop = FALSE]
ik <- as.integer(strsplit(shard, "/", fixed = TRUE)[[1]])
if (length(ik) != 2L || anyNA(ik) || ik[1] < 1L || ik[1] > ik[2]) stop("shard= must be i/k.")
runs <- runs[(seq_len(nrow(runs)) - 1L) %% ik[2] + 1L == ik[1], , drop = FALSE]

done <- if (file.exists(path)) utils::read.csv(path) else NULL
measure <- function(t, nrows, r, latinize, seed) {
  ncols <- t %/% nrows
  row <- data.frame(t = t, nrows = nrows, ncols = ncols, r = r, latinize = latinize, seed = seed,
                    row_work = NA_real_, col_work = NA_real_, stage1_work = NA_real_,
                    joint_work = NA_real_, total_work = NA_real_, repair_evaluations = NA_real_,
                    evaluations = NA_real_, budget = NA_real_, stop_reason = "", full_rank = NA,
                    stage1_seconds = NA_real_, joint_seconds = NA_real_, status = "ok", message = "")
  if (!latinize && (!ns$onestage_row_step_admitted(t, r, nrows, Inf) ||
                    r * ncols > ns$resolvable_block_limit || t * r * ncols > 1e8)) {
    row$status <- "refused"
    return(row)
  }
  budget <- ns$crossed_default_budget(t, r)
  result <- tryCatch({
    set.seed(seed)
    s1_time <- system.time(s1 <- ns$onestage_stage1(t, nrows, ncols, r, latinize, work_cap = Inf))
    js_time <- system.time(js <- ns$crossed_search(s1$treatments, t, r, nrows, ncols, shared = latinize,
                                                   budget = budget, work_cap = Inf))
    row$row_work <- s1$step_work[["rows"]]
    row$col_work <- s1$step_work[["columns"]]
    row$stage1_work <- s1$work
    row$joint_work <- js$search$work
    row$total_work <- s1$work + js$search$work
    row$repair_evaluations <- js$search$repair_evaluations
    row$evaluations <- js$search$evaluations
    row$budget <- budget
    row$stop_reason <- js$search$stop_reason
    row$full_rank <- js$full_rank
    row$stage1_seconds <- s1_time[["elapsed"]]
    row$joint_seconds <- js_time[["elapsed"]]
    row
  }, error = function(e) {
    row$status <- "error"
    row$message <- gsub("[\r\n,]+", " ", conditionMessage(e))
    row
  })
  result
}

for (i in seq_len(nrow(runs))) {
  g <- runs[i, ]
  for (seed in seq_len(seeds)) {
    if (!is.null(done) && any(done$t == g$t & done$nrows == g$k & done$r == g$r &
                              done$latinize == g$latinize & done$seed == seed)) next
    row <- measure(g$t, g$k, g$r, g$latinize, seed)
    utils::write.table(row, path, sep = ",", row.names = FALSE,
                       col.names = !file.exists(path), append = file.exists(path))
  }
}
cat(sprintf("%s: %d rows\n", path, if (file.exists(path)) nrow(utils::read.csv(path)) else 0L))
