# Work and time of the onestage build, for the weight and the limits of the
# work cap (phase 2 spec 6.1 W1, W3, W7; Task 8 Step 5):
#
#   Rscript tools/measure-onestage-work.R <out.csv> <library> [grid=os|universe]
#           [what=stages|startup|rate|build|call] [seeds=2] [tmin=0] [tmax=200]
#           [shapes=t/nrows/r,...] [latinize=both|FALSE|TRUE] [shard=i/k]
#
# One row per shape (t, nrows, r), coding (latinize FALSE / TRUE) and seed is
# appended as it finishes (rerunning with the same path resumes). Every row
# has
#   what,t,nrows,ncols,r,latinize,seed,blocks,status,message
# and the columns of its `what` (the others are NA):
#
# what=stages: search 1 of a build, call by call, with the build's limits.
#   row_status,col_status,row_work,col_work,row_seconds,col_seconds: the two
#     steps of onestage_stage1() (weighted work; engine seconds);
#   stage1_full_rank: whether the stage 1 design is full rank;
#   joint_work,joint_seconds,joint_stop,joint_capped,evaluations,full_rank:
#     one crossed_search() from it with the work left as its room;
#   repair,repair_work,repair_evaluations: the repair's status and, when
#     there was one, the work of the same call with budget = 0 less its start
#     and its charged finish.
# what=startup: each engine call (row step, column step, joint call) given
#   exactly the start plus the finish of onestage_call_work() as its engine
#   limit; the joint call with a budget of one evaluation.
#   row_work,col_work,joint_work: the engine's count (a step's plus its
#     reserved finish); row_seconds,col_seconds,joint_seconds: the call;
#   start_seconds,finish_seconds: of the joint call (mode 3 reports them);
#   row_status,col_status: "scored" or "none"; full_rank: the joint result.
# what=rate: seconds to spend onestage_work_limit with the evaluation budget
#   lifted, per call: row_work,row_seconds,row_status (the stop reason),
#   col_*, joint_work,joint_seconds,joint_stop. The work is the engine's
#   count (unweighted; a step's plus its reserved finish). Seconds per unit
#   against `blocks` (the joint block columns; row_blocks and col_blocks for
#   the steps) give the weight.
# what=build: one build_row_column_onestage() at searches = 200.
#   build_seconds (the whole call), engine_seconds (the sum over its engine
#   calls), span_seconds (first engine call to last), engine_calls, and
#   requested,started,complete,row_status,col_status,joint_stop (the
#   joint stage of search 1),repair,work of its "onestage_search" attribute.
# what=call: one row_column(iterations = 200) call.
#   call_seconds, engine_seconds, span_seconds, joint_efficiency_seconds (the
#   sum over the calls of onestage_joint_efficiency()), report_seconds
#   (report_efficiency(): the Rep, Row and Column reports), concurrence_seconds
#   (the three concurrence tables), optimization, and requested,complete of
#   infoDesign$joint_searches.
#
# Seconds are wall time on this machine: use an idle reference machine, one
# run at a time. Work does not depend on the machine.
#
# grid=os: the shapes of nested_golden_grid("os") (deduplicated over
# iterations). grid=universe: every (t, nrows, r) with t in {20, 30, 50, 100,
# 200, 300, 500, 750, 1000, 1500, 2000}, thin(divisors(t, 2)) nrows and
# r in 2:4, plus every os shape. tmin / tmax select t. shapes= names the
# shapes instead (grid, tmin and tmax are then ignored). Shapes that fail
# onestage_feasible() are skipped. Install with `R CMD INSTALL . --preclean`
# first; it stops if FielDHub is loaded from a source tree.
arguments <- commandArgs(trailingOnly = TRUE)
option <- function(name, default) {
  hit <- grep(paste0("^", name, "="), arguments, value = TRUE)
  if (length(hit) == 0L) default else sub(paste0("^", name, "="), "", hit[1])
}
positional <- arguments[!grepl("=", arguments, fixed = TRUE)]
if (length(positional) != 2L) {
  stop("Usage: Rscript tools/measure-onestage-work.R <out.csv> <library> [grid=os|universe] ",
       "[what=stages|startup|rate|build|call] [seeds=2] [tmin=0] [tmax=200] ",
       "[shapes=t/nrows/r,...] [latinize=both|FALSE|TRUE] [shard=i/k]")
}
path <- positional[1]
.libPaths(c(normalizePath(positional[2], mustWork = TRUE), .libPaths()))
grid_name <- option("grid", "os")
what <- option("what", "stages")
if (!what %in% c("stages", "startup", "rate", "build", "call")) stop("what= must be stages, startup, rate, build or call.")
seeds <- as.integer(option("seeds", "2"))
tmin <- as.numeric(option("tmin", "0"))
tmax <- as.numeric(option("tmax", "200"))
shape_list <- option("shapes", "")
codings <- switch(option("latinize", "both"), both = c(FALSE, TRUE), "FALSE" = FALSE, "TRUE" = TRUE,
                  stop("latinize= must be both, FALSE or TRUE."))
shard <- option("shard", "1/1")
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
suppressMessages(library(FielDHub))
if (dir.exists(file.path(find.package("FielDHub"), "src"))) {
  stop("FielDHub is loaded from a source tree; measure an installed build instead.")
}
source(file.path(script_dir, "..", "tests", "testthat", "helper-nested-goldens.R"))
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
Sys.setenv(FIELDHUB_ENGINE_DEBUG = "seconds")
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

shapes <- if (nzchar(shape_list)) {
  parts <- lapply(strsplit(strsplit(shape_list, ",", fixed = TRUE)[[1]], "/", fixed = TRUE), as.integer)
  if (any(lengths(parts) != 3L) || anyNA(unlist(parts))) stop("shapes= must be t/nrows/r,...")
  data.frame(t = vapply(parts, `[`, integer(1), 1L), k = vapply(parts, `[`, integer(1), 2L),
             r = vapply(parts, `[`, integer(1), 3L))
} else {
  x <- if (grid_name == "universe") onestage_universe() else {
    os <- nested_golden_grid("os")[c("t", "k", "r")]
    os[!duplicated(os), ]
  }
  x[x$t >= tmin & x$t <= tmax, , drop = FALSE]
}
runs <- merge(shapes, data.frame(latinize = codings))
runs <- runs[order(runs$t * runs$r, runs$k, runs$latinize), , drop = FALSE]
ik <- as.integer(strsplit(shard, "/", fixed = TRUE)[[1]])
if (length(ik) != 2L || anyNA(ik) || ik[1] < 1L || ik[1] > ik[2]) stop("shard= must be i/k.")
runs <- runs[(seq_len(nrow(runs)) - 1L) %% ik[2] + 1L == ik[1], , drop = FALSE]

# Every engine call and every joint efficiency is timed through a wrapper.
log <- new.env()
log$calls <- list()
log$efficiency <- 0
wrap <- function(name, record) {
  real <- get(name, envir = ns)
  unlockBinding(name, ns)
  assign(name, function(...) {
    began <- proc.time()[["elapsed"]]
    out <- real(...)
    record(began, proc.time()[["elapsed"]], out, ...)
    out
  }, envir = ns)
  lockBinding(name, ns)
}
wrap("blocks_engine_cpp", function(began, ended, out, ...) {
  log$calls[[length(log$calls) + 1L]] <- c(mode = list(...)[[14L]], began = began, ended = ended, work = out$work)
})
wrap("onestage_joint_efficiency", function(began, ended, out, ...) log$efficiency <- log$efficiency + ended - began)
wrap("report_efficiency", function(began, ended, out, ...) log$report <- log$report + ended - began)
wrap("concurrence_matrix", function(began, ended, out, ...) log$concurrence <- log$concurrence + ended - began)
reset <- function() {
  log$calls <- list()
  log$efficiency <- 0
  log$report <- 0
  log$concurrence <- 0
}
engine <- function() {
  calls <- do.call(rbind, log$calls)
  if (is.null(calls)) return(c(seconds = 0, span = 0, calls = 0))
  c(seconds = sum(calls[, "ended"] - calls[, "began"]), span = max(calls[, "ended"]) - min(calls[, "began"]),
    calls = nrow(calls))
}
mode_seconds <- function(modes) {
  calls <- do.call(rbind, log$calls)
  if (is.null(calls)) return(NA_real_)
  keep <- calls[, "mode"] %in% modes
  if (!any(keep)) NA_real_ else sum(calls[keep, "ended"] - calls[keep, "began"])
}

columns <- c("what", "t", "nrows", "ncols", "r", "latinize", "seed", "blocks", "row_blocks", "col_blocks",
             "row_status", "col_status", "row_work", "col_work", "row_seconds", "col_seconds",
             "stage1_full_rank", "joint_work", "joint_seconds", "joint_stop", "joint_capped", "evaluations",
             "full_rank", "repair", "repair_work", "repair_evaluations", "start_seconds", "finish_seconds",
             "build_seconds", "call_seconds", "engine_seconds", "span_seconds", "engine_calls",
             "joint_efficiency_seconds", "report_seconds", "concurrence_seconds", "requested", "started",
             "complete", "work", "optimization", "status", "message")
measure <- function(t, nrows, r, latinize, seed) {
  ncols <- t %/% nrows
  B_row <- if (latinize) nrows else r * nrows
  B_col <- if (latinize) ncols else r * ncols
  B <- B_row + B_col
  row <- as.list(stats::setNames(rep(NA, length(columns)), columns))
  row[c("what", "t", "nrows", "ncols", "r", "latinize", "seed", "blocks", "row_blocks", "col_blocks",
        "status", "message")] <- list(what, t, nrows, ncols, r, latinize, seed, B, B_row, B_col, "ok", "")
  limits <- ns$onestage_limits()
  modes <- c(row = if (latinize) 4L else 1L, column = if (latinize) 4L else 2L)
  rep_of <- rep(seq_len(r), each = t)
  row_in <- rep(rep(seq_len(nrows), each = ncols), r)
  col_in <- rep(seq_len(ncols), nrows * r)
  transposed <- order(rep_of, col_in, row_in)
  full_rank <- function(trt) !is.na(ns$onestage_joint_efficiency(
    ns$onestage_design_frame(as.integer(trt), t, nrows, ncols, r, latinize))$Aeffic)
  # One step of stage 1 with the engine limit `limit` and the given effort.
  step <- function(stage, start, limit, lifted = FALSE) {
    big <- .Machine$integer.max
    if (latinize) {
      out <- if (stage == "row") {
        ns$segmented_blocks(start, row_in, rep_of, t, r, nrows, searches = if (lifted) 1e6 else 20L, work_cap = limit)
      } else {
        ns$segmented_blocks(start, col_in, (rep_of - 1L) * nrows + row_in, t, r, ncols,
                            searches = if (lifted) 1e6 else 20L, work_cap = limit)
      }
      list(treatments = as.integer(out$treatments), scored = !is.na(out$a_efficiency), search = out$search)
    } else if (stage == "row") {
      out <- if (lifted) {
        ns$blocks_engine(t, r, nrows, resolvable = TRUE, work_cap = limit, admission = FALSE,
                         searches = 1e6, budget = big)
      } else ns$blocks_engine(t, r, nrows, resolvable = TRUE, work_cap = limit, admission = FALSE)
      list(treatments = as.integer(out$Design$treatments), scored = TRUE, search = out$search)
    } else {
      out <- ns$rows_within_columns(start[transposed], row_in[transposed], t, r, ncols,
                                    effort = if (lifted) big else 1, work_cap = limit, admission = FALSE)
      back <- start
      back[transposed] <- out$treatments
      list(treatments = as.integer(back), scored = !is.na(out$a_efficiency), search = out$search)
    }
  }
  quiet <- function(expr) suppressWarnings(tryCatch(expr, fieldhub_error = function(e) NULL))
  tryCatch({
    set.seed(seed)
    reset()
    if (what == "stages") {
      s1 <- ns$onestage_stage1(t, nrows, ncols, r, latinize, limits$work, TRUE, limits)
      row$row_status <- s1$steps[["row"]]
      row$col_status <- s1$steps[["column"]]
      row$row_work <- s1$step_work[["row"]]
      row$col_work <- s1$step_work[["column"]]
      row$row_seconds <- mode_seconds(modes[["row"]])
      row$col_seconds <- if (latinize) NA_real_ else mode_seconds(2L)
      if (latinize) {
        # Mode 4 runs both steps: the first call is the row step.
        calls <- do.call(rbind, log$calls)
        if (!is.null(calls)) {
          row$row_seconds <- calls[1L, "ended"] - calls[1L, "began"]
          row$col_seconds <- if (nrow(calls) > 1L) calls[2L, "ended"] - calls[2L, "began"] else NA_real_
        }
      }
      row$stage1_full_rank <- full_rank(s1$treatments)
      state <- get(".Random.seed", envir = globalenv())
      reset()
      joint_limit <- ns$onestage_engine_limit(3L, t, r, B, limits$work - s1$work, B_row)
      js <- ns$crossed_search(s1$treatments, t, r, nrows, ncols, shared = latinize, work_cap = joint_limit)
      row$joint_work <- ns$onestage_weighted_work(3L, t, r, B, js$search$work, B_row)
      row$joint_seconds <- engine()[["seconds"]]
      row$joint_stop <- js$search$stop_reason
      row$joint_capped <- js$search$work_capped
      row$evaluations <- js$search$evaluations
      row$full_rank <- js$full_rank
      row$repair <- js$search$repair
      row$repair_evaluations <- js$search$repair_evaluations
      if (js$search$repair != "none") {
        assign(".Random.seed", state, envir = globalenv())
        alone <- ns$crossed_search(s1$treatments, t, r, nrows, ncols, shared = latinize, budget = 0,
                                   work_cap = joint_limit)
        row$repair_work <- ns$onestage_work_weight(3L, t, B)[["search"]] *
          (alone$search$work - alone$search$start_work - alone$search$finish_charged)
      }
    } else if (what == "startup" || what == "rate") {
      start <- as.vector(vapply(seq_len(r), function(m) sample.int(t), integer(t)))
      for (stage in c("row", "column")) {
        mode <- modes[[stage]]
        blocks <- if (stage == "row") B_row else B_col
        work <- ns$onestage_call_work(mode, t, r, blocks)
        limit <- if (what == "startup") sum(work) - work[["finish"]] else {
          ns$onestage_engine_limit(mode, t, r, blocks, limits$work)
        }
        reset()
        out <- quiet(step(stage, start, limit, lifted = what == "rate"))
        short <- if (stage == "row") "row" else "col"
        row[[paste0(short, "_seconds")]] <- engine()[["seconds"]]
        ran <- !is.null(out) && !is.null(out$search$work)
        row[[paste0(short, "_work")]] <- if (ran) out$search$work + work[["finish"]] else NA_real_
        row[[paste0(short, "_status")]] <- if (is.null(out) || !out$scored) "none" else if (what == "rate") {
          out$search$stop_reason
        } else "scored"
        if (!is.null(out) && out$scored) start <- out$treatments
      }
      work <- ns$onestage_call_work(3L, t, r, B, B_row)
      reset()
      js <- if (what == "startup") {
        ns$crossed_search(start, t, r, nrows, ncols, shared = latinize, budget = 1, work_cap = sum(work))
      } else {
        ns$crossed_search(start, t, r, nrows, ncols, shared = latinize, budget = .Machine$integer.max,
                          work_cap = ns$onestage_engine_limit(3L, t, r, B, limits$work, B_row),
                          max_stale_escapes = .Machine$integer.max)
      }
      row$joint_work <- js$search$work
      row$joint_seconds <- engine()[["seconds"]]
      row$joint_stop <- js$search$stop_reason
      row$joint_capped <- js$search$work_capped
      row$evaluations <- js$search$evaluations
      row$full_rank <- js$full_rank
      row$repair <- js$search$repair
      row$start_seconds <- js$search$seconds[["start"]]
      row$finish_seconds <- js$search$seconds[["finish"]]
    } else if (what == "build") {
      row$build_seconds <- system.time(
        fit <- suppressWarnings(ns$build_row_column_onestage(t, nrows, ncols, r, latinize, 200L, seed))
      )[["elapsed"]]
      e <- engine()
      row$engine_seconds <- e[["seconds"]]
      row$span_seconds <- e[["span"]]
      row$engine_calls <- e[["calls"]]
      row$joint_efficiency_seconds <- log$efficiency
      s <- attr(fit, "onestage_search")
      row[c("requested", "started", "complete", "row_status", "col_status", "joint_stop", "repair", "work")] <-
        s[c("requested", "started", "complete", "row_step", "column_step", "joint_stage", "repair", "work")]
    } else {
      row$call_seconds <- system.time(
        design <- suppressWarnings(row_column(t = t, nrows = nrows, reps = r, latinize = latinize,
                                              seed = seed, iterations = 200))
      )[["elapsed"]]
      e <- engine()
      row$engine_seconds <- e[["seconds"]]
      row$span_seconds <- e[["span"]]
      row$engine_calls <- e[["calls"]]
      row$joint_efficiency_seconds <- log$efficiency
      row$report_seconds <- log$report
      row$concurrence_seconds <- log$concurrence
      row$optimization <- design$infoDesign$optimization
      row$requested <- design$infoDesign$joint_searches$requested
      row$complete <- design$infoDesign$joint_searches$completed
    }
    row
  }, error = function(e) {
    row$status <- "error"
    row$message <- gsub("[\r\n,]+", " ", conditionMessage(e))
    row
  })
}

done <- if (file.exists(path)) utils::read.csv(path) else NULL
for (i in seq_len(nrow(runs))) {
  g <- runs[i, ]
  if (!ns$onestage_feasible(g$t, g$r, g$k, g$t %/% g$k, g$latinize)) next
  for (seed in seq_len(seeds)) {
    if (!is.null(done) && any(done$what == what & done$t == g$t & done$nrows == g$k & done$r == g$r &
                              done$latinize == g$latinize & done$seed == seed)) next
    row <- as.data.frame(lapply(measure(g$t, g$k, g$r, g$latinize, seed), function(x) if (is.null(x)) NA else x),
                         stringsAsFactors = FALSE)
    utils::write.table(row[columns], path, sep = ",", row.names = FALSE,
                       col.names = !file.exists(path), append = file.exists(path))
  }
}
cat(sprintf("%s: %d rows\n", path, if (file.exists(path)) nrow(utils::read.csv(path)) else 0L))
