# Engine checks of the staged onestage row-column search (phase 2 spec 6.1).
# Engine correctness is verified here, not by testthat (constraints: Tests).
#
#   Rscript tools/check-onestage-engine.R <stage1|crossed|repair|treatment_form|report>
#           [baseline] [library] [seeds=20] [tmax=100]
#
# One PASS / FAIL line per check; exit status 1 on any FAIL. [library]
# defaults to ~/fieldhub-libs/dev; install first with
# `R CMD INSTALL . --preclean` (AGENTS.md). [baseline] is either a per-seed
# golden recording (.csv or .csv.gz, as tools/generate-nested-goldens.R
# writes it, e.g. tools/nested-goldens-os-baseline.csv.gz) or a summary
# data frame (.rds, as tests/testthat/fixtures/nested-goldens-ci.rds); pass
# "-" for none.
#
# stage1: onestage_stage1() for every `os` shape (nested_golden_grid("os"))
# with t <= tmax, both codings (latinize FALSE / TRUE), seeds 1:seeds
# (set.seed(seed) before each call). Checks, per shape and coding:
# - structure: one treatment per plot, every replicate holds each treatment
#   once, and stage 1 emits no warning;
# - the Row A floor of spec 6.1: Row A >= the baseline's constant Row A
#   - 1.5e-7 on every seed, wherever the baseline Row A has SD 0 among the
#   seeds where onestage ran (status "ok", at least two of them; if both
#   iteration counts qualify, the larger constant). Row A is the A-efficiency
#   of the rows alone: nested in the replicates (latinize = FALSE) or shared
#   across them (latinize = TRUE).
# - the four baseline constants that spec 6.1 measured (SD 0, 12 seeds) are
#   checked whether or not a baseline is given: 48/6/3 nested 0.8581470,
#   24/6/2 nested 0.6500942, 30/5/2 nested 0.7742182, 24/6/2 latinized
#   0.8697479.
# - a given baseline must be usable: it FAILs when it cannot be read, lacks
#   the needed columns, or holds no constant Row A shape; and every constant
#   shape (baseline and spec) with t <= tmax must have been checked.
# The stage 1 run time of each shape (all seeds) is printed; it is not a
# gate (timings are controller jobs).
#
# crossed: the joint stage, engine mode 3 (crossed_search(); phase 2 Task 6).
# [baseline], seeds= and tmax= are ignored. Checks:
# (a) values: 20 random complete designs (nested and shared, v <= 30, set.seed
#     fixed), crossed_search(budget = 1): `full_rank` agrees with the dense
#     reference on the returned design (smallest canonical efficiency of C/r
#     >= 1e-7, C from qr.resid() as in row_column_joint_efficiency(),
#     unrounded), and a full-rank start is never returned not full rank;
#     where full rank, `joint_log_det` (sum of log efficiencies) and `joint_d`
#     (their geometric mean) match it to 1e-9, and `row_d`, `row_a` (and the
#     stage 1 values of the start) match the one-way eigenvalue reference to
#     1e-9. This verifies the block-space identity and its full-rank test. A
#     singular start is repaired first (`repair`); a result that is still not
#     full rank has NA joint values and no evaluations.
# (b) invariants after full searches from onestage_stage1() starts (24/6/2,
#     30/5/2, 48/6/3 nested and 24/6/2, 30/5/2 latinized, seeds 1:8), run with
#     FIELDHUB_ENGINE_DEBUG=1: every replicate complete; the Row floors (see
#     below); joint log det >= the start's (reference) - 1e-9;
#     the final values match the reference to 1e-9; debug_max_error <= 1e-8
#     (the largest relative error of an accepted rank-2 ratio, joint or Row,
#     against fresh refreshes: this verifies the low-rank update). The same
#     invariants on 12/4/2 and 16/4/2 nested (stage 1 often singular or
#     nearly so) and on 60 random complete starts (v 8-16, many
#     near-singular or singular and then repaired: the ill-conditioned states
#     where a rank-2 score loses digits), at least 20 of them searched.
# (c) parity: single-search mean joint_d >= the spec 6.1 "Floor on Row only"
#     value - 0.001: 0.5929 (24/6/2 nested), 0.6349 (30/5/2 nested), 0.7173
#     (48/6/3 nested, seeds 1:4), 0.8466 (24/6/2 latinized).
# (d) the exact Row guarantee's last line of defence. With
#     FIELDHUB_ENGINE_DEBUG=slack_floors the engine skips the exact Row
#     confirmation of each swap (the search of the 1e-10 running floors
#     alone), so a final design can lie one rounding below stage 1 and the
#     end-of-search comparison must replace it. On 24/6/2, 48/6/3 and 16/8/4
#     nested, seeds 1:8 (24 runs): the Row floors hold on every seed, every
#     result is full rank and not below the start in joint log det, and at
#     least two seeds took the replacement (`floor_fallback` 1; 16/8/4 takes
#     it on several seeds, so the path does not hang on one rounding).
#     The repair's output gets the same comparison: on 8/4/3 nested seed 36
#     (a singular stage 1 start) the hook lets the repair reach a full-rank
#     design one rounding below the Row floors, which must be rejected:
#     `floor_fallback` 2, the start returned unchanged, `full_rank` FALSE with
#     NA joint values, no evaluations, and the Row floors exact.
#
# The Row floors, in `crossed` and `repair` alike (spec 6.1: Row D and Row A
# never fall below their stage 1 values):
# - exactly, on the engine's unrounded values: row_d >= stage1_row_d and
#   row_a >= stage1_row_a with no tolerance, and `floor_fallback` 0 (the
#   engine confirms each swap between rows from scratch, so the end
#   comparison never has to replace a result);
# - on the one-way eigenvalue reference of the final design against that of
#   the start, to a relative 1e-12 (the rounding of two eigen-decompositions;
#   measured differences are below 1e-14).
#
# repair: the joint-rank repair of mode 3 (spec 6.1 stage 2; phase 2 Task 7).
# [baseline], seeds= and tmax= are ignored. Every shape with r = 2-4, 2-4
# rows and 2-4 columns (v <= 16) where the rule of spec 5.1b holds: 22 nested
# and 27 shared, seeds 1:8 each (392 runs), from onestage_stage1() starts.
# Each run makes three calls from one generator state: A, the search; B, the
# same call with budget = 0 (the repair alone: a budget below one evaluation
# runs no joint stage and draws nothing); C, the search from B's design,
# continuing B's generator stream (the joint stage with the repair's work
# excluded). Checks, per shape and coding:
# - `full_rank` on every run, the dense reference agrees, and
#   row_column_joint_efficiency() of the final design has a non-NA Aeffic;
# - every replicate complete, and the Row floors (above);
# - budget: A's `evaluations` equal C's and A's design is C's; B has 0
#   evaluations and the same `repair_evaluations` and `repair_swaps` as A; a
#   start that is already full rank has `repair_evaluations` 0, a singular
#   one more than 0; C (a full-rank start) has 0;
# - criterion: the engine's block-space value before and after the repair,
#   log det((1 + eps) I_B - N'N / r + y y' / n), equals the dense
#   sum_i log(e_i + eps) + (B - v + 1) log(1 + eps), eps = 1e-6, of the start
#   and of B's design to 1e-7 (the e_i from qr.resid(), as the report).
# At least one start must be singular, or the check has tested no repair.
#
# treatment_form: the treatment-space form of the dense state (spec 5.1c, 6.1
# W3; phase 2 Task 8 Step 3b). [baseline], seeds= and tmax= are ignored. The
# engine uses the form only above 500 block columns (mode 3) or 2000 blocks
# (modes 1, 2 and 4); the check hook FIELDHUB_ENGINE_DEBUG=treatment_form
# forces it wherever there are fewer treatments than block columns, so it can
# be compared with dense references on small shapes. Every call below runs
# with FIELDHUB_ENGINE_DEBUG=1,treatment_form and must report that it used
# the form.
# (a) mode 3 on 9/3/2, 12/2/4, 12/6/3, 24/2/3 and 30/15/4 nested (a shared
#     design never has fewer treatments than block columns), seeds 1:4, and
#     on 12/2/3 (seeds 8, 9, 23, 1) and 8/4/3 (seeds 4, 9, 14, 1) nested, from
#     onestage_stage1() starts and from random complete starts. The stage 1
#     design of 12/2/4 was full rank on seeds 1:30 (measured 2026-11-22), so
#     12/2/3 and 8/4/3, whose stage 1 design is singular on the first three
#     seeds named, stand in for it as singular stage 1 starts:
#     - the engine's classification of the start equals the dense full-rank
#       test; the starts must include both sides of the trace shortcut (tr(M)
#       <= 1e7, and no usable trace: a singular start, among them stage 1
#       designs of 9/3/2 and of at least two other shapes);
#     - a budget of one evaluation and a full search: `full_rank` equals the
#       dense test of the returned design; where full rank, `joint_log_det`,
#       `joint_d`, `row_d`, `row_a` equal the dense references of `crossed` to
#       1e-9; the Row floors hold; debug_max_error <= 1e-8; and the refreshes
#       that score the start, the repaired design and the result number at
#       most 3 of the joint state and 2 Row values when the end-of-search
#       drift check does not alarm;
#     - the repair (singular starts): full rank after it, by the dense test
#       (the repair's ridge state has a small trace, which must not pass for
#       full rank), and the repair criterion before and after equals the
#       dense eigenvalue value sum_i log(e_i + eps) + (B - v + 1) log(1 + eps)
#       to 1e-9. The e_i are the eigenvalues of C / r built from the counts
#       (the v x v form of spec 5.1); one below 1e-9 in size is taken as the
#       exact zero it is (a singular design of whole counts), because its
#       computed value, near 1e-16, would otherwise move the log by 1e-10
#       against eps = 1e-6. The engine's own value carries the same rounding
#       (measured: at most 6.2e-10 from the reference).
# (b) a search cut in the middle of a descent: on 24/2/3 and 30/15/4 nested
#     from random complete starts (seeds 1:2), the smallest work limits that
#     let the joint stage apply at least 1, 10 and 100 swaps (found by
#     bisection; the same seed gives the same prefix of the search). Each
#     such call stops on its work limit and returns a complete design whose
#     reported values equal the dense references to 1e-9, full rank, with the
#     Row floors.
# (c) modes 1, 2 and 4 with the admission switched off: blocks_engine(
#     resolvable = TRUE, admission = FALSE) on 12/6/3, 30/15/4, 8/4/3 and
#     10/5/4 (t / blocks per replicate / r); rows_within_columns(admission =
#     FALSE) on the transposed 12/2/4 and 24/2/3 from random complete starts;
#     segmented_blocks() on blocks that span the replicates (6 treatments, 4
#     replicates, 12 blocks of 2; 8 treatments, 3 replicates, 12 blocks of
#     2). Seeds 1:4. D and A of the result equal the one-way eigenvalue
#     reference to 1e-9 (the first engine check of the one-factor identity
#     Bhat^-1 = (M + (k - 1) yhat yhat') / k), and debug_max_error <= 1e-8.
#
# report: the joint efficiency of an onestage design from the v x v treatment
# information (onestage_joint_efficiency(); spec 6.1 W7). [baseline], seeds=
# and tmax= are ignored. On 24/6/2, 30/5/2, 48/6/3, 9/3/2, 100/2/4, 100/50/3
# and 200/100/4, nested and shared, three onestage_stage1() designs (seeds
# 1:3) and three random complete designs each:
# - the unrounded D and A equal those of the QR reference (the arithmetic of
#   row_column_joint_efficiency(): qr.resid() of the treatment indicators on
#   the Rows + Cols model) to 1e-10;
# - the NA cases agree (smallest efficiency factor below 1e-7);
# - the rounded values are identical to row_column_joint_efficiency()'s,
#   except where the reference lies within 1e-10 of a rounding boundary of
#   the seventh decimal.
# It also times the function at 2000/2/4 nested on a random complete design
# and prints the seconds; the QR report took 158 s there (spec 6.1 W7).

arguments <- commandArgs(trailingOnly = TRUE)
named <- grepl("^[a-z]+=", arguments)
opts <- arguments[named]
positional <- arguments[!named]
usage <- "Usage: Rscript tools/check-onestage-engine.R <stage1|crossed|repair|treatment_form|report> [baseline|-] [library] [seeds=20] [tmax=100]"
if (length(positional) < 1L || length(positional) > 3L ||
    !positional[1] %in% c("stage1", "crossed", "repair", "treatment_form", "report")) {
  message(usage)
  quit(status = 1)
}
opt <- function(name, default) {
  x <- grep(paste0("^", name, "="), opts, value = TRUE)
  if (length(x) == 0L) default else as.numeric(sub(paste0("^", name, "="), "", x[1]))
}
unknown <- setdiff(sub("=.*$", "", opts), c("seeds", "tmax"))
if (length(unknown)) { message("Unknown argument(s): ", paste(unknown, collapse = ", "), "\n", usage); quit(status = 1) }
check <- positional[1]
baseline_path <- if (length(positional) >= 2L && positional[2] != "-") path.expand(positional[2]) else NULL
library_path <- if (length(positional) == 3L) positional[3] else "~/fieldhub-libs/dev"
n_seeds <- as.integer(opt("seeds", 20))
tmax <- opt("tmax", 100)
.libPaths(c(normalizePath(library_path, mustWork = TRUE), .libPaths()))
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
suppressMessages(library(FielDHub))
source(file.path(script_dir, "..", "tests", "testthat", "helper-nested-goldens.R"))
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

TOL <- 1.5e-7
failures <- 0L
report <- function(ok, label) {
  cat(if (ok) "PASS" else "FAIL", label, "\n")
  if (!ok) failures <<- failures + 1L
  invisible(ok)
}

if (check == "report") {
  ns <- asNamespace("FielDHub")
  for (f in c("onestage_joint_efficiency", "onestage_stage1", "onestage_design_frame")) {
    if (!exists(f, envir = ns, inherits = FALSE)) {
      report(FALSE, paste0(f, "() exists in the installed FielDHub"))
      cat(failures, "FAIL(s)\n")
      quit(status = 1)
    }
  }
  fast <- get("onestage_joint_efficiency", envir = ns)
  slow <- get("row_column_joint_efficiency", envir = ns)
  frame <- get("onestage_design_frame", envir = ns)
  stage1 <- get("onestage_stage1", envir = ns)
  work_limit <- get("onestage_work_limit", envir = ns)
  # The unrounded efficiency factors by the arithmetic of the QR report.
  qr_factors <- function(design) {
    blocks <- data.frame(Rows = factor(design$Level_3), Cols = factor(design$Level_2))
    TF <- factor(design$treatments)
    v <- nlevels(TF)
    resid <- qr.resid(qr(stats::model.matrix(~ Rows + Cols, data = blocks)), stats::model.matrix(~ TF - 1))
    sort(eigen(crossprod(resid), symmetric = TRUE, only.values = TRUE)$values,
         decreasing = TRUE)[seq_len(v - 1L)] / mean(table(TF))
  }
  on_boundary <- function(x) abs(x * 1e7 - floor(x * 1e7) - 0.5) <= 1e-10 * 1e7
  worst <- 0
  n_na <- 0L
  for (shape in list(c(24, 6, 2), c(30, 5, 2), c(48, 6, 3), c(9, 3, 2), c(100, 2, 4), c(100, 50, 3), c(200, 100, 4))) {
    v <- shape[1]; nrows <- shape[2]; r <- shape[3]; ncols <- v / nrows
    for (shared in c(FALSE, TRUE)) {
      label <- sprintf("%d/%d/%d %s", v, nrows, r, if (shared) "latinized" else "nested")
      problems <- character()
      designs <- 0L
      for (seed in 1:3) {
        for (kind in c("stage 1", "random")) {
          fail <- function(what) problems <<- c(problems, sprintf("seed %d, %s design: %s", seed, kind, what))
          set.seed(seed)
          trt <- if (kind == "stage 1") {
            s1 <- tryCatch(suppressWarnings(
              if ("first" %in% names(formals(stage1))) stage1(v, nrows, ncols, r, shared, work_limit, TRUE)
              else stage1(v, nrows, ncols, r, shared, work_limit)), error = function(e) e)
            if (inherits(s1, "error")) { fail(paste("stage 1 error:", conditionMessage(s1))); next }
            as.integer(s1$treatments)
          } else as.vector(vapply(seq_len(r), function(m) sample.int(v), integer(v)))
          design <- frame(trt, v, nrows, ncols, r, shared)
          designs <- designs + 1L
          e <- qr_factors(design)
          ref_na <- anyNA(e) || min(e) < 1e-7
          got <- fast(design, digits = NULL)
          rounded <- fast(design)
          old <- slow(design)
          if (!identical(is.na(got$Aeffic), ref_na) || !identical(is.na(rounded$Aeffic), is.na(old$Aeffic))) {
            fail(sprintf("NA %s, reference NA %s (smallest factor %.3g)", is.na(got$Aeffic), ref_na, min(e)))
            next
          }
          if (ref_na) { n_na <- n_na + 1L; next }
          ref <- c(D = exp(mean(log(e))), A = (v - 1) / sum(1 / e))
          err <- max(abs(got$Deffic - ref[["D"]]), abs(got$Aeffic - ref[["A"]]))
          worst <- max(worst, err)
          if (!(err <= 1e-10)) fail(sprintf("D %.13f (ref %.13f), A %.13f (ref %.13f)", got$Deffic, ref[["D"]], got$Aeffic, ref[["A"]]))
          if (!(identical(rounded$Deffic, old$Deffic) || on_boundary(ref[["D"]])) ||
              !(identical(rounded$Aeffic, old$Aeffic) || on_boundary(ref[["A"]]))) {
            fail(sprintf("rounded D %.7f A %.7f, report D %.7f A %.7f", rounded$Deffic, rounded$Aeffic, old$Deffic, old$Aeffic))
          }
        }
      }
      report(length(problems) == 0L && designs == 6L,
             sprintf("report %s: %d designs%s", label, designs, if (length(problems)) paste0(": ", problems[1]) else ""))
    }
  }
  report(n_na >= 1L, sprintf("report: %d designs with an NA joint efficiency in both (need >= 1)", n_na))
  cat(sprintf("  largest |D or A - QR reference|: %.2e\n", worst))
  set.seed(1)
  v <- 2000; nrows <- 2; r <- 4
  design <- frame(as.vector(vapply(seq_len(r), function(m) sample.int(v), integer(v))), v, nrows, v / nrows, r, FALSE)
  seconds <- system.time(big <- fast(design))[["elapsed"]]
  report(!is.na(big$Aeffic), sprintf("report 2000/2/4 nested, random complete design: D %.7f, A %.7f", big$Deffic, big$Aeffic))
  cat(sprintf("  time 2000/2/4 nested: %.1f s (the QR report: 158 s, spec 6.1 W7)\n", seconds))
  cat(failures, "FAIL(s)\n")
  quit(status = if (failures > 0L) 1 else 0)
}

FLOOR <- 1e-12
if (check %in% c("crossed", "repair", "treatment_form")) {
  ns <- asNamespace("FielDHub")
  for (f in c("crossed_search", "crossed_default_budget", "onestage_stage1")) {
    if (!exists(f, envir = ns, inherits = FALSE)) {
      report(FALSE, paste0(f, "() exists in the installed FielDHub"))
      quit(status = 1)
    }
  }
  crossed <- get("crossed_search", envir = ns)
  stage1 <- get("onestage_stage1", envir = ns)
  work_limit <- get("onestage_work_limit", envir = ns)
  VAL <- 1e-9
  # Canonical plot order: replicate, row, column (constraints).
  layout_of <- function(v, r, nrows, shared) {
    ncols <- v / nrows
    rep_of <- rep(seq_len(r), each = v)
    row_in <- rep(rep(seq_len(nrows), each = ncols), r)
    col_in <- rep(seq_len(ncols), nrows * r)
    list(rep = rep_of,
         row = if (shared) row_in else (rep_of - 1L) * nrows + row_in,
         col = if (shared) col_in else (rep_of - 1L) * ncols + col_in)
  }
  # Unrounded dense references: joint efficiencies from qr.resid() of the
  # Rows + Cols model (as row_column_joint_efficiency()), one-way from
  # C = r I - N N' / k.
  joint_ref <- function(trt, lay, v, r) {
    blocks <- data.frame(Rows = factor(lay$row), Cols = factor(lay$col))
    Tind <- stats::model.matrix(~ factor(trt, levels = seq_len(v)) - 1)
    resid <- qr.resid(qr(stats::model.matrix(~ Rows + Cols, data = blocks)), Tind)
    e <- sort(eigen(crossprod(resid), symmetric = TRUE, only.values = TRUE)$values,
              decreasing = TRUE)[seq_len(v - 1L)] / r
    list(full = min(e) >= 1e-7, log_det = if (min(e) > 0) sum(log(e)) else -Inf,
         d = if (min(e) > 0) exp(mean(log(e))) else 0, e = e)
  }
  row_ref <- function(trt, block, v, r) {
    b <- max(block)
    N <- matrix(tabulate((block - 1L) * v + trt, v * b), v, b)
    C <- r * diag(v) - N %*% (t(N) / colSums(N))
    e <- sort(eigen(C / r, symmetric = TRUE, only.values = TRUE)$values, decreasing = TRUE)[seq_len(v - 1L)]
    c(d = exp(mean(log(e))), a = (v - 1) / sum(1 / e))
  }
  close <- function(x, y) is.numeric(x) && length(x) == 1L && !is.na(x) && abs(x - y) <= VAL * max(1, abs(y))
  complete_reps <- function(trt, lay, v, r) {
    length(trt) == v * r && !anyNA(trt) &&
      all(vapply(seq_len(r), function(m) identical(sort(as.integer(trt[lay$rep == m])), seq_len(v)), logical(1)))
  }
  # The Row floors of a result against its start (header): a problem, or NULL.
  floor_problem <- function(res, trt, start, lay, v, r, fallback = 0) {
    rw <- row_ref(trt, lay$row, v, r)
    rw0 <- row_ref(start, lay$row, v, r)
    if (!(res$row_d >= res$stage1_row_d)) return(sprintf("Row D %.17g below stage 1 %.17g", res$row_d, res$stage1_row_d))
    if (!(res$row_a >= res$stage1_row_a)) return(sprintf("Row A %.17g below stage 1 %.17g", res$row_a, res$stage1_row_a))
    if (!(rw[["d"]] >= rw0[["d"]] * (1 - FLOOR))) return(sprintf("reference Row D %.15f below the start's %.15f", rw[["d"]], rw0[["d"]]))
    if (!(rw[["a"]] >= rw0[["a"]] * (1 - FLOOR))) return(sprintf("reference Row A %.15f below the start's %.15f", rw[["a"]], rw0[["a"]]))
    if (!is.null(fallback) && !identical(as.integer(res$search$floor_fallback), as.integer(fallback))) {
      return(sprintf("floor_fallback %s", format(res$search$floor_fallback)))
    }
    NULL
  }
}

if (check == "crossed") {

  # (a) values on random complete designs, budget = 1
  set.seed(20261120)
  shapes <- list()
  for (v in c(4, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 28, 30)) {
    d <- seq_len(v)[v %% seq_len(v) == 0L]
    d <- d[d >= 2L & d <= v / 2]
    if (length(d)) shapes[[length(shapes) + 1L]] <- expand.grid(v = v, nrows = d)
  }
  shapes <- do.call(rbind, shapes)
  pick <- shapes[sample.int(nrow(shapes), 20L, replace = nrow(shapes) < 20L), ]
  pick$r <- sample(2:4, 20L, replace = TRUE)
  pick$shared <- rep(c(FALSE, TRUE), 10L)
  n_full <- 0L
  for (i in seq_len(nrow(pick))) {
    v <- pick$v[i]; nrows <- pick$nrows[i]; r <- pick$r[i]; shared <- pick$shared[i]
    lay <- layout_of(v, r, nrows, shared)
    start <- as.vector(replicate(r, sample.int(v)))
    label <- sprintf("crossed values %d/%d/%d %s", v, nrows, r, if (shared) "shared" else "nested")
    res <- tryCatch(crossed(start, v, r, nrows, v / nrows, shared, budget = 1, work_cap = work_limit),
                    error = function(e) e)
    if (inherits(res, "error")) { report(FALSE, paste0(label, ": error ", conditionMessage(res))); next }
    ref0 <- joint_ref(start, lay, v, r)
    trt <- as.integer(res$treatments)
    ref <- joint_ref(trt, lay, v, r)
    if (!identical(res$full_rank, ref$full) || (ref0$full && !ref$full)) {
      report(FALSE, sprintf("%s: full_rank %s, reference %s (start %s)", label, format(res$full_rank), ref$full, ref0$full))
      next
    }
    if (!ref$full) {
      # (No repair runs when the rows of the start are disconnected.)
      report(complete_reps(trt, lay, v, r) && is.na(res$joint_d) && res$search$evaluations == 0,
             sprintf("%s: not full rank (%g repair evaluations), no joint stage", label,
                     res$search$repair_evaluations))
      next
    }
    n_full <- n_full + 1L
    if (!ref0$full) label <- paste(label, "(repaired)")
    rw <- row_ref(trt, lay$row, v, r)
    rw0 <- row_ref(start, lay$row, v, r)
    ok <- complete_reps(trt, lay, v, r) && close(res$joint_log_det, ref$log_det) &&
      close(res$joint_d, ref$d) && close(res$row_d, rw[["d"]]) && close(res$row_a, rw[["a"]]) &&
      close(res$stage1_row_d, rw0[["d"]]) && close(res$stage1_row_a, rw0[["a"]]) &&
      res$search$evaluations <= 1
    report(ok, sprintf("%s: joint log det %.12g (ref %.12g), joint D %.10f (ref %.10f), Row D %.10f (ref %.10f), Row A %.10f (ref %.10f)",
                       label, res$joint_log_det, ref$log_det, res$joint_d, ref$d,
                       res$row_d, rw[["d"]], res$row_a, rw[["a"]]))
  }
  report(n_full >= 10L, sprintf("crossed values: %d of 20 random designs full rank (need >= 10)", n_full))

  # (b) invariants and (c) parity after full searches from stage 1 starts
  Sys.setenv(FIELDHUB_ENGINE_DEBUG = "1")
  cases <- data.frame(t = c(24, 30, 48, 24, 30, 12, 16), nrows = c(6, 5, 6, 6, 5, 4, 4),
                      r = c(2, 2, 3, 2, 2, 2, 2),
                      shared = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
                      target = c(0.5929, 0.6349, 0.7173, 0.8466, NA, NA, NA),
                      parity_seeds = c(8, 8, 4, 8, NA, NA, NA))
  for (g in seq_len(nrow(cases))) {
    v <- cases$t[g]; nrows <- cases$nrows[g]; r <- cases$r[g]; shared <- cases$shared[g]
    ncols <- v / nrows
    lay <- layout_of(v, r, nrows, shared)
    label <- sprintf("%d/%d/%d %s", v, nrows, r, if (shared) "latinized" else "nested")
    joint_d <- rep(NA_real_, 8L)
    problems <- character()
    worst_debug <- 0
    seconds <- system.time(for (seed in 1:8) {
      set.seed(seed)
      s1 <- stage1(v, nrows, ncols, r, shared, work_limit)
      ref0 <- joint_ref(s1$treatments, lay, v, r)
      res <- tryCatch(crossed(s1$treatments, v, r, nrows, ncols, shared,
                              work_cap = max(work_limit - s1$work, 1)), error = function(e) e)
      if (inherits(res, "error")) { problems <- c(problems, sprintf("seed %d error: %s", seed, conditionMessage(res))); next }
      trt <- as.integer(res$treatments)
      fail <- function(what) problems <<- c(problems, sprintf("seed %d: %s", seed, what))
      if (!complete_reps(trt, lay, v, r)) { fail("a replicate is not complete"); next }
      if (!isTRUE(res$full_rank)) { if (ref0$full) fail("full-rank start reported not full rank"); next }
      ref <- joint_ref(trt, lay, v, r)
      rw <- row_ref(trt, lay$row, v, r)
      fp <- floor_problem(res, trt, s1$treatments, lay, v, r)
      if (!is.null(fp)) fail(fp)
      if (!(ref$log_det >= ref0$log_det - VAL)) fail(sprintf("joint log det %.10f below the start's %.10f", ref$log_det, ref0$log_det))
      if (!(close(res$joint_log_det, ref$log_det) && close(res$joint_d, ref$d) &&
            close(res$row_d, rw[["d"]]) && close(res$row_a, rw[["a"]]))) fail("final values differ from the reference")
      dbg <- res$search$debug_max_error
      if (!is.numeric(dbg) || is.na(dbg) || dbg > 1e-8) fail(sprintf("debug_max_error %s", format(dbg)))
      else worst_debug <- max(worst_debug, dbg)
      joint_d[seed] <- res$joint_d
    })[["elapsed"]]
    report(length(problems) == 0L, sprintf("crossed invariants %s (8 seeds, max debug error %.2e)%s", label,
                                           worst_debug, if (length(problems)) paste0(": ", problems[1]) else ""))
    cat(sprintf("  %s: %.1f s; joint D by seed %s\n", label, seconds,
                paste(sprintf("%.4f", joint_d), collapse = " ")))
    if (!is.na(cases$target[g])) {
      m <- mean(joint_d[seq_len(cases$parity_seeds[g])])
      report(!is.na(m) && m >= cases$target[g] - 0.001,
             sprintf("crossed parity %s: mean joint D %.4f over seeds 1:%d, target %.4f - 0.001",
                     label, m, cases$parity_seeds[g], cases$target[g]))
    }
  }
  # Ill-conditioned states: random complete starts, many near-singular, where
  # a rank-2 ratio loses digits (the prototype review's pitfall).
  set.seed(20261121)
  worst_debug <- 0
  problems <- character()
  searched <- 0L
  for (i in 1:60) {
    v <- sample(c(8, 9, 12, 16), 1L); nrows <- if (v == 9) 3L else 4L; r <- sample(2:3, 1L)
    shared <- i %% 3L == 0L
    lay <- layout_of(v, r, nrows, shared)
    start <- as.vector(replicate(r, sample.int(v)))
    ref0 <- joint_ref(start, lay, v, r)
    res <- crossed(start, v, r, nrows, v / nrows, shared, work_cap = work_limit)
    trt <- as.integer(res$treatments)
    ref <- joint_ref(trt, lay, v, r)
    if (!identical(res$full_rank, ref$full) || (ref0$full && !ref$full)) {
      problems <- c(problems, sprintf("start %d: full_rank differs", i))
      next
    }
    if (!ref$full) next
    searched <- searched + 1L
    dbg <- res$search$debug_max_error
    if (!complete_reps(trt, lay, v, r) || !(ref$log_det >= ref0$log_det - VAL) ||
        !is.null(floor_problem(res, trt, start, lay, v, r)) ||
        !close(res$joint_log_det, ref$log_det) || !is.numeric(dbg) || is.na(dbg) || dbg > 1e-8) {
      problems <- c(problems, sprintf("start %d (%d/%d/%d): invariant or debug error %s", i, v, nrows, r, format(dbg)))
    } else worst_debug <- max(worst_debug, dbg)
  }
  report(length(problems) == 0L && searched >= 20L,
         sprintf("crossed ill-conditioned random starts: %d searched, max debug error %.2e%s", searched,
                 worst_debug, if (length(problems)) paste0(": ", problems[1]) else ""))
  # (d) the end-of-search Row comparison, reached through the check hook
  Sys.setenv(FIELDHUB_ENGINE_DEBUG = "slack_floors")
  problems <- character()
  replaced <- character()
  hooked <- 0L
  for (shape in list(c(24, 6, 2), c(48, 6, 3), c(16, 8, 4))) {
    v <- shape[1]; nrows <- shape[2]; r <- shape[3]; ncols <- v / nrows
    lay <- layout_of(v, r, nrows, FALSE)
    for (seed in 1:8) {
      set.seed(seed)
      s1 <- stage1(v, nrows, ncols, r, FALSE, work_limit)
      res <- crossed(s1$treatments, v, r, nrows, ncols, FALSE, work_cap = max(work_limit - s1$work, 1))
      trt <- as.integer(res$treatments)
      hooked <- hooked + 1L
      tag <- sprintf("%d/%d/%d seed %d", v, nrows, r, seed)
      fp <- floor_problem(res, trt, s1$treatments, lay, v, r, fallback = NULL)
      ref <- joint_ref(trt, lay, v, r)
      ref0 <- joint_ref(s1$treatments, lay, v, r)
      if (!is.null(fp)) problems <- c(problems, paste0(tag, ": ", fp))
      else if (!complete_reps(trt, lay, v, r) || !isTRUE(res$full_rank) || !ref$full ||
               !(ref$log_det >= ref0$log_det - VAL) || !close(res$joint_log_det, ref$log_det)) {
        problems <- c(problems, paste0(tag, ": not a full-rank design at or above the start"))
      }
      if (identical(as.integer(res$search$floor_fallback), 1L)) {
        replaced <- c(replaced, sprintf("%s (joint D %.4f, start %.4f)", tag, res$joint_d, ref0$d))
      }
    }
  }
  report(length(problems) == 0L && length(replaced) >= 2L,
         sprintf("crossed end-of-search Row comparison (hook, %d runs): %d replaced (need >= 2)%s%s", hooked,
                 length(replaced), if (length(replaced)) paste0(": ", paste(replaced, collapse = "; ")) else "",
                 if (length(problems)) paste0("; PROBLEM ", problems[1]) else ""))
  # The repair's output under the hook: full rank, but below the Row floors.
  v <- 8; nrows <- 4; ncols <- 2; r <- 3
  lay <- layout_of(v, r, nrows, FALSE)
  set.seed(36)
  s1 <- stage1(v, nrows, ncols, r, FALSE, work_limit)
  start <- as.integer(s1$treatments)
  res <- crossed(start, v, r, nrows, ncols, FALSE, work_cap = max(work_limit - s1$work, 1))
  trt <- as.integer(res$treatments)
  fp <- floor_problem(res, trt, start, lay, v, r, fallback = 2)
  why <- if (joint_ref(start, lay, v, r)$full) "the start is full rank, so no repair ran"
         else if (!is.null(fp)) fp
         else if (!identical(trt, start)) "the start was not returned"
         else if (!identical(res$full_rank, FALSE) || !is.na(res$joint_d) || !is.na(res$joint_a)) "full_rank or joint values claimed"
         else if (!identical(res$search$evaluations, 0) || !(res$search$repair_swaps > 0)) "evaluations spent, or no repair swap"
         else NULL
  report(is.null(why), sprintf("crossed repair output below the Row floors (hook, 8/4/3 nested seed 36): floor_fallback %s, %g repair swaps, start returned%s",
                               format(res$search$floor_fallback), res$search$repair_swaps,
                               if (is.null(why)) "" else paste0("; PROBLEM ", why)))
  Sys.unsetenv("FIELDHUB_ENGINE_DEBUG")
  cat(failures, "FAIL(s)\n")
  quit(status = if (failures > 0L) 1 else 0)
}

if (check == "treatment_form") {
  EPS <- 1e-6
  Sys.setenv(FIELDHUB_ENGINE_DEBUG = "1,treatment_form")
  used_form <- function(res) isTRUE(res$search$check$treatment_form)
  # The repair criterion of the engine from the dense eigenvalues (header).
  criterion <- function(trt, lay, v, r, B) {
    counts <- function(block) matrix(tabulate((block - 1L) * v + trt, v * max(block)), v, max(block))
    Nr <- counts(lay$row); Nc <- counts(lay$col)
    C <- diag(v) - tcrossprod(Nr) / (r * colSums(Nr)[1]) - tcrossprod(Nc) / (r * colSums(Nc)[1]) + 1 / v
    e <- sort(eigen(C, symmetric = TRUE, only.values = TRUE)$values, decreasing = TRUE)[seq_len(v - 1L)]
    e[abs(e) < 1e-9] <- 0
    sum(log(e + EPS)) + (B - v + 1) * log1p(EPS)
  }
  # The values of a result against the dense references: a problem, or NULL.
  value_problem <- function(res, start, lay, v, r) {
    trt <- as.integer(res$treatments)
    if (!used_form(res)) return("the treatment-space form was not used")
    if (!complete_reps(trt, lay, v, r)) return("a replicate is not complete")
    ref <- joint_ref(trt, lay, v, r)
    if (!identical(res$full_rank, ref$full)) return(sprintf("full_rank %s, dense test %s", format(res$full_rank), ref$full))
    if (!ref$full) return(if (is.na(res$joint_d)) NULL else "joint values of a design that is not full rank")
    rw <- row_ref(trt, lay$row, v, r)
    rw0 <- row_ref(start, lay$row, v, r)
    if (!(close(res$joint_log_det, ref$log_det) && close(res$joint_d, ref$d) &&
          close(res$row_d, rw[["d"]]) && close(res$row_a, rw[["a"]]) &&
          close(res$stage1_row_d, rw0[["d"]]) && close(res$stage1_row_a, rw0[["a"]]))) {
      return(sprintf("values differ from the dense references (joint log det %.12g, ref %.12g)",
                     res$joint_log_det, ref$log_det))
    }
    floor_problem(res, trt, start, lay, v, r)
  }
  scoring_problem <- function(res) {
    ck <- res$search$check
    if (isTRUE(ck$end_alarm)) return(NULL)
    if (ck$scoring_refreshes[["joint"]] > 3 || ck$scoring_refreshes[["row"]] > 2) {
      return(sprintf("%g joint and %g Row scoring refreshes", ck$scoring_refreshes[["joint"]],
                     ck$scoring_refreshes[["row"]]))
    }
    NULL
  }
  debug_problem <- function(dbg) {
    if (!is.numeric(dbg) || length(dbg) != 1L || is.na(dbg) || dbg > 1e-8) sprintf("debug_max_error %s", format(dbg)) else NULL
  }
  shapes <- list(c(9, 3, 2), c(12, 2, 4), c(12, 6, 3), c(24, 2, 3), c(30, 15, 4), c(12, 2, 3), c(8, 4, 3))
  seed_sets <- list("12/2/3" = c(8L, 9L, 23L, 1L), "8/4/3" = c(4L, 9L, 14L, 1L))
  sides <- c(shortcut = 0L, no_trace = 0L, exact = 0L)
  singular_stage1 <- character()
  worst <- c(debug = 0, criterion = 0)
  repaired_runs <- 0L
  for (shape in shapes) {
    v <- shape[1]; nrows <- shape[2]; r <- shape[3]; ncols <- v / nrows
    lay <- layout_of(v, r, nrows, FALSE)
    B <- max(lay$row) + max(lay$col)
    label <- sprintf("%d/%d/%d nested", v, nrows, r)
    problems <- character()
    runs <- 0L
    seeds <- seed_sets[[sprintf("%d/%d/%d", v, nrows, r)]]
    if (is.null(seeds)) seeds <- 1:4
    for (seed in seeds) {
      for (kind in c("stage 1", "random")) {
        fail <- function(what) problems <<- c(problems, sprintf("seed %d, %s start: %s", seed, kind, what))
        set.seed(seed)
        if (kind == "stage 1") {
          s1 <- stage1(v, nrows, ncols, r, FALSE, work_limit)
          start <- as.integer(s1$treatments)
        } else {
          start <- as.vector(vapply(seq_len(r), function(m) sample.int(v), integer(v)))
        }
        ref0 <- joint_ref(start, lay, v, r)
        state <- get(".Random.seed", envir = globalenv())
        run <- function(...) {
          assign(".Random.seed", state, envir = globalenv())
          tryCatch(crossed(start, v, r, nrows, ncols, FALSE, work_cap = work_limit, ...), error = function(e) e)
        }
        one <- run(budget = 1)
        full <- run()
        none <- run(budget = 0)
        bad <- Filter(function(x) inherits(x, "error"), list(one, full, none))
        if (length(bad)) { fail(paste("error:", conditionMessage(bad[[1]]))); next }
        runs <- runs + 1L
        # the start: classification and the side of the trace shortcut
        ck <- one$search$check
        if (!identical(ck$start_full_rank, ref0$full)) {
          fail(sprintf("start classified %s, dense test %s (tr(M) %s)", format(ck$start_full_rank), ref0$full,
                       format(ck$start_trace)))
        }
        side <- if (is.na(ck$start_trace)) "no_trace" else if (ck$start_trace <= 1e7) "shortcut" else "exact"
        sides[[side]] <- sides[[side]] + 1L
        if (kind == "stage 1" && !ref0$full) singular_stage1 <- c(singular_stage1, sprintf("%d/%d/%d", v, nrows, r))
        for (res in list(one, full, none)) {
          vp <- value_problem(res, start, lay, v, r)
          if (!is.null(vp)) fail(vp)
          sp <- scoring_problem(res)
          if (!is.null(sp)) fail(sp)
        }
        if (isTRUE(full$full_rank)) {
          dp <- debug_problem(full$search$debug_max_error)
          if (!is.null(dp)) fail(dp) else worst[["debug"]] <- max(worst[["debug"]], full$search$debug_max_error)
        }
        # the repair, where the rows of a singular start are connected
        if (!ref0$full && none$search$repair_evaluations > 0) {
          repaired_runs <- repaired_runs + 1L
          if (!isTRUE(none$full_rank) || !isTRUE(full$full_rank)) fail("not full rank after the repair")
          want <- c(criterion(start, lay, v, r, B), criterion(as.integer(none$treatments), lay, v, r, B))
          got <- none$search$repair_log_det
          err <- if (length(got) == 2L && !anyNA(got)) max(abs(got - want)) else Inf
          worst[["criterion"]] <- max(worst[["criterion"]], if (is.finite(err)) err else 0)
          if (!(err <= 1e-9)) fail(sprintf("repair criterion %s, dense %s", paste(format(got, digits = 14), collapse = " -> "),
                                           paste(format(want, digits = 14), collapse = " -> ")))
        }
      }
    }
    report(length(problems) == 0L && runs == 8L,
           sprintf("treatment form, mode 3 %s: %d starts, each with a budget of 0, of 1 and a full search%s", label, runs,
                   if (length(problems)) paste0(": ", problems[1]) else ""))
  }
  report(sides[["shortcut"]] >= 1L && sides[["no_trace"]] >= 1L &&
           "9/3/2" %in% singular_stage1 && length(unique(singular_stage1)) >= 3L,
         sprintf("treatment form, start classification: %d starts with tr(M) <= 1e7, %d above, %d without a usable trace; singular stage 1 starts on %s (need 9/3/2 and two other shapes)",
                 sides[["shortcut"]], sides[["exact"]], sides[["no_trace"]],
                 if (length(singular_stage1)) paste(unique(singular_stage1), collapse = ", ") else "none"))
  report(repaired_runs >= 2L, sprintf("treatment form, repair: %d singular starts repaired (need >= 2)", repaired_runs))
  cat(sprintf("  largest debug error %.2e; largest |repair criterion - dense| %.2e\n", worst[["debug"]], worst[["criterion"]]))

  # (b) a search cut in the middle of a descent
  for (shape in list(c(24, 2, 3), c(30, 15, 4))) {
    v <- shape[1]; nrows <- shape[2]; r <- shape[3]; ncols <- v / nrows
    lay <- layout_of(v, r, nrows, FALSE)
    label <- sprintf("%d/%d/%d nested", v, nrows, r)
    problems <- character()
    found <- character()
    for (seed in 1:2) {
      set.seed(seed)
      start <- as.vector(vapply(seq_len(r), function(m) sample.int(v), integer(v)))
      state <- get(".Random.seed", envir = globalenv())
      run <- function(cap) {
        assign(".Random.seed", state, envir = globalenv())
        crossed(start, v, r, nrows, ncols, FALSE, work_cap = cap)
      }
      whole <- run(work_limit)
      if (!joint_ref(start, lay, v, r)$full) { problems <- c(problems, sprintf("seed %d: the random start is singular", seed)); next }
      for (target in c(1, 10, 100)) {
        if (whole$search$check$accepted <= target) {
          problems <- c(problems, sprintf("seed %d: the whole search applied only %g swaps", seed, whole$search$check$accepted))
          next
        }
        lo <- 1; hi <- whole$search$work
        for (step in 1:60) {
          mid <- (lo + hi) / 2
          if (run(mid)$search$check$accepted >= target) hi <- mid else lo <- mid
          if (hi - lo <= 1) break
        }
        res <- run(hi)
        tag <- sprintf("seed %d, limit for %g swaps", seed, target)
        vp <- value_problem(res, start, lay, v, r)
        if (!is.null(vp)) problems <- c(problems, paste0(tag, ": ", vp))
        else if (!isTRUE(res$search$work_capped) || !isTRUE(res$full_rank)) problems <- c(problems, paste0(tag, ": not cut by the work limit, or not full rank"))
        else if (res$search$check$accepted < target || res$search$check$accepted >= whole$search$check$accepted) {
          problems <- c(problems, sprintf("%s: %g swaps applied", tag, res$search$check$accepted))
        }
        found <- c(found, sprintf("%g", res$search$check$accepted))
      }
    }
    report(length(problems) == 0L && length(found) == 6L,
           sprintf("treatment form, mode 3 cut in a descent %s: swaps applied %s%s", label, paste(found, collapse = " "),
                   if (length(problems)) paste0(": ", problems[1]) else ""))
  }

  # (c) modes 1, 2 and 4 with the admission switched off
  one_way_problem <- function(d, a, trt, block, v, r, search) {
    if (!isTRUE(search$treatment_form)) return("the treatment-space form was not used")
    if (anyNA(trt) || is.na(d) || is.na(a)) return("no scored design")
    rw <- row_ref(as.integer(trt), as.integer(block), v, r)
    if (!close(d, rw[["d"]]) || !close(a, rw[["a"]])) {
      return(sprintf("D %.12f (ref %.12f), A %.12f (ref %.12f)", d, rw[["d"]], a, rw[["a"]]))
    }
    debug_problem(search$debug_max_error)
  }
  engine <- get("blocks_engine", envir = ns)
  within <- get("rows_within_columns", envir = ns)
  segmented <- get("segmented_blocks", envir = ns)
  for (shape in list(c(12, 6, 3), c(30, 15, 4), c(8, 4, 3), c(10, 5, 4))) {
    v <- shape[1]; blocks <- shape[2]; r <- shape[3]
    problems <- character(); searched <- 0L
    for (seed in 1:4) {
      set.seed(seed)
      res <- tryCatch(engine(v, r, blocks, resolvable = TRUE, admission = FALSE), error = function(e) e)
      if (inherits(res, "error")) { problems <- c(problems, sprintf("seed %d error: %s", seed, conditionMessage(res))); next }
      if (res$search$stop_reason %in% c("optimal_start", "construction", "closed_form")) next
      searched <- searched + 1L
      op <- one_way_problem(res$Blocks_model$`D-Efficiency`[2], res$Blocks_model$`A-Efficiency`[2],
                            res$Design$treatments, res$Design$Level_2, v, r, res$search)
      if (!is.null(op)) problems <- c(problems, sprintf("seed %d: %s", seed, op))
    }
    report(length(problems) == 0L && searched >= 1L,
           sprintf("treatment form, mode 1 %d treatments in %d blocks of %d (%d of 4 seeds searched)%s", v, r * blocks,
                   v / blocks, searched, if (length(problems)) paste0(": ", problems[1]) else ""))
  }
  for (shape in list(c(12, 2, 4), c(24, 2, 3))) {
    # The column step of stage 1: the rows of the transposed grid are the
    # ncols columns of each replicate.
    v <- shape[1]; nrows <- shape[2]; r <- shape[3]; ncols <- v / nrows
    problems <- character(); searched <- 0L
    for (seed in 1:4) {
      set.seed(seed)
      start <- as.vector(vapply(seq_len(r), function(m) sample.int(v), integer(v)))
      column_of <- rep(seq_len(nrows), ncols * r)
      res <- tryCatch(within(start, column_of, v, r, ncols, admission = FALSE), error = function(e) e)
      if (inherits(res, "error")) { problems <- c(problems, sprintf("seed %d error: %s", seed, conditionMessage(res))); next }
      if (identical(res$search$stop_reason, "optimal_start")) next
      searched <- searched + 1L
      op <- one_way_problem(res$d_efficiency, res$a_efficiency, res$treatments,
                            rep(seq_len(r * ncols), each = nrows), v, r, res$search)
      if (!is.null(op)) problems <- c(problems, sprintf("seed %d: %s", seed, op))
    }
    report(length(problems) == 0L && searched >= 1L,
           sprintf("treatment form, mode 2 %d treatments in %d rows of %d (%d of 4 seeds searched)%s", v, r * ncols,
                   nrows, searched, if (length(problems)) paste0(": ", problems[1]) else ""))
  }
  for (shape in list(c(6, 4, 12), c(8, 3, 12))) {
    v <- shape[1]; r <- shape[2]; blocks <- shape[3]
    n <- v * r
    block_of <- (seq_len(n) - 1L) %% blocks + 1L   # blocks of n / blocks plots that span the replicates
    segment_of <- rep(seq_len(r), each = v)
    problems <- character(); searched <- 0L
    for (seed in 1:4) {
      set.seed(seed)
      start <- as.vector(vapply(seq_len(r), function(m) sample.int(v), integer(v)))
      res <- tryCatch(segmented(start, block_of, segment_of, v, r, blocks), error = function(e) e)
      if (inherits(res, "error")) { problems <- c(problems, sprintf("seed %d error: %s", seed, conditionMessage(res))); next }
      if (is.na(res$a_efficiency)) next
      searched <- searched + 1L
      op <- one_way_problem(res$d_efficiency, res$a_efficiency, res$treatments, block_of, v, r, res$search)
      if (!is.null(op)) problems <- c(problems, sprintf("seed %d: %s", seed, op))
    }
    report(length(problems) == 0L && searched >= 1L,
           sprintf("treatment form, mode 4 %d treatments in %d blocks of %d (%d of 4 seeds searched)%s", v, blocks,
                   n / blocks, searched, if (length(problems)) paste0(": ", problems[1]) else ""))
  }
  Sys.unsetenv("FIELDHUB_ENGINE_DEBUG")
  cat(failures, "FAIL(s)\n")
  quit(status = if (failures > 0L) 1 else 0)
}

if (check == "repair") {
  EPS <- 1e-6
  joint_a_report <- get("row_column_joint_efficiency", envir = ns)
  grid <- expand.grid(nrows = 2:4, ncols = 2:4, r = 2:4, shared = c(FALSE, TRUE))
  grid$v <- grid$nrows * grid$ncols
  rule <- ifelse(grid$shared, grid$v * grid$r - grid$r - grid$nrows - grid$ncols + 2 >= grid$v - 1,
                 grid$r * (grid$nrows - 1) * (grid$ncols - 1) >= grid$v - 1)
  grid <- grid[rule & grid$v <= 16, ]
  grid <- grid[order(grid$shared, grid$v, grid$nrows, grid$r), ]
  report(sum(!grid$shared) == 22L && sum(grid$shared) == 27L,
         sprintf("repair grid: %d nested and %d shared shapes (review 2: 22 and 27)", sum(!grid$shared), sum(grid$shared)))
  # The repair criterion from the dense efficiency factors (header).
  criterion <- function(trt, lay, v, r, B) {
    e <- pmax(joint_ref(trt, lay, v, r)$e, 0)
    sum(log(e + EPS)) + (B - v + 1) * log1p(EPS)
  }
  total <- c(runs = 0L, full = 0L, singular = 0L)
  worst <- c(criterion = 0, score = 0)
  for (g in seq_len(nrow(grid))) {
    v <- grid$v[g]; nrows <- grid$nrows[g]; ncols <- grid$ncols[g]; r <- grid$r[g]; shared <- grid$shared[g]
    lay <- layout_of(v, r, nrows, shared)
    B <- max(lay$row) + max(lay$col)
    label <- sprintf("%d/%d/%d %s", v, nrows, r, if (shared) "latinized" else "nested")
    problems <- character()
    n_full <- 0L; n_start <- 0L; swaps <- 0; evals <- 0
    for (seed in 1:8) {
      fail <- function(what) problems <<- c(problems, sprintf("seed %d: %s", seed, what))
      set.seed(seed)
      s1 <- stage1(v, nrows, ncols, r, shared, work_limit)
      start <- as.integer(s1$treatments)
      cap <- max(work_limit - s1$work, 1)
      ref0 <- joint_ref(start, lay, v, r)
      n_start <- n_start + ref0$full
      state <- get(".Random.seed", envir = globalenv())
      run <- function(from, ...) tryCatch(crossed(from, v, r, nrows, ncols, shared, work_cap = cap, ...),
                                          error = function(e) e)
      A <- run(start)
      assign(".Random.seed", state, envir = globalenv())
      B0 <- run(start, budget = 0)
      C <- if (inherits(B0, "error")) B0 else run(as.integer(B0$treatments))
      total[["runs"]] <- total[["runs"]] + 1L
      if (!ref0$full) total[["singular"]] <- total[["singular"]] + 1L
      bad <- Filter(function(x) inherits(x, "error"), list(A, B0, C))
      if (length(bad)) { fail(paste("error:", conditionMessage(bad[[1]]))); next }
      trt <- as.integer(A$treatments)
      if (!complete_reps(trt, lay, v, r)) { fail("a replicate is not complete"); next }
      # full rank, by the engine, the reference and the report
      ref <- joint_ref(trt, lay, v, r)
      design <- data.frame(Level_3 = lay$row, Level_2 = lay$col, treatments = trt)
      if (!isTRUE(A$full_rank)) { fail("not full rank"); next }
      n_full <- n_full + 1L
      total[["full"]] <- total[["full"]] + 1L
      if (!ref$full) fail("full_rank, but the reference is not")
      if (is.na(joint_a_report(design)$Aeffic)) fail("row_column_joint_efficiency() gives NA")
      if (is.na(A$joint_a) || !close(A$joint_log_det, ref$log_det)) fail("joint values differ from the reference")
      fp <- floor_problem(A, trt, start, lay, v, r)
      if (!is.null(fp)) fail(fp)
      # budget: the repair spends none of it
      sa <- A$search; sb <- B0$search; sc <- C$search
      if (!identical(sa$evaluations, sc$evaluations) || !identical(trt, as.integer(C$treatments))) {
        fail(sprintf("evaluations %s, without the repair %s%s", format(sa$evaluations), format(sc$evaluations),
                     if (identical(trt, as.integer(C$treatments))) "" else " (designs differ)"))
      }
      if (!identical(sb$evaluations, 0) || !identical(sb$repair_evaluations, sa$repair_evaluations) ||
          !identical(sb$repair_swaps, sa$repair_swaps) || !isTRUE(B0$full_rank)) {
        fail("the repair depends on the evaluation budget")
      }
      if (!identical(sc$repair_evaluations, 0)) fail("a full-rank start was repaired")
      if (!(sa$evaluations <= get("crossed_default_budget", envir = ns)(v, r))) fail("evaluations above the budget")
      if (ref0$full) {
        if (!identical(sa$repair_evaluations, 0) || !identical(sa$repair_swaps, 0)) fail("a full-rank start was repaired")
      } else {
        if (!(sa$repair_evaluations > 0 && sa$repair_swaps > 0)) fail("a singular start has no repair evaluations")
        swaps <- max(swaps, sa$repair_swaps); evals <- max(evals, sa$repair_evaluations)
        want <- c(criterion(start, lay, v, r, B), criterion(as.integer(B0$treatments), lay, v, r, B))
        got <- sa$repair_log_det
        err <- if (length(got) == 2L && !anyNA(got)) max(abs(got - want)) else Inf
        worst[["criterion"]] <- max(worst[["criterion"]], err)
        if (!(err <= 1e-7)) fail(sprintf("repair criterion %s, dense %s", paste(format(got, digits = 12), collapse = " -> "),
                                         paste(format(want, digits = 12), collapse = " -> ")))
        worst[["score"]] <- max(worst[["score"]], sa$repair_score_error)
      }
    }
    report(length(problems) == 0L && n_full == 8L,
           sprintf("repair %s: full rank %d of 8 (at the start %d), most repair swaps %g, evaluations %g%s", label, n_full,
                   n_start, swaps, evals, if (length(problems)) paste0(": ", problems[1]) else ""))
  }
  report(total[["full"]] == total[["runs"]] && total[["singular"]] >= 1L,
         sprintf("repair: full rank on %d of %d runs; %d starts were singular", total[["full"]], total[["runs"]],
                 total[["singular"]]))
  cat(sprintf("  largest |engine criterion - dense|: %.2e; largest rank-2 score error of an accepted repair swap: %.2e\n",
              worst[["criterion"]], worst[["score"]]))
  cat(failures, "FAIL(s)\n")
  quit(status = if (failures > 0L) 1 else 0)
}

# A-efficiency of one block factor: C = r I - N K^-1 N', canonical efficiency
# factors the nonzero eigenvalues of C / r; 0 when the design is disconnected.
one_way_a <- function(treat, block, v, r) {
  b <- max(block)
  N <- matrix(tabulate((block - 1L) * v + treat, v * b), v, b)
  k <- colSums(N)
  C <- r * diag(v) - N %*% (t(N) / k)
  e <- sort(eigen(C / r, symmetric = TRUE, only.values = TRUE)$values, decreasing = TRUE)[seq_len(v - 1L)]
  if (min(e) < 1e-7) return(0)
  (v - 1) / sum(1 / e)
}

# Baseline Row A constants, keyed "t|k|r|coding". A baseline that cannot be
# read, lacks the columns the floor needs, or holds no constant Row A shape
# stops the check with a FAIL: it would otherwise pass with no floor checked.
baseline_fail <- function(reason) {
  report(FALSE, paste0("stage1 baseline ", baseline_path, ": ", reason))
  cat(failures, "FAIL(s)\n")
  quit(status = 1)
}
baseline_floors <- function(path) {
  need <- function(x, columns, what) {
    absent <- setdiff(columns, names(x))
    if (length(absent)) baseline_fail(paste0(what, " lacks column(s) ", paste(absent, collapse = ", ")))
  }
  summary <- tryCatch({
    if (grepl("\\.rds$", path)) readRDS(path) else {
      records <- utils::read.csv(path)
      need(records, c("caller", "t", "k", "r", "seed", "status", "row_A"), "the recording")
      nested_summary(records)
    }
  }, error = function(e) baseline_fail(paste("cannot be read:", conditionMessage(e))))
  if (!is.data.frame(summary)) baseline_fail("is not a data frame")
  need(summary, c("caller", "t", "k", "r", "seeds", "row_A_sd", "row_A_min"), "the summary")
  summary <- summary[summary$caller %in% nested_onestage_callers, , drop = FALSE]
  constant <- !is.na(summary$row_A_sd) & summary$row_A_sd == 0 & summary$seeds >= 2L &
    !is.na(summary$row_A_min)
  summary <- summary[constant, , drop = FALSE]
  if (nrow(summary) == 0L) {
    baseline_fail("no onestage shape with a constant Row A (SD 0 among at least two ran seeds)")
  }
  coding <- ifelse(summary$caller == "onestage_latinized", "latinized", "nested")
  key <- paste(summary$t, summary$k, summary$r, coding, sep = "|")
  floors <- tapply(summary$row_A_min, key, max)
  stats::setNames(as.numeric(floors), names(floors))
}
spec_floors <- c("48|6|3|nested" = 0.8581470, "24|6|2|nested" = 0.6500942,
                 "30|5|2|nested" = 0.7742182, "24|6|2|latinized" = 0.8697479)
floors <- if (is.null(baseline_path)) numeric() else baseline_floors(baseline_path)
cat("Baseline:", if (is.null(baseline_path)) "none" else baseline_path, "-",
    length(floors), "constant Row A shape(s)\n")

grid <- nested_golden_grid("os")
grid <- unique(grid[grid$t <= tmax, c("t", "k", "r")])
grid <- grid[order(grid$t, grid$k, grid$r), ]
exists_stage1 <- exists("onestage_stage1", envir = asNamespace("FielDHub"), inherits = FALSE)
if (!exists_stage1) {
  report(FALSE, "onestage_stage1() exists in the installed FielDHub")
  quit(status = 1)
}
stage1 <- get("onestage_stage1", envir = asNamespace("FielDHub"))
work_limit <- get("onestage_work_limit", envir = asNamespace("FielDHub"))
checked_floor <- character()
for (g in seq_len(nrow(grid))) {
  t <- grid$t[g]; nrows <- grid$k[g]; r <- grid$r[g]; ncols <- t / nrows
  for (latinize in c(FALSE, TRUE)) {
    coding <- if (latinize) "latinized" else "nested"
    key <- paste(t, nrows, r, coding, sep = "|")
    label <- sprintf("%d/%d/%d %s", t, nrows, r, coding)
    rep_of <- rep(seq_len(r), each = t)
    row_in <- rep(rep(seq_len(nrows), each = ncols), r)
    row_block <- if (latinize) row_in else (rep_of - 1L) * nrows + row_in
    row_a <- rep(NA_real_, n_seeds)
    structure_ok <- TRUE
    problems <- character()
    seconds <- system.time(for (seed in seq_len(n_seeds)) {
      set.seed(seed)
      warned <- character()
      res <- tryCatch(withCallingHandlers(stage1(t, nrows, ncols, r, latinize, work_limit),
                                          warning = function(w) {
                                            warned <<- c(warned, conditionMessage(w))
                                            invokeRestart("muffleWarning")
                                          }),
                      error = function(e) e)
      if (inherits(res, "error")) {
        structure_ok <- FALSE
        problems <- c(problems, sprintf("seed %d error: %s", seed, conditionMessage(res)))
        next
      }
      trt <- res$treatments
      complete <- is.numeric(trt) && length(trt) == t * r && !anyNA(trt) &&
        all(vapply(seq_len(r), function(m) identical(sort(as.integer(trt[rep_of == m])), seq_len(t)),
                   logical(1)))
      if (!complete) {
        structure_ok <- FALSE
        problems <- c(problems, sprintf("seed %d: a replicate does not hold each treatment once", seed))
        next
      }
      if (length(warned)) {
        structure_ok <- FALSE
        problems <- c(problems, sprintf("seed %d warned: %s", seed, warned[1]))
      }
      if (!is.numeric(res$work) || length(res$work) != 1L || is.na(res$work) || res$work < 0) {
        structure_ok <- FALSE
        problems <- c(problems, sprintf("seed %d: work is not one non-negative number", seed))
      }
      row_a[seed] <- one_way_a(as.integer(trt), row_block, t, r)
    })[["elapsed"]]
    report(structure_ok, sprintf("stage1 structure %s (%d seeds)%s", label, n_seeds,
                                 if (length(problems)) paste0(": ", problems[1]) else ""))
    cat(sprintf("  time %s: %.2f s for %d seeds; Row A min %.7f max %.7f\n",
                label, seconds, n_seeds, min(row_a, na.rm = TRUE), max(row_a, na.rm = TRUE)))
    for (source in c("baseline", "spec")) {
      value <- if (source == "baseline") floors[key] else spec_floors[key]
      if (is.null(value) || is.na(value)) next
      below <- sum(is.na(row_a) | row_a < value - TOL)
      checked_floor <- c(checked_floor, paste(source, key))
      report(below == 0L, sprintf("stage1 Row A floor (%s %.7f) %s: %d of %d seeds below, min %.7f",
                                  source, value, label, below, n_seeds, min(row_a, na.rm = TRUE)))
    }
  }
}
# Every constant shape inside the run (t <= tmax) must have been checked.
# The names are selected before pasting: paste("spec", character(0)) is "spec ".
floor_t <- function(x) as.numeric(sub("\\|.*$", "", names(x)))
for (source in c("baseline", "spec")) {
  values <- if (source == "baseline") floors else spec_floors
  if (source == "baseline" && is.null(baseline_path)) next
  due <- names(values)[floor_t(values) <= tmax]
  missing <- setdiff(due, sub(paste0("^", source, " "), "", checked_floor[startsWith(checked_floor, paste0(source, " "))]))
  report(length(missing) == 0L,
         sprintf("every %s constant shape with t <= %g was checked (%d of %d)%s",
                 if (source == "spec") "spec 6.1" else "baseline", tmax,
                 length(due) - length(missing), length(due),
                 if (length(missing)) paste0(": missing ", paste(missing, collapse = ", ")) else ""))
}
cat(failures, "FAIL(s)\n")
quit(status = if (failures > 0L) 1 else 0)
