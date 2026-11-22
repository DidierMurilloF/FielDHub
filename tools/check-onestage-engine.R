# Engine checks of the staged onestage row-column search (phase 2 spec 6.1).
# Engine correctness is verified here, not by testthat (constraints: Tests).
#
#   Rscript tools/check-onestage-engine.R
#           <stage1|crossed|repair|treatment_form|report|truncation>
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
# (d) the Row confirmation of the treatment-space form (controller ruling R9,
#     2026-11-22; src/crossed_engine.cpp): a swap between two rows is
#     confirmed with Row values from scratch only when a running Row value is
#     near its floor, with checkpoints, and the guarantee is unchanged.
#     - Same search as with every swap confirmed: on the shapes of (a) and on
#       16/8/4 nested, stage 1 and random starts, seeds 1:4, a full search
#       returns the design, the values and the evaluations of the same call
#       under the hook `confirm_floors` (which confirms every swap, as before
#       the ruling), with `floor_fallback` 0 and no rollback, and never more
#       Row values from scratch (confirmations plus checkpoints); over all
#       runs it needs fewer. The largest distance of a running Row value
#       from the value from scratch, over both runs, must stay below a
#       hundredth of the margins (1e-10 on the log det, 1e-12 relative on
#       the A denominator); it is printed.
#     - Near-floor states, forced with the hook `slack_floors`: on 16/8/4
#       nested from stage 1 starts (seeds 1:8) the search without any
#       confirmation ends one rounding below a Row floor on several seeds
#       (`floor_fallback` 1; at least two are needed), so those searches
#       pass through designs whose Row values tie with the floors. On every
#       such seed the search of the ruling must confirm at least one swap
#       from scratch, and end with `floor_fallback` 0 and the Row floors
#       exact.
#     - The rollback, with the hook `late_floors` (no margin, a checkpoint
#       after every unconfirmed swap) on the same starts: the Row floors hold
#       exactly with `floor_fallback` 0, every result is full rank and not
#       below its start in joint log det, the values equal the dense
#       references, and at least one run met a breach at a checkpoint and
#       rolled back.
#     - At the natural size, with no `treatment_form` hook, from the stage 1
#       design (seed 1) with the work the stage 1 left of the cap; each call
#       must use the treatment-space form, end with `floor_fallback` 0 and no
#       rollback, and keep the Row values at or above those of the start,
#       exactly and by the one-way eigenvalue reference:
#       - 300/2/4 nested, near-floor states: the row step ends at a Row
#         design that no swap can improve, so every swap between rows that
#         the joint stage applies ties the Row values with their floors.
#         Each must be confirmed from scratch (at least one such swap, as
#         many confirmations at least, no checkpoint), and the design is
#         that of the same call under `confirm_floors`;
#       - 1000/500/4 nested, away from the floors: the swaps between rows
#         raise the Row values by more than the margin (measured: none was
#         confirmed), so the call relies on its checkpoints (at least one)
#         and applies at least as many swaps as the same call under
#         `confirm_floors`, which confirms each of its swaps between rows.
# A build without the hook (before Task 8 Step 3b), or without the counts of
# the Row confirmation, gives one FAIL line.
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
# Then the Row and Column reports (block_incidence_efficiency(); ruling
# R10.4) from the Gram matrix of the smaller side against the singular value
# decomposition, on random complete designs of 24/6/2, 9/3/2, 100/2/4,
# 100/50/3, 200/100/4, 600/300/3 and 1002/2/3, nested and shared, seeds 1:2:
# unrounded D and A equal to 1e-10, rounded values identical (same boundary
# exception), and the default form is the Gram matrix exactly where the
# smaller of treatments and blocks is above 500. It prints the seconds of
# report_efficiency() at 2000/1000/4 nested.
#
# truncation: the work cap cuts, it never refuses (spec 6.1 W1-W6; phase 2
# Task 8 Step 3b). [baseline], seeds= and tmax= are ignored.
# Calls. The row step, the column step and the joint call, nested and shared,
# on 24/6/2, 100/5/4, 200/100/4, 500/10/4, 1000/2/4 and 1500/750/3 (the last
# two have calls whose search and start-plus-finish weights differ: modes 1,
# 3 and 4), seeds 1:3, each given
# a room of 1, 1.5 and 3 times the weighted start plus finish of
# onestage_call_work(), as onestage_stage1() and the build give it (the
# engine limit is onestage_engine_limit() of the room). The column step
# starts from the row step's design at the largest room, the joint call from
# the column step's. Per call:
# - it returns a design that is complete in each replicate and scored;
# - the joint call's result is full rank where its start is (by
#   onestage_joint_efficiency()), and not below the Row floors;
# - the engine's reported work is at most its limit, and its weighted work
#   (onestage_weighted_work()) at most the room;
# - joint call: the start formula is at least the engine's start work, the
#   finish formula equals the charged finish, and the charged finish is at
#   least the work of the finish steps the engine counted;
# - the same seed gives the same design twice.
# Builds, through build_row_column_onestage(limits = ) with lowered limits on
# 24/6/2 and 48/6/3, nested and shared, seeds 1:5. At these small caps the
# default shares would cut search 1 itself, so each build gets the shares
# that just hold the stage 1 steps the measurement saw (1.02 times their
# work over the cap):
# - search 2 cut in each of its calls in turn. The work of every call of
#   searches 1 and 2 is measured first; `work` is then set so that search 1
#   runs in full and the call of search 2 that is aimed at gets a room
#   between its start plus finish and what it used. Each build returns a
#   full-rank design, never an error, the same design twice, and reports one
#   complete search, and for each search that entered the return rule the
#   swaps of its joint stage and a joint D gain that is not negative (a
#   call whose work is not above its start plus finish
#   cannot be cut and is skipped; each of the three calls must be cut on at
#   least one seed per shape and coding);
# - a later search whose row step has room but returns no scored design
#   (review finding I-1). With correct formulas that cannot be produced by
#   `work`, so onestage_call_work() is replaced, for this check only, by one
#   whose row-step start is too small: search 2 is dropped, the build returns
#   search 1's design, and search 1 itself treats the step as not started;
# - `startup` below the joint call's need: `joint_stage` "not_started", one
#   search, and the stage 1 design returned; `startup` of one unit: the row
#   and the column step "not_started" as well (the design is then random
#   connected rows, returned if it is full rank, else onestage_infeasible);
# - 9/3/2 nested, `work` from 1e3 to 1e7 on a grid: every build returns a
#   full-rank design or ends in onestage_infeasible, and where the repair of
#   search 1 is cut it reports `repair` "cut" and ends in
#   onestage_infeasible (review finding I-4); that must occur.

arguments <- commandArgs(trailingOnly = TRUE)
named <- grepl("^[a-z]+=", arguments)
opts <- arguments[named]
positional <- arguments[!named]
usage <- "Usage: Rscript tools/check-onestage-engine.R <stage1|crossed|repair|treatment_form|report|truncation> [baseline|-] [library] [seeds=20] [tmax=100]"
if (length(positional) < 1L || length(positional) > 3L ||
    !positional[1] %in% c("stage1", "crossed", "repair", "treatment_form", "report", "truncation")) {
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
              stage1(v, nrows, ncols, r, shared, work_limit, TRUE)), error = function(e) e)
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

  # The Row and Column reports from the Gram matrix of the smaller side
  # (block_incidence_efficiency(gram = TRUE); ruling R10.4) against the
  # singular value decomposition.
  if (!"gram" %in% names(formals(ns$block_incidence_efficiency))) {
    report(FALSE, "block_incidence_efficiency() has the argument `gram`")
  } else {
    one_way <- ns$block_incidence_efficiency
    incidence <- ns$block_incidence
    worst <- 0
    compare <- function(design, default_is_gram) {
      out <- character()
      for (factor_name in c("Level_3", "Level_2")) {
        inc <- incidence(design$treatments, design[[factor_name]])
        a <- one_way(inc, digits = NULL, gram = TRUE)
        b <- one_way(inc, digits = NULL, gram = FALSE)
        err <- max(abs(a$Deffic - b$Deffic), abs(a$Aeffic - b$Aeffic))
        worst <<- max(worst, err)
        if (!(err <= 1e-10)) out <- c(out, sprintf("%s: D %.13f against %.13f, A %.13f against %.13f", factor_name, a$Deffic, b$Deffic, a$Aeffic, b$Aeffic))
        ra <- one_way(inc, gram = TRUE)
        rb <- one_way(inc, gram = FALSE)
        if (!(identical(ra$Deffic, rb$Deffic) || on_boundary(b$Deffic)) || !(identical(ra$Aeffic, rb$Aeffic) || on_boundary(b$Aeffic))) {
          out <- c(out, sprintf("%s: rounded values differ", factor_name))
        }
        default <- one_way(inc)
        if (!identical(default, if (default_is_gram && min(dim(inc)) > 500) ra else rb)) out <- c(out, sprintf("%s: the default is not the expected form", factor_name))
      }
      out
    }
    for (shape in list(c(24, 6, 2), c(9, 3, 2), c(100, 2, 4), c(100, 50, 3), c(200, 100, 4), c(600, 300, 3), c(1002, 2, 3))) {
      v <- shape[1]; nrows <- shape[2]; r <- shape[3]; ncols <- v / nrows
      for (shared in c(FALSE, TRUE)) {
        problems <- character()
        for (seed in 1:2) {
          set.seed(seed)
          design <- frame(as.vector(vapply(seq_len(r), function(m) sample.int(v), integer(v))), v, nrows, ncols, r, shared)
          # A disconnected factor gives 0 by either form; the comparison holds.
          problems <- c(problems, compare(design, TRUE))
        }
        report(length(problems) == 0L,
               sprintf("report, Gram form of the Row and Column reports, %d/%d/%d %s%s", v, nrows, r,
                       if (shared) "latinized" else "nested", if (length(problems)) paste0(": ", problems[1]) else ""))
      }
    }
    cat(sprintf("  largest |D or A, Gram form - decomposition|: %.2e\n", worst))
    set.seed(1)
    v <- 2000; nrows <- 1000; r <- 4
    design <- frame(as.vector(vapply(seq_len(r), function(m) sample.int(v), integer(v))), v, nrows, v / nrows, r, FALSE)
    seconds <- system.time(big <- ns$report_efficiency(design, nested = TRUE, joint = list(Deffic = NA_real_, Aeffic = NA_real_)))[["elapsed"]]
    report(nrow(big) == 4L && !anyNA(big$`A-Efficiency`[1:3]),
           sprintf("report 2000/1000/4 nested, random complete design: Row A %.7f, Column A %.7f", big$`A-Efficiency`[2], big$`A-Efficiency`[3]))
    cat(sprintf("  time of report_efficiency() at 2000/1000/4 nested: %.1f s (by the decomposition: 14.6 s)\n", seconds))
  }
  cat(failures, "FAIL(s)\n")
  quit(status = if (failures > 0L) 1 else 0)
}

if (check == "truncation") {
  ns <- asNamespace("FielDHub")
  needed <- c("onestage_call_work", "onestage_work_weight", "onestage_engine_limit", "onestage_weighted_work",
              "onestage_startable", "onestage_limits",
              "onestage_stage1", "crossed_search", "segmented_blocks", "onestage_joint_efficiency")
  absent <- needed[!vapply(needed, exists, logical(1), envir = ns, inherits = FALSE)]
  if (length(absent)) {
    report(FALSE, paste0(paste(absent, collapse = "(), "), "() exist in the installed FielDHub"))
    cat(failures, "FAIL(s)\n")
    quit(status = 1)
  }
  call_work <- ns$onestage_call_work
  weight_of <- ns$onestage_work_weight
  stage1 <- ns$onestage_stage1
  crossed <- ns$crossed_search
  segmented <- ns$segmented_blocks
  build <- ns$build_row_column_onestage
  joint_eff <- ns$onestage_joint_efficiency
  frame <- ns$onestage_design_frame
  complete <- function(trt, v, r) {
    length(trt) == v * r && !anyNA(trt) &&
      all(vapply(seq_len(r), function(m) identical(sort(as.integer(trt[(m - 1L) * v + seq_len(v)])), seq_len(v)), logical(1)))
  }
  full_rank <- function(trt, v, nrows, r, shared) !is.na(joint_eff(frame(as.integer(trt), v, nrows, v / nrows, r, shared))$Aeffic)
  quietly <- function(expr) tryCatch(suppressWarnings(expr), error = function(e) e)

  # ---- Calls
  factors <- c(1, 1.5, 3)
  for (shape in list(c(24, 6, 2), c(100, 5, 4), c(200, 100, 4), c(500, 10, 4), c(1000, 2, 4), c(1500, 750, 3))) {
    v <- shape[1]; nrows <- shape[2]; r <- shape[3]; ncols <- v / nrows
    rep_of <- rep(seq_len(r), each = v)
    row_in <- rep(rep(seq_len(nrows), each = ncols), r)
    col_in <- rep(seq_len(ncols), nrows * r)
    transposed <- order(rep_of, col_in, row_in)
    for (shared in c(FALSE, TRUE)) {
      label <- sprintf("%d/%d/%d %s", v, nrows, r, if (shared) "latinized" else "nested")
      B_row <- if (shared) nrows else r * nrows
      B_col <- if (shared) ncols else r * ncols
      steps <- list(
        row = list(mode = if (shared) 4L else 1L, B = B_row, work = call_work(if (shared) 4L else 1L, v, r, B_row)),
        column = list(mode = if (shared) 4L else 2L, B = B_col, work = call_work(if (shared) 4L else 2L, v, r, B_col)),
        joint = list(mode = 3L, B = B_row + B_col, work = call_work(3L, v, r, B_row + B_col, B_row)))
      problems <- list(row = character(), column = character(), joint = character())
      seconds <- system.time(for (seed in 1:3) {
        # One call of a stage at a factor of its start plus finish: a list
        # with `treatments`, `scored`, `work` (reported, NA when no engine
        # ran), `limit` (the engine limit) and the raw result.
        run <- function(stage, factor, start) {
          st <- steps[[stage]]
          room <- factor * weight_of(st$mode, v, st$B)[["fixed"]] * sum(st$work)
          limit <- ns$onestage_engine_limit(st$mode, v, r, st$B, room, if (stage == "joint") B_row)
          set.seed(1000 * seed + match(stage, names(steps)))
          out <- quietly(
            if (stage == "joint") crossed(start, v, r, nrows, ncols, shared, work_cap = limit)
            else if (shared && stage == "row") segmented(start, row_in, rep_of, v, r, nrows, work_cap = limit)
            else if (shared) segmented(start, col_in, (rep_of - 1L) * nrows + row_in, v, r, ncols, work_cap = limit)
            else if (stage == "row") ns$blocks_engine(v, r, nrows, resolvable = TRUE, work_cap = limit, admission = FALSE)
            else ns$rows_within_columns(start[transposed], row_in[transposed], v, r, ncols, effort = 1,
                                        work_cap = limit, admission = FALSE))
          if (inherits(out, "error")) return(list(error = conditionMessage(out)))
          if (stage == "joint") {
            list(treatments = as.integer(out$treatments), scored = !is.na(out$row_d) && !is.na(out$row_a),
                 work = out$search$work, limit = limit, raw = out)
          } else if (shared) {
            list(treatments = as.integer(out$treatments), scored = !is.na(out$a_efficiency),
                 work = out$search$work, limit = limit, raw = out)
          } else if (stage == "row") {
            list(treatments = as.integer(out$Design$treatments), scored = !is.na(out$Blocks_model$`A-Efficiency`[2]),
                 work = if (is.null(out$search$work)) NA_real_ else out$search$work, limit = limit, raw = out)
          } else {
            back <- start
            back[transposed] <- out$treatments
            list(treatments = as.integer(back),
                 scored = !is.na(out$a_efficiency) || identical(out$search$stop_reason, "no_swaps"),
                 work = if (is.null(out$search$work)) NA_real_ else out$search$work, limit = limit, raw = out)
          }
        }
        set.seed(seed)
        start <- as.vector(vapply(seq_len(r), function(m) sample.int(v), integer(v)))
        for (stage in names(steps)) {
          st <- steps[[stage]]
          fail <- function(what) problems[[stage]] <<- c(problems[[stage]], sprintf("seed %d, %g x: %s", seed, f, what))
          last <- NULL
          for (f in factors) {
            a <- run(stage, f, start)
            if (!is.null(a$error)) { fail(paste("error:", a$error)); next }
            b <- run(stage, f, start)
            if (!complete(a$treatments, v, r)) { fail("a replicate is not complete"); next }
            if (!a$scored) { fail("no scored design"); next }
            if (!identical(a$treatments, b$treatments)) fail("the same seed gave another design")
            if (!is.na(a$work) && !(a$work <= a$limit)) {
              fail(sprintf("reported work %.6g above the limit %.6g", a$work, a$limit))
            }
            if (!is.na(a$work)) {
              room <- f * weight_of(st$mode, v, st$B)[["fixed"]] * sum(st$work)
              used <- ns$onestage_weighted_work(st$mode, v, r, st$B, a$work, if (stage == "joint") B_row)
              if (!(used <= room * (1 + 1e-12))) fail(sprintf("weighted work %.6g above the room %.6g", used, room))
            }
            if (stage == "joint") {
              sr <- a$raw$search
              if (!(a$raw$row_d >= a$raw$stage1_row_d && a$raw$row_a >= a$raw$stage1_row_a)) fail("below the Row floors")
              if (full_rank(start, v, nrows, r, shared) && !(isTRUE(a$raw$full_rank) && full_rank(a$treatments, v, nrows, r, shared))) {
                fail("a full-rank start returned not full rank")
              }
              if (!(st$work[["start"]] >= sr$start_work)) fail(sprintf("start formula %.6g below the engine's %.6g", st$work[["start"]], sr$start_work))
              if (!isTRUE(all.equal(st$work[["finish"]], sr$finish_charged, tolerance = 1e-12))) {
                fail(sprintf("finish formula %.6g, charged %.6g", st$work[["finish"]], sr$finish_charged))
              }
              if (!(sr$finish_charged >= sr$finish_work)) fail(sprintf("charged finish %.6g below the counted %.6g", sr$finish_charged, sr$finish_work))
            }
            last <- a
          }
          if (!is.null(last)) start <- last$treatments
        }
      })[["elapsed"]]
      for (stage in names(steps)) {
        report(length(problems[[stage]]) == 0L,
               sprintf("truncation call, %s %s (start %.3g, finish %.3g; 3 seeds x 3 rooms)%s", label, stage,
                       steps[[stage]]$work[["start"]], steps[[stage]]$work[["finish"]],
                       if (length(problems[[stage]])) paste0(": ", problems[[stage]][1]) else ""))
      }
      cat(sprintf("  %s: %.1f s\n", label, seconds))
    }
  }

  # ---- Builds with lowered limits
  base <- ns$onestage_limits()
  base$row_share <- 0.9
  base$column_share <- 0.05
  lowered <- function(...) utils::modifyList(base, list(...))
  # Limits with the cap `work` and shares that hold the measured steps.
  fitted <- function(work, used, ...) {
    lowered(work = work, row_share = 1.02 * max(used[[1]][["row"]], used[[2]][["row"]]) / work,
            column_share = 1.02 * max(used[[1]][["column"]], used[[2]][["column"]]) / work, ...)
  }
  run_build <- function(v, nrows, r, shared, searches, seed, limits) {
    tryCatch(suppressWarnings(build(v, nrows, v / nrows, r, shared, searches, seed, limits = limits)),
             error = function(e) e)
  }
  # The weighted work of every call of searches 1 and 2 with no limit binding.
  measure <- function(v, nrows, r, shared, seed) {
    ncols <- v / nrows
    B_row <- if (shared) nrows else r * nrows
    B <- B_row + if (shared) ncols else r * ncols
    set.seed(seed)
    left <- base$work
    out <- list()
    for (s in 1:2) {
      s1 <- stage1(v, nrows, ncols, r, shared, left, s == 1L, base)
      left <- left - s1$work
      j <- crossed(s1$treatments, v, r, nrows, ncols, shared,
                   work_cap = ns$onestage_engine_limit(3L, v, r, B, left, B_row))
      used <- ns$onestage_weighted_work(3L, v, r, B, j$search$work, B_row)
      left <- left - used
      out[[s]] <- c(row = s1$step_work[["row"]], column = s1$step_work[["column"]], joint = used)
    }
    out
  }
  for (shape in list(c(24, 6, 2), c(48, 6, 3))) {
    v <- shape[1]; nrows <- shape[2]; r <- shape[3]; ncols <- v / nrows
    for (shared in c(FALSE, TRUE)) {
      label <- sprintf("%d/%d/%d %s", v, nrows, r, if (shared) "latinized" else "nested")
      B_row <- if (shared) nrows else r * nrows
      B_col <- if (shared) ncols else r * ncols
      need <- c(row = sum(call_work(if (shared) 4L else 1L, v, r, B_row)) * weight_of(if (shared) 4L else 1L, v, B_row)[["fixed"]],
                column = sum(call_work(if (shared) 4L else 2L, v, r, B_col)) * weight_of(if (shared) 4L else 2L, v, B_col)[["fixed"]],
                joint = sum(call_work(3L, v, r, B_row + B_col, B_row)) * weight_of(3L, v, B_row + B_col)[["fixed"]])
      problems <- character()
      hit <- c(row = 0L, column = 0L, joint = 0L)
      for (seed in 1:5) {
        used <- measure(v, nrows, r, shared, seed)
        before <- c(row = 0, column = used[[2]][["row"]], joint = used[[2]][["row"]] + used[[2]][["column"]])
        for (target in names(need)) {
          if (!(used[[2]][[target]] > need[[target]] * 1.02)) next
          work <- sum(used[[1]]) + before[[target]] + (need[[target]] + used[[2]][[target]]) / 2
          lim <- fitted(work, used)
          if (!ns$onestage_startable(3L, v, r, B_row + B_col, limits = lim, B_row = B_row)) next
          a <- run_build(v, nrows, r, shared, 40, seed, lim)
          b <- run_build(v, nrows, r, shared, 40, seed, lim)
          tag <- sprintf("seed %d, search 2 cut in its %s call", seed, target)
          if (inherits(a, "error")) { problems <- c(problems, paste0(tag, ": error ", conditionMessage(a))); next }
          info <- attr(a, "onestage_search")
          if (!identical(a, b)) problems <- c(problems, paste0(tag, ": the same seed gave another design"))
          else if (is.na(attr(a, "joint_efficiency")$Aeffic)) problems <- c(problems, paste0(tag, ": not full rank"))
          else if (length(info$joint_swaps) != info$started || length(info$joint_gain) != info$started ||
                   anyNA(info$joint_gain) || any(info$joint_swaps < 0) || any(info$joint_gain < -1e-12)) {
            problems <- c(problems, paste0(tag, ": joint swaps or joint D gain of the searches missing or negative"))
          }
          else if (info$complete != 1L || !info$started %in% 1:2 || (target == "joint" && info$started != 2L)) {
            problems <- c(problems, sprintf("%s: %d searches started, %d complete", tag, info$started, info$complete))
          } else hit[[target]] <- hit[[target]] + 1L
        }
      }
      report(length(problems) == 0L && all(hit >= 1L),
             sprintf("truncation build, %s: search 2 cut in its row, column and joint call on %s of 5 seeds%s", label,
                     paste(hit, collapse = ", "), if (length(problems)) paste0(": ", problems[1]) else ""))

      # A row step that has room but returns no scored design (the formula of
      # its start replaced by one unit, for this check only).
      real <- ns$onestage_call_work
      broken <- local({
        blocks <- B_row
        function(mode, v, r, B, B_row = NULL) {
          work <- real(mode, v, r, B, B_row)
          if (mode %in% c(1L, 4L) && B == blocks) work[["start"]] <- 1
          work
        }
      })
      swap_in <- function(f) {
        unlockBinding("onestage_call_work", ns)
        assign("onestage_call_work", f, envir = ns)
        lockBinding("onestage_call_work", ns)
      }
      problems <- character()
      for (seed in 1:5) {
        used <- measure(v, nrows, r, shared, seed)
        finish <- real(if (shared) 4L else 1L, v, r, B_row)[["finish"]]
        # Search 2's row step gets its finish plus a few units: room for the
        # broken formula, far too little to score a start.
        lim <- fitted(sum(used[[1]]) + finish + 10, used)
        whole <- run_build(v, nrows, r, shared, 20, seed, lowered())
        swap_in(broken)
        dropped <- run_build(v, nrows, r, shared, 40, seed, lim)
        first <- run_build(v, nrows, r, shared, 20, seed, lowered(work = (finish + 10) / base$row_share))
        swap_in(real)
        tag <- sprintf("seed %d", seed)
        if (inherits(dropped, "error")) { problems <- c(problems, paste0(tag, ": error ", conditionMessage(dropped))); next }
        info <- attr(dropped, "onestage_search")
        if (info$started != 1L || !identical(as.integer(dropped$treatments), as.integer(whole$treatments))) {
          problems <- c(problems, sprintf("%s: %d searches started, or not search 1's design", tag, info$started))
        }
        # In search 1 the same defect is a step that is not started.
        if (inherits(first, "error")) {
          if (!inherits(first, "onestage_infeasible")) problems <- c(problems, paste0(tag, ": error ", conditionMessage(first)))
        } else if (!identical(attr(first, "onestage_search")$row_step, "not_started")) {
          problems <- c(problems, paste0(tag, ": search 1 did not treat the step as not started"))
        }
      }
      report(length(problems) == 0L,
             sprintf("truncation build, %s: a row step with room and no scored design drops search 2, and is not started in search 1%s",
                     label, if (length(problems)) paste0(": ", problems[1]) else ""))

      # The backstop: calls that are not startable.
      problems <- character()
      outcomes <- character()
      for (seed in 1:5) {
        tag <- sprintf("seed %d", seed)
        lim <- lowered(startup = need[["joint"]] * 0.99)
        a <- run_build(v, nrows, r, shared, 200, seed, lim)
        set.seed(seed)
        s1 <- quietly(stage1(v, nrows, ncols, r, shared, lim$work, TRUE, lim))
        if (inherits(a, "error")) {
          info <- a$search
          if (!inherits(a, "onestage_infeasible") || full_rank(s1$treatments, v, nrows, r, shared)) {
            problems <- c(problems, paste0(tag, ": error ", conditionMessage(a)))
          }
        } else {
          info <- attr(a, "onestage_search")
          if (!identical(as.integer(a$treatments), as.integer(s1$treatments))) problems <- c(problems, paste0(tag, ": not the stage 1 design"))
        }
        if (!identical(info$joint_stage, "not_started") || info$started != 1L || info$complete != 0L) {
          problems <- c(problems, sprintf("%s: joint stage %s, %s searches", tag, format(info$joint_stage), format(info$started)))
        }
        none <- run_build(v, nrows, r, shared, 200, seed, lowered(startup = 1))
        info <- if (inherits(none, "error")) none$search else attr(none, "onestage_search")
        if (inherits(none, "error") && !inherits(none, "onestage_infeasible")) {
          problems <- c(problems, paste0(tag, ": error ", conditionMessage(none)))
        } else if (!identical(c(info$row_step, info$column_step, info$joint_stage), rep("not_started", 3L))) {
          problems <- c(problems, paste0(tag, ": a stage started with a start-up limit of one unit"))
        } else if (!inherits(none, "error") && !(complete(as.integer(none$treatments), v, r) &&
                                                  full_rank(none$treatments, v, nrows, r, shared))) {
          problems <- c(problems, paste0(tag, ": the design returned is not complete and full rank"))
        }
        outcomes <- c(outcomes, if (inherits(none, "error")) "infeasible" else "design")
      }
      report(length(problems) == 0L,
             sprintf("truncation build, %s: joint stage not started below its start-up need; nothing started at one unit (%s)%s",
                     label, paste(outcomes, collapse = " "), if (length(problems)) paste0(": ", problems[1]) else ""))
    }
  }
  # A repair cut in search 1
  problems <- character()
  cuts <- 0L; designs <- 0L
  for (seed in 1:5) {
    for (work in 10^seq(3, 7, length.out = 65)) {
      a <- run_build(9, 3, 2, FALSE, 20, seed, lowered(work = work, row_share = 0.45, column_share = 0.1))
      tag <- sprintf("seed %d, work %.3g", seed, work)
      if (inherits(a, "error")) {
        if (!inherits(a, "onestage_infeasible")) { problems <- c(problems, paste0(tag, ": error ", conditionMessage(a))); next }
        if (identical(a$search$repair, "cut")) cuts <- cuts + 1L
      } else {
        designs <- designs + 1L
        info <- attr(a, "onestage_search")
        if (identical(info$repair, "cut")) problems <- c(problems, paste0(tag, ": a design after a cut repair"))
        if (!full_rank(a$treatments, 9, 3, 2, FALSE)) problems <- c(problems, paste0(tag, ": not full rank"))
      }
    }
  }
  report(length(problems) == 0L && cuts >= 1L && designs >= 1L,
         sprintf("truncation build, 9/3/2 nested: %d builds ended in onestage_infeasible after a cut repair, %d returned a full-rank design%s",
                 cuts, designs, if (length(problems)) paste0(": ", problems[1]) else ""))
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
      s1 <- stage1(v, nrows, ncols, r, shared, work_limit, TRUE)
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
      s1 <- stage1(v, nrows, ncols, r, FALSE, work_limit, TRUE)
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
  s1 <- stage1(v, nrows, ncols, r, FALSE, work_limit, TRUE)
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
  # An installed build without the hook (other arguments, no `check` list),
  # or without the counts of the Row confirmation, fails here with one line
  # instead of an R error further down.
  probe <- tryCatch({
    set.seed(1)
    s1 <- stage1(12, 2, 6, 3, FALSE, work_limit, TRUE)
    crossed(s1$treatments, 12, 3, 2, 6, FALSE, budget = 0, work_cap = work_limit)
  }, error = function(e) e)
  if (inherits(probe, "error") || !used_form(probe) || is.null(probe$search$check$row_floors) ||
      !exists("segmented_blocks", envir = ns, inherits = FALSE) ||
      !"admission" %in% names(formals(get("blocks_engine", envir = ns)))) {
    report(FALSE, paste0("the installed FielDHub has the treatment_form check hook and reports the Row confirmation",
                         if (inherits(probe, "error")) paste0(": ", conditionMessage(probe)) else ""))
    cat(failures, "FAIL(s)\n")
    quit(status = 1)
  }
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
          s1 <- stage1(v, nrows, ncols, r, FALSE, work_limit, TRUE)
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

  # (d) the Row confirmation of ruling R9
  hooked <- function(hook, expr) {
    Sys.setenv(FIELDHUB_ENGINE_DEBUG = hook)
    on.exit(Sys.setenv(FIELDHUB_ENGINE_DEBUG = "1,treatment_form"))
    expr
  }
  starts_of <- function(v, nrows, r, seeds, kinds = c("stage 1", "random")) {
    out <- list()
    for (seed in seeds) for (kind in kinds) {
      set.seed(seed)
      start <- if (kind == "stage 1") as.integer(stage1(v, nrows, v / nrows, r, FALSE, work_limit, TRUE)$treatments)
               else as.vector(vapply(seq_len(r), function(m) sample.int(v), integer(v)))
      out[[length(out) + 1L]] <- list(tag = sprintf("%d/%d/%d seed %d, %s start", v, nrows, r, seed, kind),
                                      start = start, state = get(".Random.seed", envir = globalenv()))
    }
    out
  }
  search_from <- function(s, v, nrows, r) {
    assign(".Random.seed", s$state, envir = globalenv())
    crossed(s$start, v, r, nrows, v / nrows, FALSE, work_cap = work_limit)
  }
  floors_of <- function(res) res$search$check$row_floors
  problems <- character()
  fresh <- c(lazy = 0, all = 0)
  drift <- c(ld = 0, sum = 0)
  runs <- 0L
  for (shape in c(shapes, list(c(16, 8, 4)))) {
    v <- shape[1]; nrows <- shape[2]; r <- shape[3]
    lay <- layout_of(v, r, nrows, FALSE)
    for (s in starts_of(v, nrows, r, 1:4)) {
      lazy <- search_from(s, v, nrows, r)
      all <- hooked("1,treatment_form,confirm_floors", search_from(s, v, nrows, r))
      runs <- runs + 1L
      fl <- floors_of(lazy); fa <- floors_of(all)
      same <- c("treatments", "full_rank", "row_d", "row_a", "joint_d", "joint_a", "joint_log_det")
      vp <- value_problem(lazy, s$start, lay, v, r)
      if (!is.null(vp)) problems <- c(problems, paste0(s$tag, ": ", vp))
      else if (!identical(lazy[same], all[same]) || !identical(lazy$search$evaluations, all$search$evaluations)) {
        problems <- c(problems, paste0(s$tag, ": not the search of `confirm_floors`"))
      } else if (fl[["rollbacks"]] > 0 || fa[["checkpoints"]] > 0 || fa[["confirmations"]] < fa[["swaps"]] ||
                 fl[["confirmations"]] + fl[["checkpoints"]] > fa[["confirmations"]]) {
        problems <- c(problems, sprintf("%s: %g confirmations, %g checkpoints, %g rollbacks; `confirm_floors` %g confirmations of %g swaps",
                                        s$tag, fl[["confirmations"]], fl[["checkpoints"]], fl[["rollbacks"]],
                                        fa[["confirmations"]], fa[["swaps"]]))
      }
      fresh <- fresh + c(fl[["confirmations"]] + fl[["checkpoints"]], fa[["confirmations"]])
      drift <- pmax(drift, c(fl[["drift_ld"]], fl[["drift_sum"]]), c(fa[["drift_ld"]], fa[["drift_sum"]]))
    }
  }
  report(length(problems) == 0L && fresh[["lazy"]] < fresh[["all"]] && all(drift <= c(1e-10, 1e-12)),
         sprintf("treatment form, Row confirmation: %d full searches equal those of `confirm_floors`, with %g Row values from scratch against %g; largest drift of a running Row value %.2e (log det), %.2e (A denominator, relative)%s",
                 runs, fresh[["lazy"]], fresh[["all"]], drift[["ld"]], drift[["sum"]],
                 if (length(problems)) paste0(": ", problems[1]) else ""))
  # Near-floor states (hook `slack_floors`) and the rollback (hook `late_floors`)
  v <- 16; nrows <- 8; r <- 4
  lay <- layout_of(v, r, nrows, FALSE)
  problems <- character()
  late_problems <- character()
  below <- 0L; rolled <- 0L
  for (s in starts_of(v, nrows, r, 1:8, "stage 1")) {
    slack <- hooked("treatment_form,slack_floors", search_from(s, v, nrows, r))
    lazy <- search_from(s, v, nrows, r)
    late <- hooked("1,treatment_form,late_floors", search_from(s, v, nrows, r))
    if (identical(as.integer(slack$search$floor_fallback), 1L)) {
      below <- below + 1L
      vp <- value_problem(lazy, s$start, lay, v, r)
      if (!is.null(vp)) problems <- c(problems, paste0(s$tag, ": ", vp))
      else if (!(floors_of(lazy)[["confirmations"]] >= 1)) problems <- c(problems, paste0(s$tag, ": no swap was confirmed"))
    }
    vp <- value_problem(late, s$start, lay, v, r)
    ref0 <- joint_ref(s$start, lay, v, r)
    if (!is.null(vp)) late_problems <- c(late_problems, paste0(s$tag, ": ", vp))
    else if (ref0$full && !(isTRUE(late$full_rank) && late$joint_log_det >= ref0$log_det - VAL)) {
      late_problems <- c(late_problems, paste0(s$tag, ": not a full-rank design at or above the start"))
    }
    rolled <- rolled + (floors_of(late)[["rollbacks"]] > 0)
  }
  report(length(problems) == 0L && below >= 2L,
         sprintf("treatment form, Row confirmation near a floor (16/8/4 nested, 8 seeds): %d searches end below a floor without confirmation (need >= 2), each confirmed a swap and kept the floors%s",
                 below, if (length(problems)) paste0(": ", problems[1]) else ""))
  report(length(late_problems) == 0L && rolled >= 1L,
         sprintf("treatment form, rollback after a breach at a checkpoint (hook, 16/8/4 nested, 8 seeds): %d runs rolled back (need >= 1), floors kept%s",
                 rolled, if (length(late_problems)) paste0(": ", late_problems[1]) else ""))
  # At the natural size, no `treatment_form` hook
  for (shape in list(c(300, 2, 4), c(1000, 500, 4))) {
    v <- shape[1]; nrows <- shape[2]; r <- shape[3]
    near <- nrows == 2
    lay <- layout_of(v, r, nrows, FALSE)
    natural <- hooked("seconds", {
      set.seed(1)
      s1 <- stage1(v, nrows, v / nrows, r, FALSE, work_limit, TRUE)
      state <- get(".Random.seed", envir = globalenv())
      lazy <- crossed(s1$treatments, v, r, nrows, v / nrows, FALSE, work_cap = work_limit - s1$work)
      list(start = as.integer(s1$treatments), lazy = lazy, all = hooked("seconds,confirm_floors", {
        assign(".Random.seed", state, envir = globalenv())
        crossed(s1$treatments, v, r, nrows, v / nrows, FALSE, work_cap = work_limit - s1$work)
      }))
    })
    fl <- floors_of(natural$lazy); fa <- floors_of(natural$all)
    trt <- as.integer(natural$lazy$treatments)
    fp <- floor_problem(natural$lazy, trt, natural$start, lay, v, r)
    why <- if (!used_form(natural$lazy)) "the treatment-space form was not used"
           else if (!complete_reps(trt, lay, v, r) || !isTRUE(natural$lazy$full_rank)) "not a complete full-rank design"
           else if (!is.null(fp)) fp
           else if (fl[["rollbacks"]] > 0) "a rollback"
           else if (!(fl[["swaps"]] >= 1)) "no swap between rows was applied"
           else if (fa[["confirmations"]] < fa[["swaps"]]) "`confirm_floors` left a swap unconfirmed"
           else if (near && !(fl[["confirmations"]] >= fl[["swaps"]] && fl[["checkpoints"]] == 0)) "a swap that ties with the floors was not confirmed"
           else if (near && !identical(trt, as.integer(natural$all$treatments))) "not the design of `confirm_floors`"
           else if (!near && !(fl[["checkpoints"]] >= 1 && fl[["confirmations"]] < fl[["swaps"]])) "no checkpoint, or every swap confirmed"
           else if (!(natural$lazy$search$check$accepted >= natural$all$search$check$accepted)) "fewer swaps than with every swap confirmed"
           else NULL
    report(is.null(why),
           sprintf("treatment form, Row confirmation at %d/%d/%d nested (%s): %g swaps (%g with every swap confirmed), %g between rows, %g confirmed, %g checkpoints; Row D %.3g and Row A %.3g above the floors; drift %.2e, %.2e%s",
                   v, nrows, r, if (near) "ties with the floors" else "away from the floors",
                   natural$lazy$search$check$accepted, natural$all$search$check$accepted, fl[["swaps"]],
                   fl[["confirmations"]], fl[["checkpoints"]], natural$lazy$row_d - natural$lazy$stage1_row_d,
                   natural$lazy$row_a - natural$lazy$stage1_row_a, fl[["drift_ld"]], fl[["drift_sum"]],
                   if (is.null(why)) "" else paste0("; PROBLEM ", why)))
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
      s1 <- stage1(v, nrows, ncols, r, shared, work_limit, TRUE)
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
      res <- tryCatch(withCallingHandlers(stage1(t, nrows, ncols, r, latinize, work_limit, TRUE),
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
