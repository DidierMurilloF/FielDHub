# Hard gate 2, work bound (owner decision 2026-11-02; research M2
# specification section 9.4): after the deadline is observed passed, the
# search performs at most one further operation (an objective or proxy
# evaluation, a proposal, a scan move, a recolouring or a completed random
# start) before it returns. Verified by counting, never by wall time: the
# injected clock makes the deadline pass at a chosen read
# (`arc_work_bound_run()`, helper-arc-search.R) and the operations that
# follow that read are counted.

# Reads at which to let the deadline pass: every `stride`-th read up to the
# natural deadline, plus every read within `window` of each restart start and
# recolouring of an unswitched run (restart and phase boundaries). `args`
# holds further arguments of `arc_work_bound_run()`.
arc_work_bound_points <- function(alg, stride, args = list(), window = 2L) {
  full <- do.call(arc_work_bound_run, c(list(alg, Inf), args))
  last <- max(full$ops$read)
  marks <- full$ops$read[full$ops$kind %in% c("start", "recolour")]
  near <- unlist(lapply(marks, function(m) seq.int(m - window, m + window)))
  points <- sort(unique(c(seq.int(2L, last, by = stride), near, last)))
  list(points = points[points >= 2L & points <= last], full = full)
}

# Sweep the switch read over `points`; expect at most one operation after it
# in every run, and return where each switch landed.
arc_expect_work_bound <- function(alg, stride, args = list()) {
  p <- arc_work_bound_points(alg, stride, args)
  expect_gt(length(p$points), 20L)
  rows <- lapply(p$points, function(n) {
    r <- do.call(arc_work_bound_run, c(list(alg, n), args))
    expect_lte(nrow(r$after), 1L,
               label = sprintf("%s, deadline passed at read %d (%s): %s",
                               alg, n, r$where,
                               paste(r$after$kind, collapse = ", ")))
    expect_true(r$result$status %in% c("ok", "no_design"),
                label = paste(alg, n, r$result$status))
    data.frame(read = n, where = r$where, check = r$check,
               after = nrow(r$after),
               kinds = paste(r$after$kind, collapse = ","),
               stringsAsFactors = FALSE)
  })
  list(sweep = do.call(rbind, rows), full = p$full)
}

arc_work_bound_stride <- function() {
  if (identical(Sys.getenv("FIELDHUB_ARC_SEARCH_SLOW"), "true")) 1L else 6L
}

test_that("the work-bound probe counts every operation and changes nothing", {
  # Unswitched, the probe sees every kind of operation and leaves the seeded
  # result identical to a plain run with the same clock.
  for (alg in c("staged", "multistart", "anneal")) {
    probe <- arc_work_bound_run(alg, Inf)
    reads <- 0L
    plain <- switch(alg,
      staged = arc_search_staged,
      multistart = arc_search_multistart,
      anneal = arc_search_anneal
    )(12, 8, 3, max_evals = Inf, seconds = 0.5, seed = 5, now = function() {
      reads <<- reads + 1L
      reads * 1e-3
    })
    expect_identical(probe$result$design, plain$design, info = alg)
    expect_identical(probe$result$phases, plain$phases, info = alg)
    expect_identical(probe$reads, reads, info = alg)
    counted <- table(factor(probe$ops$kind, c("objective", "proxy")))
    expect_identical(as.numeric(counted[["objective"]]),
                     plain$objective_calls, info = alg)
    expect_identical(as.numeric(counted[["proxy"]]), plain$proxy_calls,
                     info = alg)
    expect_identical(as.numeric(sum(probe$ops$kind == "proposal")),
                     plain$proposals,
                     info = alg)
  }
  staged <- arc_work_bound_run("staged", Inf)
  expect_true(all(c("objective", "proxy", "proposal", "move", "recolour",
                    "start") %in% staged$ops$kind))
  expect_true(all(c("objective", "move", "start") %in%
                    arc_work_bound_run("multistart", Inf)$ops$kind))
})

test_that("staged: at most one operation after the deadline, every phase", {
  w <- arc_expect_work_bound("staged", arc_work_bound_stride())
  # The switch landed in every phase of several restarts (column = proxy
  # phase, row = recolouring loop and row annealing, joint, polish, start,
  # and the restart loop itself), on deadline checks and on other reads.
  for (r in 1:3) {
    expect_true(all(paste0("r", r, ":", c("start", "column", "row", "joint",
                                          "polish", "loop")) %in%
                      w$sweep$where), info = r)
  }
  expect_true(any(w$sweep$check) && any(!w$sweep$check))
  expect_identical(sum(w$sweep$after), 0L)
})

test_that("staged square: one recolouring at most after the deadline", {
  w <- arc_expect_work_bound("staged", arc_work_bound_stride(),
                             list(v = 12, s = 12))
  for (r in 1:3) {
    expect_true(all(paste0("r", r, ":", c("start", "column", "joint",
                                          "polish")) %in% w$sweep$where),
                info = r)
  }
  # A deadline seen in the column phase is followed by the square branch's
  # single recolouring, whose evaluation is then refused.
  expect_true(all(w$sweep$kinds[w$sweep$after == 1L] == "recolour"))
  expect_true(any(w$sweep$after == 1L))
})

test_that("multistart: at most one operation after the deadline", {
  w <- arc_expect_work_bound("multistart", arc_work_bound_stride(),
                             list(v = 8, s = 6, seconds = 1.5))
  expect_gte(sum(w$full$ops$kind == "start"), 3L)
  for (r in 1:3) {
    expect_true(all(paste0("r", r, ":", c("start", "local")) %in%
                      w$sweep$where), info = r)
  }
  expect_identical(sum(w$sweep$after), 0L)
})

test_that("anneal: at most one operation after the deadline", {
  w <- arc_expect_work_bound("anneal", arc_work_bound_stride())
  expect_true(all(c("r1:start", "r1:joint", "r1:polish") %in%
                    w$sweep$where))
  expect_identical(sum(w$sweep$after), 0L)
})
