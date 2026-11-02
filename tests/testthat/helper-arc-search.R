# Test-only helpers for the augmented row-column search engine (research M2
# specification, revision 3; implementation plan tasks 3-8). Counting
# wrappers, an injected clock for deterministic deadlines, and small
# fixtures live here. arc_local_seed() (helper-arc-random.R) restores the
# caller's RNG state.

# Run slow search tests only when FIELDHUB_ARC_SEARCH_SLOW is "true".
arc_skip_unless_search_slow <- function() {
  skip_if_not(identical(Sys.getenv("FIELDHUB_ARC_SEARCH_SLOW"), "true"),
              "Set FIELDHUB_ARC_SEARCH_SLOW=true to run slow search tests.")
}

# Wrap an objective function so calls are counted in $count().
arc_counting_objective <- function(value = 0) {
  n <- 0L
  list(fn = function(...) {
    n <<- n + 1L
    value
  }, count = function() n)
}

# Injected clock: now() reads the time, advance(dt) moves it forward.
arc_fake_clock <- function(start = 0) {
  t <- start
  list(now = function() t,
       advance = function(dt) {
         t <<- t + dt
         invisible(t)
       })
}

# Structural predicates for search-move tests (task 6). A contraction is
# column-binary when no column repeats a label, row-binary (restricted)
# when no row does.
arc_is_column_binary <- function(m) {
  all(apply(m, 2L, anyDuplicated) == 0L)
}
arc_is_row_binary <- function(m) {
  all(apply(m, 1L, anyDuplicated) == 0L)
}
# Column incidence N_C as the sorted contents of each column.
arc_column_sets <- function(m) {
  apply(m, 2L, sort)
}
# Number of columns whose contents differ between two contractions.
arc_changed_columns <- function(a, b) {
  sum(colSums(arc_column_sets(a) != arc_column_sets(b)) > 0L)
}
# A stop function that returns FALSE for its first n calls, then TRUE.
arc_stop_after <- function(n) {
  calls <- 0L
  list(fn = function() {
    calls <<- calls + 1L
    calls > n
  }, calls = function() calls)
}

# Annealing and local-search helpers (task 7). A cheap deterministic score
# of a contraction (no RNG): a weighted sum of its labels by position.
arc_cheap_score <- function(m) {
  sum(m * seq_along(m)) / length(m)
}
# Wrap a score function so every value it returns is recorded.
arc_recording_score <- function(f) {
  values <- numeric(0)
  list(fn = function(x) {
    y <- f(x)
    values <<- c(values, y)
    y
  }, values = function() values)
}

# Search-algorithm helpers (task 8). The three algorithms by name.
arc_search_algorithms <- list(
  multistart = function(...) arc_search_multistart(...),
  anneal = function(...) arc_search_anneal(...),
  staged = function(...) arc_search_staged(...)
)
# Whether a returned design passes the restricted M1 validation (connected).
# The low-df warning is expected for small fixtures and is muffled here.
arc_design_valid <- function(design, v) {
  out <- withCallingHandlers(
    arc_validate_contraction(design, v, "restricted"),
    fieldhub_arc_low_df_warning = function(w) invokeRestart("muffleWarning")
  )
  isTRUE(out$connected)
}

# Work-bound probe for hard gate 2 (owner decision 2026-11-02): after the
# deadline is observed passed, the search performs at most one further
# operation. The injected clock advances `dt` per read and, from read
# `switch_at` on, returns a time far past the deadline, so the deadline
# "passes" at a chosen read. Every search operation is counted through
# namespace wrappers that change neither results nor RNG use: objective and
# proxy evaluations (the score functions), proposals (the three samplers),
# scan moves (each candidate a lazy scan builds and visits, exhaustion checks
# included), recolourings (`arc_recolor_rows()` outside start generation) and
# completed random starts. Counting stops when final verification begins.
# Returns the operations stamped with the clock read they followed, the
# number of reads and, for the read at `switch_at`, where it happened
# (restart and phase) and whether it was a deadline check.
arc_work_bound_run <- function(alg, switch_at, v = 12, s = 8, k = 3,
                               seconds = 0.5, dt = 1e-3, seed = 5, ...) {
  orig <- mget(c("arc_search_score", "arc_column_proxy", "arc_propose_joint",
                 "arc_propose_column", "arc_propose_row",
                 "arc_scan_neighbourhood", "arc_recolor_rows",
                 "arc_random_start", "arc_search_finish"),
               envir = asNamespace("FielDHub"))
  st <- new.env(parent = emptyenv())
  st$reads <- 0L
  st$kind <- character(0)
  st$read <- integer(0)
  st$in_start <- FALSE
  st$finished <- FALSE
  st$where <- NA_character_
  st$check <- NA
  note <- function(kind) {
    if (!st$finished) {
      st$kind <- c(st$kind, kind)
      st$read <- c(st$read, st$reads)
    }
  }
  frame_context <- function() {
    calls <- sys.calls()
    fns <- vapply(calls, function(cl) {
      f <- cl[[1L]]
      if (is.name(f)) as.character(f) else ""
    }, "")
    n <- length(fns)
    check <- n >= 3L && identical(fns[n - 2L], "arc_budget_out")
    get_in <- function(fn, var) {
      i <- utils::tail(which(fns == fn), 1L)
      if (!length(i)) return(NULL)
      get0(var, envir = sys.frame(i), inherits = FALSE)
    }
    restart <- get_in(paste0("arc_search_", alg), "restart")
    phase <- get_in("arc_anneal_phase", "label")
    if (is.null(phase)) phase <- get_in("arc_search_polish", "label")
    phase <- if (!is.null(phase)) {
      sub("[0-9]+$", "", sub("^r[0-9]+:", "", phase))
    } else if (any(fns == "arc_search_start")) {
      "start"
    } else {
      "loop"
    }
    list(where = paste0("r", if (is.null(restart)) 1L else restart, ":",
                        phase), check = check)
  }
  clock <- function() {
    st$reads <- st$reads + 1L
    if (st$reads == switch_at) {
      ctx <- frame_context()
      st$where <- ctx$where
      st$check <- ctx$check
    }
    if (st$reads >= switch_at) 1e9 else st$reads * dt
  }
  local_mocked_bindings(
    arc_search_score = function(...) {
      note("objective")
      orig$arc_search_score(...)
    },
    arc_column_proxy = function(...) {
      note("proxy")
      orig$arc_column_proxy(...)
    },
    arc_propose_joint = function(...) {
      note("proposal")
      orig$arc_propose_joint(...)
    },
    arc_propose_column = function(...) {
      note("proposal")
      orig$arc_propose_column(...)
    },
    arc_propose_row = function(...) {
      note("proposal")
      orig$arc_propose_row(...)
    },
    arc_scan_neighbourhood = function(contraction, kind, visit,
                                      stop_fn = function() FALSE) {
      orig$arc_scan_neighbourhood(contraction, kind, function(x) {
        note("move")
        visit(x)
      }, stop_fn = stop_fn)
    },
    arc_recolor_rows = function(...) {
      if (!st$in_start) note("recolour")
      orig$arc_recolor_rows(...)
    },
    arc_random_start = function(...) {
      st$in_start <- TRUE
      on.exit(st$in_start <- FALSE)
      out <- orig$arc_random_start(...)
      if (!is.null(out)) note("start")
      out
    },
    arc_search_finish = function(...) {
      st$finished <- TRUE
      orig$arc_search_finish(...)
    },
    .package = "FielDHub"
  )
  # Called by name so the clock can find the algorithm's frame.
  res <- switch(alg,
    staged = arc_search_staged(v, s, k, max_evals = Inf, seconds = seconds,
                               seed = seed, now = clock, ...),
    multistart = arc_search_multistart(v, s, k, max_evals = Inf,
                                       seconds = seconds, seed = seed,
                                       now = clock, ...),
    anneal = arc_search_anneal(v, s, k, max_evals = Inf, seconds = seconds,
                               seed = seed, now = clock, ...)
  )
  ops <- data.frame(kind = st$kind, read = st$read, stringsAsFactors = FALSE)
  list(result = res, ops = ops, reads = st$reads,
       after = ops[ops$read >= switch_at, , drop = FALSE],
       where = st$where, check = st$check)
}
