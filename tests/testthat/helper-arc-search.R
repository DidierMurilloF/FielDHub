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
