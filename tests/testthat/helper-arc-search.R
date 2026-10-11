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
