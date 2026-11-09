// Test hook for the engine's random draws (spec section 6).
#include <cpp11.hpp>
#include <R_ext/Random.h>
#include <vector>
#include "engine_rng.h"

// m draws from draw_index(n), 1-based, bracketed by GetRNGstate()/PutRNGstate()
// exactly as the engine brackets its search.
[[cpp11::register]]
cpp11::integers engine_draw_indices_cpp(int n, int m) {
  std::vector<int> d(m);
  GetRNGstate();
  for (int i = 0; i < m; ++i) d[i] = draw_index(n) + 1;
  PutRNGstate();
  return cpp11::writable::integers(d.begin(), d.end());
}
