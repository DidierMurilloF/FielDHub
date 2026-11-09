// Test hook for the block-space state (derivation.md sections 4-5).
#include <cpp11.hpp>
#include <vector>
#include "block_space.h"

namespace {
const double kDisconnect = 1e-9;  // spec section 8
}

// Test hook: apply a sequence of swaps (a, p, c, q; 1-based, one per column of
// `swaps`) to a count matrix, returning each ratio before it is applied. Swaps
// with ratio <= 1e-9 are recorded but not applied; swaps that are invalid for the
// current counts get NA and are skipped. refresh_every = 0 never refreshes.
[[cpp11::register]]
cpp11::list block_space_probe_cpp(cpp11::integers counts, int v, int b, int r,
                                  cpp11::integers swaps, int refresh_every) {
  std::vector<int> c(counts.begin(), counts.end());
  BlockSpace bs(v, b, r, c);
  bool ok = bs.refresh();
  int m = swaps.size() / 4;
  cpp11::writable::doubles ratios(m);
  for (int s = 0; s < m; ++s) ratios[s] = NA_REAL;
  int applied = 0;
  for (int s = 0; s < m && ok; ++s) {
    int a = swaps[4 * s] - 1, p = swaps[4 * s + 1] - 1, cc = swaps[4 * s + 2] - 1,
        q = swaps[4 * s + 3] - 1;
    if (a < 0 || a >= v || cc < 0 || cc >= v || p < 0 || p >= b || q < 0 || q >= b ||
        a == cc || p == q || bs.count(a, p) < 1 || bs.count(cc, q) < 1) {
      ratios[s] = NA_REAL;  // not a valid swap for the current counts
      continue;
    }
    double ratio = bs.delta(a, p, cc, q);
    ratios[s] = ratio;
    if (ratio > kDisconnect) {
      bs.apply(a, p, cc, q, ratio);
      ++applied;
      if (refresh_every > 0 && applied % refresh_every == 0) bs.refresh();
    }
  }
  double drift = ok ? bs.drift() : NA_REAL;
  cpp11::writable::integers final_counts(bs.counts().begin(), bs.counts().end());
  using namespace cpp11::literals;
  return cpp11::writable::list({
      "connected"_nm = ok, "ratios"_nm = ratios, "applied"_nm = applied, "counts"_nm = final_counts,
      "log_det"_nm = ok ? bs.log_det() : NA_REAL, "drift"_nm = drift,
      "d_efficiency"_nm = ok ? bs.d_efficiency() : NA_REAL,
      "a_efficiency"_nm = ok ? bs.a_efficiency() : NA_REAL});
}
