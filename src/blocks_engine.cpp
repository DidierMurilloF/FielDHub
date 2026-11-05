// Single-level block design search (spec section 9; Edmondson 2020, sections 2.2-2.3).
// Randomized first-improvement plot swaps scored with the block-space ratio.
#include <cpp11.hpp>
#include <R_ext/Random.h>
#include <algorithm>
#include <cmath>
#include <memory>
#include <numeric>
#include <vector>
#include "block_space.h"
#include "engine_budget.h"
#include "engine_rng.h"

namespace {

const double kTie = 1e-10;        // spec section 8: accept if ratio > 1 + 1e-10 (relative, below)
const int kRefreshEvery = 256;    // spec section 8: refresh interval, halved after a drift alarm
const int kRefreshMin = 16;       // smallest interval after repeated drift alarms
const double kDriftAlarm = 1e-8;  // spec section 8: drift alarm threshold
const double kExhaustivePairs = 2000;  // exhaustive passes only when n(n-1)/2 <= 2000

struct Layout {
  int v, r, b, n;
  std::vector<int> block_of;  // plot -> block, plots ordered by block
};

Layout make_layout(int v, int r, int b) {
  Layout lay{v, r, b, v * r, std::vector<int>()};
  lay.block_of.reserve(lay.n);
  for (int j = 0; j < b; ++j) {
    int k = lay.n / b + (j < lay.n % b ? 1 : 0);
    for (int x = 0; x < k; ++x) lay.block_of.push_back(j);
  }
  return lay;
}

std::vector<int> counts_of(const Layout& lay, const std::vector<int>& treat) {
  std::vector<int> c(static_cast<size_t>(lay.v) * lay.b, 0);
  for (int x = 0; x < lay.n; ++x) c[treat[x] + lay.v * lay.block_of[x]] += 1;
  return c;
}

struct Search {
  std::vector<int> best_treat;
  double best_log_det = -INFINITY;
  double refreshes = 0, drift_events = 0, repairs = 0;
  bool debug = false;           // spec section 8: FIELDHUB_ENGINE_DEBUG=1
  double debug_max_error = 0;   // max relative error of an accepted ratio
};

// Number of connected components of the treatment-block incidence graph
// (v + b nodes, an edge where a count is positive). C has rank v - 1 exactly
// when this graph is connected.
int components(const Layout& lay, const std::vector<int>& treat) {
  std::vector<int> parent(lay.v + lay.b);
  std::iota(parent.begin(), parent.end(), 0);
  auto find = [&parent](int x) {
    while (parent[x] != x) { parent[x] = parent[parent[x]]; x = parent[x]; }
    return x;
  };
  int count = lay.v + lay.b;
  for (int x = 0; x < lay.n; ++x) {
    int a = find(treat[x]), c = find(lay.v + lay.block_of[x]);
    if (a != c) { parent[a] = c; --count; }
  }
  return count;
}

// Draw one random swap; returns false if the pair is not a valid swap.
bool draw_swap(const Layout& lay, const std::vector<int>& treat, int& x, int& y) {
  x = draw_index(lay.n);
  y = draw_index(lay.n);
  return lay.block_of[x] != lay.block_of[y] && treat[x] != treat[y];
}

// Dense work of one refresh or drift check (Cholesky, triangular inverse and
// product, about b^3 multiply-adds) plus the sparse N'N build.
double refresh_work(const Layout& lay) {
  return static_cast<double>(lay.b) * lay.b * lay.b + static_cast<double>(lay.n) * lay.r;
}

// Refresh with the time and interrupt checks the dense work needs. False if
// the time limit stops the run first or Bhat is not positive definite.
bool checked_refresh(BlockSpace& bs, const Layout& lay, Budget& budget, Search& out) {
  if (!budget.work(refresh_work(lay))) return false;
  if (!bs.refresh()) return false;
  out.refreshes += 1;
  return true;
}

// Apply an accepted swap. Returns false if a scheduled refresh fails or the
// time limit stops the run, which ends the search.
bool apply_swap(BlockSpace& bs, const Layout& lay, std::vector<int>& treat, int x, int y,
                double ratio, int& accepted, int& interval, Search& out, Budget& budget) {
  double before = 0.0;
  if (out.debug) {
    BlockSpace fresh(lay.v, lay.b, lay.r, bs.counts());
    if (fresh.refresh()) before = fresh.log_det();
  }
  bs.apply(treat[x], lay.block_of[x], treat[y], lay.block_of[y], ratio);
  std::swap(treat[x], treat[y]);
  if (out.debug) {
    BlockSpace fresh(lay.v, lay.b, lay.r, bs.counts());
    if (fresh.refresh()) {
      double exact = std::exp(fresh.log_det() - before);
      double e = std::fabs(ratio - exact) / std::max(1.0, exact);
      if (e > out.debug_max_error) out.debug_max_error = e;
    }
  }
  if (!budget.work(4.0 * lay.b * lay.b)) return false;  // the rank-2 update
  if (++accepted % interval == 0) {
    if (!budget.spend(static_cast<double>(lay.n + lay.b))) return true;
    // Spec section 8: a drift alarm triggers a refresh (done here at every
    // check) and halves the interval, so a poorly conditioned state is
    // refreshed more often.
    if (bs.drift() > kDriftAlarm) {
      out.drift_events += 1;
      interval = std::max(kRefreshMin, interval / 2);
    }
    if (!checked_refresh(bs, lay, budget, out)) return false;
  }
  return true;
}

// Improvement test, relative to the size of the two terms whose difference is
// the ratio (derivation section 8, eps_rel): a gain inside their rounding
// error is not accepted.
bool improves(double ratio, double scale) { return ratio > 1.0 + kTie * scale; }

// Random swaps that never increase the number of components, until the
// design is connected. Each check is charged one evaluation, like a swap
// evaluation; its O(n + v + b) union-find is recorded as work. Returns true if
// connected.
bool repair_connectivity(const Layout& lay, std::vector<int>& treat, Budget& budget, Search& out) {
  int comp = components(lay, treat);
  for (double tries = 0; comp > 1 && tries < 50.0 * lay.n; ++tries) {
    if (!budget.spend(1.0) || !budget.work(static_cast<double>(lay.n + lay.v + lay.b))) return false;
    int x, y;
    if (!draw_swap(lay, treat, x, y)) continue;
    std::swap(treat[x], treat[y]);
    int next = components(lay, treat);
    if (next <= comp) { comp = next; out.repairs += 1; } else { std::swap(treat[x], treat[y]); }
  }
  return comp == 1;
}

// One escape: jumps_per_escape random swaps that keep the design connected,
// checked exactly with union-find (a ratio test is unreliable for weakly
// connected designs), applied to the counts only and followed by one refresh.
// Each draw is charged one evaluation. Returns false if the search must end.
bool escape(BlockSpace& bs, const Layout& lay, std::vector<int>& treat, int jumps_per_escape,
            Budget& budget, Search& out) {
  int x, y;
  for (int jump = 0; jump < jumps_per_escape && !budget.stopped();) {
    if (!budget.spend(1.0)) break;
    if (!draw_swap(lay, treat, x, y)) continue;
    std::swap(treat[x], treat[y]);
    if (!budget.work(static_cast<double>(lay.n + lay.v + lay.b))) { std::swap(treat[x], treat[y]); break; }
    if (components(lay, treat) != 1) { std::swap(treat[x], treat[y]); continue; }
    bs.move(treat[y], lay.block_of[x], treat[x], lay.block_of[y]);
    ++jump;
  }
  // The counts may have changed even if the budget stopped mid-escape.
  return checked_refresh(bs, lay, budget, out);
}

void run_search(const Layout& lay, int jumps_per_escape, int max_stale_escapes,
                Budget& budget, Search& out) {
  std::vector<int> treat(lay.n);
  for (int x = 0; x < lay.n; ++x) treat[x] = x / lay.r;  // r copies of each treatment
  std::unique_ptr<BlockSpace> bs;
  for (int attempt = 0; attempt < 10 && !bs; ++attempt) {
    if (!budget.spend(static_cast<double>(lay.n + lay.b))) return;
    for (int x = lay.n - 1; x > 0; --x) std::swap(treat[x], treat[draw_index(x + 1)]);
    if (!repair_connectivity(lay, treat, budget, out)) continue;
    bs.reset(new BlockSpace(lay.v, lay.b, lay.r, counts_of(lay, treat)));
    if (!budget.work(refresh_work(lay)) || !bs->refresh()) bs.reset();
  }
  if (!bs) return;  // no connected start within 10 repaired shuffles
  int accepted = 0, stale = 0, interval = kRefreshEvery;
  double local_best = -INFINITY;
  std::vector<int> local_treat;
  const bool exhaustive = static_cast<double>(lay.n) * (lay.n - 1) / 2.0 <= kExhaustivePairs;
  std::vector<int> order(lay.n);
  bool ok = true;  // false once a refresh fails or the time limit ends the search
  while (ok && !budget.stopped()) {
    int x, y;
    double scale = 1.0;
    if (exhaustive) {
      // One pass over all plot pairs in random order; stop at the end of a pass
      // without improvement (a true single-swap local optimum).
      for (int i = 0; i < lay.n; ++i) order[i] = i;
      for (int i = lay.n - 1; i > 0; --i) std::swap(order[i], order[draw_index(i + 1)]);
      bool improved = false;
      for (int i = 0; ok && i < lay.n && !budget.stopped(); ++i) {
        for (int j = i + 1; j < lay.n; ++j) {
          if (!budget.spend(1.0)) break;
          x = order[i]; y = order[j];
          if (lay.block_of[x] == lay.block_of[y] || treat[x] == treat[y]) continue;
          double ratio = bs->delta(treat[x], lay.block_of[x], treat[y], lay.block_of[y], &scale);
          if (improves(ratio, scale)) {
            if (!apply_swap(*bs, lay, treat, x, y, ratio, accepted, interval, out, budget)) { ok = false; break; }
            improved = true;
          }
        }
      }
      if (!ok) break;
      if (improved || budget.stopped()) continue;
    } else {
      // Random sampling; n consecutive draws without improvement end a pass.
      int fails = 0;
      while (fails < lay.n && budget.spend(1.0)) {
        if (draw_swap(lay, treat, x, y)) {
          double ratio = bs->delta(treat[x], lay.block_of[x], treat[y], lay.block_of[y], &scale);
          if (improves(ratio, scale)) {
            if (!apply_swap(*bs, lay, treat, x, y, ratio, accepted, interval, out, budget)) { ok = false; break; }
            fails = 0;
            continue;
          }
        }
        ++fails;
      }
      if (!ok || budget.stopped()) break;
    }
    // Local optimum reached: score it from a fresh refresh (spec section 8),
    // not from the running log determinant.
    if (!checked_refresh(*bs, lay, budget, out)) { ok = false; break; }
    if (bs->log_det() > local_best + kTie) {
      local_best = bs->log_det();
      local_treat = treat;
      stale = 0;
    } else if (++stale >= max_stale_escapes) {
      break;
    }
    if (!escape(*bs, lay, treat, jumps_per_escape, budget, out)) { ok = false; break; }
  }
  // Score the final state from scratch: the budget or time limit may have
  // stopped mid-pass, and a failed refresh leaves the running state unusable.
  BlockSpace fin(lay.v, lay.b, lay.r, counts_of(lay, treat));
  if (fin.refresh() && fin.log_det() > local_best) { local_best = fin.log_det(); local_treat = treat; }
  if (!local_treat.empty() && local_best > out.best_log_det) {
    out.best_log_det = local_best;
    out.best_treat = local_treat;
  }
}

}  // namespace

[[cpp11::register]]
cpp11::list blocks_engine_cpp(int v, int r, int b, int searches, int jumps_per_escape,
                              int max_stale_escapes, double budget, double time_limit,
                              bool debug) {
  Layout lay = make_layout(v, r, b);
  Budget bud(budget, time_limit);
  Search out;
  out.debug = debug;
  int done = 0;
  GetRNGstate();
  for (int s = 0; s < searches && !bud.stopped(); ++s) {
    run_search(lay, jumps_per_escape, max_stale_escapes, bud, out);
    ++done;
  }
  PutRNGstate();
  int stop = bud.stopped() ? bud.stop() : (out.best_treat.empty() ? STOP_NO_START : STOP_STALE);
  cpp11::writable::integers treatments(lay.n), blocks(lay.n);
  double d_eff = NA_REAL, a_eff = NA_REAL, log_det = NA_REAL;
  if (!out.best_treat.empty()) {
    BlockSpace fin(v, b, r, counts_of(lay, out.best_treat));
    if (fin.refresh()) { log_det = fin.log_det(); d_eff = fin.d_efficiency(); a_eff = fin.a_efficiency(); }
    for (int x = 0; x < lay.n; ++x) { treatments[x] = out.best_treat[x] + 1; blocks[x] = lay.block_of[x] + 1; }
  } else {
    for (int x = 0; x < lay.n; ++x) { treatments[x] = NA_INTEGER; blocks[x] = lay.block_of[x] + 1; }
  }
  using namespace cpp11::literals;
  return cpp11::writable::list({
      "treatments"_nm = treatments, "blocks"_nm = blocks, "log_det"_nm = log_det,
      "d_efficiency"_nm = d_eff, "a_efficiency"_nm = a_eff,
      "evaluations"_nm = bud.used(), "stop_reason"_nm = stop, "searches_done"_nm = done,
      "refreshes"_nm = out.refreshes, "drift_events"_nm = out.drift_events, "repairs"_nm = out.repairs,
      "debug_max_error"_nm = debug ? out.debug_max_error : NA_REAL});
}
