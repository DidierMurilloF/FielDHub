// Single-level block design search (spec section 9; Edmondson 2020, sections 2.2-2.3).
// Randomized first-improvement plot swaps scored with the block-space ratio.
#include <cpp11.hpp>
#include <R_ext/Random.h>
#include <algorithm>
#include <cmath>
#include <memory>
#include <numeric>
#include <utility>
#include <vector>
#include "block_space.h"
#include "engine_budget.h"
#include "engine_rng.h"

namespace {

const double kTie = 1e-10;        // spec section 8: accept if ratio > 1 + 1e-10 (relative, below)
const int kRefreshEvery = 256;    // spec section 8: refresh interval, halved after a drift alarm
const int kRefreshMin = 16;       // smallest interval after repeated drift alarms
const double kDriftAlarm = 1e-8;  // spec section 8: drift alarm threshold
const double kExhaustivePairs = 200000;  // full passes only when n(n-1)/2 <= 2e5 (spec-m5 6.4)

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

bool is_prime(int q) {
  if (q < 2) return false;
  for (int d = 2; d * d <= q; ++d) if (q % d == 0) return false;
  return true;
}

// Square lattice start (spec-m5 6.4; Yates 1936). When v = q^2 and the b = r q
// blocks hold q plots each, treatment (i, j) goes to block i of replicate 0,
// block j of replicate 1, and block (i + (m - 1) j) mod q of replicate m >= 2:
// rows, columns and Latin squares that are mutually orthogonal for r <= 3 at
// any q and for r <= q + 1 at prime q. Treatment and block labels are randomly
// permuted. Returns an empty vector, with no random draws, for any other shape.
std::vector<int> lattice_start(const Layout& lay) {
  int q = static_cast<int>(std::lround(std::sqrt(static_cast<double>(lay.v))));
  if (q < 2 || q * q != lay.v || lay.r < 2 || lay.b != lay.r * q) return std::vector<int>();
  if (lay.r > 3 && !(is_prime(q) && lay.r <= q + 1)) return std::vector<int>();
  std::vector<int> tp(lay.v), bp(lay.b), fill(lay.b, 0), treat(lay.n);
  std::iota(tp.begin(), tp.end(), 0);
  std::iota(bp.begin(), bp.end(), 0);
  for (int x = lay.v - 1; x > 0; --x) std::swap(tp[x], tp[draw_index(x + 1)]);
  for (int x = lay.b - 1; x > 0; --x) std::swap(bp[x], bp[draw_index(x + 1)]);
  for (int i = 0; i < q; ++i) {
    for (int j = 0; j < q; ++j) {
      for (int m = 0; m < lay.r; ++m) {
        int g = m == 0 ? i : (m == 1 ? j : (i + (m - 1) * j) % q);
        int blk = bp[m * q + g];
        treat[blk * q + fill[blk]++] = tp[q * i + j];
      }
    }
  }
  return treat;
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
  return static_cast<double>(lay.b) * lay.b * lay.b + static_cast<double>(lay.n) * lay.r +
         static_cast<double>(lay.n) * lay.b;
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
  if (!budget.work(4.0 * lay.b * lay.b + 2.0 * lay.b * lay.v)) return false;  // the rank-2 updates
  if (++accepted % interval == 0) {
    if (!budget.spend(static_cast<double>(lay.n + lay.b))) return true;
    if (!budget.work(refresh_work(lay))) return false;  // the drift check below is O(b^3)
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
// error is not accepted. The scale is read only for the rare candidates that
// pass the absolute test, so the common rejection costs nothing extra.
bool improves(const BlockSpace& bs, double ratio) {
  return ratio > 1.0 + kTie && ratio > 1.0 + kTie * bs.last_scale();
}

// Escape jumps (spec section 8): a ratio clearly above zero relative to its
// scale proves the swap keeps the design connected; in the ambiguous zone the
// connectivity is checked exactly with union-find, because a weakly connected
// design's Minv is large and the ratio's rounding error grows with it.
const double kJumpSafe = 1e-6;
const double kDisconnectRel = 1e-9;

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
// applied with the rank-2 update. Each draw is charged one evaluation. A jump
// from the ambiguous zone is checked exactly and, when taken, is followed at
// once by a refresh, so later ratios come from a fresh state. Returns false if
// the search must end.
bool escape(BlockSpace& bs, const Layout& lay, std::vector<int>& treat, int jumps_per_escape,
            int& accepted, int& interval, Budget& budget, Search& out) {
  int x, y;
  for (int jump = 0; jump < jumps_per_escape && !budget.stopped();) {
    if (!budget.spend(1.0)) break;
    if (!draw_swap(lay, treat, x, y)) continue;
    double ratio = bs.delta(treat[x], lay.block_of[x], treat[y], lay.block_of[y]);
    double scale = bs.last_scale();
    if (ratio <= kJumpSafe * scale) {
      if (ratio <= kDisconnectRel * scale) continue;
      std::swap(treat[x], treat[y]);
      bool connected = budget.work(static_cast<double>(lay.n + lay.v + lay.b)) && components(lay, treat) == 1;
      std::swap(treat[x], treat[y]);
      if (!connected) continue;
      if (!apply_swap(bs, lay, treat, x, y, ratio, accepted, interval, out, budget)) return false;
      if (!checked_refresh(bs, lay, budget, out)) return false;
    } else if (!apply_swap(bs, lay, treat, x, y, ratio, accepted, interval, out, budget)) {
      return false;
    }
    ++jump;
  }
  return true;
}

// Full-pass descent to a single-swap local optimum. The plot order is shuffled
// once; the pairs are then scanned cyclically in that order, first improvement,
// until every pair has been scanned since the last accepted swap, which proves
// the local optimum without a separate confirming pass. Only valid swaps
// (different blocks, different treatments) are evaluated and charged. Returns
// false if the search must end; `optimum` is false if the budget stopped it.
bool descend_full(BlockSpace& bs, const Layout& lay, std::vector<int>& treat, std::vector<int>& order,
                  int& accepted, int& interval, Search& out, Budget& budget, bool& optimum) {
  optimum = false;
  for (int i = 0; i < lay.n; ++i) order[i] = i;
  for (int i = lay.n - 1; i > 0; --i) std::swap(order[i], order[draw_index(i + 1)]);
  const double total = static_cast<double>(lay.n) * (lay.n - 1) / 2.0;
  double since = 0.0;
  for (;;) {
    for (int i = 0; i < lay.n; ++i) {
      for (int j = i + 1; j < lay.n; ++j) {
        if (since >= total) { optimum = true; return true; }
        since += 1.0;
        int x = order[i], y = order[j];
        if (lay.block_of[x] == lay.block_of[y] || treat[x] == treat[y]) continue;
        if (!budget.spend(1.0)) return true;
        double ratio = bs.delta_cached(treat[x], lay.block_of[x], treat[y], lay.block_of[y]);
        if (improves(bs, ratio)) {
          if (!apply_swap(bs, lay, treat, x, y, ratio, accepted, interval, out, budget)) return false;
          if (budget.stopped()) return true;
          since = 0.0;
        }
      }
    }
  }
}

void run_search(const Layout& lay, int jumps_per_escape, int max_stale_escapes,
                Budget& budget, Search& out, const std::vector<int>* start = nullptr) {
  std::vector<int> treat(lay.n);
  for (int x = 0; x < lay.n; ++x) treat[x] = x / lay.r;  // r copies of each treatment
  std::unique_ptr<BlockSpace> bs;
  // Full passes read swap ratios from the cache (spec-m5 6.5); it is switched on
  // before the start's refresh, which then builds it.
  const bool exhaustive = static_cast<double>(lay.n) * (lay.n - 1) / 2.0 <= kExhaustivePairs;
  if (start) {  // a lattice start is connected; random starts follow if it can't be scored
    if (!budget.spend(static_cast<double>(lay.n + lay.b))) return;
    treat = *start;
    bs.reset(new BlockSpace(lay.v, lay.b, lay.r, counts_of(lay, treat)));
    if (exhaustive) bs->enable_cache();
    if (!budget.work(refresh_work(lay)) || !bs->refresh()) bs.reset();
  }
  for (int attempt = 0; attempt < 10 && !bs; ++attempt) {
    if (!budget.spend(static_cast<double>(lay.n + lay.b))) return;
    for (int x = lay.n - 1; x > 0; --x) std::swap(treat[x], treat[draw_index(x + 1)]);
    if (!repair_connectivity(lay, treat, budget, out)) continue;
    bs.reset(new BlockSpace(lay.v, lay.b, lay.r, counts_of(lay, treat)));
    if (exhaustive) bs->enable_cache();
    if (!budget.work(refresh_work(lay)) || !bs->refresh()) bs.reset();
  }
  if (!bs) return;  // no connected start within 10 repaired shuffles
  int accepted = 0, stale = 0, interval = kRefreshEvery;
  double local_best = -INFINITY;
  std::vector<int> local_treat;
  std::unique_ptr<BlockSpace> best_state;  // full passes: the state at local_treat
  std::vector<int> order(lay.n);
  bool ok = true;  // false once a refresh fails or the time limit ends the search
  while (ok && !budget.stopped()) {
    int x, y;
    if (exhaustive) {
      bool optimum = false;
      if (!descend_full(*bs, lay, treat, order, accepted, interval, out, budget, optimum)) { ok = false; break; }
      if (!optimum) continue;  // the budget or the time limit stopped the descent
    } else {
      // Random sampling; n consecutive draws without improvement end a pass.
      int fails = 0;
      while (fails < lay.n && budget.spend(1.0)) {
        if (draw_swap(lay, treat, x, y)) {
          double ratio = bs->delta(treat[x], lay.block_of[x], treat[y], lay.block_of[y]);
          if (improves(*bs, ratio)) {
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
      if (exhaustive) best_state.reset(new BlockSpace(*bs));
    } else if (++stale >= max_stale_escapes) {
      break;
    } else if (exhaustive) {
      // A full-pass search stands at a proven local optimum, so a failed
      // escape returns to the best one (iterated local search) instead of
      // walking on from a worse design. The state saved there was fresh.
      treat = local_treat;
      *bs = *best_state;
      if (!budget.work(static_cast<double>(lay.b) * (lay.b + lay.v))) { ok = false; break; }
    }
    if (!escape(*bs, lay, treat, jumps_per_escape, accepted, interval, budget, out)) { ok = false; break; }
  }
  // Score the final state from scratch: the budget may have stopped mid-pass,
  // and a failed refresh leaves the running state unusable. After the time
  // limit this O(b^3) step is skipped (the run is flagged as not reproducible,
  // and every local optimum already has a fresh score), unless no state has
  // been scored yet.
  if (budget.stop() != STOP_TIME || local_treat.empty()) {
    BlockSpace fin(lay.v, lay.b, lay.r, counts_of(lay, treat));
    if (fin.refresh() && fin.log_det() > local_best) { local_best = fin.log_det(); local_treat = treat; }
  }
  if (!local_treat.empty() && local_best > out.best_log_det) {
    out.best_log_det = local_best;
    out.best_treat = local_treat;
  }
}

// Even-up repair (spec-m5 section 6.1). A search stopped by its budget can
// return a design that is not a local optimum, and then some treatment may
// occur in one block at least twice more often than in another. While that
// happens: take the treatment and the pair of blocks with the largest
// difference (ties: smallest treatment, then blocks), and swap one of its plots
// in the fuller block with a plot of a partner treatment that is more frequent
// in the emptier block. With block sizes within one of each other a partner
// always exists, and each swap lowers the sum of squared counts by at least 2,
// so the loop ends; its only fixed point is the even pattern. Partners whose
// determinant ratio is at most 1e-9 of its scale would disconnect the design
// and are skipped; the highest ratio wins, ratios within a relative 1e-10 of
// it tie, and a tie goes to the smallest partner. The chosen swap is confirmed
// connected with union-find. No random numbers are drawn, and an even design
// is returned untouched. Returns the number of swaps, or -1 if no connected
// partner exists (the caller's evenness check then fails).
const double kRepairTie = 1e-10;

int even_repair(const Layout& lay, std::vector<int>& treat) {
  int swaps = 0;
  for (;;) {
    std::vector<int> c = counts_of(lay, treat);
    int ti = -1, full = -1, empty = -1, worst = 1;
    for (int i = 0; i < lay.v; ++i) {
      int hi = 0, lo = 0;
      for (int j = 1; j < lay.b; ++j) {
        if (c[i + lay.v * j] > c[i + lay.v * hi]) hi = j;
        if (c[i + lay.v * j] < c[i + lay.v * lo]) lo = j;
      }
      int d = c[i + lay.v * hi] - c[i + lay.v * lo];
      if (d > worst) { worst = d; ti = i; full = hi; empty = lo; }
    }
    if (ti < 0) return swaps;
    cpp11::check_user_interrupt();  // once per move; throws to R
    BlockSpace bs(lay.v, lay.b, lay.r, c);
    if (!bs.refresh()) return -1;
    std::vector<std::pair<double, int>> partners;  // (ratio, partner)
    for (int i = 0; i < lay.v; ++i) {
      if (i == ti || c[i + lay.v * empty] <= c[i + lay.v * full]) continue;
      double ratio = bs.delta(ti, full, i, empty);
      if (ratio > kDisconnectRel * bs.last_scale()) partners.push_back({ratio, i});
    }
    bool done = false;
    while (!done && !partners.empty()) {
      double best = -INFINITY;
      for (const auto& p : partners) best = std::max(best, p.first);
      size_t pick = 0;
      for (size_t k = 0; k < partners.size(); ++k) {
        if (partners[k].first >= best - kRepairTie * std::fabs(best)) { pick = k; break; }
      }
      int partner = partners[pick].second;
      partners.erase(partners.begin() + static_cast<long>(pick));
      int x = -1, y = -1;
      for (int k = 0; k < lay.n && (x < 0 || y < 0); ++k) {
        if (x < 0 && lay.block_of[k] == full && treat[k] == ti) x = k;
        if (y < 0 && lay.block_of[k] == empty && treat[k] == partner) y = k;
      }
      std::swap(treat[x], treat[y]);
      if (components(lay, treat) == 1) {
        done = true;
      } else {
        std::swap(treat[x], treat[y]);
      }
    }
    if (!done) return -1;
    ++swaps;
  }
}

}  // namespace

// Test hook: run the even-up repair on a design given as the treatment of each
// plot (1-based, plots in block order as in blocks_engine()). Returns the
// repaired treatments and the number of swaps (-1 if it could not finish).
[[cpp11::register]]
cpp11::list even_repair_probe_cpp(cpp11::integers treatments, int v, int r, int b) {
  Layout lay = make_layout(v, r, b);
  std::vector<int> treat(lay.n);
  for (int x = 0; x < lay.n; ++x) treat[x] = treatments[x] - 1;
  int swaps = even_repair(lay, treat);
  cpp11::writable::integers out(lay.n);
  for (int x = 0; x < lay.n; ++x) out[x] = treat[x] + 1;
  using namespace cpp11::literals;
  return cpp11::writable::list({"treatments"_nm = out, "swaps"_nm = swaps});
}

// Test hook: the lattice start for (v, r, b) as 1-based treatments by plot,
// or an empty vector when the shape is not a square lattice.
[[cpp11::register]]
cpp11::integers lattice_start_probe_cpp(int v, int r, int b) {
  Layout lay = make_layout(v, r, b);
  GetRNGstate();
  std::vector<int> start = lattice_start(lay);
  PutRNGstate();
  cpp11::writable::integers out(static_cast<R_xlen_t>(start.size()));
  for (size_t x = 0; x < start.size(); ++x) out[x] = start[x] + 1;
  return out;
}

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
  std::vector<int> lattice = lattice_start(lay);
  for (int s = 0; s < searches && !bud.stopped(); ++s) {
    run_search(lay, jumps_per_escape, max_stale_escapes, bud, out,
               s == 0 && !lattice.empty() ? &lattice : nullptr);
    ++done;
  }
  PutRNGstate();
  int stop = bud.stopped() ? bud.stop() : (out.best_treat.empty() ? STOP_NO_START : STOP_STALE);
  int even_repairs = out.best_treat.empty() ? 0 : even_repair(lay, out.best_treat);
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
      "even_repairs"_nm = even_repairs,
      "debug_max_error"_nm = debug ? out.debug_max_error : NA_REAL});
}
