// Engine mode 3: the joint stage of the onestage row-column search (phase 2
// spec 6.1, stage 3 and "Mode 3, crossed"). One search from a given complete
// design: randomized first-improvement full passes on the joint log det of
// the rows and columns (CrossedSpace, block space), under the Row D and Row A
// floors, with escapes drawn from row-preserving pairs and iterated local
// search, as resolvable mode does.
//
// - Acceptance: a swap of two plots of one replicate must raise the joint
//   log det by a ratio > 1 + 1e-10 (relative to the size of its two terms, as
//   in the other modes).
// - Row floors: checked only after the joint ratio passes, and only when the
//   two plots lie in different rows (a swap inside a row leaves the Row design
//   unchanged). The rank-2 update of the Row state gives the Row log det and
//   the Row trace after the swap; both are compared, as running values, with
//   the fixed values of the start (the stage 1 design), never as per-swap
//   ratios, so no tolerance accumulates. The slack is 1e-10 (absolute on the
//   log det, relative on the A denominator), so rounding cannot reject a swap
//   that keeps the Row design's value.
// - Escapes: jumps_per_escape random swaps of two plots of one row of one
//   replicate, which never change the Row design. A jump that would lose rank
//   (the full-rank test of spec 5.1b on the swapped design, exact) is
//   rejected; rejected jumps are not charged to the evaluation budget (their
//   dense work is). An accepted jump costs one evaluation.
// - Drift: every `refresh_every` accepted swaps the joint and Row states (M
//   and the G cache) are checked with the sparse drift; on an alarm both are
//   refreshed and the interval halves (phase 1 fix I1). The alarm is on the
//   error of M itself, max|What M - I| times max_j M_jj, and on the cache
//   error, each > 1e-8: a near-singular start has entries of M near 1 / e_min,
//   where a small relative drift is a large error. A swap whose two ratio terms
//   exceed 1e3 (last_scale()) shrinks M by a Woodbury step that cancels most
//   of it, so such a swap is followed at once by a refresh of that state.
//   Measured with FIELDHUB_ENGINE_DEBUG=1 on about 250 random complete starts
//   (v 8-20, many near-singular): the largest relative error of an accepted
//   ratio was 6.1e-10 with this refresh and 3.3e-9 without it.
// - Budget and work: every ratio evaluation costs one evaluation (and the
//   budget's evaluation work); the drift check costs n + B evaluations as in
//   resolvable mode; refreshes, rank tests, Row checks and rank-2 updates are
//   charged as work only. Scoring the start is work only, so the smallest
//   budget still returns the scored start.
// - Joint-rank repair (spec 6.1 stage 2): a start that fails the full-rank
//   test is first repaired. Criterion: log det(C / r + eps P) only, eps =
//   kRepairEpsilon, as the ridge state of crossed_space.h. Swap set and Row
//   floors of the joint stage, Column free, first improvement over the pairs
//   of each replicate in one shuffled order, scanned cyclically. A candidate
//   passes the O(r) ratio and the Row floors, and is then confirmed from
//   scratch: the criterion of the swapped design, by a factorisation from the
//   counts, must exceed the current one (computed the same way) by more than
//   kRepairGain. The rank-2 ratio of the ridge state is only a screen: on a
//   singular design M has entries near 1 / eps and the ratio loses digits
//   (`repair_score_error` reports the largest |log ratio - exact gain| of an
//   accepted swap). After every accepted swap both states are rebuilt and the
//   full-rank test of spec 5.1b runs; the repair stops when it passes. It
//   fails after a full cycle without an accepted swap or after
//   kRepairMaxPasses cycles' worth of evaluations; the search then returns
//   full_rank = FALSE with the repair's last design and runs no joint stage.
//   A repair never spends the evaluation budget (phase 1 fix I-1): each of its
//   evaluations is charged the budget's evaluation work, and its dense steps
//   their work, to the work cap only; `evaluations` excludes them and
//   `repair_evaluations` counts them. The joint stage starts from states
//   built anew from the repaired design, so it runs exactly as a call that
//   was given the repaired design (with the floors of the stage 1 design).
// - Exact Row guarantee (spec 6.1: Row D and Row A never fall below their
//   stage 1 values). The running floors keep the 1e-10 slack. In addition a
//   swap between two rows is accepted only if Row D and Row A of the swapped
//   design, computed from scratch as the reported values are, are not below
//   the stage 1 values, compared exactly. At the end of the search the final
//   Row D and Row A are compared exactly once more; on a breach the result is
//   the best recorded local optimum that meets them exactly, else the design
//   the joint stage started from (the start, or the repaired design, which
//   meets them), and `floor_fallback` is 1. The repair's output is compared
//   the same way; on a breach the repair has failed and the start is
//   returned (`floor_fallback` 2). `slack_floors` (a check hook) skips the
//   confirmation of each swap, which is the only way the end comparison can
//   fail: a value computed from scratch depends on the design alone.
// - The returned design is tested again from scratch; a design that failed
//   the full-rank test would be replaced by the design the joint stage
//   started from.
#include <cpp11.hpp>
#include <R_ext/Random.h>
#include <algorithm>
#include <cmath>
#include <memory>
#include <vector>
#include "crossed_space.h"
#include "engine_budget.h"
#include "engine_rng.h"

namespace {

const double kTie = 1e-10;
const double kDriftAlarm = 1e-8;
const int kRefreshMin = 16;
const double kFloorTol = 1e-10;
const double kDisconnectRel = 1e-9;
const double kJumpSafe = 1e-6;
const double kRefreshScale = 1e3;  // refresh after a swap with larger ratio terms
const double kRepairEpsilon = 1e-6;  // spec 6.1 stage 2, relative to C / r
const int kRepairMaxPasses = 30;     // the repair's allowance, in full passes
// Least confirmed gain of a repair swap in the criterion. A factorisation of
// the ridge matrix (condition 1 / eps) carries rounding near 1e-10 per zero
// efficiency factor; a step of rank is worth log(1 + 1e-7 / eps) = 0.095 or
// more.
const double kRepairGain = 1e-8;

struct Crossed {
  int v, r, n, B, Br;             // B joint block columns, Br row blocks
  std::vector<int> joint_block;   // n x 2: row block, Br + column block
  std::vector<int> row_block;     // n x 1
  std::vector<int> row_of;        // the row block of each plot
  int cells, cell_size;           // row-preserving segments: replicate x row
  std::vector<int> cell_plots;
};

struct CrossedOut {
  double refreshes = 0, drift_events = 0;
  bool debug = false;
  double debug_max_error = 0;
  double repair_swaps = 0, repair_evaluations = 0, repair_score_error = 0;
};

// Row D and Row A of a design from scratch: a function of the treatments
// alone (a new state, its block lists in plot order), so two calls on one
// design agree to the last bit.
struct RowValue {
  bool ok = false;
  double d = 0.0, a = 0.0;
};
RowValue row_value(const Crossed& lay, const std::vector<int>& treat) {
  RowValue x;
  CrossedSpace row(lay.v, lay.r, lay.Br, 1, lay.row_block, treat);
  x.ok = row.refresh();
  if (x.ok) { x.d = row.d_efficiency(); x.a = row.a_efficiency(); }
  return x;
}

double refresh_work(int B, int n, int F, int r) {
  return static_cast<double>(B) * B * B + static_cast<double>(n) * F * F * r + static_cast<double>(n) * F * B;
}
// One rank-2 update: M (4 B^2), the cache (2 B v), Mu and Mg (4 r B).
double swap_work(int B, int v, int r) { return 4.0 * B * B + 2.0 * static_cast<double>(B) * v + 4.0 * r * B; }
double drift_work(int B, int n, int F) { return static_cast<double>(B) * (3.0 * B + 3.0 * F * n); }

bool improves(const CrossedSpace& s, double ratio) {
  return ratio > 1.0 + kTie && ratio > 1.0 + kTie * s.last_scale();
}

class Engine {
 public:
  Engine(const Crossed& lay, Budget& budget, CrossedOut& out, int refresh_every,
         double evaluation_work, bool exact_floors)
      : lay_(lay), budget_(budget), out_(out), interval_(refresh_every),
        evaluation_work_(evaluation_work), exact_floors_(exact_floors) {}

  // The stage 1 Row D and Row A, the exact floors of every design kept.
  void set_floors(double d, double a) { floor_d_ = d; floor_a_ = a; }

  // True if `treat` meets the Row floors exactly (Row D and Row A from
  // scratch, as reported, not below the stage 1 values).
  bool meets_floors(const std::vector<int>& treat) {
    if (!budget_.work(refresh_work(lay_.Br, lay_.n, 1, lay_.r) + lay_.n)) return false;
    const RowValue x = row_value(lay_, treat);
    return x.ok && x.d >= floor_d_ && x.a >= floor_a_;
  }

  // The same for the design after swapping plots x and y of `row`.
  bool swap_meets_floors(const CrossedSpace& row, int x, int y) {
    scratch_ = row.treatments();
    std::swap(scratch_[x], scratch_[y]);
    return meets_floors(scratch_);
  }

  bool refresh(CrossedSpace& s, int F) {
    if (!budget_.work(refresh_work(s.blocks(), lay_.n, F, lay_.r))) return false;
    if (!s.refresh()) return false;
    out_.refreshes += 1;
    return true;
  }

  // Sparse drift check of both states; both are refreshed on an alarm.
  bool check(CrossedSpace& joint, CrossedSpace& row) {
    if (!budget_.work(drift_work(lay_.B, lay_.n, 2) + drift_work(lay_.Br, lay_.n, 1))) return false;
    if (accurate(joint) && accurate(row)) return true;
    out_.drift_events += 1;
    interval_ = std::max(kRefreshMin, interval_ / 2);
    return refresh(joint, 2) && refresh(row, 1);
  }

  static bool accurate(const CrossedSpace& s) {
    return s.drift() * std::max(1.0, s.max_diag()) <= kDriftAlarm && s.drift_cache() <= kDriftAlarm;
  }

  // Apply a swap to both states (the Row state only when the rows differ).
  // joint_scale and row_scale are last_scale() of the two ratios.
  bool apply(CrossedSpace& joint, CrossedSpace& row, int x, int y, double ratio, double row_ratio,
             double joint_scale, double row_scale) {
    const bool rows_differ = lay_.row_of[x] != lay_.row_of[y];
    double before = 0.0, row_before = 0.0;
    if (out_.debug) {
      before = fresh_log_det(joint, 2);
      if (rows_differ) row_before = fresh_log_det(row, 1);
    }
    joint.apply(x, y, ratio);
    row.apply(x, y, rows_differ ? row_ratio : 1.0);
    if (out_.debug) {
      note_error(ratio, before, fresh_log_det(joint, 2));
      if (rows_differ) note_error(row_ratio, row_before, fresh_log_det(row, 1));
    }
    if (!budget_.work(swap_work(lay_.B, lay_.v, lay_.r) + (rows_differ ? swap_work(lay_.Br, lay_.v, lay_.r) : 0.0))) return false;
    if (joint_scale > kRefreshScale && !refresh(joint, 2)) return false;
    if (rows_differ && row_scale > kRefreshScale && !refresh(row, 1)) return false;
    if (++accepted_ % interval_ == 0) {
      if (!budget_.spend(static_cast<double>(lay_.n + lay_.B))) return true;
      return check(joint, row);
    }
    return true;
  }

  // Full-pass first-improvement descent over the pairs of each replicate.
  bool descend(CrossedSpace& joint, CrossedSpace& row, double floor_ld, double floor_sum, bool& optimum) {
    optimum = false;
    const int v = lay_.v;
    for (int i = 0; i < lay_.n; ++i) order_[i] = i;
    for (int m = 0; m < lay_.r; ++m) {
      int* o = order_.data() + static_cast<size_t>(m) * v;
      for (int i = v - 1; i > 0; --i) std::swap(o[i], o[draw_index(i + 1)]);
    }
    const double total = lay_.r * static_cast<double>(v) * (v - 1) / 2.0;
    double since = 0.0;
    for (;;) {
      for (int m = 0; m < lay_.r; ++m) {
        const int* o = order_.data() + static_cast<size_t>(m) * v;
        for (int i = 0; i < v; ++i) {
          for (int j = i + 1; j < v; ++j) {
            if (since >= total) { optimum = true; return true; }
            since += 1.0;
            const int x = o[i], y = o[j];
            if (!budget_.spend(1.0)) return true;
            const double ratio = joint.ratio(x, y);
            if (!improves(joint, ratio)) continue;
            const double joint_scale = joint.last_scale();
            double row_ratio = 1.0, row_scale = 1.0;
            if (lay_.row_of[x] != lay_.row_of[y]) {
              if (!budget_.work((4.0 * lay_.r + 8.0) * lay_.Br)) return false;
              double tr = 0.0;
              if (!row.after(x, y, row_ratio, tr)) continue;
              row_scale = row.last_scale();
              const double ld = row.log_det() + std::log(row_ratio);
              const double sum = tr - lay_.Br + v - 1.0;
              if (ld < floor_ld - kFloorTol || sum > floor_sum * (1.0 + kFloorTol)) continue;
              if (exact_floors_ && !swap_meets_floors(row, x, y)) {
                if (budget_.stopped()) return false;
                continue;
              }
            }
            if (!apply(joint, row, x, y, ratio, row_ratio, joint_scale, row_scale)) return false;
            if (budget_.stopped()) return true;
            since = 0.0;
          }
        }
      }
    }
  }

  // One escape: row-preserving jumps that keep full rank.
  bool escape(CrossedSpace& joint, CrossedSpace& row, int jumps) {
    if (lay_.cell_size < 2) return true;
    const double tries_max = 100.0 * jumps;
    int done = 0;
    for (double tries = 0; done < jumps && tries < tries_max && !budget_.stopped(); ++tries) {
      const int* c = lay_.cell_plots.data() + static_cast<size_t>(draw_index(lay_.cells)) * lay_.cell_size;
      const int x = c[draw_index(lay_.cell_size)], y = c[draw_index(lay_.cell_size)];
      if (x == y) continue;
      if (!budget_.work(16.0 + 8.0 * lay_.r)) return false;
      const double ratio = joint.ratio(x, y);
      const double scale = joint.last_scale();
      if (ratio <= kDisconnectRel * scale) continue;
      if (!budget_.work(refresh_work(lay_.B, lay_.n, 2, lay_.r))) return false;
      if (!joint.full_rank_after(x, y)) continue;
      if (!budget_.spend(1.0)) break;
      // A jump from the ambiguous zone is refreshed at once, as in the other
      // modes (a scale above kRefreshScale already refreshes inside apply()).
      if (!apply(joint, row, x, y, ratio, 1.0, scale, 1.0)) return false;
      if (ratio <= kJumpSafe * scale && scale <= kRefreshScale && !refresh(joint, 2)) return false;
      ++done;
    }
    return true;
  }

  // The joint-rank repair (header comment). `eps` is the ridge state of the
  // start and `row` its Row state, both refreshed. `full` is set when the
  // design of `eps` passes the full-rank test. Returns false if the work cap
  // or the time limit stopped it.
  bool repair(CrossedSpace& eps, CrossedSpace& row, double floor_ld, double floor_sum, bool& full) {
    full = false;
    const int v = lay_.v;
    for (int i = 0; i < lay_.n; ++i) order_[i] = i;
    for (int m = 0; m < lay_.r; ++m) {
      int* o = order_.data() + static_cast<size_t>(m) * v;
      for (int i = v - 1; i > 0; --i) std::swap(o[i], o[draw_index(i + 1)]);
    }
    const double total = lay_.r * static_cast<double>(v) * (v - 1) / 2.0;
    const double allowance = kRepairMaxPasses * total;
    const double fresh = refresh_work(lay_.B, lay_.n, 2, lay_.r);
    double current = 0.0;
    if (!budget_.work(fresh)) return false;
    if (!eps.log_det_fresh(current)) return true;
    double since = 0.0;
    for (;;) {
      for (int m = 0; m < lay_.r; ++m) {
        const int* o = order_.data() + static_cast<size_t>(m) * v;
        for (int i = 0; i < v; ++i) {
          for (int j = i + 1; j < v; ++j) {
            if (since >= total || out_.repair_evaluations >= allowance) return true;
            since += 1.0;
            const int x = o[i], y = o[j];
            if (!budget_.work(evaluation_work_)) return false;
            out_.repair_evaluations += 1;
            const double ratio = eps.ratio(x, y);
            if (!improves(eps, ratio)) continue;
            const bool rows_differ = lay_.row_of[x] != lay_.row_of[y];
            double row_ratio = 1.0;
            if (rows_differ) {
              if (!budget_.work((4.0 * lay_.r + 8.0) * lay_.Br)) return false;
              double tr = 0.0;
              if (!row.after(x, y, row_ratio, tr)) continue;
              const double ld = row.log_det() + std::log(row_ratio);
              const double sum = tr - lay_.Br + v - 1.0;
              if (ld < floor_ld - kFloorTol || sum > floor_sum * (1.0 + kFloorTol)) continue;
              if (exact_floors_ && !swap_meets_floors(row, x, y)) {
                if (budget_.stopped()) return false;
                continue;
              }
            }
            // Confirm the gain from scratch: the ratio above is only a screen.
            if (!budget_.work(fresh)) return false;
            double after = 0.0;
            if (!eps.log_det_after(x, y, after) || !(after > current + kRepairGain)) continue;
            out_.repair_score_error = std::max(out_.repair_score_error, std::fabs(std::log(ratio) - (after - current)));
            // Both states are rebuilt after the swap, so nothing drifts.
            eps.apply(x, y, ratio);
            row.apply(x, y, rows_differ ? row_ratio : 1.0);
            if (!budget_.work(swap_work(lay_.B, lay_.v, lay_.r) + (rows_differ ? swap_work(lay_.Br, lay_.v, lay_.r) : 0.0))) return false;
            if (!refresh(eps, 2)) return !budget_.stopped();
            if (rows_differ && !refresh(row, 1)) return !budget_.stopped();
            current = after;
            out_.repair_swaps += 1;
            since = 0.0;
            if (!budget_.work(fresh)) return false;
            if (eps.full_rank()) { full = true; return true; }
          }
        }
      }
    }
  }

  double fresh_log_det(const CrossedSpace& s, int F) const {
    CrossedSpace fresh(lay_.v, lay_.r, s.blocks(), F, F == 2 ? lay_.joint_block : lay_.row_block, s.treatments());
    return fresh.refresh() ? fresh.log_det() : NAN;
  }

  void note_error(double ratio, double before, double after) {
    const double exact = std::exp(after - before);
    const double e = std::isfinite(exact) ? std::fabs(ratio - exact) / std::max(1.0, exact) : INFINITY;
    if (!(e <= out_.debug_max_error)) out_.debug_max_error = e;
  }

  std::vector<int> order_;

 private:
  const Crossed& lay_;
  Budget& budget_;
  CrossedOut& out_;
  int interval_;
  int accepted_ = 0;
  double evaluation_work_;
  bool exact_floors_;
  double floor_d_ = 0.0, floor_a_ = 0.0;
  std::vector<int> scratch_;
};

Crossed make_crossed(int v, int r, int b, const std::vector<int>& row_of, const std::vector<int>& col_of) {
  Crossed lay;
  lay.v = v; lay.r = r; lay.n = v * r; lay.Br = b;
  const int n = lay.n;
  if (v < 2 || r < 1 || b < 1) cpp11::stop("Crossed mode needs v >= 2, r >= 1 and b >= 1.");
  if (static_cast<int>(row_of.size()) != n || static_cast<int>(col_of.size()) != n) {
    cpp11::stop("`block_of` and `column_of` must give the row and column block of every plot.");
  }
  int bc = 0;
  for (int p = 0; p < n; ++p) {
    if (row_of[p] < 0 || row_of[p] >= b) cpp11::stop("`block_of` must lie in 0 .. b - 1.");
    if (col_of[p] < 0 || col_of[p] >= n) cpp11::stop("`column_of` must lie in 0 .. n - 1.");
    bc = std::max(bc, col_of[p] + 1);
  }
  std::vector<int> rsize(b, 0), csize(bc, 0);
  for (int p = 0; p < n; ++p) { rsize[row_of[p]] += 1; csize[col_of[p]] += 1; }
  for (int j = 0; j < b; ++j) if (rsize[j] != rsize[0] || rsize[j] == 0) cpp11::stop("Every row block must hold the same number of plots.");
  for (int j = 0; j < bc; ++j) if (csize[j] != csize[0] || csize[j] == 0) cpp11::stop("Every column block must hold the same number of plots.");
  // Every replicate is a complete grid: no (row, column) cell twice, and its
  // rows times its columns make v plots.
  std::vector<int> seen(static_cast<size_t>(b) * bc, -1), rseen(b, -1), cseen(bc, -1);
  std::vector<int> cell_of(n, 0);
  std::vector<int> cell_id(static_cast<size_t>(b) * r, -1);
  int cells = 0;
  for (int m = 0; m < r; ++m) {
    int nr = 0, nc = 0;
    for (int p = m * v; p < (m + 1) * v; ++p) {
      size_t key = static_cast<size_t>(row_of[p]) * bc + col_of[p];
      if (seen[key] == m) cpp11::stop("Every replicate must be a complete grid of rows and columns.");
      seen[key] = m;
      if (rseen[row_of[p]] != m) { rseen[row_of[p]] = m; ++nr; }
      if (cseen[col_of[p]] != m) { cseen[col_of[p]] = m; ++nc; }
      size_t ck = static_cast<size_t>(m) * b + row_of[p];
      if (cell_id[ck] < 0) cell_id[ck] = cells++;
      cell_of[p] = cell_id[ck];
    }
    if (nr * nc != v) cpp11::stop("Every replicate must be a complete grid of rows and columns.");
  }
  lay.B = b + bc;
  lay.cells = cells;
  lay.cell_size = n / cells;
  lay.cell_plots.assign(n, 0);
  std::vector<int> fill(cells, 0);
  for (int p = 0; p < n; ++p) {
    const int c = cell_of[p];
    if (fill[c] >= lay.cell_size) cpp11::stop("Every row must hold the same number of plots in each replicate.");
    lay.cell_plots[static_cast<size_t>(c) * lay.cell_size + fill[c]++] = p;
  }
  lay.row_of = row_of;
  lay.row_block = row_of;
  lay.joint_block.resize(static_cast<size_t>(n) * 2);
  for (int p = 0; p < n; ++p) { lay.joint_block[2 * p] = row_of[p]; lay.joint_block[2 * p + 1] = b + col_of[p]; }
  return lay;
}

}  // namespace

// Mode 3 of blocks_engine_cpp() (declared there). `start` is 0-based and
// checked by the caller for length; every replicate must hold each treatment
// once. `slack_floors` is the check hook of the header comment.
cpp11::list crossed_engine(int v, int r, int b, const std::vector<int>& row_of,
                           const std::vector<int>& col_of, const std::vector<int>& start,
                           int jumps_per_escape, int max_stale_escapes, double budget_cap,
                           double time_limit, bool debug, double work_cap, int refresh_every,
                           double evaluation_work, bool slack_floors) {
  Crossed lay = make_crossed(v, r, b, row_of, col_of);
  for (int m = 0; m < r; ++m) {
    std::vector<int> seen(v, 0);
    for (int p = m * v; p < (m + 1) * v; ++p) {
      if (start[p] < 0 || start[p] >= v || seen[start[p]]++ > 0) {
        cpp11::stop("Every replicate of `start` must hold each treatment once.");
      }
    }
  }
  Budget budget(budget_cap, time_limit, work_cap, evaluation_work);
  CrossedOut out;
  out.debug = debug;
  Engine eng(lay, budget, out, std::max(1, refresh_every), evaluation_work, !slack_floors);
  eng.order_.assign(lay.n, 0);

  CrossedSpace joint(v, r, lay.B, 2, lay.joint_block, start);
  CrossedSpace row(v, r, lay.Br, 1, lay.row_block, start);
  budget.work(2.0 * refresh_work(lay.B, lay.n, 2, r) + refresh_work(lay.Br, lay.n, 1, r));
  const bool joint_ok = joint.refresh();
  const bool start_full = joint_ok && joint.full_rank();
  const bool row_ok = row.refresh();
  out.refreshes += (joint_ok ? 1 : 0) + (row_ok ? 1 : 0);
  const double stage1_row_d = row_ok ? row.d_efficiency() : NA_REAL;
  const double stage1_row_a = row_ok ? row.a_efficiency() : NA_REAL;
  const double floor_ld = row_ok ? row.log_det() : 0.0;
  const double floor_sum = row_ok ? row.trace() - lay.Br + v - 1.0 : 0.0;
  if (row_ok) eng.set_floors(stage1_row_d, stage1_row_a);

  // `entry` is the design the joint stage starts from: the start, or the
  // repaired design. `result` is what the call returns.
  std::vector<int> entry = start, result = start;
  bool entry_full = start_full;
  int floor_fallback = 0;
  double repair_log_det_start = NA_REAL, repair_log_det = NA_REAL;
  bool rng = false;
  if (!start_full && row_ok && !budget.stopped()) {
    // Joint-rank repair, charged to the work cap only.
    CrossedSpace eps(v, r, lay.B, 2, lay.joint_block, start, kRepairEpsilon);
    if (budget.work(refresh_work(lay.B, lay.n, 2, r)) && eps.refresh()) {
      out.refreshes += 1;
      double ld = 0.0;
      if (eps.log_det_fresh(ld)) repair_log_det_start = ld;
      GetRNGstate();
      rng = true;
      bool repaired = false;
      eng.repair(eps, row, floor_ld, floor_sum, repaired);
      if (eps.log_det_fresh(ld)) repair_log_det = ld;
      // The repair's output must meet the Row floors exactly; its last design
      // is returned when the repair failed and the design meets them.
      const bool meets = !budget.stopped() && eng.meets_floors(eps.treatments());
      if (repaired && !meets && !budget.stopped()) floor_fallback = 2;
      if (meets) {
        result = eps.treatments();
        if (repaired) {
          // States built anew from the repaired design: the joint stage then
          // runs exactly as from a start equal to it.
          joint = CrossedSpace(v, r, lay.B, 2, lay.joint_block, result);
          row = CrossedSpace(v, r, lay.Br, 1, lay.row_block, result);
          budget.work(2.0 * refresh_work(lay.B, lay.n, 2, r) + refresh_work(lay.Br, lay.n, 1, r));
          if (joint.refresh() && joint.full_rank() && row.refresh()) {
            out.refreshes += 2;
            entry = result;
            entry_full = true;
          }
        }
      }
    }
  }
  bool searched = false;
  if (entry_full && row_ok && !budget.stopped()) {
    searched = true;
    // A budget below one evaluation runs no search and draws nothing.
    if (!(budget_cap >= 1.0)) budget.spend(1.0);
    if (!rng) GetRNGstate();
    rng = true;
    double local_best = -INFINITY;
    std::vector<int> local_treat, exact_treat;
    std::unique_ptr<CrossedSpace> best_joint, best_row;
    int stale = 0;
    bool ok = true;
    while (ok && !budget.stopped()) {
      bool optimum = false;
      if (!eng.descend(joint, row, floor_ld, floor_sum, optimum)) { ok = false; break; }
      if (!optimum) continue;
      if (!eng.check(joint, row)) { ok = false; break; }
      if (joint.log_det() > local_best + kTie) {
        local_best = joint.log_det();
        local_treat = joint.treatments();
        best_joint.reset(new CrossedSpace(joint));
        best_row.reset(new CrossedSpace(row));
        stale = 0;
        // The best recorded optimum that meets the Row floors exactly (with
        // the confirmation of each swap, every recorded optimum).
        if (slack_floors) {
          if (eng.meets_floors(local_treat)) exact_treat = local_treat;
          if (budget.stopped()) { ok = false; break; }
        }
      } else if (++stale >= max_stale_escapes) {
        break;
      } else {
        joint = *best_joint;
        row = *best_row;
        if (!budget.work(static_cast<double>(lay.B) * (lay.B + v) + static_cast<double>(lay.Br) * (lay.Br + v))) {
          ok = false;
          break;
        }
      }
      if (!eng.escape(joint, row, jumps_per_escape)) { ok = false; break; }
    }
    // The current design is scored from scratch (it may lie mid-descent or
    // after an escape); the better of it and the best local optimum is kept.
    // Neither is below the entry design: descents only raise the joint log
    // det, and escapes are followed by a descent before any optimum is
    // recorded. Without a recorded optimum the current design has seen no
    // escape.
    CrossedSpace current(v, r, lay.B, 2, lay.joint_block, joint.treatments());
    const double current_ld = current.refresh() ? current.log_det() : -INFINITY;
    if (!local_treat.empty() && !(current_ld > local_best)) {
      result = local_treat;
    } else if (std::isfinite(current_ld)) {
      result = joint.treatments();
    }
    // The exact Row guarantee: Row D and Row A of the result, from scratch,
    // are not below the stage 1 values. On a breach: the best recorded
    // optimum that meets them exactly, else the entry design (it meets them).
    const RowValue rv = row_value(lay, result);
    if (!(rv.ok && rv.d >= stage1_row_d && rv.a >= stage1_row_a)) {
      floor_fallback = 1;
      result = exact_treat.empty() ? entry : exact_treat;
      const RowValue again = row_value(lay, result);
      if (!(again.ok && again.d >= stage1_row_d && again.a >= stage1_row_a)) result = entry;
    }
  }
  if (rng) PutRNGstate();
  // Final values from scratch; a result that fails the full-rank test is
  // replaced by the entry design (which passed it).
  CrossedSpace fin(v, r, lay.B, 2, lay.joint_block, result);
  bool fin_full = fin.refresh() && fin.full_rank();
  if (searched && !fin_full) {
    result = entry;
    fin = CrossedSpace(v, r, lay.B, 2, lay.joint_block, result);
    fin_full = fin.refresh() && fin.full_rank();
  }
  const RowValue fin_row = row_value(lay, result);
  const double joint_ld = fin_full ? fin.log_det() : NA_REAL;
  const double joint_d = fin_full ? fin.d_efficiency() : NA_REAL;
  const double joint_a = fin_full ? fin.a_efficiency() : NA_REAL;

  int stop = budget.stopped() ? budget.stop() : (searched ? STOP_STALE : STOP_NO_START);
  cpp11::writable::integers treatments(lay.n), blocks(lay.n);
  for (int p = 0; p < lay.n; ++p) { treatments[p] = result[p] + 1; blocks[p] = row_of[p] + 1; }
  using namespace cpp11::literals;
  return cpp11::writable::list({
      "treatments"_nm = treatments, "blocks"_nm = blocks, "log_det"_nm = joint_ld,
      "d_efficiency"_nm = joint_d, "a_efficiency"_nm = joint_a,
      "evaluations"_nm = budget.used(), "stop_reason"_nm = stop, "searches_done"_nm = searched ? 1 : 0,
      "refreshes"_nm = out.refreshes, "drift_events"_nm = out.drift_events, "repairs"_nm = 0.0,
      "even_repairs"_nm = 0, "work"_nm = budget.work_used(), "work_capped"_nm = budget.capped(),
      "debug_max_error"_nm = debug ? out.debug_max_error : NA_REAL,
      "stage1_row_d"_nm = stage1_row_d, "stage1_row_a"_nm = stage1_row_a,
      "row_d"_nm = fin_row.ok ? fin_row.d : NA_REAL,
      "row_a"_nm = fin_row.ok ? fin_row.a : NA_REAL,
      "joint_d"_nm = joint_d, "joint_a"_nm = joint_a, "joint_log_det"_nm = joint_ld,
      "full_rank"_nm = fin_full, "repair_swaps"_nm = out.repair_swaps,
      "repair_evaluations"_nm = out.repair_evaluations,
      "repair_log_det_start"_nm = repair_log_det_start, "repair_log_det"_nm = repair_log_det,
      "repair_score_error"_nm = out.repair_score_error, "floor_fallback"_nm = floor_fallback});
}
