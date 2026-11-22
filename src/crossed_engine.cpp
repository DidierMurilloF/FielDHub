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
//   fail in the block-space form: a value computed from scratch depends on
//   the design alone.
// - Row confirmation in the treatment-space form (controller ruling R9,
//   2026-11-22; spec 6.1 W3). A Row value from scratch costs min(v, B_row)^3
//   there, and confirming every swap between rows left 2000/1000/4 nested 13
//   swaps inside the work cap. So, in the joint stage of the cheap form
//   (below) only, a swap between two rows that passes the running floors is
//   confirmed from scratch only when its running Row value is near a floor:
//   Row log det below the stage 1 value plus a margin, or the A denominator
//   above the stage 1 value times (1 - a margin); the margins are
//   kConfirmLd and kConfirmSum, scaled with the Row design (constants
//   below). Any other swap is applied unconfirmed. The exact guarantee is
//   kept by:
//   - checkpoints: a Row value of the current design from scratch after
//     every kCheckpointSwaps unconfirmed swaps between rows, and before a
//     local optimum is recorded as the best. A design that passes is exact,
//     and the best such design (by its running joint log det) is kept, as is
//     the design before the first unconfirmed swap after an exact one;
//   - a breach at a checkpoint (a running value was more than the margin
//     from the value from scratch): both states are rebuilt from the best
//     exact design, and from then on every swap is confirmed, as in the
//     block-space form (`row_rollbacks`);
//   - the end comparison above, whose replacement is the best exact design
//     (else the entry design).
//   At every Row value from scratch the engine records how far the running
//   value was from it (`row_drift_ld`, absolute; `row_drift_sum`, relative),
//   and a drift above a quarter of a margin refreshes the Row state at once.
//   The margins and the period are set from that drift (constants below).
//   The repair still confirms every swap. Check hooks: `confirm_floors`
//   confirms every swap as before the ruling; `late_floors` sets the margins
//   to 0 and the period to 1, so every breach is met at a checkpoint.
// - The returned design is tested again from scratch; a design that failed
//   the full-rank test would be replaced by the design the joint stage
//   started from.
// - Treatment-space form (spec 5.1c, 6.1 W3; `treatment_form`). The joint
//   state, the ridge state and the Row state are each built from the
//   treatment space when they have more block columns than treatments
//   (crossed_space.h). Where the joint state is, the call also scores less
//   ("cheap" below), because a factorisation of order 2000 is seconds:
//   - the start is tested for full rank from the trace of its fresh M
//     (CrossedSpace::full_rank_refreshed()), with the exact test only when
//     tr(M) > 1e7;
//   - after a repair the Row state is kept (it was rebuilt after its last
//     change, and the exact Row floors never read it), and the repair's
//     output is not scored again for the Row floors, which every accepted
//     repair swap has already met exactly;
//   - at the end of the search, however it ended, the running joint state
//     gets one drift check; its log det is used unless the check alarms, and
//     only then is the current design refreshed from scratch;
//   - the returned design is scored once: one refresh of the joint state
//     (full rank from its trace as above) and one Row value, which is both
//     the exact Row comparison and the reported value.
//   None of this runs in the block-space form, whose arithmetic is unchanged.
// - Work limit (spec 6.1 W2, W3). The start (the first scoring of the given
//   design) is always done and always charged. The finish (scoring the
//   result after the search has ended) is charged as a fixed amount,
//   `finish_charged`, an upper bound of what the finish does:
//   - block-space form: 5 refreshes of the joint state and 3 Row values (the
//     current design; the result and its full-rank test; both once more if
//     the result is replaced by the entry design; the Row value of the
//     result, of a replacement after a Row breach, and the one reported);
//   - cheap form: one drift check, one refresh of the joint state, one
//     factorisation (the exact full-rank test) and one Row value. A drift
//     alarm, or a result that fails the full-rank test, adds a refresh that
//     is not in the charge (spec W7: one dense step), and a result below the
//     Row floors (ruling R9: only after a running Row value drifted by more
//     than the margin) adds one Row value.
//   The repair and the search run under `work_cap` less that charge, with a
//   strict budget: a step whose charge would pass it is not done, and the
//   call goes to its finish. So the reported work, start, search and charged
//   finish, is at most `work_cap` whenever `work_cap` covers the start and
//   the finish, and the call then returns a scored design. `finish_work` is
//   the work of the finish steps actually done, for the checks.
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
// Ruling R9 (header comment): the margins inside which a swap between rows is
// confirmed from scratch, on the running Row log det (absolute) and on the A
// denominator (relative), and the unconfirmed swaps between two checkpoints.
// Engine::set_lazy() scales the margins with the Row design: both by
// c = max(1, d / 10)^2, d the largest diagonal entry of the Row state's M at
// the start (its conditioning), and the log det margin also by
// max(1, |log det| / 1000).
//
// Measured 2026-11-22 (`row_drift_ld`, `row_drift_sum` of the calls; logs
// t8s3c-drift.log, t8s3c-cond.log, t8s3c-after*.log and the check
// `treatment_form`, in ~/fieldhub-libs/runs/phase2/): the distance of a
// running Row value from the value from scratch, at every Row value from
// scratch of
// - searches that confirm every swap (hook `confirm_floors`; the running Row
//   state is never refreshed in between) from random complete starts, up to
//   2960 swaps between rows in one search (300/150/4, 400/200/3, 200/100/3,
//   1000/500/4, 2000/1000/4), and small ones forced into the form;
// - the searches of the ruling from stage 1 starts at 1000/500/4,
//   1500/750/3, 2000/1000/4 and 2000/2/4, with up to 256 unconfirmed swaps
//   before a checkpoint;
// - repairs on Row designs of two replicates in rows of two (30/15/2 to
//   600/300/2), whose Row state is ill-conditioned (d = v / 3).
// With d <= 7 (three and four replicates) the distance was at most 5.2e-12
// on the log det (2000/1000/4, |log det| 1723; 3e-15 |log det| at every
// size: the rounding of the factorisation, not of the updates) and 9.7e-15,
// relative, on the A denominator, and it did not grow with the number of
// swaps since the last refresh. On the ill-conditioned Row designs it grew
// to 1.5e-11 and 5.5e-11 at d = 200 (600/300/2), the A denominator's about
// as 1.4e-15 d^2 (3.0e-14 at d = 10, 1.3e-13 at 20, 8.4e-13 at 33, 4.6e-12
// at 67). So the margins are at least 700 times the largest distance
// measured at every conditioning. They are also below the Row gain of the
// swaps between rows of the large shapes: from the stage 1 design (seed 1,
// the joint call seeded 77) none of the 545, 93 and 7 such swaps of
// 2000/1000/4, 1500/750/3 and 1000/500/4 was confirmed, where margins of
// 1e-6 confirmed 2 of 497, 31 of 37 and 4 of 7 (at 1500/750/3 that left 466
// swaps in all, against 1475).
// The period is the drift-check interval of the running states
// (`refresh_every`, 256): the drift does not ask for a shorter one, and at
// 2000/1000/4 one Row value from scratch (8.1e9 units) per 256 swaps (about
// 4e10 units) costs a fifth more.
const double kConfirmLd = 1e-8;
const double kConfirmSum = 1e-10;
const int kCheckpointSwaps = 256;

struct Crossed {
  int v, r, n, B, Br;             // B joint block columns, Br row blocks
  bool form = false;              // treatment-space form where v < block columns
  std::vector<int> joint_block;   // n x 2: row block, Br + column block
  std::vector<int> row_block;     // n x 1
  std::vector<int> row_of;        // the row block of each plot
  int cells, cell_size;           // row-preserving segments: replicate x row
  std::vector<int> cell_plots;
};

struct CrossedOut {
  double refreshes = 0, drift_events = 0;
  // Refreshes of the plain joint state and from-scratch Row values that
  // score the start, the repaired design and the result (not those of the
  // search and the repair loops).
  double scoring_joint = 0, scoring_row = 0;
  double accepted = 0;            // swaps the joint stage applied (escape jumps included)
  // Ruling R9: swaps between rows the joint stage applied, Row values from
  // scratch that confirmed a swap (joint stage and repair), checkpoints,
  // rollbacks after a breach, and the largest distance of a running Row
  // value from the value from scratch (log det absolute, A denominator
  // relative).
  double row_swaps = 0, row_confirmations = 0, row_checkpoints = 0, row_rollbacks = 0;
  double row_drift_ld = 0, row_drift_sum = 0;
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
  double ld = 0.0, sum = 0.0;  // log det and the A denominator, as the running floors read them
};
RowValue row_value(const Crossed& lay, const std::vector<int>& treat) {
  RowValue x;
  CrossedSpace row(lay.v, lay.r, lay.Br, 1, lay.row_block, treat, 0.0, lay.form);
  x.ok = row.refresh();
  if (x.ok) {
    x.d = row.d_efficiency();
    x.a = row.a_efficiency();
    x.ld = row.log_det();
    x.sum = row.trace() - lay.Br + lay.v - 1.0;
  }
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
  // Work of a factorisation of the joint state and of a refresh of the Row
  // state (CrossedSpace::factor_work(), refresh_work()).
  void set_work(double joint_factor, double row_refresh) {
    joint_factor_ = joint_factor; row_refresh_ = row_refresh;
  }
  // Ruling R9 (header comment): confirm a swap between rows only near a
  // floor. `entry_log_det` is the joint log det of the design the joint
  // stage starts from, `row_log_det` and `row_diag` the log det and the
  // largest diagonal entry of M of the Row state at the start; `late` is the
  // check hook `late_floors`.
  void set_lazy(double entry_log_det, double row_log_det, double row_diag, bool late) {
    lazy_ = true;
    exact_ld_ = entry_log_det;
    const double c = std::max(1.0, row_diag / 10.0);
    margin_ld_ = kConfirmLd * c * c * std::max(1.0, std::fabs(row_log_det) / 1000.0);
    margin_sum_ = kConfirmSum * c * c;
    drift_ld_ = 0.25 * margin_ld_;
    drift_sum_ = 0.25 * margin_sum_;
    if (late) { margin_ld_ = 0.0; margin_sum_ = 0.0; period_ = 1; }
  }

  // True if `treat` meets the Row floors exactly (Row D and Row A from
  // scratch, as reported, not below the stage 1 values).
  bool meets_floors(const std::vector<int>& treat) {
    if (!budget_.work(row_refresh_ + lay_.n)) return false;
    const RowValue x = row_value(lay_, treat);
    return x.ok && x.d >= floor_d_ && x.a >= floor_a_;
  }

  // The same for the design after swapping plots x and y of `row`; `ld` and
  // `sum` are the running Row log det and A denominator after the swap.
  bool swap_meets_floors(const CrossedSpace& row, int x, int y, double ld, double sum) {
    scratch_ = row.treatments();
    std::swap(scratch_[x], scratch_[y]);
    if (!budget_.work(row_refresh_ + lay_.n)) return false;
    const RowValue value = row_value(lay_, scratch_);
    out_.row_confirmations += 1;
    if (value.ok) note_drift(value, ld, sum);
    return value.ok && value.d >= floor_d_ && value.a >= floor_a_;
  }

  // Records the distance of the running Row values `ld` and `sum` from the
  // values from scratch; a distance above a quarter of a margin asks for a
  // refresh of the Row state (ruling R9 only).
  void note_drift(const RowValue& exact, double ld, double sum) {
    const double e_ld = std::fabs(ld - exact.ld), e_sum = std::fabs(sum - exact.sum) / exact.sum;
    if (!(e_ld <= out_.row_drift_ld)) out_.row_drift_ld = e_ld;
    if (!(e_sum <= out_.row_drift_sum)) out_.row_drift_sum = e_sum;
    if (lazy_ && !(e_ld <= drift_ld_ && e_sum <= drift_sum_)) row_drifted_ = true;
  }
  bool refresh_drifted(CrossedSpace& row) {
    if (!row_drifted_) return true;
    row_drifted_ = false;
    return refresh(row);
  }

  // The design of `joint` meets the Row floors exactly: keep it if it is the
  // best such design so far.
  void note_exact(const CrossedSpace& joint) {
    if (joint.log_det() > exact_ld_) { exact_ld_ = joint.log_det(); exact_treat_ = joint.treatments(); }
  }

  // A checkpoint of ruling R9: the Row value of the current design from
  // scratch, when swaps between rows were applied unconfirmed since the last
  // one.
  enum Verdict { kExact, kBreach, kStopped };
  Verdict checkpoint(const CrossedSpace& joint, CrossedSpace& row) {
    if (pending_ == 0) return kExact;
    if (!budget_.work(row_refresh_ + lay_.n)) return kStopped;
    const RowValue value = row_value(lay_, row.treatments());
    out_.row_checkpoints += 1;
    if (!(value.ok && value.d >= floor_d_ && value.a >= floor_a_)) return kBreach;
    note_drift(value, row.log_det(), row.trace() - lay_.Br + lay_.v - 1.0);
    pending_ = 0;
    note_exact(joint);
    return refresh_drifted(row) ? kExact : kStopped;
  }
  // The search went back to a state whose design is exact.
  void restored() { pending_ = 0; }
  // True once, after descend() met a breach at a checkpoint.
  bool take_breach() { const bool b = breach_; breach_ = false; return b; }
  // After a breach: both states are rebuilt from the best exact design
  // (`entry` if none was kept) and every later swap is confirmed.
  bool rollback(CrossedSpace& joint, CrossedSpace& row, const std::vector<int>& entry) {
    out_.row_rollbacks += 1;
    lazy_ = false;
    pending_ = 0;
    row_drifted_ = false;
    if (!budget_.work(joint.refresh_work() + row.refresh_work())) return false;
    const std::vector<int>& to = exact_treat_.empty() ? entry : exact_treat_;
    joint = CrossedSpace(lay_.v, lay_.r, lay_.B, 2, lay_.joint_block, to, 0.0, lay_.form);
    row = CrossedSpace(lay_.v, lay_.r, lay_.Br, 1, lay_.row_block, to, 0.0, lay_.form);
    if (!joint.refresh() || !row.refresh()) return false;
    out_.refreshes += 2;
    return true;
  }
  // The best design known to meet the Row floors exactly; empty: the entry.
  const std::vector<int>& exact_treat() const { return exact_treat_; }

  bool refresh(CrossedSpace& s) {
    if (!budget_.work(s.refresh_work())) return false;
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
    return refresh(joint) && refresh(row);
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
    // Charged before it is done, so a swap the work limit refuses is not made.
    if (!budget_.work(swap_work(lay_.B, lay_.v, lay_.r) + (rows_differ ? swap_work(lay_.Br, lay_.v, lay_.r) : 0.0))) return false;
    joint.apply(x, y, ratio);
    row.apply(x, y, rows_differ ? row_ratio : 1.0);
    if (out_.debug) {
      note_error(ratio, before, fresh_log_det(joint, 2));
      if (rows_differ) note_error(row_ratio, row_before, fresh_log_det(row, 1));
    }
    out_.accepted += 1;
    if (joint_scale > kRefreshScale && !refresh(joint)) return false;
    if (rows_differ && row_scale > kRefreshScale && !refresh(row)) return false;
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
              // Ruling R9: with `lazy_`, confirm only near a floor.
              const bool confirm = exact_floors_ &&
                  (!lazy_ || ld < floor_ld + margin_ld_ || sum > floor_sum * (1.0 - margin_sum_));
              if (confirm && !swap_meets_floors(row, x, y, ld, sum)) {
                if (budget_.stopped()) return false;
                continue;
              }
              out_.row_swaps += 1;
              if (lazy_ && !confirm) {
                if (pending_ == 0) note_exact(joint);
                ++pending_;
              }
            }
            if (!apply(joint, row, x, y, ratio, row_ratio, joint_scale, row_scale)) return false;
            if (budget_.stopped()) return true;
            since = 0.0;
            if (lazy_) {
              if (!refresh_drifted(row)) return false;
              if (pending_ >= period_) {
                const Verdict verdict = checkpoint(joint, row);
                if (verdict == kStopped) return false;
                if (verdict == kBreach) { breach_ = true; return true; }
              }
            }
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
      if (!budget_.work(joint_factor_)) return false;
      if (!joint.full_rank_after(x, y)) continue;
      if (!budget_.spend(1.0)) break;
      // A jump from the ambiguous zone is refreshed at once, as in the other
      // modes (a scale above kRefreshScale already refreshes inside apply()).
      if (!apply(joint, row, x, y, ratio, 1.0, scale, 1.0)) return false;
      if (ratio <= kJumpSafe * scale && scale <= kRefreshScale && !refresh(joint)) return false;
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
    const double fresh = joint_factor_;
    double& current = repair_log_det_;
    current = NAN;
    if (!budget_.work(fresh)) return false;
    if (!eps.log_det_fresh(current)) { current = NAN; return true; }
    repair_log_det_start_ = current;
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
              if (exact_floors_ && !swap_meets_floors(row, x, y, ld, sum)) {
                if (budget_.stopped()) return false;
                continue;
              }
            }
            // Confirm the gain from scratch: the ratio above is only a screen.
            if (!budget_.work(fresh)) return false;
            double after = 0.0;
            if (!eps.log_det_after(x, y, after) || !(after > current + kRepairGain)) continue;
            out_.repair_score_error = std::max(out_.repair_score_error, std::fabs(std::log(ratio) - (after - current)));
            // Both states are rebuilt after the swap, so nothing drifts. The
            // swap, its two refreshes and the full-rank test are charged
            // together before the swap is made, so the work limit never
            // leaves a swap half done.
            if (!budget_.work(swap_work(lay_.B, lay_.v, lay_.r) + (rows_differ ? swap_work(lay_.Br, lay_.v, lay_.r) : 0.0) +
                              eps.refresh_work() + (rows_differ ? row.refresh_work() : 0.0) + fresh)) return false;
            eps.apply(x, y, ratio);
            row.apply(x, y, rows_differ ? row_ratio : 1.0);
            if (!eps.refresh()) return true;
            out_.refreshes += 1;
            if (rows_differ) {
              if (!row.refresh()) return true;
              out_.refreshes += 1;
            }
            current = after;
            out_.repair_swaps += 1;
            since = 0.0;
            if (eps.full_rank()) { full = true; return true; }
          }
        }
      }
    }
  }

  double fresh_log_det(const CrossedSpace& s, int F) const {
    CrossedSpace fresh(lay_.v, lay_.r, s.blocks(), F, F == 2 ? lay_.joint_block : lay_.row_block, s.treatments(),
                       0.0, lay_.form);
    return fresh.refresh() ? fresh.log_det() : NAN;
  }

  void note_error(double ratio, double before, double after) {
    const double exact = std::exp(after - before);
    const double e = std::isfinite(exact) ? std::fabs(ratio - exact) / std::max(1.0, exact) : INFINITY;
    if (!(e <= out_.debug_max_error)) out_.debug_max_error = e;
  }

  std::vector<int> order_;
  // The repair criterion (log det of the ridge matrix, by a factorisation
  // from the counts) of the design the repair started from and of the one it
  // holds; NaN when it could not be computed.
  double repair_log_det_start_ = NAN, repair_log_det_ = NAN;

 private:
  const Crossed& lay_;
  Budget& budget_;
  CrossedOut& out_;
  int interval_;
  int accepted_ = 0;
  double evaluation_work_;
  bool exact_floors_;
  double floor_d_ = 0.0, floor_a_ = 0.0;
  double joint_factor_ = 0.0, row_refresh_ = 0.0;
  std::vector<int> scratch_;
  // Ruling R9 (header comment).
  bool lazy_ = false;             // confirm a swap between rows only near a floor
  double margin_ld_ = kConfirmLd, margin_sum_ = kConfirmSum;
  double drift_ld_ = 0.25 * kConfirmLd, drift_sum_ = 0.25 * kConfirmSum;  // a quarter of the margins
  int period_ = kCheckpointSwaps;
  int pending_ = 0;               // unconfirmed swaps between rows since the last exact design
  bool row_drifted_ = false, breach_ = false;
  double exact_ld_ = -INFINITY;   // the best exact design and its running joint log det
  std::vector<int> exact_treat_;
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
// once. `floors` is 1, or a check hook of the header comment: 2 `slack_floors`,
// 3 `confirm_floors`, 4 `late_floors`.
cpp11::list crossed_engine(int v, int r, int b, const std::vector<int>& row_of,
                           const std::vector<int>& col_of, const std::vector<int>& start,
                           int jumps_per_escape, int max_stale_escapes, double budget_cap,
                           double time_limit, bool debug, double work_cap, int refresh_every,
                           double evaluation_work, int floors, bool treatment_form, bool strict) {
  const bool slack_floors = floors == 2;
  Crossed lay = make_crossed(v, r, b, row_of, col_of);
  lay.form = treatment_form;
  for (int m = 0; m < r; ++m) {
    std::vector<int> seen(v, 0);
    for (int p = m * v; p < (m + 1) * v; ++p) {
      if (start[p] < 0 || start[p] >= v || seen[start[p]]++ > 0) {
        cpp11::stop("Every replicate of `start` must hold each treatment once.");
      }
    }
  }
  CrossedSpace joint(v, r, lay.B, 2, lay.joint_block, start, 0.0, treatment_form);
  CrossedSpace row(v, r, lay.Br, 1, lay.row_block, start, 0.0, treatment_form);
  // The cheaper scoring of the header comment, where the joint state is built
  // from the treatment space.
  const bool cheap = joint.treatment_form();
  // The charge of the finish (header comment); the repair and the search run
  // under what the work limit leaves after it.
  const double finish_charged = cheap
      ? drift_work(lay.B, lay.n, 2) + joint.refresh_work() + joint.factor_work() + row.refresh_work()
      : 5.0 * joint.refresh_work() + 3.0 * row.refresh_work();
  Budget budget(budget_cap, time_limit, work_cap - finish_charged, evaluation_work, strict);
  double finish_work = 0.0;
  CrossedOut out;
  out.debug = debug;
  Engine eng(lay, budget, out, std::max(1, refresh_every), evaluation_work, !slack_floors);
  eng.order_.assign(lay.n, 0);

  eng.set_work(joint.factor_work(), row.refresh_work());
  // Full rank of a plain joint state that has just been refreshed.
  auto fresh_full = [&](const CrossedSpace& s) {
    if (!cheap) return s.full_rank();
    bool exact = false;
    const bool full = s.full_rank_refreshed(exact);
    if (exact) budget.force(s.factor_work());
    return full;
  };
  bool joint_ok = false, start_full = false, row_ok = false;
  if (!cheap) {
    budget.force(2.0 * refresh_work(lay.B, lay.n, 2, r) + refresh_work(lay.Br, lay.n, 1, r));
    joint_ok = joint.refresh();
    start_full = joint_ok && joint.full_rank();
    row_ok = row.refresh();
  } else {
    budget.force(joint.refresh_work() + row.refresh_work());
    joint_ok = joint.refresh();
    start_full = joint_ok && fresh_full(joint);
    row_ok = row.refresh();
  }
  out.refreshes += (joint_ok ? 1 : 0) + (row_ok ? 1 : 0);
  out.scoring_joint += 1;
  out.scoring_row += 1;
  const double start_work = budget.work_used();
  const double start_seconds = budget.elapsed();
  const double start_trace = joint_ok ? joint.trace() : NA_REAL;
  // Joint D of the given design, 0 when it is not full rank.
  const double stage1_joint_d = start_full ? joint.d_efficiency() : 0.0;
  const double stage1_row_d = row_ok ? row.d_efficiency() : NA_REAL;
  const double stage1_row_a = row_ok ? row.a_efficiency() : NA_REAL;
  const double floor_ld = row_ok ? row.log_det() : 0.0;
  const double floor_sum = row_ok ? row.trace() - lay.Br + v - 1.0 : 0.0;
  if (row_ok) eng.set_floors(stage1_row_d, stage1_row_a);
  const double row_diag = row_ok ? row.max_diag() : NA_REAL;

  // `entry` is the design the joint stage starts from: the start, or the
  // repaired design. `result` is what the call returns.
  std::vector<int> entry = start, result = start;
  bool entry_full = start_full;
  int floor_fallback = 0;
  double repair_log_det_start = NA_REAL, repair_log_det = NA_REAL;
  bool rng = false;
  // 0: the start was full rank; 1: the repair reached a full-rank design that
  // meets the Row floors; 2: the work limit or the time limit ended it first;
  // 3: it ended on its own without one.
  int repair_status = start_full ? 0 : 3;
  if (!start_full && row_ok && !budget.stopped()) {
    // Joint-rank repair, charged to the work cap only.
    CrossedSpace eps(v, r, lay.B, 2, lay.joint_block, start, kRepairEpsilon, lay.form);
    if (budget.work(eps.refresh_work()) && eps.refresh()) {
      out.refreshes += 1;
      double ld = 0.0;
      if (!cheap && eps.log_det_fresh(ld)) repair_log_det_start = ld;
      GetRNGstate();
      rng = true;
      bool repaired = false;
      eng.repair(eps, row, floor_ld, floor_sum, repaired);
      if (!cheap) {
        if (eps.log_det_fresh(ld)) repair_log_det = ld;
      } else {
        // The repair's own factorisations: the same arithmetic as a fresh one.
        if (std::isfinite(eng.repair_log_det_start_)) repair_log_det_start = eng.repair_log_det_start_;
        if (std::isfinite(eng.repair_log_det_)) repair_log_det = eng.repair_log_det_;
      }
      // The repair's output must meet the Row floors exactly; its last design
      // is returned when the repair failed and the design meets them. In the
      // cheap form every accepted repair swap was confirmed against them from
      // scratch, and a value from scratch depends on the design alone, so the
      // output is not scored again (the check hook has no such confirmation).
      const bool meets = !budget.stopped() && ((cheap && !slack_floors) || eng.meets_floors(eps.treatments()));
      if (repaired && !meets && !budget.stopped()) floor_fallback = 2;
      if (repaired && meets) repair_status = 1;
      if (meets) {
        result = eps.treatments();
        if (repaired) {
          // States built anew from the repaired design: the joint stage then
          // runs exactly as from a start equal to it.
          // If the work limit refuses them the repaired design is returned
          // as it is, scored by the finish.
          bool again = false;
          if (!cheap) {
            if (budget.work(2.0 * refresh_work(lay.B, lay.n, 2, r) + refresh_work(lay.Br, lay.n, 1, r))) {
              joint = CrossedSpace(v, r, lay.B, 2, lay.joint_block, result, 0.0, lay.form);
              row = CrossedSpace(v, r, lay.Br, 1, lay.row_block, result, 0.0, lay.form);
              again = joint.refresh() && joint.full_rank() && row.refresh();
            }
          } else if (budget.work(joint.refresh_work())) {
            // The Row state of the repair is kept (header comment).
            joint = CrossedSpace(v, r, lay.B, 2, lay.joint_block, result, 0.0, lay.form);
            again = joint.refresh() && fresh_full(joint);
            out.scoring_joint += 1;
          }
          if (again) {
            out.refreshes += cheap ? 1 : 2;
            entry = result;
            entry_full = true;
          }
        }
      }
    }
  }
  if (repair_status == 3 && budget.stopped()) repair_status = 2;
  bool searched = false;
  bool end_alarm = false;
  double search_end_seconds = -1.0;  // when the finish began
  // Row D and Row A of `result` from scratch, when they have been computed.
  RowValue result_row;
  bool result_row_known = false;
  if (entry_full && row_ok && !budget.stopped()) {
    searched = true;
    // A budget below one evaluation runs no search and draws nothing.
    if (!(budget_cap >= 1.0)) budget.spend(1.0);
    if (!rng) GetRNGstate();
    rng = true;
    // Ruling R9: in the cheap form a swap between rows is confirmed from
    // scratch only near a floor.
    const bool lazy = cheap && !slack_floors && floors != 3;
    if (lazy) eng.set_lazy(joint.log_det(), floor_ld, row_diag, floors == 4);
    double local_best = -INFINITY;
    std::vector<int> local_treat, exact_treat;
    std::unique_ptr<CrossedSpace> best_joint, best_row;
    int stale = 0;
    bool ok = true;
    while (ok && !budget.stopped()) {
      bool optimum = false;
      if (!eng.descend(joint, row, floor_ld, floor_sum, optimum)) { ok = false; break; }
      if (eng.take_breach()) {
        if (!eng.rollback(joint, row, entry)) { ok = false; break; }
        continue;
      }
      if (!optimum) continue;
      if (!eng.check(joint, row)) { ok = false; break; }
      if (joint.log_det() > local_best + kTie) {
        // Ruling R9: an optimum is recorded only when its design is exact.
        const Engine::Verdict verdict = eng.checkpoint(joint, row);
        if (verdict == Engine::kStopped) { ok = false; break; }
        if (verdict == Engine::kBreach) {
          if (!eng.rollback(joint, row, entry)) { ok = false; break; }
          continue;
        }
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
        if (!budget.work(static_cast<double>(lay.B) * (lay.B + v) + static_cast<double>(lay.Br) * (lay.Br + v))) {
          ok = false;
          break;
        }
        joint = *best_joint;
        row = *best_row;
        eng.restored();
      }
      if (!eng.escape(joint, row, jumps_per_escape)) { ok = false; break; }
    }
    search_end_seconds = budget.elapsed();
    // The current design is scored from scratch (it may lie mid-descent or
    // after an escape); the better of it and the best local optimum is kept.
    // Neither is below the entry design: descents only raise the joint log
    // det, and escapes are followed by a descent before any optimum is
    // recorded. Without a recorded optimum the current design has seen no
    // escape. In the cheap form the running log det stands for the score
    // from scratch when the running state passes one drift check.
    double current_ld = -INFINITY;
    if (cheap) finish_work += drift_work(lay.B, lay.n, 2);
    if (cheap && Engine::accurate(joint)) {
      current_ld = joint.log_det();
    } else {
      end_alarm = cheap;
      CrossedSpace current(v, r, lay.B, 2, lay.joint_block, joint.treatments(), 0.0, lay.form);
      current_ld = current.refresh() ? current.log_det() : -INFINITY;
      out.scoring_joint += 1;
      finish_work += current.refresh_work();
    }
    if (!local_treat.empty() && !(current_ld > local_best)) {
      result = local_treat;
    } else if (std::isfinite(current_ld)) {
      result = joint.treatments();
    }
    // The exact Row guarantee: Row D and Row A of the result, from scratch,
    // are not below the stage 1 values. On a breach: the best recorded
    // optimum that meets them exactly (ruling R9: the best exact design),
    // else the entry design (it meets them).
    if (lazy) exact_treat = eng.exact_treat();
    const RowValue rv = row_value(lay, result);
    out.scoring_row += 1;
    finish_work += row.refresh_work();
    result_row = rv;
    result_row_known = true;
    if (!(rv.ok && rv.d >= stage1_row_d && rv.a >= stage1_row_a)) {
      floor_fallback = 1;
      result = exact_treat.empty() ? entry : exact_treat;
      const RowValue again = row_value(lay, result);
      out.scoring_row += 1;
      finish_work += row.refresh_work();
      result_row = again;
      if (!(again.ok && again.d >= stage1_row_d && again.a >= stage1_row_a)) {
        result = entry;
        result_row_known = false;
      }
    }
  }
  if (rng) PutRNGstate();
  if (search_end_seconds < 0.0) search_end_seconds = budget.elapsed();
  // Final values from scratch; a result that fails the full-rank test is
  // replaced by the entry design (which passed it).
  CrossedSpace fin(v, r, lay.B, 2, lay.joint_block, result, 0.0, lay.form);
  // Scores `fin` and counts the work of it.
  auto score_fin = [&]() {
    out.scoring_joint += 1;
    finish_work += fin.refresh_work();
    if (!fin.refresh()) return false;
    if (!cheap) {
      finish_work += fin.factor_work();
      return fin.full_rank();
    }
    bool exact = false;
    const bool full = fin.full_rank_refreshed(exact);
    if (exact) finish_work += fin.factor_work();
    return full;
  };
  bool fin_full = score_fin();
  if (searched && !fin_full) {
    result = entry;
    result_row_known = false;
    fin = CrossedSpace(v, r, lay.B, 2, lay.joint_block, result, 0.0, lay.form);
    fin_full = score_fin();
  }
  // The Row values reported: in the cheap form those of the exact comparison
  // above when they are of this design, else (and always in the block-space
  // form) computed now.
  RowValue fin_row;
  if (cheap && result_row_known) {
    fin_row = result_row;
  } else {
    fin_row = row_value(lay, result);
    out.scoring_row += 1;
    finish_work += row.refresh_work();
  }
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
      "even_repairs"_nm = 0, "work"_nm = budget.work_used() + finish_charged, "work_capped"_nm = budget.capped(),
      "debug_max_error"_nm = debug ? out.debug_max_error : NA_REAL,
      "stage1_row_d"_nm = stage1_row_d, "stage1_row_a"_nm = stage1_row_a,
      "row_d"_nm = fin_row.ok ? fin_row.d : NA_REAL,
      "row_a"_nm = fin_row.ok ? fin_row.a : NA_REAL,
      "joint_d"_nm = joint_d, "joint_a"_nm = joint_a, "joint_log_det"_nm = joint_ld,
      "full_rank"_nm = fin_full, "repair_swaps"_nm = out.repair_swaps,
      "repair_evaluations"_nm = out.repair_evaluations,
      "repair_log_det_start"_nm = repair_log_det_start, "repair_log_det"_nm = repair_log_det,
      "repair_score_error"_nm = out.repair_score_error, "floor_fallback"_nm = floor_fallback,
      "treatment_form"_nm = cheap, "start_full_rank"_nm = start_full, "start_trace"_nm = start_trace,
      "accepted"_nm = out.accepted, "stage1_joint_d"_nm = stage1_joint_d,
      "row_swaps"_nm = out.row_swaps, "row_confirmations"_nm = out.row_confirmations,
      "row_checkpoints"_nm = out.row_checkpoints, "row_rollbacks"_nm = out.row_rollbacks,
      "row_drift_ld"_nm = out.row_drift_ld, "row_drift_sum"_nm = out.row_drift_sum,
      "row_diag"_nm = row_diag,
      "scoring_joint"_nm = out.scoring_joint,
      "scoring_row"_nm = out.scoring_row, "end_alarm"_nm = end_alarm,
      "start_work"_nm = start_work, "finish_charged"_nm = finish_charged,
      "finish_work"_nm = finish_work, "repair_status"_nm = repair_status,
      "start_seconds"_nm = start_seconds, "finish_seconds"_nm = budget.elapsed() - search_end_seconds});
}
