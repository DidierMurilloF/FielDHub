// Block-space information state for one block factor (derivation.md sections 4-5).
// B = K - N' N / r, Bhat = B + J/b, Minv = Bhat^{-1}. Equal replication r.
#pragma once
#include <vector>

class BlockSpace {
 public:
  BlockSpace(int v, int b, int r, const std::vector<int>& counts);
  // Rebuild Bhat, Minv and log det(Bhat) from the counts. False if Bhat is not
  // positive definite (design disconnected).
  bool refresh();
  // Determinant ratio det(Bhat') / det(Bhat) for moving one plot of treatment a
  // from block p to block q and one plot of treatment c from q to p.
  double delta(int a, int p, int c, int q) const;
  // Maintain G = Minv N' / r (b x v) from now on (rebuilt by refresh(), updated
  // by apply()), so that delta_cached() costs O(r) instead of O(b + r^2).
  void enable_cache();
  // The same ratio as delta(), read from Minv and G: with h = (N_a. - N_c.) / r,
  // Minv h = G_.a - G_.c, so beta is four entries of G and gamma is a sum over
  // the 2 r plots of a and c. Requires enable_cache() and a successful refresh().
  double delta_cached(int a, int p, int c, int q) const;
  // max(1, |(1 - beta)^2|, |alpha (t + gamma)|) of the last delta(): the size of
  // the two terms whose difference is the ratio, so its rounding error is
  // relative to this scale (derivation section 8, eps_rel).
  double last_scale() const;
  // Apply that swap, given its ratio from delta() (must be > 1e-9).
  void apply(int a, int p, int c, int q, double ratio);
  double log_det() const { return log_det_; }
  double d_efficiency() const;   // geometric mean of nonzero eigenvalues of C over r
  double a_efficiency() const;   // harmonic mean of nonzero eigenvalues of C over r; O(b^3)
  double drift() const;          // max |Bhat Minv - I|, dense O(b^3)
  // The same quantity from the sparse form of Bhat (K - N'N / r + J / b) in
  // O(b (b + n)); its rounding differs from drift(), so single-level mode keeps
  // drift().
  double drift_sparse() const;
  // max |G - Minv N' / r| over the cache, O(b n); 0 without a cache. G is
  // updated from its own entries, so it gathers rounding error independently of
  // Minv, and the cached ratios and the running log determinant are read from
  // it: drift_sparse() alone does not vouch for them.
  double drift_cache() const;
  // A-efficiency from the trace of Minv when every block has the same size k.
  // With S = B / k and y y' = J / b as in a_efficiency(), Bhat / k = S + J / (b k),
  // which is not S + y y'. Both add a multiple of J to S, whose null vector is
  // 1, and tr((S + c J)^{-1}) = tr(S^+) + 1 / (c b); so tr(S^+) =
  // tr((S + y y')^{-1}) - 1 = k tr(Minv) - k = k (tr(Minv) - 1). Needs a fresh
  // Minv; equal to a_efficiency() up to rounding, without its inversion.
  double a_efficiency_equal_sizes() const;
  bool cached() const { return cached_; }
  // Invert with spd_inverse8() from now on (resolvable mode only).
  void enable_fast_inverse() { fast_inverse_ = true; }
  // Build the state from the treatment space from now on (phase 2 spec 5.1c),
  // when there are fewer treatments than blocks and every block has the same
  // size k; otherwise nothing changes. With N the v x b counts,
  //   S = I_v - N N' / (r k) + J / v   (v x v; C / r on the contrasts, 1 along 1),
  //   Bhat^-1 = (I_b + N' S^-1 N / (r k) + (k - 2) J / b) / k,
  //   log det Bhat = (b - 1) log k + log det S,
  // which is the spec's Bhat^-1 = (M + (k - 1) yhat yhat') / k with
  // M = I_b + N' S^-1 N / (r k) - J / b and yhat yhat' = J / b. refresh() then
  // costs one v x v inverse and sparse products of order n (v + b), not b^3;
  // Minv, the cache and every swap update are unchanged. Used by the stage 1
  // steps of the onestage search above phase 1's block limit, never by phase
  // 1's own callers. `tools/check-onestage-engine.R treatment_form` verifies
  // the identity against the one-way eigenvalues.
  void enable_treatment_form();
  bool treatment_form() const { return form_; }
  const std::vector<int>& counts() const { return counts_; }

 private:
  // The nonzero entries of h = (N_a. - N_c.) / r in ascending block order (at
  // most 2 r) into hz_j_ / hz_v_; returns their number. Sums over them run in
  // the order of a dense loop over all blocks that skips zeros, so delta() and
  // apply() give the same bits as their dense forms.
  int h_sparse(int a, int c) const;
  void update_counts(int a, int p, int c, int q);
  // N'N from the per-treatment block lists in O(v r^2): entry (p, q) is
  // sum_i N_ip N_iq, an exact integer in double, so the order of summation
  // does not matter.
  void cross_counts(std::vector<double>& nn) const;
  int v_, b_, r_;
  double n_;
  std::vector<int> counts_;   // v x b, column-major
  std::vector<int> sizes_;    // block sizes k_j
  std::vector<int> blocks_of_;  // v x r: the block of each plot of each treatment
  std::vector<double> minv_;  // b x b
  bool cached_ = false;
  bool fast_inverse_ = false;
  bool form_ = false;
  bool refresh_treatment_form();
  std::vector<double> g_;     // b x v, column i = Minv N_i.' / r (only if cached_)
  void rebuild_g();
  mutable std::vector<int> hz_j_;     // scratch for h_sparse(), size 2 r
  mutable std::vector<double> hz_v_;
  std::vector<double> mw_, mh_;       // scratch for apply(), size b
  mutable double first_ = 0.0, second_ = 0.0;  // the two terms of the last delta()
  double log_det_;
};

// In-place inverse of a symmetric positive-definite n x n matrix (column-major)
// by Cholesky. Returns false if not positive definite; log_det receives log det.
bool spd_inverse(std::vector<double>& a, int n, double& log_det);
bool spd_inverse8(std::vector<double>& a, int n, double& log_det);
// Cholesky factorisation of the symmetric n x n matrix a (column-major, not
// changed) with eight-way dot products. Returns false unless every pivot is
// positive (a is positive definite); log_det receives log det a.
bool spd_cholesky8(const std::vector<double>& a, int n, double& log_det);
