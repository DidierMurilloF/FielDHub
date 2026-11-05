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
  // max(1, |(1 - beta)^2|, |alpha (t + gamma)|) of the last delta(): the size of
  // the two terms whose difference is the ratio, so its rounding error is
  // relative to this scale (derivation section 8, eps_rel).
  double last_scale() const;
  // Apply that swap, given its ratio from delta() (must be > 1e-9).
  void apply(int a, int p, int c, int q, double ratio);
  double log_det() const { return log_det_; }
  double d_efficiency() const;   // geometric mean of nonzero eigenvalues of C over r
  double a_efficiency() const;   // harmonic mean of nonzero eigenvalues of C over r; O(b^3)
  double drift() const;          // max |Bhat Minv - I|
  int count(int i, int j) const { return counts_[i + v_ * j]; }
  const std::vector<int>& counts() const { return counts_; }

 private:
  void h_vector(int a, int c, std::vector<double>& h) const;
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
  mutable std::vector<double> h_;  // scratch for delta(), size b
  mutable double first_ = 0.0, second_ = 0.0;  // the two terms of the last delta()
  double log_det_;
};

// In-place inverse of a symmetric positive-definite n x n matrix (column-major)
// by Cholesky. Returns false if not positive definite; log_det receives log det.
bool spd_inverse(std::vector<double>& a, int n, double& log_det);
