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
  int v_, b_, r_;
  double n_;
  std::vector<int> counts_;   // v x b, column-major
  std::vector<int> sizes_;    // block sizes k_j
  std::vector<double> minv_;  // b x b
  mutable std::vector<double> h_;  // scratch for delta(), size b
  double log_det_;
};

// In-place inverse of a symmetric positive-definite n x n matrix (column-major)
// by Cholesky. Returns false if not positive definite; log_det receives log det.
bool spd_inverse(std::vector<double>& a, int n, double& log_det);
