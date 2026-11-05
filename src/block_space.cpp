// Block-space information state (derivation.md sections 4-5, gate G0).
#include "block_space.h"
#include <cmath>
#include <limits>

bool spd_inverse(std::vector<double>& a, int n, double& log_det) {
  std::vector<double> l(static_cast<size_t>(n) * n, 0.0);
  log_det = 0.0;
  for (int j = 0; j < n; ++j) {
    double d = a[j + n * j];
    for (int k = 0; k < j; ++k) d -= l[j + n * k] * l[j + n * k];
    // Relative pivot test: a disconnected design leaves a rounding-level pivot.
    if (!std::isfinite(d) || d <= 1e-9 * std::fabs(a[j + n * j])) return false;
    double ljj = std::sqrt(d);
    l[j + n * j] = ljj;
    log_det += 2.0 * std::log(ljj);
    for (int i = j + 1; i < n; ++i) {
      double s = a[i + n * j];
      for (int k = 0; k < j; ++k) s -= l[i + n * k] * l[j + n * k];
      l[i + n * j] = s / ljj;
    }
  }
  // Invert L (lower triangular) in place into li, then A^{-1} = li' li.
  std::vector<double> li(static_cast<size_t>(n) * n, 0.0);
  for (int j = 0; j < n; ++j) {
    li[j + n * j] = 1.0 / l[j + n * j];
    for (int i = j + 1; i < n; ++i) {
      double s = 0.0;
      for (int k = j; k < i; ++k) s -= l[i + n * k] * li[k + n * j];
      li[i + n * j] = s / l[i + n * i];
    }
  }
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j <= i; ++j) {
      double s = 0.0;
      for (int k = i; k < n; ++k) s += li[k + n * i] * li[k + n * j];
      a[i + n * j] = s;
      a[j + n * i] = s;
    }
  }
  return true;
}

BlockSpace::BlockSpace(int v, int b, int r, const std::vector<int>& counts)
    : v_(v), b_(b), r_(r), n_(static_cast<double>(v) * r), counts_(counts),
      sizes_(b, 0), minv_(static_cast<size_t>(b) * b, 0.0), h_(b, 0.0),
      log_det_(std::numeric_limits<double>::quiet_NaN()) {
  for (int j = 0; j < b; ++j)
    for (int i = 0; i < v; ++i) sizes_[j] += counts_[i + v * j];
}

bool BlockSpace::refresh() {
  // Build and invert into scratch; the state changes only on success.
  std::vector<double> bh(static_cast<size_t>(b_) * b_);
  for (int p = 0; p < b_; ++p) {
    for (int q = 0; q <= p; ++q) {
      double s = 0.0;
      for (int i = 0; i < v_; ++i)
        s += static_cast<double>(counts_[i + v_ * p]) * counts_[i + v_ * q];
      double value = (p == q ? sizes_[p] : 0.0) - s / r_ + 1.0 / b_;
      bh[p + b_ * q] = value;
      bh[q + b_ * p] = value;
    }
  }
  double ld = 0.0;
  if (!spd_inverse(bh, b_, ld)) return false;
  minv_.swap(bh);
  log_det_ = ld;
  return true;
}

void BlockSpace::h_vector(int a, int c, std::vector<double>& h) const {
  h.assign(b_, 0.0);
  for (int j = 0; j < b_; ++j)
    h[j] = static_cast<double>(counts_[a + v_ * j] - counts_[c + v_ * j]) / r_;
}

double BlockSpace::delta(int a, int p, int c, int q) const {
  std::vector<double>& h = h_;
  for (int j = 0; j < b_; ++j)
    h[j] = static_cast<double>(counts_[a + v_ * j] - counts_[c + v_ * j]) / r_;
  const double t = 2.0 / r_;
  double alpha = minv_[q + b_ * q] + minv_[p + b_ * p] - 2.0 * minv_[p + b_ * q];
  double mh_p = 0.0, mh_q = 0.0, gamma = 0.0;
  for (int j = 0; j < b_; ++j) {
    if (h[j] == 0.0) continue;
    mh_p += minv_[p + b_ * j] * h[j];
    mh_q += minv_[q + b_ * j] * h[j];
    for (int l = 0; l < b_; ++l)
      if (h[l] != 0.0) gamma += h[l] * minv_[l + b_ * j] * h[j];
  }
  double beta = mh_q - mh_p;
  return (1.0 - beta) * (1.0 - beta) - alpha * (t + gamma);
}

void BlockSpace::apply(int a, int p, int c, int q, double ratio) {
  std::vector<double> h;
  h_vector(a, c, h);
  const double t = 2.0 / r_;
  std::vector<double> mw(b_), mh(b_, 0.0);
  for (int i = 0; i < b_; ++i) {
    mw[i] = minv_[i + b_ * q] - minv_[i + b_ * p];
    for (int j = 0; j < b_; ++j) mh[i] += minv_[i + b_ * j] * h[j];
  }
  double alpha = mw[q] - mw[p];
  double beta = mh[q] - mh[p];
  double gamma = 0.0;
  for (int j = 0; j < b_; ++j) gamma += h[j] * mh[j];
  // W = (T^{-1} - Q)^{-1}, T^{-1} = [[0, 1], [1, -t]], det(T^{-1} - Q) = -ratio.
  double det = alpha * (t + gamma) - (1.0 - beta) * (1.0 - beta);
  double w00 = (-t - gamma) / det, w01 = -(1.0 - beta) / det, w11 = -alpha / det;
  for (int i = 0; i < b_; ++i)
    for (int j = 0; j < b_; ++j)
      minv_[i + b_ * j] += w00 * mw[i] * mw[j] + w01 * (mw[i] * mh[j] + mh[i] * mw[j]) +
                           w11 * mh[i] * mh[j];
  counts_[a + v_ * p] -= 1; counts_[a + v_ * q] += 1;
  counts_[c + v_ * q] -= 1; counts_[c + v_ * p] += 1;
  log_det_ += std::log(ratio);
}

double BlockSpace::d_efficiency() const {
  double s = 0.0;
  for (int j = 0; j < b_; ++j) s += std::log(static_cast<double>(sizes_[j]));
  return std::exp((std::log(n_ / b_) - s + log_det_) / (v_ - 1));
}

double BlockSpace::a_efficiency() const {
  // S + y y', S = K^{-1/2} B K^{-1/2}, y = (k / n)^{1/2}; tr(S^+) = tr((S + y y')^{-1}) - 1.
  std::vector<double> s(static_cast<size_t>(b_) * b_);
  for (int p = 0; p < b_; ++p) {
    for (int q = 0; q <= p; ++q) {
      double nn = 0.0;
      for (int i = 0; i < v_; ++i)
        nn += static_cast<double>(counts_[i + v_ * p]) * counts_[i + v_ * q];
      double bpq = (p == q ? sizes_[p] : 0.0) - nn / r_;
      double value = bpq / std::sqrt(static_cast<double>(sizes_[p]) * sizes_[q]) +
                     std::sqrt(sizes_[p] / n_) * std::sqrt(sizes_[q] / n_);
      s[p + b_ * q] = value;
      s[q + b_ * p] = value;
    }
  }
  double ld = 0.0;
  if (!spd_inverse(s, b_, ld)) return 0.0;
  double tr = -1.0;
  for (int j = 0; j < b_; ++j) tr += s[j + b_ * j];
  return (v_ - 1.0) / ((v_ - b_) + tr);
}

double BlockSpace::drift() const {
  std::vector<double> bh(static_cast<size_t>(b_) * b_);
  for (int p = 0; p < b_; ++p)
    for (int q = 0; q < b_; ++q) {
      double s = 0.0;
      for (int i = 0; i < v_; ++i)
        s += static_cast<double>(counts_[i + v_ * p]) * counts_[i + v_ * q];
      bh[p + b_ * q] = (p == q ? sizes_[p] : 0.0) - s / r_ + 1.0 / b_;
    }
  double worst = 0.0;
  for (int i = 0; i < b_; ++i)
    for (int j = 0; j < b_; ++j) {
      double s = 0.0;
      for (int l = 0; l < b_; ++l) s += bh[i + b_ * l] * minv_[l + b_ * j];
      double e = std::fabs(s - (i == j ? 1.0 : 0.0));
      if (e > worst) worst = e;
    }
  return worst;
}
