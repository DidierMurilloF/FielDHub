// Block-space information state (derivation.md sections 4-5, gate G0).
#include "block_space.h"
#include <algorithm>
#include <cmath>
#include <limits>

bool spd_inverse(std::vector<double>& a, int n, double& log_det) {
  // L is stored row-major (l[i * n + k] = L_ik) so the inner products run over
  // contiguous memory; the arithmetic and its order are those of the textbook
  // column-oriented Cholesky.
  std::vector<double> l(static_cast<size_t>(n) * n, 0.0);
  log_det = 0.0;
  for (int j = 0; j < n; ++j) {
    const double* lj = &l[static_cast<size_t>(j) * n];
    double d = a[j + static_cast<size_t>(n) * j];
    for (int k = 0; k < j; ++k) d -= lj[k] * lj[k];
    // Relative pivot test: a disconnected design leaves a rounding-level pivot.
    if (!std::isfinite(d) || d <= 1e-9 * std::fabs(a[j + static_cast<size_t>(n) * j])) return false;
    double ljj = std::sqrt(d);
    l[static_cast<size_t>(j) * n + j] = ljj;
    log_det += 2.0 * std::log(ljj);
    for (int i = j + 1; i < n; ++i) {
      const double* li_row = &l[static_cast<size_t>(i) * n];
      double s = a[i + static_cast<size_t>(n) * j];
      for (int k = 0; k < j; ++k) s -= li_row[k] * lj[k];
      l[static_cast<size_t>(i) * n + j] = s / ljj;
    }
  }
  // Invert L (lower triangular) into li (column-major), then A^{-1} = li' li.
  std::vector<double> li(static_cast<size_t>(n) * n, 0.0);
  for (int j = 0; j < n; ++j) {
    li[j + static_cast<size_t>(n) * j] = 1.0 / l[static_cast<size_t>(j) * n + j];
    for (int i = j + 1; i < n; ++i) {
      const double* l_row = &l[static_cast<size_t>(i) * n];
      double s = 0.0;
      for (int k = j; k < i; ++k) s -= l_row[k] * li[k + static_cast<size_t>(n) * j];
      li[i + static_cast<size_t>(n) * j] = s / l_row[i];
    }
  }
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j <= i; ++j) {
      double s = 0.0;
      for (int k = i; k < n; ++k) s += li[k + static_cast<size_t>(n) * i] * li[k + static_cast<size_t>(n) * j];
      a[i + static_cast<size_t>(n) * j] = s;
      a[j + static_cast<size_t>(n) * i] = s;
    }
  }
  return true;
}

BlockSpace::BlockSpace(int v, int b, int r, const std::vector<int>& counts)
    : v_(v), b_(b), r_(r), n_(static_cast<double>(v) * r), counts_(counts),
      sizes_(b, 0), blocks_of_(static_cast<size_t>(v) * r, 0),
      minv_(static_cast<size_t>(b) * b, 0.0), h_(b, 0.0),
      log_det_(std::numeric_limits<double>::quiet_NaN()) {
  std::vector<int> filled(v, 0);
  for (int j = 0; j < b; ++j) {
    for (int i = 0; i < v; ++i) {
      int c = counts_[i + v * j];
      sizes_[j] += c;
      for (int x = 0; x < c && filled[i] < r; ++x) blocks_of_[static_cast<size_t>(i) * r + filled[i]++] = j;
    }
  }
}

void BlockSpace::cross_counts(std::vector<double>& nn) const {
  nn.assign(static_cast<size_t>(b_) * b_, 0.0);
  for (int i = 0; i < v_; ++i) {
    const int* bl = &blocks_of_[static_cast<size_t>(i) * r_];
    for (int x = 0; x < r_; ++x)
      for (int y = 0; y < r_; ++y) nn[bl[x] + static_cast<size_t>(b_) * bl[y]] += 1.0;
  }
}

void BlockSpace::update_counts(int a, int p, int c, int q) {
  counts_[a + v_ * p] -= 1; counts_[a + v_ * q] += 1;
  counts_[c + v_ * q] -= 1; counts_[c + v_ * p] += 1;
  int* ba = &blocks_of_[static_cast<size_t>(a) * r_];
  for (int x = 0; x < r_; ++x) if (ba[x] == p) { ba[x] = q; break; }
  int* bc = &blocks_of_[static_cast<size_t>(c) * r_];
  for (int x = 0; x < r_; ++x) if (bc[x] == q) { bc[x] = p; break; }
}



bool BlockSpace::refresh() {
  // Build and invert into scratch; the state changes only on success.
  std::vector<double> bh;
  cross_counts(bh);
  for (int p = 0; p < b_; ++p) {
    for (int q = 0; q <= p; ++q) {
      double value = (p == q ? sizes_[p] : 0.0) - bh[p + static_cast<size_t>(b_) * q] / r_ + 1.0 / b_;
      bh[p + static_cast<size_t>(b_) * q] = value;
      bh[q + static_cast<size_t>(b_) * p] = value;
    }
  }
  double ld = 0.0;
  if (!spd_inverse(bh, b_, ld)) return false;
  minv_.swap(bh);
  log_det_ = ld;
  if (cached_) rebuild_g();
  return true;
}

void BlockSpace::enable_cache() {
  cached_ = true;
  g_.assign(static_cast<size_t>(b_) * v_, 0.0);
}

void BlockSpace::rebuild_g() {
  std::fill(g_.begin(), g_.end(), 0.0);
  for (int i = 0; i < v_; ++i) {
    double* gi = &g_[static_cast<size_t>(b_) * i];
    const int* bl = &blocks_of_[static_cast<size_t>(i) * r_];
    for (int x = 0; x < r_; ++x) {
      const double* m = &minv_[static_cast<size_t>(b_) * bl[x]];
      for (int j = 0; j < b_; ++j) gi[j] += m[j];
    }
    for (int j = 0; j < b_; ++j) gi[j] /= r_;
  }
}

double BlockSpace::delta_cached(int a, int p, int c, int q) const {
  const double* ga = &g_[static_cast<size_t>(b_) * a];
  const double* gc = &g_[static_cast<size_t>(b_) * c];
  const int* ba = &blocks_of_[static_cast<size_t>(a) * r_];
  const int* bc = &blocks_of_[static_cast<size_t>(c) * r_];
  double alpha = minv_[q + b_ * q] + minv_[p + b_ * p] - 2.0 * minv_[p + b_ * q];
  double beta = (ga[q] - gc[q]) - (ga[p] - gc[p]);
  double gamma = 0.0;
  for (int x = 0; x < r_; ++x) gamma += (ga[ba[x]] - gc[ba[x]]) - (ga[bc[x]] - gc[bc[x]]);
  first_ = (1.0 - beta) * (1.0 - beta);
  second_ = alpha * (2.0 + gamma) / r_;
  return first_ - second_;
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
  first_ = (1.0 - beta) * (1.0 - beta);
  second_ = alpha * (t + gamma);
  return first_ - second_;
}

double BlockSpace::last_scale() const {
  return std::max(1.0, std::max(std::fabs(first_), std::fabs(second_)));
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
  if (cached_) {
    // G' = Minv' N' / r with N'' = N' + w u' (w = e_q - e_p, u = e_a - e_c):
    // G' = G + [mw mh] W [w'G; h'G] + (Minv' w) u' / r.
    const int* ba = &blocks_of_[static_cast<size_t>(a) * r_];
    const int* bc = &blocks_of_[static_cast<size_t>(c) * r_];
    for (int i = 0; i < v_; ++i) {
      double* gi = &g_[static_cast<size_t>(b_) * i];
      double e1 = gi[q] - gi[p], e2 = 0.0;
      for (int x = 0; x < r_; ++x) e2 += gi[ba[x]] - gi[bc[x]];
      e2 /= r_;
      double c1 = w00 * e1 + w01 * e2, c2 = w01 * e1 + w11 * e2;
      for (int j = 0; j < b_; ++j) gi[j] += c1 * mw[j] + c2 * mh[j];
    }
    double* ga = &g_[static_cast<size_t>(b_) * a];
    double* gc = &g_[static_cast<size_t>(b_) * c];
    for (int j = 0; j < b_; ++j) {
      double d = (minv_[j + b_ * q] - minv_[j + b_ * p]) / r_;
      ga[j] += d;
      gc[j] -= d;
    }
  }
  update_counts(a, p, c, q);
  log_det_ += std::log(ratio);
}

double BlockSpace::d_efficiency() const {
  double s = 0.0;
  for (int j = 0; j < b_; ++j) s += std::log(static_cast<double>(sizes_[j]));
  return std::exp((std::log(n_ / b_) - s + log_det_) / (v_ - 1));
}

double BlockSpace::a_efficiency() const {
  // S + y y', S = K^{-1/2} B K^{-1/2}, y = (k / n)^{1/2}; tr(S^+) = tr((S + y y')^{-1}) - 1.
  std::vector<double> s;
  cross_counts(s);
  for (int p = 0; p < b_; ++p) {
    for (int q = 0; q <= p; ++q) {
      double nn = s[p + static_cast<size_t>(b_) * q];
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
  std::vector<double> bh;
  cross_counts(bh);
  for (int p = 0; p < b_; ++p)
    for (int q = 0; q < b_; ++q)
      bh[p + static_cast<size_t>(b_) * q] =
          (p == q ? sizes_[p] : 0.0) - bh[p + static_cast<size_t>(b_) * q] / r_ + 1.0 / b_;
  double worst = 0.0;
  for (int i = 0; i < b_; ++i)
    for (int j = 0; j < b_; ++j) {
      // Bhat is symmetric, so row i is read as column i (contiguous).
      double s = 0.0;
      const double* bi = &bh[static_cast<size_t>(b_) * i];
      const double* mj = &minv_[static_cast<size_t>(b_) * j];
      for (int l = 0; l < b_; ++l) s += bi[l] * mj[l];
      double e = std::fabs(s - (i == j ? 1.0 : 0.0));
      if (e > worst) worst = e;
    }
  return worst;
}
