// Block-space state of crossed block factors (derivation in crossed_space.h).
#include "crossed_space.h"
#include "block_space.h"  // spd_inverse8()
#include <algorithm>
#include <cmath>
#include <limits>

namespace {

const double kFullRank = 1e-7;  // spec 5.1b: smallest canonical efficiency

// True if the symmetric n x n matrix a (column-major) is positive definite:
// a Cholesky factorisation, in place, with every pivot positive.
bool cholesky_pd(std::vector<double>& a, int n) {
  for (int j = 0; j < n; ++j) {
    double* aj = &a[static_cast<size_t>(n) * j];
    double d = aj[j];
    for (int k = 0; k < j; ++k) d -= a[j + static_cast<size_t>(n) * k] * a[j + static_cast<size_t>(n) * k];
    if (!std::isfinite(d) || d <= 0.0) return false;
    const double ljj = std::sqrt(d);
    aj[j] = ljj;
    for (int i = j + 1; i < n; ++i) {
      double s = aj[i];
      for (int k = 0; k < j; ++k) s -= a[i + static_cast<size_t>(n) * k] * a[j + static_cast<size_t>(n) * k];
      aj[i] = s / ljj;
    }
  }
  return true;
}

// log det of the positive definite a (column-major), which is destroyed.
bool cholesky_log_det(std::vector<double>& a, int n, double& log_det) {
  if (!cholesky_pd(a, n)) return false;
  log_det = 0.0;
  for (int j = 0; j < n; ++j) log_det += 2.0 * std::log(a[j + static_cast<size_t>(n) * j]);
  return true;
}

}  // namespace

CrossedSpace::CrossedSpace(int v, int r, int B, int F, const std::vector<int>& plot_block,
                           const std::vector<int>& treat, double ridge)
    : v_(v), r_(r), B_(B), F_(F), n_(v * r), ridge_(ridge), plot_block_(plot_block), wt_(B, 0.0), sq_(B, 0.0),
      treat_(treat), lists_(static_cast<size_t>(v) * F * r, 0),
      m_(static_cast<size_t>(B) * B, 0.0), g_(static_cast<size_t>(B) * v, 0.0),
      mu_(B, 0.0), mg_(B, 0.0), log_det_(std::numeric_limits<double>::quiet_NaN()) {
  std::vector<int> size(B, 0);
  for (int p = 0; p < n_; ++p)
    for (int f = 0; f < F_; ++f) size[plot_block_[static_cast<size_t>(p) * F_ + f]] += 1;
  for (int j = 0; j < B_; ++j) {
    sq_[j] = std::sqrt(static_cast<double>(size[j]));
    wt_[j] = size[j] > 0 ? 1.0 / sq_[j] : 0.0;
  }
  const int len = F_ * r_;
  std::vector<int> fill(v_, 0);
  for (int p = 0; p < n_; ++p) {
    const int i = treat_[p];
    for (int f = 0; f < F_; ++f) lists_[static_cast<size_t>(i) * len + fill[i]++] = plot_block_[static_cast<size_t>(p) * F_ + f];
  }
}

void CrossedSpace::build_what(const std::vector<int>& lists, std::vector<double>& w, double diagonal) const {
  w.assign(static_cast<size_t>(B_) * B_, 0.0);
  for (int l = 0; l < B_; ++l)
    for (int j = 0; j < B_; ++j) w[j + static_cast<size_t>(B_) * l] = (j == l ? diagonal : 0.0) + sq_[j] * sq_[l] / n_;
  const int len = F_ * r_;
  for (int i = 0; i < v_; ++i) {
    const int* li = &lists[static_cast<size_t>(i) * len];
    for (int x = 0; x < len; ++x)
      for (int y = 0; y < len; ++y) w[li[x] + static_cast<size_t>(B_) * li[y]] -= wt_[li[x]] * wt_[li[y]] / r_;
  }
}

bool CrossedSpace::refresh() {
  std::vector<double> w;
  build_what(lists_, w, 1.0 + ridge_);
  double ld = 0.0;
  if (!spd_inverse8(w, B_, ld)) return false;
  m_.swap(w);
  log_det_ = ld;
  std::fill(g_.begin(), g_.end(), 0.0);
  const int len = F_ * r_;
  for (int i = 0; i < v_; ++i) {
    double* gi = &g_[static_cast<size_t>(B_) * i];
    const int* li = &lists_[static_cast<size_t>(i) * len];
    for (int x = 0; x < len; ++x) {
      const double* mj = &m_[static_cast<size_t>(B_) * li[x]];
      const double wj = wt_[li[x]];
      for (int j = 0; j < B_; ++j) gi[j] += wj * mj[j];
    }
  }
  return true;
}

bool CrossedSpace::full_rank() const {
  std::vector<double> w;
  build_what(lists_, w, 1.0);
  for (int j = 0; j < B_; ++j) w[j + static_cast<size_t>(B_) * j] -= kFullRank;
  return cholesky_pd(w, B_);
}

bool CrossedSpace::log_det_fresh(double& log_det) const {
  std::vector<double> w;
  build_what(lists_, w, 1.0 + ridge_);
  return cholesky_log_det(w, B_, log_det);
}

bool CrossedSpace::log_det_after(int p, int q, double& log_det) const {
  std::vector<double> w;
  build_what(lists_after(p, q), w, 1.0 + ridge_);
  return cholesky_log_det(w, B_, log_det);
}

bool CrossedSpace::full_rank_after(int p, int q) const {
  std::vector<double> w;
  build_what(lists_after(p, q), w, 1.0);
  for (int j = 0; j < B_; ++j) w[j + static_cast<size_t>(B_) * j] -= kFullRank;
  return cholesky_pd(w, B_);
}

std::vector<int> CrossedSpace::lists_after(int p, int q) const {
  std::vector<int> lists = lists_;
  const int a = treat_[p], c = treat_[q], len = F_ * r_;
  for (int f = 0; f < F_; ++f) {
    const int bp = plot_block_[static_cast<size_t>(p) * F_ + f], bq = plot_block_[static_cast<size_t>(q) * F_ + f];
    if (bp == bq) continue;
    int* la = &lists[static_cast<size_t>(a) * len];
    for (int x = 0; x < len; ++x) if (la[x] == bp) { la[x] = bq; break; }
    int* lc = &lists[static_cast<size_t>(c) * len];
    for (int x = 0; x < len; ++x) if (lc[x] == bq) { lc[x] = bp; break; }
  }
  return lists;
}

int CrossedSpace::u_sparse(int p, int q) const {
  int nz = 0;
  for (int f = 0; f < F_; ++f) {
    const int bp = plot_block_[static_cast<size_t>(p) * F_ + f], bq = plot_block_[static_cast<size_t>(q) * F_ + f];
    if (bp == bq) continue;
    u_j_[nz] = bp; u_v_[nz] = wt_[bp]; ++nz;
    u_j_[nz] = bq; u_v_[nz] = -wt_[bq]; ++nz;
  }
  return nz;
}

double CrossedSpace::ratio(int p, int q) const {
  const int a = treat_[p], c = treat_[q], len = F_ * r_;
  const int nz = u_sparse(p, q);
  const double* ga = &g_[static_cast<size_t>(B_) * a];
  const double* gc = &g_[static_cast<size_t>(B_) * c];
  double alpha = 0.0, beta = 0.0, gamma = 0.0;
  for (int x = 0; x < nz; ++x) {
    const double* mx = &m_[static_cast<size_t>(B_) * u_j_[x]];
    for (int y = 0; y < nz; ++y) alpha += u_v_[x] * u_v_[y] * mx[u_j_[y]];
    beta += u_v_[x] * (gc[u_j_[x]] - ga[u_j_[x]]);
  }
  const int* lc = &lists_[static_cast<size_t>(c) * len];
  const int* la = &lists_[static_cast<size_t>(a) * len];
  for (int x = 0; x < len; ++x) {
    gamma += wt_[lc[x]] * (gc[lc[x]] - ga[lc[x]]);
    gamma -= wt_[la[x]] * (gc[la[x]] - ga[la[x]]);
  }
  const double b = 1.0 - beta / r_;
  first_ = b * b;
  second_ = alpha * (2.0 / r_ + gamma / (static_cast<double>(r_) * r_));
  return first_ - second_;
}

double CrossedSpace::last_scale() const {
  return std::max(1.0, std::max(std::fabs(first_), std::fabs(second_)));
}

void CrossedSpace::mg_of(int a, int c, std::vector<double>& mg) const {
  // M g from the columns of M at the blocks of c and a, not from the cache:
  // reading G here would feed its rounding error into the update of M, whose
  // next update of G reads M again, and the error then grew about threefold
  // per accepted swap (measured; BlockSpace::apply() also reads Minv).
  const int len = F_ * r_;
  const int* lc = &lists_[static_cast<size_t>(c) * len];
  const int* la = &lists_[static_cast<size_t>(a) * len];
  std::fill(mg.begin(), mg.end(), 0.0);
  for (int x = 0; x < len; ++x) {
    const double* mc = &m_[static_cast<size_t>(B_) * lc[x]];
    const double* ma = &m_[static_cast<size_t>(B_) * la[x]];
    const double wc = wt_[lc[x]], wa = wt_[la[x]];
    for (int j = 0; j < B_; ++j) mg[j] += wc * mc[j] - wa * ma[j];
  }
}

bool CrossedSpace::after(int p, int q, double& ratio_out, double& trace_after) const {
  const int a = treat_[p], c = treat_[q], len = F_ * r_;
  const int nz = u_sparse(p, q);
  trace_after = trace();
  if (nz == 0) { ratio_out = 1.0; return true; }
  std::fill(mu_.begin(), mu_.end(), 0.0);
  for (int x = 0; x < nz; ++x) {
    const double* mx = &m_[static_cast<size_t>(B_) * u_j_[x]];
    for (int j = 0; j < B_; ++j) mu_[j] += u_v_[x] * mx[j];
  }
  mg_of(a, c, mg_);
  double alpha = 0.0, beta = 0.0, gamma = 0.0;
  for (int x = 0; x < nz; ++x) { alpha += u_v_[x] * mu_[u_j_[x]]; beta += u_v_[x] * mg_[u_j_[x]]; }
  const int* lc = &lists_[static_cast<size_t>(c) * len];
  const int* la = &lists_[static_cast<size_t>(a) * len];
  for (int x = 0; x < len; ++x) gamma += wt_[lc[x]] * mg_[lc[x]] - wt_[la[x]] * mg_[la[x]];
  const double rr = r_;
  const double b = 1.0 - beta / rr;
  first_ = b * b;
  second_ = alpha * (2.0 / rr + gamma / (rr * rr));
  ratio_out = first_ - second_;
  if (!(ratio_out > 0.0)) return false;
  const double det = alpha * (2.0 * rr + gamma) - (rr - beta) * (rr - beta);
  const double w00 = (-2.0 * rr - gamma) / det, w01 = (beta - rr) / det, w11 = -alpha / det;
  double uu = 0.0, ug = 0.0, gg = 0.0;
  for (int j = 0; j < B_; ++j) { uu += mu_[j] * mu_[j]; ug += mu_[j] * mg_[j]; gg += mg_[j] * mg_[j]; }
  trace_after += w00 * uu + 2.0 * w01 * ug + w11 * gg;
  return true;
}

void CrossedSpace::apply(int p, int q, double ratio_in) {
  const int a = treat_[p], c = treat_[q], len = F_ * r_;
  const int nz = u_sparse(p, q);
  if (nz > 0) {
    std::fill(mu_.begin(), mu_.end(), 0.0);
    for (int x = 0; x < nz; ++x) {
      const double* mx = &m_[static_cast<size_t>(B_) * u_j_[x]];
      for (int j = 0; j < B_; ++j) mu_[j] += u_v_[x] * mx[j];
    }
    mg_of(a, c, mg_);
    double alpha = 0.0, beta = 0.0, gamma = 0.0;
    for (int x = 0; x < nz; ++x) { alpha += u_v_[x] * mu_[u_j_[x]]; beta += u_v_[x] * mg_[u_j_[x]]; }
    const int* lc = &lists_[static_cast<size_t>(c) * len];
    const int* la = &lists_[static_cast<size_t>(a) * len];
    for (int x = 0; x < len; ++x) gamma += wt_[lc[x]] * mg_[lc[x]] - wt_[la[x]] * mg_[la[x]];
    const double rr = r_;
    const double det = alpha * (2.0 * rr + gamma) - (rr - beta) * (rr - beta);
    const double w00 = (-2.0 * rr - gamma) / det, w01 = (beta - rr) / det, w11 = -alpha / det;
    // G += [Mu Mg] K^-1 (U' G): column i gains c1 Mu + c2 Mg (old lists).
    for (int i = 0; i < v_; ++i) {
      double* gi = &g_[static_cast<size_t>(B_) * i];
      double e1 = 0.0, e2 = 0.0;
      for (int x = 0; x < nz; ++x) e1 += u_v_[x] * gi[u_j_[x]];
      for (int x = 0; x < len; ++x) e2 += wt_[lc[x]] * gi[lc[x]] - wt_[la[x]] * gi[la[x]];
      const double c1 = w00 * e1 + w01 * e2, c2 = w01 * e1 + w11 * e2;
      if (c1 == 0.0 && c2 == 0.0) continue;
      for (int j = 0; j < B_; ++j) gi[j] += c1 * mu_[j] + c2 * mg_[j];
    }
    // M += [Mu Mg] K^-1 [Mu Mg]'.
    for (int l = 0; l < B_; ++l) {
      double* ml = &m_[static_cast<size_t>(B_) * l];
      const double ul = mu_[l], gl = mg_[l];
      for (int j = 0; j < B_; ++j)
        ml[j] += w00 * mu_[j] * ul + w01 * (mu_[j] * gl + mg_[j] * ul) + w11 * mg_[j] * gl;
    }
    // G += (M'' u) d', d = e_c - e_a.
    double* ga = &g_[static_cast<size_t>(B_) * a];
    double* gc = &g_[static_cast<size_t>(B_) * c];
    for (int x = 0; x < nz; ++x) {
      const double* mx = &m_[static_cast<size_t>(B_) * u_j_[x]];
      const double ux = u_v_[x];
      for (int j = 0; j < B_; ++j) { gc[j] += ux * mx[j]; ga[j] -= ux * mx[j]; }
    }
    // Block lists: one plot of a moves from each block of p to q, one of c back.
    for (int f = 0; f < F_; ++f) {
      const int bp = plot_block_[static_cast<size_t>(p) * F_ + f], bq = plot_block_[static_cast<size_t>(q) * F_ + f];
      if (bp == bq) continue;
      int* lam = &lists_[static_cast<size_t>(a) * len];
      for (int x = 0; x < len; ++x) if (lam[x] == bp) { lam[x] = bq; break; }
      int* lcm = &lists_[static_cast<size_t>(c) * len];
      for (int x = 0; x < len; ++x) if (lcm[x] == bq) { lcm[x] = bp; break; }
    }
    log_det_ += std::log(ratio_in);
  }
  std::swap(treat_[p], treat_[q]);
}

double CrossedSpace::trace() const {
  double s = 0.0;
  for (int j = 0; j < B_; ++j) s += m_[j + static_cast<size_t>(B_) * j];
  return s;
}

double CrossedSpace::max_diag() const {
  double s = 0.0;
  for (int j = 0; j < B_; ++j) s = std::max(s, m_[j + static_cast<size_t>(B_) * j]);
  return s;
}

double CrossedSpace::d_efficiency() const { return std::exp(log_det_ / (v_ - 1)); }

double CrossedSpace::a_efficiency() const {
  return (v_ - 1.0) / (trace() - B_ + v_ - 1.0);
}

double CrossedSpace::drift() const {
  // Column l of What M - I: m - (1/r) N'(N m) + y (y'm) / n - e_l.
  std::vector<double> res(B_);
  const int len = F_ * r_;
  double worst = 0.0;
  for (int l = 0; l < B_; ++l) {
    const double* ml = &m_[static_cast<size_t>(B_) * l];
    double ym = 0.0;
    for (int j = 0; j < B_; ++j) ym += sq_[j] * ml[j];
    for (int j = 0; j < B_; ++j) res[j] = (1.0 + ridge_) * ml[j] + sq_[j] * ym / n_;
    res[l] -= 1.0;
    for (int i = 0; i < v_; ++i) {
      const int* li = &lists_[static_cast<size_t>(i) * len];
      double s = 0.0;
      for (int x = 0; x < len; ++x) s += wt_[li[x]] * ml[li[x]];
      s /= r_;
      for (int x = 0; x < len; ++x) res[li[x]] -= wt_[li[x]] * s;
    }
    for (int j = 0; j < B_; ++j) worst = std::max(worst, std::fabs(res[j]));
  }
  return worst;
}

double CrossedSpace::drift_cache() const {
  std::vector<double> col(B_);
  const int len = F_ * r_;
  double worst = 0.0;
  for (int i = 0; i < v_; ++i) {
    const int* li = &lists_[static_cast<size_t>(i) * len];
    std::fill(col.begin(), col.end(), 0.0);
    for (int x = 0; x < len; ++x) {
      const double* mj = &m_[static_cast<size_t>(B_) * li[x]];
      for (int j = 0; j < B_; ++j) col[j] += wt_[li[x]] * mj[j];
    }
    const double* gi = &g_[static_cast<size_t>(B_) * i];
    for (int j = 0; j < B_; ++j) worst = std::max(worst, std::fabs(gi[j] - col[j]));
  }
  return worst;
}
