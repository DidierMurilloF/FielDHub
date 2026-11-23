// Block-space information state of F crossed block factors (phase 2 spec 5.1,
// 6.1 mode 3). F = 2 (rows and columns of a row-column design) is the joint
// state of the crossed search; F = 1 (rows alone) is its Row state.
//
// Setting. v treatments, each on r plots; every plot lies in one block of each
// factor; B block columns in all (the blocks of factor 1, then of factor 2),
// block j of size k_j, every factor's blocks summing to n = v r plots. Let
// N (v x B) have entries N_ij = n_ij / sqrt(k_j), with n_ij the number of plots
// of treatment i in block j. The treatment information is
//   F = 1: C = r I - N N';
//   F = 2: C = r I - N N' + (r / v) J  (spec 5.1, block-space form; it holds
//          because each replicate is a complete grid, so rows and columns are
//          orthogonal given the replicates).
// The efficiency factors e_1..e_(v-1) are the eigenvalues of C / r on the
// contrasts.
//
// Block-space identity. Let y = N' 1_v; y_j = sqrt(k_j) is fixed by the
// layout, and |y|^2 = sum_j k_j = F n. N N' 1 = N y = F r 1 (each treatment
// has r plots in every factor), so N N' / r has eigenvalue F along 1 and
// eigenvalues lambda_i (e_i = 1 - lambda_i) on the contrasts.
// W = I_B - N' N / r has the same nonzero spectrum shifted: 1 - F along y,
// 1 - lambda_i for the nonzero lambda_i, and 1 on the rest (the null space of
// N). Adding F y y' / |y|^2 moves the eigenvalue along y to 1 and leaves the
// others, so
//   What = I_B - N' N / r + y y' / n,   entries
//   What_jl = [j = l] - (N'N)_jl / r + sqrt(k_j k_l) / n,
// has log det What = sum_i log e_i = log det of C / r on the contrasts, for
// F = 1 and F = 2 alike. Its eigenvalues are the e_i of the nonzero lambda_i
// and 1 (the other e_i are 1 too), so:
// - What is positive definite exactly when C has rank v - 1;
// - its smallest eigenvalue is min_i e_i, so the full-rank test of spec 5.1b
//   (smallest canonical efficiency >= 1e-7) is "What - 1e-7 I is positive
//   definite", a Cholesky with no eigen-decomposition (full_rank());
// - with M = What^-1, tr(M) = sum_(lambda_i != 0) 1 / e_i + (B - m), m the
//   number of nonzero lambda_i, so sum_i 1 / e_i = tr(M) - B + v - 1 for every
//   m, and A = (v - 1) / (tr(M) - B + v - 1); D = exp(log det What / (v - 1)).
//
// Swap update (the builder's formulation, spec 11). Exchanging the treatments
// a at plot p and c at plot q (a != c) moves one plot of a from each block of p
// to the same factor's block of q and one plot of c back, so
//   N' = N + d u',  d = e_c - e_a (v),
//   u = sum_f (e_(f(p)) - e_(f(q))) / sqrt(k_f)  (B; at most 2 F entries),
// where f(p) is the block of factor f holding p; a factor whose two blocks
// coincide contributes nothing (two plots of one row: the row entries cancel).
// With g = N' d = N_c. - N_a. (the weighted block counts of c minus those of
// a) and |d|^2 = 2,
//   N''N'' = N'N + g u' + u g' + 2 u u',  What'' = What - U S U',
//   U = [u g] (B x 2),  S = (1 / r) [[2, 1], [1, 0]].
// By the matrix determinant lemma, with alpha = u'Mu, beta = u'Mg,
// gamma = g'Mg,
//   det What'' / det What = det(I - S U'MU)
//     = (1 - (2 alpha + beta) / r)(1 - beta / r) - alpha (2 beta + gamma) / r^2
//     = (1 - beta / r)^2 - alpha (2 / r + gamma / r^2),
// the two-term form of BlockSpace::delta() with h = g / r (its unit-weight,
// one-factor case). Woodbury, with S^-1 = r [[0, 1], [1, -2]] and
// K = S^-1 - U'MU = [[-alpha, r - beta], [r - beta, -2 r - gamma]],
// det K = -r^2 ratio:
//   M'' = M + [Mu Mg] K^-1 [Mu Mg]',
//   K^-1 = (1 / det K) [[-2 r - gamma, beta - r], [beta - r, -alpha]],
// and tr(M'') = tr(M) + w00 |Mu|^2 + 2 w01 (Mu . Mg) + w11 |Mg|^2 for the
// entries w of K^-1, which is how the Row floor reads Row A after a swap.
//
// Cache. G = M N' (B x v; column i is M N_i.') makes a ratio O(r): Mg =
// G_.c - G_.a, so beta reads the at most 2 F entries of u, gamma = g'(Mg) sums
// over the 2 F r block entries of a and c, and alpha reads at most (2 F)^2
// entries of M. After a swap G'' = M'' N'' ' = G + [Mu Mg] K^-1 (U' G) +
// (M'' u) d', so column i gains c1 Mu + c2 Mg with (c1, c2) = K^-1 (u'G_.i,
// g'G_.i), and then columns c and a gain +- M'' u. The update of M reads only
// M (Mu and Mg from its columns), never G, so the cache's rounding cannot
// feed back into M.
//
// Ridge (the joint-rank repair, spec 6.1 stage 2). The repair maximises
// log det(C / r + eps P), P the projector on the contrasts, whose eigenvalues
// there are e_i + eps = 1 + eps - lambda_i. In block space that is What with
// I_B replaced by (1 + eps) I_B:
//   What_eps = (1 + eps) I_B - N' N / r + y y' / n
// has eigenvalue 1 + eps - F + F = 1 + eps along y, 1 + eps - lambda_i for
// the m nonzero lambda_i and 1 + eps on the rest, so
//   log det What_eps = sum_i log(e_i + eps) + (B - v + 1) log(1 + eps),
// the repair criterion plus a constant of the layout. What_eps is positive
// definite for every design (lambda_i <= 1), and a swap changes it by the
// same - U S U', so ratio(), after() and apply() hold unchanged with
// M = What_eps^-1. A state built with `ridge` = eps is this state; the
// full-rank test never includes the ridge. M then has entries near 1 / eps on
// a singular design and a rank-2 ratio loses digits, so the repair confirms
// every accepted swap with log_det_after(), a factorisation from the counts.
// The check `tools/check-onestage-engine.R repair` verifies the identity
// (the engine's criterion value against the dense eigenvalues).
//
// The check `tools/check-onestage-engine.R crossed` verifies the identity
// (values against a dense qr.resid() reference) and this update (accepted
// ratios against fresh refreshes, FIELDHUB_ENGINE_DEBUG=1).
//
// Error of a ratio (the bound of that check, ratio_error_bound()). Let the
// state hold M + E in place of M and a cache G + E_G in place of M N', with
// |E_jl| <= dM and |(E_G)_ji| <= dG for all entries. With |u|_1 the sum of
// the |u_j| and l_1 = |N_c.|_1 + |N_a.|_1 >= |g|_1:
//   alpha = u'Mu:           |error| <= A = |u|_1^2 dM;
//   beta = u'(Mg), gamma = g'(Mg): every entry of Mg = G_.c - G_.a is wrong
//     by at most eta, eta = 2 dG from the cache (ratio()) and eta = l_1 dM
//     from M (after()), so |error| <= Bb = |u|_1 eta and Gm = l_1 eta.
// With b = 1 - beta / r and X = 2 / r + gamma / r^2 as computed, the two
// terms of the ratio, b^2 and alpha X, are wrong by at most
//   2 |b| Bb / r + Bb^2 / r^2   and   A |X| + |alpha| Gm / r^2 + A Gm / r^2
// (exact, not first order: (b + e)^2 - b^2 and (alpha + a)(X + c) - alpha X).
// So the error of a ratio is proportional to the error of the state and to
// the size of the ratio's terms, |b| and |alpha| X, which is why acceptance
// is judged relative to last_scale(); it is not bounded relative to the ratio
// itself. Entries of M are at most d = max_j M_jj and entries of G at most
// (l_1 / 2) d, so the rounding of the three sums adds (nz^2 + 1) u |u|_1^2 d
// to A, (nz + 2 F r + 2) u |u|_1 l_1 d to Bb and (4 F r + 2) u l_1^2 d to Gm
// (u = 2^-53, nz <= 2 F the entries of u), and the last operations 8 u times
// the larger term.
// dM and dG follow from what the sparse checks measure. With rho = drift() =
// max |What M - I| and M* the true inverse, M - M* = M* (What M - I), so
// dM <= B max|M*| rho <= B (d + dM) rho, that is dM <= B rho d / (1 - B rho);
// and G - M* N' = (G - M N') + (M - M*) N', so dG <= drift_cache() +
// dM l_1 / 2.
// The two measures are themselves computed in floating point and carry a
// rounding floor, so the true rho and cache error can exceed the computed
// ones by it. An entry of What M - I is a sum of at most B + F r + k + 3
// terms (y'm, the F r entries of a treatment's row of N M, the k treatments
// of the block, and three more) whose sizes add to at most (1 + 2 F) d <=
// 5 d (|m| <= d; the y term at most F d because sum_j k_j = F n; the N'N
// term at most F d because k wt^2 = 1), so rho is known to within
// 5 (B + F r + v + 3) u d (k <= v); an entry of G - M N' is a sum of F r + 1
// terms of size at most d, known to within (F r + 1) F r u d. The check adds
// both floors to the measures before it forms dM and dG.
//
// Treatment-space form (phase 2 spec 5.1c). Every factorisation above is of
// order B. With fewer treatments than block columns (v < B) the same state
// follows from a v x v matrix. Let Nt = N / sqrt(r), e = 1 / sqrt(v) (the unit
// constant vector), P = I_v - e e' and yhat = y / sqrt(n) = Nt' e. Then
//   What_eps = (1 + eps) I_B - Nt' P Nt   (Nt' e e' Nt = y y' / n),
//   S_eps = (1 + eps) I_v - P Nt Nt' P = (1 + eps) I_v - N N' / r + (F / v) J,
// because N N' 1 = F r 1 (each treatment has r plots in every factor), so
// Nt Nt' e = F e and P Nt Nt' P = Nt Nt' - F e e'. S_eps is C / r + eps I on
// the contrasts and 1 + eps along e. By the Woodbury identity on
// (1 + eps) I_B - A'A with A = P Nt, and P S_eps^-1 P = S_eps^-1 -
// e e' / (1 + eps) (S_eps e = (1 + eps) e),
//   M = What_eps^-1
//     = (I_B + Nt' S_eps^-1 Nt - yhat yhat' / (1 + eps)) / (1 + eps),
//   log det What_eps = log det S_eps + (B - v) log(1 + eps)
// (Sylvester's determinant identity), and What and S (eps = 0) have the same
// smallest eigenvalue, min(1, min_i e_i), so the full-rank test is
// "S - 1e-7 I is positive definite". A state built with `treatment_form`
// uses these for refresh(), full_rank(), log_det_fresh(), log_det_after()
// and full_rank_after() when v < B, and is the block-space state otherwise.
// N N' is accumulated block by block (sum of k_j^2 terms), S is inverted
// once (v^3), and Nt' S^-1 Nt and the cache are sparse products of order
// F n (v + B). M, G, ratio(), after() and apply() are the same in both forms.
// The check `tools/check-onestage-engine.R treatment_form` verifies the form
// against the dense references.
#pragma once
#include <vector>

class CrossedSpace {
 public:
  // plot_block: n x F, plot-major (entry p F + f is the 0-based block column,
  // in 0 .. B - 1, of plot p in factor f); treat: the 0-based treatment of
  // every plot, each treatment r times. Block sizes are read from plot_block.
  // ridge: eps of What_eps (0 for the plain state). treatment_form: build the
  // state from the treatment space when v < B (header comment).
  CrossedSpace(int v, int r, int B, int F, const std::vector<int>& plot_block,
               const std::vector<int>& treat, double ridge = 0.0, bool treatment_form = false);
  // True if this state is built from the treatment space.
  bool treatment_form() const { return form_; }
  // The full-rank test of spec 5.1b on a state that refresh() has just built
  // without a ridge. The eigenvalues of M are the 1 / e_i of the nonzero
  // lambda_i and ones, all positive, so the smallest e_i is at least
  // 1 / tr(M): when tr(M) <= 1e7 the test passes with no second
  // factorisation (`exact` false). Otherwise it is full_rank() (`exact`
  // true). Never valid on a ridge state: tr(M_eps) <= 1e7 only bounds
  // e_i + eps.
  bool full_rank_refreshed(bool& exact) const;
  // Work units (about one per multiply-add) of one refresh() and of one
  // factorisation from the counts (full_rank(), log_det_fresh() and the two
  // *_after()). Block-space form: B^3 + n F^2 r + n F B for both, the charge
  // of the engine before the treatment-space form existed.
  // Treatment-space form: v^3 + sum_j k_j^2 + F n (v + 2 B) + 2 B^2 for a
  // refresh (N N', the inverse of S, S^-1 N, N' S^-1 N and the cache) and
  // v^3 / 3 + sum_j k_j^2 + v^2 for a factorisation (N N' and one Cholesky).
  double refresh_work() const;
  double factor_work() const;
  // Rebuild M, the cache G and log det What from the design. False if What is
  // not positive definite (C below rank v - 1); the state is then unchanged.
  bool refresh();
  // The full-rank test of spec 5.1b on the current design (from the counts,
  // not the running state, and without the ridge): every efficiency factor
  // >= 1e-7.
  bool full_rank() const;
  // log det What (with the ridge) of the current design and of the design
  // after swapping plots p and q, by a Cholesky factorisation from the counts
  // (not the running state). False if What is not positive definite. Both use
  // the same arithmetic, so their difference is the exact gain of the swap up
  // to the rounding of one factorisation.
  bool log_det_fresh(double& log_det) const;
  bool log_det_after(int p, int q, double& log_det) const;
  // The same test on the design after swapping plots p and q.
  bool full_rank_after(int p, int q) const;
  // det What'' / det What for swapping plots p and q (treatments differ), O(r)
  // from M and G. last_scale() gives the size of its two terms.
  double ratio(int p, int q) const;
  double last_scale() const;
  // The ratio and tr(M'') after swapping p and q, O(r B); false if the ratio
  // is not positive (the swap would lose rank).
  bool after(int p, int q, double& ratio, double& trace_after) const;
  // Apply the swap of p and q given its ratio (> 0): M, G, the block lists and
  // the running log det.
  void apply(int p, int q, double ratio);
  double log_det() const { return log_det_; }
  double trace() const;                  // tr(M), O(B)
  double d_efficiency() const;           // exp(log det / (v - 1))
  double a_efficiency() const;           // (v - 1) / (tr(M) - B + v - 1)
  // max |What M - I| from the sparse form of What, O(B (B + n)). It is
  // relative: the error of M itself can reach max_diag() times it, which is
  // large on an ill-conditioned design (a near-singular start).
  double drift() const;
  double max_diag() const;               // max_j M_jj (>= every |M_jl|)
  // max |G - M N'| over the cache, O(B n).
  double drift_cache() const;
  // For the checks (FIELDHUB_ENGINE_DEBUG=1): a bound on the error of the
  // ratio of swapping p and q as ratio() (`cache` true) or after() (false)
  // computes it from this state, given dM >= max |M - What^-1| and dC >=
  // drift_cache() (header comment). O(r B).
  double ratio_error_bound(int p, int q, bool cache, double dM, double dC) const;
  // For the check hook `drift` of the crossed search: multiplies M_00 by
  // 1 + rel, an error the drift check must find.
  void disturb(double rel) { m_[0] *= 1.0 + rel; }
  int blocks() const { return B_; }
  int treatment(int p) const { return treat_[p]; }
  const std::vector<int>& treatments() const { return treat_; }

 private:
  // Dense What from the per-treatment block lists `lists` (v x F r), with
  // `diagonal` in place of the 1 of I_B.
  void build_what(const std::vector<int>& lists, std::vector<double>& w, double diagonal) const;
  // Dense S = diagonal I_v - N N' / r + (F / v) J from the same lists; the
  // treatments of each block (with multiplicity) go to `member`.
  void build_s(const std::vector<int>& lists, std::vector<double>& s, double diagonal,
               std::vector<int>* member = nullptr) const;
  bool refresh_treatment_form();
  // log det What (with the ridge) from the lists, in the state's form.
  bool log_det_of(const std::vector<int>& lists, double& log_det) const;
  bool full_rank_of(const std::vector<int>& lists) const;
  // The block lists after swapping plots p and q.
  std::vector<int> lists_after(int p, int q) const;
  // u of the swap of p and q: its entries in u_j_ / u_v_, returns how many.
  int u_sparse(int p, int q) const;
  void mg_of(int a, int c, std::vector<double>& mg) const;  // M g, from M
  int v_, r_, B_, F_, n_;
  double ridge_;
  bool form_;                     // treatment-space form (v < B)
  std::vector<int> off_;          // B + 1: block j holds off_[j + 1] - off_[j] plots
  double sum_k2_;                 // sum_j k_j^2
  std::vector<int> plot_block_;   // n x F
  std::vector<double> wt_;        // 1 / sqrt(k_j)
  std::vector<double> sq_;        // sqrt(k_j)
  std::vector<int> treat_;        // treatment of every plot
  std::vector<int> lists_;        // v x F r: the blocks of each treatment's plots
  std::vector<double> m_;         // B x B, M = What^-1
  std::vector<double> g_;         // B x v, G = M N'
  mutable int u_j_[4];
  mutable double u_v_[4];
  mutable double first_ = 0.0, second_ = 0.0;
  mutable std::vector<double> mu_, mg_;  // scratch, size B
  double log_det_;
};
