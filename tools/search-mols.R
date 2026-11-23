# One-off search for a pair of orthogonal Latin squares of order q (nested-
# engine spec 6.7). Not part of the package: the pairs it finds are stored in
# R/engine_mols_data.R.
#
#   Rscript tools/search-mols.R <q> <seed> <minutes> <out.rds> [method]
#
# writes list(q, seed, method, L, M, minutes) with 0-based q x q integer matrices, or
# exits with status 1 when the time box ends. `method` is "auto" (default:
# method 1 for 40% of the box, method 2 for 10%, method 3 for the rest),
# "1", "2" or "3". The search is seeded with its own generator, so the same
# arguments retrace the same search (the time limits only decide where it
# stops).
#
# Clean room: FielDHub's own code, written from the descriptions below and
# textbook algorithms. No other implementation of a search for orthogonal
# Latin squares was consulted.
#
# Starting square. L is a random Latin square: the cyclic square after a
# Jacobson & Matthews (1996, J. Combin. Des. 4:405-437) walk of 10 q^3 steps
# on the 0/1 incidence cube (one cell may be -1 between steps; the walk runs
# on until the cube is a Latin square again).
#
# Method 1: transversal exact cover. A transversal of L is q cells,
# one per row, column and symbol; q disjoint transversals T_0..T_(q-1) give
# the mate M[i, j] = index of the transversal through (i, j). Transversals are
# enumerated by backtracking with row, column and symbol bit masks (branching
# on the row, column or symbol with the fewest usable cells), and q disjoint
# ones are picked by Knuth's Algorithm X (dancing links; exact cover of the
# q^2 cells). When L has more transversals than fit in memory
# (`cap`), random disjoint transversals are removed first until those of the
# remaining cells fit, and Algorithm X covers the rest; a failed attempt
# draws new random transversals. An L with no cover within 5 minutes is
# replaced by a new one.
#
# Method 2 (when method 1 has used its share of the time): local search on
# the mate. M starts as a random Latin square. The cost is the
# number of ordered pairs (L[i, j], M[i, j]) that do not occur. A move is a
# cycle switch through a cell whose pair is repeated: between two rows, two
# columns or two symbols, each of which keeps M Latin. Moves that do not
# raise the cost are accepted. After 10^6 moves without improvement the
# search restarts from a new M.
#
# Method 3 (when methods 1 and 2 have used their shares): the same search for
# a pair, restricted to pairs that are unchanged when 1 is added (mod q - 1)
# to the rows, columns and symbols 0..q-2 while the last one, "infinity", stays
# fixed. In orthogonal-array terms (van Lint & Wilson, ch. 22: an OA(4, q)
# with columns (row, column, L, M)) such a pair is the column (inf, inf, inf,
# inf) plus q + 1 base columns developed over Z_(q-1):
#   (inf, 0, a3, a4), (0, inf, b3, b4), (0, c, inf, c4), (0, d, d3, inf)
# and q - 3 columns (0, y, s(y), f(y)) with no infinity. For every two
# coordinates, the differences over the base columns finite in both must be
# all of Z_(q-1) (own derivation: developing a column keeps its differences,
# and infinity must meet every finite symbol once). So row 0 of L is found by
# backtracking over y -> s(y) with masks for the used symbols and the used
# differences s(y) - y, then row 0 of M over y -> f(y) with masks for the
# used symbols, the used f(y) - y and the used f(y) - s(y); the variable with
# the fewest candidates is assigned next. A new L is drawn when M is not
# found within a node budget.
#
# Before writing: L and M are Latin and all q^2 ordered pairs occur once.
arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) < 4L || length(arguments) > 5L) {
  stop("Usage: Rscript tools/search-mols.R <q> <seed> <minutes> <out.rds> [method]")
}
q <- as.integer(arguments[1])
seed <- as.integer(arguments[2])
minutes <- as.numeric(arguments[3])
out <- arguments[4]
method <- if (length(arguments) == 5L) arguments[5] else "auto"
stopifnot(q >= 4L, q <= 32L, !is.na(seed), minutes > 0, method %in% c("auto", "1", "2", "3"))
# Shares of the time box for methods 1, 2 and 3.
shares <- switch(method, auto = c(0.4, 0.1, 0.5), "1" = c(1, 0, 0), "2" = c(0, 1, 0), "3" = c(0, 0, 1))
# Most transversals held at once: `cap` when all of L's fit, `prefix_cap` after
# random transversals have been removed; `node_limit` bounds one cover attempt
# of the second kind.
cap <- as.numeric(Sys.getenv("SEARCH_MOLS_CAP", "1500000"))
prefix_cap <- as.numeric(Sys.getenv("SEARCH_MOLS_PREFIX_CAP", "5000"))
node_limit <- as.numeric(Sys.getenv("SEARCH_MOLS_NODES", "20000"))

Rcpp::sourceCpp(code = '
#include <Rcpp.h>
#include <vector>
#include <chrono>
#include <cstdint>
#include <algorithm>
using namespace Rcpp;

typedef std::chrono::steady_clock Clock;
static double seconds_since(Clock::time_point t0) {
  return std::chrono::duration<double>(Clock::now() - t0).count();
}

// splitmix64 (Steele, Lea & Flood 2014)
struct Rng {
  uint64_t s;
  uint64_t next() {
    uint64_t z = (s += 0x9E3779B97F4A7C15ULL);
    z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
    z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
    return z ^ (z >> 31);
  }
  int below(int n) { return (int)(next() % (uint64_t)n); }
};

// Random Latin square: Jacobson-Matthews walk from the cyclic square.
static std::vector<int> random_latin(int n, Rng& g) {
  std::vector<signed char> f((size_t)n * n * n, 0);
  auto at = [&](int r, int c, int s) -> signed char& { return f[((size_t)r * n + c) * n + s]; };
  for (int r = 0; r < n; ++r) for (int c = 0; c < n; ++c) at(r, c, (r + c) % n) = 1;
  bool proper = true;
  int ir = 0, ic = 0, is = 0;
  long steps = 10L * n * n * n;
  for (long step = 0; step < steps || !proper; ++step) {
    int r, c, s, r2 = -1, c2 = -1, s2 = -1;
    if (proper) {
      do { r = g.below(n); c = g.below(n); s = g.below(n); } while (at(r, c, s) != 0);
      for (int x = 0; x < n; ++x) {
        if (at(r, c, x) == 1) s2 = x;
        if (at(x, c, s) == 1) r2 = x;
        if (at(r, x, s) == 1) c2 = x;
      }
    } else {
      r = ir; c = ic; s = is;
      int pick_s = g.below(2), pick_r = g.below(2), pick_c = g.below(2);
      int seen_s = 0, seen_r = 0, seen_c = 0;
      for (int x = 0; x < n; ++x) {
        if (at(r, c, x) == 1 && seen_s++ == pick_s) s2 = x;
        if (at(x, c, s) == 1 && seen_r++ == pick_r) r2 = x;
        if (at(r, x, s) == 1 && seen_c++ == pick_c) c2 = x;
      }
    }
    at(r, c, s)++; at(r, c2, s2)++; at(r2, c, s2)++; at(r2, c2, s)++;
    at(r, c, s2)--; at(r, c2, s)--; at(r2, c, s)--; at(r2, c2, s2)--;
    if (at(r2, c2, s2) < 0) { proper = false; ir = r2; ic = c2; is = s2; } else proper = true;
  }
  std::vector<int> sq(n * n);
  for (int r = 0; r < n; ++r) for (int c = 0; c < n; ++c)
    for (int s = 0; s < n; ++s) if (at(r, c, s) == 1) sq[r * n + c] = s;
  return sq;
}

// ---- Method 1 ---------------------------------------------------------------

struct Transversals {
  int n;
  const int* L;
  int col_of[32][32];        // col_of[r][s]: the column of symbol s in row r
  uint32_t avail[32];        // per row: columns whose cell is still uncovered
  std::vector<uint8_t> list; // n columns per stored transversal
  size_t cap;
  bool overflow;
  uint8_t cur[32];
  Rng* g;

  void setup(int order, const int* square, size_t limit, Rng* rng) {
    n = order; L = square; cap = limit; g = rng; overflow = false;
    for (int r = 0; r < n; ++r) {
      avail[r] = (n == 32) ? 0xFFFFFFFFu : ((1u << n) - 1u);
      for (int c = 0; c < n; ++c) col_of[r][L[r * n + c]] = c;
    }
  }
  // Backtracking over the uncovered cells. A transversal needs one cell in
  // every row, every column and every symbol; rm[r] holds the columns still
  // usable in row r, cm[c] the rows usable in column c, sm[s] the rows whose
  // cell with symbol s is usable. Each step branches on the row, column or
  // symbol with the fewest usable cells. With `random` the cells are tried
  // in random order and the search stops at the first transversal (in cur).
  bool walk(int placed, uint32_t rows, uint32_t cols, uint32_t syms,
            const uint32_t* rm, const uint32_t* cm, const uint32_t* sm, bool random) {
    if (placed == n) {
      if (random) return true;
      if (list.size() / n >= cap) { overflow = true; return false; }
      list.insert(list.end(), cur, cur + n);
      return false;
    }
    int best = 99, kind = 0, which = 0;
    for (uint32_t m = rows; m; m &= m - 1) {
      int x = __builtin_ctz(m), k = __builtin_popcount(rm[x]);
      if (k < best) { best = k; kind = 0; which = x; }
    }
    for (uint32_t m = cols; m; m &= m - 1) {
      int x = __builtin_ctz(m), k = __builtin_popcount(cm[x]);
      if (k < best) { best = k; kind = 1; which = x; }
    }
    for (uint32_t m = syms; m; m &= m - 1) {
      int x = __builtin_ctz(m), k = __builtin_popcount(sm[x]);
      if (k < best) { best = k; kind = 2; which = x; }
    }
    if (best == 0) return false;
    int cell_r[32], cell_c[32], k = 0;
    if (kind == 0) {
      for (uint32_t m = rm[which]; m; m &= m - 1) { cell_r[k] = which; cell_c[k++] = __builtin_ctz(m); }
    } else if (kind == 1) {
      for (uint32_t m = cm[which]; m; m &= m - 1) { cell_r[k] = __builtin_ctz(m); cell_c[k++] = which; }
    } else {
      for (uint32_t m = sm[which]; m; m &= m - 1) {
        int r = __builtin_ctz(m);
        cell_r[k] = r; cell_c[k++] = col_of[r][which];
      }
    }
    if (random) {
      for (int i = k - 1; i > 0; --i) {
        int j = g->below(i + 1);
        std::swap(cell_r[i], cell_r[j]); std::swap(cell_c[i], cell_c[j]);
      }
    }
    uint32_t nrm[32], ncm[32], nsm[32];
    for (int i = 0; i < k; ++i) {
      int r = cell_r[i], c = cell_c[i], s = L[r * n + c];
      uint32_t rows_left = rows & ~(1u << r), keep_r = ~(1u << r);
      for (int x = 0; x < n; ++x) { nrm[x] = rm[x]; ncm[x] = cm[x] & keep_r; nsm[x] = sm[x] & keep_r; }
      for (uint32_t m = rows_left; m; m &= m - 1) {
        int x = __builtin_ctz(m), cs = col_of[x][s];
        uint32_t keep_x = ~(1u << x);
        nrm[x] &= ~((1u << c) | (1u << cs));
        ncm[cs] &= keep_x;
        nsm[L[x * n + c]] &= keep_x;
      }
      cur[r] = (uint8_t)c;
      if (walk(placed + 1, rows_left, cols & ~(1u << c), syms & ~(1u << s), nrm, ncm, nsm, random)) return true;
      if (overflow) return false;
    }
    return false;
  }
  bool run(bool random) {
    uint32_t rm[32], cm[32], sm[32];
    uint32_t all = (n == 32) ? 0xFFFFFFFFu : ((1u << n) - 1u);
    for (int x = 0; x < n; ++x) { rm[x] = avail[x]; cm[x] = 0; sm[x] = 0; }
    for (int r = 0; r < n; ++r) for (uint32_t m = avail[r]; m; m &= m - 1) {
      int c = __builtin_ctz(m);
      cm[c] |= 1u << r; sm[L[r * n + c]] |= 1u << r;
    }
    if (!random) list.clear();
    overflow = false;
    return walk(0, all, all, all, rm, cm, sm, random);
  }
};

// Algorithm X with dancing links. Options are transversals (n nodes each, in
// consecutive slots, so the links within an option are implicit).
struct Cover {
  int n, columns;
  std::vector<int> left, right, size, up, down, col;
  std::vector<int> solution;
  double nodes, node_limit;
  Clock::time_point deadline;
  int status;  // 0 searching, 1 found, 2 limit

  void hide(int c) {
    right[left[c]] = right[c]; left[right[c]] = left[c];
    for (int i = down[c]; i != c; i = down[i]) {
      int first = columns + 1 + ((i - columns - 1) / n) * n;
      for (int j = first; j < first + n; ++j) {
        if (j == i) continue;
        up[down[j]] = up[j]; down[up[j]] = down[j]; size[col[j]]--;
      }
    }
  }
  void unhide(int c) {
    for (int i = up[c]; i != c; i = up[i]) {
      int first = columns + 1 + ((i - columns - 1) / n) * n;
      for (int j = first + n - 1; j >= first; --j) {
        if (j == i) continue;
        size[col[j]]++; up[down[j]] = j; down[up[j]] = j;
      }
    }
    right[left[c]] = c; left[right[c]] = c;
  }
  void search(int depth) {
    if (right[0] == 0) { status = 1; return; }
    nodes += 1;
    if (((long long)nodes & 0xFFFF) == 0 && (nodes > node_limit || Clock::now() > deadline)) { status = 2; return; }
    int c = right[0];
    for (int j = right[c]; j != 0; j = right[j]) if (size[j] < size[c]) c = j;
    if (size[c] == 0) return;
    hide(c);
    for (int i = down[c]; i != c; i = down[i]) {
      int first = columns + 1 + ((i - columns - 1) / n) * n;
      solution[depth] = (i - columns - 1) / n;
      for (int j = first; j < first + n; ++j) if (j != i) hide(col[j]);
      search(depth + 1);
      for (int j = first + n - 1; j >= first; --j) if (j != i) unhide(col[j]);
      if (status != 0) break;
    }
    unhide(c);
  }
};

// [[Rcpp::export]]
List search_cover(int n, double seed, double seconds, double cap, double prefix_cap, double node_limit) {
  Clock::time_point t0 = Clock::now();
  Rng g; g.s = (uint64_t)seed;
  int squares = 0;
  while (seconds_since(t0) < seconds) {
    std::vector<int> L = random_latin(n, g);
    ++squares;
    Clock::time_point tL = Clock::now();
    double box = std::min(300.0, seconds - seconds_since(t0));
    long attempts = 0;
    bool exhausted = false;
    while (!exhausted && seconds_since(tL) < box) {
      ++attempts;
      Transversals tv;
      tv.setup(n, L.data(), (size_t)cap, &g);
      std::vector<int> M(n * n, -1);
      int prefix = 0;
      bool dead = false;
      for (;;) {
        tv.run(false);
        if (!tv.overflow) break;
        tv.list.clear();
        tv.cap = (size_t)prefix_cap;
        if (!tv.run(true)) { dead = true; break; }
        for (int r = 0; r < n; ++r) {
          M[r * n + tv.cur[r]] = prefix;
          tv.avail[r] &= ~(1u << tv.cur[r]);
        }
        ++prefix;
      }
      size_t count = tv.list.size() / n;
      int need = n - prefix;
      if (attempts == 1 || attempts % 1000 == 0) {
        Rprintf("[%.0fs] square %d attempt %ld: %d random transversals, %.0f left to cover %d\\n",
                seconds_since(t0), squares, attempts, prefix, (double)count, need);
      }
      if (dead || count < (size_t)need) { if (prefix == 0) exhausted = true; continue; }
      // Options in random order.
      std::vector<int> order(count);
      for (size_t i = 0; i < count; ++i) order[i] = (int)i;
      for (size_t i = count - 1; i > 0; --i) std::swap(order[i], order[g.next() % (i + 1)]);
      Cover cv;
      cv.n = n;
      std::vector<int> column_of(n * n, 0);
      int columns = 0;
      for (int r = 0; r < n; ++r) for (int c = 0; c < n; ++c)
        if (tv.avail[r] >> c & 1u) column_of[r * n + c] = ++columns;
      cv.columns = columns;
      size_t total = (size_t)columns + 1 + count * n;
      cv.left.resize(columns + 1); cv.right.resize(columns + 1); cv.size.assign(columns + 1, 0);
      cv.up.resize(total); cv.down.resize(total); cv.col.resize(total);
      for (int c = 0; c <= columns; ++c) {
        cv.left[c] = c == 0 ? columns : c - 1;
        cv.right[c] = c == columns ? 0 : c + 1;
        cv.up[c] = cv.down[c] = c; cv.col[c] = c;
      }
      for (size_t o = 0; o < count; ++o) {
        const uint8_t* t = &tv.list[(size_t)order[o] * n];
        for (int r = 0; r < n; ++r) {
          int node = columns + 1 + (int)(o * n) + r;
          int c = column_of[r * n + t[r]];
          cv.col[node] = c;
          cv.up[node] = cv.up[c]; cv.down[node] = c;
          cv.down[cv.up[c]] = node; cv.up[c] = node;
          cv.size[c]++;
        }
      }
      cv.solution.assign(n, -1);
      cv.nodes = 0;
      cv.node_limit = prefix == 0 ? 1e300 : node_limit;
      cv.deadline = tL + std::chrono::duration_cast<Clock::duration>(std::chrono::duration<double>(box));
      cv.status = 0;
      cv.search(0);
      if (cv.status == 1) {
        for (int k = 0; k < need; ++k) {
          const uint8_t* t = &tv.list[(size_t)order[cv.solution[k]] * n];
          for (int r = 0; r < n; ++r) M[r * n + t[r]] = prefix + k;
        }
        Rprintf("[%.0fs] cover found: square %d, attempt %ld, %.0f nodes\\n",
                seconds_since(t0), squares, attempts, cv.nodes);
        return List::create(_["found"] = true, _["L"] = L, _["M"] = M, _["squares"] = squares,
                            _["seconds"] = seconds_since(t0));
      }
      if (cv.status == 0 && prefix == 0) exhausted = true;  // L has no mate
    }
    Rprintf("[%.0fs] square %d given up (%s, %ld attempts)\\n", seconds_since(t0), squares,
            exhausted ? "no cover exists" : "5 minutes", attempts);
  }
  return List::create(_["found"] = false, _["squares"] = squares, _["seconds"] = seconds_since(t0));
}

// ---- Method 2 ---------------------------------------------------------------

struct Mate {
  int n;
  const int* L;
  std::vector<int> M, row_pos, col_pos, count;
  int cost;  // ordered pairs that do not occur

  void set(int r, int c, int s) {
    int a = L[r * n + c], old = M[r * n + c];
    if (--count[a * n + old] == 0) ++cost;
    if (++count[a * n + s] == 1) --cost;
    M[r * n + c] = s;
    row_pos[r * n + s] = c;
    col_pos[c * n + s] = r;
  }
  void init(const std::vector<int>& start) {
    M = start;
    row_pos.assign(n * n, 0); col_pos.assign(n * n, 0); count.assign(n * n, 0);
    for (int r = 0; r < n; ++r) for (int c = 0; c < n; ++c) {
      int s = M[r * n + c];
      row_pos[r * n + s] = c; col_pos[c * n + s] = r; count[L[r * n + c] * n + s]++;
    }
    cost = 0;
    for (int i = 0; i < n * n; ++i) if (count[i] == 0) ++cost;
  }
  // Swap rows r1 and r2 on the cycle of columns through c0. Its own inverse.
  void row_cycle(int r1, int r2, int c0) {
    int c = c0;
    do {
      int a = M[r1 * n + c], b = M[r2 * n + c];
      int next = row_pos[r1 * n + b];
      set(r1, c, b); set(r2, c, a);
      c = next;
    } while (c != c0);
  }
  void col_cycle(int c1, int c2, int r0) {
    int r = r0;
    do {
      int a = M[r * n + c1], b = M[r * n + c2];
      int next = col_pos[c1 * n + b];
      set(r, c1, b); set(r, c2, a);
      r = next;
    } while (r != r0);
  }
  // Exchange symbols s1 = M[r0, c0] and s2 on the cycle through (r0, c0).
  // Calling it again with the same cell and s1 undoes it.
  void symbol_cycle(int r0, int c0, int s2) {
    int s1 = M[r0 * n + c0];
    int r = r0, c = c0;
    for (;;) {
      int r_next = col_pos[c * n + s2];
      set(r, c, s2);
      int c_next = row_pos[r_next * n + s1];
      set(r_next, c, s1);
      if (r_next == r0) break;
      r = r_next; c = c_next;
    }
  }
};

// [[Rcpp::export]]
List search_local(int n, double seed, double seconds, double stall_limit) {
  Clock::time_point t0 = Clock::now();
  Rng g; g.s = (uint64_t)seed;
  std::vector<int> L = random_latin(n, g);
  Mate m; m.n = n; m.L = L.data();
  long restarts = 0;
  double moves = 0;
  int overall = n * n;
  while (seconds_since(t0) < seconds) {
    m.init(random_latin(n, g));
    ++restarts;
    int best = m.cost;
    double stall = 0;
    while (m.cost > 0 && stall < stall_limit) {
      if (((long long)moves & 0xFFFFF) == 0 && seconds_since(t0) >= seconds) break;
      moves += 1; stall += 1;
      int r, c;
      do { r = g.below(n); c = g.below(n); } while (m.count[L[r * n + c] * n + m.M[r * n + c]] < 2);
      int before = m.cost, kind = g.below(3);
      int other = g.below(n - 1);
      if (kind == 0) {
        if (other >= r) ++other;
        m.row_cycle(r, other, c);
        if (m.cost > before) m.row_cycle(r, other, c);
      } else if (kind == 1) {
        if (other >= c) ++other;
        m.col_cycle(c, other, r);
        if (m.cost > before) m.col_cycle(c, other, r);
      } else {
        int s1 = m.M[r * n + c];
        if (other >= s1) ++other;
        m.symbol_cycle(r, c, other);
        if (m.cost > before) m.symbol_cycle(r, c, s1);
      }
      if (m.cost < best) { best = m.cost; stall = 0; }
    }
    if (best < overall) overall = best;
    if (m.cost == 0) {
      Rprintf("[%.0fs] pair found: restart %ld, %.0f moves\\n", seconds_since(t0), restarts, moves);
      return List::create(_["found"] = true, _["L"] = L, _["M"] = m.M, _["restarts"] = (double)restarts,
                          _["seconds"] = seconds_since(t0));
    }
    Rprintf("[%.0fs] restart %ld stopped at cost %d (best so far %d, %.0f moves)\\n",
            seconds_since(t0), restarts, best, overall, moves);
  }
  return List::create(_["found"] = false, _["restarts"] = (double)restarts, _["seconds"] = seconds_since(t0));
}

// ---- Method 3 ---------------------------------------------------------------

// Assigns a symbol of Z_g to every variable so that the symbols are distinct,
// the differences symbol - u1 are distinct and unused, and so are symbol - u2
// (u = -1: no such constraint for that variable).
struct Developed {
  int g;
  uint32_t full;
  int u1[32], u2[32], value[32];
  double nodes, node_limit;
  Rng* rng;
  uint32_t rot(uint32_t mask, int k) const {
    return k == 0 ? mask : (((mask << k) | (mask >> (g - k))) & full);
  }
  uint32_t candidates(int y, uint32_t free_v, uint32_t free_1, uint32_t free_2) const {
    uint32_t m = free_v;
    if (u1[y] >= 0) m &= rot(free_1, u1[y]);
    if (u2[y] >= 0) m &= rot(free_2, u2[y]);
    return m;
  }
  bool solve(uint32_t vars, uint32_t free_v, uint32_t free_1, uint32_t free_2) {
    if (vars == 0) return true;
    if (++nodes > node_limit) return false;
    int best = 99, y = -1;
    uint32_t options = 0;
    for (uint32_t m = vars; m; m &= m - 1) {
      int x = __builtin_ctz(m);
      uint32_t c = candidates(x, free_v, free_1, free_2);
      int k = __builtin_popcount(c);
      if (k < best) { best = k; y = x; options = c; }
    }
    if (best == 0) return false;
    int order[32], k = 0;
    for (uint32_t m = options; m; m &= m - 1) order[k++] = __builtin_ctz(m);
    for (int i = k - 1; i > 0; --i) std::swap(order[i], order[rng->below(i + 1)]);
    for (int i = 0; i < k; ++i) {
      int v = order[i];
      uint32_t f1 = free_1, f2 = free_2;
      if (u1[y] >= 0) f1 &= ~(1u << ((v - u1[y] + g) % g));
      if (u2[y] >= 0) f2 &= ~(1u << ((v - u2[y] + g) % g));
      value[y] = v;
      if (solve(vars & ~(1u << y), free_v & ~(1u << v), f1, f2)) return true;
      if (nodes > node_limit) return false;
    }
    return false;
  }
};

// [[Rcpp::export]]
List search_cyclic(int n, double seed, double seconds, double node_limit) {
  Clock::time_point t0 = Clock::now();
  Rng rng; rng.s = (uint64_t)seed;
  int g = n - 1, inf = n - 1;
  uint32_t full = (1u << g) - 1u;
  long tries = 0;
  while (seconds_since(t0) < seconds) {
    ++tries;
    // Row 0 of L: s(y) for every y but c (finite), where L(0, c) = inf.
    int c = rng.below(g);
    Developed first;
    first.g = g; first.full = full; first.rng = &rng; first.nodes = 0; first.node_limit = node_limit;
    for (int y = 0; y <= g; ++y) { first.u1[y] = y < g ? y : -1; first.u2[y] = -1; }
    uint32_t vars = (full | (1u << g)) & ~(1u << c);
    if (!first.solve(vars, full, full, full)) continue;
    uint32_t used = 0;
    for (int y = 0; y < g; ++y) if (y != c) used |= 1u << ((first.value[y] - y + g) % g);
    int a3 = __builtin_ctz(full & ~used);
    // Row 0 of M: f(y) for every y but d (finite, not c), where M(0, d) = inf.
    int d = rng.below(g - 1);
    if (d >= c) ++d;
    int a4 = rng.below(g);
    Developed second;
    second.g = g; second.full = full; second.rng = &rng; second.nodes = 0; second.node_limit = node_limit;
    for (int y = 0; y <= g; ++y) {
      second.u1[y] = y < g ? y : -1;
      second.u2[y] = y == c ? -1 : first.value[y];
    }
    vars = (full | (1u << g)) & ~(1u << d);
    if (!second.solve(vars, full, full & ~(1u << a4), full & ~(1u << ((a4 - a3 + g) % g)))) continue;
    // Develop the base columns.
    std::vector<int> L(n * n, -1), M(n * n, -1);
    auto shift = [&](int x, int t) { return x == inf ? inf : (x + t) % g; };
    auto put = [&](int x, int y, int l, int m) {
      for (int t = 0; t < g; ++t) {
        int cell = shift(x, t) * n + shift(y, t);
        L[cell] = shift(l, t); M[cell] = shift(m, t);
      }
    };
    put(inf, 0, a3, a4);
    for (int y = 0; y <= g; ++y) {
      put(0, y, y == c ? inf : first.value[y], y == d ? inf : second.value[y]);
    }
    L[inf * n + inf] = inf; M[inf * n + inf] = inf;
    Rprintf("[%.0fs] developed pair found on try %ld\\n", seconds_since(t0), tries);
    return List::create(_["found"] = true, _["L"] = L, _["M"] = M, _["tries"] = (double)tries,
                        _["seconds"] = seconds_since(t0));
  }
  return List::create(_["found"] = false, _["tries"] = (double)tries, _["seconds"] = seconds_since(t0));
}
')

is_latin <- function(square) {
  symbols <- 0:(nrow(square) - 1L)
  all(apply(square, 1L, function(x) setequal(x, symbols))) &&
    all(apply(square, 2L, function(x) setequal(x, symbols)))
}

started <- Sys.time()
elapsed <- function() as.numeric(difftime(Sys.time(), started, units = "secs"))
result <- list(found = FALSE)
used <- NA_character_
if (shares[1] > 0) {
  used <- "1 (transversal exact cover)"
  result <- search_cover(q, seed, 60 * minutes * shares[1], cap, prefix_cap, node_limit)
}
if (!result$found && shares[2] > 0) {
  used <- "2 (cycle-switch local search)"
  result <- search_local(q, seed, 60 * minutes * sum(shares[1:2]) - elapsed(), 1e6)
}
if (!result$found && shares[3] > 0) {
  used <- "3 (pair developed over Z_(q-1) with one fixed point)"
  result <- search_cyclic(q, seed, 60 * minutes - elapsed(), 5e6)
}
if (!result$found) {
  message(sprintf("Order %d: no pair within %g minutes (seed %d).", q, minutes, seed))
  quit(status = 1L)
}
L <- matrix(as.integer(result$L), q, q, byrow = TRUE)
M <- matrix(as.integer(result$M), q, q, byrow = TRUE)
stopifnot(is_latin(L), is_latin(M), length(unique(L * q + M)) == q * q)
saveRDS(list(q = q, seed = seed, method = used, L = L, M = M, minutes = elapsed() / 60), out)
message(sprintf("Order %d: pair found by method %s, seed %d, %.2f minutes.", q, used, seed, elapsed() / 60))
