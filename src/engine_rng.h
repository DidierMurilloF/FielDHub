// Random draws for the block engine. Only R's generator is used, so set.seed()
// and RNGkind() govern every draw. Callers bracket use with GetRNGstate() and
// PutRNGstate() (see blocks_engine.cpp).
#pragma once
#include <R_ext/Random.h>

// Uniform integer in [0, n), drawn with R's rejection sampler: the same draw
// that sample.int(n, 1) makes under the default sample.kind = "Rejection".
inline int draw_index(int n) {
  return static_cast<int>(R_unif_index(static_cast<double>(n)));
}
