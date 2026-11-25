// Random streams for the simulation helpers.
#include <cpp11.hpp>
#include <random>

// An isolated uniform stream for simulations made while R's Box-Muller
// generator is active. Calling R's set.seed() would discard its cached normal.

[[cpp11::register]]
SEXP simulation_rng_cpp(int seed) {
  return cpp11::external_pointer<std::mt19937_64>(new std::mt19937_64(seed));
}

[[cpp11::register]]
cpp11::doubles simulation_uniforms_cpp(SEXP stream, int n) {
  cpp11::external_pointer<std::mt19937_64> rng(stream);
  cpp11::writable::doubles draws(n);
  for (int i = 0; i < n; ++i) {
    // The open interval keeps the inverse normal finite at both endpoints.
    draws[i] = (static_cast<double>((*rng)() >> 12) + 0.5) * 0x1p-52;
  }
  return draws;
}
