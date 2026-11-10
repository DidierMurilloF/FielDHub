# Provenance of the compiled block engine

The engine is a clean-room implementation. Nobody working on it read the source
code of the blocksdesign package; blocksdesign is used only as an installed
black box in tests and benchmarks, to compare output shape and efficiency.

| File | Content | Sources |
|---|---|---|
| `block_space.h`, `block_space.cpp` | Block-space information matrix, swap ratio, Woodbury update, D- and A-efficiency | Edmondson (2020), JABES 25:500-522, sections 2.1-2.3 (model, conditional swaps, D-optimality); the matrix determinant lemma, Woodbury and Sylvester identities; research derivation `derivation.md` (gate G0, 4 Nov 2026). The cached ratio (`enable_cache()`, `delta_cached()`: G = Minv N'/r, updated by rank 2) is FielDHub's own rearrangement of the same ratio (spec-m5 section 6.5) |
| `blocks_engine.cpp` | Randomized first-improvement swap search, escapes, budget | Edmondson (2020) section 2.2; Mitchell (1974), Technometrics 16:203-210 (exchange search); spec-v2 sections 6, 8, 9 |
| `engine_rng.h`, `engine_rng.cpp` | Draws through R's generator (`R_unif_index`); test hook | Writing R Extensions, section 6.3 |
| `blocks_engine.cpp` (`even_repair()`) | Even-up repair of the chosen design: swaps that leave every treatment-block count within one of the others | FielDHub's own rule (spec-m5 section 6.1): each swap lowers the sum of squared counts by at least 2 and a partner always exists (counting argument); candidates are scored with the swap ratio above; `do_optim()` is the engine's first caller |
| `blocks_engine.cpp` (`lattice_start()`) | Square-lattice start for the first search when v = q^2 and the blocks hold q plots | Textbook construction (Yates 1936): rows, columns and the Latin squares (i + m j) mod q, orthogonal for up to three replicates at any q and q + 1 at prime q; labels randomly permuted (spec-m5 section 6.4) |
| `block_space_probe.cpp` | Test hook that replays swaps on the block-space state | spec-v2 section 12 |
| `engine_budget.h` | Evaluation budget, interrupts, time limit | spec-v2 sections 6 and 9 |
| `R/engine_blocks.R` (`block_a_bound()`) | Convexity bound on A-efficiency | John & Williams (1995), *Cyclic and Computer Generated Designs*, ch. 2 (efficiency factors); own derivation (plan review gate) |
