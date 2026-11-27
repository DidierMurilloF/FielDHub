---
title: Choose a Field Trial Design for Plant Breeding
description: Choose among RCBD, alpha lattice, augmented, p-rep, and sparse testing designs based on seed, plots, blocks, and locations. Randomize the trial in R or Shiny.
---

# Choose a field trial design for plant breeding

Choose a design by counting how many plots each entry can occupy, how many entries fit in a reasonably uniform block, and how many locations can test each entry. Then use the linked guide to randomize the plots and produce a field map and field book in R or Shiny.

| What you can plant | Design to consider |
| --- | --- |
| Every treatment once in each reasonably uniform block | [Randomized complete block design (RCBD)](articles/rcbd.md) |
| Every treatment in each replicate, with smaller blocks within replicates | [Alpha lattice](articles/alpha_lattice.md) or [resolvable incomplete blocks](articles/incomplete_blocks.md) |
| Most new lines once, with enough seed to repeat checks in every block | [Augmented RCBD](articles/RCBD_augmented.md) |
| Most new lines once in a rectangular field, with checks repeated along rows or columns | [Augmented row-column design](articles/augmented_row_column.md) |
| Some entries more than once and the rest once | [Partially replicated design (p-rep)](articles/partially_replicated.md) |
| Entry copies that need to be spread across several locations | [Multi-location p-rep](articles/multi_location_prep.md) |
| Each genotype at only a subset of locations | [Sparse testing](articles/sparse_allocation.md) |

Before randomizing, list the entries and checks, available plots and field dimensions at each location, and any replication limits. Keep the seed with the resulting layout so you can reproduce the randomization.

[Install FielDHub and run a first design](get-started.md#install) or [browse all design guides](articles/index.md).
