# 

### FielDHub 1.3.1

![](https://raw.githubusercontent.com/DidierMurilloF/unsplash-img/master/img/markus-spiske-iar-afB0QQw-unsplash.jpg)
Photo by [Markus Spiske](https://www.pexels.com/@markusspiske/) on
[Pexels](https://www.pexels.com/photo/close-up-photo-of-matrix-background-1089438/)

##### ![](https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/solid/calendar-check.svg) 2023/04/18

##### ![](https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/solid/code.svg)[FielDHub](https://github.com/DidierMurilloF/FielDHub)

##### ![](https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/solid/chalkboard-user.svg) Didier Murillo

I am thrilled to announce the release of FielDHub v1.3.1, which is the
culmination of dedicated effort and hard work. This updated version
includes improvements and new features, including **sparse allocation**,
**optimized multi-location p-rep**, and more. We are excited to share
these new capabilities with our users.

#### Changelog

#### New Features in the Shiny App

- Added a module to generate **Sparse allocation**.

- Added a module for generating **Optimized Multi-Location Partially
  Replicated (p-rep)**.

- Added **vignettes and help documentation** for all the new modules;
  Sparse Allocations and Optimized Multi-Location Partially Replicated
  (p-rep) Designs in the app.

##### Enhancements:

- Renamed the **Partially Replicated** module to **Single and
  Multi-Location p-rep**

- Improved the usability of the field dimensions dropdown menu by
  reordering the options based on the absolute value of the difference
  between the number of rows and columns for each option. This affects
  unreplicated and partially replicated design modules.

##### Fix bugs:

- Fixed issue: Upload data in the CRD module.

#### New Features in the `FielDHub` Package:

##### Standalone Functions

- Created the
  [`do_optim()`](https://didiermurillof.github.io/FielDHub/reference/do_optim.md)
  function. This function generates the sparse or p-rep allocation to
  multiple locations. It optimized the allocation by using incomplete
  blocks.

- Created the
  [`sparse_allocation()`](https://didiermurillof.github.io/FielDHub/reference/sparse_allocation.md)
  function. This new function uses the other function,
  [`do_optim()`](https://didiermurillof.github.io/FielDHub/reference/do_optim.md),
  to generate the sparse allocation, then uses the function
  [`diagonal_arrangement()`](https://didiermurillof.github.io/FielDHub/reference/diagonal_arrangement.md)
  to create unreplicated designs across multiple locations.

- Created the
  [`multi_location_prep()`](https://didiermurillof.github.io/FielDHub/reference/multi_location_prep.md)
  function. It uses within the optimization function
  [`do_optim()`](https://didiermurillof.github.io/FielDHub/reference/do_optim.md)
  to generate the partially replicated (p-rep) allocation, then uses the
  function
  [`partially_replicated()`](https://didiermurillof.github.io/FielDHub/reference/partially_replicated.md)
  to create the p-rep designs across multiple locations.

- Created the `pairs_distance()` function. This function calculates
  pairwise distances between all elements in a matrix that appears twice
  or more.

- Created the
  [`swap_pairs()`](https://didiermurillof.github.io/FielDHub/reference/swap_pairs.md)
  function. It swaps pairs in a matrix of integers and optimizes the
  p-rep design. This function modifies the input matrix $`X`$ to ensure
  that the distance between any two occurrences of the same integer is
  at least a distance $`d`$, by swapping one of the occurrences with a
  random occurrence of a different integer that is at least $`d`$ away.
  The function starts with starting dist at $`d = 3`$ and increases it
  by $`1`$ until the algorithm no longer converges or the max number of
  iterations have been performed.

- Created the `search_matrix_values()` function. It looks for values
  that appear in the same row in a matrix and return the row number,
  value, and frequency.

- Added optimization process for the partially replicated (p-rep)
  designs. It uses the function
  [`swap_pairs()`](https://didiermurillof.github.io/FielDHub/reference/swap_pairs.md).

- Added **vignettes and help documentation** for all the new functions.

##### Enhancements:

- [`partially_replicated()`](https://didiermurillof.github.io/FielDHub/reference/partially_replicated.md)
  accepts custom field dimensions at each location. For example,
  `nrows = c(23, 20, 20)` and `ncols = c(20, 23, 23)` are the field rows
  and columns for the three environments.

- Code refactoring on the
  [`diagonal_arrangement()`](https://didiermurillof.github.io/FielDHub/reference/diagonal_arrangement.md)
  function.

- Code refactoring on the utility function `pREP()`.

- Avoid cyclic reps in incomplete block designs when the number of
  treatments is square.

### Acknowledgements

FielDHub v1.3.1 results from dedicated effort and contribution from a
group of individuals who have made this release possible. We want to
extend our sincere gratitude to Mr. [Jean-Marc
Montpetit](https://www.linkedin.com/in/jean-marc-montpetit/) for his
contributions to developing the
[`swap_pairs()`](https://didiermurillof.github.io/FielDHub/reference/swap_pairs.md)
and `pairs_distance()` functions. His help enhanced the optimization of
the partially replicated (p-rep) design. Thank you, Dr. [Salvador
Gezan](https://www.linkedin.com/in/salvador-gezan-54768a1a/), for your
contributions and fresh ideas. We also thank [Matthew
Seefeldt](https://github.com/seefeldtm) for helping write documentation
and [Johan Aparicio](https://github.com/AparicioJohan) for his ideas and
reporting bugs. Thanks, [Ana María Heilman](https://github.com/tatirri),
for the support and leadership throughout the development process.
