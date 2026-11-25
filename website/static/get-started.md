---
description: Install FielDHub, open the Shiny app and make your first field design in R.
---
# Get started

FielDHub is an R package with a Shiny app. You can build a design in the app,
or call the same design function from a script: both give the same field map
and field book for the same inputs and seed.

## Install

From CRAN:

```r
install.packages("FielDHub")
```

The development version, from GitHub:

```r
remotes::install_github("DidierMurilloF/FielDHub")
```

## Open the app

```r
library(FielDHub)
run_app()
```

Choose a design from the menus, fill in its inputs and click **Run!**. The app
shows the field map and the field book, and lets you download them.

## Make a design in R

Every design in the app is also a function. This one places 270 unreplicated
entries and 4 checks in a field of 15 rows and 20 columns, with the checks on
diagonals:

```r
design <- diagonal_arrangement(
  nrows = 15, ncols = 20, lines = 270, checks = 4, seed = 1987
)
```

Every design is used in the same way:

```r
print(design)           # a summary of the design
plot(design)            # the field map
head(design$fieldBook)  # the field book, one row per plot
```

The seed makes the randomization reproducible: the same call gives the same
design.

## Save and reproduce a design

```r
write_design(design, "design.rds")

saved <- read_design("design.rds")
again <- reproduce_design(saved)
```

`write_design()` saves the design with its inputs and seed, `read_design()`
reads it back, and `reproduce_design()` builds it again from that record.

## Next steps

<div class="grid cards fieldhub-cards" markdown>

-   [Articles](articles/index.md)

    A guide for every design, in the app and in R.

-   [Reference](reference/index.md)

    The arguments and results of every function.

-   [Simulating trait data](articles/simulating_traits.md)

    Add a simulated trait to any design.

-   [Changelog](news/index.md)

    What changed in each release.

</div>
