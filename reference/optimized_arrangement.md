# Generates an Spatial Un-replicated Optimized Arrangement Design

Randomly generates a spatial un-replicated optimized arrangement design,
where the distance between checks is maximized in such a way that each
row and column have control plots. Note that design generation needs the
dimension of the field (number of rows and columns).

## Usage

``` r
optimized_arrangement(
  nrows = NULL,
  ncols = NULL,
  lines = NULL,
  amountChecks = NULL,
  checks = NULL,
  planter = "serpentine",
  l = 1,
  plotNumber = 101,
  seed = NULL,
  exptName = NULL,
  locationNames = NULL,
  spread_reps = TRUE,
  data = NULL
)
```

## Arguments

- nrows:

  Number of rows in the field.

- ncols:

  Number of columns in the field.

- lines:

  Number of genotypes, experimental lines or treatments.

- amountChecks:

  Integer with the amount total of checks or a numeric vector with the
  replicates of each check label.

- checks:

  Number of genotypes as checks.

- planter:

  Option for `serpentine` or `cartesian` arrangement. By default
  `planter = 'serpentine'`.

- l:

  Number of locations. By default `l = 1`.

- plotNumber:

  Numeric vector with the starting plot number for each location. By
  default `plotNumber = 101`.

- seed:

  (optional) Real number that specifies the starting seed to obtain
  reproducible designs.

- exptName:

  (optional) Name of the experiment.

- locationNames:

  (optional) Name for each location.

- spread_reps:

  A logical value indicating whether to maximize the spatial distance
  between replicated treatments in the field. Default is `TRUE`.

- data:

  (optional) Data frame with 3 columns: `ENTRY | NAME | REPS`.

## Value

A list with five elements.

- `infoDesign` is a list with information on the design parameters.

- `layoutRandom` is a matrix with the randomization layout.

- `plotNumber` is a matrix with the layout plot number.

- `dataEntry` is a data frame with the data input.

- `genEntries` is a list with the entries for replicated and no
  replicated part.

- `fieldBook` is a data frame with field book design. This includes the
  index (Row, Column).

## References

Clarke, G. P. Y., & Stefanova, K. T. (2011). Optimal design for
early-generation plant breeding trials with unreplicated or partially
replicated test lines. Australian & New Zealand Journal of Statistics,
53(4), 461–480.

## Author

Didier Murillo \[aut\], Salvador Gezan \[aut\], Ana Heilman \[ctb\],
Thomas Walk \[ctb\], Johan Aparicio \[ctb\], Richard Horsley \[ctb\]

## Examples

``` r
# Example 1: Generates a spatial unreplicated optimized arrangement design in one location
# with 120 genotypes + 20 check plots (4 checks) for a field with dimension 14 rows x 10 cols.
if (FALSE) { # \dontrun{
optim_unrep1 <- optimized_arrangement(
  nrows = 14, 
  ncols = 10, 
  lines = 120, 
  amountChecks = 20, 
  checks = 1:4,
  planter = "cartesian", 
  plotNumber = 101,
  exptName = "20RW1",
  locationNames = "CASSELTON",
  seed = 14124
)
optim_unrep1$infoDesign
optim_unrep1$layoutRandom
optim_unrep1$plotNumber
head(optim_unrep1$fieldBook, 12)
} # }
                  
# Example 2: Generates a spatial unreplicated optimized arrangement design in one location
# with 200 genotypes + 20 check plots (4 checks) for a field with dimension 10 rows x 22 cols.
# As example, we set up the data option with the entries list.
if (FALSE) { # \dontrun{
checks <- 4
list_checks <- paste("CH", 1:checks, sep = "")
treatments <- paste("G", 5:204, sep = "")
REPS <- c(5, 5, 5, 5, rep(1, 200))
treatment_list <- data.frame(list(ENTRY = 1:204, NAME = c(list_checks, treatments), REPS = REPS))
head(treatment_list, 12) 
tail(treatment_list, 12)
optim_unrep2 <- optimized_arrangement(
  nrows = 10, 
  ncols = 22, 
  planter = "serpentine", 
  plotNumber = 101,
  seed = 120,
  exptName = "20YWA2",
  locationNames = "MINOT",
  data = treatment_list
)
optim_unrep2$infoDesign
optim_unrep2$layoutRandom
optim_unrep2$plotNumber
head(optim_unrep2$fieldBook,12)
} # }
                  
```
