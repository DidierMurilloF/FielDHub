# Generates a Spatial Partially Replicated Arrangement Design

Randomly generates a spatial partially replicated (p-rep) design for
single or multiple locations.

## Usage

``` r
partially_replicated(
  nrows = NULL,
  ncols = NULL,
  repGens = NULL,
  repUnits = NULL,
  planter = "serpentine",
  l = 1,
  plotNumber = 101,
  spread_reps = TRUE,
  seed = NULL,
  exptName = NULL,
  locationNames = NULL,
  multiLocationData = FALSE,
  dist_method = "euclidean",
  border_penalization = 0.5,
  data = NULL
)
```

## Arguments

- nrows:

  Numeric vector with the number of rows field at each location.

- ncols:

  Numeric vector with the number of columns field at each location.

- repGens:

  Numeric vector with the amount genotypes to replicate.

- repUnits:

  Numeric vector with the number of reps of each genotype.

- planter:

  Option for `serpentine` or `cartesian` movement. By default
  `planter = 'serpentine'`.

- l:

  Number of locations. By default `l = 1`.

- plotNumber:

  Numeric vector with the starting plot number for each location. By
  default `plotNumber = 101`.

- spread_reps:

  A logical value indicating whether to maximize the spatial distance
  between replicated treatments in the field. Default is `TRUE`.

- seed:

  (optional) Real number that specifies the starting seed to obtain
  reproducible designs.

- exptName:

  (optional) Name of the experiment.

- locationNames:

  (optional) Name for each location.

- multiLocationData:

  (optional) Option to pass an entry list for multiple locations.

- dist_method:

  The method used for distance calculation. Options are "euclidean"
  (default) and "manhattan". By default `multiLocationData = FALSE`.

- border_penalization:

  A tuning parameter for the centrality penalty. Default is 0.5.

- data:

  (optional) Data frame with 3 columns: `ENTRY | NAME | REPS`. If
  `multiLocationData = TRUE` then the `data` must have 4 columns:
  `LOCATION | ENTRY | NAME | REPS`

## Value

A list with several elements.

- `infoDesign` is a list with information on the design parameters.

- `layoutRandom` is a matrix with the randomization layout.

- `plotNumber` is a matrix with the layout plot number.

- `binaryField` is a matrix with the binary field.

- `dataEntry` is a data frame with the data input.

- `genEntries` is a list with the entries for replicated and
  non-replicated parts.

- `fieldBook` is a data frame with field book design. This includes the
  index (Row, Column).

- `min_pairwise_distance` is a data frame with the minimum pairwise
  distance between each pair of locations.

- `reps_info` is a data frame with information on the number of
  replicated and non-replicated treatments at each location.

- `pairsDistance` is a data frame with the pairwise distances between
  each pair of treatments.

- `treatments_with_reps` is a list with the entries for the replicated
  part of the design.

- `treatments_with_no_reps` is a list with the entries for the
  non-replicated part of the design.

## Details

This function generates and optimizes a partially replicated (p-rep)
experimental design for a given set of treatments and replication
levels. The design is represented by a matrix and optimized using a
pairwise distance metric. The function outputs various information about
the optimized design including the field layout, replicated and
unreplicated treatments, and pairwise distances between treatments. Note
that the design generation needs the dimension of the field (number of
rows and columns).

## References

Cullis, S., B. R., & Coombes, N. E. (2006). On the design of early
generation variety trials with correlated data. Journal of Agricultural,
Biological, and Environmental Statistics, 11, 381–393.
https://doi.org/10.1198/108571106X154443

## Author

Didier Murillo \[aut\], Salvador Gezan \[aut\], Ana Heilman \[ctb\],
Thomas Walk \[ctb\], Johan Aparicio \[ctb\], Jean-Marc Montpetit
\[ctb\], Richard Horsley \[ctb\]

## Examples

``` r
# Example 1: Generates a spatial optimized partially replicated arrangement design in one 
# location with 335 genotypes for a field with dimensions 15 rows x 28 cols. 
# Note that there are 250 genotypes unreplicated (only one time), 85 genotypes replicated 
# two times, and three checks 8 times each.
if (FALSE) { # \dontrun{
prep_deseign1 <- partially_replicated(
 nrows = 12, 
 ncols = 37,  
 repGens = c(250, 85, 3),
 repUnits = c(1, 2, 8),
 planter = "cartesian", 
 plotNumber = 101,
 seed = 77
)
prep_deseign1$infoDesign
prep_deseign1$layoutRandom
prep_deseign1$plotNumber
head(prep_deseign1$fieldBook, 12)
} # }

# Example 2: Generates a spatial optimized partially replicated arrangement design with 492 
# genotypes in a field with dimensions 30 rows x 20 cols. Note that there 384 genotypes 
# unreplicated (only one time), 108 genotypes replicated two times. 
# In this case we don't have check plots.
# As example, we set up the data option with the entries list.
if (FALSE) { # \dontrun{
NAME <- paste("G", 1:492, sep = "")
repGens = c(108, 384);repUnits = c(2,1)
REPS <- rep(repUnits, repGens)
treatment_list <- data.frame(list(ENTRY = 1:492, NAME = NAME, REPS = REPS))
head(treatment_list, 12) 
tail(treatment_list, 12)
prep_deseign2 <- partially_replicated(
  nrows = 30, 
  ncols = 20, 
  planter = "serpentine", 
  plotNumber = 101,
  seed = 41,
  data = treatment_list
)
prep_deseign2$infoDesign
prep_deseign2$layoutRandom
prep_deseign2$plotNumber
head(prep_deseign2$fieldBook, 10)
} # }
```
