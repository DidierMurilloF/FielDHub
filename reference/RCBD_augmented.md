# Generates an Augmented Randomized Complete Block Design (ARCBD)

It randomly generates an augmented randomized complete block design
across locations (ARCBD).

## Usage

``` r
RCBD_augmented(
  lines = NULL,
  checks = NULL,
  b = NULL,
  l = 1,
  planter = "serpentine",
  plotNumber = 101,
  repsStack = c("vertical", "horizontal"),
  exptName = NULL,
  seed = NULL,
  locationNames = NULL,
  repsExpt = 1,
  random = TRUE,
  data = NULL,
  nrows = NULL,
  ncols = NULL
)
```

## Arguments

- lines:

  Treatments, number of lines for test.

- checks:

  Number of checks per augmented block.

- b:

  Number of augmented blocks.

- l:

  Number of locations. By default `l = 1`.

- planter:

  Option for `serpentine` or `cartesian` arrangement. By default
  `planter = 'serpentine'`.

- plotNumber:

  Numeric vector with the starting plot number for each location. By
  default `plotNumber = 101`.

- repsStack:

  Option for `horizontal` or `vertical` layout By default
  `repsStack = 'vertical'`.

- exptName:

  (optional) Name of experiment.

- seed:

  (optional) Real number that specifies the starting seed to obtain
  reproducible designs.

- locationNames:

  (optional) Name for each location.

- repsExpt:

  (optional) Number of reps of experiment. By default `repsExpt = 1`.

- random:

  Logical value to randomize treatments or not. By default
  `random = TRUE`.

- data:

  (optional) Data frame with the labels of treatments.

- nrows:

  (optional) Number of rows in the field.

- ncols:

  (optional) Number of columns in the field.
