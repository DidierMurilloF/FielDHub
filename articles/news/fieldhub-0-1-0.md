# 

### FielDHub 0.1.0

![](https://raw.githubusercontent.com/DidierMurilloF/unsplash-img/master/img/karsten-wurth-rafblRbne3o-unsplash.jpeg)
Photo by [Karsten
Würth](https://unsplash.com/@karsten_wuerth?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText)
on
[Unsplash](https://unsplash.com/s/photos/road?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText)

##### ![](https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/solid/calendar-check.svg) 2021/05/17

##### ![](https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/solid/code.svg)[FielDHub](https://github.com/DidierMurilloF/FielDHub)

##### ![](https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/solid/chalkboard-user.svg) Didier Murillo

I am delighted to announce the initial release of FielDHub to CRAN!
FielDHub is conceived to make it quick and easy to generate, randomize,
and plot complex and standard experimental designs. This initial release
is version 0.1.0 in recognition that FielDHub has been in development
for more than one year and has already been used for researchers at
NDSU, CIAT, as well as for teaching.

Install FielDHub with:

``` r
install.packages("FielDHub")
```

#### Usage

Get started using only two simple lines of code:

``` r
library(FielDHub)
run_app()
```

#### Relevant Features

Unreplicated and partially replicated designs are commonly used in plant
breeding and forestry but with a lack of free tools available to
researchers to make that randomization.

- FielDHub provides an easy way to complete designs by using the app or
  through the standalone functions such as
  [`diagonal_arrangement()`](https://didiermurillof.github.io/FielDHub/reference/diagonal_arrangement.md),
  [`optimized_arrangement()`](https://didiermurillof.github.io/FielDHub/reference/optimized_arrangement.md)
  and
  [`RCBD_augmented()`](https://didiermurillof.github.io/FielDHub/reference/RCBD_augmented.md).

- Partially replicated design can be done using the function
  [`partially_replicated()`](https://didiermurillof.github.io/FielDHub/reference/partially_replicated.md).

- The app provides novel features to make the randomization along with
  the field layout or map.

- FielDHub’s features such as generating synthetic data along with
  randomization, as well as plotting field layouts make the app suitable
  to teach statistic courses such as experimental design.

#### Acknowledgements

FielDHub has been a long time coming, and it wouldn’t have been possible
without a devoted community of users, many of whom have gone on to
contribute fixes and new ideas. I would like to particularly thank
**Dr. Richard Horsley** (Professor, Department Head & Barley Breeder at
Department of Plant Sciences) who sponsored the development of this
project. Also, a big thanks go out to **Dr. Ana María Heilman** and
**Dr. Andrew Green** for all the support in the plant
breeding/biological background. This project could not be the same
without all the contributions and knowledge of **Dr. Salvador Gezan**.
He came to the project at a critical moment and with his ideas and code,
we went beyond what was expected. Thank you to **Johan Aparicio** and
**Thomas Walk** for all the contributions to FielDHub.

FielDHub was submitted and published in The Journal of Open Source
Software. The peer review process was done by [Thiago de Paula
Oliveira](https://github.com/Prof-ThiagoOliveira) (Reviewer), [David
LeBauer](https://github.com/dlebauer) (Reviewer), and [Charlotte
Soneson](https://github.com/csoneson) (Editor). Thank you to all of you
for your work, effort, and contributions.
