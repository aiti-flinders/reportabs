
<!-- README.md is generated from README.Rmd. Please edit that file -->

# reportabs

<!-- badges: start -->

[![R-CMD-check](https://github.com/aiti-flinders/reportabs/workflows/R-CMD-check/badge.svg)](https://github.com/aiti-flinders/reportabs/actions)

<!-- badges: end -->

The `reportabs` package is designed to make reporting on ABS data
easier. It is designed to work with (most of!) the data included in the
`aitidata` package. `reportabs` contains functions to help with both
visual and textual reporting of data.

## Installation

`reportabs` can be installed from github with:

``` r
# install.packages("remotes")
remotes::install_github("f-fof/reportabs")
```

This package works with ABS Time Series data. A collection of pre-tidied
ABS data is available through the `aitidata` package. This package is
large and constantly updated, so you shouldn’t need to install it.
Instead, you can access the data through this package.

``` r
labour_force <- read_absdata("labour_force")
```

It is also recommended that the tidyverse is installed and loaded.

``` r
# install.packages("tidyverse")
library(tidyverse)
```

## What can this package do?

This package is designed to:

- report changes in labour market (or other data) indicators;
- plot labour market (or other data) indicators over time;
- generate the monthly labour force briefing

### Reporting

The following functions can assist with reporting ABS time series data:

- `current()`: Report the current value of an indicator.
- `growth()`: Report the growth of an indicator over a specified period.
- `change()`: Report the change in the level of an indicator over a
  specified period.
- `last_value()`: Report the value of an indicator for the previous year
  or month.
- `value_at()`: Report the value of an indicator in a specific year and
  month.
- `average_over()`: Report the average value of an indicator between a
  specified period.
- `release()`: Report the most recent year or month of the data.

The key to each of these functions it the argument `filter_with`. This
argument takes a list of parameters which must be specified to generate
the correct data. `filter_with` requires you to specify *at least* an
indicator by `list(indicator = " ")`. The available indicators for each
dataset can be viewed with `distinct()` from the dplyr package for
example `distinct(labour_force, indicator)`. `filter_with` can be used
to also filter by gender, a state/territory (including Australia), an
age group, an industry, and a series type.
