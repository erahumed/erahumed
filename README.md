
<!-- README.md is generated from README.Rmd. Please edit that file -->

# erahumed

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/erahumed/erahumed/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/erahumed/erahumed/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/erahumed/erahumed/graph/badge.svg?token=72POLBUEUR)](https://codecov.io/gh/erahumed/erahumed)
<!-- badges: end -->

The `{erahumed}` R package provides the infrastructure for the [ERAHUMED
Decision Support
System](https://www.erahumed.com/decision-support-system/) (DSS), that
consists of:

- **Data**. The inputs for the DSS are packaged as R `data.frame`s
  imported with `{erahumed}`.
- **Analysis**. The entire modeling pipeline is implemented by R
  functions.
- **Visualization**. All model outputs are R objects with methods for
  data exploration and visualization.
- **Interactive Interface**. All of the above has a graphical interface
  implemented as a Shiny application, which is also bundled with the
  package.

## Installation

You can install the latest release of `{erahumed}` from Github, by
running the following command in your local R session:

``` r
remotes::install_github("erahumed/erahumed@*release")
```

If you want the development version (notice: this may be unstable),
simply omit the `@*release` reference, as in:

``` r
remotes::install_github("erahumed/erahumed")  # development version
```

## Usage

### Example 1: Using the interactive dashboard

The graphical interface to the ERAHUMED DSS can be accessed through the
following R command:

``` r
erahumed::launch_app()
```

This will open the DSS dashboard in your default browser, from where you
can explore model outputs in a user friendly manner.

### Example 2: command line interface to the models

TODO [\#81](https://github.com/erahumed/erahumed/issues/81).

## Getting help

If you have issues running `{erahumed}` or want to suggest an
improvement, please [file an issue on
Github](https://github.com/erahumed/erahumed/issues).

An internal documentation, aimed at potential contributors to this
project, is available at [Github
Wiki](https://github.com/erahumed/erahumed/wiki).
