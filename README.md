
<!-- README.md is generated from README.Rmd. Please edit that file -->

# erahumed

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R-CMD-check](https://github.com/erahumed/erahumed/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/erahumed/erahumed/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/erahumed/erahumed/graph/badge.svg?token=72POLBUEUR)](https://codecov.io/gh/erahumed/erahumed)
[![Website](https://img.shields.io/badge/Website-here-blue)](https://erahumed.github.io/erahumed/)

<!-- badges: end -->

The `{erahumed}` R package implements the computational engine of the
[ERAHUMED Decision Support
System](https://www.erahumed.com/decision-support-system/), a modelling
framework for assessing hydrology, pesticide fate, and ecological risk
in the Albufera Natural Park (València, Spain). It integrates models
that simulate water flows, contaminant transport, and biological effects
across the park’s interconnected water bodies.

With `{erahumed}`, users can:

- **Configure simulations** – Define landscape, chemical, and management
  parameters, or use built-in presets for rapid setup.

- **Run the model** – Execute hydrology, exposure, and risk assessment
  modules in a reproducible R workflow.

- **Analyse outputs** – Retrieve results as tidy `data.frame`s, ready
  for statistical analysis or visualization.

- **Explore interactively** – Launch a bundled Shiny application with
  predefined plots, maps, and tables for model inputs and results.

The package is designed for both research and practical decision-making,
enabling transparent scenario analysis and reproducible environmental
assessments.

Comprehensive resources are available to support users at different
levels:

- The [package documentation](https://erahumed.github.io/erahumed)
  covers the R functions, datasets, and Shiny interface bundled with
  `{erahumed}`, serving as a practical reference for day-to-day use.

- A [user manual](https://erahumed.github.io/erahumed-book) provides an
  in-depth description of the underlying models, algorithms, and
  assumptions. It is intended for researchers who wish to understand
  and/or extend the modelling framework.

Further background on the ERAHUMED project can be found on the [main
project website](https://www.erahumed.com/).

## Installation

You can install the latest release of `{erahumed}` from Github, by
running the following command in your local R session:

``` r
install.packages("remotes")  # If necessary
remotes::install_github("erahumed/erahumed", ref = remotes::github_release())
```

If you want the development version (notice: this may be unstable),
simply omit the reference:

``` r
remotes::install_github("erahumed/erahumed")  # development version
```

## Usage

### Example 1: Using the interactive dashboard

The graphical interface to the ERAHUMED DSS can be accessed through the
following R command:

``` r
erahumed::launch_dss()
```

This will open the DSS dashboard in your default browser, from where you
can explore simulation outputs in a user friendly manner.

### Example 2: command line interface to simulations

``` r
library(erahumed)
```

The following example illustrates the workflow for manually running the
ERAHUMED simulation chain, and extracting the outputs of the various
simulation layers. For more detailed information, see the [main package
vignette](https://erahumed.github.io/erahumed/articles/erahumed-workflow.html).

Simulations are run via:

``` r
simulation <- erahumed_simulation()
#> Initializing inputs
#> Computing hydrology: lake
#> Computing hydrology: clusters
#> Computing hydrology: ditches
#> Computing exposure: clusters
#> Computing exposure: ditches
#> Computing exposure: lake
#> Computing risk: clusters
#> Computing risk: ditches
#> Computing risk: lake
```

where simulation inputs can be customized through the arguments of
`erahumed_simulation()`. Results can be inspected through:

``` r
get_results(simulation, 
            component = "exposure",  # either "hydrology", "exposure", or "risk"  
            element = "lake"         # either "lake", "ditch", or "cluster"
            ) |>
  head()
#>   element_id chemical_id chemical_name       date mf_kg mw_kg ms_kg
#> 1       lake           1   Acetamiprid 2020-01-01     0     0     0
#> 2       lake           1   Acetamiprid 2020-01-02     0     0     0
#> 3       lake           1   Acetamiprid 2020-01-03     0     0     0
#> 4       lake           1   Acetamiprid 2020-01-04     0     0     0
#> 5       lake           1   Acetamiprid 2020-01-05     0     0     0
#> 6       lake           1   Acetamiprid 2020-01-06     0     0     0
#>   mw_outflow_kg cw_kg_m3 cs_kg_m3 cs_g_kg cw_outflow_kg_m3 volume_m3 outflow_m3
#> 1             0       NA        0       0                0        NA   274652.4
#> 2             0        0        0       0                0  27344422   333355.2
#> 3             0        0        0       0                0  27397000   153987.6
#> 4             0        0        0       0                0  27456150   119992.8
#> 5             0        0        0       0                0  27479153   186673.2
#> 6             0        0        0       0                0  27397000   221466.0
```

## Getting help

The full documentation of the `{erahumed}` R package is hosted at
[erahumed.github.io/erahumed](https://erahumed.github.io/erahumed/).

If you have issues running `{erahumed}` or want to suggest an
improvement, please [file an issue on
Github](https://github.com/erahumed/erahumed/issues).
