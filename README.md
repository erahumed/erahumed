
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

The `{erahumed}` R package provides the infrastructure for the [ERAHUMED
Decision Support
System](https://www.erahumed.com/decision-support-system/) (DSS), that
consists of:

- **Data**: The DSS inputs are pre-packaged as R `data.frame`s and
  seamlessly imported with `{erahumed}`.
- **Analysis**. The entire simulation workflow is implemented as a set
  of R functions, enabling flexible and reproducible analysis.
- **Outputs**. Simulation results are returned as well-structured
  `data.frame`s, ready for further exploration and processing in R.
- **Interactive Interface**. A Shiny-based graphical interface, with
  predefined visualizations for model outputs, is bundled with the
  package, allowing users to interactively explore the results.

More information on the ERAHUMED project can be found on the [main
project’s website](https://www.erahumed.com/). The full documentation of
the `{erahumed}` R package is hosted at
[erahumed.github.io/erahumed](https://erahumed.github.io/erahumed/).

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
simulation <- erahumed_simulation(
  date_start = "2020-01-01",
  date_end = "2020-12-31",
  variety_prop = c(J.Sendra = 0.7, Bomba = 0.15, Clearfield = 0.15)
)
```

where the parameters of the simulation are set through the arguments of
`erahumed_simulation()` (see `?erahumed_simulation` or [this
article](https://erahumed.github.io/erahumed/articles/simulation-inputs.html)
for a full list of parameters.

Results can be inspected through:

``` r
get_results(simulation, 
            component = "exposure",  # either "hydrology", "exposure", or "risk"  
            element = "lake"         # either "lake", "ditch", or "cluster"
            ) |>
  head()
#>         date    chemical mf_kg mw_kg ms_kg cw_kg_m3 cw_outflow_kg_m3 cs_kg_m3
#> 1 2020-01-01 Acetamiprid     0     0     0       NA                0        0
#> 2 2020-01-02 Acetamiprid     0     0     0        0                0        0
#> 3 2020-01-03 Acetamiprid     0     0     0        0                0        0
#> 4 2020-01-04 Acetamiprid     0     0     0        0                0        0
#> 5 2020-01-05 Acetamiprid     0     0     0        0                0        0
#> 6 2020-01-06 Acetamiprid     0     0     0        0                0        0
#>   cs_g_kg volume_m3 element_id
#> 1       0        NA       lake
#> 2       0  27344422       lake
#> 3       0  27397000       lake
#> 4       0  27456150       lake
#> 5       0  27479153       lake
#> 6       0  27397000       lake
```

## Getting help

The full documentation of the `{erahumed}` R package is hosted at
[erahumed.github.io/erahumed](https://erahumed.github.io/erahumed/).

If you have issues running `{erahumed}` or want to suggest an
improvement, please [file an issue on
Github](https://github.com/erahumed/erahumed/issues).

An internal documentation, aimed at potential contributors to this
project, is available at [Github
Wiki](https://github.com/erahumed/erahumed/wiki).
