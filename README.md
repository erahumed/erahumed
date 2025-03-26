
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

The pipeline always starts by initializing an ERAHUMED simulation, via:

``` r
simulation <- erahumed_simulation()
simulation
#> An ERAHUMED simulation.
#> Computed layers: None
```

This is the main abstraction that `{erahumed}` uses to collect the
various computational layers involved in a simulation. The configuration
of specific aspects of the simulation happens through the
`setup_hydrology()`, `setup_exposure()` and `setup_risk()` functions,
for instance:

``` r
simulation <- simulation |>
  setup_hydrology(ideal_flow_rate_cm = 2.5) |>
  setup_exposure(dact_m = 0.2)
```

In order to start computations we use:

``` r
simulation <- simulation |>
  run_simulation()
```

Finally, results can be inspected through:

``` r
get_results(simulation, 
            component = "exposure",  # either "hydrology", "exposure", or "risk"  
            element = "lake"         # either "lake", "ditch", or "cluster"
            ) |>
  head()
#>         date    chemical mf_kg        mw_kg        ms_kg     cw_kg_m3
#> 1 2005-12-20 Acetamiprid     0 0.000000e+00 0.000000e+00           NA
#> 2 2005-12-21 Acetamiprid     0 0.000000e+00 0.000000e+00 0.000000e+00
#> 3 2005-12-22 Acetamiprid     0 6.558011e-05 0.000000e+00 1.343309e-12
#> 4 2005-12-23 Acetamiprid     0 1.261073e-04 1.186372e-05 2.590299e-12
#> 5 2005-12-24 Acetamiprid     0 1.301073e-04 3.379491e-05 2.670716e-12
#> 6 2005-12-25 Acetamiprid     0 1.943898e-04 5.423975e-05 4.007109e-12
#>   cw_outflow_kg_m3     cs_kg_m3      cs_g_kg volume_eod_m3 volume_sod_m3
#> 1     0.000000e+00 0.000000e+00 0.000000e+00      49088735            NA
#> 2     0.000000e+00 0.000000e+00 0.000000e+00      48819831      49088735
#> 3     0.000000e+00 0.000000e+00 0.000000e+00      48684459      48819831
#> 4     1.058758e-12 1.100530e-12 7.336869e-13      48716265      48684459
#> 5     2.076471e-12 3.134963e-12 2.089976e-12      48511235      48716265
#> 6     2.122158e-12 5.031517e-12 3.354345e-12      48091713      48511235
#>   element_id
#> 1       lake
#> 2       lake
#> 3       lake
#> 4       lake
#> 5       lake
#> 6       lake
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
