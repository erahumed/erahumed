
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

``` r
library(erahumed)
```

The following example illustrates the workflow for manually running the
ERAHUMED model chain, and extracting the outputs of the various model
layers.

The pipeline always starts by initializing a new ERAHUMED model, via:

``` r
model <- erahumed_simulation()
model
#> An ERAHUMED simulation.
#> Calculated layers:  None
```

This is the main abstraction that `{erahumed}` uses to collect the
various layers involved in the modeling chain of the DSS, and is
initially blank.

Next, we populate model layers, which is achieved through the set of
`compute_*()` functions:

- `compute_inp()` - INPut data.
- `compute_hba()` - Hydrological Balance of the Albufera lake.
- `compute_hbp()` - Hydrological Balance of rice Paddy clusters.
- `compute_ca()` - Chemical Applications.
- `compute_ct()` - Chemical Transport.

For instance, we can compute the first two layers with default arguments
(*i.e.* default input data and parameters for hydrological balance
computation) as follows:

``` r
model <- model |> compute_inp() |> compute_hba()
model
#> An ERAHUMED simulation.
#> Calculated layers:  inp, hba
```

We can extract and inspect model layers as illustrated in the following
examples:

``` r
hba(model)  # Extract the HBA model layer
#> A ERAHUMED HBA simulation layer.
#> 
#> Output columns: level, precipitation_mm, evapotranspiration_mm, date, is_imputed_level, is_imputed_outflow, volume, volume_change, volume_change_petp, outflow_pujol, outflow_perellonet, outflow_perello, outflow_extra, outflow_total, inflow_total, residence_time_days
hba(model) |> 
  layer_output() |>  # Get output data.frame
  tibble::as_tibble()    # Just for pretty printing
#> # A tibble: 6,139 × 16
#>    level precipitation_mm evapotranspiration_mm date       is_imputed_level
#>    <dbl>            <dbl>                 <dbl> <date>     <lgl>           
#>  1 0.372              8.2                  0.54 2005-12-20 FALSE           
#>  2 0.373              0.2                  0.65 2005-12-21 FALSE           
#>  3 0.368              0.2                  0.62 2005-12-22 FALSE           
#>  4 0.366              0.2                  1.02 2005-12-23 FALSE           
#>  5 0.367              0                    0.59 2005-12-24 FALSE           
#>  6 0.363              0.2                  0.86 2005-12-25 FALSE           
#>  7 0.357              0.2                  0.63 2005-12-26 FALSE           
#>  8 0.370              0                    1.96 2005-12-27 FALSE           
#>  9 0.346              0                    0.95 2005-12-28 FALSE           
#> 10 0.347              0                    1.37 2005-12-29 FALSE           
#> # ℹ 6,129 more rows
#> # ℹ 11 more variables: is_imputed_outflow <lgl>, volume <dbl>,
#> #   volume_change <dbl>, volume_change_petp <dbl>, outflow_pujol <dbl>,
#> #   outflow_perellonet <dbl>, outflow_perello <dbl>, outflow_extra <dbl>,
#> #   outflow_total <dbl>, inflow_total <dbl>, residence_time_days <dbl>
```

layers have plot methods that can help data exploration:

``` r
plot(hba(model), variable = "outflow_total")
```

(The output is an interactive plot, not shown here. Try yourself!)

Extracting a layer that has not been computed yet yields a `NULL`
result, *e.g.*:

``` r
hbp(model)
#> NULL
```

If we attempt to compute a downstream layer whose dependencies have not
been defined yet, we get an informative error. For example:

``` r
compute_ca(model)
#> Error in doTryCatch(return(expr), name, parentenv, handler): Upstream layer 'hbp' of simulation must be computed first.
```

Also, computing a layer erases the output of its downstream dependencies
(if any):

``` r
compute_inp(model) |> hba()
#> NULL
```

The order of model layers in the list above is the logical one. Each
layer depends on the previous ones (referred to as “upstream”), and is a
dependency of the subsequent ones (referred to as “downstream”).

``` r
model <- compute_inp(model)
model
#> An ERAHUMED simulation.
#> Calculated layers:  inp
```

## Getting help

If you have issues running `{erahumed}` or want to suggest an
improvement, please [file an issue on
Github](https://github.com/erahumed/erahumed/issues).

An internal documentation, aimed at potential contributors to this
project, is available at [Github
Wiki](https://github.com/erahumed/erahumed/wiki).
