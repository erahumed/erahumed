
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

- **Data**. The inputs for the DSS are packaged as R `data.frame`s
  imported with `{erahumed}`.
- **Analysis**. The entire simulation pipeline is implemented by R
  functions.
- **Visualization**. Simulation outputs are R objects with methods for
  data exploration and visualization.
- **Interactive Interface**. All of the above has a graphical interface
  implemented as a Shiny application, which is also bundled with the
  package.

More information on the ERAHUMED project can be found on the [main
project’s website](https://www.erahumed.com/). The full documentation of
the `{erahumed}` R package is hosted at
[erahumed.github.io/erahumed](https://erahumed.github.io/erahumed/).

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
A bird’s eye view on the various simulation pipeline components is also
available,
[here](https://erahumed.github.io/erahumed/articles/pipeline-scheme.html).

The pipeline always starts by initializing an ERAHUMED simulation, via:

``` r
simulation <- erahumed_simulation()
simulation
#> An ERAHUMED simulation.
#> Computed layers: None
```

This is the main abstraction that `{erahumed}` uses to collect the
various layers involved in the simulation chain of the DSS, namely:

``` r
erahumed_layers()
#> [1] "inp" "hbl" "hbp" "hbd" "ca"  "ct"
```

As the output above shows, the layers of `simulation` are initialized
but not computed yet. We can modify the configuration of specific layers
through the `setup_*()` functions, for instance:

``` r
simulation <- simulation |>
  setup_hydrology(ideal_flow_rate_cm = 2.5) |>
  setup_exposure(dact_m = 0.2)
```

In order to actually compute the layers, we use `run_simulation()`:

``` r
simulation <- simulation |>
  run_simulation(layer = "hbl")  # Run simulation until the hbl layer
```

Simulations are composed of the following layers (in dependency order,
from upstream to downstream):

- INP \[`setup_inp()`\]: INPut data.
- hbl \[`setup_hbl()`\]: Hydrological Balance of the Albufera lake.
- HBP \[`setup_hbp()`\]: Hydrological Balance of rice Paddy clusters.
- CA \[`setup_ca()`\]: Chemical Applications.
- CT \[`setup_ct()`\]: Chemical Transport.

In order to inspect the results of a given layer, we use:

``` r
get_layer(simulation, "hbl")
#> A ERAHUMED HBL simulation layer.
#> 
#> Output columns: level, precipitation_mm, evapotranspiration_mm, date, is_imputed_level, is_imputed_outflow, volume, volume_change, volume_change_petp, outflow_pujol, outflow_perellonet, outflow_perello, outflow_recirculation, outflow_total, inflow_total, residence_time_days
get_layer_parameters(simulation, "hbl")
#> $storage_curve
#> \(level) 25.58 * 1e6 + level * 63.086 * 1e6
#> <environment: 0x00000253a559e848>
#> 
#> $petp_function
#> \(p, etp) 53.9 * 1e3 * (p - etp)
#> <environment: 0x00000253a559e848>
get_layer_output(simulation, "hbl") |> head()
#>       level precipitation_mm evapotranspiration_mm       date is_imputed_level
#> 1 0.3725000              8.2                  0.54 2005-12-20            FALSE
#> 2 0.3726458              0.2                  0.65 2005-12-21            FALSE
#> 3 0.3683833              0.2                  0.62 2005-12-22            FALSE
#> 4 0.3662375              0.2                  1.02 2005-12-23            FALSE
#> 5 0.3667417              0.0                  0.59 2005-12-24            FALSE
#> 6 0.3634917              0.2                  0.86 2005-12-25            FALSE
#>   is_imputed_outflow   volume volume_change volume_change_petp outflow_pujol
#> 1               TRUE 49079535      9200.042             412874      5.739292
#> 2               TRUE 49088735   -268904.075             -24255      5.901080
#> 3               TRUE 48819831   -135372.042             -22638      3.877427
#> 4               TRUE 48684459     31805.858             -44198      5.410336
#> 5               TRUE 48716265   -205029.500             -31801      1.055759
#> 6               TRUE 48511235   -419521.900             -35574      6.922879
#>   outflow_perellonet outflow_perello outflow_recirculation outflow_total
#> 1           2.919471        2.785127                     0     11.443889
#> 2           3.140746        2.919733                     0     11.961558
#> 3           2.500297        2.075301                     0      8.453025
#> 4           3.180986        2.763805                     0     11.355127
#> 5           1.578490        1.226585                     0      3.860835
#> 6           3.786494        3.249450                     0     13.958823
#>   inflow_total residence_time_days
#> 1     6.771737            41.64606
#> 2     9.129972            41.52984
#> 3     7.148233            41.50109
#> 4    12.234802            41.36962
#> 5     1.855875            41.21872
#> 6     9.514982            41.05920
```

For more information on the simulation interface, you can consult
`?erahumed_simulation_interface`.

## Getting help

The full documentation of the `{erahumed}` R package is hosted at
[erahumed.github.io/erahumed](https://erahumed.github.io/erahumed/).

If you have issues running `{erahumed}` or want to suggest an
improvement, please [file an issue on
Github](https://github.com/erahumed/erahumed/issues).

An internal documentation, aimed at potential contributors to this
project, is available at [Github
Wiki](https://github.com/erahumed/erahumed/wiki).
