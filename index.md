# erahumed

The [erahumed](https://github.com/erahumed/erahumed) R package
implements the [ERAHUMED Decision Support
System](https://www.erahumed.com/decision-support-system/), a modelling
framework for assessing hydrology, pesticide exposure, and ecological
risk in the Albufera Natural Park (València, Spain). It integrates
models that simulate water flows, contaminant transport, and biological
effects across the park’s interconnected water bodies.

With [erahumed](https://github.com/erahumed/erahumed), users can:

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
  [erahumed](https://github.com/erahumed/erahumed).

- The [user manual](https://erahumed.github.io/erahumed-book) provides
  an in-depth description of the underlying models, algorithms, and
  assumptions.

Further background on the ERAHUMED project can be found on the [main
project website](https://www.erahumed.com/).

## Installation

You can install the latest release of
[erahumed](https://github.com/erahumed/erahumed) from GitHub, by running
the following command in R:

``` r
install.packages("remotes")  # If necessary
remotes::install_github("erahumed/erahumed")
```

### Version compatibility

Versions of [erahumed](https://github.com/erahumed/erahumed) follow
standard [semantic versioning](https://semver.org/). If you encounter
issues running code written for an earlier version of {erahumed} with
your current installation, you may consider installing the specific
version the code was developed with.

To install a particular version of
[erahumed](https://github.com/erahumed/erahumed), use:

``` r
remotes::install_github("erahumed/erahumed", ref = "vX.Y.Z")
```

where `"vX.Y.Z"` should be replaced with the desired version number.

## Usage

### Example 1: Using the interactive dashboard

The graphical interface to the ERAHUMED DSS can be accessed through the
following R command:

``` r
erahumed::launch_dss()
```

This will open the DSS dashboard in your default browser, from where you
can explore simulation outputs in a user-friendly manner.

### Example 2: Command-line interface to simulations

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
[`erahumed_simulation()`](https://erahumed.github.io/erahumed/reference/erahumed_simulation.md).
Results can be inspected through:

``` r
get_results(simulation, 
            component = "exposure",  # either "hydrology", "exposure", or "risk"  
            element = "lake"         # either "lake", "ditch", or "cluster"
            ) |>
  head()
#>   element_id       date mf_kg mw_kg ms_kg mw_outflow_kg cw_kg_m3 cs_kg_m3
#> 1       lake 2023-01-01     0     0     0             0       NA        0
#> 2       lake 2023-01-02     0     0     0             0        0        0
#> 3       lake 2023-01-03     0     0     0             0        0        0
#> 4       lake 2023-01-04     0     0     0             0        0        0
#> 5       lake 2023-01-05     0     0     0             0        0        0
#> 6       lake 2023-01-06     0     0     0             0        0        0
#>   cs_g_kg cw_outflow_kg_m3 volume_m3 outflow_m3    chemical
#> 1       0                0        NA     406452 Acetamiprid
#> 2       0                0  27170258     743304 Acetamiprid
#> 3       0                0  27081533     738990 Acetamiprid
#> 4       0                0  26992808     587238 Acetamiprid
#> 5       0                0  26966519     724464 Acetamiprid
#> 6       0                0  26928729     390420 Acetamiprid
```

## Getting help

The full documentation of the
[erahumed](https://github.com/erahumed/erahumed) R package is hosted at
[erahumed.github.io/erahumed](https://erahumed.github.io/erahumed/).

If you have issues running
[erahumed](https://github.com/erahumed/erahumed) or want to suggest an
improvement, please [file an issue on
GitHub](https://github.com/erahumed/erahumed/issues).
