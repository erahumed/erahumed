# Extract simulation results

Extract ERAHUMED simulation results from an
[erahumed_simulation](https://erahumed.github.io/erahumed/reference/erahumed_simulation.md)
object.

## Usage

``` r
get_results(
  simulation,
  component = c("hydrology", "exposure", "risk"),
  element = c("lake", "ditch", "cluster")
)
```

## Arguments

- simulation:

  `[`[erahumed_simulation](https://erahumed.github.io/erahumed/reference/erahumed_simulation.md)`]`  
  The simulation object being modified.

- component:

  `[character(1)]`  
  The simulation component to be extracted. Either `"hydrology"`,
  `"exposure"`, or `"risk"`.

- element:

  `[character(1)]`  
  The landscape element for which simulation results are requested.
  Either `"lake"`, `"ditch"`, or `"cluster"`.

## Value

A `data.frame`.
