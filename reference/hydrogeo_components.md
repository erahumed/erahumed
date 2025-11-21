# Landscape hydro-geographical components of the Albufera Natural Park

These helpers return `data.frame`s containing information on the
hydro-geographical components used by ERAHUMED to model the Albufera
Natural Park ecosystem: ditches and rice-field clusters. For a detailed
description of how these spatial units are represented and modelled
within ERAHUMED, see the [user
manual](https://erahumed.github.io/erahumed-book/chapters/birdseye.html#sec-hydrogeo).

## Usage

``` r
info_clusters(include_geometry = FALSE)

info_ditches(include_geometry = FALSE)
```

## Arguments

- include_geometry:

  `TRUE` or `FALSE`. Whether to include the geometries of the various
  elements (as a column of class `sfc_MULTIPOLYGON` from the `{sf}`
  package).

## Value

A `data.frame`.
