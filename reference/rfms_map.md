# Cluster-Based Assignment of Rice-Field Management Systems

These functions define how different rice-field management systems
(RFMSs) are assigned to spatial clusters within the simulation. Each
cluster can be associated with a specific RFMS, allowing for
heterogeneous management across the simulation domain.

## Usage

``` r
default_rfms_map(seed = 840)

new_rfms_map(default_rfms = new_rfms())
```

## Arguments

- seed:

  `[numeric(1)]`  
  Seed for random number generation in the assignment of clusters to
  management systems.

- default_rfms:

  `[`[erahumed_rfms](https://erahumed.github.io/erahumed/reference/rfms.md)`]`  
  A default management system assigned to all clusters initially.
  Typically created with
  [`new_rfms()`](https://erahumed.github.io/erahumed/reference/rfms.md)
  or a helper like
  [`jsendra()`](https://erahumed.github.io/erahumed/reference/rfms.md).

## Value

An object of class `erahumed_rfms_map`.

## Details

- `new_rfms_map()` initializes a cluster map with all clusters assigned
  the same default management system.

- `default_rfms_map()` provides a predefined map inspired by current
  practices in the Albufera Natural Park. Specifically, it uses the *J.
  Sendra* system as the default, and allocates small proportions of
  *Bomba* (10% in tancats) and *Clearfield* (10% in ditches 1–19). This
  map is a convenient starting point for scenario simulation or
  customization.

Cluster assignments can be modified using
[`allocate_surface()`](https://erahumed.github.io/erahumed/reference/allocate_surface.md).

For a detailed explanation of RFMS concepts, configuration, and spatial
allocation, see the [RFMS section of the user
manual](https://erahumed.github.io/erahumed-book/chapters/rfms.html).

## See also

[`new_rfms()`](https://erahumed.github.io/erahumed/reference/rfms.md),
[`allocate_surface()`](https://erahumed.github.io/erahumed/reference/allocate_surface.md)
