# Allocate a Management System to a Surface Fraction

Assigns a new management system to a fraction of surface area by
selecting clusters that match given spatial and structural criteria,
using only the subset of eligible clusters defined by `ditches` and
`field_type`.

## Usage

``` r
allocate_surface(
  map,
  system,
  target_fraction,
  ditches = 1:26,
  field_type = c("both", "regular", "tancat")
)
```

## Arguments

- map:

  `[erahumed_rfms_map]`  
  A cluster map created with
  [`new_rfms_map()`](https://erahumed.github.io/erahumed/reference/rfms_map.md)
  or
  [`default_rfms_map()`](https://erahumed.github.io/erahumed/reference/rfms_map.md).

- system:

  `[erahumed_rfms]`  
  A management system to assign, created with
  [`new_rfms()`](https://erahumed.github.io/erahumed/reference/rfms.md)
  or helper functions such as
  [`bomba()`](https://erahumed.github.io/erahumed/reference/rfms.md) or
  [`clearfield()`](https://erahumed.github.io/erahumed/reference/rfms.md).

- target_fraction:

  `[numeric(1)]`  
  The fraction of surface area **within the selected subset** of
  clusters (filtered by `ditches` and `field_type`) to allocate to the
  given system. Must be a number between 0 and 1.

- ditches:

  `[integer]`  
  A vector of ditch IDs (e.g., 1:26) defining the subset of clusters to
  be considered for allocation. Defaults to all ditches.

- field_type:

  `[character(1)]`  
  Type of field to consider when selecting clusters. Must be one of:

  - `"regular"` – only regular fields

  - `"tancat"` – only tancats

  - `"both"` – all field types (default).

## Value

An updated `erahumed_rfms_map` object with the new system allocated to
selected clusters.

## Details

This function allocates a management system to a subset of clusters
selected based on the `ditches` and `field_type` criteria. The
`target_fraction` refers to the fraction of **total surface area within
this eligible subset**, not the entire map.

Clusters are randomly selected from the eligible subset until the
cumulative surface area of selected clusters reaches the desired
fraction.

If the requested fraction exceeds the total available surface in the
subset (e.g., due to prior allocations), the function will issue a
warning and perform a **partial allocation** using all remaining
candidates. The map is returned with the maximum possible surface
allocated under the given constraints.
