# erahumed (development version)

# erahumed 0.0.4

### Under the hoods

* Ordering of clusters sampled by the local hydrological balance simulation 
algorithm does not depend anymore on the way these clusters are sorted in the 
input data.

* Fixed issues in the logic of local hydrological balance.

* Improved testing infrastructure

# erahumed 0.0.3

### User-visible changes

* Big namespace refactor: all datasets get prepended with the prefix 
`"albufera_"`. 

* The functions `albufera_storage_curve()` and `petp_volume_change()` 
were substituted by the function *factories* `linear_storage_curve()` and 
`linear_petp_surface()`, that create functions that perform affine 
transformations.

* Function `albufera_hydro_balance()` was renamed 
`albufera_hydro_balance_global()`. Ditch inflows are not returned anymore. In 
correspondence, we add a new function `albufera_hydro_balance_local()`, which
at the current moment is still WIP and for testing purposes.

* Functions `albufera_hydro_balance_*()` now allow to specify storage curve and
P-ETP surface by dedicated arguments. The defaults are defined throughout the 
new helpers mentioned in a previous point.

* New function `hydro_balance_global()`, that is meant to capture the abstract
part of the global hydrological balance calculations. 
The function `albufera_hydro_balance()` relies on this for its calculations of
global balance.

* Residence time is now computed in terms of the total outflow, rather than 
inflow as before. This reflects the way we handle cases of unaccounted outflow 
in `hydro_balance_global()` (set inflow to zero).

### Documentation

* Improved documentation of the `hydro_balance_global()` function, as well as
other minor improvements on documentation of helpers.

### Under the hoods

* Started to work consistently on the testing infrastructure.

### Notes

* Function names and the general API of these early versions of the package are 
understood to be provisional.

# erahumed 0.0.2

* Added `clusters` and `cluster_geometries` datasets, containing information at 
the cluster (i.e. aggregate of rice paddies) level.

* Separated computation of ditch inflow percents in a new function, 
`compute_ditch_inflow_pct()`, that leverages on the `clusters` dataset.

* Corrected calculation of total outflow. In this calculation, the Perello daily 
outflow was being set to zero in the case of negative values. This does not make
sense with the current computational approach.

* Updated the `paddy_management` dataset: the irrigation and draining input 
binary vectors used previously were incomplete, with data until the middle of 
August. For this version, the inputs are defined until the 10th of September.

# erahumed 0.0.1

First release. Contains a draft of the utilities for hydrological balance data
analysis, and a skeleton of the visualization app (currently invoked with 
`launch_app()`).
