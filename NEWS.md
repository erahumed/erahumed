# erahumed (development version)

* Big namespace refactor: all datasets get prepended with the prefix 
`"albufera_"`. The function`storage_curve()` becomes `linear_storage_curve()`.

* New function `hydro_balance_global()`, that is meant to capture the abstract
part of the global hydrological balance calculations. The function `albufera_hydro_balance()` relies on this for part of its calculations.

* The arguments of `hydro_balance()` were renamed changing the `_data` suffix
with `_df`.

*N.B.:* function names and the general API of these early versions of the 
package are understood to be provisional.

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
