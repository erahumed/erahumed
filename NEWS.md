# erahumed 0.2.1

### User visible changes

* Default arguments of `albufera_hb_global()` and `albufera_hb_local()` are now
prepended with the `erahumed::` namespace specifier.

* Unexported `residence_time()`

### Under the hoods

* Separated logical units of `hb_global()`, collected in `hb_global_units.R`.

* Introduced argument checking for `hb_global()` and its wrapper 
`albufera_hb_global()`.

* Introduced S3 class constructor for `"hb_global"` class objects (see 
`hb_global_units_s3_class.R`).

* Important improvements in the testing infrastructure of `hb_global()` and  
`albufera_hb_global()`, both at the algorithmic level (through testing of 
`hb_global_units.R`) and at the level of exceptions raised by these two 
functions.

# erahumed 0.2.0

### User visible changes

* Reduced number of `*_is_imputed` entries in `hb_global` dataframes. Right now 
we are only providing `level_is_imputed` and `outflow_is_imputed`, which should
suffice FAPP.

* Big refactoring of the HB sector naming conventions. The wrappers become `albufera_hb_global/local()`, and the underlying functions are 
`hb_global/local()`.

### Under the hoods

* We complete the work started with v0.1.2 to separate the algorithms for 
computing global and local hydrological balance, from the wrappers used to feed
this algorithms with our prepared datasets. In this version, the wrappers got 
significantly simplified, their only function being extracting variables from 
the imported datasets and feeding them to the algorithms (as it should be).

# erahumed 0.1.2

### User visible changes

* Improved appearance of cluster level plots in Shiny app UI.

### Under the hoods

* Important code refactors for the local balance algorithm, aiming to separate 
the abstract logic of the algorithm from our specific conventions used to pack together data for hydrological balance - the latter conventions being relegated
to `hb_wrappers.R` functions. Fixes #3.

* Extensive unit testing of the logical units for the local balance algorithm.

* Performance optimizations in local balance algorithm.

* Sanity check functional tests for `plot.hb_local()` and `plot.hb_global()`.


# erahumed 0.1.1

### Algorithm

* The main algorithm for local balance has been modified in such a way to ensure
that the correct number of emptying events is always produced. This is achieved
by delaying (by one day) the ideal water level plan whenever the ideal level is
zero and the real level is above a certain threshold (2.5 mm by default). 
Moreover, outside of the sowing season, the water level plan is reset to
the original one, in such a way that the Perellona does not get delayed.

### Under the hoods

* Re-enabled continuous integration, with some internal code improvements.
* Sanitizing of warnings/notes from R-cmd-check.

# erahumed 0.1.0

### Algorithm

* Switched to an improved version of the algorithm to compute the local balance
at the cluster level. The exactness of total outflows is enforced, in such a way 
that the sum of outflows of cluster that belong to a given ditch equals the 
total inflow to the Albufera lake from said ditch. Also, the logic of the 
algorithm was changed in such a way to enforce that no evaporation can occur 
when the water level of a cluster is zero. **Warning.** While correctly 
enforcing mass conservation, the present algorithm does not guarantee that the
clusters will always be emptied for periods as long as planned, for the 
application of pesticides - both aspects being at odds with the previous 
version. Correctly enforcing the emptying of clusters will is the object of 
ongoing work.

### UI

* The cluster to be plotted in the local balance tab of the shiny app can now be
selected from an interactive leaflet map of the Albufera National Park.
 
### Under the hoods

* Added internal utility `erahumed:::plot_albufera_clusters()` to obtain a 
leaflet map of clusters.

* Added internal utility `erahumed:::plot_albufera_management()` to obtain a 
plot of ideal water levels vs. day of year.


# erahumed 0.0.6

### Under the hoods

* Fixed an important bug that was causing real outflows to be zero in most 
cases.

### Testing

* Added important functional tests for real outflows. In particular, we test 
that the sum of real outflows for clusters belonging to a given ditch is always
equal to the minimum between the total ditch's capacity and ideal required 
outflow.

# erahumed 0.0.5

* The outputs of `albufera_hydro_balance_global()` and 
`albufera_hydro_balance_local()` get class attributes `"hb_global"` and
`"hb_local"`, respectively.

* Added cluster level plots to the shiny UI.

### Under the hoods

* New plot methods for `"hb_global"` and `"hb_local"` S3 classes. Currently
not exported to NAMESPACE.


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
