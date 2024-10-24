# erahumed (development version)

# erahumed 0.9.0

### Algorithm

* Major improvements to the ODE solution method underlying the CT model 
component. Physico-chemical processes are altogether treated exactly, by solving
the corresponding linear ODE obtained by removing the terms corresponding to 
outflow, chemical applications and solubility (the latter processes are 
calculated in a second stage, with the masses obtained after evolving for a time 
step according to the linear ODE system). Although this is still suboptimal from
the POV that the outflow process is considered as *instantaneous* and 
*simultaneous to application* (see #146), this already gives rise to much more 
realistic dynamics, especially in relation to the diffusion process between 
water and sediment.

### Under the hoods

* Added `ppm_to_kg_m3()` and `ppm_to_g_cm3()` internal helpers.

# erahumed 0.8.1

### Algorithm

* Fixed various errors in the internal data-set providing physico-chemical 
parameters for CT model layer. This is a first effort to address (#123), which
however requires further review.

### New features

* The CT component returns not only masses, but also concentrations (water,
sediment and outflow densities).

* Density plots are made available, in the Shiny interface as well as in the
R API, through the new argument `plot.ct_erahumed(variable)`.

### User visible changes

* The output of the CT model component is now in a "long" format, in which
`chemical` is a column, and masses are reported in the `mf`, `mw` and `ms` 
columns.

### Under the hoods

* "Bare" references to the `$output` element of model components in internal 
code have been substituted, using the `component_output()` extractor instead
(#99).

# erahumed 0.8.0

### Breaking changes

* Nomenclature change: `petp_df` (argument of `compute_inp()` as well as 
"parameter" for the INP component) becomes `weather_df`. Correspondingly, 
the `albufera_petp` dataset is renamed `albufera_weather`. These changes come as
a consequence of #20 (see below.)

### New features

* Data: added temperature daily data (minimum, maximum and average) to the 
`albufera_weather` dataset (formerly known as `albufera_petp`), closing #20.

* Introduced simplified temperature dependence in the algorithm for the CT 
model component (we simply plug in the average daily temperature in the 
Arrhenius equation for the degradation constant).

* HBP: improved plotting method (`plot.erahumed_hbp`), which now provides a 
double plot of cluster water levels (on top) and water flows (on bottom). 

* CA: added a minimalistic Shiny interface to display the results of chemical 
applications in the form of time series for individual clusters.

* CT: added plotting method (see `?plot.erahumed_ct`) and corresponding Shiny 
interface.


# erahumed 0.7.3

### Algorithm changes

* CT: the multiplier for the sediment to water diffusion term was thresholded to
one, as this number should always be a positive fraction of unity. In any case, 
multipliers larger than one are probably being created by issues in the 
pesticide parameters (*e.g.* unit errors) that are currently being addressed, so
that the threshold should become unnecessary in the long run.

### Breaking changes

* Renamed arguments of `compute_ct()` by making units explicit (*e.g.* 
`css` becomes `css_ppm`).

### Bug fixes

* Fixed inhomogeneous term in the evolution equation for `ms[t]` (due to a typo
ms was wrongly getting added the mass applied on foliage).

### Under the hoods

* Re-enabled tests that were momentarily disabled after the refactor of v0.6.0.
These consisted of a few tests on the argument checking performed by 
`compute_hbp()`, plus a snapshot test (skipped on CI) on the initial Shiny 
values.

* Various simplifications in the low-level code of the CT model components.

* Basic unit tests for the CT component functions.

# erahumed 0.7.2

### New features

* CT model component now uses actual cluster and chemical parameters in the 
evolution equations (whereas it was previously using dummy values of the 
expected order of magnitude).

# erahumed 0.7.1

### Bug fixes

* Fixes a regression introduced in v0.7.0 that introduced an error in the
computation of rain and evaporation water volumes (see #115 and #116).

# erahumed 0.7.0

### New features

* Experimental release of CT model component, with new functions `compute_ct()` 
and `ct()` (close #92).

### Breaking changes

* Removed helpers `linear_petp_surface()` and `linear_storage_curve()`. These
were substituted by simple lambdas in the specification of defaults arguments of 
`compute_hba()`, which seems to be a bit more transparent (#96).

* `compute_hba()` argument name change: `petp_surface` becomes `petp_function`.

### Improvements

* Set amounts (kg_per_ha) in albufera_ca_schedules builtin dataset (close #84).

# erahumed 0.6.0

### General info

This version provides an important refactor of the software's modeling 
interface, consolidating the concept of ERAHUMED model and model components.

The refactor necessarily breaks existing code that relied on the initial 
provisional interface. A basic example illustrating how to use the new modeling 
interface is provided by the README update that accompanies the new version.

### Breaking changes

* Essential refactor of modeling API, as mentioned above. 
* The HBP model component (formerly known as "local hydrological balance") does 
not allow anymore the specification of `date_min/max` arguments; the date range 
is univocally determined by the data fed to the INP model component.
* Removed bold from printing methods, which was not correctly rendering in 
Github flavored markdown.
* Renamed the HBP admissible plot types (value of `type` 
parameter to `plot.erahumed_hbp()`), from `"cluster_levels"` and `"map"` 
to `"cluster_view"` and `"map_view"`.
* Removed `height_thresh_cm` argument from arguments of CA component high level
functions (`compute_ca()` in the present version), to move it down to the  
corresponding functions of the HBP component. This ensures, in particular, that
a common value is used across the two different model components.

# erahumed 0.5.1

### Documentation

* Emails and ORCID of additional package authors were added to the package metadata. 

### Bug fixes

* Fix bug that was causing errors in the plots of Perellonet, Perelló and Pujol outflows in the Shiny interface (#90).

### Other

* The package has a new website, available at https://erahumed.github.io/erahumed/.

# erahumed 0.5.0

### Breaking changes

* Names of columns representing outflows in `albufera_outflows` have been prepended with `outflow_`.

* Logic of `albufera_hb_global()` has been improved in such a way that the 
outflow columns are automatically recognized from the input data.frame, and do 
not need to be exactly `"pujol"`, `"perellonet"` and ``"perello"`, as before.

* Former `irrigation` and `draining` columns from the output data.frame of 
`albufera_hb_local()` became `ideal_irrigation` and `ideal_draining`, to better
reflect that these correspond to the originally scheduled irrigation and
draining plans. In addition, new columns `real_irrigation` and `real_draining`
were added to the data.frame, which provide the actual irrigation and draining
plans followed, modified by the plan delay.

### New features

* Visualizations of the "extra outflow" variable have been added to the global 
hydrological balance module of the Shiny dashboard.

### Bug fixes

* Fixed broken functionality of 'height_thresh_cm' argument (#85).
* Correct bug that was causing the wrong delay being applied to `seed_day` in 
the `ca_choose_application_day_index()` helper, building block of `ca()`.

### Under the hoods

* Unit testing of CA model block (#77).

# erahumed 0.4.1

### New features

* First draft of `plot.erahumed_ca()` ("cluster view").

### User visible changes

* The default for the `height_thresh_cm` parameter of `ca()` has been changed 
from `2.5` to `2`.

* The `albufera_management` data-set has a new column `seed_day`, which counts
the number of day elapsed from the sowing day. This is also returned in the 
output data.frame of `albufera_hb_local()`.

* `erahumed_ca` S3 class now inherits from `hb_local`.

### Under the hoods

* Improvement in data.frame validity checks. The `assert_data.frame()` helper 
is now able to check column types. Noteworthy, this allows for much stricter 
tests in the `"hb_local"` and `"hb_global"` functions.

* Important performance improvements in the function `ca()`. These were mainly 
achieved by (i) removing validity checks in the low level function 
`ca_to_cluster()`, supplanted by less costly checks at the level of the 
`hb_local()` output; and (ii) bypassing computation using `Date` objects, thanks
to the introduced `seed_day` column in the output of `albufera_hb_local()` (see
above).

### Documentation

* Improved documentation of `albufera_hb_global()` default parameters.

# erahumed 0.4.0

### New features

This version introduces a first draft for the new Chemicals Applications (CA) 
model block, which is meant to simulate the application of chemicals 
(fertilizers and pesticides), according to the previously obtained simulation of 
local water levels and water flows, which is the output of the hydrological 
balance (HB) block.

The basic `ca()` function is provided, which takes as input an object of S3 
class `"hb_local"` (plus a few other optional input parameters), and returns a 
similar data-frame, with additional columns that provide the time series of 
chemical applications.

The accompanying dataset `albufera_ca_schedules` provides the schedules for 
chemical applications in the Albufera lake, and is one of the customizable 
inputs to `ca()`.

# erahumed 0.3.2

### Under the hoods

* Shiny app continues to work if the Albufera Leaflet map cannot be loaded for
any reason.

* Added validation of date range to `albufera_hb_local()`.

* Tested exceptions raised by `albufera_hb_local()`s argument validity checks.

* Documented plotting method for `hb_local` and `hb_global` S3 objects.

### Miscellaneous

* Added Pablo Amador and Andreu Rico to package Authors@R.

# erahumed 0.3.1

### User visible changes

* `albufera_hb_local()`, called with default arguments, now retrieves its value
from disk, from data installed alongside with `{erahumed}`.

* Due to the previous point, the package's .tar.gz file size is much larger now,
approximately 50MB. The file size will be tackled, as much as possible, in later
releases.

* Added light `print()` and `summary()` methods for the S3 objects returned by
`albufera_hb_local()` and `albufera_hb_global()`.

* `{sf}` is no longer attached upon attaching `{erahumed}`.

### Bug fixes

* Package data is prefixed everywhere in the code by the `erahumed::` NAMESPACE,
so that executing this code without having the package attached does not lead to
errors. In particular, calling `erahumed::launch_app()` without attaching 
`{erahumed}` does no longer lead to an error. 

### Under the hoods

* Fixed R-CMD-check spurious NOTEs.

# erahumed 0.3.0

### User visible changes

* New Shiny module to set parameters of the hydrological balance calculations.
* New Shiny module to modify input data to the algorithms.

### Under the hoods

* Added shinytest2 testing infrastructure, for snapshot based tests to be 
developed later.
* Basic tests for plotting utilities (succeed with default arguments).
* Server testing of `hb_shiny.R` modules, #15.

# erahumed 0.2.2

### User visible changes

* Documentation for the `albufera_hb_global()` and `albufera_hb_local()` 
wrappers.

* Exported `hbg_residence_time()` doc page.

* Former `albufera_weather` dataset becomes `albufera_petp`, and its structure is significantly simplified, now having only three columns: `date`, `rain_mm` and `evapotranspiration_mm` (previously named `date`, `P` and `ETP` respectively). All changes get propagated to downstream tables, such as *e.g.* `albufera_hb_global()`.

### Algorithm change

* In previous versions of the algorithm, the plan delays upon failing to reach
zero water levels on a given date were only being applied to the 
`ideal_height_cm` vector. In the present version, the draining and irrigation
plan also get delayed. This has been observed to give rise to slightly more 
natural water profiles (in particular, the delay accumulated during summer
is often less than 40 days, contrary to what used to happen with the previous
version).

### Under the hoods

* Add `"hb_local"` S3 class internal constructor (#21).

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
`linear_petp_function()`, that create functions that perform affine 
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
