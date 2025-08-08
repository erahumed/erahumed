# erahumed (development version)

# erahumed 0.20.1

### API changes

* Changed order in `erahumed_simulation()` arguments, matching the one used in 
documentation tables.

* `new_management_system()` is replaced by `new_rfms()`.

* `new_cluster_map()` is replaced by `new_rfms_map()`. The corresponding 
underlying S3 class is renamed to `erahumed_rfms_map`.

### Documentation

* Improved documentation of `rfms_map` argument of `erahumed_simulation()`.
* The [simulation inputs vignette](https://erahumed.github.io/erahumed/articles/simulation-inputs.html)
now also lists chemical parameters, RFMS parameters and parameters for surface
allocation.

### Under the hoods

* Improvements in internal mechanisms for programmatically generating 
documentation entries for model parameters.

# erahumed 0.20.0

This is a minor release providing several improvements and bug fixes over the 
previous one, plus a few minor algorithm changes. This prepares the publication 
of the first public release v1.0.0.

### Algorithm changes

* We now differentiate between the fraction of organic content of suspended 
solid, on one hand, and sediment on the other (#453). This is reflected in the
`foc_sed` and `foc_ss` arguments of `erahumed_simulation()`, that replace the 
former `foc` argument.
* Added a "risk quotient" method (alongside the already implemented PAF) as 
available risk metric (#413).
* The day for ground chemical applications becomes the last in the emptying 
window (#412). This was previously the *first* day in the emptying window.
* The Albufera lake's depth is now reported correctly (as Volume / Surface) 
(#439). The previous value was a placeholder that coincided with the level 
(with respect to reference) provided in CHJ data.
* Various improvements and clean-up in the tabular outputs for hydrology, 
exposure and risk, concerning both the R interface and the results downloadable 
from the GUI (including: #450, #435).
* The period in which plan delays for water management of rice fields are 
applied now extends from the beginning of the sowing season to the beginning of
the Perellona, whose concrete dates may differ among different rice-field 
management system. Previously, this window was fixed to the days between April 
20 and October 15, for all management system. For the predefined existing 
systems (J.Sendra, Clearfield, and Bomba) the change has no practical effect.
* The `dinc_m` and `drift` parameters are removed and fixed to their default 
values (#442, #425).
* The volatilization coefficient is no longer a defining property of chemicals 
(#451). Previously, this was a required input, although this numerical value was 
never used in the actual simulation, as volatilization is not currently being
taken into account.

### Data changes

* The default distribution of rice-field management systems is now as follows:
in the North (ditches 1 to 9) 80% of surface is allocated to Clearfield, and
20% to J.Sendra; in the South (ditches 10 to 26) 80% of surface is allocated to 
J.Sendra, and 20% to Clearfield.

* Reviewed numerical properties of predefined chemicals.

### API changes

* Improved printing method for objects of S3 class `erahumed_simulation` (#411).
* `allocate_surface()`: the `target_fraction` argument now refers to the 
fraction of surface of the *selected region*, whereas it was previously 
referred to the whole Albufera Natural Park (#421).
* `erahumed_simulation()` accepts a new `.progress` argument, a function that 
allows the user to customize how simulation progress is reported.

### GUI changes

* The app is now frozen while downloading the results zip archive (#303).
* The Output tab now shows a "loading" spinner while the simulation is running.
Simulation progress is reported in a notification modal (#432).
* Restored chemical selectors for exposure plots (#428).
* Increased the size of Albufera auxiliary widget, in order to make it more 
comfortable to read (#365).
* Risk plot is now a regular time-series plot (#445), with risks due to 
individual chemicals plotted as independent time-series, alongside the 
"combined effect" time-series. Previously, risks due to individual chemicals 
were displayed as stacked areas.
* Results can now be download also in XLSX (Excel) format (#447).
* The results .zip archive now contains a README file (#437) that provides 
basic information on the contents of the archive.
* The GUI for allocating rice-field management systems now has improved 
parameters descriptions (#423).
* Removed the "reset" button from the main input GUI. A global reset that also
included restoring the initial state of the chemicals and management system
databases is too complex too implement at the current stage.

### Bug fixes

* Chemical database is now working normally: chemicals can be edited multiple
time without any error being triggered (#434).
* Degradation constants of chemicals can now be set to be exactly zero without
breaking the functioning of the algorithm (#433).
* Fixed broken selector of chemicals to be displayed in the exposure plots 
(#455).

### Documentation

* New 
[`hydrogeographical-model vignette`](https://erahumed.github.io/erahumed/articles/hydrogeographical-model.html) 
replacing the older `hydrology-scheme` vignette (#239).
* Various improvements and corrections in parameter and data descriptions
(including: #441, #443, #446, #448, #438).

### Testing

* Minor improvements (including: #452). 

# erahumed 0.19.0

This is a major release that introduces several powerful features to the  
package, including:

* The ability to define new chemical compounds with custom physico-chemical  
  and toxicological properties.

* The ability to define new rice field management systems (beyond the  
  originally implemented *J. Sendra*, *Clearfield*, and *Bomba*), featuring  
  customizable crop calendar dates (start/end of sowing season and *Perellona*)  
  and user-defined pesticide application schemes.

* The ability to define a custom spatial distribution of  
  (built-in and user-defined) rice field management systems across rice field  
  clusters.

Detailed changes are discussed above.

## API additions

* New chemical compounds are defined through the `chemical()` helper. The  
  functions `acetamiprid()`, `azoxystrobin()`, `bentazone()`, `cycloxydim()`,  
  `cyhalofop_butyl()`, `difenoconazole()`, `mcpa()`, and `penoxsulam()` expose  
  the presets used by the `{erahumed}` built-in management systems.

* Rice field management systems are defined through `new_rfms()`  
  and `schedule_application()`. The *J. Sendra*, *Clearfield*, and *Bomba* systems  
  are implemented using the helpers `jsendra()`, `clearfield()`, and `bomba()`.

* The spatial distribution of management systems is defined through  
  `new_rfms_map()` and `allocate_surface()`. `default_rfms_map()` provides a  
  predefined map inspired by current practices in the Albufera Natural Park.

## API changes

* `erahumed_simulation()` no longer accepts the `variety_prop`,  
  `management_df`, and `ca_schedules_df` arguments. These have been replaced  
  by a new `rfms_map` argument, through which users provide all relevant  
  information regarding custom chemicals, management schemes, and their  
  spatial distribution.

* The `albufera_management` and `albufera_ca_schedules` datasets are no longer  
  exported.

## GUI changes

* A graphical interface for the new features has been developed and is accessible  
  through the "Agrochemical management" section of the Inputs.

# erahumed 0.18.1

### Documentation

* Improved the documentation entry of `?launch_dss`

### Testing

* The testing infrastructure of the package has been thoroughly reviewed and 
improved.

# erahumed 0.18.0

### Algorithm

* At harvesting time, the mass accumulated in the foliage compartment of 
clusters is now set to zero (#374). Correspondingly, the `albufera_management` 
built-in data-set now comes with an extra `harvesting` column (`TRUE` or 
`FALSE`).

* The algorithm for computing of application dates was significantly simplified. 
These dates are now obtained by equating the scheduled application day with an 
"irrigation plan advancement" defined as the `seed_day` minus the `plan_delay`. 
The applications days in the `albufera_ca_schedules` data-set have also been 
fixed in order to be consistent with the emptyings scheduled in the ideal 
management plan of the `albufera_management` built-in data-set. These two 
changes have no actual effect on the current results with default inputs, which 
are the same as before, but makes future generalizations (*e.g.* supporting the 
definition of custom chemicals) much easier.

* The default values for the storage curve have been reverted to the ones 
reported by CHJ, in agreement with what was actually being documented.

### API changes

* The outputs for the risk component of simulations (obtained through 
`get_results(component = "risk")`) now list PAFs both for individual chemicals
and combined toxic modes of action (#380).

* In the outputs for the risk component of simulations (obtained through 
`get_results(component = "risk")`), hazard units and SSDs standard deviations 
are no longer reported (#379).

* Variable names for `cluster_id` and `ditch` to have been renamed to 
`element_id` (#297).

* End-of-day and Start-of-day volume variables are no longer reported in CT 
output, replaced by the single "volume" variable - that is the Start-of-day one
(#377). The semantics of output columns will be clarified in the user manual.

### GUI changes

* The input tab now has a "Reset" button (#367).

* Reorganized grouping of inputs (#384, #385, #386, #387).

* Column descriptions in data-set inputs re now open by default (#382).

* Downloaded input data-sets now have self-explanatory names (#368).

* Add water body (*e.g.* grouping ditch for clusters) info in GUI (#370).

* Improved labels of hydrology inputs (#383).

* Risk plots: the breakdown is now done by chemical rather than toxic mode of 
action (#373).

* Exposure plots: density for zero volume is now set to zero (#370).

* Risk output: the only plot tab currently present has been renamed to 
"Potentially Affected Fraction" (#376).

* Label of risk plots has been changed to "Potentially affected species [%]" 
(#371).

* Label of exposure plots changed from "Density" to "Concentration" (#378).

### Documentation

* Various improvements to inputs' documentation.

### Bug fixes

* Fixed regression reintroducing the bug of #308.

* Fixed GUI crash upon trying to change date range (#361).

* Fixed implementation of #326.

# erahumed 0.17.1

### Documentation

* Updated README.

# erahumed 0.17.0

This version serves as a release candidate for v1.0.0. It presents a thorough 
simplification of the public API of the package, as well as several improvements 
on the GUI side. Specific changes are discussed below.

### Algorithm changes

* The calculation of soil application rate, which was previously (wrongly) 
reported as always zero, has been corrected (#341).

* Exchanged `wilting` and `fc` input parameters for the single `porosity`.

* The `outflows_df` data-frame no longer requires `*_is_imputed` columns (#348).

### API changes

* The simulation workflow has been considerably simplified. Full details on the
current API are available at [the main package vignette.](https://erahumed.github.io/erahumed/articles/erahumed-workflow.html)
In brief, setting up a simulation and running it (entirely) is now performed 
with the single command `erahumed_simulation()`, whose arguments are simulation
parameters.

* The `erahumed_simulation()` function has additional `date_start` and 
`date_end` parameters.

* The utility function `erahumed_input_docs()` is no longer exported (#357).

### GUI changes

* The Input tab has been completely restructured (see also #233).

* Fixed coloring of various plots (#330).

* Outputs: all chemicals are shown by default in the exposure plots.

### Bug fixes

* Fixed `get_results()` function which was not returning exposure results.

* Fixed reading of `cluster_variety_map`.

### Documentation

* Change pipeline-scheme vignette for simulation-inputs vignette (close #305).

* Improved various documentation entries for several parameters.

### Dependencies

* New soft (Suggests) dependencies: `{gt}` and `{kableExtra}`.

# erahumed 0.16.2

This version provides an important reshaping of the "Output"" section of the 
GUI. Plot methods have been harmonized across the various hydrological elements,
and the interface has been structured in such a way to show at any time the 
three simulation component (Hydrology, Exposure and Risk) results for an 
individual element (either a cluster, a ditch or the Albufera lake).

Specific changes are discussed below.

### Algorithm

* `setup_hydrology()` no longer accepts functional inputs for former the 
storage curve and PETP-function. Arguments `storage_curve` and `petp_function`,
which used to require function inputs, are now replaced by 
`storage_curve_slope_m2`, `storage_curve_intercept_m3` and `petp_surface_m2`.

* In all risk calculations, pesticide densities are now regarded to be zero 
whenever the water volume of an hydrological element is zero (concretely, this 
only happens for rice cluster at present). The former treatment regarded them as
`NA`. Notice that this affects, in particular, the definition of chronic risk.

### Visualization

* New implementations for hydrology plots, separating "flow" and "storage" 
plots, and volume *vs.* depth plots (this latter chosen through a radio button
in the GUI).

* We start using `PAF / sum(PAF)` to provide a heuristic for relative 
contributions of individual toxic modes of action to the total `msPAF`.

* Various improvements in plot axes and legends.

* Pesticide names have been corrected (NB: this change is immediately noticed 
and mostly relevant for visualizations, but it actually also affects the 
simulation outputs in R).

* Chemical density time-series for sediment and water are now separated. No 
plotting option is provided for foliage.

* Chemical densities in sediment are now reported in `mg / g` units. 

### GUI

* New `'Download Results'` button (#244).

* Former "cluster" picker input becomes a "water body" element picker.

* Card-specific settings for the "Hydrology", "Exposure" and "Risk" cards of
the output tab are now placed inside a popover triggered from a gear icon-like
button (#321).

* Plots were resized to a fixed `600px` size.

* New (much clearer) naming scheme of clusters and ditches.

### Under the hoods

* The list of chemicals available for selection in the GUI is no longer 
retrieved from simulation outputs, being now simply given by 
`names(info_chemicals())`. The changes needed for the case of defining custom
chemicals will be addressed elsewhere.

# erahumed 0.16.1

### GUI

* The X ranges of time series plots in the Output section are now synchronized
(#231).

* Parameter updates trigger a notification that prompts the user to re-run the
simulation (#301).

* Date range input is now a `shinyWidgets::airDatepickerInput()` (#269).

* Run button functionality restored (#309).

* A new `selectInput()` allows to choose the type of risk to be plotted 
(acute *vs.* chronic).

* A new radio button allows to choose whether to show mass or density in 
exposure plots.

* Card headers in the Output section are now colored in black.

### Dependencies

* We start importing `{shinyWidgets}`.

# erahumed 0.16.0

This version provides the first implementation of risk assessment through SSDs.

### New features

* `get_results(component = "risk")` can now be used to retrieve simulation 
outputs for risk.

### GUI

* New risk plots available in the Output tab of the app.

### Under the hoods

* `cluster_id` and `ditch` columns of the respective `ctc` and `ctd` layers have
been renamed to `element_id`.

# erahumed 0.15.1

This version implements the simulation of pesticide concentration dynamics in 
ditches and in the Albufera lake. 

### Algorithm changes

* Corrected volume calculations in the internal algorithm for computing chemical 
concentration time series - the new implementation is considerably simpler and 
entirely relies on the previous calculations performed in the "hydrology" 
layers. 

### New features

* `get_results(component = "exposure")`, with `element = "ditch"` or 
`element = "lake"` can now be used to retrieve simulation outputs for exposure.

### GUI

* Pesticide to be plotted in exposure plots in the "Output" tab can now be 
selected through a selectInput UI element (#293).

### Under the hoods

* Removed `compute_layer()` helper, no longer used.

* Plot methods for CT layers now have a "chemicals" argument, that allows to 
choose what chemicals to plot.

* Code refactor of chemical transport helpers (#288, #289, #290, #291).

### Testing

* Tests are stopped early if the creation of mock simulation objects fails.

* Various improvements in the testing infrastructure.

# erahumed 0.15.0

This release comes with a fresh new (simplified) API that drastically reduces 
the number of functions exported by the package. The old concept of 
"simulation layer" is no longer exposed to the end user, which only has to deal 
with much more intuitive concepts such as: 

* Simulation components: *hydrology*, *exposure* and *risk*.
* Landscape elements: *lake*, *ditch*, and *cluster*.

The usage of the new API is exemplified in the updated
[main package vignette](https://erahumed.github.io/erahumed/articles/erahumed-workflow.html).
Below are listed specific changes:

### API changes

* Removed `setup_inp()`, `setup_hba()`, `setup_hbp()`, `setup_hbd()`, 
`setup_ca()`, `setup_ct()` functions, in favor of `setup_hydrology()`, 
`setup_exposure()` and `setup_risk()` (#272).

* Removed `erahumed_layers()` helper.

* `run_simulation()` loses its `layer` argument: simulation can only be run 
all at once (#279).

* Removed `get_layer_output()` helper in favor of `get_results()`.

* Removed `get_layer_parameters()`, `get_layer_aux()` and `get_layer()` from 
public API, with no function replacing their functionality (considered 
unnecessary at the moment).

* Removed old layers' `plot()` S3 methods.

### Data

* Updated observational input data `albufera_outflows` and `albufera_weather` 
with data until 2024 (included).

### Under the hoods

* Defined a single helper to compute the time series of pesticides in clusters,
ditches and in the lake, the latter two functionalities still to be implemented
(#275).

# erahumed 0.14.3

### GUI

* The tab initially selected upon app initialization is now the "Output" tab 
(#169).
* Added a "Run simulation" button that allows to update the outputs when 
parameters are modified (first step towards addressing #194).

# erahumed 0.14.2

### Modeling advances

* New simulation layer HBD ("Hydrological Balance of Ditches"), with 
corresponding setup function `setup_hbd()` and `plot.erahumed_hbd()` method. 
The results from the layer are also included in the Shiny GUI.

### Documentation

* Unified description and title of layers across Shiny app and R documentation. 

# erahumed 0.14.1

### API changes

* Changed `height_thresh_cm` default value to '0.5' (#242)

### Documentation

* Add `ct-diagram`: "Diagram of physical processes for chemical transport".

### Dependencies

* Dropped `{shinydashboard}` Imports dependency.

### Under the hoods

* Removed old DSS-v0 app and related server functions

### Testing

* Updated server tests for `<layer>_input_server()` servers

# erahumed 0.14.0

### New features

* New screenshot feature in Shiny app (#230).
* New plot type for CT simulation outputs accessible via 
`plot.erahumed_ct(type = "max_boxplot")`, which provides a visualization of the
distribution of maximum pesticide concentration across clusters (#212). 

### API improvements

* Stricter argument validation for `management_df` of `setup_hbp()` (#216).

### GUI Improvements

* UI elements that take long to compute now show a spinner (#227).
* Restored selection of cluster from the Albufera interactive map (#229).
* Clusters in the Albufera interactive map are now color-coded by rice variety.
* Added informative 'disconnected' screen using `{sever}` (#179).
* Albufera map UI element is hidden on app startup (#226).

### Documentation

* Added informative tooltips for all model parameters in the Shiny app (#223).
* Added column descriptions in the CSV input UI (#224).
* New "About" modal in Shiny app (#171).
* Add new vignette with the scheme of the model's hydrology.
* Removed "experimental" badge from `?ct` documentation (#192).
* Corrected typo in `launch_dss()` command in README (#241).
* Improved description of `ca_schedules_df` parameter in `?ca`.

### Under the hoods

* Reduced code redundancy in `setup_*()` functions by capturing arguments of 
function call and passing down directly to underlying `setup_layer()` (#179).
* Moved argument validation of `setup_*()` functions to main body (close #205).

# erahumed 0.13.0

This release comes with a whole new Shiny GUI, modeled after the ideas 
discussed in #214. The GUI is still largely work in progress, but the various
required improvements will be diluted in following releases to better organize 
work. The new app moves away from `{shinydashboard}`, instead using the layouts 
provided by the active `{bslib}`, addressing #211.

Specific changes are discussed below.

### New features

* New Shiny app providing the GUI modeled after #214. This can be accessed via
`launch_dss()`.

### Breaking changes

* `launch_app()` was replaced by `erahumed:::launch_dss_v0()`, which gives 
access to the old GUI (this is a provisional measure which is not guaranteed to
be maintained in future).

### Other API changes

* `plot()` methods now always succeed with their default argument. Methods that
require the specification of a cluster now choose the first one available in the 
output `data.frame`, whenever the `cluster_id` argument is missing.
 
# erahumed 0.12.5

### New features

* New function `erahumed_input_docs()` to interact with the ERAHUMED documentation as 
an R object. Although this is part of the exported namespace, it is mainly added
for future internal use.

### GUI

* Data input modules are now able to handle Excel as well as CSV input and 
output (#215).

* Improved error messages of data input modules when the uploaded file does not
have the correct column structure.

### Documentation

* Added tooltips with parameters descriptions to the 
[pipeline-scheme vignette](https://erahumed.github.io/erahumed/articles/pipeline-scheme.html).
 

### Under the hoods

* Shiny modules associated with the various model layers no longer receive 
`erahumed_simulation` objects, but rather directly operate with `erahumed_layer` 
objects. This makes code logic more transparent, both for the modules themselves
as well as for the main application.

* The documentation of layer parameters is now generated programmatically and
can be read at run-time through the new helper `erahumed_input_docs()`.

# erahumed 0.12.4

### Visualization

* Switched to the `{dygraphs}` plotting library for all built-in time-series 
plots, in particular those featured in the Shiny app (#181).

### GUI

* CSV file upload now triggers informative errors when the uploaded file does 
not have the expected format (#208).

# erahumed 0.12.3

### Visualization

* Graphical improvements to the Albufera map visualization (#104).

### New features

* New function `info_ditches()` that returns information on the ditches of the
Albufera Natural Park considered in modeling.

### Documentation

* Added website badge and other minor improvements to README. (#204)

* Added links to the 
[`pipeline-scheme` vignette](https://erahumed.github.io/erahumed/articles/pipeline-scheme.html)
in various places of the documentation (#193).

### Dependencies

* Dropped dependency from `{randomcoloR}` after the fixes of #104.

# erahumed 0.12.2

### API changes

* The `'seed'` parameter moves from `setup_hbp()` to `setup_inp()`.

* Former informative function `clusters()` was renamed `info_clusters()` #189.

* Former informative functions `chemicals()` and `chemical_properties()`  
were replaced by a single function `info_chemicals()`, that returns a named
list (#189 and #190).

### GUI changes

* The Albufera map in the UI is again showing the (randomly generated9 cluster 
variety (#191).

* The "Setup" panel of the INP layer gets three new numeric inputs to setup 
`variety_prop` (#199).

* The "Setup" panel of the HBA layer gets four new numeric inputs to setup 
the `storage_curve` and `petp_function` parameters (#203).

### Under the hoods

* Important simplifications in the logic of layer computation functions. Former 
`compute_*_bare()` functions no longer exist, in favor of new `compute_*()` 
functions returning simulation-type objects. Function lookup by name (formerly
performed by internal helper `get_erahumed_fun()`) is now avoided, in favor of
more transparent and simpler R code in `compute_layer() `that employs a switch 
statement.

* Refactor of layer objects internal structure: now layer S3 objects have an 
`aux` element that stores intermediate results of layer computations.

* Cluster varieties are now stored in the `aux` list of the INP layer.

* `plot_albufera_clusters()` gets a new `cluster_variety_map` argument.

* `test_sim_small()` and `test_sim_large()` helpers get new arguments `seed` 
(seed for simulation) and `force` (whether to force recomputation of object).

# erahumed 0.12.1

### GUI

The app has a brand new GUI based on `{shinydashboard}`. The refactor is 
partially based on work from PR #198, which was proposing a tentative layout
with a "floating" map, in an attempt to address #183 and #195. The final 
version closes issues #183, #195 and #197.

### Documentation

* New vignette showing the simulation pipeline as DAG (#182).

# erahumed 0.12.0

### Algorithms changes

* The variety of clusters is no longer fixed data, but is rather generated 
randomly in each simulation, with a fixed proportion of the Albufera's total 
surface allocated to Bomba/Sendra/Clearfield varieties. The relevant parameter
is the new `variety_prop` argument of `setup_inp()` (see below), while the 
actual computation currently happens at the level of the HBP layer.

### API changes

* Removed `albufera_clusters` and `albufera_cluster_geometries` data-sets (#47).
* Removed `clusters_df` argument from `setup_hbp()`.
* New argument `variety_prop` in `setup_inp()`, to set the proportion of the
Albufera's Natural Park surface allocated for the three rice varieties (J. 
Sendra, Bomba, and Clearfield) (#185).

### GUI

* Shiny app title now includes the version of the package (#174).
* Added favicon to app (#178).
* Loading UI element now show a loading animation (#175).
* Removed unused "seed" inputs from CA and CT layers Shiny modules (#177).

### Visualizations

* CA: changed line styles of applications to dashed (#176).

### Dependencies

* Added Imports: `base64enc` and `utils`.

# erahumed 0.11.4

### Bug fixes

* Fix regression that was making the app crash if called without loading the
package, caused by unreferenced call to the `albufera_clusters` dataset.

### GUI

* Data-frames displayed in the INP Shiny module show up to 4 significant digits
(#164).

* The "Lake Level" variable for the HBA layer plots no longer exists, since the
resulting plot was identical to the "Lake Volume" plot, up to an affine 
transformation of the Y-axis (#101).

### Under the hoods

* Data generation scripts in `data-raw/` now use 'renv::use()' directive at the 
beginning of code to ensure reproducibility (#35)

* Server function of CSV input gets a new argument `sig_digits`, set by default
to `4` (#164).

# erahumed 0.11.3

### GUI

* The selected cluster is now synced across the HBP, CA and CT layers of the
Shiny interface.

### Documentation

* Documented source of `albufera_weather` dataset.

### Under the hoods

* HBP, CA and CT Shiny submodules now share a common leaflet map reactive 
object, that thus does not need to be computed several times (#131).

* Fixed broken functionality of `assert_data.frame(extends = FALSE)` (the
`extends` argument was being bypassed).

* Fix broken `summary.erahumed_simulation_layer()` method.

* Removed low-level creator for `erahumed_simulation` class objects (*i.e.* 
`new_erahumed_simulation()`).

* Remove unused `max = 100` argument of `print.erahumed_simulation()`.

### Testing

* Added snapshot tests for plot methods of ERAHUMED layers (#149).
* Added basic server tests for all shiny modules (#150).
* Added basic UI no-error tests for all shiny modules.
* Testing of `run_simulation()`.
* Test that setting seed in `setup_hbp()` ensures reproducibility, and that 
different seeds give rise to different outputs.
* Testing corner cases of assertions.
* Testing of `print()` and `summary()` S3 methods for `erahumed_simulation` and 
`erahumed_simulation_layer` S3 classes.

# erahumed 0.11.2

### New features

* `setup_hbp()` receives an additional `seed` argument, which sets the seed used
by the underlying algorithm (default to `840`). (#153)

### Minor API/GUI changes

* All modules of the GUI now have a "Download Data" button that allows to 
download the output data.frame (in CSV format) of the corresponding simulation 
layer (#144).

* "Observed/Imputed Data" labels in the legend of HBA plots become 
"Source: Observed/Imputed Outflow Data". (#100)

* `outflow_extra` column HBA output becomes `outflow_recirculation`. 
Correspondingly, the label for this variable in the GUI becomes 
`"Water Recirculation Outflow"`. (#102)

### Documentation

* `?hbp` now documents the columns of the output data.frame of the HBP layer.

* The main package vignette, as well as the `?get_layer()` documentation page 
now mention the built-in plotting methods of `{erahumed}` (#159).

# erahumed 0.11.1

### Algorithm

* The water-sediment partition coefficient of chemicals is now computed as 
`kd = koc * foc`, where `koc` is the carbon-water partition coefficient, and 
`foc` is the fraction of organic content in soil (#105). Correspondingly, 
`setup_ct()` gets a new argument `foc`, with default value `foc = 0.17`.

* The internal data-set of physico-chemical parameters used by the CT layer was 
thoroughly reviewed (#123), with sizable corrections to some parameters. In 
particular, `kd` values have been substituted by `koc` values (see previous 
point).

### API changes

* `get_layer()` now throws an error if the `layer` argument is not specified, 
whereas it would previously return the `"inp"` layer.

### New features

* New helper `erahumed_layers()` that returns the list of ERAHUMED simulation 
layers.
* New helpers `chemicals()` and `chemical_properties()` providing information on 
supported chemicals.

# erahumed 0.11.0

### API changes

Former `erahumed_model()` has been substituted by `erahumed_simulation()`. 
Beyond the terminology change, the main difference lies in the fact that 
simulation setup and computation are handled separately in the new 
implementation. The best introduction to the new API is probably the fresh new
vignette, 
[Simulation workflow](https://erahumed.github.io/erahumed/articles/erahumed-workflow.html).

As a further terminology change, what we previously referred to as a "model 
component" becomes a "simulation layer". Apart from the obvious changes in 
function names, this does not imply any conceptual or API change.

Currently, the exported namespace of the package consists of:

* **Datasets.** No changes.
* **Layer extractors.** `get_layer()`, `get_layer_output()`, 
`get_layer_parameters()`.
* **Simulation initialization.** `erahumed_simulation()`.
* **Simulation setup.** `setup_inp()`, `setup_hba()`, `setup_hbp()`, 
`setup_ca()`, `setup_ct()`.
* **Simulation execution.** `run_simulation()`.
* **Shiny App.** No changes.

### Data

* Reduced the number of significant digits in the default P-ETP function used
by the HBA component, so as to match the relative accuracy of the storage curve 
extracted from the cited CHJ report (#95). 

### Documentation

* New package vignette: [Simulation workflow](https://erahumed.github.io/erahumed/articles/erahumed-workflow.html).

* Added information and references in the documentation page of 
`albufera_clusters` and `albufera_cluster_geometries` datasets (#52).

* Citation of Martínez-Megías et al. (2023) in `?albufera_management` changed 
to APA style.

# erahumed 0.10.0

### Algorithm

In previous implementations of HBP, whenever there was a positive difference
between the ditch's outflow, and the sum of the clusters ideal outflows, this 
was compensated by sharing it uniformly among clusters (and adding equal amounts 
of irrigation to these). We stop doing this and, instead, allow for the sum of 
cluster outflows to be less than the corresponding ditch outflow. If we assume
constant ditch water levels, this amounts to assuming that the difference comes
from an independent source.

### Visualization

* HBP model component plot method: the height variable plotted is now 
`height_sod_cm`, rather than `height_eod_cm` as before. This is done in order to 
simplify visual comparisons between the plots for HBP and CT model components.

### New features

* HBP model component output now also includes an `height_sod_cm` column, 
representing the cluster's water level at "start-of-day".

### Breaking changes in API

* Variable names refactors:

  * Former `irrigation`, `draining` and `height_cm` columns of 
  `albufera_management` become `ideal_irrigation`, `ideal_draining` and 
  `ideal_height_eod_cm`.

  * Former `real_irrigation`, `real_draining` and `real_height_cm` columns of 
  the HBP component output become `irrigation`, `draining` and `height_eod_cm`.
  
  * Former `real_inflow_cm/m3_s` and `real_outflow_cm/m3_s` are renamed 
  `inflow_cm/m3_s` and `outflow_cm/m3_s`, respectively.

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
