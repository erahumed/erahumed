# Initialize an ERAHUMED simulation

Initializes an ERAHUMED simulation. See the [main package
vignette](https://erahumed.github.io/erahumed/articles/erahumed-workflow.html)
for a practical, step-by-step overview of the simulation workflow. For a
detailed explanation of the underlying models, algorithms, and
assumptions, refer to the [user
manual](https://erahumed.github.io/erahumed-book/).

## Usage

``` r
erahumed_simulation(
  date_start = "2023-01-01",
  date_end = "2024-12-31",
  seed = 840,
  outflows_df = erahumed::albufera_outflows,
  storage_curve_slope_m2 = 23.66 * 1e+06,
  storage_curve_intercept_m3 = 16.75 * 1e+06,
  petp_surface_m2 = 53.9 * 1e+06,
  weather_df = erahumed::albufera_weather,
  ideal_flow_rate_cm = 4.27,
  height_thresh_cm = 0.5,
  ditch_level_m = 1,
  covmax = 0.5,
  jgrow = 152,
  dact_m = 0.036,
  css_ppm = 50,
  foc_ss = 0.1,
  foc_sed = 0.082,
  bd_g_cm3 = 1.95,
  porosity = 0.11,
  ksetl_m_day = 2,
  rfms_map = default_rfms_map(seed = seed),
  .progress = message
)
```

## Arguments

- date_start:

  `[Date(1)]`  
  A single-value `Date` vector (or a convertible object). Defines the
  starting date of the date interval for the simulation.

- date_end:

  `[Date(1)]`  
  A single-value `Date` vector (or a convertible object). Defines the
  ending date of the date interval for the simulation.

- seed:

  `[numeric(1)]`  
  Seed for random number generation used by the simulation algorithms.

- outflows_df:

  `[data.frame]`  
  Time-series dataset that provides the observational hydrological data
  on the Albufera lake, along the template of
  [albufera_outflows](https://erahumed.github.io/erahumed/reference/albufera_outflows.md)
  (the default value).

- storage_curve_slope_m2:

  `[numeric(1)]`  
  Slope of the (linear) storage curve of the Albufera Lake, in square
  meters. Multiplying this value by the change in the lake's water level
  gives the corresponding change in water *volume*. The default numeric
  values are derived from the CHJ report [*Modelo de seguimiento de
  l’Albufera de Valencia con
  AQUATOOLDMA*](https://www.chj.es/Descargas/ProyectosOPH/Consulta%20publica/PHC-2015-2021/ReferenciasBibliograficas/HumedalesZonasProtegidas/CHJ,2012.Aquatool_Albufera.pdf).

- storage_curve_intercept_m3:

  `[numeric(1)]`  
  Intercept of the linear storage curve for the Albufera Lake, expressed
  in cubic meters. This value represents the lake's water volume when
  the water level is at sea level. The default numeric values are
  derived from the CHJ report [*Modelo de seguimiento de l’Albufera de
  Valencia con
  AQUATOOLDMA*](https://www.chj.es/Descargas/ProyectosOPH/Consulta%20publica/PHC-2015-2021/ReferenciasBibliograficas/HumedalesZonasProtegidas/CHJ,2012.Aquatool_Albufera.pdf).

- petp_surface_m2:

  `[numeric(1)]`  
  The surface area (in square meters) used to convert precipitation and
  evapotranspiration per unit area into volume changes for the Albufera
  Lake. The default numeric value was computed by the package authors
  and corresponds to the average flooded surface of the Albufera lake
  and its irrigation ditches.

- weather_df:

  `[data.frame]`  
  A dataset that provides the relevant metereological time series, along
  the template of
  [albufera_weather](https://erahumed.github.io/erahumed/reference/albufera_weather.md)
  (the default value).

- ideal_flow_rate_cm:

  `[numeric(1)]`  
  Ideal inflow/outflow of a cluster, for days in which the cluster is
  scheduled to be in flux (*i.e.* when being simultaneously irrigated
  and drained). Expressed in centimeters of water depth per day.

- height_thresh_cm:

  `[numeric(1)]`  
  A positive number. Height threshold for water levels, below which a
  cluster is considered to be emptied.

- ditch_level_m:

  `[numeric(1)]`  
  Constant water depth in ditches.

- covmax:

  `[numeric(1)]`  
  A number between `0` and `1`. Maximum potential fraction of applied
  chemicals intercepted by foliage at crop maturation.

- jgrow:

  `[numeric(1)]`  
  A positive integer. Length (in days) of crop maturation cycle in rice
  paddies.

- dact_m:

  `[numeric(1)]`  
  A positive number. Active sediment layer depth, where "active" refers
  to the portion of the sediment that is actually involved in chemical
  exchange or transport processes. Expressed in meters.

- css_ppm:

  `[numeric(1)]`  
  A positive number. Concentration of water suspended solids, expressed
  in parts per million.

- foc_ss:

  `[numeric(1)]`  
  A number between `0` and `1`. Fraction of organic matter content
  within suspended solids.

- foc_sed:

  `[numeric(1)]`  
  A number between `0` and `1`. Fraction of organic matter content
  within sediment.

- bd_g_cm3:

  `[numeric(1)]`  
  A positive number. Bulk density of the sediment (including both the
  solid material and the pore spaces). Expressed in grams per cubic
  centimeter.

- porosity:

  `[numeric(1)]`  
  A number between `0` and `1`. The fraction of the total sediment
  volume that is occupied by pore spaces.

- ksetl_m_day:

  `[numeric(1)]`  
  Settling rate, representing particle-bound transfer to sediment.

- rfms_map:

  `[erahumed_rfms_map]`  
  An object of class
  [rfms_map](https://erahumed.github.io/erahumed/reference/rfms_map.md)
  that defines how different rice field management systems (RFMSs) are
  assigned to spatial clusters within the simulation. This object also
  encapsulates the full set of user-defined agrochemical configurations,
  including custom chemicals and RFMSs, and serves as the main interface
  for scenario customization. See
  [rfms](https://erahumed.github.io/erahumed/reference/rfms.md) for an
  overview of RFMS definitions and customization options.

- .progress:

  A function used to report simulation progress. It should accept a
  single character string as input, representing the current stage of
  the simulation (e.g., `"Computing hydrology: lake"`). This parameter
  is primarily intended for user feedback mechanisms (e.g., console
  messages). By default, it prints a message to the R console.

## Value

An object of class `erahumed_simulation`.

## Details

The `rfms_map` argument plays a central role in the customization
capabilities of the ERAHUMED model. Beyond mapping rice field management
systems (RFMSs) to spatial clusters, it also carries the full
definitions of custom chemicals and RFMSs configured by the user. This
makes it the primary interface for building and running alternative
scenarios involving changes in pesticide use or management strategies.
For a detailed explanation of RFMS concepts, structure, and
configuration, see the [RFMS section of the user
manual](https://erahumed.github.io/erahumed-book/chapters/rfms.html).

## Examples

``` r
erahumed_simulation()
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
#> <ERAHUMED Simulation>
#>   Date range             : 2023-01-01 to 2024-12-31 
#>   Simulation days        : 731 
#>   Clusters               : 552 
#>   Management systems     : 3 
#>   Chemicals simulated    : 8 
#>   Total applications     : 29 
#> 
#> Need help extracting simulation outputs? Check `?get_results`.
```
