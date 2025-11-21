# Designing custom agrochemical scenarios

This vignette is a practical guide to **scenario design** in
[erahumed](https://github.com/erahumed/erahumed), focused on two
pillars:

1.  **Rice-Field Management Systems (RFMS)** — the agrochemical and
    hydrological management regimes applied to rice-field clusters, and
    their spatial allocation via an `rfms_map`.

2.  **Custom chemicals** — defining substances (identity,
    fate/transport, sorption, toxicity) that can be referenced by RFMS
    application schedules.

A conceptual explanation of RFMS is provided in the [user
manual](https://erahumed.github.io/erahumed-book/chapters/rfms.html).
Here we focus on *practical usage* within R.

``` r
library(erahumed)
```

## Built-in definitions

We begin by describing the built-in definitions of these objects, as a
way to illustrate the general concepts.

Starting with RFMSs, [erahumed](https://github.com/erahumed/erahumed)
comes with three predefined management systems - *Bomba*, *Clearfield*
and *J.Sendra* - whose definitions are given by the helper functions
[`bomba()`](https://erahumed.github.io/erahumed/reference/rfms.md),
[`clearfield()`](https://erahumed.github.io/erahumed/reference/rfms.md),
and
[`jsendra()`](https://erahumed.github.io/erahumed/reference/rfms.md).
For instance:

``` r
clearfield()
#> <Rice Field Management System>
#>   Name               :  Clearfield 
#>   Sowing period     : Day 113 to 251 
#>   Perelloná period  : Day 306 to 15 
#>   Flow height       : 10 cm during sowing season
#>   Perelloná height  : 20 cm during Perelloná
#>   Applications      : 7 chemical application(s)
```

To get a slightly more detailed view of this object, we can use the
[`summary()`](https://rdrr.io/r/base/summary.html) method, which also
prints the list of scheduled chemical applications for this RFMS:

``` r
summary(clearfield())
#> <Rice Field Management System>
#>   Name               :  Clearfield 
#>   Sowing period     : Day 113 to 251 
#>   Perelloná period  : Day 306 to 15 
#>   Flow height       : 10 cm during sowing season
#>   Perelloná height  : 20 cm during Perelloná
#>   Applications      : 7 chemical application(s)
#>        chemical   type seed_day amount_kg_ha emptying_days
#>     Acetamiprid ground       52         0.03             6
#>      Cycloxydim ground       18         0.30             2
#>      Cycloxydim ground       52         0.30             6
#>    Azoxystrobin aerial       76         0.20            NA
#>    Azoxystrobin aerial       90         0.20            NA
#>  Difenoconazole aerial       76         0.13            NA
#>  Difenoconazole aerial       90         0.13            NA
```

The way in which RFMSs are distributed across rice-field clusters is
encoded in the *RFMS map*. The default map applied by
[erahumed](https://github.com/erahumed/erahumed) can be recovered
through:

``` r
default_rfms_map()
#> <Rice-Field Management System Map>
#>   Clusters         : 552 
#>   Management systems   : 3
```

To delve under the hoods of this map, we can again use
[`summary()`](https://rdrr.io/r/base/summary.html):

``` r
summary(default_rfms_map())
#> <Rice-Field Management System Map Summary>
#>   Total clusters        : 552 
#>   Management systems: 3 
#>         rfms_id assigned_clusters
#> 1   1: J.Sendra               341
#> 2      2: Bomba                76
#> 3 3: Clearfield               135
```

As seen above, the built-in RFMS definitions rely on the definitions of
chemicals. The package includes the following predefined objects:
[`acetamiprid()`](https://erahumed.github.io/erahumed/reference/pesticides.md),
[`azoxystrobin()`](https://erahumed.github.io/erahumed/reference/pesticides.md),
[`bentazone()`](https://erahumed.github.io/erahumed/reference/pesticides.md),
[`cycloxydim()`](https://erahumed.github.io/erahumed/reference/pesticides.md),
[`cyhalofop_butyl()`](https://erahumed.github.io/erahumed/reference/pesticides.md),
[`difenoconazole()`](https://erahumed.github.io/erahumed/reference/pesticides.md),
[`mcpa()`](https://erahumed.github.io/erahumed/reference/pesticides.md)
and
[`penoxsulam()`](https://erahumed.github.io/erahumed/reference/pesticides.md).
The properties of these are inspected in the usual way, *e.g.*:

``` r
acetamiprid()
#> <erahumed_chemical>
#> Name:        Acetamiprid 
#> TMoA ID:     NicotinicAcetylcholine 
#> MW:          222.677 g/mol
#> 
#> Physico-chemical properties:
#>   Solubility:      2950.00 ppm
#>   Koc:             200.00 cm³/g
#>   Foliar extraction term:  0.200 cm
#> Degradation rates:
#>   kf (foliage):           0.1100 1/day
#>   kw (water column):         0.0154 1/day @ 20.0³C (Q10 = 2.58)
#>   ks (saturated sediment):   0.0159 1/day @ 20.0³C (Q10 = 2.58)
#>   ks (unsaturated sediment): 0.0290 1/day @ 20.0³C (Q10 = 2.58)
#> 
#> Toxicity (SSD, log₁₀ scale):
#>   Acute   mean ± sd: 2.44 ± 1.57
#>   Chronic mean ± sd: 1.99 ± 1.58
```

## Creating custom chemicals

New chemicals can be created with the
[`chemical()`](https://erahumed.github.io/erahumed/reference/chemical.md)
function, for example:

``` r
chemical(display_name = "X", tmoa_id = "Y")
#> <erahumed_chemical>
#> Name:        X 
#> TMoA ID:     Y 
#> MW:          330 g/mol
#> 
#> Physico-chemical properties:
#>   Solubility:      500.00 ppm
#>   Koc:             200.00 cm³/g
#>   Foliar extraction term:  0.200 cm
#> Degradation rates:
#>   kf (foliage):           0.2000 1/day
#>   kw (water column):         0.0500 1/day @ 20.0³C (Q10 = 2.58)
#>   ks (saturated sediment):   0.0500 1/day @ 20.0³C (Q10 = 2.58)
#>   ks (unsaturated sediment): 0.0500 1/day @ 20.0³C (Q10 = 2.58)
#> 
#> Toxicity (SSD, log₁₀ scale):
#>   Acute   mean ± sd: 7.50 ± 2.50
#>   Chronic mean ± sd: 4.50 ± 2.50
```

returns an object representing a new substance named “X” with Toxic Mode
of Action (TMoA) “Y”, filling any unspecified physico-chemical and
toxicity properties with placeholder values. To further customize the
properties of “X”, specify additional arguments in
[`chemical()`](https://erahumed.github.io/erahumed/reference/chemical.md).
For instance:

``` r
chemical(display_name = "X", tmoa_id = "Y", MW = 100)
#> <erahumed_chemical>
#> Name:        X 
#> TMoA ID:     Y 
#> MW:          100 g/mol
#> 
#> Physico-chemical properties:
#>   Solubility:      500.00 ppm
#>   Koc:             200.00 cm³/g
#>   Foliar extraction term:  0.200 cm
#> Degradation rates:
#>   kf (foliage):           0.2000 1/day
#>   kw (water column):         0.0500 1/day @ 20.0³C (Q10 = 2.58)
#>   ks (saturated sediment):   0.0500 1/day @ 20.0³C (Q10 = 2.58)
#>   ks (unsaturated sediment): 0.0500 1/day @ 20.0³C (Q10 = 2.58)
#> 
#> Toxicity (SSD, log₁₀ scale):
#>   Acute   mean ± sd: 7.50 ± 2.50
#>   Chronic mean ± sd: 4.50 ± 2.50
```

sets the molecular weight of “X” to
$100\,\text{g} \cdot \text{mol}^{- 1}$.For a full list of available
chemical parameters, see
[`?chemical`](https://erahumed.github.io/erahumed/reference/chemical.md)
or the relevant sections in the user manual (for example,
[here](https://erahumed.github.io/erahumed-book/chapters/rfms.html#sec-chemicals).

Note that to store the object in the current R session, you need to
assign it, for example:

``` r
chem_x <- chemical(display_name = "X", tmoa_id = "Y", MW = 100)
```

## Creating custom RFMS

The creation of custom RFMSs proceeds in two steps: initialization with
the
[`new_rfms()`](https://erahumed.github.io/erahumed/reference/rfms.md)
constructor, and application scheduling with
[`schedule_application()`](https://erahumed.github.io/erahumed/reference/schedule_application.md).For
example, to create a new RFMS that schedules an application of our new
compound “X”, we would proceed as follows.

We first initialize the new RFMS, which we will call “Z”:

``` r
rfms_z <- new_rfms(sowing_yday = 120, display_name = "Z")
```

As the code above illustrates, at this point we can also set some
general properties of the RFMS, such as the start date of the sowing
season (day of year 120 corresponds to May 1 on a regular non-leap
year). The full list of customizable parameters can be found in
[`?new_rfms`](https://erahumed.github.io/erahumed/reference/rfms.md).

Subsequently, we schedule chemical applications:

``` r
rfms_z <- rfms_z |> schedule_application(chemical = chem_x, 
                                         seed_day = 30,
                                         amount_kg_ha = 1,
                                         type = "ground", 
                                         )
```

Here, the `chemical` argument must be a valid chemical object, such as
the `chem_x` created above. This can also be any built-in chemical
definition, for example:

``` r
rfms_z <- rfms_z |> schedule_application(chemical = difenoconazole(), 
                                         seed_day = 80,
                                         amount_kg_ha = 0.5,
                                         type = "aerial", 
                                         )
```

The day of year on which applications should *ideally* occur is
specified with the `seed_day` argument, counted from the sowing day
(`120` in our example, so chemical “X” is scheduled for day of year
`150`). The actual application days are often delayed with respect to
this ideal, in order to satisfy hydrological constraints; this is
described in detail in the [user
manual](https://erahumed.github.io/erahumed-book/chapters/exposure.html#mass-income-rates).

We can inspect our current definition of `rfms_z` as follows:

``` r
summary(rfms_z)
#> <Rice Field Management System>
#>   Name               :  Z 
#>   Sowing period     : Day 120 to 251 
#>   Perelloná period  : Day 306 to 15 
#>   Flow height       : 10 cm during sowing season
#>   Perelloná height  : 20 cm during Perelloná
#>   Applications      : 2 chemical application(s)
#>        chemical   type seed_day amount_kg_ha emptying_days
#>               X ground       30          1.0             1
#>  Difenoconazole aerial       80          0.5            NA
```

Note that RFMS definition is incremental: you first create an empty RFMS
object, then populate it by scheduling applications one at a time. In
fact, we can also use
[`schedule_application()`](https://erahumed.github.io/erahumed/reference/schedule_application.md)
to create a modified version of the pre-defined RFMS. For instance:

``` r
clearfield_bis <- clearfield() |> schedule_application(chemical = chem_x,
                                                       seed_day = 10,
                                                       amount_kg_ha = 1,
                                                       type = "ground"
                                                       )
```

Creates a new version of the *Clearfield* system with an extra
application of chemical “X” on day 10.

A practical way to code the definition of custom RFMSs, using the R
piping syntax, is presented in the last section of this vignette.

## Creating custom RFMS map

The RFMS map defines how RFMSs are allocated across the rice-field
surface of the Albufera Natural Park. Its definition also proceeds in
two steps.

First, initialize the map by choosing a default RFMS, which can be any
valid RFMS object:

``` r
m <- new_rfms_map(default_rfms = rfms_z)
```

Then allocate portions of the surface to other RFMSs using
[`allocate_surface()`](https://erahumed.github.io/erahumed/reference/allocate_surface.md),
for example:

``` r
m <- m |> allocate_surface(system = clearfield(), target_fraction = 0.3)
```

In this example, the resulting RFMS map will be 30% Clearfield and 70%
our custom RFMS “Z”:

``` r
summary(m)
#> <Rice-Field Management System Map Summary>
#>   Total clusters        : 552 
#>   Management systems: 2 
#>         rfms_id assigned_clusters
#> 1          1: Z               405
#> 2 2: Clearfield               147
```

Allocation can also be targeted to specific types and locations of rice
fields; see
[`?allocate_surface`](https://erahumed.github.io/erahumed/reference/allocate_surface.md)
for details.

## Full workflow

We summarize here the full workflow for running a simulation with a
custom agrochemical management scenario.

Proceeding bottom-up, we begin by defining the substances to be applied
to rice fields:

``` r
chem_1 <- chemical(display_name = "Chem 1", tmoa_id = "TMoA 1")
chem_2 <- chemical(display_name = "Chem 2", tmoa_id = "TMoA 1")
chem_3 <- chemical(display_name = "Chem 3", tmoa_id = "TMoA 2")
```

(In a real study, you would also set specific physico-chemical and
toxicity properties for these chemicals. Here we just kept the defaults
for brevity.)

Once we have the full set of custom substances, we define our custom
RFMSs — either starting from scratch or by extending existing ones:

``` r
rfms_1 <- new_rfms() |>
  schedule_application(chem_1, seed_day = 10, amount_kg_ha = 1, type = "ground") |>
  schedule_application(chem_2, seed_day = 20, amount_kg_ha = 2, type = "ground") |>
  schedule_application(chem_3, seed_day = 70, amount_kg_ha = 2, type = "aerial") |>
  schedule_application(difenoconazole(), seed_day = 90, amount_kg_ha = 0.5, type = "aerial")

bomba_bis <- bomba() |>
  schedule_application(chem_3, seed_day = 70, amount_kg_ha = 2, type = "aerial")
```

As shown above, using the R pipe `|>` syntax is convenient for defining
RFMSs, thanks to their incremental nature.

Once the RFMSs are set up, we allocate them in a new RFMS map:

``` r
map <- new_rfms_map(default_rfms = rfms_1) |>
  allocate_surface(bomba_bis, target_fraction = 0.2)
```

This completes our round of definitions.

The `map` object created above encodes all details relevant to the
agrochemical scenario of the simulation. We can now run a simulation
with this scenario by passing the `map` object to the `rfms_map`
argument of
[`erahumed_simulation()`](https://erahumed.github.io/erahumed/reference/erahumed_simulation.md):

``` r
sim <- erahumed_simulation(rfms_map = map)
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
sim
#> <ERAHUMED Simulation>
#>   Date range             : 2023-01-01 to 2024-12-31 
#>   Simulation days        : 731 
#>   Clusters               : 552 
#>   Management systems     : 2 
#>   Chemicals simulated    : 10 
#>   Total applications     : 17
#> 
#> Need help extracting simulation outputs? Check `?get_results`.
```

From this point on, the analysis proceeds as usual. For more details,
see the [main package
vignette](https://erahumed.github.io/erahumed/articles/erahumed-workflow.html).
