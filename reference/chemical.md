# Define a pesticide with physico-chemical and toxicity properties

Creates a list of parameters that define a pesticide or chemical
substance, including its identity, degradation rates, sorption
properties, and toxicity to aquatic organisms.

## Usage

``` r
chemical(
  display_name,
  tmoa_id,
  MW = 330,
  sol_ppm = 500,
  koc_cm3_g = 200,
  fet_cm = 0.2,
  kf_day = 0.2,
  kw_day = 0.05,
  ks_sat_day = 0.05,
  ks_unsat_day = 0.05,
  kw_temp = 20,
  ks_sat_temp = 20,
  ks_unsat_temp = 20,
  Q10_kw = 2.58,
  Q10_ks_sat = 2.58,
  Q10_ks_unsat = 2.58,
  ssd_acute_mu = 7.5,
  ssd_acute_sigma = 2.5,
  ssd_chronic_mu = 4.5,
  ssd_chronic_sigma = 2.5,
  pnec_acute_ug_L = 1,
  pnec_chronic_ug_L = 1
)
```

## Arguments

- display_name:

  `[character(1)]`  
  The common name of the chemical (e.g., "Acetamiprid").

- tmoa_id:

  `[character(1)]`  
  Identifier for the toxic mode of action (e.g.,
  "NicotinicAcetylcholine").

- MW:

  `[numeric(1)]`  
  Molecular weight in grams per mole.

- sol_ppm:

  `[numeric(1)]`  
  Water solubility.

- koc_cm3_g:

  `[numeric(1)]`  
  Organic carbon-water partition coefficient (Koc).

- fet_cm:

  `[numeric(1)]`  
  Fraction of pesticide washed off from foliage per cm of rainfall.

- kf_day:

  `[numeric(1)]`  
  Degradation rate in foliage.

- kw_day:

  `[numeric(1)]`  
  Degradation rate in the water column.

- ks_sat_day:

  `[numeric(1)]`  
  Degradation rate in saturated sediment.

- ks_unsat_day:

  `[numeric(1)]`  
  Degradation rate in unsaturated sediment.

- kw_temp:

  `[numeric(1)]`  
  Reference temperature for kw (degradation rate in water).

- ks_sat_temp:

  `[numeric(1)]`  
  Reference temperature for ks_sat (degradation rate in saturated
  sediment).

- ks_unsat_temp:

  `[numeric(1)]`  
  Reference temperature for ks_unsat (degradation rate in unsaturated
  sediment).

- Q10_kw:

  `[numeric(1)]`  
  Q10 coefficient for temperature correction of degradation in water.

- Q10_ks_sat:

  `[numeric(1)]`  
  Q10 coefficient for temperature correction in saturated sediment.

- Q10_ks_unsat:

  `[numeric(1)]`  
  Q10 coefficient for temperature correction in unsaturated sediment.

- ssd_acute_mu:

  `[numeric(1)]`  
  Mean (log10 scale) of the acute species sensitivity distribution. The
  unit of this input (log10(microgram/liter)) is interpreted as follows:
  the base 10 exponential of the input is the median (according to the
  acute SSD) concentration in micrograms per liter.

- ssd_acute_sigma:

  `[numeric(1)]`  
  Standard deviation (log10 scale) of the acute species sensitivity
  distribution.

- ssd_chronic_mu:

  `[numeric(1)]`  
  Mean (log10 scale) of the chronic species sensitivity distribution.
  The unit of this input (log10(microgram/liter)) is interpreted as
  follows: the base 10 exponential of the input is the median (according
  to the chronic SSD) concentration in micrograms per liter.

- ssd_chronic_sigma:

  `[numeric(1)]`  
  Standard deviation (log10 scale) of the chronic species sensitivity
  distribution.

- pnec_acute_ug_L:

  `[numeric(1)]`  
  Predicted No Effect Concentration used for the computation of acute
  risk quotients.

- pnec_chronic_ug_L:

  `[numeric(1)]`  
  Predicted No Effect Concentration used for the computation of chronic
  risk quotients.

## Value

An object of class `erahumed_chemical`.

## Details

If not explicitly specified, default values will be used for all
parameters except `display_name` and `tmoa_id`. These defaults are
intended to represent typical or illustrative values and **do not
correspond to any specific real-world chemical**. They are provided
solely for convenience in prototyping or testing.

For a detailed explanation of chemical parameters, their meaning, units,
and role in the model, see the [Chemical parameters section of the user
manual](https://erahumed.github.io/erahumed-book/chapters/model-inputs.html#tbl-chemical-params).
