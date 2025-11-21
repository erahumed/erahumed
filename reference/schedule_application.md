# Schedule chemical application

Adds a chemical application to a rice field management system (RFMS),
specifying timing, application method, and chemical properties.

## Usage

``` r
schedule_application(
  system,
  chemical,
  amount_kg_ha,
  seed_day,
  type = c("ground", "aerial"),
  emptying_days = 1
)
```

## Arguments

- system:

  `[erahumed_rfms]`  
  A rice field management system to which the application is added. See
  [rfms](https://erahumed.github.io/erahumed/reference/rfms.md) for
  details.

- chemical:

  `[erahumed_chemical]`  
  Chemical compound to be applied. See
  [chemical](https://erahumed.github.io/erahumed/reference/chemical.md)
  for details.

- amount_kg_ha:

  `[numeric(1)]`  
  Amount of active ingredient applied per hectare.

- seed_day:

  `[integer(1)]`  
  Number of days after seeding when the application is performed.

- type:

  `[character(1)]`  
  Application method, either 'ground' (requiring draining) or 'aerial'.

- emptying_days:

  `[numeric(1)]`  
  Number of days the field remains empty **before** a ground
  application. The chemical is always assumed to be applied on the
  **last day** of this emptying period. Ignored if the application type
  is `"aerial"`.

## Value

An updated object of class
[erahumed_rfms](https://erahumed.github.io/erahumed/reference/rfms.md)
with the scheduled application included.

## See also

[rfms](https://erahumed.github.io/erahumed/reference/rfms.md),
[chemical](https://erahumed.github.io/erahumed/reference/chemical.md)
