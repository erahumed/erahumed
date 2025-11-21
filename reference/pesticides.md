# Predefined pesticide definitions

These functions return predefined objects of class
[chemical](https://erahumed.github.io/erahumed/reference/chemical.md),
representing commonly used pesticides and their properties.

## Usage

``` r
acetamiprid()

azoxystrobin()

bentazone()

cycloxydim()

cyhalofop_butyl()

difenoconazole()

mcpa()

penoxsulam()
```

## Value

An object of class
[chemical](https://erahumed.github.io/erahumed/reference/chemical.md).

## Examples

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
