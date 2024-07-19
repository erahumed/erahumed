library(erahumed)
library(profvis)

# Quick benchmark for diagnostic purposes
profvis({
  albufera_hydro_balance_local(date_min = "2010-01-01", date_max = "2011-01-01")
})

# Big benchmark, what a user could actually need to run
profvis({
  albufera_hydro_balance_local(date_min = "2010-01-01", date_max = "2020-01-01")
  })
