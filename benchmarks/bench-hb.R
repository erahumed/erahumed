library(erahumed)
library(profvis)

profvis({
  albufera_hydro_balance_local(date_min = "2010-01-01", date_max = "2020-01-01")
  })
