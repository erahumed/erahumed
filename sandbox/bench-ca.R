library(erahumed)
library(profvis)

hbl <- albufera_hb_local(date_min = "2010-01-01", date_max = "2011-01-01")

profvis({ ca(hbl) })
