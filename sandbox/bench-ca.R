library(erahumed)
library(profvis)

hbl <- albufera_hb_local()

profvis({ ca(hbl) })
