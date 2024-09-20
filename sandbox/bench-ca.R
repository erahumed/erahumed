library(erahumed)
library(profvis)

hbl <- albufera_hbp()

profvis({ ca(hbl) })
