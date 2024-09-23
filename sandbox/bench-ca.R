library(erahumed)
library(profvis)

hbl <- hbp()

profvis({ ca(hbl) })
