library(dplyr)

# TODO: set amounts
albufera_management_chemicals <- tribble(
  ~day,   ~variety,       ~chemical,        ~amount,
  51,     "J.Sendra",     "Acetamiprid",    0,
  51,     "Bomba",        "Acetamiprid",    0,
  51,     "Clearfield",   "Acetamiprid",    0,

  51,     "J.Sendra",     "Benta",          0,
  51,     "Bomba",        "Benta",          0,

  51,     "J.Sendra",     "MCPA",           0,
  51,     "Bomba",        "MCPA",           0,

  21,     "J.Sendra",     "Penoxulam",      0,
  21,     "Bomba",        "Penoxulam",      0,

  7,      "J.Sendra",     "Cyhalo",         0,
  7,      "Bomba",        "Cyhalo",         0,
  21,     "J.Sendra",     "Cyhalo",         0,
  21,     "Bomba",        "Cyhalo",         0,

  21,     "Clearfield",   "Cicloxidim",     0,
  51,     "Clearfield",   "Cicloxidim",     0,

  76,     "Bomba",        "Azoxy",          0,
  90,     "Bomba",        "Azoxy",          0,
  104,    "Bomba",        "Azoxy",          0,
  76,     "J.Sendra",     "Azoxy",          0,
  90,     "J.Sendra",     "Azoxy",          0,
  NA,     "J.Sendra",     "Azoxy",          0,  # TODO: why?
  76,     "Clearfield",   "Azoxy",          0,
  90,     "Clearfield",   "Azoxy",          0,
  NA,     "Clearfield",   "Azoxy",          0,  # TODO: why?

  76,     "Bomba",        "Difeno",         0,
  90,     "Bomba",        "Difeno",         0,
  104,    "Bomba",        "Difeno",         0,
  76,     "J.Sendra",     "Difeno",         0,
  90,     "J.Sendra",     "Difeno",         0,
  NA,     "J.Sendra",     "Difeno",         0,  # TODO: why?
  76,     "Clearfield",   "Difeno",         0,
  90,     "Clearfield",   "Difeno",         0,
  NA,     "Clearfield",   "Difeno",         0   # TODO: why?
)

usethis::use_data(albufera_management_chemicals, overwrite = TRUE)
