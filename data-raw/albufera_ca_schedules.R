library(dplyr)

# TODO: set amounts
albufera_ca_schedules <- tribble(
  ~day,   ~rice_variety,  ~chemical,        ~amount,        ~application_type,
  51,     "J.Sendra",     "Acetamiprid",    1,              "ground",
  51,     "Bomba",        "Acetamiprid",    1,              "ground",
  51,     "Clearfield",   "Acetamiprid",    1,              "ground",

  51,     "J.Sendra",     "Benta",          1,              "ground",
  51,     "Bomba",        "Benta",          1,              "ground",

  51,     "J.Sendra",     "MCPA",           1,              "ground",
  51,     "Bomba",        "MCPA",           1,              "ground",

  21,     "J.Sendra",     "Penoxulam",      1,              "ground",
  21,     "Bomba",        "Penoxulam",      1,              "ground",

  7,      "J.Sendra",     "Cyhalo",         1,              "ground",
  7,      "Bomba",        "Cyhalo",         1,              "ground",
  21,     "J.Sendra",     "Cyhalo",         1,              "ground",
  21,     "Bomba",        "Cyhalo",         1,              "ground",

  21,     "Clearfield",   "Cicloxidim",     1,              "ground",
  51,     "Clearfield",   "Cicloxidim",     1,              "ground",

  76,     "Bomba",        "Azoxy",          1,              "aerial",
  90,     "Bomba",        "Azoxy",          1,              "aerial",
  104,    "Bomba",        "Azoxy",          1,              "aerial",
  76,     "J.Sendra",     "Azoxy",          1,              "aerial",
  90,     "J.Sendra",     "Azoxy",          1,              "aerial",
  76,     "Clearfield",   "Azoxy",          1,              "aerial",
  90,     "Clearfield",   "Azoxy",          1,              "aerial",

  76,     "Bomba",        "Difeno",         1,              "aerial",
  90,     "Bomba",        "Difeno",         1,              "aerial",
  104,    "Bomba",        "Difeno",         1,              "aerial",
  76,     "J.Sendra",     "Difeno",         1,              "aerial",
  90,     "J.Sendra",     "Difeno",         1,              "aerial",
  76,     "Clearfield",   "Difeno",         1,              "aerial",
  90,     "Clearfield",   "Difeno",         1,              "aerial"
)

usethis::use_data(albufera_ca_schedules, overwrite = TRUE)
