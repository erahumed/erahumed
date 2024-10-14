library(dplyr)

# TODO: set amounts
albufera_ca_schedules <- tribble(
  ~day,   ~rice_variety,  ~chemical,        ~kg_per_ha,     ~application_type,
  51,     "J.Sendra",     "Acetamiprid",    0.03,           "ground",
  51,     "Bomba",        "Acetamiprid",    0.03,           "ground",
  51,     "Clearfield",   "Acetamiprid",    0.03,           "ground",

  51,     "J.Sendra",     "Benta",          1.00,           "ground",
  51,     "Bomba",        "Benta",          1.00,           "ground",

  51,     "J.Sendra",     "MCPA",           0.50,           "ground",
  51,     "Bomba",        "MCPA",           0.50,           "ground",

  21,     "J.Sendra",     "Penoxulam",      0.04,           "ground",
  21,     "Bomba",        "Penoxulam",      0.04,           "ground",

  7,      "J.Sendra",     "Cyhalo",         0.30,           "ground",
  7,      "Bomba",        "Cyhalo",         0.30,           "ground",
  21,     "J.Sendra",     "Cyhalo",         0.30,           "ground",
  21,     "Bomba",        "Cyhalo",         0.30,           "ground",

  21,     "Clearfield",   "Cicloxidim",     0.30,           "ground",
  51,     "Clearfield",   "Cicloxidim",     0.30,           "ground",

  76,     "Bomba",        "Azoxy",          0.20,           "aerial",
  90,     "Bomba",        "Azoxy",          0.20,           "aerial",
  104,    "Bomba",        "Azoxy",          0.20,           "aerial",
  76,     "J.Sendra",     "Azoxy",          0.20,           "aerial",
  90,     "J.Sendra",     "Azoxy",          0.20,           "aerial",
  76,     "Clearfield",   "Azoxy",          0.20,           "aerial",
  90,     "Clearfield",   "Azoxy",          0.20,           "aerial",

  76,     "Bomba",        "Difeno",         0.13,           "aerial",
  90,     "Bomba",        "Difeno",         0.13,           "aerial",
  104,    "Bomba",        "Difeno",         0.13,           "aerial",
  76,     "J.Sendra",     "Difeno",         0.13,           "aerial",
  90,     "J.Sendra",     "Difeno",         0.13,           "aerial",
  76,     "Clearfield",   "Difeno",         0.13,           "aerial",
  90,     "Clearfield",   "Difeno",         0.13,           "aerial"
)

