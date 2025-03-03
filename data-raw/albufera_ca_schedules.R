renv::use(
  cli        = "cli@3.6.3",
  dplyr      = "dplyr@1.1.4",
  fansi      = "fansi@1.0.6",
  generics   = "generics@0.1.3",
  glue       = "glue@1.8.0",
  lifecycle  = "lifecycle@1.0.4",
  magrittr   = "magrittr@2.0.3",
  pillar     = "pillar@1.9.0",
  pkgconfig  = "pkgconfig@2.0.3",
  R6         = "R6@2.5.1",
  renv       = "renv@1.0.11",
  rlang      = "rlang@1.1.4",
  tibble     = "tibble@3.2.1",
  tidyselect = "tidyselect@1.2.1",
  utf8       = "utf8@1.2.4",
  vctrs      = "vctrs@0.6.5",
  withr      = "withr@3.0.1"
)

library(dplyr)

albufera_ca_schedules <- tribble(
  ~day,   ~rice_variety,  ~chemical,          ~kg_per_ha,     ~application_type,
  51,     "J.Sendra",     "Acetamiprid",      0.03,           "ground",
  51,     "Bomba",        "Acetamiprid",      0.03,           "ground",
  51,     "Clearfield",   "Acetamiprid",      0.03,           "ground",

  51,     "J.Sendra",     "Bentazone",        1.00,           "ground",
  51,     "Bomba",        "Bentazone",        1.00,           "ground",

  51,     "J.Sendra",     "MCPA",             0.50,           "ground",
  51,     "Bomba",        "MCPA",             0.50,           "ground",

  21,     "J.Sendra",     "Penoxsulam",       0.04,           "ground",
  21,     "Bomba",        "Penoxsulam",       0.04,           "ground",

  7,      "J.Sendra",     "Cyhalofop-butyl",  0.30,           "ground",
  7,      "Bomba",        "Cyhalofop-butyl",  0.30,           "ground",
  21,     "J.Sendra",     "Cyhalofop-butyl",  0.30,           "ground",
  21,     "Bomba",        "Cyhalofop-butyl",  0.30,           "ground",

  21,     "Clearfield",   "Cycloxydim",       0.30,           "ground",
  51,     "Clearfield",   "Cycloxydim",       0.30,           "ground",

  76,     "Bomba",        "Azoxystrobin",     0.20,           "aerial",
  90,     "Bomba",        "Azoxystrobin",     0.20,           "aerial",
  104,    "Bomba",        "Azoxystrobin",     0.20,           "aerial",
  76,     "J.Sendra",     "Azoxystrobin",     0.20,           "aerial",
  90,     "J.Sendra",     "Azoxystrobin",     0.20,           "aerial",
  76,     "Clearfield",   "Azoxystrobin",     0.20,           "aerial",
  90,     "Clearfield",   "Azoxystrobin",     0.20,           "aerial",

  76,     "Bomba",        "Difenoconazole",   0.13,           "aerial",
  90,     "Bomba",        "Difenoconazole",   0.13,           "aerial",
  104,    "Bomba",        "Difenoconazole",   0.13,           "aerial",
  76,     "J.Sendra",     "Difenoconazole",   0.13,           "aerial",
  90,     "J.Sendra",     "Difenoconazole",   0.13,           "aerial",
  76,     "Clearfield",   "Difenoconazole",   0.13,           "aerial",
  90,     "Clearfield",   "Difenoconazole",   0.13,           "aerial"
)

