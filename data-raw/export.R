renv::use(
  askpass     = "askpass@1.2.1",
  cli         = "cli@3.6.3",
  clipr       = "clipr@0.8.0",
  crayon      = "crayon@1.5.3",
  credentials = "credentials@2.0.2",
  curl        = "curl@5.2.3",
  desc        = "desc@1.4.3",
  fs          = "fs@1.6.4",
  gert        = "gert@2.1.4",
  gh          = "gh@1.4.1",
  gitcreds    = "gitcreds@0.1.2",
  glue        = "glue@1.8.0",
  httr2       = "httr2@1.0.5",
  ini         = "ini@0.3.1",
  jsonlite    = "jsonlite@1.8.9",
  lifecycle   = "lifecycle@1.0.4",
  magrittr    = "magrittr@2.0.3",
  openssl     = "openssl@2.2.2",
  purrr       = "purrr@1.0.2",
  R6          = "R6@2.5.1",
  rappdirs    = "rappdirs@0.3.3",
  renv        = "renv@1.0.11",
  rlang       = "rlang@1.1.4",
  rprojroot   = "rprojroot@2.0.4",
  rstudioapi  = "rstudioapi@0.16.0",
  sys         = "sys@3.4.3",
  usethis     = "usethis@3.0.0",
  vctrs       = "vctrs@0.6.5",
  whisker     = "whisker@0.4.1",
  withr       = "withr@3.0.1",
  yaml        = "yaml@2.3.10",
  zip         = "zip@2.3.1"
)

source("data-raw/albufera_ca_schedules.R")
source("data-raw/albufera_clusters.R")
source("data-raw/albufera_ct_parameters.R")
source("data-raw/albufera_data.R")
source("data-raw/albufera_management.R")

usethis::use_data(albufera_outflows, overwrite = TRUE)
usethis::use_data(albufera_weather, overwrite = TRUE)
usethis::use_data(albufera_clusters, overwrite = TRUE)
usethis::use_data(albufera_management, overwrite = TRUE)
usethis::use_data(albufera_ca_schedules, overwrite = TRUE)

usethis::use_data(albufera_cluster_geometries,
                  albufera_ct_parameters,
                  internal = TRUE, overwrite = TRUE)
