renv::use(
  askpass     = "askpass@1.2.1",
  cli         = "cli@3.6.5",
  clipr       = "clipr@0.8.0",
  crayon      = "crayon@1.5.3",
  credentials = "credentials@2.0.2",
  curl        = "curl@6.4.0",
  desc        = "desc@1.4.3",
  fs          = "fs@1.6.6",
  gert        = "gert@2.1.5",
  gh          = "gh@1.5.0",
  gitcreds    = "gitcreds@0.1.2",
  glue        = "glue@1.8.0",
  httr2       = "httr2@1.1.2",
  ini         = "ini@0.3.1",
  jsonlite    = "jsonlite@2.0.0",
  lifecycle   = "lifecycle@1.0.4",
  magrittr    = "magrittr@2.0.3",
  openssl     = "openssl@2.3.3",
  purrr       = "purrr@1.0.4",
  R6          = "R6@2.6.1",
  rappdirs    = "rappdirs@0.3.3",
  renv        = "renv@1.1.4",
  rlang       = "rlang@1.1.6",
  rprojroot   = "rprojroot@2.0.4",
  rstudioapi  = "rstudioapi@0.17.1",
  sys         = "sys@3.4.3",
  usethis     = "usethis@3.1.0",
  vctrs       = "vctrs@0.6.5",
  whisker     = "whisker@0.4.1",
  withr       = "withr@3.0.2",
  yaml        = "yaml@2.3.10",
  zip         = "zip@2.3.3"
)

source("data-raw/albufera_clusters.R")
source("data-raw/albufera_data.R")

usethis::use_data(albufera_outflows, overwrite = TRUE)
usethis::use_data(albufera_weather, overwrite = TRUE)

usethis::use_data(
  albufera_clusters, albufera_cluster_geometries,
  albufera_ditches, albufera_ditches_geometries, albufera_basins_geometries,
  internal = TRUE, overwrite = TRUE)
