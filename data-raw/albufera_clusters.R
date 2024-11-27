renv::use(
  class      = "class@7.3-22",
  classInt   = "classInt@0.4-10",
  cli        = "cli@3.6.3",
  DBI        = "DBI@1.2.3",
  dplyr      = "dplyr@1.1.4",
  e1071      = "e1071@1.7-16",
  fansi      = "fansi@1.0.6",
  generics   = "generics@0.1.3",
  glue       = "glue@1.8.0",
  KernSmooth = "KernSmooth@2.23-24",
  lifecycle  = "lifecycle@1.0.4",
  magrittr   = "magrittr@2.0.3",
  MASS       = "MASS@7.3-60.2",
  pillar     = "pillar@1.9.0",
  pkgconfig  = "pkgconfig@2.0.3",
  proxy      = "proxy@0.4-27",
  R6         = "R6@2.5.1",
  Rcpp       = "Rcpp@1.0.13",
  renv       = "renv@1.0.11",
  rlang      = "rlang@1.1.4",
  s2         = "s2@1.1.7",
  sf         = "sf@1.0-18",
  tibble     = "tibble@3.2.1",
  tidyselect = "tidyselect@1.2.1",
  units      = "units@0.8-5",
  utf8       = "utf8@1.2.4",
  vctrs      = "vctrs@0.6.5",
  withr      = "withr@3.0.1",
  wk         = "wk@0.9.4"
)

library(dplyr)

remove_non_ascii <- function(geometry) {
  # Remove non-ASCII string (degree symbol) from metadata of {sf} object
  crs <- sf::st_crs(geometry)
  crs$wkt <- iconv(crs$wkt, from = "UTF-8", to = "ASCII//TRANSLIT")
  sf::st_crs(geometry) <- crs
  return(geometry)
}

folder <- "data-raw/raw/gis/"


# Clusters
clusters_raw <- sf::st_read(paste0(folder, "clusters.shp")) |>
  rename(cluster_id = line_ids) |>
  as_tibble() |>
  mutate(across(geometry, remove_non_ascii))

albufera_cluster_geometries <- clusters_raw |> select(cluster_id, geometry)
albufera_clusters <- sf::st_drop_geometry(clusters_raw) |>
  transmute(
    cluster_id,
    ditch = gsub("acq", "d", acq_code),
    area,
    tancat = tancat == "1"
    )


# Ditches
ditches_raw <- sf::st_read(paste0(folder, "ditches.shp")) |>
  as_tibble() |>
  mutate(across(geometry, remove_non_ascii))

albufera_ditches_geometries <- ditches_raw |> select(ditch, geometry)
albufera_ditches <- sf::st_drop_geometry(ditches_raw) |>
  select(ditch, width, length, surface)


# Basins
basins_raw <- sf::st_read(paste0(folder, "basins.shp")) |>
  as_tibble() |>
  mutate(across(geometry, remove_non_ascii))

albufera_basins_geometries <- basins_raw |> select(ditch, geometry)
albufera_basins <- sf::st_drop_geometry(basins_raw) |>
  select(-geometry)



