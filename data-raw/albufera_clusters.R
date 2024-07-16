library(dplyr)

clusters_raw <- sf::st_read("data-raw/raw/paddysdef.shp") |>
  rename(cluster_id = line_ids) |>
  as_tibble()

albufera_cluster_geometries <- clusters_raw |>
  select(cluster_id, geometry)

albufera_clusters <- sf::st_drop_geometry(clusters_raw) |>
  transmute(
    cluster_id,
    ditch = gsub("acq", "d", acq_code),
    area,
    tancat = tancat == "1",
    rice_variety = mngmt,
    case = gsub("Case", "", CASE) |> as.factor()
  )

usethis::use_data(albufera_clusters, overwrite = TRUE)
usethis::use_data(albufera_cluster_geometries, overwrite = TRUE)
