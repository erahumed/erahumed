source("data-raw/albufera_ca_schedules.R")
source("data-raw/albufera_clusters.R")
source("data-raw/albufera_ct_parameters.R")
source("data-raw/albufera_data.R")
source("data-raw/albufera_management.R")

usethis::use_data(albufera_outflows, overwrite = TRUE)
usethis::use_data(albufera_petp, overwrite = TRUE)
usethis::use_data(albufera_clusters, overwrite = TRUE)
usethis::use_data(albufera_management, overwrite = TRUE)
usethis::use_data(albufera_ca_schedules, overwrite = TRUE)

usethis::use_data(albufera_cluster_geometries,
                  albufera_ct_parameters,
                  internal = TRUE, overwrite = TRUE)
