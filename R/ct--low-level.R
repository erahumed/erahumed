ct_to_cluster_wrap <- function(cluster_ca_df)
{
  res <- cluster_ca_df

  ct_to_cluster_fun <- ct_to_cluster

  chemicals <- unique(albufera_ca_schedules$chemical)
  chemicals <- names(res)[names(res) %in% chemicals]
  for (chemical in chemicals)
    res[[chemical]] <- ct_to_cluster(
      application_kg = cluster_ca_df[[chemical]],
      rain_mm = cluster_ca_df[["rain_mm"]],
      temperature = rnorm(nrow(cluster_ca_df)),
      height_cm = cluster_ca_df[["real_height_cm"]],
      outflow_m3_s = cluster_ca_df[["real_outflow_m3_s"]],
      area_m2 = cluster_ca_df[["area_m2"]][[1]]
      )

  return(res)
}

ct_to_cluster <- function(application_kg,
                          rain_mm,
                          temperature,
                          height_cm,
                          outflow_m3_s,
                          area_m2)
{
  return(application_kg)
}
