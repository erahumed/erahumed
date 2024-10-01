ct_to_cluster_wrap <- function(cluster_ca_df)
{
  res <- cluster_ca_df

  ct_to_cluster_fun <- ct_to_cluster

  chemicals <- unique(albufera_ca_schedules$chemical)
  chemicals <- names(res)[names(res) %in% chemicals]
  for (chemical in chemicals)
    res[[chemical]] <- ct_to_cluster(res[[chemical]])

  return(res)
}

ct_to_cluster <- function(application_kg)

{
  return(application_kg)
}
