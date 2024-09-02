delayedAssign("albufera_cluster_geometries", local({
  requireNamespace("sf", quietly = TRUE)
  try(erahumed:::albufera_cluster_geometries, silent = TRUE)
}))
