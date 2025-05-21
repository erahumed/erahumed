new_cluster_map <- function(default_management_system = new_management_system())
{
  map_df <- data.frame(cluster_id = info_clusters()$element_id, ms_id = NA)

  res <- list(map_df = map_df,
              ms_list = list(),
              default_ms = default_management_system
              )

  class(res) <- "erahumed_cluster_map"

  return(res)
}

is_cluster_map <- function(x) {
  inherits(x, "erahumed_cluster_map")
}
