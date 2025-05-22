get_management_df <- function(cluster_map) {
  n_rfms <- length(cluster_map$ms_list)
  management_df <- lapply(1:n_rfms, function(i) {
    rfms <- cluster_map$ms_list[[i]]
    df <- wms_from_rfms(rfms)
    df$variety <- i  # this is called 'variety' for "historical" reasons.
    df
  }) |>
    Reduce(rbind, x = _)
}
