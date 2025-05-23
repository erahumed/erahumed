get_management_df <- function(cluster_map) {
  n_rfms <- length(cluster_map$ms_list)

  lapply(1:n_rfms, function(i) {
    rfms <- cluster_map$ms_list[[i]]
    df <- wms_from_rfms(rfms)
    df$variety <- i  # this is called 'variety' for "historical" reasons.
    df
  }) |>
    Reduce(rbind, x = _)
}

get_applications_df <- function(cluster_map) {
  ms_list <- cluster_map$ms_list
  lapply(seq_along(ms_list), function(i) {
    df <- get_applications_df0(ms_list[[i]])
    df$variety <- i
    df
    }) |>
    Reduce(rbind, x = _, init = data.frame())
}

