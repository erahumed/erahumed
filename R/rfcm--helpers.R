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

get_chemical_db <- function(cluster_map) {
  # If necessary, here we could make this O(N) by using an r2r::hashmap()
  res <- list()
  for (system in cluster_map$ms_list) {
    for (application in system$applications) {
      if(any( sapply(res, \(x) identical(x, application$chemical)) ))
         next
      res <- c(res, list(application$chemical))
      }
    }
  return(res)
}

match_chemical <- function(chemical, db) {
  comparisons <- sapply(db, \(x) identical(chemical, x))
  matches <- which(comparisons)

  if (length(matches) == 0) {
    return(NA)
  } else if (length(matches) > 1) {
    stop("Multiple matches for chemical")
  }

  return(matches)
}

