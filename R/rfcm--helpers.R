get_management_df <- function(cluster_map) {
  n_rfms <- length(cluster_map$rfms_list)

  lapply(1:n_rfms, function(i) {
    rfms <- cluster_map$rfms_list[[i]]
    df <- wms_from_rfms(rfms)
    df$rfms_id <- i  # this is called 'rfms_id' for "historical" reasons.
    df
  }) |>
    Reduce(rbind, x = _)
}

get_applications_df <- function(cluster_map) {
  chemical_db <- get_chemical_db(cluster_map)
  rfms_list <- cluster_map$rfms_list
  lapply(seq_along(rfms_list), function(i) {
    df <- get_applications_df0(rfms_list[[i]], chemical_db = chemical_db)
    if (nrow(df) != 0) df$rfms_id <- i
    df
    }) |>
    Reduce(rbind, x = _, init = data.frame())
}

get_chemical_db <- function(cluster_map) {
  # If necessary, here we could make this O(N) by using an r2r::hashmap()
  res <- list()
  for (system in cluster_map$rfms_list) {
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

