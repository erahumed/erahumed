new_rfa <- function(fallback_rfms = new_rfms()) {

  allocations_df <-
    data.frame(cluster_id = info_clusters()$element_id, rfms_id = NA)

  res <- list(allocations = allocations_df,
              rfms = list(),
              fallback_rfms = fallback_rfms
              )

  class(res) <- "erahumed_rfa"

  return(res)
}

is_rfa <- function(x) {
  inherits(x, "erahumed_rfa")
}
