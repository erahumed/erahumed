allocate_surface <- function(map,
                             system,
                             target_fraction,
                             ditches = paste0("d", 1:26),
                             field_type = c("both", "regular", "tancat")
)
{
  tryCatch(
    {
      assert_cluster_map(map)
      assert_management_system(system)
      assert_positive_number(target_fraction)
      stopifnot(target_fraction <= 1)
      assert_character(ditches)
      field_type <- match.arg(field_type)
    },
    error = function(e) {
      class(e) <- c("erahumed_allocate_surface_error", class(e))
      stop(e)
    })

  map[["ms_list"]] <- c(map[["ms_list"]], list(system))
  ms_id <- length(map[["ms_list"]])

  allocated_fraction <- 0
  while (allocated_fraction < target_fraction) {
    candidates <- map_candidates(map, ditches, field_type)
    if (length(candidates) == 0)
      break
    cluster_id <- sample(candidates, 1)
    map <- map_assign(map, cluster_id = cluster_id, ms_id = ms_id)

    allocated_fraction <- allocated_fraction + map_surface_fraction(cluster_id)
  }

  return(map)
}
