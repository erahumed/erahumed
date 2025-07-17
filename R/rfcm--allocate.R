#' Allocate a Management System to a Surface Fraction
#'
#' Assigns a new management system to a fraction of surface area by selecting
#' clusters that match given spatial and structural criteria, using only the
#' subset of eligible clusters defined by `ditches` and `field_type`.
#'
#' @param map `[`[erahumed_cluster_map][cluster_map]`]` \cr
#'   A cluster map created with [new_cluster_map()] or [default_cluster_map()].
#'
#' @param system `[`[erahumed_management_system][management_system]`]` \cr
#'   A management system to assign, created with [new_management_system()] or
#'   helper functions such as [bomba()] or [clearfield()].
#'
#' @param target_fraction `[numeric(1)]` \cr
#'   The fraction of surface area **within the selected subset** of clusters
#'   (filtered by `ditches` and `field_type`) to allocate to the given system.
#'   Must be a number between 0 and 1.
#'
#' @param ditches `[integer]` \cr
#'   A vector of ditch IDs (e.g., 1:26) defining the subset of clusters to be
#'   considered for allocation. Defaults to all ditches.
#'
#' @param field_type `[character(1)]` \cr
#'   Type of field to consider when selecting clusters. Must be one of:
#'   * `"regular"` – only regular fields
#'   * `"tancat"` – only tancats
#'   * `"both"` – all field types (default)
#'
#' @return An updated `erahumed_cluster_map` object with the new system
#'   allocated to selected clusters.
#'
#' @details
#' This function allocates a management system to a subset of clusters selected
#' based on the `ditches` and `field_type` criteria. The `target_fraction` refers
#' to the fraction of **total surface area within this eligible subset**, not the
#' entire map.
#'
#' Clusters are randomly selected from the eligible subset until the cumulative
#' surface area of selected clusters reaches the desired fraction.
#'
#' If the requested fraction exceeds the total available surface in the subset
#' (e.g., due to prior allocations), the
#' function will issue a warning and perform a **partial allocation** using all
#' remaining candidates. The map is returned with the maximum possible surface
#' allocated under the given constraints.

#'
#' @export
allocate_surface <- function(map,
                             system,
                             target_fraction,
                             ditches = 1:26,
                             field_type = c("both", "regular", "tancat"))
{
  tryCatch({
    assert_cluster_map(map)
    assert_management_system(system)
    assert_positive_number(target_fraction)
    stopifnot(target_fraction <= 1)
    assert_integer_vector(ditches)
    field_type <- match.arg(field_type)
  }, error = function(e) {
    class(e) <- c("erahumed_allocate_surface_error", class(e))
    stop(e)
  })

  ditches <- paste0("d", ditches)

  # Determine rfms_id (append if not yet in the list)
  rfms_id <- NULL
  for (i in seq_along(map[["rfms_list"]])) {
    if (identical(system, map[["rfms_list"]][[i]])) {
      rfms_id <- i
      break
    }
  }
  if (is.null(rfms_id)) {
    map[["rfms_list"]] <- c(map[["rfms_list"]], list(system))
    rfms_id <- length(map[["rfms_list"]])
  }

  # Get total surface of eligible clusters
  candidates_all <- rfcm_get_map_candidates(map, ditches, field_type)
  target_surface <- target_fraction *
    rfcm_get_filtered_surface(ditches = ditches, field_type = field_type)

  allocated_surface <- 0
  remaining_candidates <- candidates_all

  while (allocated_surface < target_surface) {
    if (length(remaining_candidates) == 0) {
      warning(sprintf(
        "Could not fully allocate the requested target fraction (%.2f) within the selected subset (ditches: %s, field type: '%s'). Only %.2f%% of the requested surface could be allocated.",
        target_fraction,
        paste(ditches, collapse = ", "),
        field_type,
        100 * allocated_surface / target_surface
      ))
      break
    }

    cluster_id <- sample(remaining_candidates, 1)
    map <- rfcm_map_assign(map, cluster_id = cluster_id, rfms_id = rfms_id)

    allocated_surface <- allocated_surface + rfcm_cluster_surface(cluster_id)
    remaining_candidates <- setdiff(remaining_candidates, cluster_id)
  }

  return(map)
}
