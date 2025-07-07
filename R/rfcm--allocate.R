#' Allocate a Management System to a Surface Fraction
#'
#' Assigns a new management system to a fraction of surface area by selecting
#' clusters that match given spatial and structural criteria.
#'
#' @param map `[`[erahumed_cluster_map][cluster_map]`]` \cr
#'   A cluster map created with [new_cluster_map()] or [default_cluster_map()].
#'
#' @param system `[`[erahumed_management_system][management_system]`]` \cr
#'   A management system to assign, created with [new_management_system()] or
#'   helper functions such as [bomba()] or [clearfield()].
#'
#' @param target_fraction `[numeric(1)]` \cr
#'   The fraction of total surface area to allocate to the given system. Must
#'   be a number between 0 and 1.
#'
#' @param ditches `[integer]` \cr
#'   A vector of ditch IDs where candidate clusters will be selected from.
#'   Defaults to all ditches (1:26).
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
#' This function randomly selects clusters matching the specified `ditches` and
#' `field_type` until the cumulative surface area of selected clusters reaches
#' the `target_fraction`. The new management system is added to the internal
#' list and assigned a unique ID within the map.
#'
#' If no eligible clusters remain before reaching the target fraction, the
#' function will return early with a partial allocation.
#'
#' @seealso [new_cluster_map()], [default_cluster_map()], [new_management_system()]
#'
#' @examples
#' map <- new_cluster_map(jsendra())
#' map <- allocate_surface(map, bomba(), target_fraction = 0.1, field_type = "tancat")
#'
#' @export
allocate_surface <- function(map,
                             system,
                             target_fraction,
                             ditches = 1:26,
                             field_type = c("both", "regular", "tancat")
)
{
  tryCatch(
    {
      assert_cluster_map(map)
      assert_management_system(system)
      assert_positive_number(target_fraction)
      stopifnot(target_fraction <= 1)
      assert_integer_vector(ditches)
      field_type <- match.arg(field_type)
    },
    error = function(e) {
      class(e) <- c("erahumed_allocate_surface_error", class(e))
      stop(e)
    })

  ditches <- paste0("d", ditches)

  ms_id <- NULL
  for (i in seq_along(map[["ms_list"]])) {
    if (identical(system, map[["ms_list"]][[i]]))
      ms_id <- i
  }
  if (is.null(ms_id)) {
    map[["ms_list"]] <- c(map[["ms_list"]], list(system))
    ms_id <- length(map[["ms_list"]])
  }

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
