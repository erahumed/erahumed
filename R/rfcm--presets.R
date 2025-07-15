#' @rdname cluster_map
#' @name cluster_map
#' @export
default_cluster_map <- function(seed = 840) {

  withr::with_seed(seed, {
    new_cluster_map(default_management_system = jsendra()) |>
      allocate_surface(clearfield(), target_fraction = 0.8, ditches = 1:19) |>
      allocate_surface(clearfield(), target_fraction = 0.2, ditches = 20:26)
    })


}
