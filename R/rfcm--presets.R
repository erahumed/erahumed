#' @rdname cluster_map
#' @export
default_cluster_map <- function(seed = 840) {

  withr::with_seed(seed, {
    new_cluster_map(default_management_system = jsendra()) |>
      allocate_surface(bomba(), target_fraction = 0.1, field_type = "tancat") |>
      allocate_surface(clearfield(), target_fraction = 0.1, ditches = 1:19)}
    )


}
