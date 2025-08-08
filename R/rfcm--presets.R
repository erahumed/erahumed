#' @rdname rfms_map
#' @name rfms_map
#' @export
default_rfms_map <- function(seed = 840) {

  withr::with_seed(seed, {
    new_rfms_map(default_rfms = jsendra()) |>
      allocate_surface(clearfield(), target_fraction = 0.8, ditches = 1:9) |>
      allocate_surface(clearfield(), target_fraction = 0.2, ditches = 10:26)
    })


}
