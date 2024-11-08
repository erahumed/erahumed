#' ERAHUMED simulation layers
#'
#' @description
#' Returns a list of ERAHUMED simulation layers
#' (*cf.* \link{erahumed_simulation_interface}).
#'
#' @return
#' A character vector. List of ERAHUMED simulation layers.
#'
#' @details For further information on the various layers, see `?<layer_name>`.
#'
#' @export
erahumed_layers <- function() {
  c("inp", "hba", "hbp", "ca", "ct")
}

upstream_layers <- function(layer) {
  i <- match(layer, erahumed_layers())

  erahumed_layers() |> (\(.) .[seq_along(.) < i])()
}

downstream_layers <- function(layer) {
  i <- match(layer, erahumed_layers())

  erahumed_layers() |> (\(.) .[seq_along(.) > i])()
}
