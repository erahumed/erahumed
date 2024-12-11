#' ERAHUMED simulation layers
#'
#' @description
#' Enumerates the ERAHUMED simulation layers
#' (*cf.* \link{erahumed_simulation_interface}), in dependency order (from
#' upstream to downstream). A schematic view is available
#' [on the package website](https://erahumed.github.io/erahumed/articles/pipeline-scheme.html).
#'
#' @return
#' A character vector.
#'
#' @details For further information on the various layers, see `?<layer_name>`.
#'
#' @export
erahumed_layers <- function() {
  names(erahumed_docs("layers"))
}

upstream_layers <- function(layer) {
  i <- match(layer, erahumed_layers())

  erahumed_layers() |> (\(.) .[seq_along(.) < i])()
}

downstream_layers <- function(layer) {
  i <- match(layer, erahumed_layers())

  erahumed_layers() |> (\(.) .[seq_along(.) > i])()
}
