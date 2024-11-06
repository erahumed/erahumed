erahumed_layers <- function() {
  res <- c("inp", "hba", "hbp", "ca", "ct")
}

upstream_layers <- function(layer) {
  i <- match(layer, erahumed_layers())

  erahumed_layers() |> (\(.) .[seq_along(.) < i])()
}

downstream_layers <- function(layer) {
  i <- match(layer, erahumed_layers())

  erahumed_layers() |> (\(.) .[seq_along(.) > i])()
}
