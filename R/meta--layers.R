erahumed_layers <- function() {
  res <- c("inp", "hba", "hbp", "ca", "ct")
  c(res, "dum") # Add dummy layer with upstream dependence from all others
}

upstream_layers <- function(layer) {
  i <- match(layer, erahumed_layers())

  erahumed_layers() |> (\(.) .[seq_along(.) < i])()
}

downstream_layers <- function(layer) {
  i <- match(layer, erahumed_layers())

  erahumed_layers() |> (\(.) .[seq_along(.) > i])()
}
