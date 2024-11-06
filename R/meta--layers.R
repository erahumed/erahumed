erahumed_layers <- function() {
  res <- c("inp", "hba", "hbp", "ca", "ct")
  c(res, "dum") # Add dummy layer with upstream dependence from all others
}
