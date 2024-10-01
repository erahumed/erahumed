erahumed_components <- function() {
  res <- c("inp", "hba", "hbp", "ca", "ct")
  c(res, "dum") # Add dummy component with upstream dependence from all others
}
