erahumed_components <- function() {
  res <- c("inp", "hba", "hbp", "ca")
  c(res, "dum") # Add dummy component with upstream dependence from all others
}
