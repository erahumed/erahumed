chemical_color_map <- function() {
  chemicals <- names(info_chemicals())
  res <- grDevices::palette.colors(length(chemicals))
  names(res) <- chemicals
  return(res)
}

