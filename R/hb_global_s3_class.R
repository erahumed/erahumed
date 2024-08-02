make_hb_global <- function(df) {
  name <- deparse(substitute(df))

  if ( !("data.frame" %in% class(df)) )
    stop( paste(name, "must be a 'data.frame'.") )

  # Check that df has the required columns
  required_cols <- c("level", "rain_mm", "evapotranspiration_mm", "volume",
                     "inflow_total", "outflow_total", "residence_time_days")
  for (col in required_cols) {
    if (col %in% colnames(df)) next
    stop(paste0("Missing '", col, "' column in input '", name, "'."))
  }

  class(df) <- c("hb_global", class(df))
  attr(class(df), "package") <- "erahumed"
  return(df)
}
