compute_hbd <- function(simulation)
{
  ditch_level_m <- get_input(simulation, "ditch_level_m")
  hbc_res <- get_raw_output(simulation, "hbc")

  inflow_clusters_df <- hbc_res |>
    stats::aggregate(outflow_m3 ~ ditch_element_id + date, FUN = sum) |>
    (function(df) {
      df$inflow_clusters_m3 <- df$outflow_m3
      df <- df[, c("ditch_element_id", "date", "inflow_clusters_m3")]
      return(df)
      })()

  outflow_lake_df <- hbc_res |>
    stats::aggregate(capacity_m3 ~ ditch_element_id + date, FUN = \(x) x[[1]]) |>
    (function(df) {
      df$outflow_lake_m3 <- df$capacity_m3
      df <- df[, c("ditch_element_id", "date", "outflow_lake_m3")]
      return(df)
    })()

  volume_df <- info_ditches() |>
    (function(df) {
      df$volume_m3 <- df$surface * ditch_level_m
      df$level_m <- df$volume_m3 / df$surface  # Open to generalizations
      df$area_m2 <- df$surface
      df <- df[, c("element_id", "volume_m3", "level_m", "area_m2")]
      return(df)
    })()

  out <- merge(inflow_clusters_df, outflow_lake_df, by = c("ditch_element_id", "date"))
  out$inflow_external_m3 <- out$outflow_lake_m3 - out$inflow_clusters_m3

  out <- merge(out, volume_df, by.x = "ditch_element_id", by.y = "element_id")

  names(out)[names(out) == "ditch_element_id"] <- "element_id"

  validate_hbd_output(out)

  simulation [["outputs"]] [["hbd"]] <- out

  return(simulation)
}

validate_hbd_output <- function(output) {
  assert_data.frame(output,
                    template = data.frame(outflow_lake_m3 = numeric(),
                                          inflow_clusters_m3 = numeric(),
                                          inflow_external_m3 = numeric(),
                                          volume_m3 = numeric(),
                                          element_id = character()
                                          )
                    )
}
