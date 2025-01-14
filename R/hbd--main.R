#' @title `r erahumed_docs("layers", "hbd", "title")`
#' @name hbd
#'
#' @family simulation layers
#'
#' @description `r erahumed_docs("layers", "hbd", "description")`
#'
#' @return An object of class \link{erahumed_simulation}.
#'
#' @export
setup_hbd <- function(simulation, ditch_level_m = 1)
{
  tryCatch(
    {
      assert_erahumed_simulation(simulation)
      assert_positive_number(ditch_level_m)
    },
    error = function(e) {
      class(e) <- c("validate_hbd_params_error", class(e))
      stop(e)
    })

  setup_layer(layer = "hbd")
}

compute_hbd <- function(simulation)
{
  ditch_level_m <- get_layer_parameters(simulation, "hbd")[["ditch_level_m"]]
  hbp_res <- get_layer_output(simulation, "hbp")

  inflow_clusters_df <- hbp_res |>
    stats::aggregate(outflow_m3_s ~ ditch + date, FUN = sum) |>
    (function(df) {
      df$inflow_clusters_m3 <- df$outflow_m3_s * s_per_day()
      df <- df[, c("ditch", "date", "inflow_clusters_m3")]
      return(df)
      })()

  outflow_lake_df <- hbp_res |>
    stats::aggregate(capacity_m3_s ~ ditch + date, FUN = \(x) x[[1]]) |>
    (function(df) {
      df$outflow_lake_m3 <- df$capacity_m3_s * s_per_day()
      df <- df[, c("ditch", "date", "outflow_lake_m3")]
      return(df)
    })()

  volume_df <- info_ditches() |>
    (function(df) {
      df$volume_m3 <- df$surface * ditch_level_m
      df <- df[, c("ditch", "volume_m3")]
      return(df)
    })()

  out <- merge(inflow_clusters_df, outflow_lake_df, by = c("ditch", "date"))
  out$inflow_external_m3 <- out$outflow_lake_m3 - out$inflow_clusters_m3

  out <- merge(out, volume_df, by = "ditch")

  validate_hbd_output(out)

  simulation [["hbd"]] [["output"]] <- out

  return(simulation)
}

validate_hbd_output <- function(output) {
  assert_data.frame(output,
                    template = data.frame(outflow_lake_m3 = numeric(),
                                          inflow_clusters_m3 = numeric(),
                                          inflow_external_m3 = numeric(),
                                          volume_m3 = numeric()
                                          )
                    )
}
