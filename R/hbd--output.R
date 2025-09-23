#' @export
materialize_output.erahumed_output_hbd <- function(output) {
  data.frame(
    date = output[["date"]],
    element_id = output[["element_id"]],
    area_m2 = output[["area_m2"]],
    is_tancat = NA,

    outflow_m3 = output[["outflow_lake_m3"]],
    outflow_cm = 100 * output[["outflow_lake_m3"]] / output[["area_m2"]],

    inflow_m3 = output[["inflow_clusters_m3"]] + output[["inflow_external_m3"]],
    inflow_cm =
      100 *
      (output[["inflow_clusters_m3"]] + output[["inflow_external_m3"]]) /
      output[["area_m2"]]
    ,

    volume_m3 = output[["volume_m3"]],
    depth_cm = 100 * output[["level_m"]],

    petp_cm = output[["petp_cm"]]
  )
}
