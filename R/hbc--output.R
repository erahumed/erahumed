#' @export
materialize_output.erahumed_output_hbc <- function(output) {
  data.frame(
    date = output[["date"]],
    element_id = output[["element_id"]],
    area_m2 = output[["area_m2"]],
    is_tancat = output[["tancat"]],

    outflow_m3 = output[["outflow_m3"]],
    outflow_cm = output[["outflow_cm"]],

    inflow_m3 = output[["inflow_m3"]],
    inflow_cm = output[["inflow_cm"]],

    volume_m3 = output[["area_m2"]] * output[["height_eod_cm"]] / 100,
    depth_cm = output[["height_eod_cm"]],

    petp_cm = output[["petp_cm"]]
  )
}
